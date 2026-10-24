# Euclidean projection onto {l <= w <= u, sum(w) = 1}
.proj_capped_simplex <- function(v, l, u) {
  f <- function(th) sum(pmin(pmax(v - th, l), u)) - 1
  lo <- min(v - u) - 1
  hi <- max(v - l) + 1
  th <- stats::uniroot(f, c(lo, hi), tol = 1e-14)$root
  pmin(pmax(v - th, l), u)
}

# minimise w'Sw - tau * mu'w over the capped simplex
.solve_mv <- function(mu, S, tau, l, u, w0 = NULL, tol = 1e-12,
                      maxit = 20000L) {
  n <- length(mu)
  L <- 2 * max(eigen(S, symmetric = TRUE, only.values = TRUE)$values)
  step <- if (L > 0) 1 / L else 1
  w <- if (is.null(w0)) .proj_capped_simplex(rep(1 / n, n), l, u) else w0
  for (i in seq_len(maxit)) {
    g <- 2 * as.numeric(S %*% w) - tau * mu
    wn <- .proj_capped_simplex(w - step * g, l, u)
    if (max(abs(wn - w)) < tol) {
      w <- wn
      break
    }
    w <- wn
  }
  w
}

.pstats <- function(w, mu, S) {
  r <- sum(w * mu)
  s <- sqrt(max(as.numeric(t(w) %*% S %*% w), 0))
  c(return = r, risk = s, ratio = if (s > 0) r / s else NA_real_,
    hhi = sum(w^2))
}

#' Risk-efficient destination market mix
#'
#' Applies mean-variance portfolio analysis (Markowitz, 1952) to the
#' allocation of exports across destination markets. Each market is an
#' "asset" whose return is a performance measure such as the growth of
#' export earnings or the realised export price (unit value) change. The
#' function finds destination shares \eqn{w} that minimise
#' \eqn{w'\Sigma w - \tau \mu' w} subject to \eqn{\sum w = 1} and
#' \eqn{l \le w \le u}, where share bounds reflect market-access or
#' capacity limits. It reports the minimum-variance mix, the mix with the
#' highest return per unit of risk on the efficient frontier, the current
#' mix and the efficient frontier, so that the risk reduction available
#' from diversification can be compared with the current market structure.
#'
#' The problem is solved by projected gradient descent on the bounded
#' simplex, which needs no additional packages.
#'
#' @param returns A data frame or matrix of returns, one column per market
#'   and one row per period. Alternatively supply `mean` and `cov`.
#' @param shares Optional named vector of current destination shares.
#' @param mean,cov Optional mean vector and covariance matrix of returns,
#'   used instead of `returns`.
#' @param min_share,max_share Lower and upper bounds on each market's share
#'   (scalars or one per market).
#' @param n_frontier Number of points on the efficient frontier.
#'
#' @return A list of class `market_diversification` with `weights` (data
#'   frame of current, minimum-variance and best return-to-risk shares by
#'   market), `summary` (expected return, risk, return-to-risk ratio and
#'   Herfindahl index of each mix), `frontier` (data frame of efficient
#'   portfolios) and the inputs `mean` and `cov`. A `plot()` method draws
#'   the frontier.
#'
#' @references Markowitz, H. (1952). Portfolio selection. *The Journal of
#'   Finance*, 7(1), 77-91. \doi{10.2307/2975974}
#'
#' @examples
#' grapes <- subset(hort_exports, product == "grapes")
#' wide <- reshape(grapes[, c("year", "destination", "export_value")],
#'                 idvar = "year", timevar = "destination",
#'                 direction = "wide")
#' vals <- as.matrix(wide[order(wide$year), -1])
#' colnames(vals) <- sub("export_value.", "", colnames(vals))
#' growth <- diff(log(vals))
#' cur <- vals[nrow(vals), ] / sum(vals[nrow(vals), ])
#' md <- market_diversification(growth, shares = cur, max_share = 0.4)
#' md
#' plot(md)
#' @export
market_diversification <- function(returns = NULL, shares = NULL,
                                   mean = NULL, cov = NULL, min_share = 0,
                                   max_share = 1, n_frontier = 25) {
  if (!is.null(returns)) {
    R <- as.matrix(returns)
    if (!is.numeric(R) || anyNA(R) || nrow(R) < 3L) {
      stop("`returns` must be numeric, complete and have at least 3 rows.",
           call. = FALSE)
    }
    mu <- colMeans(R)
    S <- stats::cov(R)
  } else {
    if (is.null(mean) || is.null(cov)) {
      stop("Supply `returns` or both `mean` and `cov`.", call. = FALSE)
    }
    mu <- mean
    S <- as.matrix(cov)
  }
  n <- length(mu)
  if (is.null(names(mu))) names(mu) <- colnames(S)
  if (is.null(names(mu))) names(mu) <- paste0("market_", seq_len(n))
  if (!all(dim(S) == n)) stop("`cov` must be square with one row per market.",
                              call. = FALSE)
  .chk(mu, "mean", scalar = FALSE)
  .chk(S, "cov", scalar = FALSE)
  .chk(min_share, "min_share", 0, 1, scalar = FALSE)
  .chk(max_share, "max_share", 0, 1, scalar = FALSE)
  l <- rep_len(min_share, n)
  u <- rep_len(max_share, n)
  if (any(l > u) || sum(l) > 1 + 1e-12 || sum(u) < 1 - 1e-12) {
    stop("Share bounds are infeasible: need sum(min_share) <= 1 <= ",
         "sum(max_share).", call. = FALSE)
  }
  .chk(n_frontier, "n_frontier", lower = 2)

  w_mv <- .solve_mv(mu, S, 0, l, u)
  # tau large enough to reach the maximum-return corner
  tau_max <- 2 * max(abs(S)) * 1e3 / max(1e-12, diff(range(mu)))
  taus <- c(0, exp(seq(log(tau_max * 1e-6), log(tau_max),
                       length.out = n_frontier - 1)))
  fr <- vector("list", length(taus))
  w <- w_mv
  for (i in seq_along(taus)) {
    w <- .solve_mv(mu, S, taus[i], l, u, w0 = w)
    fr[[i]] <- c(tau = taus[i], .pstats(w, mu, S), w)
  }
  frontier <- as.data.frame(do.call(rbind, fr))
  names(frontier) <- c("tau", "return", "risk", "ratio", "hhi", names(mu))
  frontier <- frontier[!duplicated(round(frontier$risk, 10)), , drop = FALSE]
  best <- which.max(ifelse(frontier$return > 0, frontier$ratio, -Inf))
  w_best <- as.numeric(frontier[best, names(mu)])

  wt <- data.frame(market = names(mu), minimum_variance = w_mv,
                   best_return_to_risk = w_best, row.names = NULL)
  smry <- rbind(minimum_variance = .pstats(w_mv, mu, S),
                best_return_to_risk = .pstats(w_best, mu, S))
  if (!is.null(shares)) {
    if (is.null(names(shares)) || !all(names(mu) %in% names(shares))) {
      stop("`shares` must be named with every market.", call. = FALSE)
    }
    cur <- as.numeric(shares[names(mu)])
    cur <- cur / sum(cur)
    wt$current <- cur
    wt <- wt[, c("market", "current", "minimum_variance",
                 "best_return_to_risk")]
    smry <- rbind(current = .pstats(cur, mu, S), smry)
  }
  out <- list(weights = wt, summary = smry, frontier = frontier,
              mean = mu, cov = S)
  class(out) <- "market_diversification"
  out
}

#' @export
print.market_diversification <- function(x, ...) {
  cat("Mean-variance destination market analysis\n\nShares\n")
  tab <- x$weights
  num <- vapply(tab, is.numeric, logical(1))
  tab[num] <- lapply(tab[num], .fmt)
  print(tab, row.names = FALSE)
  cat("\nPortfolio statistics (per period)\n")
  print(round(x$summary, 4))
  invisible(x)
}

#' Plot the efficient frontier of destination markets
#'
#' @param x A [market_diversification()] object.
#' @param ... Further arguments passed to [graphics::plot()].
#' @return `x`, invisibly.
#' @examples
#' set.seed(1)
#' r <- matrix(rnorm(60, 0.05, 0.1), 20, 3,
#'             dimnames = list(NULL, c("A", "B", "C")))
#' plot(market_diversification(r))
#' @export
plot.market_diversification <- function(x, ...) {
  fr <- x$frontier
  sd_m <- sqrt(diag(x$cov))
  xl <- range(c(fr$risk, sd_m, x$summary[, "risk"]))
  yl <- range(c(fr$return, x$mean, x$summary[, "return"]))
  graphics::plot(fr$risk, fr$return, type = "l", lwd = 2, xlim = xl,
                 ylim = yl, xlab = "Risk (standard deviation)",
                 ylab = "Expected return", ...)
  graphics::points(sd_m, x$mean, pch = 1)
  graphics::text(sd_m, x$mean, labels = names(x$mean), pos = 4, cex = 0.8)
  s <- x$summary
  graphics::points(s[, "risk"], s[, "return"], pch = 19,
                   col = seq_len(nrow(s)) + 1)
  graphics::legend("bottomright", legend = rownames(s), pch = 19,
                   col = seq_len(nrow(s)) + 1, bty = "n", cex = 0.8)
  invisible(x)
}
