# Lag helper: returns x shifted by k periods (NA padded)
.lagv <- function(x, k) if (k == 0) x else c(rep(NA_real_, k), x[seq_len(length(x) - k)])

# ADF regression on residuals without deterministic terms; lag by AIC.
.adf_resid <- function(u, max_lag) {
  du <- c(NA, diff(u))
  n <- length(u)
  start <- max_lag + 2L
  fits <- lapply(0:max_lag, function(p) {
    X <- data.frame(du = du, u1 = .lagv(u, 1))
    for (i in seq_len(p)) X[[paste0("d", i)]] <- .lagv(du, i)
    X <- X[start:n, , drop = FALSE]
    stats::lm(du ~ 0 + ., data = X)
  })
  aic <- vapply(fits, stats::AIC, numeric(1))
  best <- fits[[which.min(aic)]]
  cf <- summary(best)$coefficients
  list(stat = cf["u1", "t value"], lag = which.min(aic) - 1L,
       nobs = stats::nobs(best))
}

#' Vertical price transmission between export and domestic prices
#'
#' Tests whether domestic prices (for example wholesale or farm-gate prices)
#' move with export prices (or export unit values) in the long run and how
#' quickly deviations are corrected.
#'
#' **Step 1: long-run relationship.** \eqn{d_t = a + b\,w_t + u_t}, where
#' \eqn{b} is the long-run price transmission elasticity when prices are in
#' logs.
#'
#' **Step 2: Engle-Granger cointegration test.** An augmented Dickey-Fuller
#' regression without deterministic terms is fitted to the residuals
#' \eqn{u_t}, with the lag order chosen by AIC up to `max_lag`. The t
#' statistic is compared with the asymptotic critical values of MacKinnon
#' (2010) for two variables with a constant: -3.896 (1%), -3.336 (5%) and
#' -3.044 (10%).
#'
#' **Step 3: error correction model.**
#' \deqn{\Delta d_t = c + \beta_0 \Delta w_t + \sum_{i=1}^{p}(\phi_i
#'   \Delta d_{t-i} + \theta_i \Delta w_{t-i}) + \alpha u_{t-1} + e_t.}
#' \eqn{\beta_0} is the short-run elasticity and \eqn{\alpha} (expected to be
#' negative) the speed of adjustment; the half-life of a deviation is
#' \eqn{\ln 0.5 / \ln(1 + \alpha)} periods. With `asymmetric = TRUE` the
#' error correction term is split into positive and negative parts
#' (Granger and Lee, 1989), and an F test of equal adjustment speeds is
#' reported.
#'
#' @param domestic Numeric vector or time series of domestic prices.
#' @param world Numeric vector or time series of export or world prices,
#'   same length as `domestic`.
#' @param log Logical; take natural logarithms of prices first.
#' @param ecm_lags Number of lagged differences \eqn{p} in the error
#'   correction model.
#' @param max_lag Maximum lag order considered in the residual ADF test.
#' @param asymmetric Logical; estimate an asymmetric error correction model.
#'
#' @return A list of class `price_transmission` with `long_run` (intercept,
#'   elasticity, R-squared), `cointegration` (ADF statistic, lag, critical
#'   values and decision at 5 percent), `ecm` (coefficient table of the
#'   error correction model), `adjustment` (speed of adjustment and
#'   half-life; separate values for positive and negative deviations when
#'   asymmetric, with the F test), and the fitted `lm` models.
#'
#' @references Engle, R. F. and Granger, C. W. J. (1987). Co-integration
#'   and error correction: representation, estimation, and testing.
#'   *Econometrica*, 55(2), 251-276. \doi{10.2307/1913236}
#'
#'   Granger, C. W. J. and Lee, T. H. (1989). Investigation of production,
#'   sales and inventory relationships using multicointegration and
#'   non-symmetric error correction models. *Journal of Applied
#'   Econometrics*, 4(S1), S145-S159. \doi{10.1002/jae.3950040508}
#'
#'   MacKinnon, J. G. (2010). Critical values for cointegration tests.
#'   Queen's Economics Department Working Paper No. 1227, Queen's
#'   University, Kingston, Ontario.
#'
#' @examples
#' pt <- price_transmission(grape_prices$domestic_price,
#'                          grape_prices$export_unit_value)
#' pt
#' price_transmission(grape_prices$domestic_price,
#'                    grape_prices$export_unit_value,
#'                    asymmetric = TRUE)$adjustment
#' @export
price_transmission <- function(domestic, world, log = TRUE, ecm_lags = 1,
                               max_lag = 4, asymmetric = FALSE) {
  d <- as.numeric(domestic)
  w <- as.numeric(world)
  if (length(d) != length(w)) {
    stop("`domestic` and `world` must have the same length.", call. = FALSE)
  }
  ok <- stats::complete.cases(d, w)
  d <- d[ok]
  w <- w[ok]
  .chk(ecm_lags, "ecm_lags", lower = 0)
  .chk(max_lag, "max_lag", lower = 0)
  ecm_lags <- as.integer(round(ecm_lags))
  max_lag <- as.integer(round(max_lag))
  if (length(d) < 20L + max_lag + ecm_lags) {
    stop("At least ", 20L + max_lag + ecm_lags,
         " complete observations are required.", call. = FALSE)
  }
  if (log) {
    if (any(d <= 0) || any(w <= 0)) {
      stop("Prices must be positive when `log = TRUE`.", call. = FALSE)
    }
    d <- base::log(d)
    w <- base::log(w)
  }
  n <- length(d)

  lr <- stats::lm(d ~ w)
  u <- stats::residuals(lr)
  adf <- .adf_resid(u, max_lag)
  crit <- c(`1%` = -3.89644, `5%` = -3.33613, `10%` = -3.04445)

  dd <- c(NA, diff(d))
  dw <- c(NA, diff(w))
  X <- data.frame(dd = dd, dw = dw)
  for (i in seq_len(ecm_lags)) {
    X[[paste0("dd_l", i)]] <- .lagv(dd, i)
    X[[paste0("dw_l", i)]] <- .lagv(dw, i)
  }
  u1 <- .lagv(u, 1)
  if (asymmetric) {
    X$ect_pos <- pmax(u1, 0)
    X$ect_neg <- pmin(u1, 0)
  } else {
    X$ect <- u1
  }
  X <- X[(ecm_lags + 2L):n, , drop = FALSE]
  ecm <- stats::lm(dd ~ ., data = X)
  cf <- summary(ecm)$coefficients

  hl <- function(a) if (is.finite(a) && a < 0 && a > -1) log(0.5) / log(1 + a)
    else NA_real_
  if (asymmetric) {
    ap <- cf["ect_pos", "Estimate"]
    an <- cf["ect_neg", "Estimate"]
    Xr <- X
    Xr$ect <- Xr$ect_pos + Xr$ect_neg
    Xr$ect_pos <- NULL
    Xr$ect_neg <- NULL
    restricted <- stats::lm(dd ~ ., data = Xr)
    ft <- stats::anova(restricted, ecm)
    adjustment <- list(alpha_positive = ap, alpha_negative = an,
                       half_life_positive = hl(ap),
                       half_life_negative = hl(an),
                       symmetry_F = ft$F[2], symmetry_p = ft$`Pr(>F)`[2])
  } else {
    a <- cf["ect", "Estimate"]
    adjustment <- list(alpha = a, alpha_p = cf["ect", "Pr(>|t|)"],
                       half_life = hl(a))
  }
  out <- list(
    long_run = c(intercept = unname(stats::coef(lr)[1]),
                 elasticity = unname(stats::coef(lr)[2]),
                 r_squared = summary(lr)$r.squared),
    cointegration = list(statistic = adf$stat, lag = adf$lag,
                         critical_values = crit,
                         cointegrated_5pct = adf$stat < crit[["5%"]]),
    ecm = cf, short_run_elasticity = cf["dw", "Estimate"],
    adjustment = adjustment, asymmetric = asymmetric, log = log, n = n,
    models = list(long_run = lr, ecm = ecm))
  class(out) <- "price_transmission"
  out
}

#' @export
print.price_transmission <- function(x, ...) {
  cat("Price transmission analysis (", x$n, " observations",
      if (x$log) ", log prices" else "", ")\n\n", sep = "")
  cat("Long-run elasticity:   ", .fmt(x$long_run[["elasticity"]]),
      " (R2 = ", .fmt(x$long_run[["r_squared"]]), ")\n", sep = "")
  ci <- x$cointegration
  cat("Engle-Granger ADF:     ", .fmt(ci$statistic), " (lag ", ci$lag,
      "; 5% critical value ", .fmt(ci$critical_values[["5%"]]), ") -> ",
      if (ci$cointegrated_5pct) "cointegrated" else "no cointegration",
      " at 5%\n", sep = "")
  cat("Short-run elasticity:  ", .fmt(x$short_run_elasticity), "\n", sep = "")
  a <- x$adjustment
  if (x$asymmetric) {
    cat("Adjustment speed:      positive ", .fmt(a$alpha_positive),
        ", negative ", .fmt(a$alpha_negative), "\n", sep = "")
    cat("Half-life (periods):   positive ", .fmt(a$half_life_positive, 2),
        ", negative ", .fmt(a$half_life_negative, 2), "\n", sep = "")
    cat("Symmetry test:         F = ", .fmt(a$symmetry_F, 2), ", p = ",
        .fmt(a$symmetry_p), "\n", sep = "")
  } else {
    cat("Adjustment speed:      ", .fmt(a$alpha), " (p = ", .fmt(a$alpha_p),
        ")\n", sep = "")
    cat("Half-life (periods):   ", .fmt(a$half_life, 2), "\n", sep = "")
  }
  if (!ci$cointegrated_5pct) {
    cat("Note: without cointegration the error correction estimates should",
        "be interpreted with caution.\n")
  }
  invisible(x)
}
