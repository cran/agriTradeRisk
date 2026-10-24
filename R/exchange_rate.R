#' Exchange-rate exposure of a multi-currency export portfolio
#'
#' Treats export earnings invoiced in several currencies as a currency
#' portfolio. Using log changes of exchange rates (home currency per unit of
#' foreign currency) and the share of export value earned in each currency,
#' it computes:
#' \itemize{
#'   \item annualised volatility of each currency and their correlations;
#'   \item a trade-weighted (geometric) effective exchange-rate index;
#'   \item portfolio volatility \eqn{\sigma_p = \sqrt{s'\Sigma s}} and the
#'     undiversified volatility \eqn{\sum_i s_i \sigma_i}; their ratio is
#'     the diversification ratio (values above 1 indicate risk reduction
#'     from currency diversification);
#'   \item each currency's percentage contribution to portfolio variance,
#'     \eqn{s_i(\Sigma s)_i / \sigma_p^2};
#'   \item parametric and historical value at risk of export earnings in
#'     home currency over `horizon` periods at confidence level `conf`.
#' }
#' If an export series is supplied, the exposure elasticity is estimated by
#' regressing its log change on the log change of the effective exchange
#' rate.
#'
#' @param rates A data frame or matrix of exchange rates, one column per
#'   currency (home currency per unit of foreign currency), one row per
#'   period, in time order.
#' @param shares Named numeric vector of export shares by currency; names
#'   must match columns of `rates`. Normalised to sum to 1.
#' @param export_value Export value (home currency) exposed over the
#'   horizon, used for value at risk.
#' @param freq Periods per year (12 for monthly data) for annualisation.
#' @param horizon Value-at-risk horizon in periods.
#' @param conf Confidence level for value at risk.
#' @param exports Optional numeric series of export values or prices (same
#'   length as `rates`) for estimating the exposure elasticity.
#'
#' @return A list of class `exchange_rate_exposure` with `currencies`
#'   (share, annualised volatility and risk contribution per currency),
#'   `correlation`, `portfolio` (annualised portfolio and undiversified
#'   volatility, diversification ratio, parametric and historical value at
#'   risk), `effective_index` (trade-weighted index, first period = 100)
#'   and, if `exports` is supplied, `elasticity` (estimate, standard error,
#'   p-value).
#'
#' @examples
#' sh <- c(USD = 0.35, EUR = 0.20, GBP = 0.15, AED = 0.20, BDT = 0.10)
#' ex <- exchange_rate_exposure(fx_rates[, names(sh)], sh,
#'                              export_value = 1e9, freq = 12)
#' ex
#' @export
exchange_rate_exposure <- function(rates, shares, export_value = 1,
                                   freq = 12, horizon = 1, conf = 0.95,
                                   exports = NULL) {
  rates <- as.data.frame(rates)
  if (is.null(names(shares)) || !all(names(shares) %in% names(rates))) {
    stop("`shares` must be named with columns of `rates`.", call. = FALSE)
  }
  .chk(shares, "shares", lower = 0, scalar = FALSE)
  if (sum(shares) <= 0) stop("`shares` must not all be zero.", call. = FALSE)
  .chk(export_value, "export_value", lower = 0)
  .chk(freq, "freq", lower = 1e-9)
  .chk(horizon, "horizon", lower = 1e-9)
  .chk(conf, "conf", 0.5, 0.9999)
  s <- shares / sum(shares)
  R <- as.matrix(rates[, names(s), drop = FALSE])
  if (!is.numeric(R) || anyNA(R) || any(R <= 0)) {
    stop("Exchange rates must be positive and non-missing.", call. = FALSE)
  }
  if (nrow(R) < 13L) stop("At least 13 periods of rates are required.",
                          call. = FALSE)
  ret <- diff(log(R))
  S <- stats::cov(ret)
  sig <- sqrt(diag(S))
  port <- as.numeric(ret %*% s)
  sp <- sqrt(as.numeric(t(s) %*% S %*% s))
  undiv <- sum(s * sig)
  contrib <- as.numeric(s * (S %*% s)) / sp^2
  z <- stats::qnorm(conf)
  var_param <- z * sp * sqrt(horizon) * export_value
  h <- max(1L, as.integer(round(horizon)))
  agg <- if (h == 1L) port else
    stats::filter(port, rep(1, h), sides = 1)[h:length(port)]
  var_hist <- -stats::quantile(agg, 1 - conf, names = FALSE) * export_value
  idx <- 100 * exp(c(0, cumsum(port)))

  out <- list(
    currencies = data.frame(currency = names(s), share = as.numeric(s),
                            volatility_annual = sig * sqrt(freq),
                            risk_contribution = contrib, row.names = NULL),
    correlation = stats::cor(ret),
    portfolio = c(volatility_annual = sp * sqrt(freq),
                  undiversified_volatility_annual = undiv * sqrt(freq),
                  diversification_ratio = if (sp > 0) undiv / sp else NA,
                  var_parametric = var_param,
                  var_historical = max(var_hist, 0)),
    effective_index = idx, conf = conf, horizon = horizon)
  if (!is.null(exports)) {
    e <- as.numeric(exports)
    if (length(e) != nrow(R) || anyNA(e) || any(e <= 0)) {
      stop("`exports` must be positive, non-missing and one per period.",
           call. = FALSE)
    }
    fit <- stats::lm(diff(log(e)) ~ port)
    cf <- summary(fit)$coefficients
    out$elasticity <- c(estimate = cf["port", "Estimate"],
                        std_error = cf["port", "Std. Error"],
                        p_value = cf["port", "Pr(>|t|)"])
  }
  class(out) <- "exchange_rate_exposure"
  out
}

#' @export
print.exchange_rate_exposure <- function(x, ...) {
  cat("Exchange-rate exposure of export earnings\n\n")
  tab <- x$currencies
  tab$share <- .fmt(tab$share)
  tab$volatility_annual <- .fmt(tab$volatility_annual)
  tab$risk_contribution <- paste0(.fmt(100 * x$currencies$risk_contribution,
                                       1), "%")
  print(tab, row.names = FALSE)
  p <- x$portfolio
  cat("\nPortfolio volatility (annual): ", .fmt(p[["volatility_annual"]]),
      "\nUndiversified volatility:      ",
      .fmt(p[["undiversified_volatility_annual"]]),
      "\nDiversification ratio:         ", .fmt(p[["diversification_ratio"]]),
      "\nValue at risk (", 100 * x$conf, "%, ", x$horizon, " period):",
      " parametric ", .fmt(p[["var_parametric"]], 0), ", historical ",
      .fmt(p[["var_historical"]], 0), "\n", sep = "")
  if (!is.null(x$elasticity)) {
    cat("Exposure elasticity:           ", .fmt(x$elasticity[["estimate"]]),
        " (p = ", .fmt(x$elasticity[["p_value"]]), ")\n", sep = "")
  }
  invisible(x)
}
