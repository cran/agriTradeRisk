#' Composite market-entry risk score
#'
#' Combines indicators of the risk of entering or expanding in destination
#' markets, for example sanitary and phytosanitary rejection rates, tariff
#' rates, non-tariff measure counts, price volatility, exchange-rate
#' volatility, distance or transit time, and competitor market share, into
#' a single score. Each indicator is rescaled to 0-1 by min-max
#' normalisation and oriented so that 1 is the riskiest value; the score is
#' the weighted mean of the rescaled indicators. Optionally, a market
#' attractiveness indicator (such as import demand growth) is combined with
#' the score into a risk-adjusted attractiveness index
#' \eqn{A (1 - \rho \, risk)}, where \eqn{A} is rescaled attractiveness and
#' \eqn{\rho} the risk aversion.
#'
#' @param data A data frame with one row per market.
#' @param market Name of the market column.
#' @param indicators Names of indicator columns.
#' @param higher_is_riskier Logical vector, one per indicator: `TRUE` if a
#'   higher value means more risk.
#' @param weights Optional non-negative weights, one per indicator
#'   (normalised to sum to 1). Equal weights by default.
#' @param attractiveness Optional name of a column measuring market
#'   attractiveness (higher is better).
#' @param risk_aversion Weight \eqn{\rho} (0 to 1) of risk in the
#'   risk-adjusted attractiveness index.
#'
#' @return A data frame of class `market_entry_risk` with the market, the
#'   rescaled indicators, `risk_score`, `risk_rank` (1 = least risky) and,
#'   if `attractiveness` is given, `attractiveness_scaled`,
#'   `risk_adjusted_attractiveness` and `priority_rank`.
#'
#' @examples
#' mk <- data.frame(market = c("UAE", "UK", "Netherlands", "USA",
#'                             "Bangladesh"),
#'                  sps_rejections = c(2, 5, 8, 6, 1),
#'                  tariff = c(0, 0, 0, 2, 10),
#'                  price_cv = c(0.15, 0.10, 0.12, 0.18, 0.25),
#'                  demand_growth = c(0.06, 0.03, 0.04, 0.05, 0.08))
#' market_entry_risk(mk, "market",
#'                   c("sps_rejections", "tariff", "price_cv"),
#'                   higher_is_riskier = c(TRUE, TRUE, TRUE),
#'                   attractiveness = "demand_growth")
#' @export
market_entry_risk <- function(data, market, indicators, higher_is_riskier,
                              weights = NULL, attractiveness = NULL,
                              risk_aversion = 0.5) {
  .need_cols(data, c(market, indicators, attractiveness))
  k <- length(indicators)
  if (length(higher_is_riskier) != k || !is.logical(higher_is_riskier) ||
      anyNA(higher_is_riskier)) {
    stop("`higher_is_riskier` must be TRUE/FALSE, one per indicator.",
         call. = FALSE)
  }
  if (is.null(weights)) weights <- rep(1, k)
  .chk(weights, "weights", lower = 0, scalar = FALSE)
  if (length(weights) != k || sum(weights) <= 0) {
    stop("`weights` must be non-negative, one per indicator, not all zero.",
         call. = FALSE)
  }
  .chk(risk_aversion, "risk_aversion", 0, 1)
  wts <- weights / sum(weights)
  scale01 <- function(v) {
    r <- range(v)
    if (diff(r) == 0) rep(0.5, length(v)) else (v - r[1]) / diff(r)
  }
  out <- data.frame(market = data[[market]], stringsAsFactors = FALSE)
  M <- matrix(NA_real_, nrow(data), k)
  for (i in seq_len(k)) {
    v <- data[[indicators[i]]]
    .chk(v, indicators[i], scalar = FALSE)
    s <- scale01(v)
    if (!higher_is_riskier[i]) s <- 1 - s
    M[, i] <- s
    out[[paste0(indicators[i], "_scaled")]] <- s
  }
  out$risk_score <- as.numeric(M %*% wts)
  out$risk_rank <- rank(out$risk_score, ties.method = "min")
  if (!is.null(attractiveness)) {
    a <- data[[attractiveness]]
    .chk(a, attractiveness, scalar = FALSE)
    out$attractiveness_scaled <- scale01(a)
    out$risk_adjusted_attractiveness <- out$attractiveness_scaled *
      (1 - risk_aversion * out$risk_score)
    out$priority_rank <- rank(-out$risk_adjusted_attractiveness,
                              ties.method = "min")
  }
  class(out) <- c("market_entry_risk", "data.frame")
  out
}
