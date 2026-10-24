test_that("price transmission recovers a known cointegrated system", {
  set.seed(1)
  n <- 300
  w <- cumsum(rnorm(n, 0, 0.05))
  d <- numeric(n)
  for (t in 2:n) {
    dev <- d[t - 1] - (0.2 + 0.8 * w[t - 1])
    d[t] <- d[t - 1] + 0.4 * (w[t] - w[t - 1]) - 0.3 * dev + rnorm(1, 0, 0.02)
  }
  pt <- price_transmission(d, w, log = FALSE)
  expect_s3_class(pt, "price_transmission")
  expect_equal(pt$long_run[["elasticity"]], 0.8, tolerance = 0.05)
  expect_true(pt$cointegration$cointegrated_5pct)
  expect_equal(pt$adjustment$alpha, -0.3, tolerance = 0.2)
  expect_equal(pt$short_run_elasticity, 0.4, tolerance = 0.15)
  expect_equal(pt$adjustment$half_life, log(0.5) / log(1 + pt$adjustment$alpha))
  expect_output(print(pt), "cointegrated")
})

test_that("asymmetric ECM and input checks work", {
  pa <- price_transmission(grape_prices$domestic_price,
                           grape_prices$export_unit_value, asymmetric = TRUE)
  expect_true(all(c("alpha_positive", "alpha_negative", "symmetry_p") %in%
                    names(pa$adjustment)))
  expect_output(print(pa), "Symmetry")
  expect_error(price_transmission(1:10, 1:9))
  expect_error(price_transmission(rnorm(15), rnorm(15)))
  expect_error(price_transmission(c(-1, rep(1, 40)), rep(1, 41)))
})

test_that("exchange-rate exposure is internally consistent", {
  sh <- c(USD = 0.5, EUR = 0.3, GBP = 0.2)
  ex <- exchange_rate_exposure(fx_rates[, names(sh)], sh, export_value = 100)
  expect_equal(sum(ex$currencies$risk_contribution), 1)
  expect_gte(ex$portfolio[["diversification_ratio"]], 1)
  r <- diff(log(as.matrix(fx_rates[, names(sh)])))
  sp <- sd(r %*% sh)
  expect_equal(ex$portfolio[["volatility_annual"]], sp * sqrt(12))
  expect_equal(ex$portfolio[["var_parametric"]], qnorm(0.95) * sp * 100)
  expect_equal(length(ex$effective_index), nrow(fx_rates))
  # single pegged currency gives same volatility as the anchor
  e2 <- exchange_rate_exposure(fx_rates[, c("USD", "AED")],
                               c(USD = 1, AED = 0))
  expect_equal(e2$portfolio[["diversification_ratio"]], 1)
  expo <- exp(0.8 * log(fx_rates$USD) + rnorm(nrow(fx_rates), 0, 0.001))
  e3 <- exchange_rate_exposure(fx_rates["USD"], c(USD = 1), exports = expo)
  expect_equal(e3$elasticity[["estimate"]], 0.8, tolerance = 0.1)
  expect_output(print(e3), "elasticity")
  expect_error(exchange_rate_exposure(fx_rates, c(XYZ = 1)))
})
