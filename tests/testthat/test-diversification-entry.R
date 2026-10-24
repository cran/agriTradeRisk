test_that("minimum-variance solution matches the analytic answer", {
  S <- matrix(c(0.04, 0.006, 0.006, 0.09), 2)
  mu <- c(a = 0.05, b = 0.08)
  md <- market_diversification(mean = mu, cov = S)
  # analytic 2-asset minimum variance weight
  wa <- (S[2, 2] - S[1, 2]) / (S[1, 1] + S[2, 2] - 2 * S[1, 2])
  expect_equal(md$weights$minimum_variance[1], wa, tolerance = 1e-6)
  expect_equal(sum(md$weights$best_return_to_risk), 1, tolerance = 1e-9)
})

test_that("bounds are respected and frontier reaches max return", {
  set.seed(3)
  r <- matrix(rnorm(200, c(0.02, 0.05, 0.08, 0.03), c(.05, .1, .2, .04)),
              50, 4, byrow = TRUE, dimnames = list(NULL, letters[1:4]))
  md <- market_diversification(r, shares = c(a = .4, b = .3, c = .2, d = .1),
                               min_share = 0.05, max_share = 0.6)
  w <- as.matrix(md$frontier[, letters[1:4]])
  expect_true(all(w >= 0.05 - 1e-9 & w <= 0.6 + 1e-9))
  expect_equal(unname(rowSums(w)), rep(1, nrow(w)), tolerance = 1e-9)
  mu <- md$mean
  o <- order(mu, decreasing = TRUE)
  best <- 0.6 * mu[o[1]] + 0.3 * mu[o[2]] + 0.05 * sum(mu[o[3:4]])
  expect_equal(max(md$frontier$return), unname(best), tolerance = 1e-4)
  expect_true(all(c("current", "minimum_variance") %in% rownames(md$summary)))
  expect_lte(md$summary["minimum_variance", "risk"],
             md$summary["current", "risk"])
  expect_output(print(md), "Shares")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_invisible(plot(md))
  expect_error(market_diversification(r, max_share = 0.2))
  expect_error(market_diversification())
})

test_that("entry risk scores are scaled and ranked", {
  mk <- data.frame(m = c("A", "B", "C"), x = c(1, 2, 3), y = c(10, 5, 0),
                   g = c(0.1, 0.2, 0.3))
  er <- market_entry_risk(mk, "m", c("x", "y"), c(TRUE, FALSE),
                          weights = c(1, 1), attractiveness = "g",
                          risk_aversion = 1)
  expect_equal(er$x_scaled, c(0, 0.5, 1))
  expect_equal(er$y_scaled, c(0, 0.5, 1))
  expect_equal(er$risk_score, c(0, 0.5, 1))
  expect_equal(er$risk_rank, 1:3)
  expect_equal(er$risk_adjusted_attractiveness, c(0, 0.25, 0))
  expect_error(market_entry_risk(mk, "m", c("x", "y"), TRUE))
  expect_error(market_entry_risk(mk, "m", "x", TRUE, weights = -1))
})
