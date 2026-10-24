test_that("concentration indices match hand calculation", {
  d <- data.frame(dest = c("a", "b", "c", "a"), v = c(30, 20, 10, 40))
  ec <- export_concentration(d, "v", "dest", top = 1)
  s <- c(70, 20, 10) / 100
  expect_equal(ec$hhi, sum(s^2))
  expect_equal(ec$equivalent_partners, 1 / sum(s^2))
  expect_equal(ec$top_share, 0.7)
  expect_equal(ec$entropy, -sum(s * log(s)))
  expect_equal(ec$leading_partner, "a")
  expect_equal(ec$hhi_normalised, (sum(s^2) - 1 / 3) / (1 - 1 / 3))
})

test_that("concentration bounds and grouping", {
  eq <- export_concentration(data.frame(p = letters[1:4], v = 5), "v", "p")
  expect_equal(eq$hhi_normalised, 0)
  expect_equal(eq$entropy_normalised, 1)
  ec <- export_concentration(hort_exports, "export_value", "destination",
                             by = c("product", "year"))
  expect_equal(nrow(ec), 4 * 14)
  expect_true(all(ec$hhi > 0 & ec$hhi <= 1))
  expect_error(export_concentration(hort_exports, "nope", "destination"))
  expect_error(export_concentration(data.frame(p = "a", v = -1), "v", "p"))
})

test_that("RCA and RSCA are computed correctly", {
  d <- data.frame(x = 10, X = 100, w = 50, W = 1000, q = 5, wq = 40)
  ci <- competitiveness_index(d, "x", "X", "w", "W", "q", "wq")
  expect_equal(ci$rca, (10 / 100) / (50 / 1000))
  expect_equal(ci$rsca, (2 - 1) / (2 + 1))
  expect_equal(ci$world_share, 0.2)
  expect_equal(ci$unit_value_ratio, (10 / 5) / (50 / 40))
  expect_error(competitiveness_index(d, "x", "X", "w", "W", "q"))
})

test_that("CMS effects sum to the export change", {
  d0 <- subset(hort_exports, year == 2015)
  d1 <- subset(hort_exports, year == 2023)
  m <- merge(d0, d1, by = c("product", "destination"), suffixes = c("0", "1"))
  cms <- cms_decomposition(m, "product", "destination", "export_value0",
                           "export_value1", "world_import0", "world_import1")
  e <- cms$effects
  expect_equal(sum(e$value[1:4]), e$value[5])
  expect_equal(e$value[5], sum(m$export_value1 - m$export_value0))
  expect_equal(sum(cms$by_product$total_change), e$value[5])
  # constant market shares imply zero competitiveness effect
  m2 <- m
  m2$export_value1 <- m2$export_value0 * m2$world_import1 / m2$world_import0
  c2 <- cms_decomposition(m2, "product", "destination", "export_value0",
                          "export_value1", "world_import0", "world_import1")
  expect_equal(c2$effects$value[4], 0, tolerance = 1e-8)
  expect_output(print(cms), "competitiveness")
  expect_error(cms_decomposition(rbind(m, m[1, ]), "product", "destination",
                                 "export_value0", "export_value1",
                                 "world_import0", "world_import1"))
})
