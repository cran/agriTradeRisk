## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 6,
                      fig.height = 4)

## -----------------------------------------------------------------------------
library(agriTradeRisk)

## -----------------------------------------------------------------------------
head(hort_exports)

## -----------------------------------------------------------------------------
ec <- export_concentration(hort_exports, value = "export_value",
                           partner = "destination",
                           by = c("product", "year"))
subset(ec, year %in% c(2010, 2023),
       c(product, year, hhi, equivalent_partners, top_share,
         leading_partner, leading_share))

## -----------------------------------------------------------------------------
py <- aggregate(cbind(export_value, world_import) ~ product + year,
                data = hort_exports, FUN = sum)
ci <- competitiveness_index(merge(py, trade_totals, by = "year"),
                            "export_value", "india_total_exports",
                            "world_import", "world_total_exports")
xtabs(round(rca, 2) ~ year + product, data = subset(ci, year >= 2019))

## -----------------------------------------------------------------------------
d0 <- subset(hort_exports, year == 2015)
d1 <- subset(hort_exports, year == 2023)
m <- merge(d0, d1, by = c("product", "destination"), suffixes = c("0", "1"))
cms <- cms_decomposition(m, "product", "destination", "export_value0",
                         "export_value1", "world_import0", "world_import1")
cms
cms$by_product

## -----------------------------------------------------------------------------
pt <- price_transmission(grape_prices$domestic_price,
                         grape_prices$export_unit_value)
pt
pa <- price_transmission(grape_prices$domestic_price,
                         grape_prices$export_unit_value, asymmetric = TRUE)
pa

## -----------------------------------------------------------------------------
sh <- c(USD = 0.35, EUR = 0.20, GBP = 0.15, AED = 0.15, SAR = 0.05,
        BDT = 0.05, RUB = 0.05)
fx <- exchange_rate_exposure(fx_rates[, names(sh)], sh,
                             export_value = 5000, freq = 12)
fx
plot(fx_rates$date, fx$effective_index, type = "l",
     xlab = "", ylab = "Export-weighted rupee index (2014 = 100)")

## -----------------------------------------------------------------------------
mk <- data.frame(
  market = c("UAE", "Saudi Arabia", "UK", "Netherlands", "USA",
             "Bangladesh", "Nepal", "Russia"),
  sps_rejections = c(2, 3, 5, 8, 6, 1, 1, 4),
  tariff = c(0, 0, 0, 0, 2, 10, 5, 7),
  transit_days = c(4, 6, 18, 20, 30, 3, 2, 25),
  fx_volatility = c(0.04, 0.04, 0.07, 0.07, 0.04, 0.05, 0.01, 0.19),
  demand_growth = c(0.06, 0.05, 0.03, 0.04, 0.05, 0.08, 0.07, 0.02))
er <- market_entry_risk(mk, "market",
                        c("sps_rejections", "tariff", "transit_days",
                          "fx_volatility"),
                        higher_is_riskier = rep(TRUE, 4),
                        attractiveness = "demand_growth")
er[order(er$priority_rank),
   c("market", "risk_score", "risk_adjusted_attractiveness",
     "priority_rank")]

## -----------------------------------------------------------------------------
g <- subset(hort_exports, product == "grapes")
wide <- reshape(g[, c("year", "destination", "export_value")],
                idvar = "year", timevar = "destination", direction = "wide")
vals <- as.matrix(wide[order(wide$year), -1])
colnames(vals) <- sub("export_value.", "", colnames(vals))
growth <- diff(log(vals))
current <- vals[nrow(vals), ] / sum(vals[nrow(vals), ])
md <- market_diversification(growth, shares = current, max_share = 0.35)
md
plot(md)

