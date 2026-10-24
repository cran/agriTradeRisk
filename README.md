# agriTradeRisk

Agricultural export competitiveness and risk: an integrated, reproducible
workflow for horticultural export risk and market diversification.

## Installation

```r
install.packages("agriTradeRisk")   # once accepted on CRAN
```

## Functions

| Function | Purpose |
|---|---|
| `export_concentration()` | HHI, entropy, equivalent markets, top-k share |
| `competitiveness_index()` | Balassa RCA, RSCA, world share, relative unit value |
| `cms_decomposition()` | Constant market share decomposition of export growth |
| `price_transmission()` | Engle-Granger cointegration and (asymmetric) ECM |
| `exchange_rate_exposure()` | Currency portfolio volatility, risk contributions, VaR |
| `market_entry_risk()` | Composite entry-risk and risk-adjusted attractiveness |
| `market_diversification()` | Mean-variance efficient destination mix |

Example data (`hort_exports`, `trade_totals`, `fx_rates`, `grape_prices`)
are simulated for Indian mango, grapes, pomegranate and processed products
and are not official statistics.
