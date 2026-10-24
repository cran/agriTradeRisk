#' Revealed comparative advantage and export competitiveness
#'
#' Computes standard competitiveness indicators for a country's exports of
#' product \eqn{k}:
#' \itemize{
#'   \item Balassa revealed comparative advantage
#'     \eqn{RCA = (x_k / X) / (w_k / W)}, where \eqn{x_k} and \eqn{X} are the
#'     country's exports of the product and of all goods, and \eqn{w_k} and
#'     \eqn{W} the corresponding world exports. Values above 1 indicate a
#'     revealed comparative advantage.
#'   \item Revealed symmetric comparative advantage
#'     \eqn{RSCA = (RCA - 1)/(RCA + 1)}, bounded between -1 and 1.
#'   \item World market share \eqn{x_k / w_k}.
#'   \item If quantities are supplied, the export unit value, the world unit
#'     value and their ratio, a price-based competitiveness (or quality)
#'     indicator.
#' }
#'
#' @param data A data frame, one row per product (and year).
#' @param export Column with the country's exports of the product.
#' @param total_export Column with the country's total exports.
#' @param world_export Column with world exports of the product.
#' @param world_total Column with world total exports.
#' @param quantity,world_quantity Optional columns with the country's and
#'   the world's export quantities of the product.
#'
#' @return `data` with added columns `rca`, `rsca`, `world_share` and,
#'   when quantities are given, `unit_value`, `world_unit_value` and
#'   `unit_value_ratio`. The result has class `competitiveness_index`.
#'
#' @references Balassa, B. (1965). Trade liberalisation and "revealed"
#'   comparative advantage. *The Manchester School*, 33(2), 99-123.
#'   \doi{10.1111/j.1467-9957.1965.tb00050.x}
#'
#' @examples
#' prod_year <- aggregate(cbind(export_value, world_import) ~ product + year,
#'                        data = hort_exports, FUN = sum)
#' dat <- merge(prod_year, trade_totals, by = "year")
#' ci <- competitiveness_index(dat, "export_value", "india_total_exports",
#'                             "world_import", "world_total_exports")
#' head(ci[, c("product", "year", "rca", "rsca", "world_share")])
#' @export
competitiveness_index <- function(data, export, total_export, world_export,
                                  world_total, quantity = NULL,
                                  world_quantity = NULL) {
  cols <- c(export, total_export, world_export, world_total)
  .need_cols(data, cols)
  .chk(data[[export]], export, lower = 0, scalar = FALSE)
  for (cc in cols[-1]) .chk(data[[cc]], cc, lower = 1e-300, scalar = FALSE)
  x <- data[[export]]
  rca <- (x / data[[total_export]]) / (data[[world_export]] / data[[world_total]])
  data$rca <- rca
  data$rsca <- (rca - 1) / (rca + 1)
  data$world_share <- x / data[[world_export]]
  if (!is.null(quantity) || !is.null(world_quantity)) {
    if (is.null(quantity) || is.null(world_quantity)) {
      stop("Supply both `quantity` and `world_quantity`.", call. = FALSE)
    }
    .need_cols(data, c(quantity, world_quantity))
    q <- data[[quantity]]
    wq <- data[[world_quantity]]
    data$unit_value <- ifelse(q > 0, x / q, NA_real_)
    data$world_unit_value <- ifelse(wq > 0, data[[world_export]] / wq,
                                    NA_real_)
    data$unit_value_ratio <- data$unit_value / data$world_unit_value
  }
  class(data) <- c("competitiveness_index", "data.frame")
  data
}

#' Constant market share decomposition of export growth
#'
#' Decomposes the change in a country's exports between two periods into
#' four effects (Leamer and Stern, 1970; Ahmadi-Esfahani, 2006):
#' \deqn{\Delta X = rX^0 + \sum_k (r_k - r)X_k^0 +
#'   \sum_k\sum_j (r_{kj} - r_k)X_{kj}^0 +
#'   \sum_k\sum_j (X_{kj}^1 - X_{kj}^0 - r_{kj}X_{kj}^0)}
#' \describe{
#'   \item{world growth}{exports would have grown at the rate \eqn{r} of
#'     world imports covered by the data;}
#'   \item{commodity composition}{gain from specialising in products whose
#'     world demand grew faster than average;}
#'   \item{market distribution}{gain from exporting each product to markets
#'     where its demand grew faster;}
#'   \item{competitiveness}{the residual, the change in market shares.}
#' }
#' \eqn{r_{kj}}, \eqn{r_k} and \eqn{r} are growth rates of world imports of
#' product \eqn{k} in market \eqn{j}, of product \eqn{k} in all markets, and
#' of all products in all markets in the data.
#'
#' @param data A data frame with one row per product-market pair.
#' @param product,market Names of product and market columns.
#' @param export0,export1 Columns with the country's exports in the base
#'   and end periods.
#' @param world0,world1 Columns with world imports of the product by the
#'   market in the base and end periods (must be positive).
#'
#' @return A list of class `cms_decomposition` with `effects` (data frame
#'   of the four effects, their total and their shares of the export
#'   change), `by_product` (effects by product) and `growth` (world growth
#'   rates).
#'
#' @references Leamer, E. E. and Stern, R. M. (1970). *Quantitative
#'   International Economics*. Allyn and Bacon, Boston.
#'
#'   Ahmadi-Esfahani, F. Z. (2006). Constant market shares analysis:
#'   uses, limitations and prospects. *Australian Journal of Agricultural
#'   and Resource Economics*, 50(4), 510-526.
#'
#' @examples
#' d0 <- subset(hort_exports, year == 2015)
#' d1 <- subset(hort_exports, year == 2023)
#' cms <- merge(d0, d1, by = c("product", "destination"),
#'              suffixes = c("0", "1"))
#' cms_decomposition(cms, "product", "destination", "export_value0",
#'                   "export_value1", "world_import0", "world_import1")
#' @export
cms_decomposition <- function(data, product, market, export0, export1,
                              world0, world1) {
  .need_cols(data, c(product, market, export0, export1, world0, world1))
  .chk(data[[export0]], export0, lower = 0, scalar = FALSE)
  .chk(data[[export1]], export1, lower = 0, scalar = FALSE)
  .chk(data[[world0]], world0, lower = 1e-300, scalar = FALSE)
  .chk(data[[world1]], world1, lower = 0, scalar = FALSE)
  if (anyDuplicated(data[c(product, market)])) {
    stop("Each product-market pair must appear once.", call. = FALSE)
  }
  x0 <- data[[export0]]
  x1 <- data[[export1]]
  w0 <- data[[world0]]
  w1 <- data[[world1]]
  k <- as.character(data[[product]])
  r <- sum(w1) / sum(w0) - 1
  rk_tab <- tapply(w1, k, sum) / tapply(w0, k, sum) - 1
  rk <- as.numeric(rk_tab[k])
  rkj <- w1 / w0 - 1

  world <- r * x0
  commodity <- (rk - r) * x0
  market_eff <- (rkj - rk) * x0
  compet <- x1 - x0 - rkj * x0
  dX <- sum(x1 - x0)
  eff <- c(world_growth = sum(world), commodity_composition = sum(commodity),
           market_distribution = sum(market_eff),
           competitiveness = sum(compet))
  effects <- data.frame(effect = c(names(eff), "total_change"),
                        value = c(eff, dX),
                        share_of_change = if (dX != 0) c(eff, dX) / dX else NA,
                        row.names = NULL)
  bp <- data.frame(product = k, world_growth = world,
                   commodity_composition = commodity,
                   market_distribution = market_eff,
                   competitiveness = compet, total_change = x1 - x0)
  by_product <- stats::aggregate(. ~ product, data = bp, FUN = sum)
  out <- list(effects = effects, by_product = by_product,
              growth = c(world = r, rk_tab))
  class(out) <- "cms_decomposition"
  out
}

#' @export
print.cms_decomposition <- function(x, ...) {
  cat("Constant market share decomposition of export change\n\n")
  tab <- x$effects
  tab$value <- .fmt(tab$value, 2)
  tab$share_of_change <- paste0(.fmt(100 * x$effects$share_of_change, 1), "%")
  print(tab, row.names = FALSE)
  cat("\nGrowth of world imports in the data: ",
      .fmt(100 * x$growth[["world"]], 1), "%\n", sep = "")
  invisible(x)
}
