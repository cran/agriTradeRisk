.conc_stats <- function(v, labels, top) {
  agg <- tapply(v, factor(labels, levels = unique(labels)), sum)
  agg <- agg[agg > 0]
  tot <- sum(agg)
  n <- length(agg)
  if (n == 0L || tot <= 0) {
    return(data.frame(total = tot, n_partners = n, hhi = NA_real_,
                      hhi_normalised = NA_real_, equivalent_partners = NA_real_,
                      top_share = NA_real_, entropy = NA_real_,
                      entropy_normalised = NA_real_, leading_partner = NA,
                      leading_share = NA_real_))
  }
  s <- as.numeric(agg) / tot
  hhi <- sum(s^2)
  ent <- -sum(s * log(s))
  ord <- order(s, decreasing = TRUE)
  data.frame(total = tot, n_partners = n, hhi = hhi,
             hhi_normalised = if (n > 1) (hhi - 1 / n) / (1 - 1 / n) else 1,
             equivalent_partners = 1 / hhi,
             top_share = sum(s[ord][seq_len(min(top, n))]),
             entropy = ent,
             entropy_normalised = if (n > 1) ent / log(n) else 0,
             leading_partner = names(agg)[ord[1L]],
             leading_share = s[ord[1L]], stringsAsFactors = FALSE)
}

#' Export concentration indices
#'
#' Measures how concentrated exports are across destinations (or across
#' products, by passing the product column as `partner`). For partner
#' shares \eqn{s_j} the function reports the Herfindahl-Hirschman index
#' \eqn{HHI = \sum_j s_j^2}, its normalised form
#' \eqn{(HHI - 1/n)/(1 - 1/n)}, the equivalent number of equally sized
#' partners \eqn{1/HHI}, the combined share of the `top` largest partners,
#' and Theil's entropy \eqn{-\sum_j s_j \ln s_j} with its normalised form
#' (divided by \eqn{\ln n}). Higher HHI and lower entropy indicate greater
#' concentration and therefore greater exposure to shocks in individual
#' markets.
#'
#' @param data A data frame with one row per partner (and group).
#' @param value Name of the column holding export values (non-negative).
#' @param partner Name of the column identifying destinations (or
#'   products).
#' @param by Optional names of grouping columns, for example product and
#'   year.
#' @param top Number of leading partners for the top-share measure.
#'
#' @return A data frame of class `export_concentration` with one row per
#'   group and columns `total`, `n_partners`, `hhi`, `hhi_normalised`,
#'   `equivalent_partners`, `top_share`, `entropy`, `entropy_normalised`,
#'   `leading_partner` and `leading_share`.
#'
#' @examples
#' ec <- export_concentration(hort_exports, value = "export_value",
#'                            partner = "destination",
#'                            by = c("product", "year"))
#' head(ec)
#' @export
export_concentration <- function(data, value, partner, by = NULL, top = 3) {
  .need_cols(data, c(value, partner, by))
  .chk(data[[value]], value, lower = 0, scalar = FALSE)
  .chk(top, "top", lower = 1)
  groups <- .group_split(data, by)
  rows <- lapply(groups, function(g) {
    st <- .conc_stats(g[[value]], as.character(g[[partner]]), top)
    if (!is.null(by)) st <- cbind(g[1L, by, drop = FALSE], st)
    st
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  class(out) <- c("export_concentration", "data.frame")
  out
}
