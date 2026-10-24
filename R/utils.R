.chk <- function(x, name, lower = -Inf, upper = Inf, scalar = TRUE) {
  if (!is.numeric(x) || length(x) == 0L || anyNA(x) || any(!is.finite(x))) {
    stop(sprintf("`%s` must be finite numeric with no missing values.", name),
         call. = FALSE)
  }
  if (scalar && length(x) != 1L) {
    stop(sprintf("`%s` must be a single number.", name), call. = FALSE)
  }
  if (any(x < lower) || any(x > upper)) {
    stop(sprintf("`%s` must lie between %s and %s.", name,
                 format(lower), format(upper)), call. = FALSE)
  }
  invisible(x)
}

.need_cols <- function(data, cols) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  miss <- setdiff(cols, names(data))
  if (length(miss)) {
    stop("Column(s) not found in `data`: ", paste(miss, collapse = ", "),
         call. = FALSE)
  }
  invisible(TRUE)
}

.group_split <- function(data, by) {
  if (is.null(by) || length(by) == 0L) return(list(all = data))
  key <- interaction(data[by], drop = TRUE, lex.order = TRUE, sep = "\r")
  split(data, key)
}

.fmt <- function(x, digits = 3) {
  out <- formatC(x, format = "f", digits = digits, big.mark = ",")
  out[is.na(x)] <- "NA"
  out
}
