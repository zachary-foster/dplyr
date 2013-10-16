# Joins
#
# Joins need to match on common dimensions and duplicate as necessary
# good example would be nasa data + elevation - elevation should be
# duplicated for every year and month
# 
# Fail if dimensions don't match?
# 
# Implementation? Maybe something using expand.grid. Or maybe straightforward
# to generate join keys like for data frames? Should be easier since can produce
# id even more easily.

#' @examples
#' by_loc <- group_by(nasa, lat, long)
#' temp <- summarise(by_loc, avg_temp = mean(temperature))
#' inner_join(by_loc, temp)
#' @rdname join.tbl_array
NULL

#' @method inner_join tbl_array
#' @export
#' @rdname join.tbl_array
inner_join.tbl_array <- function(x, y, by = NULL, copy = FALSE, ...) {
  by <- by %||% common_by(x, y)
  y <- auto_copy(x, y, copy = copy)
  
  keys <- join_keys(x, y, by = by)
  x.cols <- setdiff(names(x), by)
  y.cols <- setdiff(names(y), by)
  
  ids <- join_ids(keys)
  out <- cbind(
    x[ids$x,       , drop = FALSE], 
    y[ids$y, y.cols, drop = FALSE]
  )
  attr(out, "row.names") <- .set_row_names(nrow(out))
  out
}

join_keys <- function(x, y, by = NULL) {
  by <- by %||% common_by(x, y)
  
  joint <- rbind(x[by], y[by])
  keys <- id(joint, drop = TRUE)
  
  n_x <- nrow(x)
  n_y <- nrow(y)
  
  list(
    x = keys[seq_len(n_x)],
    y = keys[n_x + seq_len(n_y)],
    n = attr(keys, "n")
  )
}
