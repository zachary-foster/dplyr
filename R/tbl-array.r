#' A tbl based on an array
#' 
#' This dense data representation. Useful for highly crossed data. This is an
#' experimental interface and little performance optimisation has been done,
#' but you may find it useful as a way of conserving memory.
#' 
#' @export
#' @param dimensions a named list of vectors
#' @param measurs a named list of arrays. The dimension of each array should
#'   be the same as the length of the dimensions.
#' @seealso \code{\link{as.tbl_array}} for ways of coercing existing data
#'   structures into a \code{tbl_array}.
#' @examples
#' nasa
#' head(as.data.frame(nasa))
#' 
#' titanic <- as.tbl_array(Titanic)
#' head(as.data.frame(titanic))
#' 
#' admit <- as.tbl_array(UCBAdmissions)
#' head(as.data.frame(admit))
#' 
#' as.tbl_array(esoph, dim_names = 1:3)
#' 
#' # Some manipulation examples with the NASA dataset --------------------------
#' 
#' # select() operates only on measures: it doesn't affect dimensions in any way
#' select(nasa, cloudhigh:cloudmid)
#' 
#' # filter() operates only on dimensions
#' filter(nasa, lat > 0, year == 2000)
#' # Each component can only refer to one dimensions, ensuring that you always 
#' # create a rectangular subset
#' \dontrun{filter(nasa, lat > long)}
#'
#' # Arrange is meaningless for tbl_arrays
#' 
#' by_loc <- group_by(nasa, lat, long)
#' summarise(by_loc, pressure = max(pressure), temp = mean(temperature))
tbl_array <- function(dimensions, measures) {
  if (!is.list(dimensions) || any_apply(dimensions, Negate(is.atomic)) || 
      is.null(names(dimensions))) {
    stop("Dimensions must be a named list of vectors", call. = FALSE)
  }

  if (!is.list(measures) || any_apply(measures, Negate(is.array)) ||
    is.null(names(measures))) {
    stop("Measures must be a named list of arrays", call. = FALSE)
  }
  
  # Check measures have correct dimensions
  dims <- vapply(dimensions, length, integer(1), USE.NAMES = FALSE)
  dims_ok <- vapply(measures, function(x) identical(unname(dim(x)), dims), 
    logical(1))
  if (any(!dims_ok)) {
    bad <- names(measures)[!dims_ok]
    stop("Measures ", paste0(bad, collapse = ", "), " don't have correct ",
      "dimensions (", paste0(dims, collapse = " x "), ")", call. = FALSE)
  }
  
  structure(list(dims = dimensions, mets = measures), class = "tbl_array")
}

#' @S3method tbl_vars tbl_array
tbl_vars.tbl_array <- function(x) names(x$labels)

#' @S3method dim tbl_array
dim.tbl_array <- function(x) {
  c(length(x$mets[[1]]), length(x$dim))
}

#' @S3method print tbl_array
print.tbl_array <- function(x) {
  cat("Source: local array ", dim_desc(x), "\n",
    sep = "")
  if (!is.null(x$group)) {
    cat("Grouped by: ", paste(names(x$dims)[x$group], collapse = ", "), 
      "\n", sep = "")
  }
  
  # Dimensions
  types <- vapply(x$dims, type_sum, character(1))
  lengths <- vapply(x$dims, length, integer(1))
  vars <- paste0("D: ", names(x$dims), " [", types, ", ", lengths, "]")
  cat(vars, sep = "\n")
  
  # Measures
  types <- vapply(x$mets, type_sum, character(1))
  vars <- paste0("M: ", names(x$mets), " [", types, "]")
  cat(vars, sep = "\n")
}

#' @S3method as.data.frame tbl_array
as.data.frame.tbl_array <- function(x) {
  dims <- expand.grid(x$dims, KEEP.OUT.ATTRS = FALSE)
  mets <- lapply(x$mets, as.vector)
  
  all <- c(dims, mets)
  class(all) <- "data.frame"
  attr(all, "row.names") <- .set_row_names(nrow(dims))
  
  all
}

# Coercion methods -------------------------------------------------------------

#' Coerce an existing data structure into a \code{tbl_array}
#' 
#' @export
as.tbl_array <- function(x, ...) UseMethod("as.tbl_array")
 
#' @method as.tbl_array array
#' @export
#' @rdname as.tbl_array
as.tbl_array.array <- function(x, met_name = deparse(substitute(x)), 
                               dim_names = names(dimnames(x)), ...) {
  force(met_name)
  
  dims <- dimnames(x)
  dims <- lapply(dims, type.convert, as.is = TRUE)
  
  if (is.table(x)) {
    class(x) <- setdiff(class(x), "table")
  }
  mets <- setNames(list(undimname(x)), met_name)
  
  tbl_array(dims, mets)
}

undimname <- function(x) {
  dimnames(x) <- NULL
  x
}

#' @method as.tbl_array table
#' @export
#' @rdname as.tbl_array
as.tbl_array.table <- as.tbl_array.array

#' @method as.tbl_array data.frame
#' @export
#' @rdname as.tbl_array
as.tbl_array.data.frame <- function(df, dim_names, ...) {
  if (!is.character(dim_names)) {
    dim_names <- names(df)[dim_names]
  }
  met_names <- setdiff(names(df), dim_names)
  
  dims <- lapply(df[dim_names], unique)
  n <- vapply(dims, length, integer(1))
  # need to check for uniqueness of combinations
  
  grid <- expand.grid(dims, KEEP.OUT.ATTRS = FALSE)
  all <- merge(grid, df, all.x = TRUE, by = dim_names)
  
  mets <- lapply(met_names, function(i) array(df[[i]], n))
  names(mets) <- met_names
  
  tbl_array(dims, mets)
}
