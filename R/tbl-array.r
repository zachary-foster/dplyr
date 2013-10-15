#' A tbl based on an array
#' 
#' This dense data representation. Useful for highly crossed data.
#' 
#' @export
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
tbl_array <- function(dimensions, measures) {
  # Dimensions is a list of vectors
  # Measures is a list of arrays - array dim matches length of dimensions
  
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
