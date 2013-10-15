#' A tbl based on an array
#' 
#' This dense data representation. Useful for highly crossed data.
#' 
#' @export
#' @examples
#' nasa
#' head(as.data.frame(nasa))
#' 
#' titanic <- from_array(Titanic)
#' head(as.data.frame(titanic))
#' 
#' expend <- from_array(USPersonalExpenditure)
#' head(as.data.frame(expend))
#' 
#' admit <- from_array(UCBAdmissions)
#' head(as.data.frame(admit))
#' 
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


from_array <- function(x, met_name = deparse(substitute(x)), dim_names = names(dimnames(x))) {
  force(met_name)
  
  dims <- dimnames(x)
  dims <- lapply(dims, type.convert, as.is = TRUE)
  
  if (is.table(x)) {
    class(x) <- setdiff(class(x), "table")
  }
  mets <- setNames(list(undimname(x)), met_name)
  
  tbl_array(dims, mets)
}

to_dense <- function(df, dim_names) {
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