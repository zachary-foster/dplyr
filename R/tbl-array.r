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


# select operates only on metrics: focussing on a subset, or reordering
# doesn't affect dimensions in any way
select.tbl_array <- function(x, ...) {
  idx <- var_index(dots(...), x$mets, parent.frame())
  x$mets <- x$mets[idx]
  x
}

# filter operates only on dimensions, focussing on a subset. Each component
# of the expression can only have one dimension: this prevents you from making
# non-rectangular subset
#
# filter(nasa, lat > 0, year == 2000)
filter.tbl_array <- function(.data, ...) {
  exprs <- dots(...)
  
  idx <- vapply(exprs, find_index_check, integer(1), names = names(.data$dims))
  for(i in seq_along(exprs)) {
    sel <- eval(exprs[[i]], .data$dims, parent.frame())
    sel <- sel & !is.na(sel)
    
    .data$dims[[idx[i]]] <- .data$dims[[idx[i]]][sel]
    .data$mets <- lapply(.data$mets, subs_index, idx[i], sel)
  }
  
  .data
}

arrange.tbl_array <- function(.data, ...) {
  exprs <- dots(...)
  
  idx <- vapply(exprs, find_index_check, integer(1), names = names(.data$dims))
  for(i in seq_along(exprs)) {
    ind <- eval(exprs[[i]], .data$dims, parent.frame())
    ord <- order(index)
    
    .data$dims[[idx[i]]] <- .data$dims[[idx[i]]][order]
    .data$mets <- lapply(.data$mets, subs_index, idx[i], order)
  }
  
  .data
}

group_by.tbl_array <- function(.data, ...) {
  idx <- var_index(dots(...), .data$dims, parent.frame())
  .data$group <- idx
  .data
}

# by_loc <- group_by(nasa, lat, long)
# summarise(by_loc, pressure = max(pressure), temp = mean(temperature))

# mutate and summarise operate similarly need to evaluate variables in special
# context - need to use the same active environment tricks as in dplyr
# for better performance

summarise.tbl_array <- function(.data, ...) {
  exprs <- named_dots(...)
  out_dims <- .data$dims[.data$group]
  n <- vapply(out_dims, length, integer(1))
  
  out_mets <- list()
  for (nm in names(exprs)) {
    out_mets[[nm]] <- array(logical(), n)
  }
  
  slices <- expand.grid(lapply(out_dims, seq_along), KEEP.OUT.ATTRS = FALSE)
  
  # Loop over each group
  for (i in seq_len(nrow(slices))) {
    index <- as.list(slices[i, , drop = FALSE])
    mets <- lapply(.data$mets, subs_index, i = .data$group, val = index, 
      drop = TRUE)
    
    # Loop over each expression
    for (j in seq_along(exprs)) {
      res <- eval(exprs[[j]], mets, parent.frame())
      out_mets[[j]][i] <- res  
    }
  }
  
  structure(list(dims = out_dims, mets = out_mets), class = "tbl_array")
}

subs_index <- function(x, i, val, drop = FALSE) {
  dims <- length(dim(x) %||% 1)
  
  args <- rep(list(quote(expr = )), dims)
  
  if (length(i) == 1 && is.atomic(val)) {
    args[[i]] <- quote(val)  
  } else if (length(i) > 1 && is.list(val)) {
    exprs <- lapply(seq_along(i), function(i) as.call(c(quote(`[[`), quote(val), i)))
    args[i] <- exprs
  } else {
    stop("Invalid input", call. = FALSE)
  }
  
  args$drop <- drop
  
  call <- as.call(c(quote(`[`), quote(x), args))
  eval(call)
}


find_index_check <- function(x, names) {
  idx <- find_index(x, names)
  if (length(idx) != 1) {
    stop(deparse(x), " does not refer to exactly one dimension.", call. = FALSE)
  }
  idx  
}

find_index <- function(x, names) {
  # Base cases
  if (is.atomic(x)) return(integer())
  if (is.name(x)) {
    var <- as.character(x)
    return(which(var == names))
  }
  
  # Recursive case: function call
  stopifnot(is.call(x))
  unlist(lapply(x[-1], find_index, names = names))
}


undimname <- function(x) {
  dimnames(x) <- NULL
  x
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