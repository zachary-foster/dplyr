\name{tbl_array}
\alias{tbl_array}
\title{A tbl based on an array}
\usage{
  tbl_array(dimensions, measures)
}
\arguments{
  \item{dimensions}{a named list of vectors}

  \item{measurs}{a named list of arrays. The dimension of
  each array should be the same as the length of the
  dimensions.}
}
\description{
  This dense data representation. Useful for highly crossed
  data. This is an experimental interface and little
  performance optimisation has been done, but you may find
  it useful as a way of conserving memory.
}
\examples{
nasa
head(as.data.frame(nasa))

titanic <- as.tbl_array(Titanic)
head(as.data.frame(titanic))

admit <- as.tbl_array(UCBAdmissions)
head(as.data.frame(admit))

as.tbl_array(esoph, dim_names = 1:3)

# Some manipulation examples with the NASA dataset --------------------------

# select() operates only on measures: it doesn't affect dimensions in any way
select(nasa, cloudhigh:cloudmid)

# filter() operates only on dimensions,
filter(nasa, lat > 0, year == 2000)
# Each component can only refer to one dimensions, ensuring that you always
# create a rectangular subset
\dontrun{filter(nasa, lat > long)}

# Arrange is meaningless for tbl_arrays

by_loc <- group_by(nasa, lat, long)
summarise(by_loc, pressure = max(pressure), temp = mean(temperature))
}
\seealso{
  \code{\link{as.tbl_array}} for ways of coercing existing
  data structures into a \code{tbl_array}.
}

