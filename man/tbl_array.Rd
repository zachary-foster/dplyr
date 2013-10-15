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
  data.
}
\examples{
nasa
head(as.data.frame(nasa))

titanic <- as.tbl_array(Titanic)
head(as.data.frame(titanic))

admit <- as.tbl_array(UCBAdmissions)
head(as.data.frame(admit))

as.tbl_array(esoph, dim_names = 1:3)
}
\seealso{
  \code{\link{as.tbl_array}} for ways of coercing existing
  data structures into a \code{tbl_array}.
}

