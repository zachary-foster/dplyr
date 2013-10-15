\name{tbl_array}
\alias{tbl_array}
\title{A tbl based on an array}
\usage{
  tbl_array(dimensions, measures)
}
\description{
  This dense data representation. Useful for highly crossed
  data.
}
\examples{
nasa
head(as.data.frame(nasa))

titanic <- from_array(Titanic)
head(as.data.frame(titanic))

expend <- from_array(USPersonalExpenditure)
head(as.data.frame(expend))

admit <- from_array(UCBAdmissions)
head(as.data.frame(admit))
}

