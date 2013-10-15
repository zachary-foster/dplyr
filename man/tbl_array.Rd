\name{tbl_array}
\alias{tbl_array}
\title{A tbl based on an array}
\usage{
  tbl_array(data, labels = NULL)
}
\description{
  This dense data representation. Useful for highly crossed
  data.
}
\examples{
nasa

seals_dense <- to_dense(seals, c("lat", "long"))
titanic <- from_array(Titanic)
expend <- from_array(USPersonalExpenditure)
admit <- from_array(UCBAdmissions)
}

