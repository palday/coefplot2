\name{extend_tab}
\alias{extend_tab}
\title{
   Create an extended coefficient table
}
\description{
  A utility function for extending coefficent tables
  (with rows of \code{NA} values) for variables that
  weren't included in a particular model
}
\usage{
extend_tab(tab, vnames)
}
\arguments{
  \item{tab}{
    A coefficient table (i.e., a matrix or data frame with
    row names specifying variable names)
}
  \item{vnames}{
    A character vector of variable names
  }
}
\value{
  A matrix or data frame (depending on the class of
  the original coefficient table) with new rows
  containing \code{NA}s for the unmatched variable names
}
\author{
  Ben Bolker
}
\examples{
  y <- x1 <- x2 <- runif(10)
  m1 <- lm(y~x1)
  extend_tab(coeftab(m1),c("(Intercept)","x1","x2"))
}
\keyword{misc}
