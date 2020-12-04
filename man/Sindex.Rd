\name{Sindex}
\alias{Sindex}

\title{
Simple index numbers
}
\description{
\code{Sindex} returns a data frame with the index numbers with a given base. An index number measures changes in a varible with respect to a characteristic (time, location, \ldots)

\code{Sindex} can also be used for computing the base change of an index number.
}
\usage{
Sindex(x, pervar, vvar, base)
}

\arguments{
  \item{x}{
Data frame containing, at least, a factor and a numeric variables.
}
  \item{pervar}{
Character string for the name of the factor variable with the characteristics.
}
  \item{vvar}{
Character string for the name of the numeric variable for which you want to calculate the index number or representing the index number for which you want to compute a base change.
}
  \item{base}{
Character string for the name of the base characteristic.
}
}
\value{
\code{Sindex} returns a data frame with one column:
\item{index_base}{The index number with base \code{base}}
}

\seealso{
\code{\link{Deflat}}, \code{\link{ComplexIN}}, \code{\link{priceIndexNum}}.
}
\examples{
data(Depositos, package = "RcmdrPlugin.TeachStat")
Sindex(Depositos, "year", "quantity", "2006")
}

