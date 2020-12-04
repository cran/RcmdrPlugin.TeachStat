\name{priceIndexNum}
\alias{priceIndexNum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Price index numbers
}
\description{
\code{priceIndexNum} computes price indices given data on products over time (prices and quantities)
}
\usage{
priceIndexNum(x, prodID, pervar, pvar, qvar, base, indexMethod = "laspeyres", 
              output = "fixedBase", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Data frame containing, at least, the characteristics (time, location, \ldots), the product identifiers, the prices and the quantities.
}
  \item{prodID}{
Character string for the name of the product identifier.
}
  \item{pervar}{
Character string for the name of the factor variable with the characteristics.
}
  \item{pvar}{
Character string for the name of the price variable.
}
  \item{qvar}{
Character string for the name of the quantity variable.
}
  \item{base}{
Character string for the name of the base characteristic.
}
  \item{indexMethod}{
Character vector to select the price index method. Tipically price index methods are laspeyres (default), paasche, and fisher, but it can also be use those in function \code{\link{priceIndex}} from package \code{IndexNumR} (dutot, carli, jevons, cswd, harmonic, tornqvist, satovartia, walsh and CES).
}
  \item{output}{
A character string specifying whether a chained (output="chained") , fixed base (output="fixedBase") or period-on-period (output="pop") price index numbers should be returned. Default is fixed base.
}
  \item{\dots}{
Further arguments passed to or from other methods.
}
}
\details{
\code{priceIndexNum} uses the function \code{\link{priceIndex}} from package \code{IndexNumR} without restricting the argument \code{pervar} from being integers starting at period 1 (base) and increasing in increments of 1 period. 
}
\value{
\code{priceIndexNum} returns a data frame with one column with the characteric variable plus as many columns as \code{indexMethod} selected:
\item{period}{The characteristic variable.}
\item{laspeyres}{The price index computed by the Laspeyres method.}
\item{paasche}{The price index computed by the Paasche method.}
\item{fisher}{The price index computed by the Fisher method.}
\item{\ldots}{}
}

\seealso{
\code{\link{priceIndex}}, \code{\link{Sindex}}, \code{\link{Deflat}}, \code{\link{ComplexIN}}.
}
\examples{
library(IndexNumR)
data(Prices, package = "RcmdrPlugin.TeachStat")

priceIndexNum(Prices, prodID = "prodID", pervar = "year", pvar = "price", 
              qvar = "quantity", base = "2003", 
              indexMethod = c("laspeyres", "paasche", "fisher"))
}