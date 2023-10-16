\name{Prices}
\alias{Prices}
\docType{data}
\title{
Data for computing price indices.
}
\description{
Data on the sold quantity and sale price of several products through some years.

It is used as an example for the use of the \emph{Price index} window of the \code{RcmdrPlugin.TeachStat} package
}
\usage{data("Prices")}
\format{
  A data frame with 15 observations on the following 4 variables.
  \describe{
    \item{\code{year}}{a factor representing the year}
    \item{\code{prodID}}{a factor with the ID of the products}
    \item{\code{price}}{the sale price}
    \item{\code{quantity}}{the sold quantity}
  }
}

\examples{
data(Prices)
priceIndexNum (Prices, prodID ="prodID", pervar ="year", pvar="price", 
               qvar ="quantity", base="2001", indexMethod =c("laspeyres", "paasche", "fisher"))
}
\keyword{datasets}
