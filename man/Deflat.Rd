\name{Deflat}
\alias{Deflat}

\title{
Deflation of an economic series
}
\description{
\code{Deflat} deflacts a current value variable into a constant value variable.
}
\usage{
Deflat(x, pervar, cvar, defl, base)
}
\arguments{
  \item{x}{
Data frame containing, at least, the characteristics (time, location, \ldots), the current value and the deflator variables.
}
  \item{pervar}{
Character string for the name of the factor variable with the characteristics.
}
  \item{cvar}{
Character string for the name of the numeric variable with the current values.
}
   \item{defl}{
Character string for the name of the numeric variable with the index number used as deflator.
}
  \item{base}{
Character string for the name of the base characteristic.
}
}
\value{
\code{Deflat} returns a data frame with one column:
\item{const_base}{The variable with constant values at base \code{base}}
}


\seealso{
\code{\link{Sindex}}, \code{\link{ComplexIN}}, \code{\link{priceIndexNum}}.
}
\examples{
data(Depositos, package = "RcmdrPlugin.TeachStat")
Deflat(Depositos, "year", "quantity", "G_IPC_2016", "2018")
}
