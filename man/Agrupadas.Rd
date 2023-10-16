\name{Agrupadas}
\alias{Agrupadas}
\docType{data}
\title{
Grouped or tabulated data set
}
\description{
Grouped or tabulated data set, given by lower and upper limits and frequency.
It is used as an example for the use of the \emph{Numerical Summaries - Tabulated data} window of the \code{RcmdrPlugin.TeachStat} package
}
\usage{data("Agrupadas")}
\format{
  Data frame with 4 cases (rows) and 3 variables (columns).
  \describe{
    \item{\code{Linf}}{Numeric value, the lower limit of the tabulated data.}
    \item{\code{Lsup}}{Numeric value, the upper limit of the tabulated data.}
    \item{\code{ni}}{Numeric value, the frequency of the tabulated data.}
  }
}

\examples{
data(Agrupadas)
calcularResumenDatosTabulados(l_inf=Agrupadas$Linf, l_sup=Agrupadas$Lsup,
  ni=Agrupadas$ni, statistics =c("mean", "sd", "IQR", "quantiles"), quantiles
  = c(0,0.25,0.5,0.75,1), tablaFrecuencia=FALSE)

}
\keyword{datasets}
