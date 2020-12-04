\name{calcularResumenVariablesContinuas}
\alias{calcularResumenVariablesContinuas}
\title{
Summary statistics for continuous variables
}
\description{
\code{calcularResumenVariablesContinuas} gives the main statistical summary for continuous variables (mean, standard deviation, coefficient of variation, skewness, kurtosis and quantiles). Also builds the frequency table (with classmark, amplitude and density).
}
\usage{
calcularResumenVariablesContinuas(data,
                                  statistics = c("mean", "sd", "se(mean)", "IQR",
                                  "quantiles", "cv", "skewness", "kurtosis"),
                                  quantiles = c(0, 0.25, 0.5, 0.75, 1), groups = NULL,
                                  tablaFrecuencia = FALSE, cortes="Sturges", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{\code{data.frame} with the continuous variables.}
  \item{statistics}{any of \code{"mean"}, \code{"sd"}, \code{"se(mean)"}, \code{"quantiles"}, \code{"cv"} (coefficient of variation - sd/mean), \code{"skewness"} or \code{"kurtosis"}; defaulting to \code{c("mean", "sd", "quantiles", "IQR")}.}
  \item{quantiles}{quantiles to report; by default is \code{c(0, 0.25, 0.5, 0.75, 1)}.}
  \item{groups}{optional variable, typically a factor, to be used to partition the data. By deafault is \code{NULL}.}
  \item{tablaFrecuencia}{logical value indicating whether or not to display the frequency table, by default is \code{FALSE}.}
  \item{cortes}{one of:
    \itemize{ 
      \item a numeric vector of two or more unique cut points,
      \item a single number (greater than or equal to 2) giving the number of intervals into which data is to be cut, 
      \item a character string naming an algorithm to compute the number of intervals into which data is to be cut (see 'Details')
    }
  by default is \code{Sturges}.}
  \item{\dots}{further arguments to be passed to \code{\link[RcmdrMisc]{numSummary}}.}
}
\details{
\code{calcularResumenVariablesContinuas} performs a descriptive analysis of continuous variables (quantitative variables that take infinite distinct values into an interval), generating a table of statistics (arithmetic mean, standard deviation, interquartile range, coefficient of variation, skewness, kurtosis, and quantiles) optionally allowing the partition of the data by a factor variable (\code{groups}).

It also allows to show the frequency table of selected continuous variables by selecting \code{tablaFrecuencia=TRUE}. Moreover it also allows to divide the range of the variables into intervals given by the argument \code{cortes} (breaks). See more info in \code{\link{cut}} and in \code{\link{hist}}.

}
\value{\code{calcularResumenVariablesContinuas} returns a list of two elements:
\item{.numsummary}{an object of class \code{"numSummary"} containing the numerical summary of the continuous variables.}
\item{.table}{a matrix containing the values of the frequency table.}
}

\seealso{
\code{\link[RcmdrMisc]{numSummary}}, \code{\link{cut}}, \code{\link{hist}}
}

\examples{
\dontrun{
data(cars93)
calcularResumenVariablesContinuas(data=cars93["FuelCapacity"],group=NULL)
calcularResumenVariablesContinuas(data=cars93["FuelCapacity"],group=cars93$Airbags)
bb <- calcularResumenVariablesContinuas(data=cars93["FuelCapacity"],group=cars93$Airbags,
                                        tablaFrecuencia=TRUE)
str(bb)
bb
bb$.summary
class(bb$.summary)

calcularResumenVariablesContinuas(data=cars93["MidPrice"], tablaFrecuencia=TRUE)
calcularResumenVariablesContinuas(data=cars93["MidPrice"], tablaFrecuencia=TRUE, cortes=5)
calcularResumenVariablesContinuas(data=cars93["MidPrice"], tablaFrecuencia=TRUE,
                                  cortes=c(7,14,21,28,63))
calcularResumenVariablesContinuas(data=cars93["MidPrice"], tablaFrecuencia=TRUE,
                                  cortes="Scott")                                
calcularResumenVariablesContinuas(data=cars93["MidPrice"], groups=cars93$Airbags, 
                                  tablaFrecuencia=TRUE, cortes=5)
}
}
