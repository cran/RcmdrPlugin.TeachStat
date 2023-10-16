\name{calcularResumenVariablesDiscretas}
\alias{calcularResumenVariablesDiscretas}
\title{
Summary statistics for discrete variables
}
\description{
\code{calcularResumenVariablesDiscretas} gives the main statistical summary for discrete variables (mean, standard deviation, coefficient of variation, skewness, kurtosis and quantiles). Also builds the frequency table %(with classmark, amplitude and density).
}
\usage{
calcularResumenVariablesDiscretas(data,
                                  statistics = c("mean", "sd", "se(mean)", "IQR",
                                  "quantiles", "cv", "skewness", "kurtosis"),
                                  quantiles = c(0, 0.25, 0.5, 0.75, 1), groups = NULL,
                                  tablaFrecuencia = FALSE, cortes=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{\code{data.frame} with the discrete variables.}
  \item{statistics}{any of \code{"mean"}, \code{"sd"}, \code{"se(mean)"}, \code{"quantiles"}, \code{"cv"} (coefficient of variation - sd/mean), \code{"skewness"} or \code{"kurtosis"}; defaulting to \code{c("mean", "sd", "quantiles", "IQR")}.}
  \item{quantiles}{quantiles to report; by default is \code{c(0, 0.25, 0.5, 0.75, 1)}.}
  \item{groups}{optional variable, typically a factor, to be used to partition the data. By default is \code{NULL}.}
  \item{tablaFrecuencia}{logical value indicating whether or not to display the frequency table, by default is \code{FALSE}.}
  \item{cortes}{one of:
    \itemize{ 
      \item a numeric vector of two or more unique cut points,
      \item a single number (greater than or equal to 2) giving the number of intervals into which data is to be cut, 
      \item a character string naming an algorithm to compute the number of intervals into which data is to be cut (see 'Details')
    }
  by default is \code{NULL}.}
}
\details{
\code{calcularResumenVariablesDiscretas} performs a descriptive analysis of discrete variables (quantitative variables that take as a finite or infinite numerable distinct values), generating a table of statistics (arithmetic mean, standard deviation, interquartile range, coefficient of variation, skewness, kurtosis, and quantiles) optionally allowing the partition of the data by a factor variable (\code{groups}).

It also allows to show the frequency table of selected discrete variables by selecting \code{tablaFrecuencia=TRUE}. Moreover it also allows to divide the range of the variables into intervals given by the argument \code{cortes} (breaks). See more info in \code{\link{cut}} and in \code{\link{hist}}.

}
\value{\code{calcularResumenVariablesDiscretas} returns a list of two elements:
\item{.numsummary}{an object of class \code{"numSummary"} containing the numerical summary of the discrete variables.}
\item{.table}{a matrix containing the values of the frequency table.}
}

\seealso{
\code{\link{cut}}, \code{\link{hist}}
}

\examples{
\dontrun{
data(cars93)
calcularResumenVariablesDiscretas(data=cars93["Cylinders"],group=NULL)
calcularResumenVariablesDiscretas(data=cars93["Cylinders"],group=cars93$Airbags)
bb <- calcularResumenVariablesDiscretas(data=cars93["Cylinders"],group=cars93$Airbags,
                                        tablaFrecuencia=TRUE)
str(bb)
bb
bb$.summary
class(bb$.summary)

calcularResumenVariablesDiscretas(data=cars93["Horsepower"], tablaFrecuencia=TRUE)
calcularResumenVariablesDiscretas(data=cars93["Horsepower"], tablaFrecuencia=TRUE, cortes=5)
calcularResumenVariablesDiscretas(data=cars93["Horsepower"], tablaFrecuencia=TRUE,
                                  cortes=c(50,100,200,250,300))
calcularResumenVariablesDiscretas(data=cars93["Horsepower"], tablaFrecuencia=TRUE,
                                  cortes="Sturges")                                
calcularResumenVariablesDiscretas(data=cars93["Horsepower"], groups=cars93$Airbags, 
                                  tablaFrecuencia=TRUE, cortes=5)
}
}
