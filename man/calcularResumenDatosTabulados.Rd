\name{calcularResumenDatosTabulados}
\alias{calcularResumenDatosTabulados}
\title{
Summary statistics for tabulated data
}
\description{
\code{calcularResumenDatosTabulados} performs the main statistical summary for tabulated data (mean, standard deviation, coefficient of variation, skewness, kurtosis, quantile and mode) are calculated. Also it allows to obtain the frequency table (with classmark, amplitude and density).
}
\usage{
calcularResumenDatosTabulados(l_inf, l_sup, ni, 
                              statistics = c("mean", "sd", "se(mean)", "IQR",
                              "quantiles", "cv", "skewness", "kurtosis"),
                              quantiles = c(0, 0.25, 0.5, 0.75, 1), 
                              tablaFrecuencia = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{l_inf}{numeric vector with the lower limit of each interval.}
  \item{l_sup}{numeric vector with the upper limit of each interval.}
  \item{ni}{numeric vector with the frequency of occurrence of values in the range between the lower limit and upper limit [l_inf [i-1], l_sup [i]).}
  \item{statistics}{any of \code{"mean"}, \code{"sd"}, \code{"se(mean)"}, \code{"quantiles"}, \code{"cv"} (coefficient of variation - sd/mean), \code{"skewness"}, \code{"kurtosis"} or \code{"mode"}; defaulting to \code{c("mean", "sd", "quantiles", "IQR")}.}
  \item{quantiles}{quantiles to report; by default is \code{c(0, 0.25, 0.5, 0.75, 1)}.}
  \item{tablaFrecuencia}{logical value indicating whether or not to display the frequency table, by default is \code{FALSE}.}
}
\details{
\code{calcularResumenDatosTabulados} performs an analysis of \bold{tabulated data} (frequently used in statistics when the number of distinct values is large or when dealing with continuous quantitative variables), represented by a table of statistics (arithmetic mean, standard deviation, interquartile range, coefficient of variation, asymmetry, kurtosis, and quantile).

It also allows to show the frequency table of the tabulated variable by selecting \code{tablaFrecuencia=TRUE}. The class mark, amplitude and density are added to the frequency table.

The \code{LOWER LIMIT or L[i-1]} and \code{UPPER LIMIT or L[i]} vectors, represent the data of continuous quantitative variables in class intervals of the form [L[i-1], L[i]) where i = 1, .. ., k.
}
\value{\code{calcularResumenDatosTabulados()} returns a list of two elements:
\item{.numsummary}{an object of class \code{"numSummary"} containing the numerical summary of the tabulated variable.}
\item{.table}{a matrix containing the values of the frequency table.}
}

\seealso{
\code{\link{cut}}
}
\examples{

data(cars93)
cortes <- seq(from=1500, to=4250, by=250)
aa <- cut( cars93$Weight, breaks=cortes, dig.lab=4)
ni <- table(aa)
l_inf <- cortes[-length(cortes)]
l_sup <- cortes[-1]
agrup <- data.frame(l_inf,l_sup,ni)
head(agrup)

calcularResumenDatosTabulados(agrup$l_inf, agrup$l_sup, agrup$Freq)
calcularResumenDatosTabulados(agrup$l_inf, agrup$l_sup, agrup$Freq, tabla=TRUE)

bb <- calcularResumenDatosTabulados(agrup$l_inf, agrup$l_sup, agrup$Freq,
                                    statistics=c("mean","mode") )
bb
str(bb)
class(bb$.summary)
class(bb$.table)
}
