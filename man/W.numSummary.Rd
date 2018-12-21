\name{W.numSummary}
\alias{W.numSummary}
\title{
Summary statistics for weighted variables
}
\description{
\code{W.numSummary} gives the main statistical summary for weighted variables (mean, standard deviation, coefficient of variation, skewness, kurtosis and quantiles). It also allows the partition of the data by a factor variable.
}
\usage{
W.numSummary(data, 
             statistics = c("mean", "sd", "se(mean)", "IQR",
             "quantiles", "cv", "skewness", "kurtosis"),type = c("2", "1", "3"),
             quantiles = c(0, 0.25, 0.5, 0.75, 1),groups = NULL, weights)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{\code{data.frame} with the variables.}
  \item{statistics}{any of \code{"mean"}, \code{"sd"}, \code{"se(mean)"}, \code{"quantiles"}, \code{"cv"} (coefficient of variation - sd/mean), \code{"skewness"} or \code{"kurtosis"}; defaulting to \code{c("mean", "sd", "quantiles", "IQR")}.}
  \item{type}{definition to use in computing skewness and kurtosis; see the \code{\link[e1071]{skewness}} and \code{\link[e1071]{kurtosis}} functions in the \pkg{e1071} package. The default is "2".}
  \item{quantiles}{quantiles to report; by default is \code{c(0, 0.25, 0.5, 0.75, 1)}.}
  \item{groups}{optional variable, typically a factor, to be used to partition the data. By deafault is \code{NULL}.}
  \item{weights}{numeric vector of weights. Zero values are allowed.}
}
\details{
\code{W.numSummary} performs a descriptive analisys of quantitative variables weighted (or not) by a numeric variable which determines the importance of each subject in the data frame. Optionally it allows the partition of the data by a factor variable (\code{groups}).

Note that, unlike the \code{\link[RcmdrMisc]{numSummary}} function, the sample standard deviation is calculated instead of the sample standard quasideviation.
}
\value{An object with class \code{"numSummary"}.
}
\seealso{
\code{\link[RcmdrMisc]{numSummary}}, \code{\link[e1071]{skewness}}, \code{\link[e1071]{kurtosis}}.
}

\examples{
data(cars93)

# no weighted
W.numSummary(data=cars93[,c("CityMPG")], statistics =c("mean", "sd", "IQR", "quantiles"),
   quantiles = c(0,0.25,0.5,0.75,1), weights=NULL, groups=NULL)
# weighted
W.numSummary(data=cars93[,c("CityMPG")], statistics =c("mean", "sd", "IQR", "quantiles"),
   quantiles = c(0,0.25,0.5,0.75,1), weights=cars93$FuelCapacity, groups=NULL)
# no weighted
W.numSummary(data=cars93[,c("CityMPG")], statistics =c("mean", "sd", "IQR", "quantiles"),
   quantiles = c(0,0.25,0.5,0.75,1), weights=NULL, groups=cars93$Manual)
# weighted
bb <- W.numSummary(data=cars93[,c("CityMPG")], statistics =c("mean", "sd", "IQR", "quantiles"),
   quantiles = c(0,0.25,0.5,0.75,1), weights=cars93$FuelCapacity, groups=cars93$Manual)

bb
str(bb)
class(bb)
}
