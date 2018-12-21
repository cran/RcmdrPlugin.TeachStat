\name{MKV.test}
\alias{MKV.test}
\title{
Z-test for the mean of a Normal variable with known population variance.
}
\description{
Under the assumption that the data come from a Normal distribution, it makes the hypothesis testing and the confidence interval for the mean with known population variance.
}
\usage{
MKV.test(x, mu = 0, sd, alternative = c("two.sided", "less", "greater"),
         conf.level = 0.95, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a (non-empty) numeric vector of data values.}
  \item{mu}{a number indicating the true value of the mean - Null hypothesis.}
  \item{sd}{numerical value indicating the population standard deviation assumed to be known (mandatory).}
  \item{alternative}{ a character string specifying the alternative hypothesis, must be one of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}. You can specify just the initial letter.}
  \item{conf.level}{confidence level of the interval.}
  \item{\dots}{further arguments to be passed to or from methods.}
}

\value{
 A list with class \code{"htest"} l containing the following components:
  \item{statistic}{the value of the test statistic.}
  \item{parameter}{sample length, population standard deviation and sample standard deviation.}
  \item{p.value}{the p-value for the test.}
  \item{conf.int}{a confidence interval for the mean appropriate to the specified alternative hypothesis.}
  \item{estimate}{the estimated mean.}
  \item{null.value}{the specified hypothesized value of the mean.}
  \item{alternative}{a character string describing the alternative hypothesis.}
  \item{method}{a character string indicating what type of statistical test was performed.}
  \item{data.name}{a character string giving the name of the data.}
}
\seealso{
\code{\link{t.test}}
}
\examples{
data(cars93) # Dataset provided with the package
# Mean maximum price (MaxPrice) less than 20 thousand $ assuming that the 
# variance is known and equal to 11
MKV.test(cars93$MaxPrice, sd=11, alternative="less", mu=20, conf.level=0.95)

}

