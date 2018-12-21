\name{VUM.test}
\alias{VUM.test}
\title{
Chi-square test for the variance of a Normal variable with unknown population mean.
}
\description{
Under the assumption that the data come from a Normal distribution, it performs the hypothesis testing and the confidence interval for the variance with unknown population mean.
}
\usage{
VUM.test(x, sigma = 1, sigmasq = sigma^2,
         alternative = c("two.sided", "less", "greater"), conf.level = 0.95,
         ...)
}
\arguments{
  \item{x}{a (non-empty) numeric vector of data values.}
  \item{sigma}{a number indicating the true value of the population standar deviation - Null hypothesis.}
  \item{sigmasq}{control argument.}
  \item{alternative}{a character string specifying the alternative hypothesis, must be one of  \code{"two.sided"}, \code{"greater"} o \code{"less"}. You can specify just the initial letter.}
  \item{conf.level}{confidence level of the interval.}
  \item{\dots}{further arguments to be passed to or from methods.}
}

\value{
A list with class \code{"htest"} containing the following components:
  \item{statistic}{the value of the test statistic}
  \item{parameter}{the degrees of freedom for the test statistic}
  \item{p.value}{the p-value for the test.}
  \item{conf.int}{confidence interval for variance with unknown population mean associated with the specified alternative hypothesis.}
  \item{estimate}{vthe estimated variance.}
  \item{null.value}{the specified hypothesized value of the variance.}
  \item{alternative}{a character string describing the alternative hypothesis.}
  \item{method}{a character string indicating what type of statistical test was performed.}
  \item{data.name}{a character string giving the name of the data.}
}


\seealso{
\code{\link{VKM.test}} , \code{\link{var.test}}
}
\examples{
data(cars93) # Dataset provided with the package
# Variance of the maximum price (MaxPrice) assuming that the population mean 
# price is unknown
VUM.test(cars93$MaxPrice, alternative="two.sided", sigma=11, conf.level=0.95)
}
