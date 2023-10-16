\name{DMKV.test}
\alias{DMKV.test}
\title{
Z-test for the difference of means of two independent Normal variables with known population variances.
}
\description{
Under the assumption that the data come from two independent Normal distributions, it performs the hypothesis test and the confidence interval for the difference of means with known population variances.
}
\usage{
DMKV.test(x, y, difmu = 0, sdx, sdy,
          alternative = c("two.sided", "less", "greater"), conf.level = 0.95,
          ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{numerical vector (non-empty) that contains the data of the first sample.}
  \item{y}{numerical vector (non-empty) containing the data of the second sample.}
  \item{difmu}{numeric value indicating the value of the difference in population means between the two samples.}
  \item{sdx}{numerical value indicating the population standard deviation of the first sample, which is assumed to be known (mandatory).}
  \item{sdy}{numeric value indicating the population standard deviation of the second sample, which is assumed to be known (mandatory).}
  \item{alternative}{a character string specifying the alternative hypothesis, must be one of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}. You can specify just the initial letter.}
  \item{conf.level}{confidence level of the interval.}
  \item{\dots}{further arguments to be passed to or from methods.}
}

\value{
 A list with class \code{"htest"} containing the following components:

  \item{statistic}{the value of the test statistic.}
  \item{parameter}{sample lengths and population standard deviations.}
  \item{p.value}{the p-value for the test.}
  \item{conf.int}{confidence interval for the difference of means with known population variances associated with the specified alternative hypothesis.}
  \item{estimate}{the estimated difference in means.}
  \item{null.value}{the specified hypothesized value of the mean difference.}
  \item{alternative}{a character string describing the alternative hypothesis.}
  \item{method}{a character string indicating what type of statistical method was performed.}
  \item{data.name}{a character string giving the name(s) of the data.}
}

\seealso{ \code{\link{t.test}}
}

\examples{
data(cars93) # Data set provided with the package
# Maximum price difference (MaxPrice) in means between cars manufactured in the
# US and those manufactured outside, assuming that the variances are known and 
# equal to 64 and 169, respectively
var1<-subset(cars93, USA=="nonUS", select=MaxPrice)
var2<-subset(cars93, USA=="US", select=MaxPrice)
DMKV.test(var1, var2, sdx=13, sdy=8, difmu=0,
alternative="greater", conf.level=0.95)

}
