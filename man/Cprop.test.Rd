\name{Cprop.test}
\alias{Cprop.test}
\title{
Test for proportions of one or two samples
}
\description{
Performs hypothesis testing and confidence interval for a proportion or difference of two proportions. The values of the samples necessary to perform the function are the number of successes and the number of trails.
}
\usage{
Cprop.test(ex, nx, ey = NULL, ny = NULL, p.null = 0.5,
           alternative = c("two.sided", "less", "greater"), conf.level = 0.95,
           ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ex}{numeric value that represents the number of successes of the first sample (see Details).}
  \item{nx}{numerical value representing the total number of trails of the first sample.}
  \item{ey}{ (optional) numerical value representing the number of success of the second sample (see Details).}
  \item{ny}{(optional) numerical value representing the total number of trails of the second sample.}
  \item{p.null}{numeric value that represents the value of the population proportion or the difference between the two population proportions, depending on whether there are one or two samples (see Details).}
  \item{alternative}{a character string specifying the alternative hypothesis, must be one of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}. You can specify just the initial letter.}
  \item{conf.level}{confidence level of the interval.}
  \item{\dots}{further arguments to be passed to or from methods.}
}
\details{
So that the contrast can be made must be fulfilled that at least 1 hit. That is, in the case of a sample \code{ex} must be greater than or equal to 1 and in the case of two samples, \code{ex} or \code{ey} must be greater than or equal to 1.

Furthermore, for the case of a sample value p.null must be strictly positive.

}
\value{
A list with class "htest" containing the following components:
  \item{statistic}{the value of the test statistic.}
  \item{parameter}{number of trails and value of the population proportion or the difference in population proportions.}
  \item{p.value}{the p-value for the test.}
  \item{conf.int}{a confidence interval for the proportion or for the difference in proportions, appropriate to the specified alternative hypothesis.}
  \item{estimate}{a value with the sample proportions.}
  \item{null.value}{the value of the null hypothesis.}
  \item{alternative}{a character string describing the alternative.}
  \item{method}{a character string indicating the method used, and whether Yates' continuity correction was applied.}
  \item{data.name}{a character string giving the names of the data.}
}


\seealso{
\code{\link{prop.test}}

}
\examples{
## Proportion for a sample
Cprop.test(1,6) # 1 success in 6 attempts

#### With a data set: proportion of cars not manufactured in US
data(cars93) #data set provided with the package
exitos<-sum(cars93$USA == "nonUS")
total<-length(cars93$USA)
Cprop.test(ex=exitos, nx=total)

## Difference of proportions
Cprop.test(1,6,3,15)
  # Sample 1: 1 success in 6 attempts
  # Sample 2: 3 success in 15 attempts

#### With a data set: difference of proportions of cars not manufactured in US 
#### between manual and automatic
exitosx<-sum(cars93$USA == "nonUS" & cars93$Manual == "Yes" )
totalx<-sum(cars93$Manual == "Yes")
exitosy<-sum(cars93$USA == "nonUS" & cars93$Manual == "No" )
totaly<-sum(cars93$Manual == "No")
Cprop.test(ex=exitosx, nx=totalx,ey=exitosy, ny=totaly)
}
