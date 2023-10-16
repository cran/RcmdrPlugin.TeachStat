\name{randomnessMenu}
\title{Randomness test}
\alias{Randomness test}
\alias{numeric.runs.test}
\alias{twolevelfactor.runs.test}
\docType{package}
\encoding{UTF-8}
\description{
  In the "Nonparametric Tests" menu, two new entries are provided to
  perform the randomness test.

  The first "Randomness test for two level factor..." can be used to
  contrast the randomness of a factor with two levels. This option use the function \code{runs.test} from
  \code{tseries} package. %% For more information see
  \code{\link[tseries]{runs.test}}.
  
  The second entry in the menu
  "Randomness test for numeric variable..." is used to test the
  randomness of a numerical variable. This option use the function \code{runs.test} from
  \code{randtest} package. %% For more information see \code{\link[randtests]{runs.test}}.
}

\details{
  Here is an example of "Randomness test for a two level factor..." menu
  entry.

  Load data "AMSsurvey" selecting from Rcmdr menu: "Data" ->
  "Data in packages" -> "Read data set from an attached package..." then
  double-click on "car", click on "AMSsurvey" and on "OK". Rcmdr reply
  with the following command in source pane (R Script)

  \code{data(AMSsurvey, package="car")}

  To make randomness test on variable "sex", select from Rcmdr menu:
  "Statistics" -> "Nonparametric tests" ->
  "Randomness test for two level factor..." select "sex" and "OK". Rcmdr
  reply with the following command in source pane (R Script)

  \code{with(AMSsurvey, twolevelfactor.runs.test(sex))}

  Here is an example of "Randomness test for a numeric variable..." menu
  entry.

  Load data "sweetpotato" selecting from Rcmdr menu: "Data" ->
  "Data in packages" -> "Read data set from an attached package..." then
  double-click on "randtests", click on "sweetpotato" and on "OK". Rcmdr
  reply with the following command in source pane (R Script)

  \code{data(sweetpotato, package="randtests")}

  \code{sweetpotato <- as.data.frame(sweetpotato)}

  To make randomness test on variable "yield", select from Rcmdr menu:
  "Statistics" -> "Nonparametric tests" ->
  "Randomness test for numeric variable..." select "yield" and
  "OK". Rcmdr reply with the following command in source pane (R Script)

  \code{with(sweetpotato, numeric.runs.test(yield))}
  }

\author{
Manuel Munoz-Marquez <manuel.munoz@uca.es>
}

\keyword{package}

\seealso{
For more information see \code{\link[Rcmdr]{Rcmdr-package}}. 
}
