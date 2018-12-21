\name{listTypesVariables}
\alias{listTypesVariables}

\title{
List of variables and types of a Data Frame
}
\description{
\code{listTypesVariables} returns a vector with the names and types of the variables of a data frame.
}
\usage{
listTypesVariables(dataSet)
}
\arguments{
  \item{dataSet}{
  the quoted name of a data frame in memory.
  }
}
\value{
A character vector
}
\seealso{
\code{\link{names}}
}
\examples{
require(datasets)
listTypesVariables("iris")
}

