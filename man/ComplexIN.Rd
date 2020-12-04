\name{ComplexIN}
\alias{ComplexIN}

\title{
Complex index numbers
}
\description{
\code{ComplexIN} computes the aggregation of a set of index numbers using the arithmetic, geometric or harmonic means.
}
\usage{
ComplexIN(data, means = c("arithmetic", "geometric", "harmonic"), zero.rm = TRUE, 
          na.rm = TRUE, ...)
}

\arguments{
  \item{data}{
Data frame containing, the index numbers to aggregate.
}
  \item{means}{
Character vector with the name of the mean to compute. \code{mean} can be \code{arithmetic}, \code{geometric} or \code{harmonic}.
}
  \item{zero.rm}{
Logical string for geometric and harmonic means. \code{TRUE} (default) indicates that negative and zero values should be deleted before computing the geometric mean and that zero values should be deleted before computing the harmonic mean.
}
  \item{na.rm}{
Logical value indicating whether NA values should be stripped before the computation proceeds. It is \code{TRUE} by default.
}
  \item{\dots}{
Further arguments passed to or from other methods.
}
}
\details{
\code{\link{Sindex}}, \code{\link{Deflat}}, \code{\link{priceIndexNum}}.
}
\value{
Matrix with as many rows as columns of \code{x} and as many columns as \code{means} selected.
}

\examples{

df <- data.frame(Index=round(runif(12,80,105),2))
ComplexIN(df, means = c("arithmetic", "geometric", "harmonic"))
}

