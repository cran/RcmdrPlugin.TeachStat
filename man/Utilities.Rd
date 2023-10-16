\name{Utilities}
\alias{twoOrMoreLevelFactors}
\alias{twoOrMoreLevelFactorsP}
\alias{listDistrs}
\alias{DiscreteDistrsP}
\alias{AbscontDistrsP}
\title{
\code{RcmdrPlugin.TeachStat} Utility Functions
}
\description{
\code{twoOrMoreLevelFactorsP()} returns TRUE if there is at least one factor in the active dataset that has two or more levels.

\code{twoOrMoreLevelFactors()} returns the object name of those factors that are active in the dataset that have at least two levels.

\code{listDistrs(class, env)} returns the object name of those distributions of class \code{class} (see package \code{\link{distr}}) in the \code{env} environment.

\code{DiscreteDistrsP()} returns TRUE if there is at least one distribution of class \code{DiscreteDistribution} (see \code{\link{DiscreteDistribution}}).

\code{AbscontDistrsP()} returns TRUE if there is at least one distribution of class \code{AbscontDistribution} (see \code{\link{AbscontDistribution}}).
}


\usage{
twoOrMoreLevelFactors()
twoOrMoreLevelFactorsP()
listDistrs(class = "UnivariateDistribution", envir = .GlobalEnv, ...)
DiscreteDistrsP()
AbscontDistrsP()
}

\arguments{
  \item{class}{
string with the name of the class to be listed.
}
  \item{envir}{
string with the name of the environment.
}
  \item{\dots}{
further arguments.
}
}

