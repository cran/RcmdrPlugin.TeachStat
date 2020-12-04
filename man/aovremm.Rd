\name{aovremm}
\alias{aovremm}

\title{
ANOVA with random effects using the Moments method.
}
\description{
Print the ANOVA table with random effects and compute the classical point estimations
of the variance components using the Moments method.
}
\usage{
aovremm(formula, data = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
%%     ~~Describe \code{formula} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}

\value{
A list%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}


\seealso{
\code{\link{aov}}, \code{\link{lmer}}, \code{\link{aovreml}}.
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (formula, data = NULL, ...) 
{
    ANOV <- aov(formula, data, ...)
    .ANOV <- summary(ANOV)
    cat("-------------------------------")
    cat("\n", gettext("ANOVA table", domain = "R-RcmdrPlugin.TeachStat"), 
        ":\n", sep = "")
    print(.ANOV)
    cat("\n-------------------------------\n\n")
    .sighat2 <- .ANOV[[1]]$`Mean Sq`[2]
    .vars <- all.vars(formula)
    .groups <- data[[.vars[2]]][!is.na(data[[.vars[1]]])]
    .n <- length(.groups)
    .ni <- table(.groups)
    .c <- (.n^2 - sum(.ni^2))/(.n * (length(.ni) - 1))
    .sighatalph2 <- (.ANOV[[1]]$`Mean Sq`[1] - .sighat2)/.c
    if (.sighatalph2 < 0) 
        warning("Estimation of any variance component is not positive. The variance 
                 component model is inadequate.")
    .prop <- .sighatalph2/(.sighatalph2 + .sighat2)
    estim <- c(.sighat2, .sighatalph2, .prop)
    names(estim) <- c("var (Error)", "var (Effect)", "\% var (Effect)")
    cat("\n", gettext("Components of Variance", domain = "R-RcmdrPlugin.TeachStat"), 
        ":\n", sep = "")
    print(estim)
    return(invisible(list(model = ANOV, estimation = estim)))
  }
}
