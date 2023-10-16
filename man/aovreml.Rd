\name{aovreml}
\alias{aovreml}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ANOVA with random effects using the (REstricted) Maximum Likelihood method.
}
\description{
Print the ANOVA table with random effects and compute the point estimations
of the variance components using the maximum likelihood method or the REstricted 
Maximum Likelihood (REML) method. It also provides some confidence intervals.
}
\usage{
aovreml(formula, data = NULL, Lconfint = FALSE, REML = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
A formula specifying the model. Random-effects terms are distinguished by vertical bars (|) separating expressions for design matrices from grouping factors. Two vertical bars (||) can be used to specify multiple uncorrelated random effects for the same grouping variable. (Because of the way it is implemented, the ||-syntax works only for design matrices containing numeric (continuous) predictors.)
}
  \item{data}{
an optional data frame containing the variables named in \code{formula}. By default the variables are taken from the environment of \code{formula}.
}
  \item{Lconfint}{
logical scalar - Should the confidence intervals be printed?
}
  \item{REML}{
logical scalar - Should the estimates be chosen to optimize the REML criterion (as opposed to the log-likelihood)?
}
  \item{\dots}{
Arguments to be passed to other functions.
}
}
\value{
A list %%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\seealso{
\code{\link{aov}}, \code{\link{lmer}}, \code{\link{aovremm}}.
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (formula, data = NULL, Lconfint = FALSE, REML = TRUE, 
    ...) 
{
    vars <- all.vars(formula)
    formulaaov <- as.formula(paste(vars[1], "~", vars[2]))
    ANOV <- aov(formulaaov, data, ...)
    .ANOV <- summary(ANOV)
    cat("-------------------------------")
    cat("\n", gettext("ANOVA table", domain = "R-RcmdrPlugin.TeachStat"), 
        ":\n", sep = "")
    print(.ANOV)
    cat("\n-------------------------------\n")
    .sol <- lme4::lmer(formula, data = data, REML = REML, ...)
    .varcor <- lme4::VarCorr(.sol)
    .sighat2 <- unname(attr(.varcor, "sc"))^2
    .sighatalph2 <- unname(attr(.varcor[[vars[2]]], "stddev"))^2
    .prop <- .sighatalph2/(.sighatalph2 + .sighat2)
    estim <- c(.sighat2, .sighatalph2, .prop)
    names(estim) <- c("var (Error)", "var (Effect)", "\% var (Effect)")
    cat("\n", gettext("Components of Variance", domain = "R-RcmdrPlugin.TeachStat"), 
        " (", lme4::methTitle(.sol@devcomp$dims), "):\n", sep = "")
    print(estim)
    if (Lconfint) {
        cat("\n", gettext("Confidence intervals", domain = "R-RcmdrPlugin.TeachStat"), 
            ":\n", sep = "")
        print(confint(.sol, oldNames = FALSE))
    }
    return(invisible(list(model = .sol, estimation = estim)))
  }
}