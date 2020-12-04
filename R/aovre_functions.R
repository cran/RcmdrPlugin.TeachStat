aovremm <- function (formula, data = NULL, ...){
  # Print the ANOVA table with random effects and compute the classical point estimations
  # of the variance components using the Moments method
  ANOV<- aov(formula, data, ...)
  .ANOV <- summary(ANOV)
  cat("-------------------------------")
  cat("\n",gettext("ANOVA table",domain="R-RcmdrPlugin.TeachStat"),":\n", sep="")
  print(.ANOV)
  cat("\n-------------------------------\n\n")
  .sighat2 <- .ANOV[[1]]$`Mean Sq`[2]
  .vars <- all.vars(formula)
  .groups <- data[[.vars[2]]][!is.na(data[[.vars[1]]])]
  .n <- length(.groups)
  .ni <- table(.groups)
  .c <- (.n^2-sum(.ni^2))/(.n*(length(.ni)-1))
  .sighatalph2 <- (.ANOV[[1]]$`Mean Sq`[1]-.sighat2)/.c
  if(.sighatalph2<0) warning("Estimation of some variance component is negative. The variance component model is inadequate.")
  .prop <- .sighatalph2/(.sighatalph2+.sighat2)
  estim <- c(.sighat2,.sighatalph2,.prop)
  names(estim) <- c("var (Error)","var (Effect)","% var (Effect)")
  cat("\n",gettext("Components of Variance",domain="R-RcmdrPlugin.TeachStat"),":\n", sep="")
  print(estim)
  return(invisible(list(model=ANOV,estimation=estim)))
}

aovreml <- function (formula, data = NULL, Lconfint=FALSE,REML=TRUE,...){
  # Print the ANOVA table with random effects and compute the point estimations
  # of the variance components using the maximum likelihood method and the restricted 
  # maximum likelihood (REML) method. It also provides some confidence intervals.
  vars <- all.vars(formula)
  formulaaov <- as.formula(paste(vars[1],"~",vars[2]))
  ANOV<- aov(formulaaov, data, ...)
  .ANOV <- summary(ANOV)
  cat("-------------------------------")
  cat("\n",gettext("ANOVA table",domain="R-RcmdrPlugin.TeachStat"),":\n", sep="")
  print(.ANOV)
  cat("\n-------------------------------\n")
  .sol <- lme4::lmer(formula, data = data, REML = REML,...)
  .varcor <- lme4::VarCorr(.sol)
  .sighat2 <- unname(attr(.varcor,"sc"))^2
  .sighatalph2 <- unname(attr(.varcor[[vars[2]]],"stddev"))^2
  .prop <- .sighatalph2/(.sighatalph2+.sighat2)*100
  estim <- c(.sighat2,.sighatalph2,.prop)
  names(estim) <- c("var (Error)","var (Effect)","% var (Effect)")
  cat("\n",gettext("Components of Variance",domain="R-RcmdrPlugin.TeachStat")," (",lme4::methTitle(.sol@devcomp$dims),"):\n", sep="")
  print(estim)
  if(Lconfint){
    cat("\n",gettext("Confidence intervals",domain="R-RcmdrPlugin.TeachStat"),":\n", sep="")
    print(confint(.sol,oldNames=FALSE))
  } 
  
  return(invisible(list(model=.sol,estimation=estim)))
}