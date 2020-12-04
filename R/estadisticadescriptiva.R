
##Funciones de estadisticos Descriptivos

### menu: 01 Resumenes numercios - v. cualitativa
calcular_frecuencia <- function(df.nominal, ordenado.frec=FALSE, df.ordinal, cuantil.p=0.5, iprint=TRUE,...)
{
  tabla.frec <- list(.nominal=NULL,.ordinal=NULL,df.cuantil=NULL)
  if (!is.null(df.nominal)) {
    tabla.frec$.nominal <- lapply(df.nominal,tabla.frec.cualitativa, ordenado.frec=ordenado.frec)
    if(iprint) {
      cat("\n-------------------------------\n")
      cat("\n",gettext("Nominal variables",domain="R-RcmdrPlugin.TeachStat"),":\n", sep="")
      lapply(1:length(tabla.frec$.nominal), pprint, Table=tabla.frec$.nominal)
    }
  }
  if (!is.null(df.ordinal)) {     # ordeado polos factores
    tabla.frec$.ordinal <- lapply(df.ordinal,tabla.frec.cualitativa, ordinal=TRUE)
    cuantil <- sapply(1:length(tabla.frec$.ordinal), function(x,cuantil.p) tabla.frec$.ordinal[[x]][,"Fi"][ !(tabla.frec$.ordinal[[x]][,"Fi"] < cuantil.p) ][1], cuantil.p=cuantil.p)
    tabla.frec$df.cuantil <- data.frame(Variable=names(cuantil), Fi=cuantil)
    row.names(tabla.frec$df.cuantil) <- names(df.ordinal)


    if(iprint) {
      cat("\n-------------------------------\n")
      cat("\n",gettext("Ordinal variables",domain="R-RcmdrPlugin.TeachStat"),":\n", sep="")
      lapply(1:(length(tabla.frec$.ordinal)), pprint, Table=tabla.frec$.ordinal)
      cat("\n",gettext("Quantile:",domain="R-RcmdrPlugin.TeachStat")," ",cuantil.p, "\n", sep="")
      print(tabla.frec$df.cuantil, 3)  }
  }
  return( invisible(tabla.frec))
}


###

tabla.frec.cualitativa <- function(v.factor, ordenado.frec=FALSE, na.rm=FALSE, ordinal=FALSE, ...)
{
  .Table <- table(v.factor, useNA= if(na.rm) "always" else "no")
  N <- sum(.Table)
  if(!ordinal) {
    .Frec <- cbind(ni=.Table,fi=.Table/N)
    if(ordenado.frec) {
      return(.Frec[order(.Frec[,1],decreasing=TRUE),])
    } else  return(.Frec)
  } else {
    .Frec <- cbind(ni=.Table,fi=.Table/N, Ni= Ni<- cumsum(.Table), Fi=Ni/N)
    return(.Frec)
  }
}

pprint <- function(x,Table) {
  cat("\n",gettext("Variable",domain="R-RcmdrPlugin.TeachStat"),": ",names(Table)[x],"\n", sep="")
  print(Table[[x]], digits=3)
  cat("N= ",sum(Table[[x]][,1]),"\n", sep="")
}





### menu: 02 Resumen numerico - V. discreta

# Dependencias:
# Arquivo: 01 Resumenes numericos - v. cualitativa.r
#   tabla.frec.cualitativa()
#   pprint()
# Package: RcmdrMisc
# numSummary()
calcularResumenVariablesDiscretas <- function (data, statistics = c("mean", "sd", "se(mean)", "IQR", "quantiles", "cv", "skewness", "kurtosis"), quantiles = c(0, 0.25, 0.5, 0.75, 1), 
                                               groups=NULL, tablaFrecuencia=FALSE, cortes=NULL)
{
  .resumen <- list(.summary=NULL,.table=NULL)
  data<-as.data.frame(data)
  
  if(is.null(groups)) {
    cat("\n-------------------------------\n")
    cat("\n",gettext("Numerical summary",domain="R-RcmdrPlugin.TeachStat"),":\n", sep="")
    print(.resumen$.summary <- numSummary(data, statistics=statistics, quantiles=quantiles))
    if (tablaFrecuencia) {
      if(is.null(cortes)) {
        .resumen$.table <- lapply(data,tabla.frec.cualitativa, ordinal=TRUE)
      } else {
        dd <- function(x, cortes){
          ######### Para que calcule automaticamente el número de cortes segun  las fórmulas de Sturges, Freedman-Diaconis o Scott     
          if (is.character(cortes)) {
            cortes <- match.arg(tolower(cortes), c("sturges",
                                                   "fd", "freedman-diaconis", "scott"))
            cortes <- switch(cortes, sturges = nclass.Sturges(na.omit(x)),
                             `freedman-diaconis` = , fd = nclass.FD(na.omit(x)), scott = nclass.scott(na.omit(x)),
                             stop("unknown 'cortes' algorithm"))
          }
          ######################
          xx <- cut(x,breaks=cortes,include.lowest=TRUE, right=FALSE)
          return(tabla.frec.cualitativa(xx,ordinal=TRUE))
        }
        .resumen$.table <- lapply(data,dd, cortes)
      }
      cat("\n-------------------------------\n")
      cat("\n",gettext("Frequency distribution for discrete variables",domain="R-RcmdrPlugin.TeachStat"),":\n", sep="")
      lapply(1:(length(.resumen$.table)), pprint, Table=.resumen$.table)
    }
  }
  else {
    print(.resumen$.summary <- numSummary(data, groups=groups, statistics=statistics, quantiles=quantiles))
    if (tablaFrecuencia) {
      if(is.null(cortes)) {
        #        .resumen$.table <- lapply(data,tabla.frec.cualitativa, ordinal=TRUE)
        .resumen$.table <- by(data, groups, function(x) lapply(x,tabla.frec.cualitativa, ordinal=TRUE) )
      } else {
        dd <- function(x, cortes){
          ######### Para que calcule automaticamente el número de cortes segun  las fórmulas de Sturges, Freedman-Diaconis o Scott     
          if (is.character(cortes)) {
            cortes <- match.arg(tolower(cortes), c("sturges",
                                                   "fd", "freedman-diaconis", "scott"))
            cortes <- switch(cortes, sturges = nclass.Sturges(na.omit(x)),
                             `freedman-diaconis` = , fd = nclass.FD(na.omit(x)), scott = nclass.scott(na.omit(x)),
                             stop("unknown 'breaks' algorithm"))
          }
          ######################
          xx <- cut(x,breaks=cortes,include.lowest=TRUE, right=FALSE)
          return(tabla.frec.cualitativa(xx,ordinal=TRUE))
        }
        #        .resumen$.table <- lapply(data,dd, cortes)
        .resumen$.table <- by(data, groups, function(x) lapply(x,dd, cortes) )
        
      }
      cat("\n-------------------------------\n")
      cat("\n",gettext("Frequency distribution for discrete variables",domain="R-RcmdrPlugin.TeachStat"),":\n", sep="")
      if(length(names(data))==1 )
      {
        .xx <- lapply( 1:length(.resumen$.table), function(x) .resumen$.table[[x]] )
        names(.xx) <-  names(.resumen$.table)
        cat("\n ",gettext("Variable",domain="R-RcmdrPlugin.TeachStat"),": ",names(data), "\n", sep="")
        lapply(1:(length(.resumen$.table)),
               function (x, Table) {
                 cat("\n ",gettext("Group",domain="R-RcmdrPlugin.TeachStat"),": ", names(Table)[x], "\n", sep="")
                 print(Table[[x]], digits = 3)
                 cat("N= ", sum(Table[[x]][, 1]), "\n", sep="") } ,
               Table= .xx )
      } else {
        for( ii in 1:length(names(data)))
        {
          .xx <- lapply( 1:length(.resumen$.table), function(x) .resumen$.table[[x]][[ii]] )
          names(.xx) <-  attr(.resumen$.table,"dimnames")$groups
          cat("\n ",gettext("Variable",domain="R-RcmdrPlugin.TeachStat"),": ",names(data)[ii], "\n", sep="")
          lapply(1:(length(.resumen$.table)),
                 function (x, Table) {
                   cat("\n ",gettext("Group",domain="R-RcmdrPlugin.TeachStat"),": ", names(Table)[x], "\n", sep="")
                   print(Table[[x]], digits = 3)
                   cat("N= ", sum(Table[[x]][, 1]), "\n", sep="") } ,
                 Table= .xx )
        }
      }
    }
  }
  return(invisible(.resumen))
}


tabla.frec.cuantitativa <- function(x, cortes="Sturges", include.lowest=TRUE, right=FALSE,...)
{
  if (is.character(cortes)) {
    cortes <- match.arg(tolower(cortes), c("sturges",
                                           "fd", "freedman-diaconis", "scott"))
    cortes <- switch(cortes, sturges = nclass.Sturges(na.omit(x)),
                     `freedman-diaconis` = , fd = nclass.FD(na.omit(x)), scott = nclass.scott(na.omit(x)),
                     stop("unknown 'cortes' algorithm"))
  }
  ######################
  xx <- cut(x,breaks=cortes,include.lowest=include.lowest, right=right,...)
  lims <- levels(xx)
  indi<-as.numeric(regexpr(",",lims))
  l_inf <- as.numeric(substr(lims,2,indi-1))
  l_sup <- as.numeric(substr(lims,indi+1,nchar(lims)-1))
  xi <- (l_inf + l_sup)/2
  ai <- l_sup - l_inf
  ni<-as.numeric(table(xx))
  
  N <- sum(ni)
  .Frec <- cbind(Li_1=l_inf, Li=l_sup, xi=xi, ni=ni, fi=ni/N, Ni= Ni<- cumsum(ni), Fi=Ni/N, ai= ai, hi= ni/N/ai)
  rownames(.Frec) <- lims
  return(.Frec)
}

### menu: 02.1 Resumen numerico 

# Dependencias:
# Arquivo: 01 Resumenes numericos - v. cualitativa.r
#   tabla.frec.cuantitativa()
# Package: RcmdrMisc
# numSummary()

calcularResumenVariablesContinuas <- function (data, statistics = c("mean", "sd", "se(mean)", "IQR", "quantiles", "cv", "skewness", "kurtosis"), quantiles = c(0, 0.25, 0.5, 0.75, 1), 
                                               groups=NULL, tablaFrecuencia=FALSE, cortes="Sturges",...)
{
  .resumen <- list(.summary=NULL,.table=NULL)
  data<-as.data.frame(data)
  
  if(is.null(groups)) {
    cat("\n-------------------------------\n")
    cat("\n",gettext("Numerical summary",domain="R-RcmdrPlugin.TeachStat"),":\n", sep="")
    print(.resumen$.summary <- numSummary(data, statistics=statistics, quantiles=quantiles,...))
    if (tablaFrecuencia) {
      .resumen$.table <- lapply(data,tabla.frec.cuantitativa, cortes)
      cat("\n-------------------------------\n")
      cat("\n",gettext("Frequency distribution for continuous variables",domain="R-RcmdrPlugin.TeachStat"),":\n", sep="")
      lapply(1:(length(.resumen$.table)), 
             function(x,Table) {
               cat("\n",gettext("Variable",domain="R-RcmdrPlugin.TeachStat"),": ",names(Table)[x],"\n", sep="")
               print(Table[[x]], digits=3)
               cat("N= ",sum(Table[[x]][,4]),"\n", sep="")
             },
             Table=.resumen$.table)
    }
  }
  else {
    cat("\n-------------------------------\n")
    cat("\n",gettext("Numerical summary",domain="R-RcmdrPlugin.TeachStat"),":\n", sep="")
    print(.resumen$.summary <- numSummary(data, groups=groups, statistics=statistics, quantiles=quantiles,...))
    if (tablaFrecuencia) {
      .resumen$.table <- by(data, groups, function(x) lapply(x,tabla.frec.cuantitativa, cortes) )
      cat("\n-------------------------------\n")
      cat("\n",gettext("Frequency distribution for continuous variables",domain="R-RcmdrPlugin.TeachStat"),":\n", sep="")
      if(length(names(data))==1 )
      {
        .xx <- lapply( 1:length(.resumen$.table), function(x) .resumen$.table[[x]] )
        names(.xx) <-  names(.resumen$.table)
        cat("\n ",gettext("Variable",domain="R-RcmdrPlugin.TeachStat"),": ",names(data), "\n", sep="")
        lapply(1:(length(.resumen$.table)),
               function (x, Table) {
                 cat("\n ",gettext("Group",domain="R-RcmdrPlugin.TeachStat"),": ", names(Table)[x], "\n", sep="")
                 print(Table[[x]], digits = 3)
                 cat("N= ", sum(Table[[x]][, 4]), "\n", sep="") } ,
               Table= .xx )
      } else {
        for( ii in 1:length(names(data)))
        {
          .xx <- lapply( 1:length(.resumen$.table), function(x) .resumen$.table[[x]][[ii]] )
          names(.xx) <-  attr(.resumen$.table,"dimnames")$groups
          cat("\n ",gettext("Variable",domain="R-RcmdrPlugin.TeachStat"),": ",names(data)[ii], "\n", sep="")
          lapply(1:(length(.resumen$.table)),
                 function (x, Table) {
                   cat("\n ",gettext("Group",domain="R-RcmdrPlugin.TeachStat"),": ", names(Table)[x], "\n", sep="")
                   print(Table[[x]], digits = 3)
                   cat("N= ", sum(Table[[x]][, 4]), "\n", sep="") } ,
                 Table= .xx )
        }
      }
    }
  }
  return(invisible(.resumen))
}

####### 3 . VARIABLES AGRUPADAS #############################################################3

### menu: 04 Resumen numerico Ponderada

# Dependencias:
# Arquivo:
#
# Package:    Hmisc
#library(Hmisc)
# wtd.mean(x, weights=NULL, normwt="ignored", na.rm=TRUE)
# wtd.var(x, weights=NULL, normwt=FALSE, na.rm=TRUE,method=c('unbiased', 'ML'))
#wtd.quantile(x, weights=NULL, probs=c(0, .25, .5, .75, 1), type=c('quantile','(i-1)/(n-1)','i/(n+1)','i/n'), normwt=FALSE, na.rm=TRUE)
#wtd.table(x, weights=NULL, type=c('list','table'), normwt=FALSE, na.rm=TRUE)
# wtd.rank(x, weights=NULL, normwt=FALSE, na.rm=TRUE)
W.numSummary <- function (data, statistics = c("mean", "sd", "se(mean)", "IQR","quantiles", "cv", "skewness", "kurtosis"), type = c("2","1", "3"), quantiles = c(0, 0.25, 0.5, 0.75, 1), groups=NULL, weights)
{
  cat("\n-------------------------------\n")
  cat("\n",gettext("Numerical summary weighted by",domain="R-RcmdrPlugin.TeachStat"),": ", deparse(substitute(weights)),"\n", sep="")
  weights <- as.double(weights)
  #    sd <- function(x, type, ...) {
  #        apply(as.matrix(x), 2, stats::sd, na.rm = TRUE)
  #    }
  W.sd <- function(x, weights, na.rm=TRUE, ...) {
    x <- as.matrix(x)
    if (na.rm) {
      s <- complete.cases( cbind(x,weights)) #!is.na(x + weights)
      x <- x[s,,drop=FALSE]
      weights <- weights[s]
    }
    sqrt(diag(cov.wt(x,weights,method="ML")$cov))
    #       sqrt(Hmisc::wtd.var(x,weights,normwt=FALSE,method="ML"))
  }
  #    IQR <- function(x, type, ...) {
  #        apply(as.matrix(x), 2, stats::IQR, na.rm = TRUE)
  #    }
  W.IQR <- function(x, weights, ...) {
    diff(apply(as.matrix(x), 2, Hmisc::wtd.quantile, weights, probs=c(.25,.75), type="i/n", na.rm = TRUE))
  }
  #    std.err.mean <- function(x, ...) {
  #        x <- as.matrix(x)
  #        sd <- sd(x)
  #        n <- colSums(!is.na(x))
  #        sd/sqrt(n)
  #    }
  W.std.err.mean <- function(x,weights, ...) {
    x <- as.matrix(x)
    sd <- W.sd(x,weights)
    n <- sum(weights)
    sd/sqrt(n)
  }
  #    cv <- function(x, ...) {
  #        x <- as.matrix(x)
  #        mean <- colMeans(x, na.rm = TRUE)
  #        sd <- sd(x)
  #        if (any(x <= 0, na.rm = TRUE))
  #            warning("not all values are positive")
  #        cv <- sd/mean
  #        cv[mean <= 0] <- NA
  #        cv
  #    }
  W.cv <- function(x, weights, ...) {
    x <- as.matrix(x)
    mean <- sapply(as.data.frame(x), Hmisc::wtd.mean, weights, na.rm = TRUE)
    sd <- W.sd(x,weights)
    if (any(weights <= 0, na.rm = TRUE))
      warning("not all values are positive")
    W.cv <- sd/mean
    W.cv[mean <= 0] <- NA
    W.cv
  }
  #    skewness <- function(x, type, ...) {
  #        if (is.vector(x))
  #            return(e1071::skewness(x, type = type, na.rm = TRUE))
  #        apply(x, 2, skewness, type = type)
  #    }
  ## equivalente a type=1, m_3/m_2^1.5
  W.skewness <- function(x, weights, ...) {
    skew <- function(x, weights, na.rm=TRUE)
      sum(((x-wtd.mean(x, weights))^3)*weights)/sum(weights)/ W.sd(x, weights)^3
    if (is.vector(x))
      return(skew(x, weights, na.rm = TRUE))
    apply(x, 2, skew, weights=weights)
  }
  #    kurtosis <- function(x, type, ...) {
  #        if (is.vector(x))
  #            return(e1071::kurtosis(x, type = type, na.rm = TRUE))
  #        apply(x, 2, kurtosis, type = type)
  #    }
  ## equivalente a type=1, m_4/m_2^2 - 3
  W.kurtosis <- function(x, weights, ...) {
    kurt <- function(x, weights, na.rm=TRUE)
      sum(((x-Hmisc::wtd.mean(x, weights))^4)*weights)/sum(weights)/ W.sd(x, weights)^4 - 3
    if (is.vector(x))
      return(kurt(x, weights, na.rm = TRUE))
    apply(x, 2, kurt, weights, na.rm=TRUE)
  }
  data <- as.data.frame(data)
  #    if (!missing(groups)) {
  if (!missing(groups) & !is.null(groups)) {
    groups <- as.factor(groups)
    counts <- table(groups)
    if (any(counts == 0)) {
      levels <- levels(groups)
      warning("the following groups are empty: ", paste(levels[counts ==
                                                                 0], collapse = ", "))
      groups <- factor(groups, levels = levels[counts !=
                                                 0])
    }
  }
  variables <- names(data)
  if (missing(statistics))
    statistics <- c("mean", "sd", "quantiles", "IQR")
  statistics <- match.arg(statistics, c("mean", "sd", "se(mean)",
                                        "IQR", "quantiles", "cv", "skewness", "kurtosis"), several.ok = TRUE)
  type <- match.arg(type)
  type <- as.numeric(type)
  ngroups <- if (missing(groups) | is.null(groups))
    1
  else length(grps <- levels(groups))
  quantiles <- if ("quantiles" %in% statistics)
    quantiles
  else NULL
  quants <- if (length(quantiles) > 0)
    paste(100 * quantiles, "%", sep = "")
  else NULL
  nquants <- length(quants)
  stats <- c(c("mean", "sd", "se(mean)", "IQR", "cv", "skewness",
               "kurtosis")[c("mean", "sd", "se(mean)", "IQR", "cv",
                             "skewness", "kurtosis") %in% statistics], quants)
  nstats <- length(stats)
  nvars <- length(variables)


  #### chequeo de weights
  if(missing(weights) | sum(!is.na(weights))<2) {
    warning("no weights avaliable.")
    weights <- rep(1.0, dim(data)[1])
  }


  result <- list()
  if ((ngroups == 1) && (nvars == 1) && (length(statistics) ==
                                         1)) {
    if (statistics == "quantiles")
      table <- Hmisc::wtd.quantile(data[, variables], weights=weights, probs = quantiles, type="i/n", na.rm = TRUE)
    else {
      stats <- statistics
      stats[stats == "se(mean)"] <- "std.err.mean"
      stats <- switch(stats, mean="wtd.mean", sd="W.sd", IQR="W.IQR", std.err.mean="W.std.err.mean", cv="W.cv", skewness="W.skewness", kurtosis="W.kurtosis")
      table <- do.call(stats, list(x = data[, variables], weights, na.rm = TRUE))
      names(table) <- statistics
    }
    NAs <- sum(is.na(data[, variables]))
    n <- nrow(data) - NAs
    result$type <- 1
  }
  else if ((ngroups > 1) && (nvars == 1) && (length(statistics) ==
                                             1)) {
    if (statistics == "quantiles") {
      table <- matrix(unlist(tapply(data[, variables],
                                    groups, Hmisc::wtd.quantile, weights=weights, probs = quantiles, type="i/n", na.rm = TRUE)), ngroups, nquants, byrow = TRUE)
      rownames(table) <- grps
      colnames(table) <- quants
    }
    else table <- tapply(data[, variables], groups, statistics,
                         na.rm = TRUE, type = type)
    NAs <- tapply(data[, variables], groups, function(x) sum(is.na(x)))
    n <- table(groups) - NAs
    result$type <- 2
  }
  else if ((ngroups == 1)) {
    X <- as.matrix(data[, variables])
    table <- matrix(0, nvars, nstats)
    rownames(table) <- if (length(variables) > 1)
      variables
    else ""
    colnames(table) <- stats
    if ("mean" %in% stats)
      table[, "mean"] <- sapply(as.data.frame(X), Hmisc::wtd.mean, weights, na.rm = TRUE)
    #            table[, "mean"] <- colMeans(X, na.rm = TRUE)
    if ("sd" %in% stats)
      table[, "sd"] <- W.sd(X, weights)
    if ("se(mean)" %in% stats)
      table[, "se(mean)"] <- W.std.err.mean(X, weights)
    if ("IQR" %in% stats)
      table[, "IQR"] <- W.IQR(X, weights)
    if ("cv" %in% stats)
      table[, "cv"] <- W.cv(X, weights)
    if ("skewness" %in% statistics)
      table[, "skewness"] <- W.skewness(X, weights)
    if ("kurtosis" %in% statistics)
      table[, "kurtosis"] <- W.kurtosis(X, weights)
    if ("quantiles" %in% statistics) {
      #            table[, quants] <- t(apply(data[, variables, drop = FALSE], 2, quantile, probs = quantiles, na.rm = TRUE))
      table[, quants] <- t(apply(data[, variables, drop = FALSE], 2, Hmisc::wtd.quantile, probs = quantiles, weights=weights, type="i/n", na.rm = TRUE))
    }
    NAs <- colSums(is.na(data[, variables, drop = FALSE]))
    n <- nrow(data) - NAs
    result$type <- 3
  }
  else {
    data <- cbind(data,weights)                #### new
    table <- array(0, c(ngroups, nstats, nvars), dimnames = list(Group = grps,
                                                                 Statistic = stats, Variable = variables))
    NAs <- matrix(0, nvars, ngroups)
    rownames(NAs) <- variables
    colnames(NAs) <- grps
    for (variable in variables) {
      if ("mean" %in% stats)
        #                table[, "mean", variable] <- tapply(data[, variable],
        #                  groups, Hmisc::wtd.mean, weights=weights, na.rm = TRUE)
        table[, "mean", variable] <- by(data[, c(variable,"weights")],
                                        groups, function(x) Hmisc::wtd.mean(x[,1],x[,2],na.rm=TRUE) )
      if ("sd" %in% stats)
        #                table[, "sd", variable] <- tapply(data[, variable],
        #                  groups, W.sd, weights=weights, na.rm = TRUE)
        table[, "sd", variable] <- by(data[, c(variable,"weights")],
                                      groups,function(x) W.sd(x[,1],x[,2],na.rm=TRUE) )
      if ("se(mean)" %in% stats)
        #                table[, "se(mean)", variable] <- tapply(data[,
        #                  variable], groups, W.std.err.mean, weights=weights, na.rm = TRUE)
        table[, "se(mean)", variable] <- by(data[, c(variable,"weights")],
                                            groups,function(x) W.std.err.mean(x[,1],x[,2],na.rm=TRUE) )
      if ("IQR" %in% stats)
        #                table[, "IQR", variable] <- tapply(data[, variable],
        #                  groups, W.IQR, weights=weights, na.rm = TRUE)
        table[, "IQR", variable] <- by(data[, c(variable,"weights")],
                                       groups,function(x) W.IQR(x[,1],x[,2],na.rm=TRUE) )
      if ("cv" %in% stats)
        #                table[, "cv", variable] <- tapply(data[, variable],
        #                  groups, W.cv, weights=weights)
        table[, "cv", variable] <- by(data[, c(variable,"weights")],
                                      groups,function(x) W.cv(x[,1],x[,2],na.rm=TRUE) )
      if ("skewness" %in% stats)
        #                table[, "skewness", variable] <- tapply(data[,
        #                  variable], groups, W.skewness, weights=weights)
        table[, "skewness", variable] <- by(data[, c(variable,"weights")],
                                            groups,function(x) W.skewness(x[,1],x[,2],na.rm=TRUE) )
      if ("kurtosis" %in% stats)
        #                table[, "kurtosis", variable] <- tapply(data[,
        #                  variable], groups, W.kurtosis, weights=weights)
        table[, "kurtosis", variable] <- by(data[, c(variable,"weights")],
                                            groups,function(x) W.kurtosis(x[,1],x[,2],na.rm=TRUE) )
      if ("quantiles" %in% statistics) {
        #                res <- matrix(unlist(tapply(data[, variable],
        #                  groups, wtd.quantile, weights=weights, probs = quantiles, na.rm = TRUE, type="i/n")), ngroups, nquants, byrow = TRUE)
        res <- by(data[, c(variable,"weights")],
                  groups,function(x) wtd.quantile(x=x[,1],weights=x[,2],probs=quantiles,  type="i/n", na.rm=TRUE) )
        table[, quants, variable] <- t(simplify2array(res))
      }
      NAs[variable, ] <- tapply(data[, variable], groups,
                                function(x) sum(is.na(x)))
    }
    if (nstats == 1)
      table <- table[, 1, ]
    if (nvars == 1)
      table <- table[, , 1]
    n <- table(groups)
    n <- matrix(n, nrow = nrow(NAs), ncol = ncol(NAs), byrow = TRUE)
    n <- n - NAs
    result$type <- 4
  }
  result$table <- table
  result$statistics <- statistics
  result$n <- n
  if (any(NAs > 0))
    result$NAs <- NAs
  class(result) <- "numSummary"

  result
}



###4. DATOS TABULADOS#########################################################3


### menu: 05 Resumen numerico - V. Agrupada

# Dependencias:
# Arquivo: 01 Resumenes numericos - v. cualitativa.r
#   tabla.frec.cualitativa()
#   pprint()
# Package: RcmdrMisc
# numSummary()
calcularResumenDatosTabulados<- function(l_inf, l_sup, ni, statistics = c("mean", "sd", "se(mean)", "IQR", "quantiles", "cv", "skewness", "kurtosis"), quantiles = c(0, 0.25, 0.5, 0.75, 1),tablaFrecuencia=FALSE)
{
  .resumen <- list(.summary=NULL,.table=NULL)
  xi <- (l_inf + l_sup)/2
  ai <- l_sup - l_inf

  N <- sum(ni)
  .resumen$.table <- cbind(Li_1=l_inf, Li=l_sup, xi=xi, ni=ni, fi=ni/N, Ni= Ni<- cumsum(ni), Fi=Ni/N, ai= ai, hi= ni/N/ai)
  ni <- as.numeric(ni)
  .resumen$.summary <- W.numSummary(data=as.data.frame(xi), weights=ni, statistics=statistics, quantiles=quantiles)
  if ("mode" %in% statistics)
  {
    if("quantiles" %in% statistics)
    {
      if(is.null(dim(.resumen$.summary$table)))
      {
        .resumen$.summary$table <- as.matrix(t(.resumen$.summary$table))
        rownames(.resumen$.summary$table) <- ""
      } 
      .resumen$.summary$table[, (length(statistics)-1):(length(statistics) + length(quantiles) - 2)] <- sapply(quantiles, percentil, tabla = .resumen$.table)
    }
    .resumen$.summary$table <- cbind(.resumen$.summary$table, mode=moda(.resumen$.table) )
  }else if ("quantiles" %in% statistics)
  {
    if(is.null(dim(.resumen$.summary$table)))
    {
      .resumen$.summary$table <- as.matrix(t(.resumen$.summary$table))
      rownames(.resumen$.summary$table) <- ""
    } 
    .resumen$.summary$table[, (length(statistics)):(length(statistics) + length(quantiles) - 1)] <- sapply(quantiles, percentil, tabla = .resumen$.table)
  }
  .resumen$.summary$n <- N


  cat("\n-------------------------------\n")
  cat("\n",gettext("Numerical summary",domain="R-RcmdrPlugin.TeachStat"),":\n", sep="")
  print(.resumen$.summary)

  if (tablaFrecuencia) {

    cat("\n-------------------------------\n")
    cat("\n",gettext("Frequency distribution for the tabulated variable",domain="R-RcmdrPlugin.TeachStat"),":\n", sep="")
    dimnames(.resumen$.table)[[1]] <- rep("",nrow(.resumen$.table))
    print(.resumen$.table)
  }
  return(invisible(.resumen))
}


#Cálculo percentil p
percentil<-function(p, tabla){
  N <- sum(tabla[,"ni"])
  Np<-N*p
  indice<-min(which(tabla[,"Ni"]>=Np))
  if(indice==1){
    xp<-tabla[indice,"Li_1"]+Np/tabla[indice,"ni"]*tabla[indice,"ai"]
  } else{
    xp<-tabla[indice,"Li_1"]+(Np-tabla[indice-1,"Ni"])/tabla[indice,"ni"]*tabla[indice,"ai"]
  }
  return(xp)
}

#Cálculo de la moda
moda <- function(tabla) {
  indicem<-which.max(tabla[,"hi"])
  if(indicem==1){
    xm<-tabla[2,"Li_1"]
  } else if(indicem==nrow(tabla)){
    xm<-tabla[indicem,"Li_1"]
  } else{
    xm<-tabla[indicem,"Li_1"]+tabla[indicem+1,"hi"]/(tabla[indicem+1,"hi"]+tabla[indicem-1,"hi"])*tabla[indicem,"ai"]
  }
  return(xm)
}
