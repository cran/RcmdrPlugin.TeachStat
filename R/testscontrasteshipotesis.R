
##Funcion calcular contraste de hipotesis para la media con varianza conocida

MKV.test<-function(x, mu=0, sd, alternative = c("two.sided", "less", "greater"),
                   conf.level = 0.95, ... ) {

  if(missing(sd)) stop("You must specify a Standard Deviation of the population")
  if(sd<=0) stop("Population Standard Deviation must be positive")

  
  alternative <- match.arg(alternative)
  dname <- deparse(substitute(x))
  xok<-!is.na(x)
  x<-x[xok]
  n<-length(x)
  mx<-mean(x)
  vx<-var(x)
  
  if (n < 1) 
    stop("not enough observations")
  stderr <- sd/sqrt(n)
  if (stderr/sd*sqrt(vx) < 10 * .Machine$double.eps * abs(mx)) 
    stop("data are essentially constant")

  z <- (mx-mu)/stderr

  out <- list(statistic=c(z=z))
  class(out) <- 'htest'
  out$parameter <- c(n=n,"Std. Dev." = sd,
                     "Std. Dev. of the sample mean" = stderr)

  out$p.value <- switch(alternative,
                        two.sided = 2*pnorm(abs(z),lower.tail=FALSE),
                        less = pnorm(z),
                        greater = pnorm(z, lower.tail=FALSE) )

  out$conf.int <- switch(alternative,
                         two.sided = mx +
                           c(-1,1)*qnorm(1-(1-conf.level)/2)*stderr,
                         less = c(-Inf, mx+qnorm(conf.level)*stderr),
                         greater = c(mx-qnorm(conf.level)*stderr, Inf)
  )
  attr(out$conf.int, "conf.level") <- conf.level

  out$estimate <- c("mean of x" = mx)
  out$null.value <- c("mean" = mu)
  out$alternative <- alternative
  out$method <- "One Sample z-test for Mean with known population Variance"
  out$data.name <- dname
  names(out$estimate) <- paste("mean of", out$data.name)
  return(out)
}

### Contraste de Hipotesis y Intervalo confianza para la varianza con media desconocida

VUM.test<-function (x, sigma = 1, sigmasq = sigma^2,
                    alternative = c("two.sided", "less", "greater"),
                    conf.level = 0.95, ...) {
  alternative <- match.arg(alternative)
  sigma <- sqrt(sigmasq)
  if(sigma<=0) stop("Standard Deviation must be positive (>0)")
  dname<-deparse(substitute(x))
  xok<-!is.na(x)
  x<-x[xok]
  n <- length(x)
  mx<-mean(x)
  vx<-var(x)
  if (n < 2) 
    stop("not enough observations")
  stderr <- sqrt(vx/n)
  if (stderr < 10 * .Machine$double.eps * abs(mx)) 
    stop("data are essentially constant")
  
  
  xs <- vx*(n-1)/sigma^2
  out <- list(statistic = c("X-squared" = xs))
  class(out) <- "htest"
  out$parameter <- c(df = n-1)
  minxs <- min(c(xs, 1/xs))
  maxxs <- max(c(xs, 1/xs))
  PVAL <- pchisq(xs, df = n - 1)

  out$p.value <- switch(alternative,
                        two.sided = 2*min(PVAL, 1 - PVAL),
                        less = PVAL,
                        greater = 1 - PVAL)
  out$conf.int <- switch(alternative,
                         two.sided = xs * sigma^2 *
                           1/c(qchisq(1-(1-conf.level)/2, df = n-1), qchisq((1-conf.level)/2, df
                                                                            = n-1)),
                         less = c(0, xs * sigma^2 /
                                    qchisq(1-conf.level, df = n-1)),
                         greater = c(xs * sigma^2 /
                                       qchisq(conf.level, df = n-1), Inf))
  attr(out$conf.int, "conf.level") <- conf.level
  out$estimate <- c("var of x" = vx)
  out$null.value <- c(variance = sigma^2)
  out$alternative <- alternative
  out$method <- "One sample Chi-squared test for variance with unknown population mean"
  out$data.name <- dname
  names(out$estimate) <- paste("var of", out$data.name)
  return(out)
}

### Contraste de Hipotesis y Intervalo confianza para la varianza con media conocida

VKM.test<-function (x, sigma = 1, sigmasq = sigma^2,mu,
                    alternative = c("two.sided", "less", "greater"),
                    conf.level = 0.95, ...) {
  if(missing(mu)) stop("You must specify a mean of the population")
  alternative <- match.arg(alternative)
  sigma <- sqrt(sigmasq)
  if(sigma<=0) stop("Standard Deviation must be positive (>0)")
  dname<-deparse(substitute(x))
  xok<-!is.na(x)
  x<-x[xok]
  n <- length(x)
  mx<-mean(x)
  vx<-var(x)
  if (n < 1) 
    stop("not enough observations")
  stderr <- sqrt(vx/n)
  if (stderr < 10 * .Machine$double.eps * abs(mx)) 
    stop("data are essentially constant")
  
  x_mu<-sum((x-mu)^2)
  xs <- x_mu/sigma^2
  out <- list(statistic = c("X-squared" = xs))
  class(out) <- "htest"
  out$parameter <- c(df = n, "Mean" = mu)
  minxs <- min(c(xs, 1/xs))
  maxxs <- max(c(xs, 1/xs))
  PVAL <- pchisq(xs, df = n)

  out$p.value <- switch(alternative,
                        two.sided = 2*min(PVAL, 1 - PVAL),
                        less = PVAL,
                        greater = 1 - PVAL)
  out$conf.int <- switch(alternative,
                         two.sided = xs * sigma^2 *
                           1/c(qchisq(1-(1-conf.level)/2, df = n), qchisq((1-conf.level)/2, df
                                                                          = n)),
                         less = c(0, xs * sigma^2 /
                                    qchisq(1-conf.level, df = n)),
                         greater = c(xs * sigma^2 /
                                       qchisq(conf.level, df = n), Inf))
  attr(out$conf.int, "conf.level") <- conf.level
  out$estimate <- c("var of x" = vx)
  out$null.value <- c(variance = sigma^2)
  out$alternative <- alternative
  out$method <- "One sample Chi-squared test for variance with known population mean"
  out$data.name <- dname
  names(out$estimate) <- paste("var of", out$data.name)
  return(out)
}


### Funcion one and two clasical Proportion test

Cprop.test<-function(ex, nx, ey=NULL, ny=NULL, p.null=0.5, alternative = c("two.sided", "less", "greater"),
                     conf.level = 0.95, ... ) {

  alternative <- match.arg(alternative)
  # To avoid integer overflows (only needed tochange the type of one of them)
  ex<-as.double(ex)
  
  if (!missing(p.null) && (length(p.null) != 1 || abs(p.null)>1 || is.na(p.null)))
    stop("'p.null' must be a single number between -1 and 1")
  if (!missing(conf.level) && (length(conf.level) != 1 || !is.finite(conf.level) ||
                               conf.level < 0 || conf.level > 1))
    stop("'conf.level' must be a single number between 0 and 1")
  if((is.null(ey) && !is.null(ny)) || (is.null(ny) && !is.null(ey)) )
    stop("'ey' or 'ny' are missing")
  if(is.null(ey) && (p.null<=0 || p.null >=1))
    stop("With one sample, 'p.null' must be > 0 and < 1")
   if((is.null(ey) && (ex*(nx-ex)==0)) || (!is.null(ey) && (ex*(nx-ex)*ey*(ny-ey)==0)) )
     stop("There must be at least 1 success and 1 failure in each sample")
  if(nx<1) stop("not enough 'x' observations")
  if((ex>nx) || (!is.null(ey) && (ey>ny)))
    stop("The number of tries must be greater than or equal to the number of success")
  
  
  
  #   if (!is.null(ey)) {
  #     dname <- "and"
  #   }
  #   else {
  #     dname <- deparse(substitute(ex))
  #   }

  if (is.null(ey)) {
    est<-ex/nx
    z <- (est-p.null)/sqrt(p.null*(1-p.null)/nx)

    errest<-sqrt(ex*(nx-ex)/nx^3)
    pmini<-0


    parameter <- c(nx=nx,"null probability" = p.null)
    method <- "Classical One Sample Proportion test"
    estimate <- c("proportion" = ex/nx)
    nullvalue <- c("proportion" = p.null)

  }else {
    if(ny<1) stop("not enough 'y' observations")
    
    est<-ex/nx-ey/ny
    pxy<-(ex+ey)/(nx+ny)

    z <- (est-p.null)/sqrt(pxy*(1-pxy)*(1/nx+1/ny))

    errest<-sqrt(ex*(nx-ex)/nx^3 + ey*(ny-ey)/ny^3)
    pmini<--1

    parameter <- c("nx"=nx,"ny"=ny,"null difference" = p.null)
    method <- "Classical Two Sample Proportions test"
    estimate <- c("proportion in Group 1" = ex/nx,"proportion in Group 2" =ey/ny)
    nullvalue <- c("difference in proportions" = p.null)
  }

  out <- list(statistic=c(z=z))
  class(out) <- 'htest'

  out$parameter <- parameter
  out$method <- method
  out$estimate <- estimate
  out$null.value <- nullvalue

  out$p.value <- switch(alternative,
                        two.sided = 2*pnorm(abs(z),lower.tail=FALSE),
                        less = pnorm(z),
                        greater = pnorm(z, lower.tail=FALSE) )

  out$conf.int <- switch(alternative,
                         two.sided = c(max(pmini,est - qnorm(1-(1-conf.level)/2)*errest),
                                       min(1,est + qnorm(1-(1-conf.level)/2)*errest)),
                         less = c(pmini, min(1,est+qnorm(conf.level)*errest)),
                         greater = c(max(pmini,est-qnorm(conf.level)*errest),1)
  )
  attr(out$conf.int, "conf.level") <- conf.level

  out$alternative <- alternative
  # out$data.name <- dname

  return(out)
}



### Funcion calculo Two Samples diferencia medias independiente varianzas conocidas
DMKV.test<-function(x, y, difmu=0, sdx, sdy,
                    alternative = c("two.sided", "less", "greater"),
                    conf.level = 0.95, ... ) {

  if(missing(sdx) || missing(sdy)) stop("You must specify both Standard Deviations of the population")
  if(sdx<=0) stop("Population Standard Deviation of 'x' must be positive")
  if(sdy<=0) stop("Population Standard Deviation of 'y' must be positive")

  alternative <- match.arg(alternative)
  dnamex <- deparse(substitute(x))
  dnamey <- deparse(substitute(y))
  xok<-!is.na(x)
  yok<-!is.na(y)
  x<-x[xok]
  y<-y[yok]

  nx <- length(x)
  ny <- length(y)
  mx<-mean(x)
  my<-mean(y)
  vx<-var(x)
  vy<-var(y)
  
  if (nx < 1) 
    stop("not enough 'x' observations")
  if (ny < 1) 
    stop("not enough 'y' observations")
  stderrx <- sqrt(vx/nx)
  stderry <- sqrt(vy/ny)
  stderr <- sqrt(stderrx^2 + stderry^2)
  if (stderr < 10 * .Machine$double.eps * max(abs(mx),abs(my))) 
    stop("data are essentially constant")

  z <- (mx-my-difmu)/sqrt(sdx^2/nx+sdy^2/ny)

  out <- list(statistic=c(z=z))
  class(out) <- 'htest'

  out$parameter <- c("nx"=nx, "ny"=ny, "Std. Dev. x" = sdx, "Std. Dev. y" = sdy)

  out$p.value <- switch(alternative,
                        two.sided = 2*pnorm(abs(z),lower.tail=FALSE),
                        less = pnorm(z),
                        greater = pnorm(z, lower.tail=FALSE) )

  out$conf.int <- switch(alternative,
                         two.sided = mx-my +
                           c(-1,1)*qnorm(1-(1-conf.level)/2)*sqrt(sdx^2/nx+sdy^2/ny),
                         less = c(-Inf, mx-my+qnorm(conf.level)*sqrt(sdx^2/nx+sdy^2/ny)),
                         greater = c(mx-my-qnorm(conf.level)*sqrt(sdx^2/nx+sdy^2/ny), Inf)
  )
  attr(out$conf.int, "conf.level") <- conf.level

  out$estimate <- c("mean of x" = mx,"mean of y" = my)
  out$null.value <- c("difference in means" = difmu)
  out$alternative <- alternative
  out$method <- "Two Sample z-test for difference of means with known population Variances"
  out$data.name <- c(dnamex,dnamey)
  names(out$estimate) <- paste("mean of", out$data.name)
  out$data.name <- paste(dnamex, " and ", dnamey)

  return(out)
}
