twoOrMoreLevelFactors<- function(){
  if(!activeDataSetP()){return(NULL)}
  else{
    datasetactivo <- ActiveDataSet()
    datasetactivo<-get(datasetactivo)
    factores<-NULL
    for(i in names(datasetactivo)){ if(is.factor(datasetactivo[,i])){factores<-c(factores,i)}}
    twoormorelevelsfactor<-NULL
    for(i in factores){if(length(levels(datasetactivo[,i]))>1){twoormorelevelsfactor<-c(twoormorelevelsfactor,i)}}
    return(twoormorelevelsfactor)}
}



twoOrMoreLevelFactorsP<- function(){
  if(!activeDataSetP()){return(FALSE)}
  else{
    datasetactivo <- ActiveDataSet()
    datasetactivo<-get(datasetactivo)
    factores<-NULL
    for(i in names(datasetactivo)){ if(is.factor(datasetactivo[,i])){factores<-c(factores,i)}}
    twoormorelevelsfactor<-NULL
    for(i in factores){if(length(levels(datasetactivo[,i]))>1){twoormorelevelsfactor<-c(twoormorelevelsfactor,i)}}
    if(length(twoormorelevelsfactor)==0){return(FALSE)}
    else{return(TRUE)}}
}


# Needed functions for the new Random Variable menu
listDistrs <- function(class="UnivariateDistribution", envir=.GlobalEnv, ...) {
  # List all distribution of a particular class. By default the class is "UnivariateDistribution"
  objects <- ls(envir=envir, all.names = TRUE, ...)
  if (length(objects) == 0) NULL
  else names(which(sapply(objects,
                          function(.x) is(get(.x,envir=envir), class))))
}

# listDistrsP<- function(class="UnivariateDistribution"){
#   obj <- listDistrs(class)
#   (!is.null(obj)) && length(obj) >= 1
#   # if(length(obj)==0){
#   #   return(FALSE)
#   # } else{
#   #   return(TRUE)
#   # }
# }

DiscreteDistrsP<- function(){
  obj <- listDistrs("DiscreteDistribution")
  (!is.null(obj)) && length(obj) >= 1
  # if(length(obj)==0){
  #   return(FALSE)
  # } else{
  #   return(TRUE)
  # }
}

AbscontDistrsP<- function(){
  obj <- listDistrs("AbscontDistribution")
  (!is.null(obj)) && length(obj) >= 1
  # if(length(obj)==0){
  #   return(FALSE)
  # } else{
  #   return(TRUE)
  # }
}


listTypesVariables <- function(dataSet) {
  vars <- names(get(dataSet, envir=.GlobalEnv))
  cmd <- paste0("with(",dataSet,",c(",paste0("paste0(class(",vars,"),collapse=\" \")",collapse=","),"))")
  vtypes <- eval(parse(text=cmd))
  paste0(vars, "[",vtypes ,"]")
}

isstring <- function (x) 
{
  return(is.character(x) && length(x)==1)
}

# Based on checkNames() in package IndexNumR

checkNames <- function (x, namesVector) 
{
  goodNames <- colnames(x)
  badNames <- namesVector[!(namesVector %in% goodNames)]
  if (length(badNames) >= 1) {
    err <- paste("The specified name(s) -", paste('"',badNames,'"', 
                                                  collapse = ", ", sep=""), "- are not column names of the input data frame.")
    return(list(result = FALSE, message = err))
  }
  else {
    return(list(result = TRUE))
  }
}


.panel.mingle <- function (dots, element) 
{
  # An exact copy from distr package since it is not exported from that package
  pF <- dots[[element]]
  if (is.list(pF)) 
    return(pF)
  pFr <- if (typeof(pF) == "symbol") 
    eval(pF)
  else {
    pFc <- as.call(pF)
    if (as.list(pFc)[[1]] == "list") {
      lis <- vector("list", length(as.list(pFc)) - 1)
      for (i in 1:length(lis)) {
        lis[[i]] <- pFc[[i + 1]]
      }
      lis
    }
    else pF
  }
  return(pFr)
}

plotRegions <- function(D, add=FALSE, regions = NULL, col = "gray", legend = TRUE,
                        legend.pos = "topright", to.draw.arg=1, verticals=FALSE, ngrid=1000,
                        cex.points=par("cex"), mfColRow=FALSE, lwd=par("lwd"),...){
  
  dots <- match.call(call = sys.call(0), expand.dots = FALSE)$...
  if (!is.null(dots[["panel.first"]])) {
    pF <- .panel.mingle(dots, "panel.first")
  } else if(to.draw.arg==1){
    pF <- quote(abline(h = 0, col = "gray"))
  } else if(to.draw.arg==2){
    pF <- quote(abline(h = 0:1, col = "gray"))
  } else{
    pF<-NULL
  }
  
  dots$panel.first <- pF
  
  if(!add){
    do.call(distr::plot,c(list(D, to.draw.arg=to.draw.arg, cex.points=cex.points, mfColRow= mfColRow,
                        verticals=verticals),dots))
  }
  discrete <- is(D,"DiscreteDistribution")
  if (discrete) {
    x <- support(D)
    if (hasArg("xlim")) {
      if (length(xlim) != 2) 
        stop("Wrong length of Argument xlim")
      x <- x[(x >= xlim[1]) & (x <= xlim[2])]
    }
    if (!is.null(regions)) {
      col <- rep(col, length = length(regions))
      for (i in 1:length(regions)) {
        region <- regions[[i]]
        which.xs <- (x > region[1] & x <= region[2])
        xs <- x[which.xs]
        ps <- d(D)(x)[which.xs]
        lines(xs, ps, type="h", col = col[i], lwd=3*lwd, ...)
        points(xs, ps, pch=16, col = col[i], cex=2*cex.points, ...)
      }
      if (legend) {
        if (length(unique(col)) > 1) {
          legend(legend.pos, title = if (length(regions) > 
                                         1) 
            "Regions"
            else "Region", legend = sapply(regions, function(region) {
              paste(round(region[1], 2), "to", round(region[2], 
                                                     2))
            }), col = col, pch = 15, pt.cex = 2.5, inset = 0.02)
        }
        else {
          legend(legend.pos, title = if (length(regions) > 
                                         1) 
            "Regions"
            else "Region", legend = sapply(regions, function(region) {
              paste(round(region[1], 2), "to", round(region[2], 
                                                     2))
            }), inset = 0.02)
        }
      }
    }
  }
  else {
    lower0 <- getLow(D, eps = getdistrOption("TruncQuantile") * 
                       2)
    upper0 <- getUp(D, eps = getdistrOption("TruncQuantile") * 
                      2)
    me <- distr::q.l(D)(1/2)
    s <- distr::q.l(D)(3/4) - distr::q.l(D)(1/4)
    lower1 <- me - 6 * s
    upper1 <- me + 6 * s
    lower <- max(lower0, lower1)
    upper <- min(upper0, upper1)
    dist <- upper - lower
    if (hasArg("xlim")) {
      if (length(xlim) != 2) 
        stop("Wrong length of Argument xlim")
      x <- seq(xlim[1], xlim[2], length = ngrid)
    }
    else x <- seq(from = lower - 0.1 * dist, to = upper + 0.1 * dist, length = ngrid)
    if (!is.null(regions)) {
      col <- rep(col, length = length(regions))
      for (i in 1:length(regions)) {
        region <- regions[[i]]
        which.xs <- (x >= region[1] & x <= region[2])
        xs <- x[which.xs]
        ps <- d(D)(x)[which.xs]
        xs <- c(xs[1], xs, xs[length(xs)])
        ps <- c(0, ps, 0)
        polygon(xs, ps, col = col[i])
      }
      if (legend) {
        if (length(unique(col)) > 1) {
          legend(legend.pos, title = if (length(regions) > 
                                         1) 
            "Regions"
            else "Region", legend = sapply(regions, function(region) {
              paste(round(region[1], 2), "to", round(region[2], 
                                                     2))
            }), col = col, pch = 15, pt.cex = 2.5, inset = 0.02)
        }
        else {
          legend(legend.pos, title = if (length(regions) > 
                                         1) 
            "Regions"
            else "Region", legend = sapply(regions, function(region) {
              paste(round(region[1], 2), "to", round(region[2], 
                                                     2))
            }), inset = 0.02)
        }
      }
    }
  }
  return(invisible(NULL))
}


characRV <- function(D, charact=c("expectation", "median", "sd", "IQR", "skewness", "kurtosis",
                                  "moment","cmoment"), moment=1, cmoment=2){
  if (missing(charact)) 
    charact <- c("expectation", "sd")
  charact <- match.arg(charact, c("expectation", "median", "sd", "IQR", "skewness", "kurtosis",
                                  "moment","cmoment"), several.ok = TRUE)
  moment <- if ("moment" %in% charact) 
    moment
  else NULL
  cmoment <- if ("cmoment" %in% charact) 
    cmoment
  else NULL
  mom <- if(!is.null(moment)) 
    paste("alpha_",moment, sep = "")
  else NULL
  cmom <- if(!is.null(cmoment))
    paste("mu_",cmoment, sep = "")
  else NULL
  chars <- c(c("expectation", "median", "sd", "IQR", "skewness", "kurtosis")[c("expectation", "median",
                                                                               "sd", "IQR", "skewness", "kurtosis") %in% charact],
             mom, cmom)
  nchars <- length(chars)
  table <- matrix(0, 1, nchars)
  rownames(table) <- gsub("[[:space:]]","",deparse(substitute(D)))
  
  
  colnames(table) <- chars
  if ("expectation" %in% chars) 
    table[, "expectation"] <- distrEx::E(D)
  if ("median" %in% chars) 
    table[, "median"] <- distrEx::median(D)
  if ("sd" %in% chars) 
    table[, "sd"] <- distrEx::sd(D)
  if ("IQR" %in% chars) 
    table[, "IQR"] <- distrEx::IQR(D)
  if ("skewness" %in% chars) 
    table[, "skewness"] <- distrEx::skewness(D)
  if ("kurtosis" %in% chars) 
    table[, "kurtosis"] <- distrEx::kurtosis(D)
  if ("moment" %in% charact)
    table[, mom] <- distrEx::E(D,fun = function(x){x^moment})
  if ("cmoment" %in% charact)
    table[, cmom] <- distrEx::E(D,fun = function(x){(x-distrEx::E(D))^cmoment})
  print(table)
  return(invisible(table))
}


# An exact copy from Rcmdr package since they are not exported from that package

removeRedundantExtension <- function(file){
  find.ext <- regexpr("\\.(?:.(?!\\.))+$", file, perl=TRUE)
  if (find.ext == -1) return(file)
  ext <- substring(file, find.ext, find.ext + attr(find.ext, "match.length"))
  file <- sub(paste0(ext, ext, "$"), ext, file)
  file
}

col2hex <- function (col) 
{
  hexcolor <- colorConverter(toXYZ = function(hex, ...) {
    rgb <- t(col2rgb(hex))/255
    colorspaces$sRGB$toXYZ(rgb, ...)
  }, fromXYZ = function(xyz, ...) {
    rgb <- colorspaces$sRGB$fromXYZ(xyz, ...)
    rgb <- round(rgb, 5)
    if (min(rgb) < 0 || max(rgb) > 1) 
      as.character(NA)
    else rgb(rgb[1], rgb[2], rgb[3])
  }, white = "D65", name = "#rrggbb")
  cols <- t(col2rgb(col))
  convertColor(cols, from = "sRGB", to = hexcolor, scale.in = 255, 
               scale.out = NULL)
}


rgb2col <- local({
  all.names <- colors(distinct=TRUE)
  all.lab <- t(convertColor(t(col2rgb(all.names)), from = "sRGB", 
                            to = "Lab", scale.in = 255))
  findNear <- function(x.lab) {
    sq.dist <- colSums((all.lab - x.lab)^2)
    rbind(all.names[which.min(sq.dist)], min(sq.dist))
  }
  function(cols.hex, near = 15) { # near = 2.3 is nominally the JND
    cols.lab <- t(convertColor(t(col2rgb(cols.hex)), from = "sRGB", 
                               to = "Lab", scale.in = 255))
    cols.near <- apply(cols.lab, 2, findNear)
    ifelse(as.numeric(cols.near[2, ]) < near^2, cols.near[1, ], toupper(cols.hex))
  }
})


convert <- function (color) 
{
  rgb <- col2rgb(color)/255
  L <- c(0.2, 0.6, 0) %*% rgb
  ifelse(L >= 0.2, "#000060", "#FFFFA0")
}

pickColor <- function (initialcolor, parent) 
{
  newcolor <- tclvalue(.Tcl(paste("tk_chooseColor", .Tcl.args(title = "Select a Color", 
                                                              initialcolor = initialcolor, parent = parent))))
  newcolor <- toupper(newcolor)
  if (newcolor == "") 
    initialcolor
  else newcolor
}





# An exact copy from randtests package since we received a mail warning about the possible archiving of the package.
# 

##
##  Wald Wolfowitz Runs Test
##
randtests.runs.test <- function(x, alternative="two.sided", threshold=median(x), pvalue="normal", plot=FALSE){
  # Performs the Runs Test for Randomness.
  #
  # Args:
  #   x: a numeric vector containing the data.
  #   alternative hypothesis, must be one of "two.sided" (default), "left.sided" or "right.sided"
  #   threshold: 
  #
  # Returns:
  #   statistic: the (normalized) value of the statistic test.
  #   n: the sample size, after the remotion of consecutive duplicate values.
  #   p.value: the asymptotic p-value.
  #
  dname <- deparse(substitute(x))
  if (alternative == "t"){alternative <- "two.sided"} 
  if (alternative == "l"){alternative <- "left.sided"}
  if (alternative == "r"){alternative <- "right.sided"}    
  if (alternative != "two.sided" & alternative != "left.sided" & alternative != "right.sided")
  {stop("must give a valid alternative")}
  # Remove NAs
  x <- na.omit(x)
  stopifnot(is.numeric(x))
  # Remove values equal to the level
  x <- x[x!=threshold]
  s <- sign(x-threshold)
  n1 <- length(s[s>0]) 
  n2 <- length(s[s<0])
  runs <- rle(s)
  r1 <- length(runs$lengths[runs$values==1])
  r2 <- length(runs$lengths[runs$values==-1])  
  n <- n1+n2
  mu <- 1 + 2*n1*n2/(n1+n2)
  vr <- 2*n1*n2*(2*n1*n2-n1-n2)/(n^2*(n-1))
  rr <- r1+r2
  #
  # Plot the data if requested by the user
  if (plot){
    plot((1:n)[s>0],x[s>0], xlim=c(1,n), ylim=c(min(x),max(x)), xlab="", ylab=dname)
    points((1:n)[s<0],x[s<0], col="red")
    abline(h=threshold, col=gray(.4))
    for (i in 1:(n-1)){
      if (s[i]*s[i+1]<0){abline(v=i+0.5, lty=2)}
    }
  }
  #
  # Computes the p-value
  pv <- 0
  if (pvalue == "exact"){    
    if (alternative=="two.sided"){
      pv1<-sum(druns(1:rr,n1,n2))
      pv2<-sum(druns(rr:(n1+n2),n1,n2))
      pv <- 2*min(pv1,pv2)
    }
    if (alternative=="left.sided"){pv<-sum(druns(2:rr,n1,n2))}
    if (alternative=="right.sided") {pv<-sum(druns(rr:(n1+n2),n1,n2))}    
  }
  if (pvalue=="normal"){
    pv0 <- pnorm((rr - mu) / sqrt(vr))
    if (alternative=="two.sided"){pv <- 2*min(pv0,1-pv0)}
    if (alternative=="left.sided"){pv <- pv0}
    if (alternative=="right.sided") {pv <- 1-pv0}
  }  
  if (alternative=="two.sided"){alternative<-"nonrandomness"}
  if (alternative=="left.sided"){alternative<-"trend"}
  if (alternative=="right.sided") {alternative<-"first-order negative autocorrelation"}
  #
  rval <- list(statistic = c(statistic=(rr - mu) / sqrt(vr)), p.value = pv, runs=rr, mu=mu, var=vr,  
               method = "Runs Test", data.name = dname, parameter=c(runs=rr, n1=n1,n2=n2,n=n), alternative=alternative)  
  class(rval) <- "htest"
  return(rval)
  
}  


# An exact copy from randtests package since we received a mail warning about the possible archiving of the package.
# 

##
##  probability function of the runs statistic
##
druns <- function(x, n1, n2, log = FALSE){
  stopifnot(is.numeric(x))
  x <- ifelse(x == round(x),x,1)
  r0 <- ifelse(x %% 2==0, 2*choose(n1-1, round(x/2)-1)*choose(n2-1, round(x/2)-1), 
               choose(n1-1, round((x-1)/2))*choose(n2-1, round((x-3)/2))+choose(n1-1, round((x-3)/2))*choose(n2-1, round((x-1)/2)))  
  r0<-r0/choose(n1+n2, n1)
  # if TRUE, probabilities p are given as log(p).  
  ifelse(log,return(log(r0)),return(r0))  
}

