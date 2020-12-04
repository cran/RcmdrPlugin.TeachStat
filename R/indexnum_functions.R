
Sindex <- function(x,pervar,vvar,base){
  # Compute the simple index number. It is also used for computing the base change of an index number
  colNameCheck <- checkNames(x, c(pervar,vvar))
  if (colNameCheck$result == FALSE) {
    stop(colNameCheck$message)
  }
  
  if(!all(sapply(list(pervar,vvar,base),isstring))){
    stop('Arguments pervar, vvar, and base must be character strings')
  }
  
  base <- as.character(base)
  values <- x[[vvar]]
  if(anyDuplicated(x[[pervar]])!=0){
    stop('The base variable cannot have repeated values')
  }
  names(values) <- x[[pervar]]
  if(!base %in% names(values)){
    stop(paste(base, ' is not a value in the variable "',pervar,'"',sep=''))
  }
  return(eval(parse(text=paste('data.frame(index_',make.names(base),'=values/values[base])',sep=""))))
}

Deflat <- function(x,pervar,cvar,defl,base){
  # Deflact a value list.
  colNameCheck <- checkNames(x, c(pervar,cvar,defl))
  if (colNameCheck$result == FALSE) {
    stop(colNameCheck$message)
  }
  if(!all(sapply(list(pervar,cvar, defl, base),isstring))){
    stop('Arguments pervar, cvar, defl, and base must be character strings')
  }
  base <- as.character(base)
  current <- x[[cvar]]
  deflator <- x[[defl]]
  if(anyDuplicated(x[[pervar]])!=0){
    stop('The base variable cannot have repeated values')
  }
  names(current) <- names(deflator) <-x[[pervar]]
  if(!base %in% names(current)){
    stop(paste(base, ' is not a value in the variable "',pervar,'"',sep=''))
  }
  return(eval(parse(text=paste('data.frame(const_',make.names(base),'=current/deflator*deflator[base])',sep=""))))
}


priceIndexNum <- function (x, prodID, pervar, pvar, qvar, base, indexMethod = "laspeyres", output = "fixedBase",  ...){
  # Compute the price index number using the function 'priceIndex' (with some improvements) in the package 'IndexNumR'
  # if(!require(IndexNumR)){
  #   stop('package "IndexNumR" is necessary')
  # }
  
  colNameCheck <- checkNames(x, c(prodID, pervar, pvar, qvar))
  if (colNameCheck$result == FALSE) {
    stop(colNameCheck$message)
  }
  if(!all(sapply(list(prodID, pervar, pvar, qvar, base),isstring))){
    stop('Arguments prodID, pervar, pvar, qvar, and base must be character strings')
  }
  
  x[[pervar]] <- as.factor(x[[pervar]])
  isord <- FALSE
  if(is.ordered(x[[pervar]])) isord <- TRUE
  
  if(!base %in% levels(x[[pervar]])){
    stop(paste(base, ' is not a value in the variable "',pervar,'"',sep=''))
  }
  pos<-which(levels(x[[pervar]])==base)
  oldlevels <- levels(x[[pervar]])
  if(isord)  class(x[[pervar]]) <- "factor"
  x[[pervar]] <- relevel(x[[pervar]],base)
  x[[pervar]] <- as.numeric(x[[pervar]])
  meth <- function(y,...) { 
    numInd <- IndexNumR::priceIndex(x,pvar = pvar,qvar = qvar, pervar = pervar,prodID = prodID, indexMethod = y, output = output, ...)
    numInd <- append(numInd[-1],numInd[1],after=(pos-1))
    return(numInd)
  }
  numInd <- lapply(indexMethod, meth,...)
  priceind <- data.frame(oldlevels,numInd)
  names(priceind) <- c("period",indexMethod)
  if(isord) priceind[["period"]] <- as.ordered(priceind[["period"]])
  return(priceind)
}


ComplexIN <- function (data, means = c("arithmetic", "geometric", "harmonic"), zero.rm=TRUE, na.rm=TRUE,...)
{
  # Compute complex index numbers
  
  
  # aux functions
  meanG <- function(x,zero.rm=FALSE,...){
    if (zero.rm){
      x <- x[x>0]
    }
    return(exp(mean(log(x),...)))
  }
  
  meanH <- function(x,zero.rm=FALSE,...){
    if (zero.rm){
      x <- x[x!=0]
    }
    return(1/mean(1/x,...))
  }
  
  data <- as.data.frame(data)
  variables <- names(data)
  if (missing(means)) means <- c("arithmetic", "geometric", "harmonic")
  means <- match.arg(means, c("arithmetic", "geometric", "harmonic"), several.ok=TRUE)
  
  avge <- c("mean", "meanG", "meanH")[c("arithmetic", "geometric", "harmonic") %in% means]
  navge <- length(avge)
  nvars <- length(variables)
  #result <- list()
  
  # if ((nvars == 1) && (length(means) == 1)){
  #   table <- do.call(avge, list(x=data[,variables],zero.rm=zero.rm, na.rm=na.rm))
  #   names(table) <- means
  #   NAs <- sum(is.na(data[,variables]))
  #   n <- nrow(data) - NAs
  #   result$type <- 1
  # } else{
    table <- matrix(0, nvars, navge)
    rownames(table) <- variables
    colnames(table) <- means
    if ("mean" %in% avge) table[,"arithmetic"] <- colMeans(data, na.rm=na.rm)
    if ("meanG" %in% avge) table[,"geometric"] <- sapply(data,meanG,zero.rm=zero.rm,na.rm=na.rm,...)
    if ("meanH" %in% avge) table[, "harmonic"] <- sapply(data,meanH,zero.rm=zero.rm,na.rm=na.rm,...)
    NAs <- colSums(is.na(data[, variables, drop=FALSE]))
    n <- nrow(data) - NAs
    # result$type <- 3
  # }
  # result$table <- table
  # result$statistics <- means
  # result$n <- n
  # if (any(NAs > 0)) result$NAs <- NAs
  # class(result) <- "numSummary"
  # result
    table
}