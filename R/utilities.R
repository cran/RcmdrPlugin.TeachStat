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


removeRedundantExtension <- function(file){
  # An exact copy of "removeRedundantExtension" from Rcmdr package since it is not exported from that package
  find.ext <- regexpr("\\.(?:.(?!\\.))+$", file, perl=TRUE)
  if (find.ext == -1) return(file)
  ext <- substring(file, find.ext, find.ext + attr(find.ext, "match.length"))
  file <- sub(paste0(ext, ext, "$"), ext, file)
  file
}

listTypesVariables <- function(dataSet) {
  vars <- names(get(dataSet, envir=.GlobalEnv))
  cmd <- paste0("with(",dataSet,",c(",paste0("paste0(class(",vars,"),collapse=\" \")",collapse=","),"))")
  vtypes <- eval(parse(text=cmd))
  paste0(vars, "[",vtypes ,"]")
}
