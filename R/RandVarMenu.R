

# Functions to compute probabilities on intervals of values for the most popular distributions.
# They are modificatiosn of the functions in the Rcmdr files "distributions-menu.R" and "distributions-plotDistributions-menu.R"

normalDistr <- list(titleName="Normal",
                    funName="norm",
                    distrName="Norm",
                    params=c("mean","sd"),
                    paramsLabels=c("Mean","Standard deviation"),
                    initialValues=c("0","1"),
                    errorTexts=c("Mean not specified.",
                                 "Standard deviation must be positive."),
                    errorConds=c("is.na(vars[1])",
                                 "is.na(vars[2]) || (vars[2] <= 0)"),
                    paramsRound=c()
)

tDistr <- list(titleName="t",
               funName="t",
               distrName="Td",
               params=c("df"),
               paramsLabels=c("Degrees of freedom"),
               initialValues=c("1"),
               errorTexts=c("Degrees of freedom not specified.",
                            "Degrees of freedom must be positive."),
               errorConds=c("is.na(vars[1])",
                            "(vars[1] <= 0)"),
               paramsRound=c()
)

chisqDistr <- list(titleName="ChiSquared",
                   funName="chisq",
                   distrName="Chisq",
                   params=c("df"),
                   paramsLabels=c("Degrees of freedom"),
                   initialValues=c("1"),
                   errorTexts=c("Degrees of freedom not specified.",
                                "Degrees of freedom must be positive."),
                   errorConds=c("is.na(vars[1])",
                                "(vars[1] <= 0)"),
                   paramsRound=c()
)

FDistr <- list(titleName="F",
               funName="f",
               distrName="Fd",
               params=c("df1","df2"),
               paramsLabels=c("Numerator degrees of freedom",
                              "Denominator degrees of freedom"),
               initialValues=c("1","1"),
               errorTexts=c("Degrees of freedom not specified.",
                            "Degrees of freedom must be positive."),
               errorConds=c("is.na(vars[1]) || is.na(vars[2])",
                            "(vars[1] <= 0 || vars[2] <= 0)"),
               paramsRound=c()
)

exponentialDistr <- list(titleName="Exponential",
                         funName="exp",
                         distrName="Exp",
                         params=c("rate"),
                         paramsLabels=c("Rate"),
                         initialValues=c("1"),
                         errorTexts=c("Rate must be positive."),
                         errorConds=c("is.na(vars[1]) || vars[1] <= 0"),
                         paramsRound=c()
)

uniformDistr <- list(titleName="Uniform",
                     funName="unif",
                     distrName="Unif",
                     params=c("Min","Max"),
                     paramsLabels=c("Minimum","Maximum"),
                     initialValues=c("0","1"),
                     errorTexts=c("Lower limit must be less than upper limit."),
                     errorConds=c("is.na(vars[1]) || is.na(vars[2]) || vars[1] >= vars[2]"),
                     paramsRound=c()
)

binomialDistr <- list(titleName="Binomial",
                      funName="binom",
                      distrName="Binom",
                      params=c("size","prob"),
                      paramsLabels=c("Binomial trials","Probability of success"),
                      initialValues=c("1","0.5"),
                      errorTexts=c("Binomial trials not specified.",
                                   "Binomial trials must be positive.",
                                   "Probability of success not specified.",
                                   "Probability of success must be between 0 and 1."),
                      errorConds=c("is.na(vars[1])",
                                   "vars[1]<=0",
                                   "is.na(vars[2])",
                                   "vars[2]<0 || vars[2]>1"),
                      paramsRound=c(1)
)

PoissonDistr <- list(titleName="Poisson",
                     funName="pois",
                     distrName="Pois",
                     params=c("lambda"),
                     paramsLabels=c("Mean"),
                     initialValues=c("1"),
                     errorTexts=c("Poisson mean not specified.",
                                  "Poisson mean cannot be negative."),
                     errorConds=c("is.na(vars[1])",
                                  "vars[1]<0"),
                     paramsRound=c()
)

geomDistr <- list(titleName="Geometric",
                  funName="geom",
                  distrName="Geom",
                  params=c("prob"),
                  paramsLabels=c("Probability of success"),
                  initialValues=c("0.5"),
                  errorTexts=c("Probability of success not specified.",
                               "Probability of success must be between 0 and 1."),
                  errorConds=c("is.na(vars[1])",
                               "vars[1] < 0 || vars[1] > 1"),
                  paramsRound=c()
)

negbinomialDistr <- list(titleName="NegativeBinomial",
                         funName="nbinom",
                         distrName="Nbinom",
                         params=c("size","prob"),
                         paramsLabels=c("Target number of successes",
                                        "Probability of success"),
                         initialValues=c("1","0.5"),
                         errorTexts=c("Target number of successes not specified.",
                                      "Target number of successes cannot be negative.",
                                      "Probability of success not specified.",
                                      "Probability of success must be between 0 and 1."),
                         errorConds=c("is.na(vars[1])",
                                      "vars[1]<0",
                                      "is.na(vars[2])",
                                      "vars[2] < 0 || vars[2] >1 "),
                         paramsRound=c(1)
)

genericDistr <- list(titleName="Generic",
                     funName="generic",
                     distrName="",
                     params=c("dist"),
                     paramsLabels=c("Distribution"),
                     initialValues=c(""),
                     errorTexts=c("Distribution not specified."),
                     errorConds=c('is.na(vars[1]) || (vars[1] == "")'),
                     paramsRound=c()
)


# Graphics

normalDistrPlot <- function() {distrPlot("normal")}
tDistrPlot <- function() {distrPlot("t")}
chisqDistrPlot <- function() {distrPlot("chisq")}
FDistrPlot <- function() {distrPlot("F")}
exponentialDistrPlot <- function() {distrPlot("exponential")}
uniformDistrPlot <- function() {distrPlot("uniform")}
genericDistrPlot <- function() {distrPlot("generic")}

binomialDistrPlot <- function(){distrPlot("binomial",discrete=TRUE)}
PoissonDistrPlot <- function(){distrPlot("Poisson",discrete=TRUE)}
geomDistrPlot <- function(){distrPlot("geom",discrete=TRUE)}
negbinomialDistrPlot <- function(){distrPlot("negbinomial",discrete=TRUE)}
DgenericDistrPlot <- function() {distrPlot("generic",discrete=TRUE)}


distrPlot <- function(nameVar,discrete=FALSE){
  # Author: J. Fox and Miroslav Ristic (Modified by M. A. Mosquera, 30 Nov 19)
  Library("distr")
  env <- environment()
  fVar<-get(paste(nameVar,"Distr",sep=""))
  
  # To ensure that menu name is included in pot file
  gettext(paste("Plot ", if(nameVar!="generic") "" else paste0("generic ", if(discrete)
    "discrete " else "continuous "),
    fVar$titleName," distribution...",sep=""), domain="R-RcmdrPlugin.TeachStat")
  
  nnVar<-length(fVar$params)
  dialogName <- paste(if(nameVar=="generic" & discrete)"D",nameVar,"DistrPlot", sep="")
  defaults <- list(initialValues=fVar$initialValues, dens="1", cum="0", 
                   valuesOrQuantiles="values", from1="", from2="", to1="", to2="", col=c("gray", "gray"),
                   legendPosition="topright")
  initial <- getDialog(dialogName, defaults=defaults)
  initializeDialog(title=gettext(paste(if(nameVar!="generic") "" else if(discrete) "Discrete " else "Continuous ",
                                       fVar$titleName," Distribution",sep=""),domain="R-RcmdrPlugin.TeachStat"))
  entriesFrame <- tkframe(top)
  paramsVar<-paste(fVar$params,"Var",sep="")
  paramsEntry<-paste(fVar$params,"Entry",sep="")
  paramsFrame<-paste(fVar$params,"Frame",sep="")
  if(nameVar!="generic"){
    for (i in 1:nnVar) {
      eval(parse(text=paste(paramsVar[i],"<-tclVar('",initial$initialValues[i],"')",sep="")))
      eval(parse(text=paste(paramsEntry[i],"<-ttkentry(entriesFrame, width='6', textvariable=",paramsVar[i],")",sep="")))
    }
  } else{
    distype <- if(discrete) "DiscreteDistribution" else "AbscontDistribution"
    listdist <- listDistrs(distype)
    for (i in 1:nnVar) {
      combo <- variableComboBox(entriesFrame, variableList=listdist, initialSelection=initial$initialValues[i], 
                                title=gettext("Distribution (pick one)",domain="R-RcmdrPlugin.TeachStat"))
      # Deleting <no variable selected> option from the list
      varcombobox <- if(length(listdist)==0) gettext("<no distribution selected>",domain="R-RcmdrPlugin.TeachStat") 
      else listdist
      tkconfigure(combo$combobox,values=varcombobox)
      
      eval(parse(text=paste(paramsVar[i],'<- combo$combovar',sep="")))
      eval(parse(text=paste(paramsEntry[i],'<- combo$combobox',sep="")))
      eval(parse(text=paste(paramsFrame[i],'<- getFrame(combo)',sep="")))
    }
  }
  
  denslabel <- if(discrete) "Plot probability mass function" else "Plot density function"
  checkBoxes(top, frame="buttonFrame", boxes=c("cum", "dens"), 
             initialValues=c(initial$cum,initial$dens), 
             labels=gettextRcmdr(c("Plot distribution function",denslabel)), 
             title=gettext("Select function to plot:",domain="R-RcmdrPlugin.TeachStat"))
  
  regionsFrame <- tkframe(top)
  radioButtons(regionsFrame, "valuesOrQuantiles", buttons=c("values", "quantiles"), 
               labels=gettextRcmdr(c("x-values", "quantiles")), title=gettextRcmdr("Optionally specify regions under the density function by"),
               initialValue = initial$valuesOrQuantiles)
  from1variable <- tclVar(initial$from1)
  from2variable <- tclVar(initial$from2)
  to1variable <- tclVar(initial$to1)
  to2variable <- tclVar(initial$to2)
  region1Frame <- tkframe(regionsFrame)
  region2Frame <- tkframe(regionsFrame)
  from1box <- ttkentry(regionsFrame, width="10", textvariable=from1variable)
  from2box <- ttkentry(regionsFrame, width="10", textvariable=from2variable)
  to1box   <- ttkentry(regionsFrame, width="10", textvariable=to1variable)
  to2box   <- ttkentry(regionsFrame, width="10", textvariable=to2variable)
  hex <- col2hex(initial$col)
  for (i in 1:2) assign(paste("hex", i, sep="."), hex[i], envir=env)
  colorField1 <- labelRcmdr(region1Frame, text=rgb2col(hex[1]), fg=hex[1], background="white")
  button1 <- tkbutton(region1Frame, text=hex[1], bg = hex[1],
                      fg=convert(hex[1]),
                      command=function() {
                        color <- pickColor(hex[1], parent=button1)
                        fg <- convert(color)
                        tkconfigure(button1, bg=color, fg=fg, text=toupper(color))
                        tkconfigure(colorField1, text=rgb2col(color), foreground=color)
                        assign("hex.1", color, envir=env)
                      }
  )
  colorField2 <- labelRcmdr(region2Frame, text=rgb2col(hex[2]), fg=hex[2], background="white")
  button2 <- tkbutton(region2Frame, text=hex[2], bg = hex[2],
                      fg=convert(hex[2]),
                      command=function() {
                        color <- pickColor(hex[2], parent=button2)
                        fg <- convert(color)
                        tkconfigure(button2, bg=color, fg=fg, text=toupper(color))
                        tkconfigure(colorField2, text=rgb2col(color), foreground=color)
                        assign("hex.2", color, envir=env)
                      }
  )
  radioButtons(regionsFrame, "legendPosition", buttons=c("topright", "topleft", "top"), 
               labels=gettextRcmdr(c("Top right", "Top left", "Top center")), 
               title=gettextRcmdr("Position of Legend"),
               initialValue = initial$legendPosition)
  onOK <- function(){
    nameVarF<-get(dialogName,mode="function")
    closeDialog()
    warn <- options(warn=-1)
    vars<-numeric(nnVar)
    for (i in 1:nnVar) {
      vars[i]<- tclvalue(get(paramsVar[i]))
    }
    if(nameVar!="generic") vars <- as.numeric(vars)
    if (length(fVar$paramsRound)>0) {
      for (j in fVar$paramsRound) {
        vars[j]<-round(vars[j])
      }
    }
    options(warn)
    for (i in 1:length(fVar$errorConds)) {
      if (eval(parse(text=fVar$errorConds[i]))) {
        errorCondition(recall=nameVarF, message=gettextRcmdr(fVar$errorTexts[i]))
        return()
      }
    }
    
    densVar <- tclvalue(densVariable)
    cumVar <- tclvalue(cumVariable)
    
    #Check error
    if(densVar=="0" & cumVar=="0"){
      errorCondition(recall=nameVarF, message=gettext("No plot selected.",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    
    pasteVar<-paste(fVar$params,"=",vars,sep="",collapse=", ")
    mainVar<-""
    
    for (i in 1:nnVar) {
      mainVar<-paste(mainVar,", ",fVar$paramsLabels[i],"=",vars[i],sep="")
    }
    
    if(nameVar!="generic"){
      Distr <- paste(fVar$distrName,"(",pasteVar,")",sep="")
    } else {
      Distr <- vars[1]
    }
    if(discrete){
      switch(nameVar,
             "binomial" = xlabVar<-gettext("Number of Successes",domain="R-RcmdrPlugin.TeachStat"),
             "Poisson" = xlabVar<-"x",
             "geom" = xlabVar<-gettext("Number of Failures until Success",domain="R-RcmdrPlugin.TeachStat"),
             "hyper" = xlabVar<-gettext("Number of White Balls in Sample",domain="R-RcmdrPlugin.TeachStat"),
             "negbinomial" = xlabVar <-gettext("Number of Failures Until Target Successes",domain="R-RcmdrPlugin.TeachStat"),
             "generic" = xlabVar <- "x"
      )
      if (nameVar=="negbinomial") {
        mainVar<-paste(", ",gettext("Trials",domain="R-RcmdrPlugin.TeachStat"),"=",vars[1],", Prob=",vars[2],sep="")
      } else if (nameVar=="hyper") {
        mainVar<-paste(", m=",vars[1],", n=",vars[2],", k=",vars[3],sep="")
      } else {
      }
      ylabVar <- gettext("Probability Mass",domain="R-RcmdrPlugin.TeachStat")
    } else{
      if (nameVar=="F") {
        mainVar<-paste(", ",gettext("Numerator",domain="R-RcmdrPlugin.TeachStat"),
                       " df = ",vars[1],", ",gettext("Denominator",domain="R-RcmdrPlugin.TeachStat"),
                       " df = ",vars[2],sep="")}
      xlabVar <- "x"
      ylabVar <- gettext("Density",domain="R-RcmdrPlugin.TeachStat")
    }
    ylabVarC <- if(cumVar=="1") gettext("Cumulative Probability",domain="R-RcmdrPlugin.TeachStat")
    
    command <- paste("local({\n  .D <- ", Distr, sep="")
    doVar<-"\n  plotRegions(.D"
    
    valuesOrQuantiles <- tclvalue(valuesOrQuantilesVariable)
    legendPosition <- tclvalue(legendPositionVariable)
    save.col <- c(hex.1, hex.2)
    from1 <- trim.blanks(tclvalue(from1variable))
    
    cmd1 <- cmd2 <- ""
    if(densVar=="1" & cumVar=="1" ){
      cmd1 <- "\n  opar <- par(mfrow=c(1,2))"
      cmd2 <- "\n  par(opar)"
    }
    
    if (from1 == "" || densVar != "1"){
      save.from1 <- save.to1 <- save.from2 <- save.to2 <- ""
      command <- paste(command, cmd1, if(cumVar=="1"){
        paste0("  ",doVar, ', to.draw.arg=2, pch.u="", cex.points=par("cex"), 
               verticals=FALSE, mfColRow=FALSE, xlab="',xlabVar,'", ylab="',ylabVarC,'", main=paste("',
               fVar$titleName,' Distribution: ',substr(mainVar,2,nchar(mainVar)),'"))')},
        if(densVar=="1"){ 
          paste0("  ",doVar,', to.draw.arg=1, mfColRow=FALSE, xlab="',xlabVar,'", ylab="',ylabVar,'", main=paste("',
                 fVar$titleName,' Distribution: ',substr(mainVar,2,nchar(mainVar)),'"))')},
        cmd2,'\n})', sep="")
    }
    else {
      if (!is.valid.number(from1)){
        errorCondition(recall=nameVarF, message=paste(from1, gettextRcmdr("is not a valid number.")))
        return()
      }
      from2 <- trim.blanks(tclvalue(from2variable))
      if (from2 != "" && !is.valid.number(from2)){
        errorCondition(recall=nameVarF, message=paste(from2, gettextRcmdr("is not a valid number.")))
        return()
      }
      to1 <-  trim.blanks(tclvalue(to1variable))
      if (to1 == ""){
        errorCondition(recall=nameVarF, message=paste(gettextRcmdr("You must specify 'from' and 'to' for at least one range.")))
        return()
      }
      if (!is.valid.number(to1)){
        errorCondition(recall=nameVarF, message=paste(to1, gettextRcmdr("is not a valid number.")))
        return()
      }
      to2 <-  trim.blanks(tclvalue(to2variable))
      if (to2 != "" && !is.valid.number(to2)){
        errorCondition(recall=nameVarF, message=paste(to2, gettextRcmdr("is not a valid number.")))
        return()
      }
      if (as.numeric(to1) <= as.numeric(from1)){
        errorCondition(recall=nameVarF, message=gettextRcmdr("In specifying a range, 'to' must be greater than 'from'."))
        return()
      }
      if (from2 != "" && to2 == "" ){
        errorCondition(recall=nameVarF, message=gettext("You must specify 'to' for the range.",domain="R-RcmdrPlugin.TeachStat"))
        return()
      }
      if (from2 != "" && to2 != "" && as.numeric(to2) <= as.numeric(from2)){
        errorCondition(recall=nameVarF, message=gettextRcmdr("In specifying a range, 'to' must be greater than 'from'."))
        return()
      }
      save.from1 <- from1
      save.from2 <- from2
      save.to1 <- to1
      save.to2 <- to2
      if (valuesOrQuantiles == "quantiles"){
        if (as.numeric(from1) < 0 || as.numeric(from1) >= 1 || as.numeric(to1) <= 0 || as.numeric(to1) > 1 ){
          errorCondition(recall=nameVarF, message=gettextRcmdr("Quantiles must be between 0 and 1."))
          return()
        }
        .D1 <- eval(parse(text=Distr))
        from1 <- eval(parse(text=paste("distr::q(.D1)(", from1, ")", sep="")))
        to1 <- eval(parse(text=paste("distr::q(.D1)(", to1, ")", sep="")))
        if (from2 != "") {
          if (as.numeric(from2) < 0 || as.numeric(from2) >= 1 || as.numeric(to2) <= 0 || as.numeric(to2) > 1 ){
            errorCondition(recall=nameVarF, message=gettextRcmdr("Quantiles must be between 0 and 1."))
            return()
          }
          from2 <- eval(parse(text=paste("distr::q(.D1)(", from2, ")", sep="")))
          to2 <- eval(parse(text=paste("distr::q(.D1)(", to2, ")", sep="")))
        }
      }
      cmdcum <- if(cumVar=="1"){
        paste0("  ",doVar,',to.draw.arg=2, pch.u="", cex.points=par("cex"), 
                 verticals=FALSE, mfColRow=FALSE, xlab="',xlabVar,'", ylab="',ylabVarC,'", main=paste("',
               fVar$titleName,' Distribution: ',substr(mainVar,2,nchar(mainVar)),'"))')
      } else {""}
      
      cmddens <- ""
      if(densVar=="1"){ 
        cmddens <- paste0("  ",doVar, ', regions=list(c(', from1, ', ', to1, ')', 
                          if (from2 != "") paste(', c(', from2, ', ', to2, ')', sep=""), ')',
                          ', col=c("', hex.1, '", "', hex.2, '"), legend.pos="', legendPosition,
                          '", to.draw.arg=1, mfColRow=FALSE, xlab="',xlabVar,'", ylab="',
                          ylabVar,'", main=paste("', fVar$titleName,' Distribution: ',substr(mainVar,2,nchar(mainVar)),'"))')
      }
      command <- paste(command, cmd1, cmdcum, cmddens,cmd2,'\n})', sep="")
    }
    doItAndPrint(command)
    tkfocus(CommanderWindow())
    putDialog(dialogName, list(initialValues=vars, dens=densVar, cum=cumVar, 
                               valuesOrQuantiles=valuesOrQuantiles,
                               from1=save.from1, from2=save.from2, to1=save.to1, to2=save.to2, col=save.col,
                               legendPosition=legendPosition), 
              resettable=FALSE)
    }
  OKCancelHelp(helpSubject="plotRegions", reset=dialogName, apply=dialogName)
  for (i in 1:nnVar) {
    if(nameVar!="generic"){
      tkgrid(labelRcmdr(entriesFrame, text=gettextRcmdr(fVar$paramsLabels[i])), get(paramsEntry[i]), sticky="w", padx=6)
    } else{
      tkgrid(get(paramsFrame[i]), sticky="w", padx=6)
    }
  }
  tkgrid(entriesFrame, sticky="w")
  tkgrid(buttonFrame, sticky="w")
  for (i in 1:nnVar) {
    tkgrid.configure(get(paramsEntry[i]), sticky="w")
  }
  tkgrid(labelRcmdr(top, text=""))
  tkgrid(valuesOrQuantilesFrame, sticky="w")
  tkgrid(labelRcmdr(regionsFrame, text=gettextRcmdr("Regions to Fill (specify one or two, or leave blank)"), 
                    fg=getRcmdr("title.color"), font="RcmdrTitleFont"), sticky="w")
  tkgrid(labelRcmdr(region1Frame, text=gettextRcmdr("Region 1: from ")), from1box, 
         labelRcmdr(region1Frame, text=gettextRcmdr(" to ")), to1box, 
         labelRcmdr(region1Frame, text=gettextRcmdr(" color ")), button1, colorField1, sticky="w")
  tkgrid(labelRcmdr(region2Frame, text=gettextRcmdr("Region 2: from ")), from2box, 
         labelRcmdr(region2Frame, text=gettextRcmdr(" to ")), to2box, 
         labelRcmdr(region2Frame, text=gettextRcmdr(" color ")), button2, colorField2, sticky="w")
  tkgrid(region1Frame, sticky="w")
  tkgrid(region2Frame, sticky="w")
  tkgrid(legendPositionFrame, sticky="w")
  tkgrid(regionsFrame, sticky="w")
  tkgrid(buttonsFrame, sticky="ew")
  dialogSuffix(focus=get(paramsEntry[1]))
}


## Interval Probabilities

normalProbabilitiesInter <-function() {distrProbabilitiesInter("normal")}
tProbabilitiesInter <-function() {distrProbabilitiesInter("t")}
chisqProbabilitiesInter <-function() {distrProbabilitiesInter("chisq")}
FProbabilitiesInter <-function() {distrProbabilitiesInter("F")}
exponentialProbabilitiesInter <-function() {distrProbabilitiesInter("exponential")}
uniformProbabilitiesInter <-function() {distrProbabilitiesInter("uniform")}
genericProbabilitiesInter <-function() {distrProbabilitiesInter("generic")}

binomialProbabilitiesInter <-function() {distrProbabilitiesInter("binomial",discrete=TRUE)}
PoissonProbabilitiesInter <-function() {distrProbabilitiesInter("Poisson",discrete=TRUE)}
geomProbabilitiesInter <-function() {distrProbabilitiesInter("geom",discrete=TRUE)}
negbinomialProbabilitiesInter <-function() {distrProbabilitiesInter("negbinomial",discrete=TRUE)}
DgenericProbabilitiesInter <-function() {distrProbabilitiesInter("generic",discrete=TRUE)}

distrProbabilitiesInter <- function(nameVar,discrete=FALSE){
  # Author: J. Fox and Miroslav Ristic (Modified by M. A. Mosquera, 30 Sep 19)
  Library("distr")
  env <- environment()
  fVar<-get(paste(nameVar,"Distr",sep=""))
  
  # To ensure that menu name is included in pot file
  gettext(paste(if(nameVar!="generic") "" else if(discrete)
    "Discrete " else "Continuous ",
    fVar$titleName, if(discrete) " tail", " probabilities...",sep=""), domain="R-RcmdrPlugin.TeachStat")
  
  
  nnVar<-length(fVar$params)
  dialogName <- paste(if(nameVar=="generic" & discrete)"D",nameVar,"ProbabilitiesInter", sep="")
  defaults <- list(initialValues=fVar$initialValues, from="", to="", graph="", col="gray",legendPosition="topright")
  initial <- getDialog(dialogName, defaults=defaults)
  initializeDialog(title=gettext(paste(if(nameVar!="generic") "" else if(discrete) 
    "Discrete " else "Continuous ",fVar$titleName," Probabilities",sep=""),domain="R-RcmdrPlugin.TeachStat"))
  entryFrame <- ttkframe(top)
  probabilitiesFrame <- ttkframe(entryFrame,borderwidth=1,relief="solid",padding=6)
  fromVar <- tclVar(initial$from)
  toVar <- tclVar(initial$to)
  fromEntry <- ttkentry(probabilitiesFrame, width="16", textvariable=fromVar)
  toEntry <- ttkentry(probabilitiesFrame, width="16", textvariable=toVar)
  paramFrame <- ttkframe(entryFrame,borderwidth=1,relief="solid",padding=6)
  paramsVar<-paste(fVar$params,"Var",sep="")
  paramsEntry<-paste(fVar$params,"Entry",sep="")
  paramsFrame<-paste(fVar$params,"Frame",sep="")
  
  if(nameVar!="generic"){
    for (i in 1:nnVar) {
      eval(parse(text=paste(paramsVar[i],"<-tclVar('",initial$initialValues[i],"')",sep="")))
      eval(parse(text=paste(paramsEntry[i],"<-ttkentry(paramFrame, width='6', textvariable=",paramsVar[i],")",sep="")))
    }
  } else{
    distype <- if(discrete) "DiscreteDistribution" else "AbscontDistribution"
    listdist <- listDistrs(distype)
    for (i in 1:nnVar) {
      combo <- variableComboBox(paramFrame, variableList=listdist, initialSelection=initial$initialValues[i], 
                                title=gettext("Distribution (pick one)",domain="R-RcmdrPlugin.TeachStat"))
      # Deleting <no variable selected> option from the list
      varcombobox <- if(length(listdist)==0) gettext("<no distribution selected>",domain="R-RcmdrPlugin.TeachStat") 
      else listdist
      tkconfigure(combo$combobox,values=varcombobox)
      
      eval(parse(text=paste(paramsVar[i],'<- combo$combovar',sep="")))
      eval(parse(text=paste(paramsEntry[i],'<- combo$combobox',sep="")))
      eval(parse(text=paste(paramsFrame[i],'<- getFrame(combo)',sep="")))
    }
  }
  
  graphVar <- tclVar(initial$graph)
  buttonFrame <- ttkframe(top)
  graphButton <- ttkcheckbutton(buttonFrame, variable=graphVar, text=gettext("Plot region",domain="R-RcmdrPlugin.TeachStat"),
                                command = function(){ 
                                  if(tclvalue(graphVar)==1){
                                    tk2state.set(colorField, state = "normal")
                                    tk2state.set(button, state = "normal")
                                  } else{
                                    tk2state.set(colorField, state = "disabled")
                                    tk2state.set(button, state = "disabled")
                                  }
                                })
  hex <- col2hex(initial$col)
  
  if(initial$graph!=1){
    mostrar <- "disabled"
  }else {
    mostrar<-"normal"
  }
  
  colorField <- labelRcmdr(buttonFrame, text=rgb2col(hex), fg=hex, background="white",state=mostrar)
  button <- tkbutton(buttonFrame, text=hex, bg = hex,
                     fg=convert(hex),state=mostrar,
                     command=function() {
                       color <- pickColor(hex, parent=button)
                       fg <- convert(color)
                       tkconfigure(button, bg=color, fg=fg, text=toupper(color))
                       tkconfigure(colorField, text=rgb2col(color), foreground=color)
                       assign("hex", color, envir=env)
                     }
  )
  
  radioButtons(buttonFrame, "legendPosition", buttons=c("topright", "topleft", "top"), 
               labels=gettextRcmdr(c("Top right", "Top left", "Top center")), 
               title=gettextRcmdr("Position of Legend"), initialValue = initial$legendPosition)
  
  
  onOK <- function(){
    nameVarF<-get(dialogName,mode="function")
    closeDialog()
    from <- gsub(" +", ",", gsub(",", " ", tclvalue(fromVar)))
    # from <-  trim.blanks(tclvalue(fromVar))
    to <- gsub(" +", ",", gsub(",", " ", tclvalue(toVar)))
    # to <-  trim.blanks(tclvalue(toVar))
    if ("" == from && "" == to) {
      errorCondition(recall=nameVarF, message=gettextRcmdr("No values specified."))
      return()
    }
    if(from == "") from <- "-Inf"
    if(to == "") to <- "Inf"
    fromV <- eval(parse(text=paste("c(",from,")")))
    toV <- eval(parse(text=paste("c(",to,")")))
    nncol <- length(fromV)
    if (!is.valid.number(fromV)){
      errorCondition(recall=nameVarF, message=paste(from, 
                                                    gettext("is not a valid list of numbers.",domain="R-RcmdrPlugin.TeachStat")))
      return()
    }
    if (!is.valid.number(toV)){
      errorCondition(recall=nameVarF, message=paste(to, 
                                                    gettext("is not a valid list of numbers.",domain="R-RcmdrPlugin.TeachStat")))
      return()
    }
    if (!all(toV > fromV)){
      errorCondition(recall=nameVarF, message=gettextRcmdr("'to' must be greater than 'from'."))
      return()
    }
    warn <- options(warn=-1)
    vars<-numeric(nnVar)
    for (i in 1:nnVar) {
      vars[i]<-tclvalue(get(paramsVar[i]))
    }
    if(nameVar!="generic") vars <- as.numeric(vars)
    if (length(fVar$paramsRound)>0) {
      for (j in fVar$paramsRound) {
        vars[j]<-round(vars[j])
      }
    }
    options(warn)
    for (i in 1:length(fVar$errorConds)) {
      if (eval(parse(text=fVar$errorConds[i]))) {
        errorCondition(recall=nameVarF, message=gettextRcmdr(fVar$errorTexts[i]))
        return()
      }
    }
    graph <- tclvalue(graphVar)
    pasteVar<-paste(fVar$params,"=",vars,sep="",collapse=", ")
    
    if(nameVar!="generic"){
      Distr <- paste(fVar$distrName,"(",pasteVar,")",sep="")
    } else {
      Distr <- vars[1]
    }
    
    command <- paste("local({\n  .D <- ", Distr, sep="")
    command <- paste(command,"\n  .fr <- c(", from,")", sep="")
    command <- paste(command,"\n  .to <- c(", to,")", sep="")
    command <- paste(command,"\n  .p <- p(.D)(.to) - p(.D)(.fr)", sep="")
    command <- paste(command,'\n  cat(paste("P(",.fr," < X <= ",.to,") = ",.p,sep="",collapse="\\n"),"\\n")',sep="")
    save.from <- from
    save.to <- to
    save.col <- hex
    legendPosition <- tclvalue(legendPositionVariable)
    if(graph == 1){
      mainVar<-""
      for (i in 1:nnVar) {
        mainVar<-paste(mainVar,", ",fVar$paramsLabels[i],"=",vars[i],sep="")
      }
      if(discrete){
        switch(nameVar,
               "binomial" = xlabVar<-gettext("Number of Successes",domain="R-RcmdrPlugin.TeachStat"),
               "Poisson" = xlabVar<-"x",
               "geom" = xlabVar<-gettext("Number of Failures until Success",domain="R-RcmdrPlugin.TeachStat"),
               "hyper" = xlabVar<-gettext("Number of White Balls in Sample",domain="R-RcmdrPlugin.TeachStat"),
               "negbinomial" = xlabVar <-gettext("Number of Failures Until Target Successes",domain="R-RcmdrPlugin.TeachStat"),
               "generic" = xlabVar <- "x"
        )
        if (nameVar=="negbinomial") {
          mainVar<-paste(", ",gettext("Trials",domain="R-RcmdrPlugin.TeachStat"),"=",vars[1],", Prob=",vars[2],sep="")
        } else if (nameVar=="hyper") {
          mainVar<-paste(", m=",vars[1],", n=",vars[2],", k=",vars[3],sep="")
        } else {
        }
        ylabVar <- gettext("Probability Mass",domain="R-RcmdrPlugin.TeachStat")
      } else{
        if (nameVar=="F") {
          mainVar<-paste(", ",gettext("Numerator",domain="R-RcmdrPlugin.TeachStat"),
                         " df = ",vars[1],", ",gettext("Denominator",domain="R-RcmdrPlugin.TeachStat"),
                         " df = ",vars[2],sep="")
        }
        xlabVar <- "x"
        ylabVar <- gettext("Density",domain="R-RcmdrPlugin.TeachStat")
      }
      doVar<-"\n  plotRegions(.D"
      col <- grDevices::colorRampPalette(c(hex,rgb(grDevices::colorRamp(c(hex,"white"))(0.8),maxColorValue = 255)))(nncol)
      col <- paste0(', col=c(',paste0('"',col,'"',sep="",collapse=", "),')')
      command <- paste(command, doVar, ', regions=list(',paste('c(',fromV,", ",toV,')', sep="",collapse=", "),')', col,
                       ',legend=FALSE',', to.draw.arg=1, mfColRow=FALSE, xlab="', xlabVar,'", ylab="',
                       ylabVar,'", main=paste("',fVar$titleName,' Distribution: ',substr(mainVar,2,nchar(mainVar)), '"))', sep="")
      command <- paste(command,'\n  legend("', legendPosition,'",legend=c(paste("P(",.fr," < X <= ",.to,")=",round(.p,4),sep=""))',col,',pch = 15, pt.cex = 2.5,inset=0.02)', sep="")
    }
    command <- paste(command,'\n})', sep="")
    doItAndPrint(command)
    tkfocus(CommanderWindow())
    putDialog(dialogName, list(initialValues=vars, graph=tclvalue(graphVar), from=save.from,
                               to=save.to, col=save.col,legendPosition=legendPosition),
              resettable=FALSE)
  }
  OKCancelHelp(helpSubject="p", helpPackage ="distr", reset = dialogName, apply = dialogName)
  if(nameVar!="generic"){
    tkgrid(labelRcmdr(paramFrame, text=gettext(paste(fVar$titleName,"distribution's parameters:"),domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ), sticky="w")
  }
  
  
  for (i in 1:nnVar) {
    if(nameVar!="generic"){
      tkgrid(labelRcmdr(paramFrame, text=gettextRcmdr(fVar$paramsLabels[i])), get(paramsEntry[i]), sticky="w", padx=6)
    } else{
      tkgrid(get(paramsFrame[i]), sticky="w", padx=6)
    }
  }
  tkgrid(labelRcmdr(probabilitiesFrame, text=gettext("Probabilities:",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ), sticky="w")
  tkgrid(labelRcmdr(probabilitiesFrame, text=gettext("From (>)",domain="R-RcmdrPlugin.TeachStat")), fromEntry, sticky="w", padx=6)
  tkgrid(labelRcmdr(probabilitiesFrame, text=gettext("To (<=)",domain="R-RcmdrPlugin.TeachStat")), toEntry, sticky="w", padx=6)
  tkgrid(graphButton, labelRcmdr(buttonFrame, text=gettext(" color: ",domain="R-RcmdrPlugin.TeachStat")), button, colorField, padx=3, pady=6, sticky="w")
  tkgrid(legendPositionFrame, sticky="w")
  tkgrid(paramFrame, probabilitiesFrame, padx=6, sticky="nw")
  tkgrid(entryFrame,sticky="w")
  tkgrid(buttonFrame, sticky="w")
  tkgrid(buttonsFrame, sticky="ew")
  for (i in 1:nnVar) {
    tkgrid.configure(get(paramsEntry[i]), sticky="w")
  }
  tkgrid.configure(fromEntry, sticky="w")
  tkgrid.configure(toEntry, sticky="w")
  dialogSuffix(focus=fromEntry)
}



# Quantiles

normalDistrQuantiles <- function() {distrQuantiles("normal")}
tDistrQuantiles <-function() {distrQuantiles("t")}
chisqDistrQuantiles <-function() {distrQuantiles("chisq")}
FDistrQuantiles <-function() {distrQuantiles("F")}
exponentialDistrQuantiles <-function() {distrQuantiles("exponential")}
uniformDistrQuantiles <-function() {distrQuantiles("uniform")}
genericDistrQuantiles <-function() {distrQuantiles("generic")}

binomialDistrQuantiles <-function() {distrQuantiles("binomial",discrete=TRUE)}
PoissonDistrQuantiles <-function() {distrQuantiles("Poisson",discrete=TRUE)}
geomDistrQuantiles <-function() {distrQuantiles("geom",discrete=TRUE)}
negbinomialDistrQuantiles <-function() {distrQuantiles("negbinomial",discrete=TRUE)}
DgenericDistrQuantiles <-function() {distrQuantiles("generic",discrete=TRUE)}


distrQuantiles <- function(nameVar,discrete=FALSE){
  # Author: J. Fox and Miroslav Ristic (Modified by M. A. Mosquera, 30 Nov 19)
  Library("distr")
  env <- environment()
  fVar<-get(paste(nameVar,"Distr",sep=""))
  
  # To ensure that menu name is included in pot file
  gettext(paste(if(nameVar!="generic") "" else if(discrete)
    "Discrete " else "Continuous ",
    fVar$titleName," quantiles...",sep=""), domain="R-RcmdrPlugin.TeachStat")
  
  nnVar<-length(fVar$params)
  dialogName <- paste(if(nameVar=="generic" & discrete)"D",nameVar,"DistrQuantiles", sep="")
  defaults <- list(initialValues=fVar$initialValues, tail="lower", quantiles="")
  initial <- getDialog(dialogName, defaults=defaults)
  initializeDialog(title=gettext(paste(if(nameVar!="generic") "" else if(discrete)
    "Discrete " else "Continuous ",
    fVar$titleName," Quantiles",sep=""),domain="R-RcmdrPlugin.TeachStat"))
  entryFrame <- tkframe(top)
  quantilesVar <- tclVar(initial$quantiles)
  quantilesEntry <- ttkentry(entryFrame, width="30", textvariable=quantilesVar)
  paramsVar<-paste(fVar$params,"Var",sep="")
  paramsEntry<-paste(fVar$params,"Entry",sep="")
  paramsFrame<-paste(fVar$params,"Frame",sep="")
  
  if(nameVar!="generic"){
    for (i in 1:nnVar) {
      eval(parse(text=paste(paramsVar[i],"<-tclVar('",initial$initialValues[i],"')",sep="")))
      eval(parse(text=paste(paramsEntry[i],"<-ttkentry(entryFrame, width='6', textvariable=",paramsVar[i],")",sep="")))
    }
  } else{
    distype <- if(discrete) "DiscreteDistribution" else "AbscontDistribution"
    listdist <- listDistrs(distype)
    for (i in 1:nnVar) {
      combo <- variableComboBox(entryFrame, variableList=listdist, initialSelection=initial$initialValues[i], 
                                title=gettext("Distribution (pick one)",domain="R-RcmdrPlugin.TeachStat"))
      # Deleting <no variable selected> option from the list
      varcombobox <- if(length(listdist)==0) gettext("<no distribution selected>",domain="R-RcmdrPlugin.TeachStat") 
      else listdist
      tkconfigure(combo$combobox,values=varcombobox)
      
      eval(parse(text=paste(paramsVar[i],'<- combo$combovar',sep="")))
      eval(parse(text=paste(paramsEntry[i],'<- combo$combobox',sep="")))
      eval(parse(text=paste(paramsFrame[i],'<- getFrame(combo)',sep="")))
    }
  }
  
  tailVar <- tclVar(initial$tail)
  buttonFrame <- tkframe(top)
  lowerTailButton <- ttkradiobutton(buttonFrame, variable=tailVar, value="lower")
  upperTailButton <- ttkradiobutton(buttonFrame, variable=tailVar, value="upper")
  onOK <- function(){
    nameVarF<-get(dialogName,mode="function")
    closeDialog()
    quantiles <- gsub(" +", ",", gsub(",", " ", tclvalue(quantilesVar)))
    if ("" == quantiles) {
      errorCondition(recall=nameVarF, message=gettextRcmdr("No probabilities specified."))
      return()
    }
    warn <- options(warn=-1)
    vars<-numeric(nnVar)
    for (i in 1:nnVar) {
      vars[i]<- tclvalue(get(paramsVar[i]))
    }
    if(nameVar!="generic") vars <- as.numeric(vars)
    if (length(fVar$paramsRound)>0) {
      for (j in fVar$paramsRound) {
        vars[j]<-round(vars[j])
      }
    }
    options(warn)
    for (i in 1:length(fVar$errorConds)) {
      if (eval(parse(text=fVar$errorConds[i]))) {
        errorCondition(recall=nameVarF, message=gettextRcmdr(fVar$errorTexts[i]))
        return()
      }
    }
    tail <- tclvalue(tailVar)
    
    pasteVar<-paste(fVar$params,"=",vars,sep="",collapse=", ")
    if(nameVar!="generic"){
      Distr <- paste(fVar$distrName,"(",pasteVar,")",sep="")
    } else {
      Distr <- vars[1]
    }
    
    doItAndPrint(paste("distr::q(",Distr,")(c(", quantiles, "), lower.tail=", tail == "lower",")", sep=""))
    putDialog(dialogName, list(initialValues=vars, tail=tclvalue(tailVar), quantiles=tclvalue(quantilesVar)), resettable=FALSE)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="q", helpPackage ="distr", reset = dialogName, apply=dialogName)
  tkgrid(labelRcmdr(entryFrame, text=gettextRcmdr("Probabilities")), quantilesEntry, sticky="w", padx=6)
  for (i in 1:nnVar) {
    if(nameVar!="generic"){
      tkgrid(labelRcmdr(entryFrame, text=gettextRcmdr(fVar$paramsLabels[i])), get(paramsEntry[i]), sticky="w", padx=6)
    } else{
      tkgrid(get(paramsFrame[i]), sticky="w", padx=6)
    }
  }
  
  tkgrid(lowerTailButton, labelRcmdr(buttonFrame, text=gettextRcmdr("Lower tail")), sticky="w")
  tkgrid(upperTailButton, labelRcmdr(buttonFrame, text=gettextRcmdr("Upper tail")), sticky="w")
  tkgrid(entryFrame, sticky="w")
  tkgrid(buttonFrame, sticky="w")
  tkgrid.configure(quantilesEntry, sticky="w")
  for (i in 1:nnVar) {
    tkgrid.configure(get(paramsEntry[i]), sticky="w")
  }
  tkgrid(buttonsFrame, sticky="ew")
  dialogSuffix(focus=quantilesEntry)
}


# Distribution Mass

binomialDistrMass <-function() {distrMass("binomial")}
PoissonDistrMass <-function() {distrMass("Poisson")}
geomDistrMass <-function() {distrMass("geom")}
negbinomialDistrMass <-function() {distrMass("negbinomial")}
DgenericDistrMass <-function() {distrMass("generic")}


distrMass  <- function(nameVar){
  # Author: J. Fox and Miroslav Ristic (Modified by M. A. Mosquera, 30 Nov 19)
  
  Library("distr")
  env <- environment()
  fVar<-get(paste(nameVar,"Distr",sep=""))
  
  # To ensure that menu name is included in pot file
  gettext(paste(if(nameVar!="generic") "" else "Discrete ",
                fVar$titleName," probabilities...",sep=""), domain="R-RcmdrPlugin.TeachStat")
  
  
  nnVar<-length(fVar$params)
  dialogName <- paste(if(nameVar=="generic")"D",nameVar,"DistrMass", sep="")
  defaults <- list(initialValues=fVar$initialValues,cum="")
  initial <- getDialog(dialogName, defaults=defaults)
  checkRange <- function(range){
    messageVar<-gettextRcmdr("Range of values over which to plot, %d, is large.\nCreate long output?")
    RcmdrTkmessageBox(message=sprintf(messageVar,range),
                      icon="warning", type="yesno", default="no")
  }
  initializeDialog(title=gettext(paste(if(nameVar!="generic") "" else "Discrete ",
                                       fVar$titleName," Probabilities",sep=""),
                                 domain="R-RcmdrPlugin.TeachStat"))
  entryFrame <- tkframe(top)
  paramsVar<-paste(fVar$params,"Var",sep="")
  paramsEntry<-paste(fVar$params,"Entry",sep="")
  paramsFrame<-paste(fVar$params,"Frame",sep="")
  
  if(nameVar!="generic"){
    for (i in 1:nnVar) {
      eval(parse(text=paste(paramsVar[i],"<-tclVar('",initial$initialValues[i],"')",sep="")))
      eval(parse(text=paste(paramsEntry[i],"<-ttkentry(entryFrame, width='6', textvariable=",paramsVar[i],")",sep="")))
    }
  } else{
    distype <- "DiscreteDistribution"
    listdist <- listDistrs(distype)
    for (i in 1:nnVar) {
      combo <- variableComboBox(entryFrame, variableList=listdist, initialSelection=initial$initialValues[i], 
                                title=gettext("Distribution (pick one)",domain="R-RcmdrPlugin.TeachStat"))
      # Deleting <no variable selected> option from the list
      varcombobox <- if(length(listdist)==0) gettext("<no distribution selected>",domain="R-RcmdrPlugin.TeachStat") 
      else listdist
      tkconfigure(combo$combobox,values=varcombobox)
      
      eval(parse(text=paste(paramsVar[i],'<- combo$combovar',sep="")))
      eval(parse(text=paste(paramsEntry[i],'<- combo$combobox',sep="")))
      eval(parse(text=paste(paramsFrame[i],'<- getFrame(combo)',sep="")))
    }
  }
  
  cumVar <- tclVar(initial$cum)
  buttonFrame <- ttkframe(top)
  cumButton <- ttkcheckbutton(buttonFrame, variable=cumVar, text=gettext("Show cumulative probabilities",
                                                                         domain="R-RcmdrPlugin.TeachStat")
                              )
  onOK <- function(){
    nameVarF<-get(dialogName,mode="function")
    closeDialog()
    warn <- options(warn=-1)
    vars<-numeric(nnVar)
    for (i in 1:nnVar) {
      vars[i]<-tclvalue(get(paramsVar[i]))
    }
    if(nameVar!="generic") vars <- as.numeric(vars)
    if (length(fVar$paramsRound)>0) {
      for (j in fVar$paramsRound) {
        vars[j]<-round(vars[j])
      }
    }
    options(warn)
    for (i in 1:length(fVar$errorConds)) {
      if (eval(parse(text=fVar$errorConds[i]))) {
        errorCondition(recall=nameVarF, message=gettextRcmdr(fVar$errorTexts[i]))
        return()
      }
    }
    
    pasteVar<-paste(fVar$params,"=",vars,sep="",collapse=", ")
    if(nameVar!="generic"){
      Distr <- paste(fVar$distrName,"(",pasteVar,")",sep="")
    } else {
      Distr <- vars[1]
    }
    
    .D1 <- eval(parse(text=Distr))
    lsupp <- eval(parse(text="length(support(.D1))"))
    if (lsupp > 51){
      if ("no" == tclvalue(checkRange(lsupp))){
        if (getRcmdr("grab.focus")) tkgrab.release(top)
        tkdestroy(top)
        nameVarF()
        return()
      }
    }
    
    cum <- tclvalue(cumVar)
    command <- paste("local({\n  .D <- ",Distr, sep="")
    command <- paste(command,"\n  .p <- d(.D)(support(.D))" , sep="")
    command <- paste(command,"\n  .Table <- data.frame(Probability=.p", if(cum=="1") ", Cum.probability=cumsum(.p)",")" , sep="")
    command <- paste(command, "\n  rownames(.Table) <- support(.D)", sep="")
    command <- paste(command, "\n  print(.Table)\n})", sep="")
    doItAndPrint(command)
    tkfocus(CommanderWindow())
    putDialog(dialogName, list(initialValues=vars,cum=tclvalue(cumVar)), resettable=FALSE)
  }
  OKCancelHelp(helpSubject="d", helpPackage ="distr", reset = dialogName, apply = dialogName)
  for (i in 1:nnVar) {
    if(nameVar!="generic"){
      tkgrid(labelRcmdr(entryFrame, text=gettextRcmdr(fVar$paramsLabels[i])), get(paramsEntry[i]), sticky="w", padx=6)
    } else{
      tkgrid(get(paramsFrame[i]), sticky="w", padx=6)
    }
  }
  tkgrid(entryFrame, sticky="w")
  tkgrid(cumButton,sticky="w")
  tkgrid(buttonFrame,sticky="w")
  tkgrid(buttonsFrame, columnspan=2, sticky="w")
  for (i in 1:nnVar) {
    tkgrid.configure(get(paramsEntry[i]), sticky="w")
  }
  dialogSuffix(focus=get(paramsEntry[1]))
}


# Random variable characteristics

normalDistrCharac <- function() {distrCharac("normal")}
tDistrCharac <-function() {distrCharac("t")}
chisqDistrCharac <-function() {distrCharac("chisq")}
FDistrCharac <-function() {distrCharac("F")}
exponentialDistrCharac <-function() {distrCharac("exponential")}
uniformDistrCharac <-function() {distrCharac("uniform")}
genericDistrCharac <-function() {distrCharac("generic")}

binomialDistrCharac <-function() {distrCharac("binomial",discrete=TRUE)}
PoissonDistrCharac <-function() {distrCharac("Poisson",discrete=TRUE)}
geomDistrCharac <-function() {distrCharac("geom",discrete=TRUE)}
negbinomialDistrCharac <-function() {distrCharac("negbinomial",discrete=TRUE)}
DgenericDistrCharac <-function() {distrCharac("generic",discrete=TRUE)}


distrCharac <- function(nameVar, discrete=FALSE){
  Library("distr")
  
  env <- environment()
  fVar<-get(paste(nameVar,"Distr",sep=""))
  
  # To ensure that menu name is included in pot file
  gettext(paste(if(nameVar!="generic") "" else if(discrete)
    "Discrete " else "Continuous ",
    fVar$titleName," characteristics...",sep=""), domain="R-RcmdrPlugin.TeachStat")
  
  nnVar<-length(fVar$params)
  dialogName <- paste(if(nameVar=="generic" & discrete)"D",nameVar,"DistrCharac", sep="")
  defaults <- list(initialValues=fVar$initialValues, expec="1", median="0", sd="1", IQR="0", 
                   moment="", moment.value="1", cmoment="", cmoment.value="2", skewness="0", kurtosis="0")
  initial <- getDialog(dialogName, defaults=defaults)
  initializeDialog(title=gettext(paste(if(nameVar!="generic") "" else if(discrete)
    "Discrete " else "Continuous ",
    fVar$titleName," Characteristics",sep=""),domain="R-RcmdrPlugin.TeachStat"))
  selectFrame <- ttkframe(top)
  entryFrame <- ttkframe(selectFrame)
  subentryFrame <- ttkframe(entryFrame)
  paramsVar<-paste(fVar$params,"Var",sep="")
  paramsEntry<-paste(fVar$params,"Entry",sep="")
  paramsFrame<-paste(fVar$params,"Frame",sep="")
  
  if(nameVar!="generic"){
    for (i in 1:nnVar) {
      eval(parse(text=paste(paramsVar[i],"<-tclVar('",initial$initialValues[i],"')",sep="")))
      eval(parse(text=paste(paramsEntry[i],"<-ttkentry(subentryFrame, width='6', textvariable=",paramsVar[i],")",sep="")))
    }
  } else{
    distype <- if(discrete) "DiscreteDistribution" else "AbscontDistribution"
    listdist <- listDistrs(distype)
    for (i in 1:nnVar) {
      combo <- variableComboBox(subentryFrame, variableList=listdist, initialSelection=initial$initialValues[i], 
                                title=gettext("Distribution (pick one)",domain="R-RcmdrPlugin.TeachStat"))
      # Deleting <no variable selected> option from the list
      varcombobox <- if(length(listdist)==0) gettext("<no distribution selected>",domain="R-RcmdrPlugin.TeachStat") 
      else listdist
      tkconfigure(combo$combobox,values=varcombobox)
      
      eval(parse(text=paste(paramsVar[i],'<- combo$combovar',sep="")))
      eval(parse(text=paste(paramsEntry[i],'<- combo$combobox',sep="")))
      eval(parse(text=paste(paramsFrame[i],'<- getFrame(combo)',sep="")))
    }
  }
  
  charFrame <- ttkframe(selectFrame)
  othFrame <- ttkframe(charFrame)
  checkBoxes(othFrame, frame="checkBoxFrame", boxes=c("expec", "median", "sd", "IQR", "skewness", "kurtosis"), 
             initialValues=c(initial$expec, initial$median, initial$sd, initial$IQR, 
                             initial$skewness, initial$kurtosis), 
             labels=gettext(c("Expectation", "Median", "Standard Deviation", "Interquartile Range",
                              "Skewness", "Kurtosis"),domain="R-RcmdrPlugin.TeachStat"), 
             title=gettext("Characteristics",domain="R-RcmdrPlugin.TeachStat"),columns = 2)
  momentFrame <- ttkframe(charFrame)
  momentVariable <- tclVar(initial$moment)
  momentCheckBox <- ttkcheckbutton(momentFrame, variable=momentVariable, text=gettext("Moment of order:", domain="R-RcmdrPlugin.TeachStat"),
                                command = function(){ 
                                  if(tclvalue(momentVariable)==1){
                                    tk2state.set(momentEntry, state = "normal")
                                  } else{
                                    tk2state.set(momentEntry, state = "disabled")
                                  }
                                })
  
  if(initial$moment!=1){
    mostrar2 <- "disabled"
  }else {
    mostrar2<-"normal"
  }
  
  momvalVariable <- tclVar(initial$moment.value)
  momentEntry <- ttkentry(momentFrame, width="4", textvariable=momvalVariable,state=mostrar2)
  
  cmomentVariable <- tclVar(initial$cmoment)
  cmomentCheckBox <- ttkcheckbutton(momentFrame, variable=cmomentVariable, 
                                    text=gettext("Central moment of order:", domain="R-RcmdrPlugin.TeachStat"), 
                                    command = function(){ 
                                     if(tclvalue(cmomentVariable)==1){
                                       tk2state.set(cmomentEntry, state = "normal")
                                     } else{
                                       tk2state.set(cmomentEntry, state = "disabled")
                                     }
                                   })
  
  if(initial$cmoment!=1){
    mostrar3 <- "disabled"
  }else {
    mostrar3<-"normal"
  }
  
  cmomvalVariable <- tclVar(initial$cmoment.value)
  cmomentEntry <- ttkentry(momentFrame, width="4", textvariable=cmomvalVariable,state=mostrar3)
  
  
  onOK <- function(){
    nameVarF<-get(dialogName,mode="function")
    closeDialog()
    
    expecVar <- tclvalue(expecVariable)
    medianVar <- tclvalue(medianVariable)
    sdVari <- tclvalue(sdVariable)
    IQRVar <- tclvalue(IQRVariable)
    momentVar <- tclvalue(momentVariable)
    cmomentVar <- tclvalue(cmomentVariable)
    skewnessVar <- tclvalue(skewnessVariable)
    kurtosisVar <- tclvalue(kurtosisVariable)
    moment <- tclvalue(momvalVariable)
    moment <- if(moment!="") as.numeric(moment) else ""
    cmoment <- tclvalue(cmomvalVariable)
    cmoment <- if(cmoment!="") as.numeric(cmoment) else ""
    
    warn <- options(warn=-1)
    vars<-numeric(nnVar)
    for (i in 1:nnVar) {
      vars[i]<- tclvalue(get(paramsVar[i]))
    }
    if(nameVar!="generic") vars <- as.numeric(vars)
    if (length(fVar$paramsRound)>0) {
      for (j in fVar$paramsRound) {
        vars[j]<-round(vars[j])
      }
    }
    options(warn)
    for (i in 1:length(fVar$errorConds)) {
      if (eval(parse(text=fVar$errorConds[i]))) {
        errorCondition(recall=nameVarF, message=gettextRcmdr(fVar$errorTexts[i]))
        return()
      }
    }
    
    if(momentVar=="1" & (is.na(as.integer(moment)) | !(isTRUE(all.equal(as.numeric(moment),as.integer(moment)))))){
      errorCondition(recall=nameVarF, message=gettext("Moment not specified or not an integer number.",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    if(cmomentVar=="1" & (is.na(as.integer(cmoment)) | !(isTRUE(all.equal(as.numeric(cmoment),as.integer(cmoment)))))){
      errorCondition(recall=nameVarF, message=gettext("Central moment not specified or not an integer number.",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    
    chars <- paste("c(",
                   paste(c('"expectation"', '"median"','"sd"', '"IQR"', '"skewness"', '"kurtosis"',
                           '"moment"', '"cmoment"')[c(expecVar, medianVar, sdVari, IQRVar, 
                                                      skewnessVar, kurtosisVar, momentVar, cmomentVar) == 1], 
                         collapse=", "), ")", sep="")
    if (chars == "c()"){
      errorCondition(recall=nameVarF, message=gettext("No characteristics selected.",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
   
    
    pasteVar<-paste(fVar$params,"=",vars,sep="",collapse=", ")
    if(nameVar!="generic"){
      Distr <- paste(fVar$distrName,"(",pasteVar,")",sep="")
    } else {
      Distr <- vars[1]
    }

    doItAndPrint(paste("characRV(", Distr, ", charact=", chars, 
                       if(momentVar=="1") paste0(", moment=", moment), if(cmomentVar=="1") paste0(", cmoment=", cmoment), ")", sep=""))

    putDialog(dialogName, list(initialValues=vars, expec=expecVar, median=medianVar, sd=sdVari,
                               IQR=IQRVar, moment=momentVar, moment.value=moment, cmoment=cmomentVar,
                               cmoment.value=cmoment, skewness=skewnessVar, kurtosis=kurtosisVar),
              resettable=FALSE)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="characRV", reset = dialogName, apply=dialogName)
  if(nameVar!="generic"){
    tkgrid(labelRcmdr(entryFrame, text=gettext(paste(fVar$titleName,"distribution's parameters:"),domain="R-RcmdrPlugin.TeachStat"),
                      foreground=getRcmdr("title.color") ), sticky="w")
  } 
  
  for (i in 1:nnVar) {
    if(nameVar!="generic"){
      tkgrid(labelRcmdr(subentryFrame, text=gettextRcmdr(fVar$paramsLabels[i])), get(paramsEntry[i]), sticky="w", padx=6)
    } else{
      tkgrid(get(paramsFrame[i]), sticky="w", padx=6)
    }
  }
  
  
  tkgrid(subentryFrame,sticky="w")
  tkgrid(checkBoxFrame,sticky="w")
  tkgrid(momentCheckBox, labelRcmdr(momentFrame,text="  "),momentEntry, sticky="w")
  tkgrid(cmomentCheckBox, labelRcmdr(momentFrame,text="  "),cmomentEntry, sticky="w")
  tkgrid(othFrame,sticky="w")
  tkgrid(momentFrame,padx=3,sticky="w")
  
  tkgrid(entryFrame, charFrame, padx="16", sticky="nw")
  tkgrid(selectFrame, sticky="w")
  
  for (i in 1:nnVar) {
    tkgrid.configure(get(paramsEntry[i]), sticky="w")
  }
  tkgrid(buttonsFrame, sticky="ew")
  dialogSuffix()
}


# Distribution Samples


normalDistrSamples <- function(){distrSamples("normal")}
tDistrSamples <-function() {distrSamples("t")}
chisqDistrSamples <-function() {distrSamples("chisq")}
FDistrSamples <-function() {distrSamples("F")}
exponentialDistrSamples <-function() {distrSamples("exponential")}
uniformDistrSamples <-function() {distrSamples("uniform")}
genericDistrSamples <-function() {distrSamples("generic")}

binomialDistrSamples <-function() {distrSamples("binomial",discrete=TRUE)}
PoissonDistrSamples <-function() {distrSamples("Poisson",discrete=TRUE)}
geomDistrSamples <-function() {distrSamples("geom",discrete=TRUE)}
negbinomialDistrSamples <-function() {distrSamples("negbinomial",discrete=TRUE)}
DgenericDistrSamples <-function() {distrSamples("generic",discrete=TRUE)}


distrSamples <- function(nameVar,discrete=FALSE) {
  # Author: J. Fox and Miroslav Ristic (Modified by M. A. Mosquera, 30 Nov 19)
  
  Library("distr")
  env <- environment()
  fVar<-get(paste(nameVar,"Distr",sep=""))
  
  # To ensure that menu name is included in pot file
  gettext(paste("Sample from ",if(nameVar!="generic") "" else if(discrete)
    "Discrete " else "Continuous ",
    fVar$titleName," distribution...",sep=""), domain="R-RcmdrPlugin.TeachStat")
  
  nnVar<-length(fVar$params)
  dialogName <- paste(if(nameVar=="generic" & discrete)"D",nameVar,"DistrSamples", sep="")
  defaults <- list(initialValues=fVar$initialValues, dsname=paste(if(nameVar=="generic" & discrete)"D",fVar$titleName,"Samples",sep=""),
                   nobs="1", nsamples="100", mean="0", sum="0", sd="0")
  initial <- getDialog(dialogName, defaults=defaults)
  initializeDialog(title=gettext(paste("Sample from ",if(nameVar!="generic") "" else if(discrete)
    "Discrete " else "Continuous ", fVar$titleName," Distribution",sep=""),domain="R-RcmdrPlugin.TeachStat"))
  entryFrame <- tkframe(top)
  dsname <- tclVar(initial$dsname)
  dsFrame <- tkframe(top)
  entryDsname <- ttkentry(dsFrame, width="20", textvariable=dsname)
  paramsVar<-paste(fVar$params,"Var",sep="")
  paramsEntry<-paste(fVar$params,"Entry",sep="")
  paramsFrame<-paste(fVar$params,"Frame",sep="")
  
  if(nameVar!="generic"){
    for (i in 1:nnVar) {
      eval(parse(text=paste(paramsVar[i],"<-tclVar('",initial$initialValues[i],"')",sep="")))
      eval(parse(text=paste(paramsEntry[i],"<-ttkentry(entryFrame, width='6', textvariable=",paramsVar[i],")",sep="")))
    }
  } else{
    distype <- if(discrete) "DiscreteDistribution" else "AbscontDistribution"
    listdist <- listDistrs(distype)
    for (i in 1:nnVar) {
      combo <- variableComboBox(entryFrame, variableList=listdist, initialSelection=initial$initialValues[i], 
                                title=gettext("Distribution (pick one)",domain="R-RcmdrPlugin.TeachStat"))
      # Deleting <no variable selected> option from the list
      varcombobox <- if(length(listdist)==0) gettext("<no distribution selected>",domain="R-RcmdrPlugin.TeachStat") 
      else listdist
      tkconfigure(combo$combobox,values=varcombobox)
      
      eval(parse(text=paste(paramsVar[i],'<- combo$combovar',sep="")))
      eval(parse(text=paste(paramsEntry[i],'<- combo$combobox',sep="")))
      eval(parse(text=paste(paramsFrame[i],'<- getFrame(combo)',sep="")))
    }
  }
  
  obserVar <- tclVar(initial$nobs)
  obserEntry <- ttkentry(entryFrame, width="6", textvariable=obserVar)
  samplesVar <- tclVar(initial$nsamples)
  samplesEntry <- ttkentry(entryFrame, width="6", textvariable=samplesVar)
  checkBoxes(frame="checkBoxFrame", boxes=c("mean", "sum", "sd"), 
             initialValues=c(initial$mean, initial$sum, initial$sd), 
             labels=gettextRcmdr(c("Sample means", "Sample sums",
                                   "Sample standard deviations")))
  onOK <- function(){
    nameVarF<-get(dialogName,mode="function")
    closeDialog()
    dsnameValue <- trim.blanks(tclvalue(dsname))
    if (dsnameValue == "") {
      errorCondition(recall=nameVarF, 
                     message=gettextRcmdr("You must enter the name of a data set."))  
      return()
    }  
    if (!is.valid.name(dsnameValue)) {
      errorCondition(recall=nameVarF,
                     message=paste('"', dsnameValue, '" ',
                                   gettextRcmdr("is not a valid name."), sep=""))
      return()
    }
    if (is.element(dsnameValue, listDataSets())) {
      if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
        nameVarF()
        return()
      }
    }
    warn <- options(warn=-1)
    vars<-numeric(nnVar)
    for (i in 1:nnVar) {
      vars[i]<-tclvalue(get(paramsVar[i]))
    }
    if(nameVar!="generic") vars <- as.numeric(vars)
    if (length(fVar$paramsRound)>0) {
      for (j in fVar$paramsRound) {
        vars[j]<-round(vars[j])
      }
    }
    options(warn)
    for (i in 1:length(fVar$errorConds)) {
      if (eval(parse(text=fVar$errorConds[i]))) {
        errorCondition(recall=nameVarF, message=gettextRcmdr(fVar$errorTexts[i]))
        return()
      }
    }
    obser <- as.numeric(tclvalue(obserVar))
    samples <- as.numeric(tclvalue(samplesVar))
    if (is.na(obser) || obser <= 0) {
      errorCondition(recall=nameVarF, 
                     message=gettextRcmdr("Sample size must be positive."))
      return()
    }
    if (is.na(samples) || samples <= 0) {
      errorCondition(recall=nameVarF, 
                     message=gettextRcmdr("Number of samples must be positive."))
      return()
    }
    pasteVar<-paste(fVar$params,"=",vars,sep="",collapse=", ")
    if(nameVar!="generic"){
      Distr <- paste(fVar$distrName,"(",pasteVar,")",sep="")
    } else {
      Distr <- vars[1]
    }
    
    command.1 <- paste(dsnameValue, " <- as.data.frame(matrix(r(",Distr,")(", samples,
                       "*", obser, "), ncol=", obser, "))", sep="")   
    doItAndPrint(command.1)
    command.1 <- if (samples == 1) 
      paste("rownames(", dsnameValue, ') <- "sample"', sep="")
    else paste("rownames(", dsnameValue, ') <- paste("sample", 1:', samples,
               ', sep="")', sep="")
    doItAndPrint(command.1)
    command.2 <- if (obser == 1) 
      paste("colnames(", dsnameValue, ') <- "obs"', sep="")
    else paste("colnames(", dsnameValue, ') <- paste("obs", 1:', obser,
               ', sep="")', sep="")
    doItAndPrint(command.2)
    any.summaries <- FALSE
    command.3 <- paste(dsnameValue," <- within(", dsnameValue, ", {", sep="")
    if (tclvalue(meanVariable) == "1") {
      any.summaries <- TRUE
      command.3 <- paste(command.3, "\n  mean <- rowMeans(", dsnameValue,
                         "[,1:", obser, "])", sep="")
    }
    if (tclvalue(sumVariable) == "1") {
      any.summaries <- TRUE
      command.3 <- paste(command.3, "\n  sum <- rowSums(", dsnameValue,
                         "[,1:", obser, "])", sep="")
    }
    if (tclvalue(sdVariable) == "1") {
      any.summaries <- TRUE
      command.3 <- paste(command.3, "\n  sd <- apply(", dsnameValue,
                         "[,1:", obser, "], 1, sd)", sep="")
    }
    command.3 <- paste(command.3, "\n})", sep="")
    if (any.summaries) doItAndPrint(command.3)
    activeDataSet(dsnameValue)
    tkfocus(CommanderWindow())
    putDialog(dialogName, list(initialValues=vars, dsname=dsnameValue,
                               nobs=obser, nsamples=samples, 
                               mean=tclvalue(meanVariable), sum=tclvalue(sumVariable), sd=tclvalue(sdVariable)),
              resettable=FALSE)
  }
  OKCancelHelp(helpSubject="r", helpPackage ="distr", reset=dialogName, apply=dialogName)
  tkgrid(labelRcmdr(dsFrame, text=gettextRcmdr("Enter name for data set:")), entryDsname, 
         sticky="w")
  tkgrid(dsFrame, columnspan=2, sticky="w")
  tkgrid(labelRcmdr(top, text=""))
  for (i in 1:nnVar) {
    if(nameVar!="generic"){
      tkgrid(labelRcmdr(entryFrame, text=gettextRcmdr(fVar$paramsLabels[i])), get(paramsEntry[i]), sticky="w", padx=6)
    } else{
      tkgrid(get(paramsFrame[i]), sticky="w", padx=6)
    }
  }
  
  tkgrid(labelRcmdr(entryFrame, text=gettextRcmdr("Number of samples (rows) ")), samplesEntry, sticky="w", padx=6)
  tkgrid(labelRcmdr(entryFrame, text=gettextRcmdr("Number of observations (columns) ")), obserEntry, sticky="w", padx=6)
  tkgrid(entryFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=""))
  tkgrid(labelRcmdr(top, text=gettextRcmdr("Add to Data Set:"), fg=getRcmdr("title.color"), font="RcmdrTitleFont"), sticky="w")
  tkgrid(checkBoxFrame, columnspan=2, sticky="w")
  tkgrid(labelRcmdr(top, text=""))
  tkgrid(buttonsFrame, columnspan=2, sticky="w")
  dialogSuffix(focus=get(paramsEntry[1]))
}



# Define distributions

normalDistrDefine <- function(){distrDefine("normal")}
tDistrDefine <-function() {distrDefine("t")}
chisqDistrDefine <-function() {distrDefine("chisq")}
FDistrDefine <-function() {distrDefine("F")}
exponentialDistrDefine <-function() {distrDefine("exponential")}
uniformDistrDefine <-function() {distrDefine("uniform")}
genericDistrDefine <-function() {distrDefine("generic")}

binomialDistrDefine <-function() {distrDefine("binomial",discrete=TRUE)}
PoissonDistrDefine <-function() {distrDefine("Poisson",discrete=TRUE)}
geomDistrDefine <-function() {distrDefine("geom",discrete=TRUE)}
negbinomialDistrDefine <-function() {distrDefine("negbinomial",discrete=TRUE)}
DgenericDistrDefine <-function() {distrDefine("generic",discrete=TRUE)}


distrDefine <- function(nameVar, discrete=FALSE){
  Library("distr")
  env <- environment()
  fVar<-get(paste(nameVar,"Distr",sep=""))
  
  # To ensure that menu name is included in pot file
  gettext(paste(if(nameVar!="generic") "" else if(discrete)
    "Discrete " else "Continuous ",
    fVar$titleName," distribution definition...",sep=""), domain="R-RcmdrPlugin.TeachStat")
  
  nnVar<-length(fVar$params)
  dialogName <- paste(if(nameVar=="generic" & discrete)"D",nameVar,"DistrDefine", sep="")
  defaults <- list(initialValues=fVar$initialValues, dname=if(nameVar=="generic") "G" else paste(fVar$distrName,
                                                                 paste(fVar$params,fVar$initialValues, sep="_",collapse="_"),sep="_"),
                   massorcum="mass", densorcum="dens", supp="", proba="", npieces=1, def="", from="", to="")
  initial <- getDialog(dialogName, defaults=defaults)
  initializeDialog(title=gettext(paste(if(nameVar!="generic") "" else if(discrete)
    "Discrete " else "Continuous ",
    fVar$titleName," Distribution Definition",sep=""),domain="R-RcmdrPlugin.TeachStat"))
  entryFrame  <- ttkframe(top)
  dname <- tclVar(initial$dname)
  entryDname <- ttkentry(entryFrame, width="20", textvariable=dname)
  
  paramFrame <- ttkframe(entryFrame)
  
  vars <- initial$initialValues
  massorcum <- initial$massorcum
  densorcum <- initial$densorcum
  supp <- initial$supp
  proba <- initial$proba
  npieces <- initial$npieces
  def <- initial$def
  from <- initial$from
  to <- initial$to
  
  if(nameVar!="generic"){
    paramsVar<-paste(fVar$params,"Var",sep="")
    paramsEntry<-paste(fVar$params,"Entry",sep="")
    for (i in 1:nnVar) {
      eval(parse(text=paste(paramsVar[i],"<-tclVar('",initial$initialValues[i],"')",sep="")))
      eval(parse(text=paste(paramsEntry[i],"<-ttkentry(paramFrame, width='6', textvariable=",paramsVar[i],")",sep="")))
    }
  } else if (discrete){
    radioButtons(paramFrame, "massorcum", buttons=c("mass", "cum"),
                 labels=gettext(c("Probability mass function", "Distribution function"),domain="R-RcmdrPlugin.TeachStat"),
                 title=gettext("Select function to provide:",domain="R-RcmdrPlugin.TeachStat"),
                 initialValue = initial$massorcum, columns=2)
    suppVar<-tclVar(initial$supp)
    suppEntry <- ttkentry(paramFrame, width='20', textvariable=suppVar)
    probVar<-tclVar(initial$proba)
    probEntry <- ttkentry(paramFrame, width='20', textvariable=probVar)
  } else{
    addPiece <- function(...){
      assign("npieces", npieces+1, envir = env)
      def.Var <- paste0("def",npieces,".Var")
      assign("defVar", c(defVar,def.Var), envir = env)
      def.Entry <- paste0("def",npieces,".Entry")
      from.Var <- paste0("from",npieces,".Var")
      assign("fromVar", c(fromVar,from.Var), envir = env)
      from.Entry <- paste0("from",npieces,".Entry")
      to.Var <- paste0("to",npieces,".Var")
      assign("toVar", c(toVar,to.Var), envir = env)
      to.Entry <- paste0("to",npieces,".Entry")
      eval(parse(text=paste(def.Var," <- tclVar()", sep="")), envir=env)
      eval(parse(text=paste(def.Entry," <- ttkentry(pieceFrame, width='40', textvariable=",def.Var,")",sep="")), envir=env)
      eval(parse(text=paste(from.Var," <- tclVar()", sep="")), envir=env)
      eval(parse(text=paste(from.Entry," <- ttkentry(pieceFrame, width='10', textvariable=",from.Var,")",sep="")), envir=env)
      eval(parse(text=paste(to.Var," <- tclVar()", sep="")), envir=env)
      eval(parse(text=paste(to.Entry," <- ttkentry(pieceFrame, width='10', textvariable=",to.Var,")",sep="")), envir=env)
      
      eval(parse(text=paste('tkgrid(',def.Entry,', ', from.Entry,', ', to.Entry, ', padx=6, pady=2, sticky="w")',sep="")), envir=env)
    }
    delPiece <- function(...){
      def.Var <- paste0("def",npieces,".Var")
      def.Entry <- paste0("def",npieces,".Entry")
      from.Var <- paste0("from",npieces,".Var")
      from.Entry <- paste0("from",npieces,".Entry")
      to.Var <- paste0("to",npieces,".Var")
      to.Entry <- paste0("to",npieces,".Entry")
      eval(parse(text=paste("tkgrid.forget(",paste(c(def.Entry,from.Entry,to.Entry),collapse=","),")",sep="")), envir=env)
      eval(parse(text=paste("rm(",paste(c(def.Entry,from.Entry,to.Entry),collapse=","),")",sep="")), envir=env)
      assign("defVar",defVar[defVar!=def.Var], envir = env)
      assign("fromVar",fromVar[fromVar!=from.Var], envir = env)
      assign("toVar",toVar[toVar!=to.Var], envir = env)
      assign("npieces", npieces-1, envir = env)
    }
    radioButtons(paramFrame, "densorcum", buttons=c("dens", "cum"), 
                 labels=gettext(c("Density function", "Distribution function"),domain="R-RcmdrPlugin.TeachStat"),
                 title=gettext("Select function to provide:",domain="R-RcmdrPlugin.TeachStat"),
                 initialValue = initial$densorcum, columns=2)
    pieceFrame <- ttkframe(paramFrame)
    
    defVar<-paste("def",1:npieces,".Var",sep="")
    defEntry<-paste("def",1:npieces,".Entry",sep="")
    fromVar<-paste("from",1:npieces,".Var",sep="")
    fromEntry<-paste("from",1:npieces,".Entry",sep="")
    toVar<-paste("to",1:npieces,".Var",sep="")
    toEntry<-paste("to",1:npieces,".Entry",sep="")
    for (i in 1:npieces) {
      eval(parse(text=paste(defVar[i],"<-tclVar('",initial$def[i],"')",sep="")))
      eval(parse(text=paste(defEntry[i],"<-ttkentry(pieceFrame, width='40', textvariable=",defVar[i],")",sep="")))
      eval(parse(text=paste(fromVar[i],"<-tclVar('",initial$from[i],"')",sep="")))
      eval(parse(text=paste(fromEntry[i],"<-ttkentry(pieceFrame, width='10', textvariable=",fromVar[i],")",sep="")))
      eval(parse(text=paste(toVar[i],"<-tclVar('",initial$to[i],"')",sep="")))
      eval(parse(text=paste(toEntry[i],"<-ttkentry(pieceFrame, width='10', textvariable=",toVar[i],")",sep="")))
    }
    
    buttonFrame <- ttkframe(paramFrame)
    addpieceButton <- buttonRcmdr(buttonFrame, text = gettext("Add piece",domain="R-RcmdrPlugin.TeachStat"), 
                             command = function(){
                               addPiece()
                               if(npieces>1) tk2state.set(delpieceButton, state = "normal")
                             })
    delpieceButton <- buttonRcmdr(buttonFrame, text = gettext("Delete last piece",domain="R-RcmdrPlugin.TeachStat"), 
                                  command = function(){
                                    delPiece()
                                    if(npieces<=1) tk2state.set(delpieceButton, state = "disabled")
                                  }, state=if(npieces<=1) "disabled" else "normal")
  }
  
  
  onOK <- function(){
    nameVarF<-get(dialogName,mode="function")
    closeDialog()
    
    dnameValue <- trim.blanks(tclvalue(dname))
    if (dnameValue == "") {
      errorCondition(recall=nameVarF, 
                     message=gettext("You must enter the name of a distribution.",domain="R-RcmdrPlugin.TeachStat"))  
      return()
    }  
    if (!is.valid.name(dnameValue)) {
      errorCondition(recall=nameVarF,
                     message=paste('"', dsnameValue, '" ',
                                   gettextRcmdr("is not a valid name."), sep=""))
      return()
    }
    if (is.element(dnameValue, listDistrs())) {
      if ("no" == tclvalue(checkReplace(dnameValue, gettext("Distribution",domain="R-RcmdrPlugin.TeachStat")))){
        nameVarF()
        return()
      }
    }
    
    if(nameVar!="generic"){
      warn <- options(warn=-1)
      vars<-numeric(nnVar)
      for (i in 1:nnVar) {
        vars[i]<- as.numeric(tclvalue(get(paramsVar[i])))
      }
      if (length(fVar$paramsRound)>0) {
        for (j in fVar$paramsRound) {
          vars[j]<-round(vars[j])
        }
      }
      options(warn)
      for (i in 1:length(fVar$errorConds)) {
        if (eval(parse(text=fVar$errorConds[i]))) {
          errorCondition(recall=nameVarF, message=gettextRcmdr(fVar$errorTexts[i]))
          return()
        }
      }
      
      pasteVar<-paste(fVar$params,"=",vars,sep="",collapse=", ")
      Distr <- paste(fVar$distrName,"(",pasteVar,")",sep="")
      
      command<-paste0(dnameValue,' <- ',Distr)
      
    } else if(discrete){
      massorcum <- tclvalue(massorcumVariable)
      supp <- gsub(" +", ",", gsub(",", " ", tclvalue(suppVar)))
      if ("" == supp) {
        errorCondition(recall=nameVarF, message=gettext("No support specified.",domain="R-RcmdrPlugin.TeachStat"))
        return()
      }
      suppo <- paste0("c(", supp, ")")
      .supp <- eval(parse(text=suppo))
      if(!identical(all.equal(.supp,sort(.supp)),TRUE)){
        errorCondition(recall=nameVarF, message=gettext("Support is not ordered.",domain="R-RcmdrPlugin.TeachStat"))
        return()
      }
      rm(.supp)
      
      proba <- gsub(" +", ",", gsub(",", " ", tclvalue(probVar)))
      probab <- paste0("c(", proba, ")")
      if((massorcum!="mass") & (probab !="c()")){
        .proba <- eval(parse(text=probab))
        if(!identical(all.equal(.proba,sort(.proba)),TRUE)){
          errorCondition(recall=nameVarF, message=gettext("Probabilities are not ordered.",domain="R-RcmdrPlugin.TeachStat"))
          return()
        }
        if(!identical(all.equal(rev(.proba)[1], 1), TRUE)){
          errorCondition(recall=nameVarF, message=gettext("Last probability must be 1.",domain="R-RcmdrPlugin.TeachStat"))
          return()
        }
        probab <- paste0("c(",paste(c(.proba[1],diff(.proba)),collapse=","),")")
        rm(.proba)
      }
      command <- paste0(dnameValue,' <- DiscreteDistribution(',suppo,if(probab !="c()") paste(",",probab),")" )
      
    } else{
      densorcum <- tclvalue(densorcumVariable)
      warn <- options(warn=-1)
      def <- from <- to <- character(npieces)
      for (i in 1:npieces) {
        def[i]<- tclvalue(get(defVar[i]))
        from[i]<- as.numeric(tclvalue(get(fromVar[i])))
        to[i]<- as.numeric(tclvalue(get(toVar[i])))
      }
      options(warn)
      
      if (any(def == "")) {
        errorCondition(recall=nameVarF, message=gettext("Some definition not specified.", domain="R-RcmdrPlugin.TeachStat"))
        return()
      }
      if (any(is.na(from[-1]))) {
        errorCondition(recall=nameVarF, message=gettext("Some 'from' not specified.", domain="R-RcmdrPlugin.TeachStat"))
        return()
      }
      if (any(is.na(rev(to)[-1]))) {
        errorCondition(recall=nameVarF, message=gettext("Some 'to' not specified.", domain="R-RcmdrPlugin.TeachStat"))
        return()
      }
      
      if(is.na(from[1])) from[1] <- -Inf
      if(is.na(to[npieces])) to[npieces] <- Inf
      for (i in 1:npieces){
        if (!is.valid.number(from[i])){
          errorCondition(recall=nameVarF, message=paste(from[i], gettextRcmdr("is not a valid number.")))
          return()
        }
        if (!is.valid.number(to[i])){
          errorCondition(recall=nameVarF, message=paste(to[i], gettextRcmdr("is not a valid number.")))
          return()
        }
      }
      if (!all(from<to)) {
        errorCondition(recall=nameVarF, message=gettext("'to' must be greater than 'from'.",domain="R-RcmdrPlugin.TeachStat"))
        return()
      }
      if (!all(to[-npieces]<=from[-1])) {
        errorCondition(recall=nameVarF, message=gettext("Overlapping definitions.",domain="R-RcmdrPlugin.TeachStat"))
        return()
      }
      if (!identical(all.equal(from[-1],to[-npieces]),TRUE)) {
        errorCondition(recall=nameVarF, message=gettext("Definition intervals must be consecutive.",domain="R-RcmdrPlugin.TeachStat"))
        return()
      }
      low1t <- if(from[1]>-Inf) paste0(", low1=",from[1]) else ""
      up1t <- if(to[npieces]<Inf) paste0(", up1=",to[npieces]) else ""
      if(densorcum=="dens"){
        dorp <- "d = "
        func <- paste0("function(y) sapply(y, function(x) {\n\t\t\t d <- if(x < ",from[1],"){\n\t\t\t\t\t 0")
        for(i in 1:npieces){
          ineq <- if(npieces==1) " <= " else " < "
          func <- paste0(func,"\n\t\t\t\t } else if(x",ineq,to[i],"){\n\t\t\t\t\t ",def[i])
        }
        func <- paste0(func,"\n\t\t\t\t } else{\n\t\t\t\t\t 0 \n\t\t\t\t }")
        func <- paste0(func,"\n\t\t\t return(d)\n\t\t})",low1t,up1t,", withStand=TRUE")
      } else{
        dorp <- "d = "
        func <- paste0("function(y) sapply(y, function(x, ...) {\n\t\t\t p <- if(x < ",from[1],"){\n\t\t\t\t\t 0")
        
        for(i in 1:npieces){
          ineq <- if(npieces==1) " <= " else " < "
          func <- paste0(func,'\n\t\t\t\t } else if(x',ineq,to[i],'){\n\t\t\t\t\t eval(D(expression(',def[i],'),"x"))')
        }
        func <- paste0(func,"\n\t\t\t\t } else{\n\t\t\t\t\t 0 \n\t\t\t\t }")
        func <- paste0(func,"\n\t\t\t return(p)\n\t\t})",low1t,up1t,", withStand=TRUE")
        
      }
      
      command <- paste0(dnameValue,' <- AbscontDistribution(',dorp,func,")" )
    }
    result <- doItAndPrint(command)
    if (class(result)[1] !=  "try-error") activateMenus()
    
    putDialog(dialogName, list(initialValues=vars, dname= dnameValue, massorcum=massorcum, 
                               densorcum=densorcum, supp=supp, proba=proba, npieces=npieces, def=def, from=from, to=to),
              resettable=FALSE)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="distrDefine", reset = dialogName, apply=dialogName)
  tkgrid(labelRcmdr(entryFrame, text=gettext("Enter name for the distribution", domain="R-RcmdrPlugin.TeachStat")), entryDname, 
         sticky="w", padx="6",pady="10")
  
  tkgrid(labelRcmdr(paramFrame, text=gettext(paste(if(nameVar!="generic") "" else if(discrete)
    "Discrete " else "Continuous ", fVar$titleName," distribution's parameters:",sep=""),domain="R-RcmdrPlugin.TeachStat"),
                    foreground=getRcmdr("title.color")),columns="2",sticky="w")
  if(nameVar!="generic"){
    for (i in 1:nnVar) {
      tkgrid(labelRcmdr(paramFrame, text=gettextRcmdr(fVar$paramsLabels[i])), get(paramsEntry[i]), sticky="w", padx=6)
    }
  } else if(discrete){
    tkgrid(massorcumFrame, columns="2", pady="3",sticky="w")
    tkgrid(labelRcmdr(paramFrame, text=gettext("Support",domain="R-RcmdrPlugin.TeachStat")), suppEntry, sticky="w", padx=6)
    tkgrid(labelRcmdr(paramFrame, text=gettext("Probabilities",domain="R-RcmdrPlugin.TeachStat")), probEntry, sticky="w", padx=6)
  } else{
    tkgrid(densorcumFrame, columns="2", pady="3",sticky="w")
    tkgrid(labelRcmdr(pieceFrame, text=gettext("Definition",domain="R-RcmdrPlugin.TeachStat")),
           labelRcmdr(pieceFrame, text=gettext("From",domain="R-RcmdrPlugin.TeachStat")), 
           labelRcmdr(pieceFrame, text=gettext("To",domain="R-RcmdrPlugin.TeachStat")), sticky="w", padx=6)
    for (i in 1:npieces){
      tkgrid(get(defEntry[i]), get(fromEntry[i]), get(toEntry[i]) , padx=6, pady=2, sticky="w")
    }
    
    tkgrid(pieceFrame, sticky="w")
    tkgrid(addpieceButton,delpieceButton, padx=2, pady="6", sticky="e")
    tkgrid(buttonFrame, sticky="e")
  }
  tkgrid(paramFrame, columns="2", sticky="w")
  tkgrid(entryFrame, sticky="w")
  
  tkgrid(buttonsFrame, sticky="ew")
  dialogSuffix()
}
