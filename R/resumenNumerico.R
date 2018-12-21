resumenNumerico <- function(){
  Library("abind")
  Library("e1071")
  defaults <- list(initial.x=NULL,initial.sg2=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),initial.sg="0",initial.g=NULL, initial.mean="1", initial.sd="1", initial.se.mean="0", initial.IQR="1", initial.cv="0",
                   initial.quantiles.variable="1",
                   initial.quantiles="0, .25, .5, .75, 1",
                   initial.skewness="0", initial.kurtosis="0", initial.type="2",
                   initial.group=NULL, initial.tab=0)

  dialog.values <- getDialog("resumenNumerico", defaults)
  initial.group <- dialog.values$initial.group
  initializeDialog(title=gettext("Numerical summaries",domain="R-RcmdrPlugin.TeachStat"), use.tabs=TRUE, tabs=c("dataTab", "statisticsTab"))

   xBox <- variableListBox(dataTab, Numeric(), selectmode="multiple", title=gettext("Variables (pick one or more)",domain="R-RcmdrPlugin.TeachStat"),
                          initialSelection=varPosn(dialog.values$initial.x, "numeric"))
   if (length(Factors())!=0){
     mostrar<-"readonly"
   }else {
     mostrar<-"disabled"
   }

  selectGroupComboBox <- variableComboBox(dataTab, variableList=Factors(), state=mostrar,
                    initialSelection=dialog.values$initial.sg2, title=gettext("Group (pick one)",domain="R-RcmdrPlugin.TeachStat"))

  groupFrame<-tkframe(dataTab)
  checkBoxes(window = dataTab, frame="groupFrame", boxes="sgroups",
             initialValues=dialog.values$initial.sg,labels=gettext("Pick a group",domain="R-RcmdrPlugin.TeachStat"))
  gBox <- variableListBox(dataTab, gettext("No factors",domain="R-RcmdrPlugin.TeachStat"), selectmode="single",title=gettext("Group (pick one)",domain="R-RcmdrPlugin.TeachStat"),
                          initialSelection=varPosn(dialog.values$initial.g, "factor"))




  checkBoxes(window = statisticsTab, frame="checkBoxFrame", boxes=c("mean", "sd", "se.mean", "IQR", "cv"),
             initialValues=c(dialog.values$initial.mean, dialog.values$initial.sd, dialog.values$initial.se.mean, dialog.values$initial.IQR, dialog.values$initial.cv),
             labels=gettext(c("Mean", "Standard Deviation", "Standard Error of Mean", "Interquartile Range", "Coefficient of Variation"),domain="R-RcmdrPlugin.TeachStat"))
  skFrame <- tkframe(statisticsTab)
  checkBoxes(window = skFrame, frame="skCheckBoxFrame", boxes=c("skewness", "kurtosis"),
             initialValues=c(dialog.values$initial.skewness, dialog.values$initial.kurtosis),
             labels=gettext(c("Skewness", "Kurtosis"),domain="R-RcmdrPlugin.TeachStat"))
  radioButtons(window = skFrame, name="typeButtons", buttons=c("b1", "b2", "b3"), values=c("1", "2", "3"),
               initialValue=dialog.values$initial.type,
               labels=gettext(c("Type 1", "Type 2", "Type 3"),domain="R-RcmdrPlugin.TeachStat"))
  quantilesVariable <- tclVar(dialog.values$initial.quantiles.variable)
  quantilesFrame <- tkframe(statisticsTab)
  quantilesCheckBox <- tkcheckbutton(quantilesFrame, variable=quantilesVariable,
                                     text=gettext("Quantiles:",domain="R-RcmdrPlugin.TeachStat"))
  quantiles <- tclVar(dialog.values$initial.quantiles)
  quantilesEntry <- ttkentry(quantilesFrame, width="20", textvariable=quantiles)
  groupsBox(recall=resumenNumerico, label=gettext("Summarize by:",domain="R-RcmdrPlugin.TeachStat"),
            initialLabel=if (is.null(initial.group)) gettext("Summarize by groups",domain="R-RcmdrPlugin.TeachStat")
            else paste(gettext("Summarize by:",domain="R-RcmdrPlugin.TeachStat"), initial.group),
            initialGroup=initial.group, window = dataTab)
  onOK <- function(){
    tab <- if (as.character(tkselect(notebook)) == dataTab$ID) 0 else 1
    x <- getSelection(xBox)
    g<- getSelection(gBox)
    sgVar<-tclvalue(sgroupsVariable)
    sg2var <- getSelection(selectGroupComboBox)
    #doItAndPrint(str(sg2var))
    quants <- tclvalue(quantiles)
    meanVar <- tclvalue(meanVariable)
    sdVar <- tclvalue(sdVariable)
    se.meanVar <- tclvalue(se.meanVariable)
    IQRVar <- tclvalue(IQRVariable)
    cvVar <- tclvalue(cvVariable)
    quantsVar <- tclvalue(quantilesVariable)
    skewnessVar <- tclvalue(skewnessVariable)
    kurtosisVar <- tclvalue(kurtosisVariable)
    typeVar <- tclvalue(typeButtonsVariable)
    putDialog("resumenNumerico", list(
      initial.x=x,initial.sg2=sg2var,initial.sg=sgVar, initial.g=g, initial.mean=meanVar, initial.sd=sdVar, initial.se.mean=se.meanVar, initial.IQR=IQRVar, initial.cv=cvVar,
      initial.quantiles.variable=quantsVar, initial.quantiles=quants,
      initial.skewness=skewnessVar, initial.kurtosis=kurtosisVar, initial.type=typeVar,
      initial.group=if (.groups != FALSE) .groups else NULL, initial.tab=tab
    ))
    if (length(x) == 0){
      errorCondition(recall=resumenNumerico, message=gettext("No variable selected",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    closeDialog()
    quants <- paste(gsub(",+", ",", gsub(" ", ",", quants)), sep="")
    
    quants <- as.numeric( strsplit(quants,split=",")[[1]])
    
    if(((NA %in% quants)||(length( quants[(quants<0)|(quants>1)])!=0) || length(quants)<=1) &&(quantsVar==1)){
      errorCondition(recall=resumenNumerico, message=gettext("Quantiles must be a numeric vector in [0,1]",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    
    if((length(quants)==0 )&&(quantsVar==1)){
      errorCondition(recall=resumenNumerico, message=gettext("Quantiles must be a numeric vector in [0,1]",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    
    quants<-paste("c(",paste(quants, collapse=",", sep=""),")",sep="")
    
    .activeDataSet <- ActiveDataSet()
    vars <- if (length(x) == 1) paste('"', x, '"', sep="")
    else paste("c(", paste('"', x, '"', collapse=", ", sep=""), ")", sep="")
    vars <- paste(.activeDataSet, "[,", vars, "]", sep="")
    stats <- paste("c(",
                   paste(c('"mean"', '"sd"', '"se(mean)"', '"IQR"', '"quantiles"', '"cv"', '"skewness"', '"kurtosis"')
                         [c(meanVar, sdVar, se.meanVar, IQRVar, quantsVar, cvVar, skewnessVar, kurtosisVar) == 1],
                         collapse=", "), ")", sep="")
    if (stats == "c()"){
      errorCondition(recall=resumenNumerico, message=gettext("No statistics selected",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    type.text <- if (skewnessVar == 1 || kurtosisVar == 1) paste(', type="', typeVar, '"', sep="") else ""
    command <- if (length(Factors())!=0 && sg2var!=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")) {
      grps <- paste(.activeDataSet, "$", sg2var, sep="")
      paste("numSummary(", vars, ", groups=", grps, ", statistics=", stats,
            ", quantiles=", quants, type.text, ")", sep="")
    }
    else  paste("numSummary(", vars, ", statistics=", stats,
                ", quantiles=", quants, type.text, ")", sep="")
    doItAndPrint(command)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="numSummary", reset="resumenNumerico", apply ="resumenNumerico")
  #if (length(Factors())!=0){
  tkgrid(getFrame(xBox),labelRcmdr(dataTab, text="     "),getFrame(selectGroupComboBox),sticky="nw")
 #}else tkgrid(getFrame(xBox),labelRcmdr(dataTab, text="     "),getFrame(gBox),sticky="nw")
  #tkgrid(getFrame(xBox),sticky="nw")
  #tkgrid(getFrame(selectGroupComboBox), sticky="nw")
 # tkgrid(groupFrame,sticky="e")
# tkgrid(getFrame(gBox),sticky="e")
  tkgrid(checkBoxFrame, sticky="nw")
  tkgrid(skCheckBoxFrame, typeButtonsFrame, sticky="nw")
  tkgrid(skFrame, sticky="w")
  tkgrid(quantilesCheckBox, quantilesEntry, sticky="w")
  tkgrid(quantilesFrame)
  #tkgrid(groupsFrame, sticky = "w", padx=6)
  dialogSuffix(use.tabs=TRUE, grid.buttons=TRUE, tabs=c("dataTab", "statisticsTab"),
               tab.names=c("Data", "Statistics"))
}
