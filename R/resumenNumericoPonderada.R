resumenNumericoPonderada <- function(){
  Library("abind")
  Library("e1071")
#  Library("Hmisc")

  defaults <- list(initial.x=NULL,initial.sg=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),
                   initial.sg2=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),
                   initial.mean="1",
                   initial.sd="1", initial.se.mean="0", initial.IQR="1", initial.cv="0",
                   initial.quantiles.variable="1",
                   initial.quantiles="0, .25, .5, .75, 1",
                   initial.skewness="0", initial.kurtosis="0", initial.tab=0)

  dialog.values <- getDialog("resumenNumericoPonderada", defaults)
  initial.group <- dialog.values$initial.group
  initializeDialog(title=gettext("Numerical summaries - Weighted variables",domain="R-RcmdrPlugin.TeachStat"), use.tabs=TRUE, tabs=c("dataTab", "statisticsTab"))

   xBox <- variableListBox(dataTab, Numeric(), selectmode="multiple", title=gettext("Variables (pick one or more)",domain="R-RcmdrPlugin.TeachStat"),
                          initialSelection=varPosn(dialog.values$initial.x, "numeric"))

  selectVariablePonderacion <- variableComboBox(dataTab, variableList=Numeric(),
                              initialSelection=dialog.values$initial.sg, title=gettext("Weight variable",domain="R-RcmdrPlugin.TeachStat"))

    if (length(Factors())!=0){
     mostrar<-"readonly"
   }else {
     mostrar<-"disabled"
   }

  selectGroupComboBox <- variableComboBox(dataTab, variableList=Factors(), state=mostrar,
                    initialSelection=dialog.values$initial.sg2, title=gettext("Group (pick one)",domain="R-RcmdrPlugin.TeachStat"))



  checkBoxes(window = statisticsTab, frame="checkBoxFrame", boxes=c("mean", "sd", "se.mean", "IQR", "cv","skewness", "kurtosis"),
             initialValues=c(dialog.values$initial.mean, dialog.values$initial.sd, dialog.values$initial.se.mean, dialog.values$initial.IQR, dialog.values$initial.cv,dialog.values$initial.skewness, dialog.values$initial.kurtosis),
             labels=gettext(c("Mean", "Standard Deviation", "Standard Error of Mean", "Interquartile Range", "Coefficient of Variation","Skewness", "Kurtosis"),domain="R-RcmdrPlugin.TeachStat"))

  quantilesVariable <- tclVar(dialog.values$initial.quantiles.variable)
  quantilesFrame <- tkframe(statisticsTab)
  quantilesCheckBox <- tkcheckbutton(quantilesFrame, variable=quantilesVariable,
                                     text=gettext("Quantiles:",domain="R-RcmdrPlugin.TeachStat"))
  quantiles <- tclVar(dialog.values$initial.quantiles)
  quantilesEntry <- ttkentry(quantilesFrame, width="20", textvariable=quantiles)


  onOK <- function(){
    tab <- if (as.character(tkselect(notebook)) == dataTab$ID) 0 else 1
    x <- getSelection(xBox)
    pondVar<-getSelection(selectVariablePonderacion)
    g<- getSelection(selectGroupComboBox)

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

    putDialog("resumenNumericoPonderada", list(
      initial.x=x,initial.sg=pondVar,initial.sg2=g, initial.mean=meanVar, initial.sd=sdVar, initial.se.mean=se.meanVar, initial.IQR=IQRVar, initial.cv=cvVar,
      initial.quantiles.variable=quantsVar, initial.quantiles=quants,
      initial.skewness=skewnessVar, initial.kurtosis=kurtosisVar, initial.tab=tab
    ))
    if (length(x) == 0){
      errorCondition(recall=resumenNumericoPonderada, message=gettext("No variable selected",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    closeDialog()
    
    quants <- paste(gsub(",+", ",", gsub(" ", ",", quants)), sep="")
    
    quants <- as.numeric( strsplit(quants,split=",")[[1]])

    posiblesstatistic<-c("mean", "sd", "se(mean)", "IQR", "quantiles", "cv", "skewness", "kurtosis")
    statselegidas<-c(meanVar, sdVar, se.meanVar, IQRVar, quantsVar, cvVar, skewnessVar, kurtosisVar)

    #print(posiblesstatistic)
    #print(statselegidas)

     stats <- posiblesstatistic[as.logical(as.numeric(statselegidas))]

    if (length(stats) == 0){
      errorCondition(recall=resumenNumericoPonderada, message=gettext("No statistics selected",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }

    if(((NA %in% quants)||(length( quants[(quants<0)|(quants>1)])!=0) || length(quants)<=1)&&(quantsVar==1)){
       errorCondition(recall=resumenNumericoPonderada, message=gettext("Quantiles must be a numeric vector in [0,1]",domain="R-RcmdrPlugin.TeachStat"))
       return()
    }

     if((length(quants)==0)&&(quantsVar==1)){
       errorCondition(recall=resumenNumericoPonderada, message=gettext("Quantiles must be a numeric vector in [0,1]",domain="R-RcmdrPlugin.TeachStat"))
       return()
     }

    activeDataSet <- ActiveDataSet()
    activeDataSet<-get(activeDataSet)
    vSeleccionadas<-subset(activeDataSet,select = x)

     if(pondVar==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){variablePonderacion<-NULL}
    else{variablePonderacion<-activeDataSet[,pondVar]}

    if(g==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){factorAgrupacion<-NULL}
    else{factorAgrupacion<-activeDataSet[,g]}

##################### Imprimir la funci?n a llamar por RCommander ###########################################

    .activeDataSet<-ActiveDataSet()


    if(0 == length(x)) vponderada<-"NULL"
    else{        if (length(x) == 1){vponderada<- paste('"', x, '"', sep="")
    vponderada<-paste(.activeDataSet, "[,c(", vponderada, ")]", sep="")
    }
      else{ vponderada<-paste("c(", paste('"', x, '"', collapse=", ", sep=""), ")", sep="")

      vponderada <- paste(.activeDataSet, "[,", vponderada, "]", sep="")
      }

}
    stadisticas <- paste("c(",
                         paste(c('"mean"', '"sd"', '"se(mean)"', '"IQR"', '"quantiles"', '"cv"', '"skewness"', '"kurtosis"')
                               [c(meanVar, sdVar, se.meanVar, IQRVar, quantsVar, cvVar, skewnessVar, kurtosisVar) == 1],
                               collapse=", "), ")", sep="")




    if(pondVar==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){vPonderacion<-"NULL"}
    else{vPonderacion<-paste(.activeDataSet,"$",pondVar, sep="")}



    if(g==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){grupo<-"NULL"}
    else{grupo<-paste(.activeDataSet,"$",g, sep="")}



    if(0 == length(quants)) cuantiles <-"NULL"
    else{
      cuantiles <- if (length(quants) == 1) paste(quants , sep="")
      else paste("c(", paste(quants, collapse=",", sep=""), ")", sep="")
    }


    command<- paste("W.numSummary(data=", vponderada,", statistics =", stadisticas,", quantiles = ",cuantiles,", weights=",vPonderacion,", groups=", grupo,")",sep="" )

    doItAndPrint(command)

    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="W.numSummary", reset="resumenNumericoPonderada", apply ="resumenNumericoPonderada")

  tkgrid(getFrame(xBox),labelRcmdr(dataTab, text="     "),getFrame(selectVariablePonderacion),labelRcmdr(dataTab, text="     "),getFrame(selectGroupComboBox),sticky="nw")

 tkgrid(checkBoxFrame, sticky="nw")

  tkgrid(quantilesCheckBox, quantilesEntry, sticky="w")
  tkgrid(quantilesFrame)

  dialogSuffix(use.tabs=TRUE, grid.buttons=TRUE, tabs=c("dataTab", "statisticsTab"),
               tab.names=c("Data", "Statistics"))
}




