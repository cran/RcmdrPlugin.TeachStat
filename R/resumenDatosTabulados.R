resumenDatosTabulados <- function(){
  Library("abind")
  Library("e1071")
  defaults <- list(initial.li=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),initial.ls=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),
                   initial.fr=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"), initial.tablafrecuencia="0",
                   initial.mean="1",
                   initial.sd="1", initial.se.mean="0", initial.IQR="1",initial.mode="0", initial.cv="0",
                   initial.quantiles.variable="1",
                   initial.quantiles="0, .25, .5, .75, 1",
                   initial.skewness="0", initial.kurtosis="0", initial.tab=0)

  activeDataSet <- ActiveDataSet()
  dialog.values <- getDialog("resumenDatosTabulados", defaults)
  initial.group <- dialog.values$initial.group
  initializeDialog(title=gettext("Numerical summaries - Tabulated data",domain="R-RcmdrPlugin.TeachStat"), use.tabs=TRUE, tabs=c("dataTab", "statisticsTab"))

 limiteInferiorBox <- variableComboBox(dataTab, variableList=Numeric(),
                            initialSelection=dialog.values$initial.li, title=gettext("Lower bound",domain="R-RcmdrPlugin.TeachStat"))


 limiteSuperiorBox<- variableComboBox(dataTab, variableList=Numeric(),
                                                initialSelection=dialog.values$initial.ls, title=gettext("Upper bound",domain="R-RcmdrPlugin.TeachStat"))

 frecuenciaBox <- variableComboBox(dataTab, variableList=Numeric(),
                                          initialSelection=dialog.values$initial.fr, title=gettext("Frequency",domain="R-RcmdrPlugin.TeachStat"))

 stkFrame<-tkframe(dataTab)
 tablafrecuenciaVar<-tclVar(dialog.values$initial.tablafrecuencia)
 tablafrecuenciaCheckBox<-ttkcheckbutton(stkFrame, variable=tablafrecuenciaVar,text=gettext("Frequency table",domain="R-RcmdrPlugin.TeachStat"))


  checkBoxes(window = statisticsTab, frame="checkBoxFrame", boxes=c("mean", "sd", "se.mean", "IQR","mode", "cv","skewness", "kurtosis"),
             initialValues=c(dialog.values$initial.mean, dialog.values$initial.sd, dialog.values$initial.se.mean, dialog.values$initial.IQR,dialog.values$initial.mode, dialog.values$initial.cv,dialog.values$initial.skewness, dialog.values$initial.kurtosis),
             labels=gettext(c("Mean", "Standard Deviation", "Standard Error of Mean", "Interquartile Range","Mode", "Coefficient of Variation","Skewness", "Kurtosis"),domain="R-RcmdrPlugin.TeachStat"))

  quantilesVariable <- tclVar(dialog.values$initial.quantiles.variable)
  quantilesFrame <- tkframe(statisticsTab)
  quantilesCheckBox <- tkcheckbutton(quantilesFrame, variable=quantilesVariable,
                                     text=gettext("Quantiles:",domain="R-RcmdrPlugin.TeachStat"))
  quantiles <- tclVar(dialog.values$initial.quantiles)
  quantilesEntry <- ttkentry(quantilesFrame, width="20", textvariable=quantiles)


  onOK <- function(){
    tab <- if (as.character(tkselect(notebook)) == dataTab$ID) 0 else 1
    linfvar<- getSelection(limiteInferiorBox)
    lsupvar<-getSelection(limiteSuperiorBox)
    frevar<- getSelection(frecuenciaBox )
    tfrec<-tclvalue(tablafrecuenciaVar)

    quants <- tclvalue(quantiles)
    meanVar <- tclvalue(meanVariable)
    sdVar <- tclvalue(sdVariable)
    se.meanVar <- tclvalue(se.meanVariable)
    IQRVar <- tclvalue(IQRVariable)
    modeVar <- tclvalue(modeVariable)
    cvVar <- tclvalue(cvVariable)
    quantsVar <- tclvalue(quantilesVariable)
    skewnessVar <- tclvalue(skewnessVariable)
    kurtosisVar <- tclvalue(kurtosisVariable)

    putDialog("resumenDatosTabulados", list(
      initial.li=linfvar,initial.ls=lsupvar,initial.fr=frevar,initial.tablafrecuencia=tfrec, initial.mean=meanVar, initial.sd=sdVar, initial.se.mean=se.meanVar, initial.IQR=IQRVar,initial.mode=modeVar, initial.cv=cvVar,
      initial.quantiles.variable=quantsVar, initial.quantiles=quants,
      initial.skewness=skewnessVar, initial.kurtosis=kurtosisVar, initial.tab=tab
    ))


    activeDataSet <- ActiveDataSet()
    activeDataSet<-get(activeDataSet)

     if(linfvar==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){errorCondition(recall=resumenDatosTabulados, message=gettext("No variable selected for lower bound",domain="R-RcmdrPlugin.TeachStat"))
      return()}
    else{variableLimiteInferior<-activeDataSet[,linfvar]}

    if(lsupvar==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){errorCondition(recall=resumenDatosTabulados, message=gettext("No variable selected for upper bound",domain="R-RcmdrPlugin.TeachStat"))
      return()}
    else{variableLimiteSuperior<-activeDataSet[,lsupvar]}

    if(frevar==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){errorCondition(recall=resumenDatosTabulados, message=gettext("No variable selected for frequency",domain="R-RcmdrPlugin.TeachStat"))
      return()}
    else{variableFrecuencia<-activeDataSet[,frevar]}

    ##Distintas verificacaciones de Limite Inferior y Limite Superior - para comprobar que son variables de Datos Tabulados

    ##Comprobamos que limite inferior y limite superior est?n ordenados

    if(prod(variableLimiteInferior==sort(variableLimiteInferior))!=TRUE){errorCondition(recall=resumenDatosTabulados,
     message=gettext("Lower bound must be an ordered vector",domain="R-RcmdrPlugin.TeachStat"))
    return()}

    if(prod(variableLimiteSuperior==sort(variableLimiteSuperior))!=TRUE){errorCondition(recall=resumenDatosTabulados,
    message=gettext("Upper bound must be an ordered vector",domain="R-RcmdrPlugin.TeachStat"))
      return()}

    if(prod(variableLimiteInferior<=variableLimiteSuperior)!=TRUE){errorCondition(recall=resumenDatosTabulados,
      message=gettext("Lower bound must be smaller than upper bound",domain="R-RcmdrPlugin.TeachStat"))
      return()}
    if(prod(variableLimiteSuperior[-length(variableLimiteSuperior)]<=variableLimiteInferior[-1])!=TRUE){ errorCondition(recall=resumenDatosTabulados,
    message=gettext("Lower and upper bounds must hold Lb[i]>=Up[i-1]",domain="R-RcmdrPlugin.TeachStat"))
    return()}

    closeDialog()
    
    quants <- paste(gsub(",+", ",", gsub(" ", ",", quants)), sep="")
    quants <- as.numeric( strsplit(quants,split=",")[[1]])

    posiblesstatistic<-c("mean", "sd", "se(mean)", "IQR","mode", "quantiles", "cv", "skewness", "kurtosis")
    statselegidas<-c(meanVar, sdVar, se.meanVar, IQRVar, modeVar, quantsVar, cvVar, skewnessVar, kurtosisVar)

    stats <- posiblesstatistic[as.logical(as.numeric(statselegidas))]

    if (length(stats) == 0){
      errorCondition(recall=resumenDatosTabulados, message=gettext("No statistics selected",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }

    if(((NA %in% quants)||(length( quants[(quants<0)|(quants>1)])!=0)|| length(quants)<1)&&(quantsVar==1)){
      errorCondition(recall=resumenDatosTabulados, message=gettext("Quantiles must be a numeric vector in [0,1]",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }

    if((length(quants)==0)&&(quantsVar==1)){
      errorCondition(recall=resumenDatosTabulados, message=gettext("Quantiles must be a numeric vector in [0,1]",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }




##################### Imprimir la función a llamar por RCommander ###########################################

    .activeDataSet<-ActiveDataSet()

    if(linfvar==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){vLinf<-"NULL"}
    else{vLinf<-paste(.activeDataSet,"$",linfvar, sep="")}

    if(lsupvar==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){vLsup<-"NULL"}
    else{vLsup<-paste(.activeDataSet,"$",lsupvar, sep="")}

    if(frevar==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){vFrec<-"NULL"}
    else{vFrec<-paste(.activeDataSet,"$",frevar, sep="")}

    stadisticas <- paste("c(",
                         paste(c('"mean"', '"sd"', '"se(mean)"', '"IQR"','"mode"', '"quantiles"', '"cv"', '"skewness"', '"kurtosis"')
                               [c(meanVar, sdVar, se.meanVar, IQRVar,modeVar, quantsVar, cvVar, skewnessVar, kurtosisVar) == 1],
                               collapse=", "), ")", sep="")



    if(0 == length(quants)) cuantiles <-"NULL"
    else{
      cuantiles <- if (length(quants) == 1) paste(quants , sep="")
      else paste("c(", paste(quants, collapse=",", sep=""), ")", sep="")
    }

    print_tabla_Frecuencia<-as.logical(as.numeric(tfrec))

    command<- paste("calcularResumenDatosTabulados(l_inf=", vLinf,", l_sup=",vLsup,", ni=",vFrec ,", statistics =", stadisticas,", quantiles = ",cuantiles,", tablaFrecuencia=",print_tabla_Frecuencia,")",sep="" )
    #print(command)
    doItAndPrint(command)

    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="calcularResumenDatosTabulados", reset="resumenDatosTabulados", apply ="resumenDatosTabulados")


 tkgrid(getFrame(limiteInferiorBox),sticky="nw")
  tkgrid(getFrame(limiteSuperiorBox),sticky="nw")
  tkgrid(getFrame(frecuenciaBox),sticky="nw")
  tkgrid(labelRcmdr(dataTab, text="     "),sticky="nw")
  tkgrid(tablafrecuenciaCheckBox, sticky="w")
  tkgrid(stkFrame, sticky="nw")


  tkgrid(checkBoxFrame, sticky="nw")
  tkgrid(quantilesCheckBox, quantilesEntry, sticky="w")
  tkgrid(quantilesFrame)

  dialogSuffix(use.tabs=TRUE, grid.buttons=TRUE, tabs=c("dataTab", "statisticsTab"),
               tab.names=c("Data", "Statistics"))
}


