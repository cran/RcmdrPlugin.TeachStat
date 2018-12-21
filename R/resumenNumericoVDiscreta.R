
resumenNumericoVDiscreta <- function(){
  Library("abind")
  Library("e1071")
  defaults <- list(initial.x=NULL,initial.sg2=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),
                   initial.tablafrecuencia="0", initial.cortes="0", initial.valorcortes="",
                   initial.g=NULL, initial.mean="1",
                   initial.sd="1", initial.se.mean="0", initial.IQR="1", initial.cv="0",
                   initial.quantiles.variable="1",
                   initial.quantiles="0, .25, .5, .75, 1",
                   initial.skewness="0", initial.kurtosis="0", initial.tab=0)

  dialog.values <- getDialog("resumenNumericoVDiscreta", defaults)
  initial.group <- dialog.values$initial.group
  initializeDialog(title=gettext("Numerical summaries - Discrete variables",domain="R-RcmdrPlugin.TeachStat"), use.tabs=TRUE, tabs=c("dataTab", "statisticsTab"))

   xBox <- variableListBox(dataTab, Numeric(), selectmode="multiple", title=gettext("Variables (pick one or more)",domain="R-RcmdrPlugin.TeachStat"),
                          initialSelection=varPosn(dialog.values$initial.x, "numeric"))

    if (length(Factors())!=0){
     mostrar<-"readonly"
   }else {
     mostrar<-"disabled"
   }

  selectGroupComboBox <- variableComboBox(dataTab, variableList=Factors(), state=mostrar,
                    initialSelection=dialog.values$initial.sg2, title=gettext("Group (pick one)",domain="R-RcmdrPlugin.TeachStat"))

  stkFrame<-tkframe(dataTab)
  tablafrecuenciaVar<-tclVar(dialog.values$initial.tablafrecuencia)
  tablafrecuenciaCheckBox<-ttkcheckbutton(stkFrame, variable=tablafrecuenciaVar,text=gettext("Frequency table",domain="R-RcmdrPlugin.TeachStat"),
                                          command = function(){ if(tclvalue(tablafrecuenciaVar)=="1"){
                                              tk2state.set(cortesCheckBox, state = "normal")} else
                                              { tk2state.set(cortesCheckBox, state = "disabled")
                                              tclvalue(cortesVar)<-"0"
                                              tclvalue(valorcortesVar)<-""
                                              }
                                          })

  cortesFrame<-tkframe(dataTab)

  if(dialog.values$initial.cortes=="0"){
    mostrar<-"disabled"
  }
  else{mostrar<-"normal"}

  cortesVar<-tclVar(dialog.values$initial.cortes)
  cortesCheckBox<-ttkcheckbutton(cortesFrame, variable=cortesVar,text=gettext("Breaks",domain="R-RcmdrPlugin.TeachStat"),state=mostrar,
                                 command = function(){ if(tclvalue(cortesVar)=="1"){
                                   tk2state.set(cortesEntry, state = "normal")} else
                                   { tk2state.set(cortesEntry, state = "disabled")
                                     tclvalue(valorcortesVar)<-""}
                                 })


  if(dialog.values$initial.valorcortes==""){
    mostrar<-"disabled"
  }
  else{mostrar<-"normal"}

   valorcortesVar <- tclVar(dialog.values$initial.valorcortes)
   cortesEntry <- ttkentry(cortesFrame, width="12", textvariable=valorcortesVar, state=mostrar)


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
    g<- getSelection(selectGroupComboBox)
    tfrec<-tclvalue(tablafrecuenciaVar)

    ### Cortes
    cortes<-tclvalue(cortesVar)
    valorcortes<-tclvalue(valorcortesVar)

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
    
    

    putDialog("resumenNumericoVDiscreta", list(
      initial.x=x,initial.sg2=g,initial.tablafrecuencia="0",initial.cortes="0",initial.valorcortes=valorcortes, initial.mean=meanVar, initial.sd=sdVar, initial.se.mean=se.meanVar, initial.IQR=IQRVar, initial.cv=cvVar,
      initial.quantiles.variable=quantsVar, initial.quantiles=quants,
      initial.skewness=skewnessVar, initial.kurtosis=kurtosisVar, initial.tab=tab
    ))

    closeDialog()

    if (length(x) == 0){
      errorCondition(recall=resumenNumericoVDiscreta, message=gettext("No variable selected",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    
    valorcortes <- paste(gsub(",+", ",", gsub(" ", ",", valorcortes)), sep="")

    valorcortes<-as.numeric( strsplit(valorcortes,split=",")[[1]])

    if((length(valorcortes)==0 )&&(cortes==1)){
      errorCondition(recall=resumenNumericoVDiscreta, message=gettext("Breaks must be an integer > 1 or a numeric vector of length > 1",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }


    if(NA %in% valorcortes){
    errorCondition(recall=resumenNumericoVDiscreta, message=gettext("Breaks must be an integer > 1 or a numeric vector of length > 1",domain="R-RcmdrPlugin.TeachStat"))
     return()
     }

    if(length(valorcortes)==1){
      if (((valorcortes%%1)!=0)|(valorcortes<=1)){
        errorCondition(recall=resumenNumericoVDiscreta, message=gettext("Breaks must be an integer > 1 or a numeric vector of length > 1",domain="R-RcmdrPlugin.TeachStat"))
        return()
    }}

    quants <- paste(gsub(",+", ",", gsub(" ", ",", quants)), sep="")

    quants <- as.numeric( strsplit(quants,split=",")[[1]])

    if(((NA %in% quants)||(length( quants[(quants<0)|(quants>1)])!=0) || length(quants)<=1)&&(quantsVar==1)){
      errorCondition(recall=resumenNumericoVDiscreta, message=gettext("Quantiles must be a numeric vector in [0,1]",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }

    if((length(quants)==0 )&&(quantsVar==1)){
      errorCondition(recall=resumenNumericoVDiscreta, message=gettext("Quantiles must be a numeric vector in [0,1]",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }



    posiblesstatistic<-c("mean", "sd", "se(mean)", "IQR", "quantiles", "cv", "skewness", "kurtosis")
    statselegidas<-c(meanVar, sdVar, se.meanVar, IQRVar, quantsVar, cvVar, skewnessVar, kurtosisVar)

    #print(posiblesstatistic)
    #print(statselegidas)

     stats <- posiblesstatistic[as.logical(as.numeric(statselegidas))]

    if (length(stats) == 0){
      errorCondition(recall=resumenNumericoVDiscreta, message=gettext("No statistics selected",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }

    activeDataSet <- ActiveDataSet()
    activeDataSet<-get(activeDataSet)
    vDiscretasSeleccionadas<-subset(activeDataSet,select = x)
    if(g==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){factorSeleccionado<-NULL}
    else{factorSeleccionado<-activeDataSet[,g]}

    print_tabla_Frecuencia<-as.logical(as.numeric(tfrec))


  ###################### Imprimir la función a llamar por RCommander ###########################################

    .activeDataSet<-ActiveDataSet()

    if(0 == length(x)) vdiscreta<-"NULL"
    else{        if (length(x) == 1){vdiscreta<- paste('"', x, '"', sep="")
    vdiscreta<-paste(.activeDataSet, "[", vdiscreta, "]", sep="")
    }
      else{ vdiscreta<-paste("c(", paste('"', x, '"', collapse=", ", sep=""), ")", sep="")

      vdiscreta <- paste(.activeDataSet, "[,", vdiscreta, "]", sep="")
      }
}
#### Si quisieramos pasarle solo un vector(factor)

#   if(0 == length(x)) vdiscreta <-"NULL"
#    else{
#      vdiscreta <- if (length(x) == 1) paste('"', x, '"', sep="")
#     else paste("c(", paste('"', x, '"', collapse=", ", sep=""), ")", sep="")
#      vdiscreta <- paste(.activeDataSet, "[,", vdiscreta, "]", sep="")
#    }



    stadisticas <- paste("c(",
                   paste(c('"mean"', '"sd"', '"se(mean)"', '"IQR"', '"quantiles"', '"cv"', '"skewness"', '"kurtosis"')
                         [c(meanVar, sdVar, se.meanVar, IQRVar, quantsVar, cvVar, skewnessVar, kurtosisVar) == 1],
                         collapse=", "), ")", sep="")




    if(g==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){grupo<-"NULL"}
    else{grupo<-paste(.activeDataSet,"$",g, sep="")}



    if(0 == length(quants)) cuantiles <-"NULL"
    else{
      cuantiles <- if (length(quants) == 1) paste(quants , sep="")
      else paste("c(", paste(quants, collapse=",", sep=""), ")", sep="")
    }

    # if(0 == length(valorcortes)) vcortes <-"NULL"
    if(0 == cortes) vcortes <-"NULL"
    else{
      vcortes <- if (length(valorcortes) == 1) paste(valorcortes , sep="")
      else paste("c(", paste(valorcortes, collapse=",", sep=""), ")", sep="")
    }

 command<- paste("calcularResumenVariablesDiscretas(data=", vdiscreta,", statistics =", stadisticas,", quantiles = ",cuantiles,", groups=", grupo,", tablaFrecuencia=",print_tabla_Frecuencia,", cortes=",vcortes,")",sep="" )

 doItAndPrint(command)

      ###Funcion a llamar
   ## calcularResumenVariablesDiscretas(data=vDiscretasSeleccionadas, statistics = stats, quantiles = quants, groups=factorSeleccionado,tablaFrecuencia=print_tabla_Frecuencia)





    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="calcularResumenVariablesDiscretas", reset="resumenNumericoVDiscreta", apply ="resumenNumericoVDiscreta")
  #if (length(Factors())!=0){
  tkgrid(getFrame(xBox),labelRcmdr(dataTab, text="     "),getFrame(selectGroupComboBox),sticky="nw")
  tkgrid(labelRcmdr(stkFrame,text="  "),sticky="nw")
  tkgrid(tablafrecuenciaCheckBox, sticky="w")
  tkgrid(stkFrame, sticky="nw")
  tkgrid(cortesCheckBox,labelRcmdr(cortesFrame,text=" "),cortesEntry,sticky="nw")
  tkgrid(cortesFrame,sticky="nw")


 #}else tkgrid(getFrame(xBox),labelRcmdr(dataTab, text="     "),getFrame(gBox),sticky="nw")
  #tkgrid(getFrame(xBox),sticky="nw")
  #tkgrid(getFrame(selectGroupComboBox), sticky="nw")
 # tkgrid(groupFrame,sticky="e")
# tkgrid(getFrame(gBox),sticky="e")
 tkgrid(checkBoxFrame, sticky="nw")

  tkgrid(quantilesCheckBox, quantilesEntry, sticky="w")
  tkgrid(quantilesFrame)
  #tkgrid(groupsFrame, sticky = "w", padx=6)
  dialogSuffix(use.tabs=TRUE, grid.buttons=TRUE, tabs=c("dataTab", "statisticsTab"),
               tab.names=c("Data", "Statistics"))
}

