
resumenNumericoVDiscreta <- function(){
  Library("abind")
  Library("e1071")
  
  dialogName <- "resumenNumericoVDiscreta"
  defaults <- list(initial.x=NULL,initial.sg2=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),
                   initial.tablafrecuencia="0", initial.cortes="0", initial.valorcortes="",
                   initial.cortes.n="", initial.cortes.v="", initial.cortes.al="", initial.cortes.from="", initial.cortes.to="", initial.cortes.by="",
                   #initial.g=NULL, 
                   initial.mean="1",
                   initial.sd="1", initial.se.mean="0", initial.IQR="1", initial.cv="0",
                   initial.quantiles.variable="1",
                   initial.quantiles="0, .25, .5, .75, 1",
                   initial.skewness="0", initial.kurtosis="0", initial.tab=0)
  
  dialog.values <- getDialog(dialogName, defaults)
  #initial.group <- dialog.values$initial.group
  initializeDialog(title=gettext("Numerical summaries - Discrete variables",domain="R-RcmdrPlugin.TeachStat"), use.tabs=TRUE, tabs=c("dataTab", "statisticsTab"))
  
  varFrame<-tkframe(dataTab)
  
  xBox <- variableListBox(varFrame, Numeric(), selectmode="multiple", title=gettext("Variables (pick one or more)",domain="R-RcmdrPlugin.TeachStat"),
                          initialSelection=varPosn(dialog.values$initial.x, "numeric"))
  
  if (length(Factors())!=0){
    mostrar<-"readonly"
  }else {
    mostrar<-"disabled"
  }
  
  selectGroupComboBox <- variableComboBox(varFrame, variableList=Factors(), state=mostrar,
                                          initialSelection=dialog.values$initial.sg2, title=gettext("Group (pick one)",domain="R-RcmdrPlugin.TeachStat"))
  
  stkFrame<-tkframe(dataTab)
  tablafrecuenciaVar<-tclVar(dialog.values$initial.tablafrecuencia)
  tablafrecuenciaCheckBox<-ttkcheckbutton(stkFrame, variable=tablafrecuenciaVar,text=gettext("Frequency table",domain="R-RcmdrPlugin.TeachStat"),
                                          command = function(){ if(tclvalue(tablafrecuenciaVar)=="1"){
                                            tk2state.set(cortesCheckBox, state = "normal")} else
                                            { tk2state.set(cortesCheckBox, state = "disabled")
                                              tk2state.set(ncortesRadioButton, state = "disabled")
                                              tk2state.set(ncortesEntry, state = "disabled")
                                              tk2state.set(vcortesRadioButton, state = "disabled")
                                              tk2state.set(vcortesEntry, state = "disabled")
                                              tk2state.set(alcortesRadioButton, state = "disabled")
                                              tk2state.set(alcortesEntry, state = "disabled")
                                              tk2state.set(seqcortesRadioButton, state = "disabled")
                                              tk2state.set(fromcortesEntry, state = "disabled")
                                              tk2state.set(tocortesEntry, state = "disabled")
                                              tk2state.set(tocortesLabel, state = "disabled")
                                              tk2state.set(bycortesEntry, state = "disabled")
                                              tk2state.set(bycortesLabel, state = "disabled")
                                              tclvalue(cortesVar)<-"0"
                                              tclvalue(valorcortesVar)<-""
                                              tclvalue(nvalorcortesVar)<-""
                                              tclvalue(vvalorcortesVar)<-""
                                              tclvalue(alvalorcortesVar)<-""
                                              tclvalue(fromvalorcortesVar)<-""
                                              tclvalue(tovalorcortesVar)<-""
                                              tclvalue(byvalorcortesVar)<-""
                                            }
                                          })
  
  cortesFrame<-tkframe(dataTab)
  
  if(dialog.values$initial.tablafrecuencia=="0"){
    mostrar_cortes<-"disabled"
  }
  else{mostrar_cortes<-"normal"}
  
  cortesVar<-tclVar(dialog.values$initial.cortes)
  cortesCheckBox<-ttkcheckbutton(cortesFrame, variable=cortesVar,text=gettext("Breaks",domain="R-RcmdrPlugin.TeachStat"),state=mostrar_cortes,
                                 command = function(){ if(tclvalue(cortesVar)=="1"){
                                   tk2state.set(ncortesRadioButton, state = "normal")
                                   tk2state.set(vcortesRadioButton, state = "normal")
                                   tk2state.set(alcortesRadioButton, state = "normal")
                                   tk2state.set(seqcortesRadioButton, state = "normal")
                                   tk2state.set(tocortesLabel, state = "normal")
                                   tk2state.set(bycortesLabel, state = "normal")
                                 } else
                                 { tk2state.set(ncortesRadioButton, state = "disabled")
                                   tk2state.set(ncortesEntry, state = "disabled")
                                   tk2state.set(vcortesRadioButton, state = "disabled")
                                   tk2state.set(vcortesEntry, state = "disabled")
                                   tk2state.set(alcortesRadioButton, state = "disabled")
                                   tk2state.set(alcortesEntry, state = "disabled")
                                   tk2state.set(seqcortesRadioButton, state = "disabled")
                                   tk2state.set(fromcortesEntry, state = "disabled")
                                   tk2state.set(tocortesEntry, state = "disabled")
                                   tk2state.set(tocortesLabel, state = "disabled")
                                   tk2state.set(bycortesEntry, state = "disabled")
                                   tk2state.set(bycortesLabel, state = "disabled")
                                   tclvalue(valorcortesVar)<-""
                                   tclvalue(nvalorcortesVar)<-""
                                   tclvalue(vvalorcortesVar)<-""
                                   tclvalue(alvalorcortesVar)<-""
                                   tclvalue(fromvalorcortesVar)<-""
                                   tclvalue(tovalorcortesVar)<-""
                                   tclvalue(byvalorcortesVar)<-""}
                                 })
  
  
  optcortesFrame<-tkframe(cortesFrame)
  
  valorcortesVar<-tclVar(dialog.values$initial.valorcortes)
  
  if(dialog.values$initial.cortes=="0"){
    mostrar_opcortes<-"disabled"
  }
  else{mostrar_opcortes<-"normal"}
  
  # number of breaks
  ncortesFrame<-tkframe(optcortesFrame)
  ncortesRadioButton <- ttkradiobutton(ncortesFrame, variable=valorcortesVar, text=gettext("Breaks number:",domain="R-RcmdrPlugin.TeachStat"), 
                                       value="ncortes",state=mostrar_opcortes,
                                       command=function(){ if(tclvalue(valorcortesVar)=="ncortes"){
                                         tk2state.set(ncortesEntry, state = "normal")
                                         tk2state.set(vcortesEntry, state = "disabled")
                                         tk2state.set(alcortesEntry, state = "disabled")
                                         tk2state.set(fromcortesEntry, state = "disabled")
                                         tk2state.set(tocortesEntry, state = "disabled")
                                         #tk2state.set(tocortesLabel, state = "disabled")
                                         tk2state.set(bycortesEntry, state = "disabled")
                                         #tk2state.set(bycortesLabel, state = "disabled")
                                         tclvalue(vvalorcortesVar)<-""
                                         tclvalue(alvalorcortesVar)<-""
                                         tclvalue(fromvalorcortesVar)<-""
                                         tclvalue(tovalorcortesVar)<-""
                                         tclvalue(byvalorcortesVar)<-""
                                       } else
                                       { tk2state.set(ncortesEntry, state = "disabled")
                                       }
                                       })
  
  
  if(dialog.values$initial.valorcortes=="ncortes"){
    mostrar<-"normal"
  }
  else{mostrar<-"disabled"}
  
  nvalorcortesVar <- tclVar(dialog.values$initial.cortes.n)
  ncortesEntry <- ttkentry(ncortesFrame, width="12", textvariable=nvalorcortesVar, state=mostrar)
  
  
  # Vector of breaks
  vcortesFrame<-tkframe(optcortesFrame)
  vcortesRadioButton <- ttkradiobutton(vcortesFrame, variable=valorcortesVar, text=gettext("Breaks vector:",domain="R-RcmdrPlugin.TeachStat"), 
                                       value="vcortes",state=mostrar_opcortes,
                                       command=function(){ if(tclvalue(valorcortesVar)=="vcortes"){
                                         tk2state.set(vcortesEntry, state = "normal")
                                         tk2state.set(ncortesEntry, state = "disabled")
                                         tk2state.set(alcortesEntry, state = "disabled")
                                         tk2state.set(fromcortesEntry, state = "disabled")
                                         tk2state.set(tocortesEntry, state = "disabled")
                                         #tk2state.set(tocortesLabel, state = "disabled")
                                         tk2state.set(bycortesEntry, state = "disabled")
                                         #tk2state.set(bycortesLabel, state = "disabled")
                                         tclvalue(nvalorcortesVar)<-""
                                         tclvalue(alvalorcortesVar)<-""
                                         tclvalue(fromvalorcortesVar)<-""
                                         tclvalue(tovalorcortesVar)<-""
                                         tclvalue(byvalorcortesVar)<-""
                                       } else
                                       { tk2state.set(vcortesEntry, state = "disabled")
                                       }
                                       })
  
  
  if(dialog.values$initial.valorcortes=="vcortes"){
    mostrar<-"normal"
  }
  else{mostrar<-"disabled"}
  
  vvalorcortesVar <- tclVar(dialog.values$initial.cortes.v)
  vcortesEntry <- ttkentry(vcortesFrame, width="12", textvariable=vvalorcortesVar, state=mostrar)
  
  # algorithm of breaks
  alcortesFrame<-tkframe(optcortesFrame)
  alcortesRadioButton <- ttkradiobutton(alcortesFrame, variable=valorcortesVar, text=gettext("Breaks algorithm:",domain="R-RcmdrPlugin.TeachStat"), 
                                        value="alcortes",state=mostrar_opcortes,
                                        command=function(){ if(tclvalue(valorcortesVar)=="alcortes"){
                                          tk2state.set(vcortesEntry, state = "disabled")
                                          tk2state.set(ncortesEntry, state = "disabled")
                                          tk2state.set(alcortesEntry, state = "normal")
                                          tk2state.set(fromcortesEntry, state = "disabled")
                                          tk2state.set(tocortesEntry, state = "disabled")
                                          #tk2state.set(tocortesLabel, state = "disabled")
                                          tk2state.set(bycortesEntry, state = "disabled")
                                          #tk2state.set(bycortesLabel, state = "disabled")
                                          tclvalue(nvalorcortesVar)<-""
                                          tclvalue(vvalorcortesVar)<-""
                                          tclvalue(fromvalorcortesVar)<-""
                                          tclvalue(tovalorcortesVar)<-""
                                          tclvalue(byvalorcortesVar)<-""
                                        } else
                                        { tk2state.set(alcortesEntry, state = "disabled")
                                        }
                                        })
  
  
  if(dialog.values$initial.valorcortes=="alcortes"){
    mostrar<-"normal"
  }
  else{mostrar<-"disabled"}
  
  alvalorcortesVar <- tclVar(dialog.values$initial.cortes.al)
  alcortesEntry <- ttkentry(alcortesFrame, width="12", textvariable=alvalorcortesVar, state=mostrar)
  
  # use of seq() for breaks
  seqcortesFrame<-tkframe(optcortesFrame)
  seqcortesRadioButton <- ttkradiobutton(seqcortesFrame, variable=valorcortesVar, text=gettext("From:",domain="R-RcmdrPlugin.TeachStat"), 
                                         value="seqcortes",state=mostrar_opcortes,
                                         command=function(){ if(tclvalue(valorcortesVar)=="seqcortes"){
                                           tk2state.set(vcortesEntry, state = "disabled")
                                           tk2state.set(ncortesEntry, state = "disabled")
                                           tk2state.set(alcortesEntry, state = "disabled")
                                           tk2state.set(fromcortesEntry, state = "normal")
                                           tk2state.set(tocortesEntry, state = "normal")
                                           #tk2state.set(tocortesLabel, state = "normal")
                                           tk2state.set(bycortesEntry, state = "normal")
                                           #tk2state.set(bycortesLabel, state = "normal")
                                           tclvalue(nvalorcortesVar)<-""
                                           tclvalue(vvalorcortesVar)<-""
                                           tclvalue(alvalorcortesVar)<-""
                                         } else
                                         { tk2state.set(fromcortesEntry, state = "disabled")
                                           tk2state.set(tocortesEntry, state = "disabled")
                                           #tk2state.set(tocortesLabel, state = "disabled")
                                           tk2state.set(bycortesEntry, state = "disabled")
                                           #tk2state.set(bycortesLabel, state = "disabled")
                                           tclvalue(valorcortesVar)<-""}
                                         })
  
  
  if(dialog.values$initial.valorcortes=="seqcortes"){
    mostrar<-"normal"
  }
  else{mostrar<-"disabled"}
  
  fromvalorcortesVar <- tclVar(dialog.values$initial.cortes.from)
  fromcortesEntry <- ttkentry(seqcortesFrame, width="12", textvariable=fromvalorcortesVar, state=mostrar)
  tovalorcortesVar <- tclVar(dialog.values$initial.cortes.to)
  tocortesLabel<-labelRcmdr(seqcortesFrame,text=gettext("to:",domain="R-RcmdrPlugin.TeachStat"),state=mostrar_opcortes)
  tocortesEntry <- ttkentry(seqcortesFrame, width="12", textvariable=tovalorcortesVar, state=mostrar)
  byvalorcortesVar <- tclVar(dialog.values$initial.cortes.by)
  bycortesEntry <- ttkentry(seqcortesFrame, width="12", textvariable=byvalorcortesVar, state=mostrar)
  bycortesLabel<-labelRcmdr(seqcortesFrame,text=gettext("by:",domain="R-RcmdrPlugin.TeachStat"),state=mostrar_opcortes)
  
  
  
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
    nvalorcortes<-tclvalue(nvalorcortesVar)
    vvalorcortes<-tclvalue(vvalorcortesVar)
    alvalorcortes<-tclvalue(alvalorcortesVar)
    fromvalorcortes<-tclvalue(fromvalorcortesVar)
    tovalorcortes<-tclvalue(tovalorcortesVar)
    byvalorcortes<-tclvalue(byvalorcortesVar)
    
    
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
    
    
    
    putDialog(dialogName, list(
      initial.x=x,initial.sg2=g,initial.tablafrecuencia=tfrec,initial.cortes=cortes,initial.valorcortes=valorcortes,
      initial.cortes.n=nvalorcortes, initial.cortes.v=vvalorcortes, initial.cortes.al=alvalorcortes, initial.cortes.from=fromvalorcortes,
      initial.cortes.to=tovalorcortes, initial.cortes.by=byvalorcortes,
      initial.mean=meanVar, initial.sd=sdVar, initial.se.mean=se.meanVar, initial.IQR=IQRVar, initial.cv=cvVar,
      initial.quantiles.variable=quantsVar, initial.quantiles=quants,
      initial.skewness=skewnessVar, initial.kurtosis=kurtosisVar, initial.tab=tab
    ))
    
    closeDialog()
    
    dialogNameF <- get(dialogName,mode="function")
    
    if (length(x) == 0){
      errorCondition(recall=dialogNameF, message=gettext("No variable selected",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    
    nvalorcortes <- as.numeric(nvalorcortes)
    vvalorcortes <- paste(gsub(",+", ",", gsub(" ", ",", vvalorcortes)), sep="")
    vvalorcortes<-as.numeric( strsplit(vvalorcortes,split=",")[[1]])
    fromvalorcortes <- as.numeric(fromvalorcortes)
    tovalorcortes <- as.numeric(tovalorcortes)
    byvalorcortes <- as.numeric(byvalorcortes)
    
    if(cortes==1){
      if(valorcortes=="ncortes"){
        if(length(nvalorcortes)==0){
          errorCondition(recall=dialogNameF, message=gettext("Breaks number must be provided",domain="R-RcmdrPlugin.TeachStat"))
          return()
        }
        
        if(NA %in% nvalorcortes | length(nvalorcortes)>1 | (nvalorcortes%%1)!=0 | (nvalorcortes<=1)){
          errorCondition(recall=dialogNameF, message=gettext("Breaks number must be an integer > 1",domain="R-RcmdrPlugin.TeachStat"))
          return()
        } 
        
        # if (((nvalorcortes%%1)!=0)|(nvalorcortes<=1)){
        #   errorCondition(recall=dialogNameF, message=gettext("Breaks must be an integer > 1",domain="R-RcmdrPlugin.TeachStat"))
        #   return()
        # }
      } else if(valorcortes=="vcortes"){
        if(length(vvalorcortes)==0){
          errorCondition(recall=dialogNameF, message=gettext("Breaks vector must be provided",domain="R-RcmdrPlugin.TeachStat"))
          return()
        }
        
        if(NA %in% vvalorcortes | length(vvalorcortes)<=1){
          errorCondition(recall=dialogNameF, message=gettext("Breaks vector must be a numeric vector of length > 1",domain="R-RcmdrPlugin.TeachStat"))
          return()
        }
        
      } else if(valorcortes=="alcortes"){
        if(length(alvalorcortes)>1 | !(tolower(alvalorcortes)%in%c("sturges","fd", "freedman-diaconis", "scott"))){
          errorCondition(recall=dialogNameF, message=gettext("Breaks algorithm must be a name of a known 'breaks' algorithm",domain="R-RcmdrPlugin.TeachStat"))
          return()
        }
      } else if(valorcortes=="seqcortes"){
        if(length(fromvalorcortes)==0 & length(tovalorcortes)==0 & length(tovalorcortes)==0){
          errorCondition(recall=dialogNameF, message=gettext("'From', 'to' and 'by' must be provided",domain="R-RcmdrPlugin.TeachStat"))
          return()
        }
        
        if(NA %in% fromvalorcortes | length(fromvalorcortes)>1 | 
           NA %in% tovalorcortes | length(tovalorcortes)>1 | 
           NA %in% byvalorcortes | length(byvalorcortes)>1){
          errorCondition(recall=dialogNameF, message=gettext("'From', 'to' and 'by' must be numeric values",domain="R-RcmdrPlugin.TeachStat"))
          return()
        } 
        
      } else{
        errorCondition(recall=dialogNameF, message=gettext("A 'breaks' option must be selected",domain="R-RcmdrPlugin.TeachStat"))
        return()
      }
    }
    
    
    quants <- paste(gsub(",+", ",", gsub(" ", ",", quants)), sep="")
    
    quants <- as.numeric( strsplit(quants,split=",")[[1]])
    
    if(((NA %in% quants)||(length( quants[(quants<0)|(quants>1)])!=0) || length(quants)<1)&&(quantsVar==1)){
      errorCondition(recall=dialogNameF, message=gettext("Quantiles must be a numeric vector in [0,1]",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    
    if((length(quants)==0 )&&(quantsVar==1)){
      errorCondition(recall=dialogNameF, message=gettext("Quantiles must be a numeric vector in [0,1]",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    
    
    
    posiblesstatistic<-c("mean", "sd", "se(mean)", "IQR", "quantiles", "cv", "skewness", "kurtosis")
    statselegidas<-c(meanVar, sdVar, se.meanVar, IQRVar, quantsVar, cvVar, skewnessVar, kurtosisVar)
    
    #print(posiblesstatistic)
    #print(statselegidas)
    
    stats <- posiblesstatistic[as.logical(as.numeric(statselegidas))]
    
    if (length(stats) == 0){
      errorCondition(recall=dialogNameF, message=gettext("No statistics selected",domain="R-RcmdrPlugin.TeachStat"))
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
      vcortes <- if(valorcortes=="ncortes"){
        paste(nvalorcortes , sep="")
      } else if(valorcortes=="vcortes"){
        paste("c(", paste(vvalorcortes, collapse=",", sep=""), ")", sep="")
      } else if(valorcortes=="alcortes"){
        paste('"',alvalorcortes,'"', sep="")
      } else{
        paste("seq(from = ", fromvalorcortes,", to = ",tovalorcortes,", by = ",byvalorcortes, ")", sep="")
      }
    }
    
    command<- paste("calcularResumenVariablesDiscretas(data=", vdiscreta,", statistics =", stadisticas,", quantiles = ",cuantiles,", groups=", grupo,", tablaFrecuencia=",print_tabla_Frecuencia,", cortes=",vcortes,")",sep="" )
    
    doItAndPrint(command)
    
    ###Funcion a llamar
    ## calcularResumenVariablesDiscretas(data=vDiscretasSeleccionadas, statistics = stats, quantiles = quants, groups=factorSeleccionado,tablaFrecuencia=print_tabla_Frecuencia)
    
    
    
    
    
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="calcularResumenVariablesDiscretas", reset=dialogName, apply =dialogName)
  #if (length(Factors())!=0){
  tkgrid(getFrame(xBox),labelRcmdr(varFrame, text="     "),getFrame(selectGroupComboBox),sticky="nw")
  tkgrid(varFrame, sticky="nw")
  tkgrid(labelRcmdr(stkFrame,text="  "),sticky="nw")
  tkgrid(tablafrecuenciaCheckBox, sticky="w")
  tkgrid(stkFrame, sticky="nw")
  tkgrid(ncortesRadioButton,labelRcmdr(ncortesFrame,text=" "),ncortesEntry,sticky="nw")
  tkgrid(ncortesFrame,sticky="nw")
  tkgrid(vcortesRadioButton,labelRcmdr(vcortesFrame,text=" "),vcortesEntry,sticky="nw")
  tkgrid(vcortesFrame,sticky="nw")
  tkgrid(alcortesRadioButton,labelRcmdr(alcortesFrame,text=" "),alcortesEntry,sticky="nw")
  tkgrid(alcortesFrame,sticky="nw")
  tkgrid(seqcortesRadioButton,labelRcmdr(seqcortesFrame,text=" "),fromcortesEntry,labelRcmdr(seqcortesFrame,text=" "),
         tocortesLabel,labelRcmdr(seqcortesFrame,text=" "),tocortesEntry,
         labelRcmdr(seqcortesFrame,text=" "),bycortesLabel,labelRcmdr(seqcortesFrame,text=" "),bycortesEntry,sticky="nw")
  tkgrid(seqcortesFrame,sticky="nw")
  tkgrid(cortesCheckBox,labelRcmdr(cortesFrame,text=" "),optcortesFrame,sticky="nw")
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