intervaloConfianzaVarianza <- function () {
  #invisible(library(tcltk2))
  
  dialogName <- "intervaloConfianzaVarianza"
  
  defaults <- list (initial.var=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),initial.media="0",
                    initial.valormediaconocida="",initial.nconf="0.95",
                    initial.alternative = "two.sided")
  dialog.values <- getDialog (dialogName, defaults)
  initializeDialog(title = gettext("Confidence Interval for the variance",domain="R-RcmdrPlugin.TeachStat"))
  
  comboBoxFrame<-tkframe(top)
  
  selectVariableICVarianza <- variableComboBox(comboBoxFrame, variableList=Numeric(),
                                               initialSelection=dialog.values$initial.var, title=gettext("Variable (pick one)",domain="R-RcmdrPlugin.TeachStat"))
  
  
  radioButtonsFrame<-tkframe(comboBoxFrame)
  Etiqueta<-labelRcmdr(radioButtonsFrame, text=gettext("Mean",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color"))
  radioButtons(window = radioButtonsFrame , name="media", buttons=c("C", "D"), values=c("1", "0"),
               initialValue=dialog.values$initial.media,
               labels=gettext(c("Known", "Unknown"),domain="R-RcmdrPlugin.TeachStat"),
               
               command = function(){ if(tclvalue(mediaVariable)=="1"){
                 tk2state.set(mediaEntry, state = "normal")} else
                 { tk2state.set(mediaEntry, state = "disabled")}
                 
               }
  )
  
  if(dialog.values$initial.media=="1"){
    mostrar<-"normal"
  } else{
    mostrar<-"disabled"
  }
  
  mediaconocida <- tclVar(dialog.values$initial.valormediaconocida)
  mediaEntry <- ttkentry(radioButtonsFrame, width="10", textvariable=mediaconocida, state=mostrar)
  
  ### Tipo de Intervalo
  nconftypeFrame<-tkframe(top)
  
  radioButtonstypeFrame<-tkframe(nconftypeFrame)
  
  
  radioButtons(window = radioButtonstypeFrame , name = "tipo", buttons = c("two.sided", "less","greater"),
               initialValue = dialog.values$initial.alternative,
               title = gettext("Interval type",domain="R-RcmdrPlugin.TeachStat"),
               labels = gettext(c("Two-sided", "Left-sided","Right-sided"),domain="R-RcmdrPlugin.TeachStat"),
  )
  ####
  
  nConfianzaFrame<-tkframe(nconftypeFrame)
  nConfianzaVar<-tclVar(dialog.values$initial.nconf)
  nConfianzaEntry<-ttkentry(nConfianzaFrame,width="5",textvariable=nConfianzaVar)
  
  onOK <- function(){
    dialogNameF <- get(dialogName,mode="function")
    
    varICVarianza<-getSelection(selectVariableICVarianza)
    activeDataSet <- ActiveDataSet()
    activeDataSet<-get(activeDataSet)
    
    if(varICVarianza==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){errorCondition(recall=dialogNameF, message=gettext("No variable selected",domain="R-RcmdrPlugin.TeachStat"))
      return()}
    else{activeDataSet <- ActiveDataSet()
    activeDataSet<-get(activeDataSet)
    variableICVarianza<-subset(activeDataSet,select = varICVarianza)}
    
    varConocida <- tclvalue(mediaVariable)
    if(varConocida=="1"){valormedia<-tclvalue(mediaconocida)
    if(is.na(as.numeric(valormedia))) {
      valormedia=""
      errorCondition(recall=dialogNameF, message=gettext("Known mean value must be numeric",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{valormedia<-as.numeric(valormedia)}
    }else{valormedia<-""}
    
    ### Tipo de Intervalo
    varTipo <- tclvalue(tipoVariable)
    
    
    valornConfianza<-tclvalue(nConfianzaVar)
    
    if(is.na(as.numeric(valornConfianza)) || (as.numeric(valornConfianza)<0)||(as.numeric(valornConfianza)>1)) {
      valornConfianza=0.95
      errorCondition(recall=dialogNameF, message=gettext("Confidence level must be between 0 and 1",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{valornConfianza<-as.numeric(valornConfianza)}
    
    
    putDialog (dialogName, list(initial.var=varICVarianza,initial.media=varConocida,initial.valormediaconocida=valormedia,
                                initial.nconf=valornConfianza,
                                initial.alternative=varTipo))
    closeDialog()
    
    valormedia<-as.numeric(valormedia)
    varConocida<-as.logical(as.numeric(varConocida))
    
    ###################### Imprimir la función a llamar por RCommander ###########################################
    
    .activeDataSet<-ActiveDataSet()
    
    vICVarianza<-paste(.activeDataSet,"$",varICVarianza, sep="")
    
    # command<- paste("calcular_ICVarianza(v.numerica=", vICVarianza,", media.conocida=", varConocida,", valor.media=", valormedia,", nivel.confianza=",valornConfianza,")",sep="" )
    
    # doItAndPrint(command)
    
    if(varConocida==FALSE){
      command2<- paste("aux<-VUM.test(",vICVarianza,", conf.level=",valornConfianza,
                       ", alternative='",varTipo,"')",sep="" )
      tipointervalo<-paste("\\n",gettext("Confidence Interval for the variance with unknown mean",domain="R-RcmdrPlugin.TeachStat"),"\\n", sep="")
    }
    else{
      command2<- paste("aux<-VKM.test(",vICVarianza,", mu=", valormedia,", conf.level=",valornConfianza,
                       ", alternative='",varTipo,"')",sep="")
      tipointervalo<-paste("\\n",gettext("Confidence Interval for the variance with known mean =",domain="R-RcmdrPlugin.TeachStat")," ",valormedia,"\\n", sep="")
    }
    
    linaux<-paste(rep(c("-"," "),(nchar(tipointervalo)/2)),collapse="")
    #tipointervalo<-paste(tipointervalo, linaux,sep="")
    
    ### Tipo de Intervalo
    typeinterval<-switch(varTipo,
                         two.sided=gettext("Two-sided",domain="R-RcmdrPlugin.TeachStat"),
                         less=gettext("Left-sided",domain="R-RcmdrPlugin.TeachStat"),
                         greater=gettext("Right-sided",domain="R-RcmdrPlugin.TeachStat"))
    
    
    command2<- paste("local({\n",command2,"\n",'aux2<-as.vector(aux[["conf.int"]]) \n',sep="")
    command2<- paste(command2, 'cat("',tipointervalo, '")\n',sep="" )
    command2<- paste(command2, 'cat("',linaux, '\\n")\n',sep="" )
    command2<-paste(command2,'cat("',gettext("Interval type",domain="R-RcmdrPlugin.TeachStat"),': ',typeinterval,'\\n")\n',sep="")
    command2<- paste(command2, 'cat("',gettext("Confidence level",domain="R-RcmdrPlugin.TeachStat"),': ', valornConfianza*100,'%\\n")\n',sep="" )
    command2<- paste(command2, 'cat("',gettext("Variable",domain="R-RcmdrPlugin.TeachStat"),': ', varICVarianza,'\\n")\n',sep="" )
    
    # resultado<-paste('cat("',tipointervalo, '\\n',gettext("Confidence level",domain="R-RcmdrPlugin.TeachStat"),': ', valornConfianza*100,'%\\n',gettext("Variable",domain="R-RcmdrPlugin.TeachStat"),': ', varICVarianza,'\\n")', sep="" )
    # command2<- paste(command2, resultado,"\n",sep="" )
    
    command2<-paste(command2,'cat("',gettext("Sample estimate",domain="R-RcmdrPlugin.TeachStat"),':",names(aux$estimate),as.numeric(aux$estimate),"\\n")',"\n",sep="")
    command2<-paste(command2,'cat("',gettext("Interval",domain="R-RcmdrPlugin.TeachStat"),': (",aux2[1],",",aux2[2],")\\n")',"\n})",sep="")
    
    doItAndPrint(command2)
    
    
    ###############################################################################################################
    
    ####calcular_ICVarianza(v.numerica=variableICVarianza,media.conocida=varConocida,valor.media=valormedia, nivel.confianza=valornConfianza)
    
    tkfocus(CommanderWindow())
  }
  
  OKCancelHelp(helpSubject = dialogName, reset=dialogName, apply=dialogName)
  
  
  tkgrid(Etiqueta,sticky="nw" )
  tkgrid(mediaFrame , mediaEntry, sticky="nw")
  tkgrid(tipoFrame , sticky="nw")
  
  tkgrid(getFrame(selectVariableICVarianza),labelRcmdr(comboBoxFrame, text="          "),radioButtonsFrame, sticky="nw")
  
  
  
  tkgrid(comboBoxFrame, sticky="nw")
  
  tkgrid(labelRcmdr(top, text="          "))
  
  tkgrid(labelRcmdr(nConfianzaFrame, text=gettext("Confidence level:",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color")),nConfianzaEntry, sticky="nw")
  tkgrid(nConfianzaFrame, 
         labelRcmdr(nconftypeFrame, text="          "),radioButtonstypeFrame, sticky="nw")
  tkgrid(nconftypeFrame, sticky="w")
  
  dialogSuffix(grid.buttons=TRUE)
}



