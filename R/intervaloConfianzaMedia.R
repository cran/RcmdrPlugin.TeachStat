intervaloConfianzaMedia <- function () {
  #invisible(library(tcltk2))
  
  dialogName <- "intervaloConfianzaMedia"
  
  defaults <- list (initial.var=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),initial.varianza="0",
                    initial.valorvarianzaconocida="",initial.nconf="0.95",
                    initial.alternative = "two.sided")
  dialog.values <- getDialog (dialogName, defaults)
  initializeDialog(title = gettext("Confidence Interval for the mean",domain="R-RcmdrPlugin.TeachStat"))
  
  comboBoxFrame<-tkframe(top)
  
  selectVariableICMedia <- variableComboBox(comboBoxFrame, variableList=Numeric(),
                                            initialSelection=dialog.values$initial.var, 
                                            title=gettext("Variable (pick one)",domain="R-RcmdrPlugin.TeachStat"))
  
  
  radioButtonsFrame<-tkframe(comboBoxFrame)
  Etiqueta<-labelRcmdr(radioButtonsFrame, text=gettext("Variance",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color"))
  radioButtons(window = radioButtonsFrame , name="varianza", buttons=c("C", "D"), values=c("1", "0"),
               initialValue=dialog.values$initial.varianza,
               labels=gettext(c("Known", "Unknown"),domain="R-RcmdrPlugin.TeachStat"),
               command = function(){ 
                 if(tclvalue(varianzaVariable)=="1"){
                   tk2state.set(varianzaEntry, state = "normal")
                 } else{
                   tk2state.set(varianzaEntry, state = "disabled")
                 }
               }
  )
  if(dialog.values$initial.varianza=="1"){
    mostrar<-"normal"
  } else{
    mostrar<-"disabled"
  }
  
  varianzaconocida <- tclVar(dialog.values$initial.valorvarianzaconocida)
  varianzaEntry <- ttkentry(radioButtonsFrame, width="10", textvariable=varianzaconocida, state=mostrar)
  
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
    
    varICMedia<-getSelection(selectVariableICMedia)
    activeDataSet <- ActiveDataSet()
    activeDataSet<-get(activeDataSet)
    
    if(varICMedia==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){errorCondition(recall=dialogNameF, message=gettext("No variable selected",domain="R-RcmdrPlugin.TeachStat"))
      return()}
    else{activeDataSet <- ActiveDataSet()
    activeDataSet<-get(activeDataSet)
    variableICMedia<-subset(activeDataSet,select = varICMedia)}
    
    varConocida <- tclvalue(varianzaVariable)
    if(varConocida=="1"){valorvarianza<-tclvalue(varianzaconocida)
    if(is.na(as.numeric(valorvarianza)) || (as.numeric(valorvarianza)<=0)) {
      valorvarianza=""
      errorCondition(recall=dialogNameF, message=gettext("Known variance value must be a positive number",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{valorvarianza<-as.numeric(valorvarianza)}
    }else{valorvarianza<-""}
    
    ### Tipo de Intervalo
    varTipo <- tclvalue(tipoVariable)
    
    
    valornConfianza<-tclvalue(nConfianzaVar)
    
    if(is.na(as.numeric(valornConfianza)) || (as.numeric(valornConfianza)<0)||(as.numeric(valornConfianza)>1)) {
      valornConfianza=0.95
      errorCondition(recall=dialogNameF, message=gettext("Confidence level must be between 0 and 1",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{valornConfianza<-as.numeric(valornConfianza)}
    
    
    putDialog (dialogName, list(initial.var=varICMedia,initial.varianza=varConocida,initial.valorvarianzaconocida=valorvarianza,
                                initial.nconf=valornConfianza,
                                initial.alternative=varTipo))
    closeDialog()
    
    valorvarianza<-as.numeric(valorvarianza)
    varConocida<-as.logical(as.numeric(varConocida))
    
    ###################### Imprimir la función a llamar por RCommander ###########################################
    
    .activeDataSet<-ActiveDataSet()
    
    vICMedia<-paste(.activeDataSet,"$",varICMedia, sep="")
    
    # command<- paste("calcular_ICMedia(v.numerica=", vICMedia,", varianza.conocida=", varConocida,", valor.varianza=", valorvarianza,", nivel.confianza=",valornConfianza,")",sep="" )
    
    # doItAndPrint(command)
    
    
    if(varConocida==FALSE){
      command2<- paste("aux<-t.test(",vICMedia,", conf.level=",valornConfianza,
                       ", alternative='",varTipo,"')",sep="" )
      tipointervalo<-paste("\\n",gettext("Confidence Interval for the mean with unknown variance",domain="R-RcmdrPlugin.TeachStat"),"\\n", sep="")
    }
    else{
      command2<- paste("aux<-MKV.test(",vICMedia,", sd=", sqrt(valorvarianza),", conf.level=",valornConfianza,
                       ", alternative='",varTipo,"')",sep="")
      tipointervalo<-paste("\\n",gettext("Confidence Interval for the mean with known variance =",domain="R-RcmdrPlugin.TeachStat")," ",valorvarianza,"\\n", sep="")
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
    command2<- paste(command2, 'cat("',gettext("Variable",domain="R-RcmdrPlugin.TeachStat"),': ', varICMedia,'\\n")\n',sep="" )
    
    #resultado<-paste('cat("',tipointervalo, '\\n',gettext("Confidence level",domain="R-RcmdrPlugin.TeachStat"),': ', valornConfianza*100,'%\\n',gettext("Variable",domain="R-RcmdrPlugin.TeachStat"),': ', varICMedia,'\\n")', sep="" )
    #command2<- paste(command2, resultado,"\n",sep="" )
    command2<-paste(command2,'cat("',gettext("Sample estimate",domain="R-RcmdrPlugin.TeachStat"),':",names(aux$estimate),as.numeric(aux$estimate),"\\n")\n',sep="")
    command2<-paste(command2,'cat("',gettext("Interval",domain="R-RcmdrPlugin.TeachStat"),': (",aux2[1],",",aux2[2],")\\n")',"\n})",sep="")
    doItAndPrint(command2)
    
    
    tkfocus(CommanderWindow())
  }
  
  OKCancelHelp(helpSubject = dialogName, reset=dialogName, apply=dialogName)
  
  
  tkgrid(Etiqueta,sticky="nw" )
  tkgrid(varianzaFrame , varianzaEntry, sticky="nw")
  tkgrid(tipoFrame , sticky="nw")
  tkgrid(getFrame(selectVariableICMedia), labelRcmdr(comboBoxFrame, text="          "),radioButtonsFrame,sticky="nw")
  
  
  tkgrid(comboBoxFrame,sticky="nw")
  
  tkgrid(labelRcmdr(top, text="          "))
  
  tkgrid(labelRcmdr(nConfianzaFrame, text=gettext("Confidence level:",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ),nConfianzaEntry, sticky="nw")
  tkgrid(nConfianzaFrame, 
         labelRcmdr(nconftypeFrame, text="          "),radioButtonstypeFrame, sticky="nw")
  tkgrid(nconftypeFrame, sticky="w")
  
  dialogSuffix(grid.buttons=TRUE)
}




