intervaloConfianzaMediasPareadas <- function () {
  #invisible(library(tcltk2))
  dialogName <- "intervaloConfianzaMediasPareadas"
  
  defaults <- list (initial.var1=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"), 
                    initial.var2=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),
                    initial.nconf="0.95",
                    initial.alternative = "two.sided")
  dialog.values <- getDialog (dialogName, defaults)
  initializeDialog(title = gettext("Confidence Interval for paired means",domain="R-RcmdrPlugin.TeachStat"))
  
  comboBoxFrame<-tkframe(top)
  
  selectVariable1ICMediasPareadas <- variableComboBox(comboBoxFrame, variableList=Numeric(),
                                                      initialSelection=dialog.values$initial.var1, title=gettext("First variable (pick one)",domain="R-RcmdrPlugin.TeachStat"))
  
  selectVariable2ICMediasPareadas <- variableComboBox(comboBoxFrame, variableList=Numeric(),
                                                      initialSelection=dialog.values$initial.var2, title=gettext("Second variable (pick one)",domain="R-RcmdrPlugin.TeachStat"))
  
  nconftypeFrame<-tkframe(top)
  
  
  ### Tipo de Intervalo
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
    
    var1ICMedia<-getSelection(selectVariable1ICMediasPareadas)
    var2ICMedia<-getSelection(selectVariable2ICMediasPareadas)
    
    if(var1ICMedia==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){
      errorCondition(recall=dialogNameF, message=gettext("No variable selected in First variable",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    if(var2ICMedia==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){
      errorCondition(recall=dialogNameF, message=gettext("No variable selected in Second variable",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    
    if(var1ICMedia==var2ICMedia){
      errorCondition(recall=dialogNameF, message=gettext("Selected variables must be different",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    
    ### Tipo de Intervalo
    varTipo <- tclvalue(tipoVariable)
    
    valornConfianza<-tclvalue(nConfianzaVar)
    
    if(is.na(as.numeric(valornConfianza)) || (as.numeric(valornConfianza)<0)||(as.numeric(valornConfianza)>1)) {
      valornConfianza=0.95
      errorCondition(recall=dialogNameF, message=gettext("Confidence level must be between 0 and 1",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{valornConfianza<-as.numeric(valornConfianza)}
    
    
    putDialog (dialogName, list(initial.var1=var1ICMedia,initial.var2=var2ICMedia,initial.nconf=valornConfianza,
                                initial.alternative=varTipo))
    closeDialog()
    
    
    ###################### Imprimir la función a llamar por RCommander ###########################################
    
    .activeDataSet<-ActiveDataSet()
    
    vICMedia1<-paste(.activeDataSet,"$",var1ICMedia, sep="")
    vICMedia2<-paste(.activeDataSet,"$",var2ICMedia, sep="")
    
    # command<- paste("calcular_ICMediasPareadas(v.primera=", v1ICMedia,", v.segunda=", v2ICMedia,", nivel.confianza=",valornConfianza,")",sep="" )
    #  doItAndPrint(command)
    
    command2<- paste("aux<-t.test(",vICMedia1,", ",vICMedia2,", paired = TRUE, conf.level=",valornConfianza,", alternative='",varTipo,"')",sep="")
    
    tipointervalo<-paste("\\n",gettext("Confidence Interval for the mean difference of two paired samples",domain="R-RcmdrPlugin.TeachStat"),"\\n",sep="")
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
    command2<- paste(command2, 'cat("',gettext("Variable",domain="R-RcmdrPlugin.TeachStat"),': ', var1ICMedia,' vs. ',var2ICMedia,'\\n")\n',sep="" )
    
    
    # resultado<-paste('cat("',tipointervalo, '\\n',
    #                  gettext("Confidence level",domain="R-RcmdrPlugin.TeachStat"),': ' , valornConfianza*100,'%\\n',
    #                  gettext("Variables",domain="R-RcmdrPlugin.TeachStat"),': ', var1ICMedia,' vs. ',var2ICMedia, '\\n")', sep="" )
    # command2<- paste(command2, resultado,"\n",sep="" )
    command2<-paste(command2,'cat("',gettext("Sample estimate",domain="R-RcmdrPlugin.TeachStat"),':",names(aux$estimate),as.numeric(aux$estimate),"\\n")',"\n",sep="")
    command2<-paste(command2,'cat("',gettext("Interval",domain="R-RcmdrPlugin.TeachStat"),': (",aux2[1],",",aux2[2],")\\n")',"\n})",sep="")
    
    doItAndPrint(command2)
    
    ###############################################################################################################
    
    ##calcular_ICMediasPareadas(v.numerica=variableICMedia,varianza.conocida=varConocida,valor.varianza=valorvarianza, nivel.confianza=valornConfianza)
    
    tkfocus(CommanderWindow())
  }
  
  OKCancelHelp(helpSubject = "t.test", reset=dialogName, apply=dialogName)
  
  tkgrid(tipoFrame , sticky="nw")
  tkgrid(getFrame(selectVariable1ICMediasPareadas),
         labelRcmdr(comboBoxFrame, text="          "),getFrame(selectVariable2ICMediasPareadas), sticky="nw")
  tkgrid(comboBoxFrame, sticky="nw")
  tkgrid(labelRcmdr(top, text="          "), sticky="nw")
  
  tkgrid(labelRcmdr(nConfianzaFrame, text=gettext("Confidence level:",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ),nConfianzaEntry, sticky="nw")
  tkgrid(nConfianzaFrame, 
         labelRcmdr(nconftypeFrame, text="          "),radioButtonstypeFrame, sticky="nw")
  tkgrid(nconftypeFrame, sticky="w")
  
  dialogSuffix(grid.buttons=TRUE)
}

