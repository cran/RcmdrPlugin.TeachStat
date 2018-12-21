intervaloConfianzaMediasPareadas <- function () {
 #invisible(library(tcltk2))

  defaults <- list (initial.var1=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"), initial.var2=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),initial.nconf="0.95")
  dialog.values <- getDialog ("intervaloConfianzaMediasPareadas", defaults)
  initializeDialog(title = gettext("Confidence Interval for paired means",domain="R-RcmdrPlugin.TeachStat"))

  comboBoxFrame<-tkframe(top)

  selectVariable1ICMediasPareadas <- variableComboBox(comboBoxFrame, variableList=Numeric(),
     initialSelection=dialog.values$initial.var1, title=gettext("First variable (pick one)",domain="R-RcmdrPlugin.TeachStat"))

  selectVariable2ICMediasPareadas <- variableComboBox(comboBoxFrame, variableList=Numeric(),
                                                      initialSelection=dialog.values$initial.var2, title=gettext("Second variable (pick one)",domain="R-RcmdrPlugin.TeachStat"))


  nConfianzaFrame<-tkframe(top)

  nConfianzaVar<-tclVar(dialog.values$initial.nconf)
  nConfianzaEntry<-ttkentry(nConfianzaFrame,width="5",textvariable=nConfianzaVar)
  tkgrid(labelRcmdr(nConfianzaFrame, text=gettext("Confidence level:",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ),nConfianzaEntry, sticky="nw")

  onOK <- function(){

    var1ICMedia<-getSelection(selectVariable1ICMediasPareadas)
    var2ICMedia<-getSelection(selectVariable2ICMediasPareadas)

    if(var1ICMedia==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){errorCondition(recall=intervaloConfianzaMediasPareadas, message=gettext("No variable selected in First variable",domain="R-RcmdrPlugin.TeachStat"))
      return()}
    if(var2ICMedia==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){errorCondition(recall=intervaloConfianzaMediasPareadas, message=gettext("No variable selected in Second variable",domain="R-RcmdrPlugin.TeachStat"))
      return()}

     if(var1ICMedia==var2ICMedia){errorCondition(recall=intervaloConfianzaMediasPareadas, message=gettext("Selected variables must be different",domain="R-RcmdrPlugin.TeachStat"))
      return()}

    valornConfianza<-tclvalue(nConfianzaVar)

    if(is.na(as.numeric(valornConfianza)) || (as.numeric(valornConfianza)<0)||(as.numeric(valornConfianza)>1)) {
      valornConfianza=0.95
      errorCondition(recall=intervaloConfianzaMediasPareadas, message=gettext("Confidence level must be between 0 and 1",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{valornConfianza<-as.numeric(valornConfianza)}


    putDialog ("intervaloConfianzaMediasPareadas", list(initial.var1=var1ICMedia,initial.var2=var2ICMedia,initial.nconf=valornConfianza))
    closeDialog()


###################### Imprimir la función a llamar por RCommander ###########################################

   .activeDataSet<-ActiveDataSet()

    vICMedia1<-paste(.activeDataSet,"$",var1ICMedia, sep="")
    vICMedia2<-paste(.activeDataSet,"$",var2ICMedia, sep="")

   # command<- paste("calcular_ICMediasPareadas(v.primera=", v1ICMedia,", v.segunda=", v2ICMedia,", nivel.confianza=",valornConfianza,")",sep="" )
   #  doItAndPrint(command)

  command2<- paste("aux<-t.test(",vICMedia1,", ",vICMedia2,", paired = TRUE, conf.level=",valornConfianza,")",sep="")

  tipointervalo<-paste("\\n",gettext("Confidence Interval for the mean difference of two paired samples",domain="R-RcmdrPlugin.TeachStat"),"\\n",sep="")
  linaux<-paste(rep(c("-"," "),(nchar(tipointervalo)/2)),collapse="")
  tipointervalo<-paste(tipointervalo, linaux,sep="")


  command2<- paste("local({\n",command2,"\n",'aux2<-as.vector(aux[["conf.int"]]) \n',sep="")

  resultado<-paste('cat("',tipointervalo, '\\n',gettext("Confidence level",domain="R-RcmdrPlugin.TeachStat"),': ' , valornConfianza*100,'%\\n',gettext("Variables",domain="R-RcmdrPlugin.TeachStat"),': ', var1ICMedia,' vs. ',var2ICMedia, '\\n")', sep="" )
  command2<- paste(command2, resultado,"\n",sep="" )
  command2<-paste(command2,'cat("',gettext("Sample estimate",domain="R-RcmdrPlugin.TeachStat"),':",names(aux$estimate),as.numeric(aux$estimate),"\\n")',"\n",sep="")
  command2<-paste(command2,'cat("',gettext("Interval",domain="R-RcmdrPlugin.TeachStat"),': (",aux2[1],",",aux2[2],")\\n")',"\n})",sep="")

  doItAndPrint(command2)

###############################################################################################################

    ##calcular_ICMediasPareadas(v.numerica=variableICMedia,varianza.conocida=varConocida,valor.varianza=valorvarianza, nivel.confianza=valornConfianza)

   tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "t.test", reset="intervaloConfianzaMediasPareadas", apply="intervaloConfianzaMediasPareadas")

  tkgrid(getFrame(selectVariable1ICMediasPareadas),labelRcmdr(comboBoxFrame, text="          "),getFrame(selectVariable2ICMediasPareadas), sticky="nw")
  tkgrid(comboBoxFrame, sticky="nw")
  tkgrid(labelRcmdr(top, text="          "), sticky="nw")
  tkgrid(nConfianzaFrame, sticky="nw")
  tkgrid(buttonsFrame, sticky="w")

  dialogSuffix()
}


