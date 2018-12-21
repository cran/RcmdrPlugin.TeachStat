contrasteHipotesisMediasPareadas <- function () {
 #invisible(library(tcltk2))

  defaults <- list (initial.var1=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"), initial.var2=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),initial.nconf="0.95",initial.alternative = "two.sided",initial.mu = "0.0")
  dialog.values <- getDialog ("contrasteHipotesisMediasPareadas", defaults)
  initializeDialog(title = gettext("Hypothesis Testing for paired means",domain="R-RcmdrPlugin.TeachStat"))

  comboBoxFrame<-tkframe(top)

  selectVariable1ICMediasPareadas <- variableComboBox(comboBoxFrame, variableList=Numeric(),
     initialSelection=dialog.values$initial.var1, title=gettext("First variable (pick one)",domain="R-RcmdrPlugin.TeachStat"))

  selectVariable2ICMediasPareadas <- variableComboBox(comboBoxFrame, variableList=Numeric(),
                                                      initialSelection=dialog.values$initial.var2, title=gettext("Second variable (pick one)",domain="R-RcmdrPlugin.TeachStat"))




  optionsFrame <- tkframe(top)
  radioButtons(optionsFrame, name = "alternative", buttons = c("twosided",
                                                               "less", "greater"), values = c("two.sided", "less", "greater"),
               labels = gettext(c("Difference between population means != mu0_1-mu0_2", "Difference between population means < mu0_1-mu0_2",
                                       "Difference between population means > mu0_1-mu0_2"),domain="R-RcmdrPlugin.TeachStat"), title = gettext("Alternative hypothesis",domain="R-RcmdrPlugin.TeachStat"),
               initialValue = dialog.values$initial.alternative)


  rightFrame<-tkframe(top)

  muFrame <- tkframe(rightFrame)
  muVariable <- tclVar(dialog.values$initial.mu)
  muField <- ttkentry(muFrame, width = "5", textvariable = muVariable)
  tkgrid(labelRcmdr(muFrame, text=gettext("Null hypothesis: mu_1 - mu_2 =",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color")), muField, sticky="nw")

  nConfianzaFrame<-tkframe(rightFrame)
  nConfianzaVar<-tclVar(dialog.values$initial.nconf)
  nConfianzaEntry<-ttkentry(nConfianzaFrame,width="5",textvariable=nConfianzaVar)
#  tkgrid(labelRcmdr(nConfianzaFrame, text="Nivel de Confianza (1-alpha)= ", foreground="blue" ),nConfianzaEntry, sticky="nw")



  onOK <- function(){

    var1ICMedia<-getSelection(selectVariable1ICMediasPareadas)
    var2ICMedia<-getSelection(selectVariable2ICMediasPareadas)

    if(var1ICMedia==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){errorCondition(recall=contrasteHipotesisMediasPareadas, message=gettext("No variable selected in First variable",domain="R-RcmdrPlugin.TeachStat"))
      return()}
    if(var2ICMedia==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){errorCondition(recall=contrasteHipotesisMediasPareadas, message=gettext("No variable selected in Second variable",domain="R-RcmdrPlugin.TeachStat"))
      return()}

     if(var1ICMedia==var2ICMedia){errorCondition(recall=contrasteHipotesisMediasPareadas, message=gettext("Selected variables must be different",domain="R-RcmdrPlugin.TeachStat"))
      return()}

    valornConfianza<-tclvalue(nConfianzaVar)

    if(is.na(as.numeric(valornConfianza)) || (as.numeric(valornConfianza)<0)||(as.numeric(valornConfianza)>1)) {
      valornConfianza=0.95
      errorCondition(recall=contrasteHipotesisMediasPareadas, message=gettext("Confidence level must be between 0 and 1",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{valornConfianza<-as.numeric(valornConfianza)}

    valormu0<-tclvalue(muVariable)

    if(is.na(as.numeric(valormu0))){
      valormu0="0.0"
      errorCondition(recall=contrasteHipotesisMediasPareadas, message=gettext("No valid value for the Null Hypothesis",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{ valormu0<-as.numeric(valormu0)}

    varHAlternativa<-tclvalue(alternativeVariable)


    putDialog ("contrasteHipotesisMediasPareadas", list(initial.var1=var1ICMedia,initial.var2=var2ICMedia,initial.nconf=valornConfianza,
                                                        initial.alternative = varHAlternativa,initial.mu = valormu0))
    closeDialog()


###################### Imprimir la función a llamar por RCommander ###########################################

   .activeDataSet<-ActiveDataSet()

    vICMedia1<-paste(.activeDataSet,"$",var1ICMedia, sep="")
    vICMedia2<-paste(.activeDataSet,"$",var2ICMedia, sep="")

    Haltern<-paste('"',varHAlternativa,'"',sep="")


    command2<- paste("aux<-t.test(",vICMedia1,", ",vICMedia2,", mu=",valormu0,", alternative=",Haltern,", paired = TRUE, conf.level=",valornConfianza,")",sep="")

    tipointervalo<-paste('\\n',gettext("Hypothesis Testing for the mean difference of two paired samples",domain="R-RcmdrPlugin.TeachStat"),'\\n',sep="")
    linaux<-paste(rep(c("-"," "),(nchar(tipointervalo)/2)),collapse="")
    tipointervalo<-paste(tipointervalo, linaux,sep="")
    command2<- paste("local({\n",command2,"\n",sep="")

    resultado<-paste('cat("',tipointervalo, "\\nVariables: ", var1ICMedia," vs. ",var2ICMedia, "\\n",'")', sep="" )
    distribucion<-paste('\n cat("',gettext("Distribution",domain="R-RcmdrPlugin.TeachStat"),':",names(aux$statistic),"',gettext("with",domain="R-RcmdrPlugin.TeachStat"),'",as.numeric(aux$parameter),"',gettext("degrees of freedom",domain="R-RcmdrPlugin.TeachStat"),'\\n")',sep="")
    e.contraste<- paste('\n cat("',gettext("Test statistics value",domain="R-RcmdrPlugin.TeachStat"),':",as.numeric(aux$statistic),"\\n")',sep="")
    
    p.valor<-paste('\n if(aux$p.value>0.05){cat("',gettext("p-value",domain="R-RcmdrPlugin.TeachStat"),':",format.pval(aux$p.value),"\\n")}
                   if(aux$p.value>=0.025 && aux$p.value<=0.05){cat("',gettext("p-value",domain="R-RcmdrPlugin.TeachStat"),':",format.pval(aux$p.value),"*\\n")}
                   if(aux$p.value>=0.0001 && aux$p.value<=0.025){cat("',gettext("p-value",domain="R-RcmdrPlugin.TeachStat"),':",format.pval(aux$p.value),"**\\n")}
                   if(aux$p.value>=0 && aux$p.value<=0.0001){cat("',gettext("p-value",domain="R-RcmdrPlugin.TeachStat"),':",format.pval(aux$p.value),"***\\n")}\n',sep="")

    if (varHAlternativa == "two.sided"){h.alt<-paste(gettext("Population difference is not equal to",domain="R-RcmdrPlugin.TeachStat"),valormu0,sep=" ")}
    if (varHAlternativa == "less"){h.alt<-paste(gettext("Population difference is less than",domain="R-RcmdrPlugin.TeachStat"),valormu0,sep=" ")}
    if (varHAlternativa == "greater"){h.alt<- paste(gettext("Population difference is greater than",domain="R-RcmdrPlugin.TeachStat"),valormu0,sep=" ")}
    h.alt<-paste('\n cat("',gettext("Alternative hypothesis",domain="R-RcmdrPlugin.TeachStat"),':","',h.alt,'","\\n")',sep="")

    e.muestral<-paste('\n cat("',gettext("Sample estimate",domain="R-RcmdrPlugin.TeachStat"),':",names(aux$estimate),as.numeric(aux$estimate),"\\n")',sep="")
    command2<- paste(command2, resultado, distribucion,e.contraste,p.valor, h.alt,e.muestral,"\n})",sep="" )

    doItAndPrint(command2)


###############################################################################################################

    ##calcular_HCMediasPareadas(v.numerica=variableICMedia,varianza.conocida=varConocida,valor.varianza=valorvarianza, nivel.confianza=valornConfianza)

   tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "t.test", reset="contrasteHipotesisMediasPareadas", apply="contrasteHipotesisMediasPareadas")

  tkgrid(getFrame(selectVariable1ICMediasPareadas),labelRcmdr(comboBoxFrame, text="          "),getFrame(selectVariable2ICMediasPareadas), sticky="nw")
  tkgrid(comboBoxFrame, sticky="nw")
  tkgrid(labelRcmdr(top, text="          "), sticky="nw")

  tkgrid(labelRcmdr(rightFrame, text="        "),sticky="nw")
  tkgrid(muFrame,sticky="nw")
  tkgrid(nConfianzaFrame, sticky="nw")
  tkgrid(alternativeFrame,labelRcmdr(optionsFrame, text="          "),rightFrame, sticky="nw")
  tkgrid(optionsFrame,sticky="nw")

  tkgrid(buttonsFrame, sticky="w")

  dialogSuffix()
}


