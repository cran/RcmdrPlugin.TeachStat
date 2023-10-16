contrasteHipotesisMedia <- function () {
 #invisible(library(tcltk2))

  defaults <- list (initial.var=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),initial.varianza="0",initial.valorvarianzaconocida="",
                    initial.nconf="0.95",initial.alternative = "two.sided",initial.mu = "0.0")

  dialog.values <- getDialog ("contrasteHipotesisMedia", defaults)
  initializeDialog(title = gettext("Hypothesis Testing for the mean",domain="R-RcmdrPlugin.TeachStat"))

  comboBoxFrame<-tkframe(top)

  selectVariableHCMedia <- variableComboBox(comboBoxFrame, variableList=Numeric(),
     initialSelection=dialog.values$initial.var, title=gettext("Variable (pick one)",domain="R-RcmdrPlugin.TeachStat"))


radioButtonsFrame<-tkframe(top)
  Etiqueta<-labelRcmdr(radioButtonsFrame, text=gettext("Variance",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color"))
  radioButtons(window = radioButtonsFrame , name="varianza", buttons=c("C", "D"), values=c("1", "0"),
               initialValue=dialog.values$initial.varianza,
               labels=gettext(c("Known", "Unknown"),domain="R-RcmdrPlugin.TeachStat"),

               command = function(){ if(tclvalue(varianzaVariable)=="1"){
                 tk2state.set(varianzaEntry, state = "normal")} else
                 { tk2state.set(varianzaEntry, state = "disabled")}

                 }
               )

  varianzaconocida <- tclVar(dialog.values$initial.valorvarianzaconocida)
  varianzaEntry <- ttkentry(radioButtonsFrame, width="10", textvariable=varianzaconocida, state="disabled")


  optionsFrame <- tkframe(top)
  radioButtons(optionsFrame, name = "alternative", buttons = c("twosided",
                                                               "less", "greater"), values = c("two.sided", "less", "greater"),
               labels = gettext(c("Population mean != mu0", "Population mean < mu0",
                                       "Population mean > mu0"),domain="R-RcmdrPlugin.TeachStat"), title = gettext("Alternative hypothesis",domain="R-RcmdrPlugin.TeachStat"),
               initialValue = dialog.values$initial.alternative)





  rightFrame<-tkframe(top)

  muFrame <- tkframe(rightFrame)
  muVariable <- tclVar(dialog.values$initial.mu)
  muField <- ttkentry(muFrame, width = "5", textvariable = muVariable)
  tkgrid(labelRcmdr(muFrame, text=gettext("Null hypothesis: mu =",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ), muField, sticky="nw")

  nConfianzaFrame<-tkframe(rightFrame)
  nConfianzaVar<-tclVar(dialog.values$initial.nconf)
  nConfianzaEntry<-ttkentry(nConfianzaFrame,width="5",textvariable=nConfianzaVar)
 # tkgrid(labelRcmdr(nConfianzaFrame, text="Nivel de Confianza = ", foreground="blue" ),nConfianzaEntry, sticky="nw")




  onOK <- function(){

    varHCMedia<-getSelection(selectVariableHCMedia)
    activeDataSet <- ActiveDataSet()
    activeDataSet<-get(activeDataSet)

    if(varHCMedia==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){errorCondition(recall=contrasteHipotesisMedia, message=gettext("No variable selected",domain="R-RcmdrPlugin.TeachStat"))
      return()}
    else{activeDataSet <- ActiveDataSet()
         activeDataSet<-get(activeDataSet)
         variableHCMedia<-subset(activeDataSet,select=varHCMedia)}

    varConocida <- tclvalue(varianzaVariable)
    if(varConocida=="1"){valorvarianza<-tclvalue(varianzaconocida)
                if(is.na(as.numeric(valorvarianza)) || (as.numeric(valorvarianza)<=0)) {
                  valorvarianza<-""
                  errorCondition(recall=contrasteHipotesisMedia, message=gettext("Known variance value must be a positive number",domain="R-RcmdrPlugin.TeachStat"))
                  return()
                }
                else{valorvarianza<-as.numeric(valorvarianza)}
    }else{valorvarianza<-""}


    valornConfianza<-tclvalue(nConfianzaVar)

    if(is.na(as.numeric(valornConfianza)) || (as.numeric(valornConfianza)<0)||(as.numeric(valornConfianza)>1)) {
      valornConfianza<-0.95
      errorCondition(recall=contrasteHipotesisMedia, message=gettext("Confidence level must be between 0 and 1",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{valornConfianza<-as.numeric(valornConfianza)}

    valormu0<-tclvalue(muVariable)

    if(is.na(as.numeric(valormu0))){
      valormu0<-"0.0"
      errorCondition(recall=contrasteHipotesisMedia, message=gettext("No valid value for the Null Hypothesis",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{ valormu0<-as.numeric(valormu0)}

    varHAlternativa<-tclvalue(alternativeVariable)

    putDialog ("contrasteHipotesisMedia", list(initial.var=varHCMedia,initial.varianza="0",initial.valorvarianzaconocida=valorvarianza,initial.nconf=valornConfianza,
                  initial.alternative = varHAlternativa,initial.mu = valormu0))
    closeDialog()

   valorvarianza<-as.numeric(valorvarianza)
   varConocida<-as.logical(as.numeric(varConocida))

###################### Imprimir la función a llamar por RCommander ###########################################

   .activeDataSet<-ActiveDataSet()

   vHCMedia<-paste(.activeDataSet,"$",varHCMedia, sep="")

   Haltern<-paste('"',varHAlternativa,'"',sep="")

#   command<- paste("calcular_CHMedia(v.numerica=", vHCMedia,", varianza.conocida=", varConocida,", valor.varianza=", valorvarianza,", hipotesis.alternativa=",Haltern,", mu0=",valormu0, ", nivel.confianza=",valornConfianza,")",sep="" )

#   doItAndPrint(command)



   if(varConocida==FALSE){
     command2<- paste("aux<-t.test(",vHCMedia,", alternative=",Haltern,", mu=",valormu0,", conf.level=",valornConfianza,")",sep="" )
     tipointervalo<-paste("\\n",gettext("Hypothesis Testing for the mean with unknown variance",domain="R-RcmdrPlugin.TeachStat"),"\\n", sep="")
   }
   else{
     command2<- paste("aux<-MKV.test(",vHCMedia,", sd=", sqrt(valorvarianza),", alternative=",Haltern,", mu=",valormu0,", conf.level=",valornConfianza,")",sep="")
     tipointervalo<-paste("\\n",gettext("Hypothesis Testing for the mean with known variance =",domain="R-RcmdrPlugin.TeachStat")," ",valorvarianza,"\\n", sep="")
   }

   linaux<-paste(rep(c("-"," "),(nchar(tipointervalo)/2)),collapse="")
   tipointervalo<-paste(tipointervalo, linaux,sep="")
   command2<- paste("local({\n",command2,"\n",sep="")

   resultado<-paste('cat("',tipointervalo, '\\n',gettext("Variable",domain="R-RcmdrPlugin.TeachStat"),': ', varHCMedia, '\\n")', sep="" )
   distribucion<-paste('\n if(names(aux$statistic)=="z"){\ncat("',gettext("Distribution",domain="R-RcmdrPlugin.TeachStat"),':","N(0,1)\\n")\n} else {\ncat("',gettext("Distribution",domain="R-RcmdrPlugin.TeachStat"),':",names(aux$statistic),"',gettext("with",domain="R-RcmdrPlugin.TeachStat"),'",as.numeric(aux$parameter),"',gettext("degrees of freedom",domain="R-RcmdrPlugin.TeachStat"),'\\n")\n}',sep="")

   e.contraste<- paste('\n cat("',gettext("Test statistics value",domain="R-RcmdrPlugin.TeachStat"),':",as.numeric(aux$statistic),"\\n")',sep="")

   p.valor<-paste('\n if(aux$p.value>0.05){cat("',gettext("p-value",domain="R-RcmdrPlugin.TeachStat"),':",format.pval(aux$p.value),"\\n")}
    if(aux$p.value>=0.025 && aux$p.value<=0.05){cat("',gettext("p-value",domain="R-RcmdrPlugin.TeachStat"),':",format.pval(aux$p.value),"*\\n")}
    if(aux$p.value>=0.0001 && aux$p.value<=0.025){cat("',gettext("p-value",domain="R-RcmdrPlugin.TeachStat"),':",format.pval(aux$p.value),"**\\n")}
    if(aux$p.value>=0 && aux$p.value<=0.0001){cat("',gettext("p-value",domain="R-RcmdrPlugin.TeachStat"),':",format.pval(aux$p.value),"***\\n")}\n',sep="")


   if (varHAlternativa == "two.sided"){h.alt<-paste(gettext("Population mean is not equal to",domain="R-RcmdrPlugin.TeachStat"),valormu0,sep=" ")}
   if (varHAlternativa == "less"){h.alt<-paste(gettext("Population mean is less than",domain="R-RcmdrPlugin.TeachStat"),valormu0,sep=" ")}
   if (varHAlternativa == "greater"){h.alt<- paste(gettext("Population mean is greater than",domain="R-RcmdrPlugin.TeachStat"),valormu0,sep=" ")}
   h.alt<-paste('\n cat("',gettext("Alternative hypothesis",domain="R-RcmdrPlugin.TeachStat"),':","',h.alt,'","\\n")',sep="")

   e.muestral<-paste('\n cat("',gettext("Sample estimate",domain="R-RcmdrPlugin.TeachStat"),':",names(aux$estimate),as.numeric(aux$estimate),"\\n")',sep="")

   command2<- paste(command2, resultado, distribucion,e.contraste,p.valor, h.alt,e.muestral,"\n})",sep="" )

   doItAndPrint(command2)

###############################################################################################################


   ###  calcular_CHMedia(v.numerica=variableHCMedia,varianza.conocida=varConocida,valor.varianza=valorvarianza, hipotesis.alternativa=varHAlternativa,mu0=valormu0,nivel.confianza=valornConfianza)

   tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "intervaloConfianzaMedia", reset="contrasteHipotesisMedia", apply="contrasteHipotesisMedia")


  tkgrid(Etiqueta,sticky="nw" )
  tkgrid(varianzaFrame , varianzaEntry, sticky="nw")

  tkgrid(getFrame(selectVariableHCMedia),labelRcmdr(comboBoxFrame, text="                 "),radioButtonsFrame, sticky="nw")
  tkgrid(comboBoxFrame, sticky="nw")

  tkgrid(labelRcmdr(top, text="          "),sticky="nw")

  tkgrid(labelRcmdr(rightFrame, text="        "),sticky="nw")
  tkgrid(muFrame,sticky="nw")
  tkgrid(nConfianzaFrame, sticky="nw")

  tkgrid(alternativeFrame,labelRcmdr(optionsFrame, text="          "),rightFrame, sticky="nw")
  tkgrid(optionsFrame,sticky="nw")


  tkgrid(buttonsFrame, sticky="w")


  dialogSuffix()
}



