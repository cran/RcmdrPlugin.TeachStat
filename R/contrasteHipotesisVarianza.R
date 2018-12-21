contrasteHipotesisVarianza <- function () {
 #invisible(library(tcltk2))

  defaults <- list (initial.var=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),initial.Media="0",initial.valorMediaconocida="",
                    initial.nconf="0.95",initial.alternative = "two.sided",initial.sigma2 = "1")

  dialog.values <- getDialog ("contrasteHipotesisVarianza", defaults)
  initializeDialog(title = gettext("Hypothesis Testing for the variance",domain="R-RcmdrPlugin.TeachStat"))

  comboBoxFrame<-tkframe(top)

  selectVariableHVarianzaC <- variableComboBox(comboBoxFrame, variableList=Numeric(),
     initialSelection=dialog.values$initial.var, title=gettext("Variable (pick one)",domain="R-RcmdrPlugin.TeachStat"))


radioButtonsFrame<-tkframe(top)
  Etiqueta<-labelRcmdr(radioButtonsFrame, text=gettext("Mean",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color"))
  radioButtons(window = radioButtonsFrame , name="Media", buttons=c("C", "D"), values=c("1", "0"),
               initialValue=dialog.values$initial.Media,
               labels=gettext(c("Known", "Unknown"),domain="R-RcmdrPlugin.TeachStat"),

               command = function(){ if(tclvalue(MediaVariable)=="1"){
                 tk2state.set(MediaEntry, state = "normal")} else
                 { tk2state.set(MediaEntry, state = "disabled")}

                 }
               )

  Mediaconocida <- tclVar(dialog.values$initial.valorMediaconocida)
  MediaEntry <- ttkentry(radioButtonsFrame, width="10", textvariable=Mediaconocida, state="disabled")


  optionsFrame <- tkframe(top)
  radioButtons(optionsFrame, name = "alternative", buttons = c("twosided",
                                                               "less", "greater"), values = c("two.sided", "less", "greater"),
               labels = gettext(c("Population variance != sigma^2_0", "Population variance < sigma^2_0",
                                       "Population variance > sigma^2_0"),domain="R-RcmdrPlugin.TeachStat"), title = gettext("Alternative hypothesis",domain="R-RcmdrPlugin.TeachStat"),
               initialValue = dialog.values$initial.alternative)





  rightFrame<-tkframe(top)

  sigma2Frame <- tkframe(rightFrame)
  sigma2Variable <- tclVar(dialog.values$initial.sigma2)
  sigma2Field <- ttkentry(sigma2Frame, width = "5", textvariable = sigma2Variable)
  tkgrid(labelRcmdr(sigma2Frame, text=gettext("Null hypothesis: sigma^2 = ",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ), sigma2Field, sticky="nw")

  nConfianzaFrame<-tkframe(rightFrame)
  nConfianzaVar<-tclVar(dialog.values$initial.nconf)
  nConfianzaEntry<-ttkentry(nConfianzaFrame,width="5",textvariable=nConfianzaVar)
 # tkgrid(labelRcmdr(nConfianzaFrame, text="Nivel de Confianza = ", foreground="blue" ),nConfianzaEntry, sticky="nw")




  onOK <- function(){

    varHVarianzaC<-getSelection(selectVariableHVarianzaC)
    activeDataSet <- ActiveDataSet()
    activeDataSet<-get(activeDataSet)

    if(varHVarianzaC==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){errorCondition(recall=contrasteHipotesisVarianza, message=gettext("No variable selected",domain="R-RcmdrPlugin.TeachStat"))
      return()}
    else{activeDataSet <- ActiveDataSet()
         activeDataSet<-get(activeDataSet)
         variableHVarianzaC<-subset(activeDataSet,select=varHVarianzaC)}

    varConocida <- tclvalue(MediaVariable)
    if(varConocida=="1"){valorMedia<-tclvalue(Mediaconocida)
                if(is.na(as.numeric(valorMedia))) {
                  valorMedia=""
                  errorCondition(recall=contrasteHipotesisVarianza, message=gettext("Known mean value must be numeric",domain="R-RcmdrPlugin.TeachStat"))
                  return()
                }
                else{valorMedia<-as.numeric(valorMedia)}
    }else{valorMedia<-""}


    valornConfianza<-tclvalue(nConfianzaVar)

    if(is.na(as.numeric(valornConfianza)) || (as.numeric(valornConfianza)<0)||(as.numeric(valornConfianza)>1)) {
      valornConfianza=0.95
      errorCondition(recall=contrasteHipotesisVarianza, message=gettext("Confidence level must be between 0 and 1",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{valornConfianza<-as.numeric(valornConfianza)}

    valorsigma20<-tclvalue(sigma2Variable)

    if(is.na(as.numeric(valorsigma20))||(as.numeric(valorsigma20)<=0)){
      valorsigma20="0.0"
      errorCondition(recall=contrasteHipotesisVarianza, message=gettext("Value for the null hypothesis must be a positive number",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{ valorsigma20<-as.numeric(valorsigma20)}

    varHAlternativa<-tclvalue(alternativeVariable)

    putDialog ("contrasteHipotesisVarianza", list(initial.var=varHVarianzaC,initial.Media="0",initial.valorMediaconocida=valorMedia,initial.nconf=valornConfianza,
                  initial.alternative = varHAlternativa,initial.sigma2 = valorsigma20))
    closeDialog()

   valorMedia<-as.numeric(valorMedia)
   varConocida<-as.logical(as.numeric(varConocida))


   ###################### Imprimir la función a llamar por RCommander ###########################################

   .activeDataSet<-ActiveDataSet()

   vHCVarianza<-paste(.activeDataSet,"$",varHVarianzaC, sep="")

   Haltern<-paste('"',varHAlternativa,'"',sep="")

  # command<- paste("calcular_CHVarianza(v.numerica=", vHCVarianza,", media.conocida=", varConocida,", valor.media=", valorMedia,", hipotesis.alternativa=",Haltern,", sigma20=",valorsigma20, ", nivel.confianza=",valornConfianza,")",sep="" )


   if(varConocida==FALSE){
     command2<- paste("aux<-VUM.test(",vHCVarianza,", alternative=",Haltern,", sigma=",sqrt(valorsigma20),", conf.level=",valornConfianza,")",sep="" )
     tipointervalo<-paste("\\n",gettext("Hypothesis Testing for the variance with unknown mean",domain="R-RcmdrPlugin.TeachStat"),"\\n", sep="")
   }
   else{
     command2<- paste("aux<-VKM.test(",vHCVarianza,", alternative=",Haltern,", sigma=",sqrt(valorsigma20),", mu=", valorMedia,", conf.level=",valornConfianza,")",sep="")
     tipointervalo<-paste("\\n",gettext("Hypothesis Testing for the variance with known mean =",domain="R-RcmdrPlugin.TeachStat")," ",valorMedia,"\\n", sep="")
   }

      linaux<-paste(rep(c("-"," "),(nchar(tipointervalo)/2)),collapse="")
   tipointervalo<-paste(tipointervalo, linaux,sep="")
   command2<- paste("local({\n",command2,"\n",sep="")

   resultado<-paste('cat("',tipointervalo, '\\n',gettext("Variable",domain="R-RcmdrPlugin.TeachStat"),': ', varHVarianzaC, '\\n','")', sep="" )
   #distribucion<-'\n if(names(aux$statistic)=="z"){cat("Distribuci?n:",names(aux$statistic),"con distribuci?n N(0,1)\\n")}
   #                    else {cat("Distribuci?n:",names(aux$statistic),"con",as.numeric(aux$parameter),"grados de libertad\\n")}'
   
   distribucion<-paste('\n cat("',gettext("Distribution",domain="R-RcmdrPlugin.TeachStat"),':",names(aux$statistic),"',gettext("with",domain="R-RcmdrPlugin.TeachStat"),'",as.numeric(aux$parameter[1]),"',gettext("degrees of freedom",domain="R-RcmdrPlugin.TeachStat"),'\\n")',sep="")
   e.contraste<- paste('\n cat("',gettext("Test statistics value",domain="R-RcmdrPlugin.TeachStat"),':",as.numeric(aux$statistic),"\\n")',sep="")

   p.valor<-paste('\n if(aux$p.value>0.05){cat("',gettext("p-value",domain="R-RcmdrPlugin.TeachStat"),':",format.pval(aux$p.value),"\\n")}
    if(aux$p.value>=0.025 && aux$p.value<=0.05){cat("',gettext("p-value",domain="R-RcmdrPlugin.TeachStat"),':",format.pval(aux$p.value),"*\\n")}
    if(aux$p.value>=0.0001 && aux$p.value<=0.025){cat("',gettext("p-value",domain="R-RcmdrPlugin.TeachStat"),':",format.pval(aux$p.value),"**\\n")}
    if(aux$p.value>=0 && aux$p.value<=0.0001){cat("',gettext("p-value",domain="R-RcmdrPlugin.TeachStat"),':",format.pval(aux$p.value),"***\\n")}\n',sep="")


   if (varHAlternativa == "two.sided"){h.alt<-paste(gettext("Population variance is not equal to",domain="R-RcmdrPlugin.TeachStat"),valorsigma20,sep=" ")}
   if (varHAlternativa == "less"){h.alt<-paste(gettext("Population variance is less than",domain="R-RcmdrPlugin.TeachStat"),valorsigma20,sep=" ")}
   if (varHAlternativa == "greater"){h.alt<- paste(gettext("Population variance is greater than",domain="R-RcmdrPlugin.TeachStat"),valorsigma20,sep=" ")}
   h.alt<-paste('\n cat("',gettext("Alternative hypothesis",domain="R-RcmdrPlugin.TeachStat"),':","',h.alt,'","\\n")',sep="")

   e.muestral<-paste('\n cat("',gettext("Sample estimate",domain="R-RcmdrPlugin.TeachStat"),':",names(aux$estimate),as.numeric(aux$estimate),"\\n")',sep="")


   command2<- paste(command2, resultado, distribucion,e.contraste,p.valor, h.alt,e.muestral,"\n})",sep="" )

   doItAndPrint(command2)

   ###############################################################################################################

   tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "intervaloConfianzaVarianza", reset="contrasteHipotesisVarianza", apply="contrasteHipotesisVarianza")


  tkgrid(Etiqueta,sticky="nw" )
  tkgrid(MediaFrame , MediaEntry, sticky="nw")

  tkgrid(getFrame(selectVariableHVarianzaC),labelRcmdr(comboBoxFrame, text="                 "),radioButtonsFrame, sticky="nw")
  tkgrid(comboBoxFrame, sticky="nw")

  tkgrid(labelRcmdr(top, text="          "),sticky="nw")

  tkgrid(labelRcmdr(rightFrame, text="        "),sticky="nw")
  tkgrid(sigma2Frame,sticky="nw")
  tkgrid(nConfianzaFrame, sticky="nw")


  tkgrid(alternativeFrame,labelRcmdr(optionsFrame, text="          "),rightFrame, sticky="nw")
  tkgrid(optionsFrame,sticky="nw")

  tkgrid(buttonsFrame, sticky="w")


  dialogSuffix()
}



