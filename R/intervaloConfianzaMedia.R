intervaloConfianzaMedia <- function () {
 #invisible(library(tcltk2))

  defaults <- list (initial.var=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),initial.varianza="0",initial.valorvarianzaconocida="",initial.nconf="0.95")
  dialog.values <- getDialog ("intervaloConfianzaMedia", defaults)
  initializeDialog(title = gettext("Confidence Interval for the mean",domain="R-RcmdrPlugin.TeachStat"))

  comboBoxFrame<-tkframe(top)

  selectVariableICMedia <- variableComboBox(comboBoxFrame, variableList=Numeric(),
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

  nConfianzaFrame<-tkframe(top)

  nConfianzaVar<-tclVar(dialog.values$initial.nconf)
  nConfianzaEntry<-ttkentry(nConfianzaFrame,width="5",textvariable=nConfianzaVar)
  tkgrid(labelRcmdr(nConfianzaFrame, text=gettext("Confidence level:",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ),nConfianzaEntry, sticky="nw")

  onOK <- function(){

    varICMedia<-getSelection(selectVariableICMedia)
    activeDataSet <- ActiveDataSet()
    activeDataSet<-get(activeDataSet)

    if(varICMedia==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){errorCondition(recall=intervaloConfianzaMedia, message=gettext("No variable selected",domain="R-RcmdrPlugin.TeachStat"))
      return()}
    else{activeDataSet <- ActiveDataSet()
         activeDataSet<-get(activeDataSet)
         variableICMedia<-subset(activeDataSet,select = varICMedia)}

    varConocida <- tclvalue(varianzaVariable)
    if(varConocida=="1"){valorvarianza<-tclvalue(varianzaconocida)
                if(is.na(as.numeric(valorvarianza)) || (as.numeric(valorvarianza)<=0)) {
                  valorvarianza=""
                  errorCondition(recall=intervaloConfianzaMedia, message=gettext("Known variance value must be a positive number",domain="R-RcmdrPlugin.TeachStat"))
                  return()
                }
                else{valorvarianza<-as.numeric(valorvarianza)}
    }else{valorvarianza<-""}


    valornConfianza<-tclvalue(nConfianzaVar)

    if(is.na(as.numeric(valornConfianza)) || (as.numeric(valornConfianza)<0)||(as.numeric(valornConfianza)>1)) {
      valornConfianza=0.95
      errorCondition(recall=intervaloConfianzaMedia, message=gettext("Confidence level must be between 0 and 1",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{valornConfianza<-as.numeric(valornConfianza)}


    putDialog ("intervaloConfianzaMedia", list(initial.var=varICMedia,initial.varianza="0",initial.valorvarianzaconocida=valorvarianza,initial.nconf=valornConfianza))
    closeDialog()

   valorvarianza<-as.numeric(valorvarianza)
   varConocida<-as.logical(as.numeric(varConocida))

###################### Imprimir la función a llamar por RCommander ###########################################

   .activeDataSet<-ActiveDataSet()

    vICMedia<-paste(.activeDataSet,"$",varICMedia, sep="")

   # command<- paste("calcular_ICMedia(v.numerica=", vICMedia,", varianza.conocida=", varConocida,", valor.varianza=", valorvarianza,", nivel.confianza=",valornConfianza,")",sep="" )

   # doItAndPrint(command)


    if(varConocida==FALSE){
      command2<- paste("aux<-t.test(",vICMedia,", conf.level=",valornConfianza,")",sep="" )
      tipointervalo<-paste("\\n",gettext("Confidence Interval for the mean with unknown variance",domain="R-RcmdrPlugin.TeachStat"),"\\n", sep="")
      }
    else{
      command2<- paste("aux<-MKV.test(",vICMedia,", sd=", sqrt(valorvarianza),", conf.level=",valornConfianza,")",sep="")
      tipointervalo<-paste("\\n",gettext("Confidence Interval for the mean with known variance =",domain="R-RcmdrPlugin.TeachStat")," ",valorvarianza,"\\n", sep="")
      }

    
    linaux<-paste(rep(c("-"," "),(nchar(tipointervalo)/2)),collapse="")
    tipointervalo<-paste(tipointervalo, linaux,sep="")


    command2<- paste("local({\n",command2,"\n",'aux2<-as.vector(aux[["conf.int"]]) \n',sep="")
    resultado<-paste('cat("',tipointervalo, '\\n',gettext("Confidence level",domain="R-RcmdrPlugin.TeachStat"),': ', valornConfianza*100,'%\\n',gettext("Variable",domain="R-RcmdrPlugin.TeachStat"),': ', varICMedia,'\\n")', sep="" )
    command2<- paste(command2, resultado,"\n",sep="" )
    command2<-paste(command2,'cat("',gettext("Sample estimate",domain="R-RcmdrPlugin.TeachStat"),':",names(aux$estimate),as.numeric(aux$estimate),"\\n")\n',sep="")
    command2<-paste(command2,'cat("',gettext("Interval",domain="R-RcmdrPlugin.TeachStat"),': (",aux2[1],",",aux2[2],")\\n")',"\n})",sep="")
    doItAndPrint(command2)


   tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "intervaloConfianzaMedia", reset="intervaloConfianzaMedia", apply="intervaloConfianzaMedia")


  tkgrid(Etiqueta,sticky="nw" )
  tkgrid(varianzaFrame , varianzaEntry, sticky="nw")

  tkgrid(getFrame(selectVariableICMedia),labelRcmdr(comboBoxFrame, text="          "),radioButtonsFrame, sticky="nw")



  tkgrid(comboBoxFrame, sticky="nw")
  tkgrid(nConfianzaFrame, sticky="nw")


  tkgrid(buttonsFrame, sticky="w")


  dialogSuffix()
}




