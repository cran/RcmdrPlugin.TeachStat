contrasteHipotesisMediasIndependientes <- function () {
 #invisible(library(tcltk2))

  defaults <- list (initial.var="<no variable selected>",initial.varianza="2",initial.valorvarianzaconocida1="",initial.valorvarianzaconocida2="",initial.nconf="0.95",initial.alternative = "two.sided",initial.mu = "0.0")
  dialog.values <- getDialog ("contrasteHipotesisMediasIndependientes", defaults)
  initializeDialog(title = gettext("Hypothesis Testing for the difference of independent means",domain="R-RcmdrPlugin.TeachStat"))

##Creación de los ComboBox

  selectFactorsFrame<-tkframe(top)
  
  variablenumericaFrame<-tkframe(selectFactorsFrame)
  selectVariableHCMedia <- variableComboBox(variablenumericaFrame, variableList=Numeric(),
                                            initialSelection=dialog.values$initial.var, title=gettext("Variable (pick one)",domain="R-RcmdrPlugin.TeachStat"))
  tkgrid(getFrame(selectVariableHCMedia), sticky="nw")
  
  
  comboBoxFrame<-tkframe(selectFactorsFrame)

  twoOrMoreLevelFactors<-twoOrMoreLevelFactors() ##NULL SI NO ACTIVEDATASET

  if (length(twoOrMoreLevelFactors())!=0){
    mostrar<-"readonly"
  }else {
    mostrar<-"disabled"
  }


  valuescombo_box<-c(gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),twoOrMoreLevelFactors())
  varcombo_box=tclVar(valuescombo_box[1])

  combo_box<-ttkcombobox(comboBoxFrame,values=valuescombo_box,textvariable=varcombo_box,state=mostrar)
  tkgrid(labelRcmdr(comboBoxFrame, text=gettext("Groups (pick one)",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color")), sticky="nw")
  tkgrid(combo_box,sticky="nw")


  tkbind(combo_box, "<<ComboboxSelected>>",function(){

    value<-tclvalue(varcombo_box)
    if(value!=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){
      datasetactivo <- ActiveDataSet()
      datasetactivo<-get(datasetactivo)
      niveles<-levels(datasetactivo[,value])
      tkconfigure(combo_box2,values=niveles)
      tclvalue(varcombo_box2)<-niveles[1]
      tk2state.set(combo_box2, state="readonly")
      tkconfigure(combo_box3,values=niveles)
      tclvalue(varcombo_box3)<-niveles[2]
      tk2state.set(combo_box3, state="readonly")
      tkfocus(combo_box2)}
    else{tk2state.set(combo_box2, state="disabled")
         tk2state.set(combo_box3, state="disabled")
         niveles<-gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")
          tkconfigure(combo_box2,values=niveles)
          tclvalue(varcombo_box2)<-niveles
          tkconfigure(combo_box3,values=niveles)
          tclvalue(varcombo_box3)<-niveles}})

  varcombo_box2=tclVar(gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"))
  combo_box2<-ttkcombobox(comboBoxFrame,values=varcombo_box2,textvariable=varcombo_box2,state="disabled")

  tkgrid(labelRcmdr(comboBoxFrame, text=gettext("Group 1",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ), sticky="nw")
  tkgrid(combo_box2, sticky="nw")

  varcombo_box3=tclVar(gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"))
  combo_box3<-ttkcombobox(comboBoxFrame,values=varcombo_box3,textvariable=varcombo_box3,state="disabled")

  tkgrid(labelRcmdr(comboBoxFrame, text=gettext("Group 2",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ), sticky="nw")
  tkgrid(combo_box3, sticky="nw")

##Fin creación comboBox




###Inicio Varianza Conocida

  radioButtonsFrame<-tkframe(top)
  Etiqueta<-labelRcmdr(radioButtonsFrame, text=gettext("Variances",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color"))
  radioButtons(window = radioButtonsFrame , name="varianza", buttons=c("C", "DI", "DD"), values=c("1", "2","3"),
               initialValue=dialog.values$initial.varianza,
               labels=gettext(c("Known", "Unknown and equal", "Unknown and unequal"),domain="R-RcmdrPlugin.TeachStat"),

               command = function(){ if(tclvalue(varianzaVariable)=="1"){
                 tk2state.set(varianzaEntry1, state = "normal")
                 tk2state.set(varianzaEntry2, state = "normal")
                 } else
                 { tk2state.set(varianzaEntry1, state = "disabled")
                   tk2state.set(varianzaEntry2, state = "disabled")}

               }
  )

  Etiqueta2<-labelRcmdr(radioButtonsFrame, text="   Sigma^2_1",foreground=getRcmdr("title.color"))
  Etiqueta3<-labelRcmdr(radioButtonsFrame, text="   Sigma^2_2",foreground=getRcmdr("title.color"))
  varianzaconocida1 <- tclVar(dialog.values$initial.valorvarianzaconocida1)
  varianzaEntry1 <- ttkentry(radioButtonsFrame, width="10", textvariable=varianzaconocida1, state="disabled")
  varianzaconocida2 <- tclVar(dialog.values$initial.valorvarianzaconocida2)
  varianzaEntry2 <- ttkentry(radioButtonsFrame, width="10", textvariable=varianzaconocida2, state="disabled")

  tkgrid(Etiqueta,Etiqueta2,Etiqueta3,sticky="nw" )
  tkgrid(varianzaFrame , varianzaEntry1, varianzaEntry2, sticky="nw")

#### Fin Radio Buttons

#### Inicio Contraste Hipotesis Alternativa, Nula y Nivel Confianza (Frame)

  optionsFrame <- tkframe(top)
  radioButtons(optionsFrame, name = "alternative", buttons = c("twosided",
                                                               "less", "greater"), values = c("two.sided", "less", "greater"),
               labels = gettext(c("Population mean difference != mu0_1-mu0_2", "Population mean difference < mu0_1-mu0_2",
                                       "Population mean difference > mu0_1-mu0_2"),domain="R-RcmdrPlugin.TeachStat"), title = gettext("Alternative hypothesis",domain="R-RcmdrPlugin.TeachStat"),
               initialValue = dialog.values$initial.alternative)


  rightFrame<-tkframe(top)

  muFrame <- tkframe(rightFrame)
  muVariable <- tclVar(dialog.values$initial.mu)
  muField <- ttkentry(muFrame, width = "5", textvariable = muVariable)
  tkgrid(labelRcmdr(muFrame, text=gettext("Null hypothesis: mu_1 - mu_2 = ",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color")), muField, sticky="nw")

  nConfianzaFrame<-tkframe(rightFrame)
  nConfianzaVar<-tclVar(dialog.values$initial.nconf)
  nConfianzaEntry<-ttkentry(nConfianzaFrame,width="5",textvariable=nConfianzaVar)
#  tkgrid(labelRcmdr(nConfianzaFrame, text="Nivel de Confianza (1-alpha)= ", foreground="blue" ),nConfianzaEntry, sticky="nw")

#######################################

onOK <- function(){

    varHCMediasIndependientes<-tclvalue(varcombo_box)
    if(varHCMediasIndependientes==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){errorCondition(recall=contrasteHipotesisMediasIndependientes, message=gettext("No group selected",domain="R-RcmdrPlugin.TeachStat"))
      return()}
    if ((length(twoOrMoreLevelFactors)!=0)&&(varHCMediasIndependientes !=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"))){
      varGrupo1<-tclvalue(varcombo_box2)
      varGrupo2<-tclvalue(varcombo_box3)}
    else {
      variableHCProporcion<-NULL
      varGrupo1<-NULL
      varGrupo2<-NULL
    }


    varHCMedia<-getSelection(selectVariableHCMedia)
    if(varHCMedia==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){errorCondition(recall=contrasteHipotesisMediasIndependientes, message=gettext("No variable selected",domain="R-RcmdrPlugin.TeachStat"))
      return()}

    if(varGrupo1==varGrupo2){errorCondition(recall=contrasteHipotesisMediasIndependientes, message=gettext("Group 1 and Group 2 must not coincide",domain="R-RcmdrPlugin.TeachStat"))
      return()}

    tipoVarianza <- tclvalue(varianzaVariable)
    if(tipoVarianza=="1"){valorsigma1<-tclvalue(varianzaconocida1)
                          valorsigma2<-tclvalue(varianzaconocida2)
          if(is.na(as.numeric(valorsigma1)) || (as.numeric(valorsigma1)<=0)||is.na(as.numeric(valorsigma2)) || (as.numeric(valorsigma2)<=0) ){
             valorsigma1=""
             valorsigma2=""
             errorCondition(recall=contrasteHipotesisMediasIndependientes, message=gettext("Known variances values must be positive numbers",domain="R-RcmdrPlugin.TeachStat"))
              return()
          }
         else{valorsigma1<-as.numeric(valorsigma1);valorsigma2<-as.numeric(valorsigma2)}
    }else{valorsigma1<-"";valorsigma2<-""}



    valornConfianza<-tclvalue(nConfianzaVar)

    if(is.na(as.numeric(valornConfianza)) || (as.numeric(valornConfianza)<0)||(as.numeric(valornConfianza)>1)) {
      valornConfianza=0.95
      errorCondition(recall=contrasteHipotesisMediasIndependientes, message=gettext("Confidence level must be between 0 and 1",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{valornConfianza<-as.numeric(valornConfianza)}

####

    valormu0<-tclvalue(muVariable)

    if(is.na(as.numeric(valormu0))){
      valormu0="0.0"
      errorCondition(recall=contrasteHipotesisMediasIndependientes, message=gettext("No valid value for the Null Hypothesis",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{ valormu0<-as.numeric(valormu0)}

    varHAlternativa<-tclvalue(alternativeVariable)

####

    putDialog ("contrasteHipotesisMediasIndependientes", list(initial.var=varHCMedia,initial.varianza="2",initial.valorvarianzaconocida1=valorsigma1,initial.valorvarianzaconocida2=valorsigma2,initial.nconf=valornConfianza,
                                                              initial.alternative = varHAlternativa,initial.mu = valormu0))
    closeDialog()


###################### Imprimir la función a llamar por RCommander ###########################################

   .activeDataSet<-ActiveDataSet()
    level1<-tclvalue(varcombo_box2)
    level2<-tclvalue(varcombo_box3)

     vHCGrupos<-paste(.activeDataSet,"$",varHCMediasIndependientes, sep="")

    vLevelGrupos<-paste("c(",'"',tclvalue(varcombo_box2),'"',",",'"',tclvalue(varcombo_box3),'"',")",sep="")
    vVariable<-paste(.activeDataSet,"$",varHCMedia, sep="")
  if(tipoVarianza=="1"){ valorVarianza<-paste("c(",valorsigma1,",",valorsigma2,")",sep="")}
  else{valorVarianza=NA}

  Haltern<-paste('"',varHAlternativa,'"',sep="")

 # command<- paste("calcular_HCMediasIndependientes(grupo =", vHCGrupos,", levels.grupo =", vLevelGrupos,", variable =", vVariable,", tipovarianza=", tipoVarianza,", valorVarianzaConocida=", valorVarianza,", hipotesis.alternativa=",Haltern,", hipotesis.nula=",valormu0,", nivel.confianza=",valornConfianza,")",sep="" )
 #  doItAndPrint(command)

  command2<- paste("local({\n","var1<-subset(",.activeDataSet,", ", varHCMediasIndependientes,"==",'"',level1,'"',", select=",varHCMedia,")",sep="")
  command2<- paste(command2,"\n","var2<-subset(",.activeDataSet,", ", varHCMediasIndependientes,"==",'"',level2,'"',", select=",varHCMedia,")\n",sep="")

  if(tipoVarianza=="1"){
    command2<-paste(command2, "aux<-DMKV.test(var1, var2, sdx=",sqrt(valorsigma1),", sdy=", sqrt(valorsigma2), ", difmu=",valormu0,", alternative=",Haltern,", conf.level=",valornConfianza,")\n",sep="")
    tipointervalo<-paste('\\n',gettext("Hypothesis Testing for the mean difference of two independent samples with known variances",domain="R-RcmdrPlugin.TeachStat"),' (Sigma^2_1= ', valorsigma1,", Sigma^2_2= ",valorsigma2,')\\n', sep="")
  } else if(tipoVarianza=="2"){
    command2<-paste(command2, "aux<-t.test(var1, var2, var.equal = TRUE",", mu=",valormu0,", alternative=",Haltern,", conf.level=",valornConfianza,")\n",sep="" )
    tipointervalo<-paste('\\n',gettext("Hypothesis Testing for the mean difference of two independent samples with unknown and equal variances",domain="R-RcmdrPlugin.TeachStat"),'\\n', sep="")
  } else {
    command2<-paste(command2, "aux<-t.test(var1, var2",", mu=",valormu0,", alternative=",Haltern,", conf.level=",valornConfianza,")\n",sep="" )
    tipointervalo<-paste('\\n',gettext("Hypothesis Testing for the mean difference of two independent samples with unknown and unequal variances",domain="R-RcmdrPlugin.TeachStat"),'\\n', sep="")
  }


  
  linaux<-paste(rep(c("-"," "),(nchar(tipointervalo)/2)),collapse="")
  tipointervalo<-paste(tipointervalo, linaux,sep="")
  command2<- paste(command2,"\n",sep="")

  resultado<-paste('cat("',tipointervalo,'\\n',gettext("Variable",domain="R-RcmdrPlugin.TeachStat"),': ', varHCMedia,'\\n',gettext("Groups",domain="R-RcmdrPlugin.TeachStat"),': ',varHCMediasIndependientes,' [',level1,' vs. ', level2 ,']\\n")', sep="" )
  distribucion<-paste('\n if(names(aux$statistic)=="z"){cat("',gettext("Distribution",domain="R-RcmdrPlugin.TeachStat"),':","Normal(0,1)\\n")}
                      else {cat("',gettext("Distribution",domain="R-RcmdrPlugin.TeachStat"),':",names(aux$statistic),"',gettext("with",domain="R-RcmdrPlugin.TeachStat"),'",as.numeric(aux$parameter),"',gettext("degrees of freedom",domain="R-RcmdrPlugin.TeachStat"),'\\n")}',sep="")

  #distribucion<-'\n cat("Distribuci?n:",names(aux$statistic),"con",as.numeric(aux$parameter[1]),"grados de libertad\\n")'
  e.contraste<- paste('\n cat("',gettext("Test statistics value",domain="R-RcmdrPlugin.TeachStat"),':",as.numeric(aux$statistic),"\\n")',sep="")
  
  p.valor<-paste('\n if(aux$p.value>0.05){cat("',gettext("p-value",domain="R-RcmdrPlugin.TeachStat"),':",format.pval(aux$p.value),"\\n")}
                   if(aux$p.value>=0.025 && aux$p.value<=0.05){cat("',gettext("p-value",domain="R-RcmdrPlugin.TeachStat"),':",format.pval(aux$p.value),"*\\n")}
                 if(aux$p.value>=0.0001 && aux$p.value<=0.025){cat("',gettext("p-value",domain="R-RcmdrPlugin.TeachStat"),':",format.pval(aux$p.value),"**\\n")}
                 if(aux$p.value>=0 && aux$p.value<=0.0001){cat("',gettext("p-value",domain="R-RcmdrPlugin.TeachStat"),':",format.pval(aux$p.value),"***\\n")}\n',sep="")

  if (varHAlternativa == "two.sided"){h.alt<-paste(gettext("Population difference is not equal to",domain="R-RcmdrPlugin.TeachStat"),valormu0,sep=" ")}
  if (varHAlternativa == "less"){h.alt<-paste(gettext("Population difference is less than",domain="R-RcmdrPlugin.TeachStat"),valormu0,sep=" ")}
  if (varHAlternativa == "greater"){h.alt<- paste(gettext("Population difference is greater than",domain="R-RcmdrPlugin.TeachStat"),valormu0,sep=" ")}
  h.alt<-paste('\n cat("',gettext("Alternative hypothesis",domain="R-RcmdrPlugin.TeachStat"),':","',h.alt,'","\\n")',sep="")

  e.muestral<-paste('\n cat("',gettext("Sample estimates",domain="R-RcmdrPlugin.TeachStat"),':",names(aux$estimate)[1],as.numeric(aux$estimate)[1],",", names(aux$estimate)[2],as.numeric(aux$estimate)[2],"\\n")',sep="")
  command2<- paste(command2, resultado, distribucion,e.contraste,p.valor, h.alt,e.muestral,"\n})",sep="" )

  doItAndPrint(command2)


###############################################################################################################

   tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "intervaloConfianzaMediasIndependientes", reset="contrasteHipotesisMediasIndependientes", apply="contrasteHipotesisMediasIndependientes")

  tkgrid(variablenumericaFrame,labelRcmdr(selectFactorsFrame, text="          "),comboBoxFrame,labelRcmdr(selectFactorsFrame, text="          "),radioButtonsFrame, sticky="nw")
  tkgrid(selectFactorsFrame, sticky="nw")

  tkgrid(labelRcmdr(top, text="          "))

  ###
  tkgrid(labelRcmdr(rightFrame, text="        "),sticky="nw")
  tkgrid(muFrame,sticky="nw")
  tkgrid(nConfianzaFrame, sticky="nw")
  tkgrid(alternativeFrame,labelRcmdr(optionsFrame, text="          "),rightFrame, sticky="nw")
  tkgrid(optionsFrame,sticky="nw")
  ###

  tkgrid(buttonsFrame, sticky="w")


  dialogSuffix()
}




