contrasteHipotesis2Varianzas <- function () {
 #invisible(library(tcltk2))

  defaults <- list (initial.var=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),initial.nconf="0.95",initial.alternative = "two.sided",initial.sigma = "1")
  dialog.values <- getDialog ("contrasteHipotesis2Varianzas", defaults)
  initializeDialog(title = gettext("Hypothesis Testing for the ratio of two variances",domain="R-RcmdrPlugin.TeachStat"))

##Creaci?n de los ComboBox

  selectFactorsFrame<-tkframe(top)
  
  variablenumericaFrame<-tkframe( selectFactorsFrame)
  selectVariableICMedia <- variableComboBox(variablenumericaFrame, variableList=Numeric(),
                                            initialSelection=dialog.values$initial.var, title=gettext("Variable (pick one)",domain="R-RcmdrPlugin.TeachStat"))
  tkgrid(getFrame(selectVariableICMedia), sticky="nw")
  
  comboBoxFrame<-tkframe(selectFactorsFrame)

  twoOrMoreLevelFactors<-twoOrMoreLevelFactors() ##NULL SI NO ACTIVEDATASET

  if (length(twoOrMoreLevelFactors())!=0){
    mostrar<-"readonly"
  }else {
    mostrar<-"disabled"
  }


  valuescombo_box<-c(gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),twoOrMoreLevelFactors())
  varcombo_box<-tclVar(valuescombo_box[1])

  combo_box<-ttkcombobox(comboBoxFrame,values=valuescombo_box,textvariable=varcombo_box,state=mostrar)
  tkgrid(labelRcmdr(comboBoxFrame, text=gettext("Groups (pick one)",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ), sticky="nw")
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

  varcombo_box2<-tclVar(gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"))
  combo_box2<-ttkcombobox(comboBoxFrame,values=varcombo_box2,textvariable=varcombo_box2,state="disabled")

  tkgrid(labelRcmdr(comboBoxFrame, text=gettext("Group 1",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ), sticky="nw")
  tkgrid(combo_box2, sticky="nw")

  varcombo_box3<-tclVar(gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"))
  combo_box3<-ttkcombobox(comboBoxFrame,values=varcombo_box3,textvariable=varcombo_box3,state="disabled")

  tkgrid(labelRcmdr(comboBoxFrame, text=gettext("Group 2",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ), sticky="nw")
  tkgrid(combo_box3, sticky="nw")

##Fin creación comboBox




  #### Inicio Contraste Hipotesis Alternativa, Nula y Nivel Confianza (Frame)

  optionsFrame <- tkframe(top)
  radioButtons(optionsFrame, name = "alternative", buttons = c("twosided",
                                                               "less", "greater"), values = c("two.sided", "less", "greater"),
               labels = gettext(c(gettext("Ratio of population variances != sigma^2_1/sigma^2_2",domain="R-RcmdrPlugin.TeachStat"), gettext("Ratio of population variances < sigma^2_1/sigma^2_2",domain="R-RcmdrPlugin.TeachStat"),
                                  gettext("Ratio of population variances > sigma^2_1/sigma^2_2",domain="R-RcmdrPlugin.TeachStat")),domain="R-RcmdrPlugin.TeachStat"), title = gettext("Alternative hypothesis",domain="R-RcmdrPlugin.TeachStat"),
               initialValue = dialog.values$initial.alternative)


  rightFrame<-tkframe(top)

  sigmaFrame <- tkframe(rightFrame)
  sigmaVariable <- tclVar(dialog.values$initial.sigma)
  sigmaField <- ttkentry(sigmaFrame, width = "5", textvariable = sigmaVariable)
  tkgrid(labelRcmdr(sigmaFrame, text=gettext("Null hypothesis: sigma^2_1/sigma^2_2 = ",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ), sigmaField, sticky="nw")

  nConfianzaFrame<-tkframe(rightFrame)
  nConfianzaVar<-tclVar(dialog.values$initial.nconf)
  nConfianzaEntry<-ttkentry(nConfianzaFrame,width="5",textvariable=nConfianzaVar)
 # tkgrid(labelRcmdr(nConfianzaFrame, text="Nivel de Confianza (1-alpha)= ", foreground="blue" ),nConfianzaEntry, sticky="nw")

  #######################################

  onOK <- function(){

    grupoICVarianzas<-tclvalue(varcombo_box)
    if(grupoICVarianzas==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){errorCondition(recall=contrasteHipotesis2Varianzas, message=gettext("No group selected",domain="R-RcmdrPlugin.TeachStat"))
      return()}
    if ((length(twoOrMoreLevelFactors)!=0)&&(grupoICVarianzas !=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"))){
      varGrupo1<-tclvalue(varcombo_box2)
      varGrupo2<-tclvalue(varcombo_box3)}
    else {
      variableICProporcion<-NULL
      varGrupo1<-NULL
      varGrupo2<-NULL
    }


    varICVarianzas<-getSelection(selectVariableICMedia)
    if(varICVarianzas==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){errorCondition(recall=contrasteHipotesis2Varianzas, message=gettext("No variable selected",domain="R-RcmdrPlugin.TeachStat"))
      return()}

    if(varGrupo1==varGrupo2){errorCondition(recall=contrasteHipotesis2Varianzas, message=gettext("Group 1 and Group 2 must not coincide",domain="R-RcmdrPlugin.TeachStat"))
      return()}


    valornConfianza<-tclvalue(nConfianzaVar)

    if(is.na(as.numeric(valornConfianza)) || (as.numeric(valornConfianza)<0)||(as.numeric(valornConfianza)>1)) {
      valornConfianza<-0.95
      errorCondition(recall=contrasteHipotesis2Varianzas, message=gettext("Confidence level must be between 0 and 1",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{valornConfianza<-as.numeric(valornConfianza)}

    valorsigma0<-tclvalue(sigmaVariable)

    if(is.na(as.numeric(valorsigma0))||(as.numeric(valorsigma0)<=0)){
      valorsigma0<-"0.0"
      errorCondition(recall=contrasteHipotesis2Varianzas, message=gettext("Value for the null hypothesis must be a positive number",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{ valorsigma0<-as.numeric(valorsigma0)}

    varHAlternativa<-tclvalue(alternativeVariable)


    putDialog ("contrasteHipotesis2Varianzas", list(initial.var=varICVarianzas,initial.nconf=valornConfianza,initial.alternative = varHAlternativa,initial.sigma = valorsigma0))
    closeDialog()


###################### Imprimir la función a llamar por RCommander ###########################################

   .activeDataSet<-ActiveDataSet()
    level1<-tclvalue(varcombo_box2)
    level2<-tclvalue(varcombo_box3)

     vICGrupos<-paste(.activeDataSet,"$",grupoICVarianzas, sep="")

    vLevelGrupos<-paste("c(",'"',tclvalue(varcombo_box2),'"',",",'"',tclvalue(varcombo_box3),'"',")",sep="")

    vVariable<-paste(.activeDataSet,"$",varICVarianzas, sep="")

    Haltern<-paste('"',varHAlternativa,'"',sep="")


 # command<- paste("calcular_HC2Varianzas(grupo =", vICGrupos,", levels.grupo =", vLevelGrupos,", variable =", vVariable,", hipotesis.alternativa=",Haltern,", hipotesis.nula=",valorsigma0,", nivel.confianza=",valornConfianza,")",sep="" )
 #  doItAndPrint(command)

    command2<- paste("local({\n","var1<-subset(",.activeDataSet,",", grupoICVarianzas,"==",'"',level1,'"',", select=",varICVarianzas,")",sep="")
    command2<- paste(command2,"\n","var2<-subset(",.activeDataSet,",", grupoICVarianzas,"==",'"',level2,'"',", select=",varICVarianzas,")",sep="")

    command2<-paste(command2,"\n","aux<-var.test(var1[,1], var2[,1]",", ratio=",valorsigma0,", alternative=",Haltern ,", conf.level=",valornConfianza,")\n",sep="")

    tipointervalo<-paste("\\n",gettext("Hypothesis testing for the ratio of two variances",domain="R-RcmdrPlugin.TeachStat")," \\n",sep="")
    linaux<-paste(rep(c("-"," "),(nchar(tipointervalo)/2)),collapse="")
    tipointervalo<-paste(tipointervalo, linaux,sep="")

    resultado<-paste('cat("',tipointervalo, '\\n',gettext("Variable",domain="R-RcmdrPlugin.TeachStat"),': ', varICVarianzas,'\\n',gettext("Groups",domain="R-RcmdrPlugin.TeachStat"),': ',grupoICVarianzas,' [',level1,' vs. ', level2 ,']\\n','")', sep="" )

    distribucion<-paste('\n cat("',gettext("Distribution",domain="R-RcmdrPlugin.TeachStat"),':",names(aux$statistic),"',gettext("with",domain="R-RcmdrPlugin.TeachStat"),'",as.numeric(aux$parameter[1]),"',gettext("and",domain="R-RcmdrPlugin.TeachStat"),'", as.numeric(aux$parameter[2]),"',gettext("degrees of freedom",domain="R-RcmdrPlugin.TeachStat"),'\\n")',sep="")
    e.contraste<- paste('\n cat("',gettext("Test statistics value",domain="R-RcmdrPlugin.TeachStat"),':",as.numeric(aux$statistic),"\\n")',sep="")

    p.valor<-paste('\n if(aux$p.value>0.05){cat("',gettext("p-value",domain="R-RcmdrPlugin.TeachStat"),':",format.pval(aux$p.value),"\\n")}
    if(aux$p.value>=0.025 && aux$p.value<=0.05){cat("',gettext("p-value",domain="R-RcmdrPlugin.TeachStat"),':",format.pval(aux$p.value),"*\\n")}
    if(aux$p.value>=0.0001 && aux$p.value<=0.025){cat("',gettext("p-value",domain="R-RcmdrPlugin.TeachStat"),':",format.pval(aux$p.value),"**\\n")}
    if(aux$p.value>=0 && aux$p.value<=0.0001){cat("',gettext("p-value",domain="R-RcmdrPlugin.TeachStat"),':",format.pval(aux$p.value),"***\\n")}\n',sep="")


    if (varHAlternativa == "two.sided"){h.alt<-paste(gettext("Ratio of population variances is not equal to ",domain="R-RcmdrPlugin.TeachStat"),valorsigma0,sep="")}
    if (varHAlternativa == "less"){h.alt<-paste(gettext("Ratio of population variances is less than ",domain="R-RcmdrPlugin.TeachStat"),valorsigma0,sep="")}
    if (varHAlternativa == "greater"){h.alt<- paste(gettext("Ratio of population variances is greater than ",domain="R-RcmdrPlugin.TeachStat"),valorsigma0,sep="")}
    h.alt<-paste('\n cat("',gettext("Alternative hypothesis",domain="R-RcmdrPlugin.TeachStat"),':","',h.alt,'","\\n")',sep="")

    e.muestral<-paste('\n cat("',gettext("Sample estimate",domain="R-RcmdrPlugin.TeachStat"),':",names(aux$estimate),as.numeric(aux$estimate),"\\n")',sep="")
    command2<- paste(command2, resultado, distribucion,e.contraste,p.valor, h.alt,e.muestral,"\n})",sep="" )

    doItAndPrint(command2)

    ###############################################################################################################

   tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "var.test", reset="contrasteHipotesis2Varianzas", apply="contrasteHipotesis2Varianzas")

  tkgrid(variablenumericaFrame,labelRcmdr(selectFactorsFrame, text="          "),comboBoxFrame, sticky="nw")
  tkgrid(selectFactorsFrame, sticky="nw")

  tkgrid(labelRcmdr(top, text="          "))
  ###
  tkgrid(labelRcmdr(rightFrame, text="        "),sticky="nw")
  tkgrid(sigmaFrame,sticky="nw")
  tkgrid(nConfianzaFrame, sticky="nw")
  tkgrid(alternativeFrame,labelRcmdr(optionsFrame, text="          "),rightFrame, sticky="nw")
  tkgrid(optionsFrame,sticky="nw")
  ###

  tkgrid(buttonsFrame, sticky="w")


  dialogSuffix()
}




