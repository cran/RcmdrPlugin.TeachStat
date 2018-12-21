intervaloConfianza2Varianzas <- function () {
 #invisible(library(tcltk2))

  defaults <- list (initial.var=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),initial.nconf="0.95")
  dialog.values <- getDialog ("intervaloConfianza2Varianzas", defaults)
  initializeDialog(title = gettext("Confidence Interval for the ratio of two variances",domain="R-RcmdrPlugin.TeachStat"))

  ##Creación de los ComboBox
  
  selectFactorsFrame<-tkframe(top)
  
  variablenumericaFrame<-tkframe(selectFactorsFrame)
  
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
  varcombo_box=tclVar(valuescombo_box[1])

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

  varcombo_box2=tclVar(gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"))
  combo_box2<-ttkcombobox(comboBoxFrame,values=varcombo_box2,textvariable=varcombo_box2,state="disabled")

  tkgrid(labelRcmdr(comboBoxFrame, text=gettext("Group 1",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ), sticky="nw")
  tkgrid(combo_box2, sticky="nw")

  varcombo_box3=tclVar(gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"))
  combo_box3<-ttkcombobox(comboBoxFrame,values=varcombo_box3,textvariable=varcombo_box3,state="disabled")

  tkgrid(labelRcmdr(comboBoxFrame, text=gettext("Group 2",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ), sticky="nw")
  tkgrid(combo_box3, sticky="nw")

##Fin creación comboBox




#### Inicio Nivel Confianza(Frame)
    nConfianzaFrame<-tkframe(top)

  nConfianzaVar<-tclVar(dialog.values$initial.nconf)
  nConfianzaEntry<-ttkentry(nConfianzaFrame,width="5",textvariable=nConfianzaVar)
  tkgrid(labelRcmdr(nConfianzaFrame, text=gettext("Confidence level:",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ),nConfianzaEntry, sticky="nw")

  onOK <- function(){

    grupoICVarianzas<-tclvalue(varcombo_box)
    if(grupoICVarianzas==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){errorCondition(recall=intervaloConfianza2Varianzas, message=gettext("No group selected",domain="R-RcmdrPlugin.TeachStat"))
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
    if(varICVarianzas==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){errorCondition(recall=intervaloConfianza2Varianzas, message=gettext("No variable selected",domain="R-RcmdrPlugin.TeachStat"))
      return()}

    if(varGrupo1==varGrupo2){errorCondition(recall=intervaloConfianza2Varianzas, message=gettext("Group 1 and Group 2 must not coincide",domain="R-RcmdrPlugin.TeachStat"))
      return()}


    valornConfianza<-tclvalue(nConfianzaVar)

    if(is.na(as.numeric(valornConfianza)) || (as.numeric(valornConfianza)<0)||(as.numeric(valornConfianza)>1)) {
      valornConfianza=0.95
      errorCondition(recall=intervaloConfianza2Varianzas, message=gettext("Confidence level must be between 0 and 1",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{valornConfianza<-as.numeric(valornConfianza)}


    putDialog ("intervaloConfianza2Varianzas", list(initial.var=varICVarianzas,initial.nconf=valornConfianza))
    closeDialog()


###################### Imprimir la función a llamar por RCommander ###########################################

   .activeDataSet<-ActiveDataSet()
    level1<-tclvalue(varcombo_box2)
    level2<-tclvalue(varcombo_box3)

     vICGrupos<-paste(.activeDataSet,"$",grupoICVarianzas, sep="")

    vLevelGrupos<-paste("c(",'"',tclvalue(varcombo_box2),'"',",",'"',tclvalue(varcombo_box3),'"',")",sep="")

    vVariable<-paste(.activeDataSet,"$",varICVarianzas, sep="")


#  command<- paste("calcular_IC2Varianzas(grupo =", vICGrupos,", levels.grupo =", vLevelGrupos,", variable =", vVariable,", nivel.confianza=",valornConfianza,")",sep="" )
#  doItAndPrint(command)

    command2<- paste("local({\n","var1<-subset(",.activeDataSet,",", grupoICVarianzas,"==",'"',level1,'"',", select=",varICVarianzas,")",sep="")
    command2<- paste(command2,"\n","var2<-subset(",.activeDataSet,",", grupoICVarianzas,"==",'"',level2,'"',", select=",varICVarianzas,")",sep="")

    command2<-paste(command2,"\n","aux<- var.test(var1[,1], var2[,1], conf.level=",valornConfianza,")",sep="")

    tipointervalo<-paste("\\n",gettext("Confidence Interval for the ratio of two variances",domain="R-RcmdrPlugin.TeachStat")," \\n",sep="")
    linaux<-paste(rep(c("-"," "),(nchar(tipointervalo)/2)),collapse="")
    tipointervalo<-paste(tipointervalo, linaux,sep="")


    command2<- paste(command2,"\n",'aux2<-as.vector(aux[["conf.int"]]) \n',sep="")

    resultado<-paste('cat("',tipointervalo, '\\n',gettext("Confidence level",domain="R-RcmdrPlugin.TeachStat"),': ', valornConfianza*100,'%\\n',gettext("Variable",domain="R-RcmdrPlugin.TeachStat"),': ', varICVarianzas,'\\n',gettext("Groups",domain="R-RcmdrPlugin.TeachStat"),': ',grupoICVarianzas,' [',level1,' vs. ', level2 ,']\\n','")', sep="" )
    command2<- paste(command2, resultado,"\n",sep="" )
    command2<-paste(command2,'cat("',gettext("Sample estimate",domain="R-RcmdrPlugin.TeachStat"),':",names(aux$estimate),as.numeric(aux$estimate),"\\n")',"\n",sep="")

    command2<-paste(command2,'cat("',gettext("Interval",domain="R-RcmdrPlugin.TeachStat"),': (",aux2[1],",",aux2[2],")\\n")',"\n})",sep="")

    doItAndPrint(command2)



###############################################################################################################

   tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "var.test", reset="intervaloConfianza2Varianzas", apply="intervaloConfianza2Varianzas")

  tkgrid(variablenumericaFrame,labelRcmdr(selectFactorsFrame, text="          "),comboBoxFrame, sticky="nw")
  tkgrid(selectFactorsFrame, sticky="nw")
  
  tkgrid(labelRcmdr(top, text="          "))
  tkgrid(nConfianzaFrame, sticky="nw")
  
  tkgrid(buttonsFrame, sticky="w")
  
  dialogSuffix()
}




