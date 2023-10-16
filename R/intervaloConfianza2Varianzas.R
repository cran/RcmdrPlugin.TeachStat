intervaloConfianza2Varianzas <- function () {
  #invisible(library(tcltk2))
  dialogName <- "intervaloConfianza2Varianzas"
  
  defaults <- list (initial.var=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),
                    initial.varfact=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),
                    initial.varlevel1=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),
                    initial.varlevel2=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),
                    initial.nconf="0.95",
                    initial.alternative = "two.sided")
  dialog.values <- getDialog (dialogName, defaults)
  initializeDialog(title = gettext("Confidence Interval for the ratio of two variances",domain="R-RcmdrPlugin.TeachStat"))
  
  ##Creación de los ComboBox
  
  selectFactorsFrame<-tkframe(top)
  
  variablenumericaFrame<-tkframe(selectFactorsFrame)
  
  selectVariableICMedia <- variableComboBox(variablenumericaFrame, variableList=Numeric(),
                                            initialSelection=dialog.values$initial.var, 
                                            title=gettext("Variable (pick one)",domain="R-RcmdrPlugin.TeachStat"))
  
  
  
  
  comboBoxFrame<-tkframe(selectFactorsFrame)
  
  twoOrMoreLevelFactors<-twoOrMoreLevelFactors() ##NULL SI NO ACTIVEDATASET
  
  if (length(twoOrMoreLevelFactors())!=0){
    mostrar<-"readonly"
  }else {
    mostrar<-"disabled"
  }
  
  
  valuescombo_box<-c(gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),twoOrMoreLevelFactors())
  varcombo_box<-tclVar(dialog.values$initial.varfact)
  
  combo_box<-ttkcombobox(comboBoxFrame,values=valuescombo_box,textvariable=varcombo_box,state=mostrar)
  
  datasetactivo <- ActiveDataSet()
  datasetactivo <- get(datasetactivo)
  
  
  tkbind(combo_box, "<<ComboboxSelected>>",function(){
    
    value<-tclvalue(varcombo_box)
    if(value!=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){
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
  
  if(dialog.values$initial.varfact==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){
    mostrar<-"disabled"
    valuescombo_box2<-gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")
    valuescombo_box3<-gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")
  } else{
    mostrar<-"readonly"
    valuescombo_box2<-valuescombo_box3<-levels(datasetactivo[,dialog.values$initial.varfact])
  }
  
  varcombo_box2<-tclVar(dialog.values$initial.varlevel1)
  combo_box2<-ttkcombobox(comboBoxFrame,values=valuescombo_box2,textvariable=varcombo_box2,state=mostrar)
  
  varcombo_box3<-tclVar(dialog.values$initial.varlevel2)
  combo_box3<-ttkcombobox(comboBoxFrame,values=valuescombo_box3,textvariable=varcombo_box3,state=mostrar)
  
  
  ##Fin creación comboBox
  
  nconftypeFrame<-tkframe(top)
  ### Tipo de Intervalo
  radioButtonstypeFrame<-tkframe(nconftypeFrame)
  radioButtons(window = radioButtonstypeFrame , name = "tipo", buttons = c("two.sided", "less","greater"),
               initialValue = dialog.values$initial.alternative,
               title = gettext("Interval type",domain="R-RcmdrPlugin.TeachStat"),
               labels = gettext(c("Two-sided", "Left-sided","Right-sided"),domain="R-RcmdrPlugin.TeachStat"),
  )
  ####
  
  
  #### Inicio Nivel Confianza(Frame)
  nConfianzaFrame<-tkframe(nconftypeFrame)
  
  nConfianzaVar<-tclVar(dialog.values$initial.nconf)
  nConfianzaEntry<-ttkentry(nConfianzaFrame,width="5",textvariable=nConfianzaVar)
  
  onOK <- function(){
    dialogNameF <- get(dialogName,mode="function")
    
    grupoICVarianzas<-tclvalue(varcombo_box)
    if(grupoICVarianzas==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){
      errorCondition(recall=dialogNameF, message=gettext("No group selected",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    if ((length(twoOrMoreLevelFactors)!=0)&&(grupoICVarianzas !=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"))){
      varGrupo1<-tclvalue(varcombo_box2)
      varGrupo2<-tclvalue(varcombo_box3)
    } else{
      varGrupo1<-gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")
      varGrupo2<-gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")
    }
    
    
    varICVarianzas<-getSelection(selectVariableICMedia)
    if(varICVarianzas==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){errorCondition(recall=dialogNameF, message=gettext("No variable selected",domain="R-RcmdrPlugin.TeachStat"))
      return()}
    
    if(varGrupo1==varGrupo2){errorCondition(recall=dialogNameF, message=gettext("Group 1 and Group 2 must not coincide",domain="R-RcmdrPlugin.TeachStat"))
      return()}
    
    ### Tipo de Intervalo
    varTipo <- tclvalue(tipoVariable)
    
    valornConfianza<-tclvalue(nConfianzaVar)
    
    if(is.na(as.numeric(valornConfianza)) || (as.numeric(valornConfianza)<0)||(as.numeric(valornConfianza)>1)) {
      valornConfianza<-0.95
      errorCondition(recall=dialogNameF, message=gettext("Confidence level must be between 0 and 1",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{valornConfianza<-as.numeric(valornConfianza)}
    
    
    putDialog (dialogName, list(initial.var=varICVarianzas,
                                initial.varfact=grupoICVarianzas,
                                initial.varlevel1=varGrupo1,
                                initial.varlevel2=varGrupo2,
                                initial.nconf=valornConfianza,
                                initial.alternative=varTipo))
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
    
    command2<-paste(command2,"\n","aux<- var.test(var1[,1], var2[,1], conf.level=",valornConfianza,
                    ", alternative='",varTipo,"')",sep="")
    
    tipointervalo<-paste("\\n",gettext("Confidence Interval for the ratio of two variances",domain="R-RcmdrPlugin.TeachStat")," \\n",sep="")
    linaux<-paste(rep(c("-"," "),(nchar(tipointervalo)/2)),collapse="")
    #tipointervalo<-paste(tipointervalo, linaux,sep="")
    
    ### Tipo de Intervalo
    typeinterval<-switch(varTipo,
                         two.sided=gettext("Two-sided",domain="R-RcmdrPlugin.TeachStat"),
                         less=gettext("Left-sided",domain="R-RcmdrPlugin.TeachStat"),
                         greater=gettext("Right-sided",domain="R-RcmdrPlugin.TeachStat"))
    
    
    command2<- paste(command2,"\n",'aux2<-as.vector(aux[["conf.int"]]) \n',sep="")
    command2<- paste(command2, 'cat("',tipointervalo, '")\n',sep="" )
    command2<- paste(command2, 'cat("',linaux, '\\n")\n',sep="" )
    command2<-paste(command2,'cat("',gettext("Interval type",domain="R-RcmdrPlugin.TeachStat"),': ',typeinterval,'\\n")\n',sep="")
    command2<- paste(command2, 'cat("',gettext("Confidence level",domain="R-RcmdrPlugin.TeachStat"),': ', valornConfianza*100,'%\\n")\n',sep="" )
    command2<- paste(command2, 'cat("',gettext("Variable",domain="R-RcmdrPlugin.TeachStat"),': ', varICVarianzas,'\\n")\n',sep="" )
    command2<- paste(command2, 'cat("',gettext("Groups",domain="R-RcmdrPlugin.TeachStat"),': ',grupoICVarianzas,' [',level1,' vs. ', level2 ,']\\n")\n',sep="" )
    
    
    # resultado<-paste('cat("',tipointervalo, '\\n',gettext("Confidence level",domain="R-RcmdrPlugin.TeachStat"),': ', valornConfianza*100,'%\\n',
    #                  gettext("Variable",domain="R-RcmdrPlugin.TeachStat"),': ', varICVarianzas,'\\n',
    #                  gettext("Groups",domain="R-RcmdrPlugin.TeachStat"),': ',grupoICVarianzas,' [',level1,' vs. ', level2 ,']\\n','")', sep="" )
    # command2<- paste(command2, resultado,"\n",sep="" )
    command2<-paste(command2,'cat("',gettext("Sample estimate",domain="R-RcmdrPlugin.TeachStat"),':",names(aux$estimate),as.numeric(aux$estimate),"\\n")',"\n",sep="")
    
    command2<-paste(command2,'cat("',gettext("Interval",domain="R-RcmdrPlugin.TeachStat"),': (",aux2[1],",",aux2[2],")\\n")',"\n})",sep="")
    
    doItAndPrint(command2)
    
    
    
    ###############################################################################################################
    
    tkfocus(CommanderWindow())
  }
  
  OKCancelHelp(helpSubject = "var.test", reset=dialogName, apply=dialogName)
  
  tkgrid(getFrame(selectVariableICMedia), sticky="nw")
  
  tkgrid(labelRcmdr(comboBoxFrame, text=gettext("Groups (pick one)",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ), sticky="nw")
  tkgrid(combo_box,sticky="nw")
  
  tkgrid(labelRcmdr(comboBoxFrame, text=gettext("Group 1",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ), sticky="nw")
  tkgrid(combo_box2, sticky="nw")
  
  tkgrid(labelRcmdr(comboBoxFrame, text=gettext("Group 2",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ), sticky="nw")
  tkgrid(combo_box3, sticky="nw")
  
  tkgrid(tipoFrame , sticky="nw")
  
  tkgrid(labelRcmdr(nConfianzaFrame, text=gettext("Confidence level:",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ),nConfianzaEntry, sticky="nw")
  
  
  
  tkgrid(variablenumericaFrame,labelRcmdr(selectFactorsFrame, text="          "),comboBoxFrame, sticky="nw")
  tkgrid(selectFactorsFrame, sticky="nw")
  
  tkgrid(labelRcmdr(top, text="          "))
  tkgrid(nConfianzaFrame, 
         labelRcmdr(nconftypeFrame, text="          "),radioButtonstypeFrame, sticky="nw")
  tkgrid(nconftypeFrame, sticky="w")
  
  dialogSuffix(grid.buttons=TRUE)
}




