intervaloConfianzaMediasIndependientes <- function () {
  #invisible(library(tcltk2))
  
  dialogName <- "intervaloConfianzaMediasIndependientes"
  
  defaults <- list (initial.var=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),
                    initial.varfact=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),
                    initial.varlevel1=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),
                    initial.varlevel2=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),
                    initial.varianza="2",initial.valorvarianzaconocida1="",initial.valorvarianzaconocida2="",initial.nconf="0.95",
                    initial.alternative = "two.sided")
  dialog.values <- getDialog (dialogName, defaults)
  initializeDialog(title = gettext("Confidence Interval for the difference of independent means",domain="R-RcmdrPlugin.TeachStat"))
  
  selectFrame<-tkframe(top)
  
  variablenumericaFrame<-tkframe(selectFrame)
  selectVariableICMedia <- variableComboBox(variablenumericaFrame, variableList=Numeric(),
                                            initialSelection=dialog.values$initial.var, 
                                            title=gettext("Variable (pick one)",domain="R-RcmdrPlugin.TeachStat"))
  
  
  ##Creación de los ComboBox
  
  
  comboBoxFrame<-tkframe(selectFrame)
  
  twoOrMoreLevelFactors<-twoOrMoreLevelFactors() ##NULL SI NO ACTIVEDATASET
  
  if (length(twoOrMoreLevelFactors())!=0){
    mostrar<-"readonly"
  }else {
    mostrar<-"disabled"
  }
  
  
  valuescombo_box<-c(gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),twoOrMoreLevelFactors())
  #varcombo_box<-tclVar(valuescombo_box[1])
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
  
  #varcombo_box2=tclVar(gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"))
  varcombo_box2<-tclVar(dialog.values$initial.varlevel1)
  combo_box2<-ttkcombobox(comboBoxFrame,values=valuescombo_box2,textvariable=varcombo_box2,state=mostrar)
  
  varcombo_box3<-tclVar(dialog.values$initial.varlevel2)
  combo_box3<-ttkcombobox(comboBoxFrame,values=valuescombo_box3,textvariable=varcombo_box3,state=mostrar)
  
  
  
  ##Fin creación comboBox
  
  #   variablenumericaFrame<-tkframe(top)
  #   selectVariableICMedia <- variableComboBox(variablenumericaFrame, variableList=Numeric(),
  #                                             initialSelection=dialog.values$initial.var, title=gettext("Variable (pick one)",domain="R-RcmdrPlugin.TeachStat"))
  #   tkgrid(getFrame(selectVariableICMedia), sticky="nw")
  
  
  ###Inicio Varianza Conocida
  
  radioButtonsFrame<-tkframe(selectFrame)
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
  if(dialog.values$initial.varianza=="1"){
    mostrar<-"normal"
  } else{
    mostrar<-"disabled"
  }
  
  Etiqueta2<-labelRcmdr(radioButtonsFrame, text="   Sigma^2_1",foreground=getRcmdr("title.color"))
  Etiqueta3<-labelRcmdr(radioButtonsFrame, text="   Sigma^2_2",foreground=getRcmdr("title.color"))
  varianzaconocida1 <- tclVar(dialog.values$initial.valorvarianzaconocida1)
  varianzaEntry1 <- ttkentry(radioButtonsFrame, width="10", textvariable=varianzaconocida1, state=mostrar)
  varianzaconocida2 <- tclVar(dialog.values$initial.valorvarianzaconocida2)
  varianzaEntry2 <- ttkentry(radioButtonsFrame, width="10", textvariable=varianzaconocida2, state=mostrar)
  
  
  
  #### Fin Radio Buttons
  
  
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
  tkgrid(labelRcmdr(nConfianzaFrame, text=gettext("Confidence level:",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ),nConfianzaEntry, sticky="nw")
  
  onOK <- function(){
    dialogNameF <- get(dialogName,mode="function")
    
    varICMediasIndependientes<-tclvalue(varcombo_box)
    if(varICMediasIndependientes==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){
      errorCondition(recall=dialogNameF, message=gettext("No group selected",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    if ((length(twoOrMoreLevelFactors)!=0)&&
        (varICMediasIndependientes !=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"))){
      varGrupo1<-tclvalue(varcombo_box2)
      varGrupo2<-tclvalue(varcombo_box3)
    } else{
      varGrupo1<-gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")
      varGrupo2<-gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")
    }
    
    
    varICMedia<-getSelection(selectVariableICMedia)
    if(varICMedia==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){errorCondition(recall=dialogNameF, message=gettext("No variable selected",domain="R-RcmdrPlugin.TeachStat"))
      return()}
    
    if(varGrupo1==varGrupo2){errorCondition(recall=dialogNameF, message=gettext("Group 1 and Group 2 must not coincide",domain="R-RcmdrPlugin.TeachStat"))
      return()}
    
    tipoVarianza <- tclvalue(varianzaVariable)
    if(tipoVarianza=="1"){valorsigma1<-tclvalue(varianzaconocida1)
    valorsigma2<-tclvalue(varianzaconocida2)
    if(is.na(as.numeric(valorsigma1)) || (as.numeric(valorsigma1)<=0)||is.na(as.numeric(valorsigma2)) || (as.numeric(valorsigma2)<=0) ){
      valorsigma1=""
      valorsigma2=""
      errorCondition(recall=dialogNameF, message=gettext("Known variances values must be positive numbers",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{valorsigma1<-as.numeric(valorsigma1);valorsigma2<-as.numeric(valorsigma2)}
    }else{valorsigma1<-"";valorsigma2<-""}
    
    ### Tipo de Intervalo
    varTipo <- tclvalue(tipoVariable)
    
    
    valornConfianza<-tclvalue(nConfianzaVar)
    
    if(is.na(as.numeric(valornConfianza)) || (as.numeric(valornConfianza)<0)||(as.numeric(valornConfianza)>1)) {
      valornConfianza=0.95
      errorCondition(recall=dialogNameF, message=gettext("Confidence level must be between 0 and 1",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{valornConfianza<-as.numeric(valornConfianza)}
    
    
    putDialog (dialogName, list(initial.var=varICMedia,
                                initial.varfact=varICMediasIndependientes,
                                initial.varlevel1=varGrupo1,
                                initial.varlevel2=varGrupo2,
                                initial.varianza=tipoVarianza,
                                initial.valorvarianzaconocida1=valorsigma1,
                                initial.valorvarianzaconocida2=valorsigma2,
                                initial.nconf=valornConfianza,
                                initial.alternative=varTipo))
    
    closeDialog()
    
    
    ###################### Imprimir la función a llamar por RCommander ###########################################
    
    .activeDataSet<-ActiveDataSet()
    level1<-tclvalue(varcombo_box2)
    level2<-tclvalue(varcombo_box3)
    
    
    vICGrupos<-paste(.activeDataSet,"$",varICMediasIndependientes, sep="")
    
    vLevelGrupos<-paste("c(",'"',tclvalue(varcombo_box2),'"',",",'"',tclvalue(varcombo_box3),'"',")",sep="")
    vVariable<-paste(.activeDataSet,"$",varICMedia, sep="")
    if(tipoVarianza=="1"){ valorVarianza<-paste("c(",valorsigma1,",",valorsigma2,")",sep="")}
    else{valorVarianza=NA}
    
    
    # command<- paste("calcular_ICMediasIndependientes(grupo =", vICGrupos,", levels.grupo =", vLevelGrupos,", variable =", vVariable,", tipovarianza=", tipoVarianza,", valorVarianzaConocida=", valorVarianza,", nivel.confianza=",valornConfianza,")",sep="" )
    # doItAndPrint(command)
    
    
    command2<- paste("local({\n","var1<-subset(",.activeDataSet,", ", varICMediasIndependientes,"==",'"',level1,'"',", select=",varICMedia,")",sep="")
    command2<- paste(command2,"\n","var2<-subset(",.activeDataSet,", ", varICMediasIndependientes,"==",'"',level2,'"',", select=",varICMedia,")",sep="")
    
    
    if(tipoVarianza=="1"){
      command2<-paste(command2,"\n","aux<- DMKV.test(var1, var2, sdx=",sqrt(valorsigma1),", sdy=", sqrt(valorsigma2), ", conf.level=",valornConfianza,
                      ", alternative='",varTipo,"')",sep="")
      tipointervalo<-paste('\\n',gettext("Confidence Interval for the mean difference of two independent samples with known variances",domain="R-RcmdrPlugin.TeachStat"),' (Sigma^2_1= ', valorsigma1,", Sigma^2_2= ",valorsigma2,')\\n', sep="")
    } else if(tipoVarianza=="2"){
      command2<-paste(command2,"\n","aux<- t.test(var1, var2, var.equal = TRUE, conf.level=",valornConfianza,
                      ", alternative='",varTipo,"')",sep="" )
      tipointervalo<-paste('\\n',gettext("Confidence Interval for the mean difference of two independent samples with unknown and equal variances",domain="R-RcmdrPlugin.TeachStat"),'\\n', sep="")
    } else{
      command2<-paste(command2,"\n","aux<- t.test(var1, var2, conf.level=",valornConfianza,
                      ", alternative='",varTipo,"')",sep="" )
      tipointervalo<-paste('\\n',gettext("Confidence Interval for the mean difference of two independent samples with unknown and unequal variances",domain="R-RcmdrPlugin.TeachStat"),'\\n', sep="")
    }
    
    
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
    command2<- paste(command2, 'cat("',gettext("Interval type",domain="R-RcmdrPlugin.TeachStat"),': ',typeinterval,'\\n")\n',sep="")
    command2<- paste(command2, 'cat("',gettext("Confidence level",domain="R-RcmdrPlugin.TeachStat"),': ', valornConfianza*100,'%\\n")\n',sep="" )
    command2<- paste(command2, 'cat("',gettext("Variable",domain="R-RcmdrPlugin.TeachStat"),': ', varICMedia,'\\n")\n',sep="" )
    command2<- paste(command2, 'cat("',gettext("Groups",domain="R-RcmdrPlugin.TeachStat"),': ',varICMediasIndependientes,' [',level1,' vs. ', level2 ,']\\n")\n',sep="" )
    
    
    # resultado<-paste('cat("',tipointervalo, '\\n',
    #                  gettext("Confidence level",domain="R-RcmdrPlugin.TeachStat"),': ' , valornConfianza*100,'%\\n',
    #                  gettext("Variable",domain="R-RcmdrPlugin.TeachStat"),': ', varICMedia,'\\n',
    #                  gettext("Groups",domain="R-RcmdrPlugin.TeachStat"),': ',varICMediasIndependientes,' [',level1,' vs. ', level2 ,']\\n")', sep="" )
    # command2<- paste(command2, resultado,"\n",sep="" )
    command2<-paste(command2,'cat("',gettext("Sample estimates",domain="R-RcmdrPlugin.TeachStat"),':",names(aux$estimate)[1],as.numeric(aux$estimate)[1],",", names(aux$estimate)[2],as.numeric(aux$estimate)[2],"\\n")\n',sep="")
    command2<-paste(command2,'cat("',gettext("Interval",domain="R-RcmdrPlugin.TeachStat"),': (",aux2[1],",",aux2[2],")\\n")\n})',sep="")
    
    
    doItAndPrint(command2)
    
    ###############################################################################################################
    
    tkfocus(CommanderWindow())
  }
  
  OKCancelHelp(helpSubject = dialogName, reset=dialogName, apply=dialogName)
  
  #   tkgrid(comboBoxFrame,labelRcmdr(selectFrame, text="          "),variablenumericaFrame,labelRcmdr(selectFrame, text="          "),radioButtonsFrame, sticky="nw")
  #   tkgrid(labelRcmdr(top, text="          "))
  #   tkgrid(nConfianzaFrame, sticky="nw")
  #   
  #   tkgrid(buttonsFrame, sticky="w")
  
  tkgrid(getFrame(selectVariableICMedia), sticky="nw")
  
  tkgrid(labelRcmdr(comboBoxFrame, text=gettext("Groups (pick one)",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ), sticky="nw")
  tkgrid(combo_box,sticky="nw")
  
  tkgrid(labelRcmdr(comboBoxFrame, text=gettext("Group 1",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color")  ), sticky="nw")
  tkgrid(combo_box2, sticky="nw")
  tkgrid(labelRcmdr(comboBoxFrame, text=gettext("Group 2",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color")  ), sticky="nw")
  tkgrid(combo_box3, sticky="nw")
  
  tkgrid(Etiqueta,Etiqueta2,Etiqueta3,sticky="nw" )
  tkgrid(varianzaFrame , varianzaEntry1, varianzaEntry2, sticky="nw")
  
  tkgrid(tipoFrame , sticky="nw")
  
  
  tkgrid(variablenumericaFrame,labelRcmdr(selectFrame, text="          "),comboBoxFrame,
         labelRcmdr(selectFrame, text="          "),radioButtonsFrame, sticky="nw")
  tkgrid(selectFrame, sticky="nw")
  
  tkgrid(labelRcmdr(top, text="          "))
  tkgrid(nConfianzaFrame, 
         labelRcmdr(nconftypeFrame, text="          "),radioButtonstypeFrame, sticky="nw")
  tkgrid(nconftypeFrame, sticky="w")
  
  #tkgrid(buttonsFrame, sticky="w")
  
  
  dialogSuffix(grid.buttons=TRUE)
}


