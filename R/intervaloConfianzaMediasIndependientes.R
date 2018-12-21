intervaloConfianzaMediasIndependientes <- function () {
 #invisible(library(tcltk2))

  defaults <- list (initial.var=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),initial.varianza="2",initial.valorvarianzaconocida1="",initial.valorvarianzaconocida2="",initial.nconf="0.95")
  dialog.values <- getDialog ("intervaloConfianzaMediasIndependientes", defaults)
  initializeDialog(title = gettext("Confidence Interval for the difference of independent means",domain="R-RcmdrPlugin.TeachStat"))

  selectFactorsFrame<-tkframe(top)
  
  variablenumericaFrame<-tkframe(selectFactorsFrame)
  selectVariableICMedia <- variableComboBox(variablenumericaFrame, variableList=Numeric(),
                                            initialSelection=dialog.values$initial.var, title=gettext("Variable (pick one)",domain="R-RcmdrPlugin.TeachStat"))
  tkgrid(getFrame(selectVariableICMedia), sticky="nw")  
 
##Creación de los ComboBox

 
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

  tkgrid(labelRcmdr(comboBoxFrame, text=gettext("Group 1",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color")  ), sticky="nw")
  tkgrid(combo_box2, sticky="nw")

  varcombo_box3=tclVar(gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"))
  combo_box3<-ttkcombobox(comboBoxFrame,values=varcombo_box3,textvariable=varcombo_box3,state="disabled")

  tkgrid(labelRcmdr(comboBoxFrame, text=gettext("Group 2",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color")  ), sticky="nw")
  tkgrid(combo_box3, sticky="nw")

##Fin creación comboBox

#   variablenumericaFrame<-tkframe(top)
#   selectVariableICMedia <- variableComboBox(variablenumericaFrame, variableList=Numeric(),
#                                             initialSelection=dialog.values$initial.var, title=gettext("Variable (pick one)",domain="R-RcmdrPlugin.TeachStat"))
#   tkgrid(getFrame(selectVariableICMedia), sticky="nw")


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

#### Inicio Nivel Confianza(Frame)
    nConfianzaFrame<-tkframe(top)

  nConfianzaVar<-tclVar(dialog.values$initial.nconf)
  nConfianzaEntry<-ttkentry(nConfianzaFrame,width="5",textvariable=nConfianzaVar)
  tkgrid(labelRcmdr(nConfianzaFrame, text=gettext("Confidence level:",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ),nConfianzaEntry, sticky="nw")

  onOK <- function(){

    varICMediasIndependientes<-tclvalue(varcombo_box)
    if(varICMediasIndependientes==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){errorCondition(recall=intervaloConfianzaMediasIndependientes, message=gettext("No group selected",domain="R-RcmdrPlugin.TeachStat"))
      return()}
    if ((length(twoOrMoreLevelFactors)!=0)&&(varICMediasIndependientes !=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"))){
      varGrupo1<-tclvalue(varcombo_box2)
      varGrupo2<-tclvalue(varcombo_box3)}
    else {
      variableICProporcion<-NULL
      varGrupo1<-NULL
      varGrupo2<-NULL
    }


    varICMedia<-getSelection(selectVariableICMedia)
    if(varICMedia==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){errorCondition(recall=intervaloConfianzaMediasIndependientes, message=gettext("No variable selected",domain="R-RcmdrPlugin.TeachStat"))
      return()}

    if(varGrupo1==varGrupo2){errorCondition(recall=intervaloConfianzaMediasIndependientes, message=gettext("Group 1 and Group 2 must not coincide",domain="R-RcmdrPlugin.TeachStat"))
      return()}

    tipoVarianza <- tclvalue(varianzaVariable)
    if(tipoVarianza=="1"){valorsigma1<-tclvalue(varianzaconocida1)
                          valorsigma2<-tclvalue(varianzaconocida2)
          if(is.na(as.numeric(valorsigma1)) || (as.numeric(valorsigma1)<=0)||is.na(as.numeric(valorsigma2)) || (as.numeric(valorsigma2)<=0) ){
             valorsigma1=""
             valorsigma2=""
             errorCondition(recall=intervaloConfianzaMediasIndependientes, message=gettext("Known variances values must be positive numbers",domain="R-RcmdrPlugin.TeachStat"))
              return()
          }
         else{valorsigma1<-as.numeric(valorsigma1);valorsigma2<-as.numeric(valorsigma2)}
    }else{valorsigma1<-"";valorsigma2<-""}



    valornConfianza<-tclvalue(nConfianzaVar)

    if(is.na(as.numeric(valornConfianza)) || (as.numeric(valornConfianza)<0)||(as.numeric(valornConfianza)>1)) {
      valornConfianza=0.95
      errorCondition(recall=intervaloConfianzaMediasIndependientes, message=gettext("Confidence level must be between 0 and 1",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{valornConfianza<-as.numeric(valornConfianza)}


    putDialog ("intervaloConfianzaMediasIndependientes", list(initial.var=varICMedia,initial.varianza="2",initial.valorvarianzaconocida1=valorsigma1,initial.valorvarianzaconocida2=valorsigma2,initial.nconf=valornConfianza))
    
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
    command2<-paste(command2,"\n","aux<- DMKV.test(var1, var2, sdx=",sqrt(valorsigma1),", sdy=", sqrt(valorsigma2), ", conf.level=",valornConfianza,")",sep="")
    tipointervalo<-paste('\\n',gettext("Confidence Interval for the mean difference of two independent samples with known variances",domain="R-RcmdrPlugin.TeachStat"),' (Sigma^2_1= ', valorsigma1,", Sigma^2_2= ",valorsigma2,')\\n', sep="")
  } else if(tipoVarianza=="2"){
    command2<-paste(command2,"\n","aux<- t.test(var1, var2, var.equal = TRUE, conf.level=",valornConfianza,")",sep="" )
    tipointervalo<-paste('\\n',gettext("Confidence Interval for the mean difference of two independent samples with unknown and equal variances",domain="R-RcmdrPlugin.TeachStat"),'\\n', sep="")
  } else{
    command2<-paste(command2,"\n","aux<- t.test(var1, var2, conf.level=",valornConfianza,")",sep="" )
    tipointervalo<-paste('\\n',gettext("Confidence Interval for the mean difference of two independent samples with unknown and unequal variances",domain="R-RcmdrPlugin.TeachStat"),'\\n', sep="")
  }


  linaux<-paste(rep(c("-"," "),(nchar(tipointervalo)/2)),collapse="")
  tipointervalo<-paste(tipointervalo, linaux,sep="")


  command2<- paste(command2,"\n",'aux2<-as.vector(aux[["conf.int"]]) \n',sep="")

  resultado<-paste('cat("',tipointervalo, '\\n',gettext("Confidence level",domain="R-RcmdrPlugin.TeachStat"),': ' , valornConfianza*100,'%\\n',gettext("Variable",domain="R-RcmdrPlugin.TeachStat"),': ', varICMedia,'\\n',gettext("Groups",domain="R-RcmdrPlugin.TeachStat"),': ',varICMediasIndependientes,' [',level1,' vs. ', level2 ,']\\n")', sep="" )
  command2<- paste(command2, resultado,"\n",sep="" )
  command2<-paste(command2,'cat("',gettext("Sample estimates",domain="R-RcmdrPlugin.TeachStat"),':",names(aux$estimate)[1],as.numeric(aux$estimate)[1],",", names(aux$estimate)[2],as.numeric(aux$estimate)[2],"\\n")\n',sep="")
  command2<-paste(command2,'cat("',gettext("Interval",domain="R-RcmdrPlugin.TeachStat"),': (",aux2[1],",",aux2[2],")\\n")\n})',sep="")


  doItAndPrint(command2)

###############################################################################################################

   tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "intervaloConfianzaMediasIndependientes", reset="intervaloConfianzaMediasIndependientes", apply="intervaloConfianzaMediasIndependientes")

#   tkgrid(comboBoxFrame,labelRcmdr(selectFactorsFrame, text="          "),variablenumericaFrame,labelRcmdr(selectFactorsFrame, text="          "),radioButtonsFrame, sticky="nw")
#   tkgrid(labelRcmdr(top, text="          "))
#   tkgrid(nConfianzaFrame, sticky="nw")
#   
#   tkgrid(buttonsFrame, sticky="w")

  tkgrid(variablenumericaFrame,labelRcmdr(selectFactorsFrame, text="          "),comboBoxFrame,labelRcmdr(selectFactorsFrame, text="          "),radioButtonsFrame, sticky="nw")
  tkgrid(selectFactorsFrame, sticky="nw")

  tkgrid(labelRcmdr(top, text="          "))
  tkgrid(nConfianzaFrame, sticky="nw")

  tkgrid(buttonsFrame, sticky="w")


  dialogSuffix()
}



