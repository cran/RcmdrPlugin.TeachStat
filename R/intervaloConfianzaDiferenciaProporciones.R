intervaloConfianzaDiferenciaProporciones <- function () {
 #invisible(library(tcltk2))

  defaults <- list (initial.nexitos="",initial.nfracasos="",initial.nexitos2="",initial.nfracasos2="",initial.nconf="0.95")
  dialog.values <- getDialog ("intervaloConfianzaDiferenciaProporciones", defaults)
  initializeDialog(title = gettext("Confidence interval for difference of proportions",domain="R-RcmdrPlugin.TeachStat"))

seleccionFrame<-tkframe(top)
selectFactorsFrame<-ttkframe(seleccionFrame, borderwidth=1, relief="solid", padding=c(5,5,8,8))

  ##Creación de los ComboBox 1

  #comboBoxFrame<-ttklabelframe(selectFactorsFrame, text="Variable 1", padding=c(5,5,5,5))
  comboBoxFrame<-tkframe(selectFactorsFrame)
  twoOrMoreLevelFactors<-twoOrMoreLevelFactors() ##NULL SI NO ACTIVEDATASET

  if (length(twoOrMoreLevelFactors())!=0){
    mostrar<-"readonly"
  }else {
    mostrar<-"disabled"
  }


  valuescombo_box<-c(gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),twoOrMoreLevelFactors())
  varcombo_box=tclVar(valuescombo_box[1])

  DatosFrame<-tkframe(comboBoxFrame)
  combo_box<-ttkcombobox(DatosFrame,values=valuescombo_box,textvariable=varcombo_box,state=mostrar)

  Eti<-labelRcmdr(DatosFrame, text=gettext("Groups (pick one)",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color"))
  tkgrid(Eti,sticky="nw")
  tkgrid(combo_box,sticky="nw")
  tkgrid(DatosFrame, sticky="nw")


  tkbind(combo_box, "<<ComboboxSelected>>",function(){

    value<-tclvalue(varcombo_box)
    if(value!=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){
      datasetactivo <- ActiveDataSet()
      datasetactivo<-get(datasetactivo)
      niveles<-levels(datasetactivo[,value])
      tkconfigure(combo_box2,values=niveles)
      tclvalue(varcombo_box2)<-niveles[1]
      tk2state.set(combo_box2, state="readonly")
      tkconfigure(combo_box22,values=niveles)
      tclvalue(varcombo_box22)<-niveles[2]
      tk2state.set(combo_box22, state="readonly")
      tkfocus(combo_box2)}
    else{tk2state.set(combo_box2, state="disabled")
         tk2state.set(combo_box22, state="disabled")
         niveles<-gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")
          tkconfigure(combo_box2,values=niveles)
          tclvalue(varcombo_box2)<-niveles
          tkconfigure(combo_box22,values=niveles)
          tclvalue(varcombo_box22)<-niveles}})

  ExitoFrame<-tkframe(comboBoxFrame)
  varcombo_box2=tclVar(gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"))
  combo_box2<-ttkcombobox(ExitoFrame,values=varcombo_box2,textvariable=varcombo_box2,state="disabled")
  # tkgrid(labelRcmdr(ExitoFrame, text=""),sticky="nw")
  Etiqueta<-labelRcmdr(ExitoFrame, text=gettext("Group 1",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color")  )
  tkgrid(Etiqueta, sticky="nw")
  tkgrid(combo_box2, sticky="nw")
  varcombo_box22=tclVar(gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"))
  combo_box22<-ttkcombobox(ExitoFrame,values=varcombo_box22,textvariable=varcombo_box22,state="disabled")
  # tkgrid(labelRcmdr(ExitoFrame, text=""),sticky="nw")
  Etiqueta<-labelRcmdr(ExitoFrame, text=gettext("Group 2",domain="R-RcmdrPlugin.TeachStat"),foreground=getRcmdr("title.color"))
  tkgrid(Etiqueta, sticky="nw")
  tkgrid(combo_box22, sticky="nw")
  tkgrid(ExitoFrame, sticky="nw")

##Fin creacin comboBox

##Creación de los ComboBox 2

#  comboBoxFrame2<-ttklabelframe(selectFactorsFrame, text="Variable 2", padding=c(5,5,5,5))
  comboBoxFrame2<-tkframe(selectFactorsFrame)
  valuescombo_box3<-c(gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),twoOrMoreLevelFactors())
  varcombo_box3=tclVar(valuescombo_box3[1])

  DatosFrame2<-tkframe(comboBoxFrame2)
  combo_box3<-ttkcombobox(DatosFrame2,values=valuescombo_box3,textvariable=varcombo_box3,state=mostrar)

  Eti3<-labelRcmdr(DatosFrame2, text=gettext("Variable",domain="R-RcmdrPlugin.TeachStat"),foreground=getRcmdr("title.color"))
  tkgrid(Eti3, sticky="nw")
  tkgrid(combo_box3, sticky="nw")
  tkgrid(DatosFrame2, sticky="nw")


  tkbind(combo_box3, "<<ComboboxSelected>>",function(){

    value<-tclvalue(varcombo_box3)
    if(value!=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){
      datasetactivo <- ActiveDataSet()
      datasetactivo<-get(datasetactivo)
      niveles<-levels(datasetactivo[,value])
      tkconfigure(combo_box4,values=niveles)
      tclvalue(varcombo_box4)<-niveles[1]
      tk2state.set(combo_box4, state="readonly")
    }
    else{tk2state.set(combo_box4, state="disabled")
      niveles<-gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")
      tkconfigure(combo_box4,values=niveles)
      tclvalue(varcombo_box4)<-niveles}})

  Exito2Frame<-tkframe(comboBoxFrame2)
  varcombo_box4=tclVar(gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"))
  combo_box4<-ttkcombobox(Exito2Frame,values=varcombo_box4,textvariable=varcombo_box4,state="disabled")
  # tkgrid(labelRcmdr(Exito2Frame, text=""),sticky="nw")
  Etiqueta2<-labelRcmdr(Exito2Frame, text=gettext("Success",domain="R-RcmdrPlugin.TeachStat"),foreground=getRcmdr("title.color"))
  tkgrid(Etiqueta2, sticky="nw")
  tkgrid(combo_box4, sticky="nw")
  tkgrid(Exito2Frame, sticky="nw")


##Fin creación comboBox2



muestrasFrame<-ttkframe(seleccionFrame, borderwidth=1, relief="solid", padding=c(5,5,8,8))
 # muestrasFrame<-ttklabelframe(top, text="Muestras no conocidas", padding=c(5,5,8,8))

## Inico exitos y fracasos Muestra1


  muestra1Frame<-ttklabelframe(muestrasFrame, text=gettext("Sample 1",domain="R-RcmdrPlugin.TeachStat"), padding=c(5,5,5,5))

  exitosFracasosFrame<-tkframe(muestra1Frame)
  nExitosVar<-tclVar(dialog.values$initial.nexitos)
  nExitosEntry<-ttkentry(exitosFracasosFrame,width="5",textvariable=nExitosVar)
  tkgrid(labelRcmdr(exitosFracasosFrame, text=gettext("No. successes",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color")),nExitosEntry, sticky="nw")

  nFracasosVar<-tclVar(dialog.values$initial.nfracasos)
  nFracasosEntry<-ttkentry(exitosFracasosFrame,width="5",textvariable=nFracasosVar)
  tkgrid(labelRcmdr(exitosFracasosFrame, text=gettext("No. failures",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color")),nFracasosEntry, sticky="nw")

#  tkgrid(labelRcmdr(muestra1Frame, text="     Muestra 1   ", foreground="blue" ), sticky="nwe")
  tkgrid(exitosFracasosFrame,sticky="nw")
  tkgrid(muestra1Frame, sticky="nw")

##Fin exitos y fracasos Muestra 1

## Inico exitos y fracasos Muestra 2


  muestra2Frame<-ttklabelframe(muestrasFrame, text=gettext("Sample 2",domain="R-RcmdrPlugin.TeachStat"), padding=c(5,5,5,5))

  exitosFracasosFrame2<-tkframe(muestra2Frame)
  nExitosVar2<-tclVar(dialog.values$initial.nexitos2)
  nExitosEntry2<-ttkentry(exitosFracasosFrame2,width="5",textvariable=nExitosVar2)
  tkgrid(labelRcmdr(exitosFracasosFrame2, text=gettext("No. successes",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ),nExitosEntry2, sticky="nw")
  nFracasosVar2<-tclVar(dialog.values$initial.nfracasos2)
  nFracasosEntry2<-ttkentry(exitosFracasosFrame2,width="5",textvariable=nFracasosVar2)
  tkgrid(labelRcmdr(exitosFracasosFrame2, text=gettext("No. failures",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color")),nFracasosEntry2, sticky="nw")
 # tkgrid(labelRcmdr(muestra1Frame, text="     Muestra 2   ", foreground="blue" ), sticky="nwe")
  tkgrid(exitosFracasosFrame2,sticky="nw")
  tkgrid(muestra2Frame, sticky="nw")

  ##Fin exitos y fracasos Muestra 2

  nConfianzaFrame<-tkframe(top)

  nConfianzaVar<-tclVar(dialog.values$initial.nconf)
  nConfianzaEntry<-ttkentry(nConfianzaFrame,width="5",textvariable=nConfianzaVar)
  tkgrid(labelRcmdr(nConfianzaFrame, text=gettext("Confidence level:",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ),nConfianzaEntry, sticky="nw")

  onOK <- function(){

    varICProporcion<-tclvalue(varcombo_box)
    if ((length(twoOrMoreLevelFactors)!=0)&&(varICProporcion !=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"))){
      variableICProporcion<-tclvalue(varcombo_box)
      variableGrupo1<-tclvalue(varcombo_box2)
      variableGrupo2<-tclvalue(varcombo_box22)}
    else {
      variableICProporcion<-NULL
      variableGrupo1<-NULL
      variableGrupo2<-NULL
    }

    varICProporcion2<-tclvalue(varcombo_box3)
    if ((length(twoOrMoreLevelFactors)!=0)&&(varICProporcion2 !=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"))){
      variableICProporcion2<-tclvalue(varcombo_box3)
      variableLevelICProporcion2<-tclvalue(varcombo_box4)}
    else {
      variableICProporcion2<-NULL
      variableLevelICProporcion2<-NULL
    }




    valornexitos<-tclvalue(nExitosVar)
                if( (valornexitos!="") && (is.na(as.integer(valornexitos)) || (as.integer(valornexitos)<0) || !(isTRUE(all.equal(as.numeric(valornexitos),as.integer(valornexitos)))) )) {
                  valornexitos=""
                  errorCondition(recall=intervaloConfianzaDiferenciaProporciones, message=gettext("Successes number value of Sample 1 must be a positive integer number",domain="R-RcmdrPlugin.TeachStat"))
                  return()
                }
                else{if(valornexitos!=""){valornexitos<-as.integer(valornexitos)}}

    valornfracasos<-tclvalue(nFracasosVar)
    if((valornfracasos!="") && (is.na(as.integer(valornfracasos)) || (as.integer(valornfracasos)<0) || !(isTRUE(all.equal(as.numeric(valornfracasos),as.integer(valornfracasos)))) )){
      valornfracasos=""
      errorCondition(recall=intervaloConfianzaDiferenciaProporciones, message=gettext("Failures number value of Sample 1 must be a positive integer number",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{if(valornfracasos!=""){valornfracasos<-as.integer(valornfracasos)}}


    valornexitos2<-tclvalue(nExitosVar2)
    if( (valornexitos2!="") && (is.na(as.integer(valornexitos2)) || (as.integer(valornexitos2)<0) || !(isTRUE(all.equal(as.numeric(valornexitos2),as.integer(valornexitos2)))) )) {
      valornexitos2=""
      errorCondition(recall=intervaloConfianzaDiferenciaProporciones, message=gettext("Successes number value of Sample 2 must be a positive integer number",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{if(valornexitos2!=""){valornexitos2<-as.integer(valornexitos2)}}

    valornfracasos2<-tclvalue(nFracasosVar2)
    if((valornfracasos2!="") && (is.na(as.integer(valornfracasos2)) || (as.integer(valornfracasos2)<0) || !(isTRUE(all.equal(as.numeric(valornfracasos2),as.integer(valornfracasos2)))) )){
      valornfracasos2=""
      errorCondition(recall=intervaloConfianzaDiferenciaProporciones, message=gettext("Failures number value of Sample 2 must be a positive integer number",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{if(valornfracasos2!=""){valornfracasos2<-as.integer(valornfracasos2)}}


   if ((is.null(variableICProporcion)||is.null(variableICProporcion2)) && ((valornexitos=="")|(valornfracasos=="")|(valornexitos2=="")|(valornfracasos2==""))){
      errorCondition(recall=intervaloConfianzaDiferenciaProporciones, message=gettext("Variables or successes number and failures number of two samples must be selected",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }

    if((!is.null(variableGrupo1)) && (variableGrupo1==variableGrupo2)){errorCondition(recall=intervaloConfianzaDiferenciaProporciones, message=gettext("Group 1 and Group 2 must not coincide",domain="R-RcmdrPlugin.TeachStat"))
      return()}

    sumaexitosfracasos<-sum(as.integer(valornexitos),as.integer(valornfracasos))
    sumaexitosfracasos2<-sum(as.integer(valornexitos2),as.integer(valornfracasos2))

    if ((is.null(variableICProporcion)||is.null(variableICProporcion2))&&(sumaexitosfracasos==0 ||sumaexitosfracasos2==0)){
      errorCondition(recall=intervaloConfianzaDiferenciaProporciones, message=gettext("A variable must be selected or the sum of successes and failures numbers must be positive",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }

    muestrasnulas <- ((valornexitos=="")&(valornfracasos=="")&(valornexitos2=="")&(valornfracasos2==""))
    muestrasrellenas <- ((!valornexitos=="")&(!valornfracasos=="")&(!valornexitos2=="")&(!valornfracasos2==""))

    if (!(is.null(variableICProporcion)||is.null(variableICProporcion2)) && (!(muestrasnulas | muestrasrellenas))){
      errorCondition(recall=intervaloConfianzaDiferenciaProporciones, message=gettext("Successes and failures numbers for the two samples must be selected",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }

      if ((is.null(variableICProporcion) && !is.null(variableICProporcion2)) || (!is.null(variableICProporcion) && is.null(variableICProporcion2))){
      errorCondition(recall=intervaloConfianzaDiferenciaProporciones, message=gettext("Group 1, Group 2 and Success must be selected",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }

    valornConfianza<-tclvalue(nConfianzaVar)

    if(is.na(as.numeric(valornConfianza)) || (as.numeric(valornConfianza)<0)||(as.numeric(valornConfianza)>1)) {
      valornConfianza=0.95
      errorCondition(recall=intervaloConfianzaDiferenciaProporciones, message=gettext("Confidence level must be between 0 and 1",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{valornConfianza<-as.numeric(valornConfianza)}


    putDialog ("intervaloConfianzaDiferenciaProporciones", defaults) #list(initial.nexitos=valornexitos,initial.nfracasos=valornfracasos,initial.nexitos2=valornexitos2,initial.nfracasos2=valornfracasos2,initial.nconf="0.95"))
    closeDialog()

   ##variableICProporcion TwoLevelFactoSeleccionado o nada
    valornexitos<-as.integer(valornexitos)
    valornfracasos<-as.integer(valornfracasos)
    valornexitos2<-as.integer(valornexitos2)
    valornfracasos2<-as.integer(valornfracasos2)
    valornConfianza<-as.numeric(valornConfianza)

###################### Imprimir la función a llamar por RCommander ###########################################

    .activeDataSet<-ActiveDataSet()

    if ((length(twoOrMoreLevelFactors)!=0)&&(varICProporcion !=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"))&&(varICProporcion2 !=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"))){

      vICProporcion<-paste(.activeDataSet,"$",varICProporcion, sep="")
      vICProporcion2<-paste(.activeDataSet,"$",varICProporcion2, sep="")
      vGrupo1<-paste('"',tclvalue(varcombo_box2),'"',sep="")
      vGrupo2<-paste('"',tclvalue(varcombo_box22),'"',sep="")
      vVariable<-paste('"',tclvalue(varcombo_box4),'"',sep="")}
    else {
      vICProporcion<-NULL
      vICProporcion2<-NULL
      vGrupo1<-NULL
      vGrupo2<-NULL
      vVariable<-NULL
    }
    
    tipointervalo<-paste('\\n',gettext("Confidence interval for difference of proportions",domain="R-RcmdrPlugin.TeachStat"),'\\n', sep="")
    linaux<-paste(rep(c("-"," "),(nchar(tipointervalo)/2)),collapse="")
    tipointervalo<-paste(tipointervalo, linaux,sep="")

    if(!is.null(vICProporcion)){

      command<- paste("exitos<-sum(",vICProporcion," == ", vGrupo1," & ",vICProporcion2,"==",vVariable,",na.rm = TRUE)",sep="")
      command<- paste(command,"\n","total<-sum(",vICProporcion," == ", vGrupo1,",na.rm = TRUE)",sep="")
      command<- paste(command,"\n","exitos2<-sum(",vICProporcion," == ", vGrupo2," & ",vICProporcion2,"==",vVariable,",na.rm = TRUE)",sep="")
      command<- paste(command,"\n","total2<-sum(",vICProporcion," == ", vGrupo2,",na.rm = TRUE)",sep="")
      command<- paste(command,"\n","aux<- Cprop.test(ex=exitos, nx=total,ey=exitos2,ny=total2, conf.level=",valornConfianza,")",sep="")
      


      command<- paste("local({\n",command,"\n",'aux2<-as.vector(aux[["conf.int"]]) \n',sep="")

      resultado<-paste('cat("',tipointervalo, '\\n',gettext("Confidence level",domain="R-RcmdrPlugin.TeachStat"),': ' , valornConfianza*100,'%\\n',gettext("Variable",domain="R-RcmdrPlugin.TeachStat"),': ', varICProporcion2,'[ ',tclvalue(varcombo_box4),' vs. ', gettext("Other levels",domain="R-RcmdrPlugin.TeachStat"), ' ]','\\n',gettext("Group 1",domain="R-RcmdrPlugin.TeachStat"),': ', varICProporcion,' [ ',tclvalue(varcombo_box2),' ] --> ',gettext("No. successes",domain="R-RcmdrPlugin.TeachStat"),' = ",exitos," -- ',gettext("No. attempts",domain="R-RcmdrPlugin.TeachStat"),' =", total',',"\\n',gettext("Group 2",domain="R-RcmdrPlugin.TeachStat"),': ","', varICProporcion,' [ ',tclvalue(varcombo_box22),' ] --> ',gettext("No. successes",domain="R-RcmdrPlugin.TeachStat"),' = ",exitos2," -- ',gettext("No. attempts",domain="R-RcmdrPlugin.TeachStat"),' =", total2,"\\n")', sep="" )
      command<- paste(command, resultado,"\n",sep="" )
      command<-paste(command,'cat("',gettext("Sample estimates",domain="R-RcmdrPlugin.TeachStat"),':",names(aux$estimate)[1],as.numeric(aux$estimate)[1],",", names(aux$estimate)[2],as.numeric(aux$estimate)[2],"\\n")',"\n",sep="")
      command<-paste(command,'cat("',gettext("Interval",domain="R-RcmdrPlugin.TeachStat"),': (",aux2[1],",",aux2[2],")\\n")',"\n})",sep="")
      doItAndPrint(command)
    }

    if(!is.na(valornexitos)){
      if(valornexitos*(valornfracasos)*valornexitos2*(valornfracasos2)<1){
        errorCondition(recall=contrastHipotesisProporcion, message=gettext("There must be at least 1 success and 1 failure for both samples",domain="R-RcmdrPlugin.TeachStat"))
        return()
      }
      
      command<- paste("aux<- Cprop.test(ex=",valornexitos,", nx=",valornexitos + valornfracasos,", ey=",valornexitos2,", ny=",valornexitos2 + valornfracasos2,", conf.level=",valornConfianza,")",sep="")
      
      command<- paste("local({\n",command,"\n",'aux2<-as.vector(aux[["conf.int"]]) \n',sep="")

      resultado<-paste('cat("',tipointervalo, '\\n',gettext("Confidence level",domain="R-RcmdrPlugin.TeachStat"),': ' , valornConfianza*100,'%\\n',gettext("Sample 1",domain="R-RcmdrPlugin.TeachStat"),': ',gettext("No. successes",domain="R-RcmdrPlugin.TeachStat"),' = ', valornexitos ,' -- ',gettext("No. attempts",domain="R-RcmdrPlugin.TeachStat"),' = ', valornexitos + valornfracasos,'\\n',gettext("Sample 2",domain="R-RcmdrPlugin.TeachStat"),': ',gettext("No. successes",domain="R-RcmdrPlugin.TeachStat"),' = ', valornexitos2 ,' -- ',gettext("No. attempts",domain="R-RcmdrPlugin.TeachStat"),' = ', valornexitos2 + valornfracasos2,'\\n")', sep="" )
      command<- paste(command, resultado,"\n",sep="" )
      command<-paste(command,'cat("',gettext("Sample estimates",domain="R-RcmdrPlugin.TeachStat"),':",names(aux$estimate)[1],as.numeric(aux$estimate)[1],",", names(aux$estimate)[2],as.numeric(aux$estimate)[2],"\\n")',"\n",sep="")
      command<-paste(command,'cat("',gettext("Interval",domain="R-RcmdrPlugin.TeachStat"),': (",aux2[1],",",aux2[2],")\\n")',"\n})",sep="")
      doItAndPrint(command)

    }
    
 
  #  command<- paste("calcular_ICDiferenciaProporciones(variable1 =", vICProporcion,", level.variable1 =", vLevelICProporcion,",variable2 =", vICProporcion2,", level.variable2 =", vLevelICProporcion2,", n.exitos.muestra1=", valornexitos,", n.fracasos.muestra1=", valornfracasos,", n.exitos.muestra2=", valornexitos2,", n.fracasos.muestra2=", valornfracasos2,", nivel.confianza=",valornConfianza,")",sep="" )
  #  doItAndPrint(command)



###############################################################################################################


  ##  calcular_ICDiferenciaProporciones(factor.twoormorelevels = variableICProporcion, level.factor = variableLevelICProporcion,n.exitos=valornexitos,n.fracasos=valornfracasos, nivel.confianza=valornConfianza)

   tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Cprop.test", reset="intervaloConfianzaDiferenciaProporciones", apply="intervaloConfianzaDiferenciaProporciones")
  tkgrid(comboBoxFrame2,labelRcmdr(selectFactorsFrame, text="          "),comboBoxFrame,sticky="nw")

  tkgrid(selectFactorsFrame,labelRcmdr(seleccionFrame, text="          "),muestrasFrame,sticky="nw")
  tkgrid(seleccionFrame, sticky="nw")
  tkgrid(labelRcmdr(top, text="          "))
  tkgrid(nConfianzaFrame, sticky="nw")

  tkgrid(buttonsFrame, sticky="w")


  dialogSuffix()
}

