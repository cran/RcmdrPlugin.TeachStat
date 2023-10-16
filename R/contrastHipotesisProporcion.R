contrastHipotesisProporcion <- function () {
 #invisible(library(tcltk2))

  defaults <- list (initial.nexitos="",initial.nfracasos="",initial.nconf="0.95",initial.alternative = "two.sided",initial.p0 = "0.5")
  dialog.values <- getDialog ("contrastHipotesisProporcion", defaults)
  initializeDialog(title = gettext("Hypothesis Testing for a proportion",domain="R-RcmdrPlugin.TeachStat"))

##Creación de los ComboBox

  selectFactorsFrame<-tkframe(top)
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
  tkgrid(labelRcmdr(comboBoxFrame, text=gettext("Variable (pick one)",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ), sticky="nw")
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
      tkfocus(combo_box2)}
    else{tk2state.set(combo_box2, state="disabled")
         niveles<-gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")
          tkconfigure(combo_box2,values=niveles)
          tclvalue(varcombo_box2)<-niveles}})

  varcombo_box2<-tclVar(gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"))
  combo_box2<-ttkcombobox(comboBoxFrame,values=varcombo_box2,textvariable=varcombo_box2,state="disabled")

  tkgrid(labelRcmdr(comboBoxFrame, text=gettext("Proportion for the level (pick one)",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ), sticky="nw")
  tkgrid(combo_box2, sticky="nw")

##Fin creación comboBox



  exitosFracasosFrame<-tkframe(top)

  nExitosVar<-tclVar(dialog.values$initial.nexitos)
  nExitosEntry<-ttkentry(exitosFracasosFrame,width="5",textvariable=nExitosVar)
  tkgrid(labelRcmdr(exitosFracasosFrame, text=gettext("No. successes",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color")),nExitosEntry, sticky="nw")




  nFracasosVar<-tclVar(dialog.values$initial.nfracasos)
  nFracasosEntry<-ttkentry(exitosFracasosFrame,width="5",textvariable=nFracasosVar)
  tkgrid(labelRcmdr(exitosFracasosFrame, text=gettext("No. failures",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ),nFracasosEntry, sticky="nw")

 ############################

  optionsFrame <- tkframe(top)
  radioButtons(optionsFrame, name = "alternative", buttons = c("twosided",
                                                               "less", "greater"), values = c("two.sided", "less", "greater"),
               labels = gettext(c("Population proportion != p0", "Population proportion < p0",
                                       "Population proportion > p0"),domain="R-RcmdrPlugin.TeachStat"), title = gettext("Alternative hypothesis",domain="R-RcmdrPlugin.TeachStat"),
               initialValue = dialog.values$initial.alternative)


  rightFrame<-tkframe(top)

  p0Frame <- tkframe(rightFrame)
  p0Variable <- tclVar(dialog.values$initial.p0)
  p0Field <- ttkentry(p0Frame, width = "5", textvariable = p0Variable)
  tkgrid(labelRcmdr(p0Frame, text=gettext("Null hypothesis: p =",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ), p0Field, sticky="nw")

  nConfianzaFrame<-tkframe(rightFrame)
  nConfianzaVar<-tclVar(dialog.values$initial.nconf)
  nConfianzaEntry<-ttkentry(nConfianzaFrame,width="5",textvariable=nConfianzaVar)
 # tkgrid(labelRcmdr(nConfianzaFrame, text="Nivel de Confianza = ", foreground="blue" ),nConfianzaEntry, sticky="nw")




  onOK <- function(){

    varICProporcion<-tclvalue(varcombo_box)
    if ((length(twoOrMoreLevelFactors)!=0)&&(varICProporcion !=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"))){
      activeDataSet <- ActiveDataSet()
      activeDataSet<-get(activeDataSet)
      variableICProporcion<-activeDataSet[,varICProporcion]
      variableLevelICProporcion<-tclvalue(varcombo_box2)}
    else {
      variableICProporcion<-NULL
      variableLevelICProporcion<-NULL
    }

    valornexitos<-tclvalue(nExitosVar)
                if( (valornexitos!="") && (is.na(as.integer(valornexitos)) || (as.integer(valornexitos)<0) || !(isTRUE(all.equal(as.numeric(valornexitos),as.integer(valornexitos)))) )) {
                  valornexitos<-""
                  errorCondition(recall=contrastHipotesisProporcion, message=gettext("Successes number value must be a positive integer number",domain="R-RcmdrPlugin.TeachStat"))
                  return()
                }
                else{if(valornexitos!=""){valornexitos<-as.integer(valornexitos)}}

    valornfracasos<-tclvalue(nFracasosVar)
    if((valornfracasos!="") && (is.na(as.integer(valornfracasos)) || (as.integer(valornfracasos)<0) || !(isTRUE(all.equal(as.numeric(valornfracasos),as.integer(valornfracasos)))) )){
      valornfracasos<-""
      errorCondition(recall=contrastHipotesisProporcion, message=gettext("Failures number value must be a positive integer number",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{if(valornfracasos!=""){valornfracasos<-as.integer(valornfracasos)}}


   if ((is.null(variableICProporcion))&&((valornexitos=="")||(valornfracasos==""))){
      errorCondition(recall=contrastHipotesisProporcion, message=gettext("A variable or successes number and failures number must be selected",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }

    sumaexitosfracasos<-sum(as.integer(valornexitos),as.integer(valornfracasos))

    if ((is.null(variableICProporcion))&&(sumaexitosfracasos==0)){
      errorCondition(recall=contrastHipotesisProporcion, message=gettext("A variable must be selected or the sum of successes and failures numbers must be positive",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }

    muestrasnulas <- ((valornexitos=="")&(valornfracasos==""))
    muestrasrellenas <- ((!valornexitos=="")&(!valornfracasos==""))

    if (!(is.null(variableICProporcion)) && (!(muestrasnulas | muestrasrellenas))){
      errorCondition(recall=contrastHipotesisProporcion, message=gettext("Successes number and failures number must be provided",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }

    valornConfianza<-tclvalue(nConfianzaVar)

    if(is.na(as.numeric(valornConfianza)) || (as.numeric(valornConfianza)<=0)||(as.numeric(valornConfianza)>=1)) {
      valornConfianza<-0.95
      errorCondition(recall=contrastHipotesisProporcion, message=gettext("Confidence level must be between 0 and 1",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{valornConfianza<-as.numeric(valornConfianza)}

    valorp0<-tclvalue(p0Variable)

    if((is.na(as.numeric(valorp0))) || (as.numeric(valorp0)<0)||(as.numeric(valorp0)>1)){
      valorp0<-"0.0"
      errorCondition(recall=contrastHipotesisProporcion, message=gettext("Value for the null hypothesis must be between 0 and 1 (and not be)",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{ valorp0<-as.numeric(valorp0)}

    varHAlternativa<-tclvalue(alternativeVariable)


    putDialog ("contrastHipotesisProporcion", list(initial.nexitos=valornexitos,initial.nfracasos=valornfracasos,initial.nconf=valornConfianza,initial.alternative = varHAlternativa,initial.p0 = valorp0))
    closeDialog()

   ##variableICProporcion TwoLevelFactoSeleccionado o nada
    valornexitos<-as.integer(valornexitos)
    valornfracasos<-as.integer(valornfracasos)
    valornConfianza<-as.numeric(valornConfianza)

    ###################### Imprimir la función a llamar por RCommander ###########################################

    .activeDataSet<-ActiveDataSet()

    if ((length(twoOrMoreLevelFactors)!=0)&&(varICProporcion !=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"))){
      vICProporcion<-paste(.activeDataSet,"$",varICProporcion, sep="")
      vLevelICProporcion<-paste('"',tclvalue(varcombo_box2),'"',sep="")}
    else {
      vICProporcion<-NULL
      vLevelICProporcion<-NULL
    }

    Haltern<-paste('"',varHAlternativa,'"',sep="")
    
    tipointervalo<-paste("\\n",gettext("Hypothesis Testing for a proportion",domain="R-RcmdrPlugin.TeachStat"),"\\n", sep="")
    linaux<-paste(rep(c("-"," "),(nchar(tipointervalo)/2)),collapse="")
    tipointervalo<-paste(tipointervalo, linaux,sep="")


    if(!is.null(vICProporcion)){

  #     auxi<-sum(variableICProporcion==vLevelICProporcion)
  #      if(auxi<1){
  #      errorCondition(recall=contrastHipotesisProporcion, message=gettext("N?mero de Exitos no puede ser menor que 1",domain="R-RcmdrPlugin.TeachStat"))
  #      return()
  #    }

      command<- paste("exitos<-sum(",vICProporcion," == ", vLevelICProporcion,",na.rm = TRUE)",sep="")
      command<- paste(command,"\n","total<-length(",vICProporcion,")",sep="")
      command<-paste(command,"\n","aux<- Cprop.test(ex=exitos, nx=total, p.null=",valorp0,", alternative=",Haltern,")",sep="")
      command<- paste(command,"\n","levelsaux<-levels(",vICProporcion,")",sep="")
      command<- paste(command,"\n","levelsaux<-levelsaux[levelsaux!=",vLevelICProporcion,"]",sep="")
      
      

      command<- paste("local({\n",command,"\n",sep="")

      if(length(levels(variableICProporcion))<=2){
        resultado<-paste('cat("',tipointervalo, '\\n',gettext("No. successes",domain="R-RcmdrPlugin.TeachStat"),': ', varICProporcion,' [',tclvalue(varcombo_box2),' vs.", levelsaux,"] --> ',gettext("No. successes",domain="R-RcmdrPlugin.TeachStat"),' = ",exitos," -- ',gettext("No. attempts",domain="R-RcmdrPlugin.TeachStat"),' =", total,"\\n"',')', sep="" )}
      else{resultado<-paste('cat("',tipointervalo, '\\n',gettext("No. successes",domain="R-RcmdrPlugin.TeachStat"),': ', varICProporcion,' [',tclvalue(varcombo_box2),'"," vs. ', gettext("Other levels",domain="R-RcmdrPlugin.TeachStat"), '] --> ',gettext("No. successes",domain="R-RcmdrPlugin.TeachStat"),' = ",exitos," -- ',gettext("No. attempts",domain="R-RcmdrPlugin.TeachStat"),' =", total,"\\n"',')', sep="" )}

      command<- paste(command, resultado,"\n",sep="" )
      distribucion<-paste('cat("',gettext("Distribution",domain="R-RcmdrPlugin.TeachStat"),':","Normal(0,1)\\n")',sep="")
      e.contraste<- paste('\n cat("',gettext("Test statistics value",domain="R-RcmdrPlugin.TeachStat"),':",as.numeric(aux$statistic),"\\n")',sep="")

      p.valor<-paste('\n if(aux$p.value>0.05){cat("',gettext("p-value",domain="R-RcmdrPlugin.TeachStat"),':",format.pval(aux$p.value),"\\n")}
    if(aux$p.value>=0.025 && aux$p.value<=0.05){cat("',gettext("p-value",domain="R-RcmdrPlugin.TeachStat"),':",format.pval(aux$p.value),"*\\n")}
                     if(aux$p.value>=0.0001 && aux$p.value<=0.025){cat("',gettext("p-value",domain="R-RcmdrPlugin.TeachStat"),':",format.pval(aux$p.value),"**\\n")}
                     if(aux$p.value>=0 && aux$p.value<=0.0001){cat("',gettext("p-value",domain="R-RcmdrPlugin.TeachStat"),':",format.pval(aux$p.value),"***\\n")}\n',sep="")


      if (varHAlternativa == "two.sided"){h.alt<-paste(gettext("Population proportion is not equal to",domain="R-RcmdrPlugin.TeachStat"),valorp0,sep=" ")}
      if (varHAlternativa == "less"){h.alt<-paste(gettext("Population proportion is less than",domain="R-RcmdrPlugin.TeachStat"),valorp0,sep=" ")}
      if (varHAlternativa == "greater"){h.alt<- paste(gettext("Population proportion is greater than",domain="R-RcmdrPlugin.TeachStat"),valorp0,sep=" ")}
      h.alt<-paste('\n cat("',gettext("Alternative hypothesis",domain="R-RcmdrPlugin.TeachStat"),':","',h.alt,'","\\n")',sep="")

      e.muestral<-paste('\n cat("',gettext("Sample estimate",domain="R-RcmdrPlugin.TeachStat"),':",names(aux$estimate),as.numeric(aux$estimate),"\\n")',sep="")

      command<- paste(command, distribucion,e.contraste,p.valor, h.alt,e.muestral,"\n})",sep="" )

      doItAndPrint(command)

    }

    if(!is.na(valornexitos)){

      if((valornexitos==0)||(valornfracasos==0)){
        errorCondition(recall=contrastHipotesisProporcion, message=gettext("There must be at least 1 success and 1 failure",domain="R-RcmdrPlugin.TeachStat"))
        return()
      }

      command2<- paste("aux<- Cprop.test(ex=",valornexitos,", nx=",valornexitos + valornfracasos,",p.null=",valorp0,", alternative=",Haltern,")",sep="" )
#       tipointervalo<-paste("\\nCONTRASTE DE HIP?TESIS PARA UNA PROPORCI?N","\\n", sep="")
#       linaux<-paste(rep(c("-"," "),(nchar(tipointervalo)/2)),collapse="")
#       tipointervalo<-paste(tipointervalo, linaux,sep="")

      command2<- paste("local({\n",command2,"\n",sep="")
      resultado<-paste('cat("',tipointervalo,'\\n',gettext("Sample",domain="R-RcmdrPlugin.TeachStat"),': ',gettext("No. successes",domain="R-RcmdrPlugin.TeachStat"),' = ', valornexitos, ' -- ',gettext("No. attempts",domain="R-RcmdrPlugin.TeachStat"),' = ', valornexitos + valornfracasos, '\\n")', sep="" )

      command2<- paste(command2, resultado,"\n",sep="" )
      distribucion<-paste('cat("',gettext("Distribution",domain="R-RcmdrPlugin.TeachStat"),':","Normal(0,1)\\n")',sep="")
      e.contraste<- paste('\n cat("',gettext("Test statistics value",domain="R-RcmdrPlugin.TeachStat"),':",as.numeric(aux$statistic),"\\n")',sep="")

      p.valor<-paste('\n if(aux$p.value>0.05){cat("',gettext("p-value",domain="R-RcmdrPlugin.TeachStat"),':",format.pval(aux$p.value),"\\n")}
    if(aux$p.value>=0.025 && aux$p.value<=0.05){cat("',gettext("p-value",domain="R-RcmdrPlugin.TeachStat"),':",format.pval(aux$p.value),"*\\n")}
                     if(aux$p.value>=0.0001 && aux$p.value<=0.025){cat("',gettext("p-value",domain="R-RcmdrPlugin.TeachStat"),':",format.pval(aux$p.value),"**\\n")}
                     if(aux$p.value>=0 && aux$p.value<=0.0001){cat("',gettext("p-value",domain="R-RcmdrPlugin.TeachStat"),':",format.pval(aux$p.value),"***\\n")}\n',sep="")
      
      
      if (varHAlternativa == "two.sided"){h.alt<-paste(gettext("Population proportion is not equal to",domain="R-RcmdrPlugin.TeachStat"),valorp0,sep=" ")}
      if (varHAlternativa == "less"){h.alt<-paste(gettext("Population proportion is less than",domain="R-RcmdrPlugin.TeachStat"),valorp0,sep=" ")}
      if (varHAlternativa == "greater"){h.alt<- paste(gettext("Population proportion is greater than",domain="R-RcmdrPlugin.TeachStat"),valorp0,sep=" ")}
      h.alt<-paste('\n cat("',gettext("Alternative hypothesis",domain="R-RcmdrPlugin.TeachStat"),':","',h.alt,'","\\n")',sep="")
      
      e.muestral<-paste('\n cat("',gettext("Sample estimate",domain="R-RcmdrPlugin.TeachStat"),':",names(aux$estimate),as.numeric(aux$estimate),"\\n")',sep="")
      
      
#       distribucion<-'cat("Distribuci?n:","Normal(0,1)\\n")'
#       e.contraste<- '\n cat("Estad?stico contraste:",as.numeric(aux$statistic),"\\n")'
# 
#       p.valor<-'\n if(aux$p.value>0.05){cat("P.valor:",format.pval(aux$p.value),"\\n")}
# if(aux$p.value>=0.025 && aux$p.value<=0.05){cat("P.valor:",format.pval(aux$p.value),"*\\n")}
# if(aux$p.value>=0.0001 && aux$p.value<=0.025){cat("P.valor:",format.pval(aux$p.value),"**\\n")}
#       if(aux$p.value>=0 && aux$p.value<=0.0001){cat("P.valor:",format.pval(aux$p.value),"***\\n")}'
# 
# 
#       if (varHAlternativa == "two.sided"){h.alt<-paste("Proporci?n poblacional no es igual a ",valorp0,sep="")}
#       if (varHAlternativa == "less"){h.alt<-paste("Proporci?n poblacional es menor a ",valorp0,sep="")}
#       if (varHAlternativa == "greater"){h.alt<- paste("Proporci?n poblacional es mayor a ",valorp0,sep="")}
#       h.alt<-paste('\n cat("Hipotesis alternativa:","',h.alt,'","\\n")')

      #e.muestral<-'\n cat("Estimador muestral:",names(aux$estimate),as.numeric(aux$estimate),"\\n")'

      command2<- paste(command2, distribucion,e.contraste,p.valor, h.alt,e.muestral,"\n})",sep="" )

      doItAndPrint(command2)


    }

  ###############################################################################################################


 ##   calcular_CHProporcion(factor.twoormorelevels = variableICProporcion, level.factor = variableLevelICProporcion,n.exitos=valornexitos,n.fracasos=valornfracasos, hipotesis.alternativa=varHAlternativa,p0=valorp0, nivel.confianza=valornConfianza)
   tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "Cprop.test", reset="contrastHipotesisProporcion", apply="contrastHipotesisProporcion")

  tkgrid(comboBoxFrame,labelRcmdr(selectFactorsFrame, text="          "),exitosFracasosFrame, sticky="nw")
  tkgrid(selectFactorsFrame, sticky="nw")

  tkgrid(labelRcmdr(top, text="          "))

  tkgrid(labelRcmdr(rightFrame, text="        "),sticky="nw")
  tkgrid(p0Frame,sticky="nw")
  tkgrid(nConfianzaFrame, sticky="nw")


  tkgrid(alternativeFrame,labelRcmdr(optionsFrame, text="          "),rightFrame, sticky="nw")
  tkgrid(optionsFrame,sticky="nw")


  tkgrid(buttonsFrame, sticky="w")


  dialogSuffix()
}



