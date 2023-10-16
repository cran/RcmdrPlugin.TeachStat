intervaloConfianzaProporcion <- function () {
  #invisible(library(tcltk2))
  
  dialogName <- "intervaloConfianzaProporcion"
  
  
  
  defaults <- list (initial.var=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),
                    initial.varlevel=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),
                    initial.nexitos="",initial.nfracasos="",initial.nconf="0.95",
                    initial.alternative = "two.sided")
  dialog.values <- getDialog (dialogName, defaults)
  initializeDialog(title = gettext("Confidence interval for a proportion",domain="R-RcmdrPlugin.TeachStat"))
  
  ##Creación de los ComboBox
  
  selectFrame<-tkframe(top)
  comboBoxFrame<-ttkframe(selectFrame,borderwidth=1, relief="solid", padding=c(5,5,8,8))
  
  twoOrMoreLevelFactors<-twoOrMoreLevelFactors() ##NULL SI NO ACTIVEDATASET
  
  if (length(twoOrMoreLevelFactors())!=0){
    mostrar<-"readonly"
  }else {
    mostrar<-"disabled"
  }
  
  
  valuescombo_box<-c(gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),twoOrMoreLevelFactors())
  #varcombo_box=tclVar(valuescombo_box[1])
  varcombo_box<-tclVar(dialog.values$initial.var)
  
  combo_box<-ttkcombobox(comboBoxFrame,values=valuescombo_box,textvariable=varcombo_box,state=mostrar)
  
  tkbind(combo_box, "<<ComboboxSelected>>",function(){
    
    value<-tclvalue(varcombo_box)
    if(value!=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){
      datasetactivo <- ActiveDataSet()
      datasetactivo <- get(datasetactivo)
      niveles<-levels(datasetactivo[,value])
      tkconfigure(combo_box2,values=niveles)
      tclvalue(varcombo_box2)<-niveles[1]
      tk2state.set(combo_box2, state="readonly")
      tkfocus(combo_box2)
    }else{
      tk2state.set(combo_box2, state="disabled")
      niveles<-gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")
      tkconfigure(combo_box2,values=niveles)
      tclvalue(varcombo_box2)<-niveles
    }
  })
  
  if(dialog.values$initial.var==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){
    mostrar<-"disabled"
    valuescombo_box2<-gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")
  } else{
    datasetactivo <- ActiveDataSet()
    datasetactivo <- get(datasetactivo)
    mostrar<-"readonly"
    valuescombo_box2<-levels(datasetactivo[,dialog.values$initial.var])
  }
  
  #varcombo_box2=tclVar(gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"))
  varcombo_box2<-tclVar(dialog.values$initial.varlevel)
  combo_box2<-ttkcombobox(comboBoxFrame,values=valuescombo_box2,textvariable=varcombo_box2,state=mostrar)
  
  
  ##Fin creación comboBox
  
  
  
  exitosFracasosFrame<-ttkframe(selectFrame,borderwidth=1, relief="solid", padding=c(5,5,8,8))
  
  nExitosVar<-tclVar(dialog.values$initial.nexitos)
  nExitosEntry<-ttkentry(exitosFracasosFrame,width="5",textvariable=nExitosVar)
  
  
  
  
  nFracasosVar<-tclVar(dialog.values$initial.nfracasos)
  nFracasosEntry<-ttkentry(exitosFracasosFrame,width="5",textvariable=nFracasosVar)
  
  ### Tipo de Intervalo
  nconftypeFrame<-tkframe(top)
  
  radioButtonstypeFrame<-tkframe(nconftypeFrame)
  radioButtons(window = radioButtonstypeFrame , name = "tipo", buttons = c("two.sided", "less","greater"),
               initialValue = dialog.values$initial.alternative,
               title = gettext("Interval type",domain="R-RcmdrPlugin.TeachStat"),
               labels = gettext(c("Two-sided", "Left-sided","Right-sided"),domain="R-RcmdrPlugin.TeachStat"),
  )
  ####
  
  
  nConfianzaFrame<-tkframe(nconftypeFrame)
  
  nConfianzaVar<-tclVar(dialog.values$initial.nconf)
  nConfianzaEntry<-ttkentry(nConfianzaFrame,width="5",textvariable=nConfianzaVar)
  
  onOK <- function(){
    dialogNameF <- get(dialogName,mode="function")
    
    varICProporcion<-tclvalue(varcombo_box)
    if ((length(twoOrMoreLevelFactors)!=0)&&(varICProporcion !=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"))){
      activeDataSet <- ActiveDataSet()
      activeDataSet<-get(activeDataSet)
      variableICProporcion<-activeDataSet[,varICProporcion]
      variableLevelICProporcion<-tclvalue(varcombo_box2)}
    else {
      variableICProporcion<-NULL
      variableLevelICProporcion<-gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")
    }
    
    valornexitos<-tclvalue(nExitosVar)
    if( (valornexitos!="") && (is.na(as.integer(valornexitos)) || (as.integer(valornexitos)<0) || !(isTRUE(all.equal(as.numeric(valornexitos),as.integer(valornexitos)))) )) {
      valornexitos=""
      errorCondition(recall=dialogNameF, message=gettext("Successes number value must be a positive integer number",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{if(valornexitos!=""){valornexitos<-as.integer(valornexitos)}}
    
    valornfracasos<-tclvalue(nFracasosVar)
    if((valornfracasos!="") && (is.na(as.integer(valornfracasos)) || (as.integer(valornfracasos)<0) || !(isTRUE(all.equal(as.numeric(valornfracasos),as.integer(valornfracasos)))) )){
      valornfracasos=""
      errorCondition(recall=dialogNameF, message=gettext("Failures number value must be a positive integer number",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{if(valornfracasos!=""){valornfracasos<-as.integer(valornfracasos)}}
    
    
    if ((is.null(variableICProporcion))&&((valornexitos=="")||(valornfracasos==""))){
      errorCondition(recall=dialogNameF, message=gettext("A variable or successes number and failures number must be selected",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    
    sumaexitosfracasos<-sum(as.integer(valornexitos),as.integer(valornfracasos))
    
    if ((is.null(variableICProporcion))&&(sumaexitosfracasos==0)){
      errorCondition(recall=dialogNameF, message=gettext("A variable must be selected or the sum of successes and failures numbers must be positive",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    
    muestrasnulas <- ((valornexitos=="")&(valornfracasos==""))
    muestrasrellenas <- ((!valornexitos=="")&(!valornfracasos==""))
    
    if (!(is.null(variableICProporcion)) && (!(muestrasnulas | muestrasrellenas))){
      errorCondition(recall=dialogNameF, message=gettext("Successes number and failures number must be provided",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    
    ### Tipo de Intervalo
    varTipo <- tclvalue(tipoVariable)
    
    valornConfianza<-tclvalue(nConfianzaVar)
    
    if(is.na(as.numeric(valornConfianza)) || (as.numeric(valornConfianza)<0)||(as.numeric(valornConfianza)>1)) {
      valornConfianza=0.95
      errorCondition(recall=dialogNameF, message=gettext("Confidence level must be between 0 and 1",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    else{valornConfianza<-as.numeric(valornConfianza)}
    
    
    putDialog (dialogName, list(initial.var=varICProporcion,
                                initial.varlevel=variableLevelICProporcion,
                                initial.nexitos=valornexitos,initial.nfracasos=valornfracasos,initial.nconf="0.95",
                                initial.alternative=varTipo))
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
    
    tipointervalo<-paste("\\n",gettext("Confidence Interval for a proportion",domain="R-RcmdrPlugin.TeachStat"),"\\n", sep="")
    linaux<-paste(rep(c("-"," "),(nchar(tipointervalo)/2)),collapse="")
    # tipointervalo<-paste(tipointervalo, linaux,sep="")
    
    ### Tipo de Intervalo
    typeinterval<-switch(varTipo,
                         two.sided=gettext("Two-sided",domain="R-RcmdrPlugin.TeachStat"),
                         less=gettext("Left-sided",domain="R-RcmdrPlugin.TeachStat"),
                         greater=gettext("Right-sided",domain="R-RcmdrPlugin.TeachStat"))
    
    if(!is.null(vICProporcion)){
      
      command<- paste("exitos<-sum(",vICProporcion," == ", vLevelICProporcion,",na.rm = TRUE)",sep="")
      command<- paste(command,"\n","total<-length(",vICProporcion,")",sep="")
      command<-paste(command,"\n","aux<- Cprop.test(ex=exitos, nx=total, conf.level=",valornConfianza,
                     ", alternative='",varTipo,"')",sep="")
      command<- paste(command,"\n","levelsaux<-levels(",vICProporcion,")",sep="")
      command<- paste(command,"\n","levelsaux<-levelsaux[levelsaux!=",vLevelICProporcion,"]",sep="")
      
      
      
      
      command<- paste("local({\n",command,"\n",'aux2<-as.vector(aux[["conf.int"]]) \n',sep="")
      command<- paste(command, 'cat("',tipointervalo, '")\n',sep="" )
      command<- paste(command, 'cat("',linaux, '\\n")\n',sep="" )
      command<- paste(command, 'cat("',gettext("Interval type",domain="R-RcmdrPlugin.TeachStat"),': ',typeinterval,'\\n")\n',sep="")
      command<- paste(command, 'cat("',gettext("Confidence level",domain="R-RcmdrPlugin.TeachStat"),': ', valornConfianza*100,'%\\n")\n',sep="" )
      
      labelsother<-if(length(levels(variableICProporcion))<=2){
        '", levelsaux,"'
      }else {
        gettext("Other levels",domain="R-RcmdrPlugin.TeachStat")
      }
      
      command<- paste(command, 'cat("',gettext("Variable",domain="R-RcmdrPlugin.TeachStat"),': ', varICProporcion,
                      " [",tclvalue(varcombo_box2),' vs. ', labelsother, '] --> ',
                      gettext("No. successes",domain="R-RcmdrPlugin.TeachStat"),' = ",exitos," -- ',
                      gettext("No. attempts",domain="R-RcmdrPlugin.TeachStat"),' =", total,"\\n")\n',sep="" )
      
      
      # if(length(levels(variableICProporcion))<=2){
      #   resultado<-paste('cat("',tipointervalo, '\\n',
      #                    gettext("Confidence level",domain="R-RcmdrPlugin.TeachStat"),': ', valornConfianza*100,'%\\n',
      #                    gettext("Variable",domain="R-RcmdrPlugin.TeachStat"),': ', varICProporcion,
      #                    ' [',tclvalue(varcombo_box2),' vs.", levelsaux,"] --> ',
      #                    gettext("No. successes",domain="R-RcmdrPlugin.TeachStat"),' = ",exitos," -- ',
      #                    gettext("No. attempts",domain="R-RcmdrPlugin.TeachStat"),' =", total,"\\n"',')', sep="" )
      # } else{
      #   resultado<-paste('cat("',tipointervalo, '\\n',
      #                    gettext("Confidence level",domain="R-RcmdrPlugin.TeachStat"),': ', valornConfianza*100,'%\\n',
      #                    gettext("Variable",domain="R-RcmdrPlugin.TeachStat"),': ', varICProporcion,
      #                    " [",tclvalue(varcombo_box2),' vs. ', gettext("Other levels",domain="R-RcmdrPlugin.TeachStat"), '] --> ',
      #                    gettext("No. successes",domain="R-RcmdrPlugin.TeachStat"),' = ",exitos," -- ',
      #                    gettext("No. attempts",domain="R-RcmdrPlugin.TeachStat"),' =", total,"\\n"',')', sep="" )
      # }
      
      # command<- paste(command, resultado,"\n",sep="" )
      command<-paste(command,'cat("',gettext("Sample estimate",domain="R-RcmdrPlugin.TeachStat"),':",names(aux$estimate),as.numeric(aux$estimate),"\\n")',"\n",sep="")
      command<-paste(command,'cat("',gettext("Interval",domain="R-RcmdrPlugin.TeachStat"),': (",aux2[1],",",aux2[2],")\\n")',"\n})",sep="")
      doItAndPrint(command)
    }
    
    if(!is.na(valornexitos)){
      if((valornexitos==0)||(valornfracasos==0)){
        errorCondition(recall=contrastHipotesisProporcion, message=gettext("There must be at least 1 success and 1 failure",domain="R-RcmdrPlugin.TeachStat"))
        return()
      }
      
      command2<- paste("aux<- Cprop.test(ex=",valornexitos,", nx=",valornexitos + valornfracasos,", conf.level=",valornConfianza,
                       ", alternative='",varTipo,"')",sep="" )
      #     tipointervalo<-paste("\\nINTERVALO DE CONFIANZA PARA UNA PROPORCI?N","\\n", sep="")
      #     linaux<-paste(rep(c("-"," "),(nchar(tipointervalo)/2)),collapse="")
      #     tipointervalo<-paste(tipointervalo, linaux,sep="")
      
      
      command2<- paste("local({\n",command2,"\n",'aux2<-as.vector(aux[["conf.int"]]) \n',sep="")
      
      command2<- paste(command2, 'cat("',tipointervalo, '")\n',sep="" )
      command2<- paste(command2, 'cat("',linaux, '\\n")\n',sep="" )
      command2<- paste(command2, 'cat("',gettext("Interval type",domain="R-RcmdrPlugin.TeachStat"),': ',typeinterval,'\\n")\n',sep="")
      command2<- paste(command2, 'cat("',gettext("Confidence level",domain="R-RcmdrPlugin.TeachStat"),': ', valornConfianza*100,'%\\n")\n',sep="" )
      command2<- paste(command2, 'cat("',gettext("Sample",domain="R-RcmdrPlugin.TeachStat"),': ',
                       gettext("No. successes",domain="R-RcmdrPlugin.TeachStat"),' = ', valornexitos, ' -- ',
                       gettext("No. attempts",domain="R-RcmdrPlugin.TeachStat"),' = ', valornexitos + valornfracasos,'\\n")\n',sep="" )
      
      
      # resultado<-paste('cat("',tipointervalo, '\\n',
      #                  gettext("Confidence level",domain="R-RcmdrPlugin.TeachStat"),': ' , valornConfianza*100,'%\\n',
      #                  gettext("Sample",domain="R-RcmdrPlugin.TeachStat"),': ',
      #                  gettext("No. successes",domain="R-RcmdrPlugin.TeachStat"),' = ', valornexitos, ' -- ',
      #                  gettext("No. attempts",domain="R-RcmdrPlugin.TeachStat"),' = ', valornexitos + valornfracasos,
      #                  '\\n")', sep="" )
      # command2<- paste(command2, resultado,"\n",sep="" )
      command2<-paste(command2,'cat("',gettext("Sample estimate",domain="R-RcmdrPlugin.TeachStat"),':",names(aux$estimate),as.numeric(aux$estimate),"\\n")',"\n",sep="")
      command2<-paste(command2,'cat("',gettext("Interval",domain="R-RcmdrPlugin.TeachStat"),': (",aux2[1],",",aux2[2],")\\n")',"\n})",sep="")
      doItAndPrint(command2)
      
      
    }
    
    tkfocus(CommanderWindow())
  }
  
  OKCancelHelp(helpSubject = "Cprop.test", reset=dialogName, apply=dialogName)
  
  tkgrid(labelRcmdr(comboBoxFrame, text=gettext("Variable (pick one)",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color")), sticky="nw")
  tkgrid(combo_box,sticky="nw")
  
  tkgrid(labelRcmdr(comboBoxFrame, text=gettext("Proportion for the level (pick one)",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ), sticky="nw")
  tkgrid(combo_box2, sticky="nw")
  
  tkgrid(labelRcmdr(exitosFracasosFrame, text=gettext("No. successes",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color")),nExitosEntry, sticky="nw")
  tkgrid(labelRcmdr(exitosFracasosFrame, text=gettext("No. failures",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ),nFracasosEntry, sticky="nw")
  
  tkgrid(tipoFrame , sticky="nw")
  
  tkgrid(comboBoxFrame,labelRcmdr(selectFrame, text="          "),exitosFracasosFrame,  sticky="nw")
  
  tkgrid(labelRcmdr(selectFrame, text="          "))
  tkgrid(labelRcmdr(nConfianzaFrame, text=gettext("Confidence level:",domain="R-RcmdrPlugin.TeachStat"), foreground=getRcmdr("title.color") ),
         nConfianzaEntry, sticky="nw")
  
  tkgrid(selectFrame, sticky="nw")
  tkgrid(nConfianzaFrame, 
         labelRcmdr(nconftypeFrame, text="          "),radioButtonstypeFrame, sticky="nw")
  tkgrid(nconftypeFrame, sticky="w")
  
  #tkgrid(buttonsFrame, sticky="w")
  
  
  dialogSuffix(grid.buttons=TRUE)
}





