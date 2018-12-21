resumenVariablesCualitativas <- function () {
  #Library("abind")
  #Library("e1071")

  defaults <- list (initial.var1=NULL, initial.var2=NULL, initial.ordenadofrecuenciaVar="0", initial.cuantil=0.5)
  dialog.values <- getDialog ("resumenVariablesCualitativas", defaults)
  initializeDialog(title = gettext("Frequency distributions qualitative variables",domain="R-RcmdrPlugin.TeachStat"))


  #Instrucciones para hallar las variables cualitativas ordinal
  activeDataSet<-ActiveDataSet()
  activeDataSet<-get(activeDataSet)
  factores_en_ActiveDataSet<-Factors()
  datos_tipo_ordenado<-names(activeDataSet[factores_en_ActiveDataSet])[(sapply(activeDataSet[factores_en_ActiveDataSet], is.ordered))]
  nominales_en_ActiveDataSet<-factores_en_ActiveDataSet[!factores_en_ActiveDataSet %in% datos_tipo_ordenado]
  ordinales_en_ActiveDataSet<-factores_en_ActiveDataSet[factores_en_ActiveDataSet %in% datos_tipo_ordenado]
  
  
  


  #Construyo los diferentes Box existentes

  variablesFrame<-tkframe(top)
  #nonumeric<-Variables()[!Variables()%in%Numeric()]
  vcualitativas <- variableListBox(variablesFrame, c(nominales_en_ActiveDataSet,""), selectmode="multiple", title = gettext("Nominal variables",domain="R-RcmdrPlugin.TeachStat"),
                                       initialSelection = varPosn(dialog.values$initial.var1,"factor"))
  vcualitativaordinal <- variableListBox(variablesFrame, c(ordinales_en_ActiveDataSet,"") , selectmode="multiple", title = gettext("Ordinal variables",domain="R-RcmdrPlugin.TeachStat"),
                                 initialSelection = varPosn(dialog.values$initial.var2,"all"))
  skFrame <- tkframe(top)
  ordenadofrecuenciaVar<-tclVar(dialog.values$initial.ordenadofrecuenciaVar)
  ordenadofrecuenciaCheckBox<-ttkcheckbutton(skFrame, variable=ordenadofrecuenciaVar,text=gettext("Sort by frequency",domain="R-RcmdrPlugin.TeachStat"))

  cuantilFrame<-tkframe(top)

  cuantilVar<-tclVar(dialog.values$initial.cuantil)
  cuantilEntry<-ttkentry(cuantilFrame,width="5",textvariable=cuantilVar)
  tkgrid(labelRcmdr(cuantilFrame, text=gettext("Quantile:",domain="R-RcmdrPlugin.TeachStat")),cuantilEntry, sticky="nw")

  onOK <- function() {

    var1<-getSelection(vcualitativas)
    var2<-getSelection(vcualitativaordinal)
    var3<-tclvalue(ordenadofrecuenciaVar)
    var4<-tclvalue(cuantilVar)
    
    if("" %in% var1) var1<-var1[var1!=""]
    if("" %in% var2) var2<-var2[var2!=""]
    
   
    #Se verifica que como minimo hay una variable seleccionada
    
    if((0 == length(var1)) & (0 == length(var2))){
            errorCondition(recall = resumenVariablesCualitativas, message = gettext("At least one variable must be selected",domain="R-RcmdrPlugin.TeachStat"))
            return()
      }



    #Se verifica que el Percentil elegido es valido

    if ((var4!="") && ((var4<0)||(var4>1))){
      errorCondition(recall = resumenVariablesCualitativas, message = gettext("Quantile must be in [0,1]",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }

    putDialog ("resumenVariablesCualitativas", list(initial.var1=var1, initial.var2=var2, initial.ordenadofrecuenciaVar=var3,initial.cuantil=var4))
    closeDialog()


    vCualitativaNoNumerica<-subset(activeDataSet,select = var1)
    vCualitativaOrdinal<-subset(activeDataSet,select = var2)
    ordenado_Frecuencia<-as.logical(as.numeric(var3))
    cuantilP<-var4

###################### Imprimir la función a llamar por RCommander ###########################################

    .activeDataSet<-ActiveDataSet()

    if(0 == length(var1)) vnominal<-"NULL"
    else{        if (length(var1) == 1){vnominal<- paste('"', var1, '"', sep="")
                                 vnominal<-paste(.activeDataSet, "[", vnominal, "]", sep="")
                                 }
                 else{ vnominal<-paste("c(", paste('"', var1, '"', collapse=", ", sep=""), ")", sep="")

                       vnominal <- paste(.activeDataSet, "[,", vnominal, "]", sep="")
                      }
    }

    if(0 == length(var2)) vordinal<-"NULL"
    else{           if (length(var2) == 1){vordinal<- paste('"', var2, '"', sep="")
                                          vordinal<-paste(.activeDataSet, "[", vordinal, "]", sep="")
                    }
                    else{ vordinal<-paste("c(", paste('"', var2, '"', collapse=", ", sep=""), ")", sep="")
                         vordinal <- paste(.activeDataSet, "[,", vordinal, "]", sep="")
                     }
    }


    command<-paste("calcular_frecuencia(df.nominal=", vnominal,", ordenado.frec=",ordenado_Frecuencia ,", df.ordinal=",vordinal,", cuantil.p=",cuantilP,", iprint = TRUE)", sep="")
    doItAndPrint(command)

############M?todo de funcionamiento inicial No muestra resultados en RCommander #############################
        ### calcular_frecuencia(df.nominal=vCualitativaNoNumerica,ordenado.frec=ordenado_Frecuencia, df.ordinal=vCualitativaOrdinal, cuantil.p=cuantilP, iprint = TRUE)


     tkfocus(CommanderWindow())
  }

 OKCancelHelp(helpSubject = "calcular_frecuencia", reset="resumenVariablesCualitativas", apply="resumenVariablesCualitativas")
#  OKCancelHelp(helpSubject = "calcular_frecuencia", reset="resumenVariablesCualitativas")


  tkgrid(getFrame(vcualitativas),labelRcmdr(variablesFrame,text="   "), getFrame(vcualitativaordinal),sticky="nw")
  tkgrid(variablesFrame,sticky="w")
  tkgrid(labelRcmdr(top,text=" "))

  tkgrid(ordenadofrecuenciaCheckBox,labelRcmdr(skFrame,text="               "),cuantilFrame, sticky="w")
  tkgrid(skFrame, sticky="nw")

  tkgrid(buttonsFrame, sticky="w")


  dialogSuffix()
}




