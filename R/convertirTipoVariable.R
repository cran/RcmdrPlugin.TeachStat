ConvertVariables<- function () {
  # To ensure that menu name is included in pot file
  gettext("Change variables type...", domain="R-RcmdrPlugin.TeachStat")
  
  env <- environment()

  defaults <- list (initial.var1=NULL, initial.var2=NULL, initial.var3=NULL)
  dialog.values <- getDialog ("ConvertVariables", defaults)
  initializeDialog(title = gettext("Change variables type",domain="R-RcmdrPlugin.TeachStat"))


  #Instrucciones para hallar las variables cualitativas ordinal
  activeDataSet <- ActiveDataSet()
  activeDataSet <- get(activeDataSet)
  factores_en_ActiveDataSet <- Factors()
  if(length(factores_en_ActiveDataSet)==0){
    ordinales_en_ActiveDataSet <- nominales_en_ActiveDataSet <- otros_en_ActiveDataSet <- vector("character")
  } else {
    datos_tipo_factor<-names(activeDataSet[factores_en_ActiveDataSet])[(sapply(activeDataSet[factores_en_ActiveDataSet], is.factor))]
    if(length(datos_tipo_factor)==0){
      nominales_en_ActiveDataSet <- ordinales_en_ActiveDataSet <- vector("character")
      otros_en_ActiveDataSet <- factores_en_ActiveDataSet
    } else{
      datos_tipo_ordenado <- names(activeDataSet[datos_tipo_factor])[(sapply(activeDataSet[datos_tipo_factor], is.ordered))]
      ordinales_en_ActiveDataSet <- datos_tipo_factor[datos_tipo_factor %in% datos_tipo_ordenado]
      nominales_en_ActiveDataSet <- datos_tipo_factor[!datos_tipo_factor %in% datos_tipo_ordenado]
      otros_en_ActiveDataSet <- factores_en_ActiveDataSet[!factores_en_ActiveDataSet %in% datos_tipo_factor]
    }
  }
  


  #Construyo los diferentes Box existentes

  variablesFrame <- tkframe(top)
 
  vnominal <- variableListBox(variablesFrame, nominales_en_ActiveDataSet, selectmode="multiple", title = gettext("Nominal variables",domain="R-RcmdrPlugin.TeachStat"))
                                       # initialSelection = varPosn(dialog.values$initial.var1,"factor"))
  # deselection of all items
  tkselection.clear(vnominal$listbox, 0, "end")
  
  vordinal <- variableListBox(variablesFrame, ordinales_en_ActiveDataSet, selectmode="multiple", title = gettext("Ordinal variables",domain="R-RcmdrPlugin.TeachStat"))
                                 # initialSelection = varPosn(dialog.values$initial.var2,"all"))
  tkselection.clear(vordinal$listbox, 0, "end")
  
  vnumerica <- variableListBox(variablesFrame, Numeric() , selectmode="multiple", title = gettext("Numeric variables",domain="R-RcmdrPlugin.TeachStat"))
                                         # initialSelection = varPosn(dialog.values$initial.var3,"numeric"))
  tkselection.clear(vnumerica$listbox, 0, "end")
  
  votros <- variableListBox(variablesFrame, otros_en_ActiveDataSet, selectmode="multiple", title = gettext("Other variables",domain="R-RcmdrPlugin.TeachStat"))
  # initialSelection = varPosn(dialog.values$initial.var1,"factor"))
  tkselection.clear(votros$listbox, 0, "end")
  

  
  onOK <- function() {
    var1<-as.character(tkget(vnominal$listbox,0,"end"))
    var2<-as.character(tkget(vordinal$listbox,0,"end"))
    var3<-as.character(tkget(vnumerica$listbox,0,"end"))
    var4<-as.character(tkget(votros$listbox,0,"end"))
 
    if("" %in% var1) var1<-var1[var1!=""]
    if("" %in% var2) var2<-var2[var2!=""]
    if("" %in% var3) var3<-var3[var3!=""]
    if("" %in% var4) var4<-var4[var4!=""]
    
   
    putDialog ("ConvertVariables", list(initial.var1=var1, initial.var2=var2, initial.var3=var3))
    closeDialog()


   
    

###################### Imprimir la función a llamar por RCommander ###########################################

    .activeDataSet<-ActiveDataSet()
    asNominal <- setdiff(var1,nominales_en_ActiveDataSet)
    asOrdinal <- setdiff(var2,ordinales_en_ActiveDataSet)
    asNumeric <- setdiff(var3,Numeric())
    if ((length(asNominal)==0) && (length(asOrdinal)==0) && (length(asNumeric)==0)){
      errorCondition(recall = ConvertVariables, message = gettext("At least one variable must be selected",domain="R-RcmdrPlugin.TeachStat"))
      return()
    } else {
      datasetactivo<-get(.activeDataSet, envir = env)
      command <- NULL
      if(length(asNominal)>0){
        for (name in asNominal){
          # warning about variable replacement
          # if (is.element(name, Variables())) {
          #   if ("no" == tclvalue(checkReplace(name))){
          #     ConvertVariables()
          #     return()
          #   }
          # }
          
          if(is.ordered(datasetactivo[,name])){
            command <- paste(command, '\n  class(', name, ') <- "factor"', sep="")
          } else{
            command <- paste(command, "\n  ", name, " <- as.factor(", name,")", sep="")
          }
        }
      }
      if(length(asOrdinal)>0){
        for (name in asOrdinal){
          # warning about variable replacement
          # if (is.element(name, Variables())) {
          #   if ("no" == tclvalue(checkReplace(name))){
          #     ConvertVariables()
          #     return()
          #   }
          # }
          command <- paste(command, "\n  ", name, " <- as.ordered(", name,")", sep="")
        }
      }
      if(length(asNumeric)>0){
        noasnum <- NULL
        for (name in asNumeric){
          # warning about variable replacement
          # if (is.element(name, Variables())) {
          #   if ("no" == tclvalue(checkReplace(name))){
          #     ConvertVariables()
          #     return()
          #   }
          # }
          
          # check if it can be converted to numeric (it has numerical values)
          cmd <- paste("is.numeric(type.convert(",.activeDataSet,"$",name,"))",sep="")
          benum <- eval(parse(text=cmd), envir=.GlobalEnv)
          if (benum){
            command <- paste(command, "\n  ", name, " <- as.numeric(levels(", name,"))[",name,"]", sep="")
          } else{
            noasnum <- c(noasnum,name)
            next
          }
        }
        if(length(noasnum)>0) 
          Message(message=paste(gettext("The variables",domain="R-RcmdrPlugin.TeachStat"),paste("\"",noasnum,"\"",sep="", collapse=", "),gettext("can not be coerce in numeric",domain="R-RcmdrPlugin.TeachStat")), type="warning")
      }
      if(!is.null(command)){
        command <- paste(.activeDataSet," <- within(",.activeDataSet,", {",command,"\n})",sep="")
        result <- doItAndPrint(command)
        if (class(result)[1] !=  "try-error") activeDataSet(.activeDataSet, flushModel=FALSE, flushDialogMemory=FALSE)
      }
    }
    tkfocus(CommanderWindow())
  }
  
  toNominal <- function() {
    selOt <- as.numeric(tkcurselection(votros$listbox)) #Positions to change
    selOther<- NULL
    for(i in selOt) selOther <- c(selOther, as.character(tkget(votros$listbox,i)))
    for(i in rev(selOt)) tkdelete(votros$listbox,i)
    for(var in selOther) tkinsert(vnominal$listbox,"end",var)
    
    selOr <- as.numeric(tkcurselection(vordinal$listbox)) #Positions to change
    selOrdinal <- NULL
    for(i in selOr) selOrdinal <- c(selOrdinal, as.character(tkget(vordinal$listbox,i)))
    for(i in rev(selOr)) tkdelete(vordinal$listbox,i)
    for(var in selOrdinal) tkinsert(vnominal$listbox,"end",var)
    
    selNu <- as.numeric(tkcurselection(vnumerica$listbox)) #Positions to change
    selNumeric <- NULL
    for(i in selNu) selNumeric <- c(selNumeric, as.character(tkget(vnumerica$listbox,i)))
    for(i in rev(selNu)) tkdelete(vnumerica$listbox,i)
    for(var in selNumeric) tkinsert(vnominal$listbox,"end",var)
  }
  
  toOrdinal <- function() {
    selOt <- as.numeric(tkcurselection(votros$listbox)) #Positions to change
    selOther<- NULL
    for(i in selOt) selOther <- c(selOther, as.character(tkget(votros$listbox,i)))
    for(i in rev(selOt)) tkdelete(votros$listbox,i)
    for(var in selOther) tkinsert(vordinal$listbox,"end",var)
    
    selNo <- as.numeric(tkcurselection(vnominal$listbox)) #Positions to change
    selNominal <- NULL
    for(i in selNo) selNominal <- c(selNominal, as.character(tkget(vnominal$listbox,i)))
    for(i in rev(selNo)) tkdelete(vnominal$listbox,i)
    for(var in selNominal) tkinsert(vordinal$listbox,"end",var)
    
    selNu <- as.numeric(tkcurselection(vnumerica$listbox)) #Positions to change
    selNumeric <- NULL
    for(i in selNu) selNumeric <- c(selNumeric, as.character(tkget(vnumerica$listbox,i)))
    for(i in rev(selNu)) tkdelete(vnumerica$listbox,i)
    for(var in selNumeric) tkinsert(vordinal$listbox,"end",var)
  }
  
  toNumeric <- function() {
    selOt <- as.numeric(tkcurselection(votros$listbox)) #Positions to change
    selOther<- NULL
    for(i in selOt) selOther <- c(selOther, as.character(tkget(votros$listbox,i)))
    for(i in rev(selOt)) tkdelete(votros$listbox,i)
    for(var in selOther) tkinsert(vnumerica$listbox,"end",var)
    
    selOr <- as.numeric(tkcurselection(vordinal$listbox)) #Positions to change
    selOrdinal <- NULL
    for(i in selOr) selOrdinal <- c(selOrdinal, as.character(tkget(vordinal$listbox,i)))
    for(i in rev(selOr)) tkdelete(vordinal$listbox,i)
    for(var in selOrdinal) tkinsert(vnumerica$listbox,"end",var)
    
    selNo <- as.numeric(tkcurselection(vnominal$listbox)) #Positions to change
    selNominal <- NULL
    for(i in selNo) selNominal <- c(selNominal, as.character(tkget(vnominal$listbox,i)))
    for(i in rev(selNo)) tkdelete(vnominal$listbox,i)
    for(var in selNominal) tkinsert(vnumerica$listbox,"end",var)
  }
  

 OKCancelHelp(helpSubject = "ConvertVariables", reset="ConvertVariables", apply="ConvertVariables")

 tkgrid(labelRcmdr(variablesFrame, text=gettext("Select one or more variables to modify their type:",domain="R-RcmdrPlugin.TeachStat"),fg = getRcmdr("title.color"), font="RcmdrTitleFont"),columnspan=3, sticky="w") 
 tkgrid(getFrame(vnominal), labelRcmdr(variablesFrame, text="   "),getFrame(vordinal),labelRcmdr(variablesFrame, text="   "),getFrame(vnumerica),labelRcmdr(variablesFrame, text="   "),getFrame(votros) ,sticky="nw")
 
 
 ConvertButtons <- tkframe(top) 
 NominalButton <- buttonRcmdr(ConvertButtons, text=gettext("Nominal",domain="R-RcmdrPlugin.TeachStat"), command=toNominal)
 OrdinalButton <- buttonRcmdr(ConvertButtons, text=gettext("Ordinal",domain="R-RcmdrPlugin.TeachStat"), command=toOrdinal)
 NumericButton <- buttonRcmdr(ConvertButtons, text=gettext("Numeric",domain="R-RcmdrPlugin.TeachStat"), command=toNumeric)
 
 tkgrid(labelRcmdr(ConvertButtons, text=gettext("Convert selected variables to: ",domain="R-RcmdrPlugin.TeachStat"),fg = getRcmdr("title.color"), font="RcmdrTitleFont"), NominalButton,labelRcmdr(ConvertButtons, text=""),OrdinalButton , labelRcmdr(ConvertButtons, text=""),NumericButton)
 
 tkgrid(variablesFrame,columnspan=2,sticky="w")
 tkgrid(labelRcmdr(top,text="  "))
 tkgrid(ConvertButtons, sticky="w")
 tkgrid(labelRcmdr(top,text="  "))

  
 tkgrid(buttonsFrame, sticky="we")


 dialogSuffix()
}


