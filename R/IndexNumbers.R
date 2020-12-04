Sindexnumbers <- function(){
  # To ensure that menu name is included in pot file
  gettext("Simple index numbers...", domain="R-RcmdrPlugin.TeachStat")
  
  env <- environment()
  dialogName <- "Sindexnumbers"
  defaults <- list (initial.valvar=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),
                    initial.basvar=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),
                    initial.baslevar=gettext("<no base level selected>",domain="R-RcmdrPlugin.TeachStat"),
                    initial.save="",initial.name="")
  dialog.values <- getDialog (dialogName, defaults)
  initializeDialog(title = gettext("Simple Index Numbers",domain="R-RcmdrPlugin.TeachStat"))
  
  selectFrame <- ttkframe(top)
  
  varFrame <- ttkframe(selectFrame)
  #comboBoxFrame<-tkframe(varFrame)
  selectValueVariable <- variableComboBox(varFrame, variableList=Numeric(), initialSelection=dialog.values$initial.valvar, 
                                          title=gettext("Value variable (pick one)",domain="R-RcmdrPlugin.TeachStat"))
  
  
  baseFrame <- ttkframe(selectFrame)
  # comboBox2Frame<-tkframe(baseFrame)
  
  twoOrMoreLevelFactors<-twoOrMoreLevelFactors() ##NULL SI NO ACTIVEDATASET

  if (length(twoOrMoreLevelFactors())!=0){
    mostrar<-"readonly"
  }else {
    mostrar<-"disabled"
  }
  selectBaseVariable <- variableComboBox(baseFrame, variableList=twoOrMoreLevelFactors(), state=mostrar,
                                          initialSelection=dialog.values$initial.basvar, title=gettext("Base variable (pick one)",domain="R-RcmdrPlugin.TeachStat"))

  tkbind(selectBaseVariable$combobox, "<<ComboboxSelected>>",function(){
    
    value<-getSelection(selectBaseVariable)
    if(value!=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){
      datasetactivo <- ActiveDataSet()
      datasetactivo<-get(datasetactivo)
      niveles<-levels(datasetactivo[,value])
      tkconfigure(selectBaseLevel$combobox,values=niveles)
      tclvalue(selectBaseLevel$combovar)<-niveles[1]
      tk2state.set(selectBaseLevel$combobox, state="readonly")
      tclvalue(nameVar) <- paste("index_",make.names(tclvalue(selectBaseLevel$combovar)),sep="")
      tkfocus(selectBaseLevel$combobox)
    } else{
      tk2state.set(selectBaseLevel$combobox, state="disabled")
      niveles<-gettext("<no level selected>",domain="R-RcmdrPlugin.TeachStat")
      tkconfigure(selectBaseLevel$combobox,values=niveles)
      tclvalue(selectBaseLevel$combovar)<-niveles
      tclvalue(nameVar) <-""
    }
  })
  
  if(dialog.values$initial.basvar==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){
    mostrar2 <- "disabled"
    varcombobox3<-gettext("<no level selected>",domain="R-RcmdrPlugin.TeachStat")
  }else {
    mostrar2<-"readonly"
    datasetactivo <- ActiveDataSet()
    datasetactivo<-get(datasetactivo)
    varcombobox3<-levels(datasetactivo[,dialog.values$initial.basvar])
  }
  
  selectBaseLevel <- variableComboBox(baseFrame, variableList=varcombobox3, state=mostrar2,
                                         initialSelection=dialog.values$initial.baslevar, title=gettext("Base (pick one)",domain="R-RcmdrPlugin.TeachStat"))
  # Deleting <no variable selected> option from the list
  tkconfigure(selectBaseLevel$combobox,values=varcombobox3)
  
  tkbind(selectBaseLevel$combobox, "<<ComboboxSelected>>",function(){

    value<-getSelection(selectBaseLevel)
    if(value!=gettext("<no level selected>",domain="R-RcmdrPlugin.TeachStat")){
      tclvalue(nameVar) <- paste("index_",make.names(tclvalue(selectBaseLevel$combovar)),sep="")
      # tkconfigure(nameEntry,textvariable=tclVar(paste("index_",value,sep="")))
    } else{
      tclvalue(nameVar) <- ""
      # tkconfigure(nameEntry,textvariable=tclVar(""))
    }
  })
 
  saveVar <- tclVar(dialog.values$initial.save)
  nameVar <- tclVar(dialog.values$initial.name)
  saveFrame <- ttkframe(top)
  saveButton <- ttkcheckbutton(saveFrame, variable=saveVar, text=gettext("Add to data frame",domain="R-RcmdrPlugin.TeachStat"),
                               command = function(){ 
                                 if(tclvalue(saveVar)==1){
                                   tk2state.set(nameEntry, state = "normal")
                                   tk2state.set(nameLabel, state = "normal")
                                 } else{
                                   tk2state.set(nameEntry, state = "disabled")
                                   tk2state.set(nameLabel, state = "disabled")
                                 }
                               })
  
  if(dialog.values$initial.save!=1){
    mostrar3 <- "disabled"
  }else {
    mostrar3<-"normal"
  }
  nameLabel <- labelRcmdr(saveFrame, text=gettext("Variable name:",domain="R-RcmdrPlugin.TeachStat"),state=mostrar3)
  nameEntry <- ttkentry(saveFrame, width="20", textvariable=nameVar,state=mostrar3)
  
  onOK <- function(){
    .activeDataSet <- ActiveDataSet()
    # Error check
    varValueVar<-getSelection(selectValueVariable)
    varBaseVar<-getSelection(selectBaseVariable)
    varBaseLev<-getSelection(selectBaseLevel)
    saveval <- tclvalue(saveVar)
    nameval <- trim.blanks(tclvalue(nameVar))
    
    dialogNameF <- get(dialogName,mode="function")
    
    if(varValueVar==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){
      errorCondition(recall=dialogNameF, message=gettext("No value variable selected",domain="R-RcmdrPlugin.TeachStat"))
      return()
    } else if(varBaseVar==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){
      errorCondition(recall=dialogNameF, message=gettext("No base variable selected",domain="R-RcmdrPlugin.TeachStat"))
      return()
    } else if(varBaseLev==gettext("<no level selected>",domain="R-RcmdrPlugin.TeachStat")){
      errorCondition(recall=dialogNameF, message=gettext("No base level selected",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    
    putDialog(dialogName, list(initial.valvar=varValueVar,initial.basvar=varBaseVar,initial.baslevar=varBaseLev,initial.save=saveval,initial.name=nameval))#, resettable=FALSE)
    closeDialog()
    
    # Check that there are no repeated values in the base variable
    datasetactivo<-get(.activeDataSet, envir = env)
    vals <- datasetactivo[,varBaseVar]
    
    if(anyDuplicated(vals)!=0){
      errorCondition(recall=dialogNameF, message=gettext("'The base variable cannot have repeated values'",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    
    
    # command construction
    command <- paste('local({\n  .Sindex <- Sindex(',.activeDataSet,', "',varBaseVar,'", "',varValueVar,'", "',varBaseLev,'")*100', sep="")
    command <- paste(command,'\n  print(.Sindex)',sep="")
    
    if(saveval==1){
      if (!is.valid.name(nameval)){
        errorCondition(recall=dialogNameF,
                       message=paste('"', nameval, '" ', gettextRcmdr("is not a valid name."), sep=""))
        return()
      }
      #Check if there exists that name in the data.frame
      if (is.element(nameval, Variables())) {
        if ("no" == tclvalue(checkReplace(nameval))){
          Sindexnumbers()
          return()
        }
      }
      # command <- paste(command,'\n  ',.activeDataSet,' <<- cbind(',.activeDataSet,', .Sindex)',sep="")
      command <- paste(command,'\n  ',.activeDataSet,'$',nameval,' <<- drop(as.matrix(.Sindex))',sep="")
    }
    command <- paste(command,'\n})', sep="")
    result <- doItAndPrint(command)
    #For refreshing the activedataset if the index number is added to the data frame
    if (saveval==1 && class(result)[1] !=  "try-error") activeDataSet(.activeDataSet, flushModel=FALSE, flushDialogMemory=FALSE)
    tkdestroy(top)
    tkfocus(CommanderWindow())
    
  }
  OKCancelHelp(helpSubject="Sindex", reset = dialogName, apply = dialogName)
  tkgrid(getFrame(selectValueVariable),sticky="w")
  
  tkgrid(getFrame(selectBaseVariable),sticky="w")
  
  tkgrid(getFrame(selectBaseLevel),sticky="w")
  
  tkgrid(varFrame,baseFrame,padx=6,sticky="nw")
  
  tkgrid(selectFrame, sticky="w")
  
  tkgrid(saveButton,nameLabel,nameEntry,padx=3,pady=6,sticky="w")
  # tkgrid(saveButton,pady=6,sticky="w")
  tkgrid(saveFrame, sticky="w")
  
  tkgrid(buttonsFrame, sticky="ew")
  dialogSuffix()
}


BCindexnumbers <- function(){
  # To ensure that menu name is included in pot file
  gettext("Base change...", domain="R-RcmdrPlugin.TeachStat")
  
  env <- environment()
  dialogName <- "BCindexnumbers"
  defaults <- list (initial.valvar=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),
                    initial.basvar=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat"),
                    initial.baslevar=gettext("<no base level selected>",domain="R-RcmdrPlugin.TeachStat"),
                    initial.save="",initial.name="")
  dialog.values <- getDialog (dialogName, defaults)
  initializeDialog(title = gettext("Base Change",domain="R-RcmdrPlugin.TeachStat"))
  
  selectFrame <- ttkframe(top)
  
  varFrame <- ttkframe(selectFrame)
  #comboBoxFrame<-tkframe(varFrame)
  selectValueVariable <- variableComboBox(varFrame, variableList=Numeric(),
                                          initialSelection=dialog.values$initial.valvar, title=gettext("Index variable (pick one)",domain="R-RcmdrPlugin.TeachStat"))
  
  
  baseFrame <- ttkframe(selectFrame)
  # comboBox2Frame<-tkframe(baseFrame)
  
  twoOrMoreLevelFactors<-twoOrMoreLevelFactors() ##NULL SI NO ACTIVEDATASET
  
  if (length(twoOrMoreLevelFactors())!=0){
    mostrar<-"readonly"
  }else {
    mostrar<-"disabled"
  }
  selectBaseVariable <- variableComboBox(baseFrame, variableList=twoOrMoreLevelFactors(), state=mostrar,
                                         initialSelection=dialog.values$initial.basvar, title=gettext("Base variable (pick one)",domain="R-RcmdrPlugin.TeachStat"))
  
  tkbind(selectBaseVariable$combobox, "<<ComboboxSelected>>",function(){
    
    value<-getSelection(selectBaseVariable)
    if(value!=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){
      datasetactivo <- ActiveDataSet()
      datasetactivo<-get(datasetactivo)
      niveles<-levels(datasetactivo[,value])
      tkconfigure(selectBaseLevel$combobox,values=niveles)
      tclvalue(selectBaseLevel$combovar)<-niveles[1]
      tk2state.set(selectBaseLevel$combobox, state="readonly")
      tclvalue(nameVar) <- paste("index_",make.names(tclvalue(selectBaseLevel$combovar)),sep="")
      tkfocus(selectBaseLevel$combobox)
    } else{
      tk2state.set(selectBaseLevel$combobox, state="disabled")
      niveles<-gettext("<no level selected>",domain="R-RcmdrPlugin.TeachStat")
      tkconfigure(selectBaseLevel$combobox,values=niveles)
      tclvalue(selectBaseLevel$combovar)<-niveles
      tclvalue(nameVar) <-""
    }
  })
  
  if(dialog.values$initial.basvar==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){
    mostrar2 <- "disable"
    varcombobox3<-gettext("<no level selected>",domain="R-RcmdrPlugin.TeachStat")
  }else {
    mostrar2<-"readonly"
    datasetactivo <- ActiveDataSet()
    datasetactivo<-get(datasetactivo)
    varcombobox3<-levels(datasetactivo[,dialog.values$initial.basvar])
  }
  
  selectBaseLevel <- variableComboBox(baseFrame, variableList=varcombobox3, state=mostrar2,
                                      initialSelection=dialog.values$initial.baslevar, title=gettext("New base (pick one)",domain="R-RcmdrPlugin.TeachStat"))
  # Deleting <no variable selected> option from the list
  tkconfigure(selectBaseLevel$combobox,values=varcombobox3)
  
  tkbind(selectBaseLevel$combobox, "<<ComboboxSelected>>",function(){
    
    value<-getSelection(selectBaseLevel)
    if(value!=gettext("<no level selected>",domain="R-RcmdrPlugin.TeachStat")){
      tclvalue(nameVar) <- paste("index_",make.names(tclvalue(selectBaseLevel$combovar)),sep="")
      # tkconfigure(nameEntry,textvariable=tclVar(paste("index_",value,sep="")))
    } else{
      tclvalue(nameVar) <- ""
      # tkconfigure(nameEntry,textvariable=tclVar(""))
    }
  })
  
  saveVar <- tclVar(dialog.values$initial.save)
  nameVar <- tclVar(dialog.values$initial.name)
  saveFrame <- ttkframe(top)
  saveButton <- ttkcheckbutton(saveFrame, variable=saveVar, text=gettext("Add to data frame",domain="R-RcmdrPlugin.TeachStat"),
                               command = function(){ 
                                 if(tclvalue(saveVar)==1){
                                   tk2state.set(nameEntry, state = "normal")
                                   tk2state.set(nameLabel, state = "normal")
                                 } else{
                                   tk2state.set(nameEntry, state = "disabled")
                                   tk2state.set(nameLabel, state = "disabled")
                                 }
                               })
  
  if(dialog.values$initial.save!=1){
    mostrar3 <- "disabled"
  }else {
    mostrar3<-"normal"
  }
  nameLabel <- labelRcmdr(saveFrame, text=gettext("Variable name:",domain="R-RcmdrPlugin.TeachStat"),state=mostrar3)
  nameEntry <- ttkentry(saveFrame, width="20", textvariable=nameVar,state=mostrar3)
  
  
  onOK <- function(){
    closeDialog()
    .activeDataSet <- ActiveDataSet()
    # Error check
    varValueVar<-getSelection(selectValueVariable)
    varBaseVar<-getSelection(selectBaseVariable)
    varBaseLev<-getSelection(selectBaseLevel)

    dialogNameF <- get(dialogName,mode="function")

    if(varValueVar==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){
      errorCondition(recall=dialogNameF, message=gettext("No index variable selected",domain="R-RcmdrPlugin.TeachStat"))
      return()
    } else if(varBaseVar==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){
      errorCondition(recall=dialogNameF, message=gettext("No base variable selected",domain="R-RcmdrPlugin.TeachStat"))
      return()
    } else if(varBaseLev==gettext("<no level selected>",domain="R-RcmdrPlugin.TeachStat")){
      errorCondition(recall=dialogNameF, message=gettext("No new base level selected",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }

    # Check that there are no repeated values in the base variable
    datasetactivo<-get(.activeDataSet, envir = env)
    vals <- datasetactivo[,varBaseVar]
    
    if(anyDuplicated(vals)!=0){
      errorCondition(recall=dialogNameF, message=gettext("'The base variable cannot have repeated values'",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }

    # command construction
    command <- paste('local({\n  .Sindex <- Sindex(',.activeDataSet,', "',varBaseVar,'", "',varValueVar,'", "',varBaseLev,'")*100', sep="")
    command <- paste(command,'\n  print(.Sindex)',sep="")
    saveval <- tclvalue(saveVar)
    nameval <- trim.blanks(tclvalue(nameVar))
    if(saveval==1){
      if (!is.valid.name(nameval)){
        errorCondition(recall=dialogNameF,
                       message=paste('"', nameval, '" ', gettextRcmdr("is not a valid name."), sep=""))
        return()
      }
      #Check if there exists that name in the data.frame
      if (is.element(nameval, Variables())) {
        if ("no" == tclvalue(checkReplace(nameval))){
          BCindexnumbers()
          return()
        }
      }
      command <- paste(command,'\n  ',.activeDataSet,'$',nameval,' <<- drop(as.matrix(.Sindex))',sep="")
    }
    command <- paste(command,'\n})', sep="")
    result <- doItAndPrint(command)
    #For refreshing the activedataset
    if (saveval==1 && class(result)[1] !=  "try-error") activeDataSet(.activeDataSet, flushModel=FALSE, flushDialogMemory=FALSE)
    tkfocus(CommanderWindow())
    putDialog(dialogName, list(initial.valvar=varValueVar,initial.basvar=varBaseVar,initial.baslevar=varBaseLev,
                               initial.save=saveval,initial.name=nameval))#, resettable=FALSE)

  }
  OKCancelHelp(helpSubject="Sindex", reset = dialogName, apply = dialogName)
  tkgrid(getFrame(selectValueVariable),sticky="w")
  
  tkgrid(getFrame(selectBaseVariable),sticky="w")
  
  tkgrid(getFrame(selectBaseLevel),sticky="w")
  
  tkgrid(varFrame,baseFrame,padx=6,sticky="nw")
  
  tkgrid(selectFrame, sticky="w")
  
  tkgrid(saveButton,nameLabel,nameEntry,padx=3,pady=6,sticky="w")
  
  tkgrid(saveFrame, sticky="w")
  
  tkgrid(buttonsFrame, sticky="ew")
  dialogSuffix()
}


Pindexnumbers <- function(){
  # To ensure that menu name is included in pot file
  gettext("Price index...", domain="R-RcmdrPlugin.TeachStat")
  
  Library("IndexNumR")
  env <- environment()
  dialogName <- "Pindexnumbers"
  novariablesel <- gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")
  defaults <- list (initial.basvar=novariablesel,
                    initial.baslevar=gettext("<no base level selected>",domain="R-RcmdrPlugin.TeachStat"),
                    initial.prodvar=novariablesel,initial.pvar=novariablesel,initial.qvar=novariablesel,initial.las="1",
                    initial.paa="0",initial.fis="0",initial.oth="0", initial.oth.value=""
                    ,initial.save="0", initial.name=""
                    )
  dialog.values <- getDialog (dialogName, defaults)
  initializeDialog(title = gettext("Price Index",domain="R-RcmdrPlugin.TeachStat"))
  
  selectFrame <- ttkframe(top)
  
  varFrame <- ttkframe(selectFrame)
  
  if (length(twoOrMoreLevelFactors())!=0){
    mostrar<-"readonly"
  }else {
    mostrar<-"disabled"
  }
  selectBaseVariable <- variableComboBox(varFrame, variableList=twoOrMoreLevelFactors(), state=mostrar,
                                         initialSelection=dialog.values$initial.basvar, 
                                         title=gettext("Base variable (pick one)",domain="R-RcmdrPlugin.TeachStat"))
  
  tkbind(selectBaseVariable$combobox, "<<ComboboxSelected>>",function(){
    
    value<-getSelection(selectBaseVariable)
    if(value!=novariablesel){
      datasetactivo <- ActiveDataSet()
      datasetactivo<-get(datasetactivo)
      niveles<-levels(datasetactivo[,value])
      tkconfigure(selectBaseLevel$combobox,values=niveles)
      tclvalue(selectBaseLevel$combovar)<-niveles[1]
      tk2state.set(selectBaseLevel$combobox, state="readonly")
      tkfocus(selectBaseLevel$combobox)
    } else{
      tk2state.set(selectBaseLevel$combobox, state="disabled")
      niveles<-gettext("<no level selected>",domain="R-RcmdrPlugin.TeachStat")
      tkconfigure(selectBaseLevel$combobox,values=niveles)
      tclvalue(selectBaseLevel$combovar)<-niveles
    }
  })
  
  if(dialog.values$initial.basvar==novariablesel){
    mostrar2 <- "disabled"
    varcombobox3<-gettext("<no level selected>",domain="R-RcmdrPlugin.TeachStat")
  }else {
    mostrar2<-"readonly"
    datasetactivo <- ActiveDataSet()
    datasetactivo<-get(datasetactivo)
    varcombobox3<-levels(datasetactivo[,dialog.values$initial.basvar])
  }
  
  selectBaseLevel <- variableComboBox(varFrame, variableList=varcombobox3, state=mostrar2,
                                      initialSelection=dialog.values$initial.baslevar, 
                                      title=gettext("Base (pick one)",domain="R-RcmdrPlugin.TeachStat"))
  # Deleting <no variable selected> option from the list
  tkconfigure(selectBaseLevel$combobox,values=varcombobox3)
  
  selectProdVariable <- variableComboBox(varFrame, variableList=twoOrMoreLevelFactors(),state=mostrar,
                                          initialSelection=dialog.values$initial.prodvar, 
                                         title=gettext("Product variable (pick one)",domain="R-RcmdrPlugin.TeachStat"))
  selectPVariable <- variableComboBox(varFrame, variableList=Numeric(),
                                         initialSelection=dialog.values$initial.pvar, 
                                      title=gettext("Price variable (pick one)",domain="R-RcmdrPlugin.TeachStat"))
  selectQVariable <- variableComboBox(varFrame, variableList=Numeric(),
                                         initialSelection=dialog.values$initial.qvar, 
                                      title=gettext("Quantity variable (pick one)",domain="R-RcmdrPlugin.TeachStat"))
  
  
  priceFrame <- ttkframe(selectFrame)
  checkBoxes(priceFrame, frame="checkBoxFrame", boxes=c("las", "paa", "fis"), 
             initialValues=c(dialog.values$initial.las, dialog.values$initial.paa, dialog.values$initial.fis), 
             labels=gettext(c("Laspeyres", "Paasche", "Fisher"),domain="R-RcmdrPlugin.TeachStat"), 
             title=gettext("Aggregation Method",domain="R-RcmdrPlugin.TeachStat"))
  
  othVariable <- tclVar(dialog.values$initial.oth)
  othFrame <- ttkframe(priceFrame)
  othCheckBox <- ttkcheckbutton(othFrame, variable=othVariable, text=gettext("Other:"),
                               command = function(){ 
                                 if(tclvalue(othVariable)==1){
                                   tk2state.set(otherEntry, state = "normal")
                                 } else{
                                   tk2state.set(otherEntry, state = "disabled")
                                 }
                               })
  
  if(dialog.values$initial.oth!=1){
    mostrar2 <- "disabled"
  }else {
    mostrar2<-"normal"
  }
  
  other <- tclVar(dialog.values$initial.oth.value)
  otherEntry <- ttkentry(othFrame, width="20", textvariable=other,state=mostrar2)
  
  
  saveFrame <- ttkframe(priceFrame)
  
  saveVariable <- tclVar(dialog.values$initial.save)
  nameVariable <- tclVar(dialog.values$initial.name)
  
  saveLabel <- labelRcmdr(saveFrame, text=gettext("Save as a new data frame:",domain="R-RcmdrPlugin.TeachStat"),fg=getRcmdr("title.color"))
  saveButton <- ttkcheckbutton(saveFrame, variable=saveVariable,
                               command = function(){
                                 if(tclvalue(saveVariable)==1){
                                   tk2state.set(nameEntry, state = "normal")
                                   tk2state.set(nameLabel, state = "normal")
                                 } else{
                                   tk2state.set(nameEntry, state = "disabled")
                                   tk2state.set(nameLabel, state = "disabled")
                                 }
                               })

  if(dialog.values$initial.save!=1){
    mostrar3 <- "disabled"
  }else {
    mostrar3<-"normal"
  }
  
  nameFrame <- ttkframe(saveFrame)
  nameLabel <- labelRcmdr(nameFrame, text=gettext("name:",domain="R-RcmdrPlugin.TeachStat"),state=mostrar3)
  nameEntry <- ttkentry(nameFrame, width="20", textvariable=nameVariable,state=mostrar3)
  
  onOK <- function(){
    closeDialog()
    .activeDataSet <- ActiveDataSet()
    # Error check
    varProdVar<-getSelection(selectProdVariable)
    varBaseVar<-getSelection(selectBaseVariable)
    varBaseLev<-getSelection(selectBaseLevel)
    varPVar<-getSelection(selectPVariable)
    varQVar<-getSelection(selectQVariable)

    dialogNameF <- get(dialogName,mode="function")

    if(varProdVar==novariablesel){
      errorCondition(recall=dialogNameF, message=gettext("No product variable selected.",domain="R-RcmdrPlugin.TeachStat"))
      return()
    } else if(varBaseVar==novariablesel){
      errorCondition(recall=dialogNameF, message=gettext("No base variable selected.",domain="R-RcmdrPlugin.TeachStat"))
      return()
    } else if(varBaseLev==gettext("<no level selected>",domain="R-RcmdrPlugin.TeachStat")){
      errorCondition(recall=dialogNameF, message=gettext("No base level selected.",domain="R-RcmdrPlugin.TeachStat"))
      return()
    } else if(varPVar==novariablesel){
      errorCondition(recall=dialogNameF, message=gettext("No price variable selected.",domain="R-RcmdrPlugin.TeachStat"))
      return()
    } else if(varQVar==novariablesel){
      errorCondition(recall=dialogNameF, message=gettext("No quantitity variable selected.",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    # 
    oth <- tclvalue(other) 
    lasVar <- tclvalue(lasVariable)
    paaVar <- tclvalue(paaVariable)
    fisVar <- tclvalue(fisVariable)
    othVar <- tclvalue(othVariable)
    
    oth <- paste(gsub(",+", ",", gsub(" ", ",", oth)), sep="")
    othpas <- unlist(strsplit(oth,","))
    
    if(othVar==1 && length(othpas)==0){
      errorCondition(recall=dialogNameF, message=gettext("No other aggregation method was selected.",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    othpas <- paste('"',othpas,'"',collapse=", ",sep="")
    
    IndexMethod <- paste("c(", paste(c('"laspeyres"', '"paasche"', '"fisher"',othpas)[c(lasVar, paaVar, fisVar,othVar) == 1], 
                         collapse=", "), ")", sep="")
    if (IndexMethod == "c()"){
      errorCondition(recall=dialogNameF, message=gettext("No aggregation method selected.",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    
    saveVar <- tclvalue(saveVariable)
    nameVar <- trim.blanks(tclvalue(nameVariable))
    if(saveVar==1 && nameVar==""){
      errorCondition(recall=dialogNameF, message=gettext("No name for data frame.",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    

    # command construction
    command <- paste('local({\n  .df <- priceIndexNum (',.activeDataSet,', prodID ="',varProdVar,'", pervar ="',varBaseVar,
                     '", pvar ="',varPVar,'", qvar ="',varQVar,'", base="',varBaseLev,'", indexMethod =',IndexMethod,')', sep="")
    command <- paste(command,'\n  .df[,-1] <- .df[,-1]*100',sep="")
    command <- paste(command,'\n  print(.df)',sep="")
    
    if(saveVar==1){
      if (!is.valid.name(nameVar)) {
        errorCondition(recall=dialogNameF,
                       message=paste('"', nameVar, '" ', gettextRcmdr("is not a valid name."), sep=""))
        return()
      }
      if (is.element(nameVar, listDataSets())) {
        if ("no" == tclvalue(checkReplace(nameVar, gettextRcmdr("Data set")))){
          Pindexnumbers()
          return()
        }
      }
      command <- paste(command,'\n  ',nameVar,' <<- .df',sep="")
      # Save Data Frame to a file
      savefile <- tclvalue(tkgetSaveFile(filetypes=
                                           gettextRcmdr('{"All Files" {"*"}} {"R Data Files" {".RData" ".rda" ".Rda" ".RDA"}}'),
                                         defaultextension=".RData", initialfile=paste(nameVar, ".RData", sep="")))
      savefile <- removeRedundantExtension(savefile)
      if (savefile == "") return()
      savefile <- sub(".RData.RData$", ".RData", savefile)
      
      cmd <- paste("save(\"",nameVar,"\",file=\"",savefile,"\")",sep="")
      command <- paste(command,'\n  ',cmd,sep="")
    }
    command <- paste(command,'\n})', sep="")
    doItAndPrint(command)
    tkfocus(CommanderWindow())
    putDialog(dialogName, list(initial.basvar=varBaseVar,initial.baslevar=varBaseLev,initial.prodvar=varProdVar,
                               initial.pvar=varPVar,initial.qvar=varQVar,initial.las=lasVar,initial.paa=paaVar,
                               initial.fis=fisVar,initial.oth=othVar, initial.oth.value=oth
                               ,initial.save=saveVar, initial.name=nameVar
                               ))#, resettable=FALSE)
  }
  OKCancelHelp(helpSubject="priceIndexNum", reset = dialogName, apply = dialogName)
  
  
  tkgrid(getFrame(selectBaseVariable),labelRcmdr(varFrame,text="  "),getFrame(selectBaseLevel),sticky="w")
  
  tkgrid(getFrame(selectProdVariable),sticky="w")
  tkgrid(getFrame(selectPVariable),sticky="w")
  tkgrid(getFrame(selectQVariable),sticky="w")
  
  tkgrid(checkBoxFrame,sticky="w")
  tkgrid(othCheckBox, labelRcmdr(othFrame,text="  "),otherEntry, sticky="w")
  tkgrid(othFrame, sticky="w")
  
  tkgrid(saveLabel,saveButton,sticky="w")
  tkgrid(nameLabel,nameEntry,sticky="w")
  tkgrid(nameFrame,columnspan=2,sticky="w")
  tkgrid(saveFrame, pady=20, sticky="w")
  
  tkgrid(varFrame,priceFrame,padx=10,sticky="new")
 
  tkgrid(selectFrame, sticky="w")

  tkgrid(buttonsFrame, sticky="ew")
  dialogSuffix()
}



Cindexnumbers <- function(){
  # To ensure that menu name is included in pot file
  gettext("Complex index numbers...", domain="R-RcmdrPlugin.TeachStat")
  
  env <- environment()
  dialogName <- "Cindexnumbers"
  defaults <- list (initial.x=NULL, initial.ari="0", initial.geo="0", initial.har="0")
  dialog.values <- getDialog (dialogName, defaults)
  initializeDialog(title = gettext("Complex Index Numbers",domain="R-RcmdrPlugin.TeachStat"))
  
  selectFrame <- tkframe(top)
  
  varFrame <- tkframe(selectFrame)
  
  xBox <- variableListBox(varFrame, Numeric(), selectmode="multiple", title=gettextRcmdr("Variables (pick one or more)"),
                         initialSelection=varPosn(dialog.values$initial.x, "numeric"))
  
  avgeFrame <- tkframe(selectFrame)
  checkBoxes(avgeFrame, frame="checkBoxFrame", boxes=c("ari", "geo", "har"), 
             initialValues=c(dialog.values$initial.ari, dialog.values$initial.geo, dialog.values$initial.har), 
             labels=gettext(c("Arithmetic", "Geometric", "Harmonic"),domain="R-RcmdrPlugin.TeachStat"), 
             title=gettext("Aggregation Method",domain="R-RcmdrPlugin.TeachStat"))
  
  onOK <- function(){
    # For list boxes, operetions has to be made before 'closeDialog()' command
    x <- getSelection(xBox)
    
    closeDialog()
    .activeDataSet <- ActiveDataSet()
    
    # Error check
    dialogNameF <- get(dialogName,mode="function")
    if (length(x) == 0){
      errorCondition(recall=dialogNameF, message=gettextRcmdr("You must select a variable."))
      return()
    }
    
    ariVar <- tclvalue(ariVariable)
    geoVar <- tclvalue(geoVariable)
    harVar <- tclvalue(harVariable)
    
    vars <- if (length(x) == 1) paste('"', x, '"', sep="") 
    else paste("c(", paste('"', x, '"', collapse=", ", sep=""), ")", sep="")
    ds.vars <- paste(.activeDataSet, "[,", vars, ", drop=FALSE]", sep="")
    means <- paste("c(", paste(c('"arithmetic"', '"geometric"', '"harmonic"')[c(ariVar, geoVar, harVar) == 1], 
                                     collapse=", "), ")", sep="")
    if (means == "c()"){
      errorCondition(recall=dialogNameF, message=gettext("No aggregation method selected.",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    
    datasetactivo<-get(.activeDataSet, envir = env)
    subds <- datasetactivo[,x,drop=FALSE]
    
    negvars <- x[apply(subds<=0,2,any,na.rm=TRUE)]
    nnegvars <- length(negvars)
    zerovars <- x[apply(subds==0,2,any,na.rm=TRUE)]
    nzerovars <- length(zerovars)
    if(geoVar==1 & nnegvars > 0){
      if(nnegvars==1){
        errorCondition(recall=dialogNameF, message=paste(gettext("Geometric mean cannot be computed for variable",domain="R-RcmdrPlugin.TeachStat"),
                                                         negvars, gettext("because it has non positive values",domain="R-RcmdrPlugin.TeachStat")))
        return()
      }else{
        errorCondition(recall=dialogNameF, message=paste(gettext("Geometric mean cannot be computed for variables",domain="R-RcmdrPlugin.TeachStat"),
                                                         paste(negvars,collapse=", "), gettext("because they have non positive values",domain="R-RcmdrPlugin.TeachStat")))
        return()
      }
      
    }
    if(harVar==1 & nzerovars > 0){
      if(nzerovars==1){
        errorCondition(recall=dialogNameF, message=paste(gettext("Harmonic mean cannot be computed for variable",domain="R-RcmdrPlugin.TeachStat"),
                                                         zerovars, gettext("because it has zero values",domain="R-RcmdrPlugin.TeachStat")))
        return()
      }else{
        errorCondition(recall=dialogNameF, message=paste(gettext("Harmonic mean cannot be computed for variables",domain="R-RcmdrPlugin.TeachStat"),
                                                         paste(zerovars,collapse=", "), gettext("because they have zero values",domain="R-RcmdrPlugin.TeachStat")))
        return()
      }
    }
    
    
    # command construction
    command <- paste('ComplexIN(',ds.vars, ', means=', means,')', sep="")
    doItAndPrint(command)
    tkfocus(CommanderWindow())
    putDialog(dialogName, list(initial.x=x,initial.ari=ariVar,initial.geo=geoVar,
                               initial.har=harVar))#, resettable=FALSE)
  }
  OKCancelHelp(helpSubject="ComplexIN", reset = dialogName, apply = dialogName)
  
  
  tkgrid(getFrame(xBox),sticky="w")
  
  tkgrid(checkBoxFrame,sticky="w")
  
  tkgrid(varFrame,avgeFrame,padx=20,sticky="new")
  
  tkgrid(selectFrame, sticky="w")
  
  tkgrid(buttonsFrame, sticky="ew")
  dialogSuffix()
}



Deflation <- function(){
  # To ensure that menu name is included in pot file
  gettext("Deflation...", domain="R-RcmdrPlugin.TeachStat")
  
  env <- environment()
  
  novariablesel <- gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")
  dialogName <- "Deflation"
  defaults <- list (initial.valvar=novariablesel,initial.defvar=novariablesel,initial.basvar=novariablesel,
                    initial.baslevar=gettext("<no base level selected>",domain="R-RcmdrPlugin.TeachStat"),initial.save="",initial.name="")
  dialog.values <- getDialog (dialogName, defaults)
  initializeDialog(title = gettext("Deflation",domain="R-RcmdrPlugin.TeachStat"))
  
  selectFrame <- ttkframe(top)
  
  varFrame <- ttkframe(selectFrame)
  #comboBoxFrame<-tkframe(varFrame)
  selectValueVariable <- variableComboBox(varFrame, variableList=Numeric(),
                                          initialSelection=dialog.values$initial.valvar, title=gettext("Current value (pick one)",domain="R-RcmdrPlugin.TeachStat"))
  
  selectDeflatVariable <- variableComboBox(varFrame, variableList=Numeric(),
                                          initialSelection=dialog.values$initial.defvar, title=gettext("Deflator (pick one)",domain="R-RcmdrPlugin.TeachStat"))
  
  
  
  baseFrame <- ttkframe(selectFrame)
  # comboBox2Frame<-tkframe(baseFrame)
  
  twoOrMoreLevelFactors<-twoOrMoreLevelFactors() ##NULL SI NO ACTIVEDATASET
  
  if (length(twoOrMoreLevelFactors())!=0){
    mostrar<-"readonly"
  }else {
    mostrar<-"disabled"
  }
  selectBaseVariable <- variableComboBox(baseFrame, variableList=twoOrMoreLevelFactors(), state=mostrar,
                                         initialSelection=dialog.values$initial.basvar, title=gettext("Base variable (pick one)",domain="R-RcmdrPlugin.TeachStat"))
  
  tkbind(selectBaseVariable$combobox, "<<ComboboxSelected>>",function(){
    
    value<-getSelection(selectBaseVariable)
    if(value!=gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){
      datasetactivo <- ActiveDataSet()
      datasetactivo<-get(datasetactivo)
      niveles<-levels(datasetactivo[,value])
      tkconfigure(selectBaseLevel$combobox,values=niveles)
      tclvalue(selectBaseLevel$combovar)<-niveles[1]
      tk2state.set(selectBaseLevel$combobox, state="readonly")
      tclvalue(nameVar) <- paste("const_",make.names(tclvalue(selectBaseLevel$combovar)),sep="")
      tkfocus(selectBaseLevel$combobox)
    } else{
      tk2state.set(selectBaseLevel$combobox, state="disabled")
      niveles<-gettext("<no level selected>",domain="R-RcmdrPlugin.TeachStat")
      tkconfigure(selectBaseLevel$combobox,values=niveles)
      tclvalue(selectBaseLevel$combovar)<-niveles
      tclvalue(nameVar) <-""
    }
  })
  
  if(dialog.values$initial.basvar==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){
    mostrar2 <- "disabled"
    varcombobox3<-gettext("<no level selected>",domain="R-RcmdrPlugin.TeachStat")
  }else {
    mostrar2<-"readonly"
    datasetactivo <- ActiveDataSet()
    datasetactivo<-get(datasetactivo)
    varcombobox3<-levels(datasetactivo[,dialog.values$initial.basvar])
  }
  
  selectBaseLevel <- variableComboBox(baseFrame, variableList=varcombobox3, state=mostrar2,
                                      initialSelection=dialog.values$initial.baslevar, title=gettext("Base (pick one)",domain="R-RcmdrPlugin.TeachStat"))
  # Deleting <no variable selected> option from the list
  tkconfigure(selectBaseLevel$combobox,values=varcombobox3)
  
  tkbind(selectBaseLevel$combobox, "<<ComboboxSelected>>",function(){
    
    value<-getSelection(selectBaseLevel)
    if(value!=gettext("<no level selected>",domain="R-RcmdrPlugin.TeachStat")){
      tclvalue(nameVar) <- paste("const_",make.names(tclvalue(selectBaseLevel$combovar)),sep="")
      # tkconfigure(nameEntry,textvariable=tclVar(paste("index_",value,sep="")))
    } else{
      tclvalue(nameVar) <- ""
      # tkconfigure(nameEntry,textvariable=tclVar(""))
    }
  })
  
  saveVar <- tclVar(dialog.values$initial.save)
  nameVar <- tclVar(dialog.values$initial.name)
  saveFrame <- ttkframe(top)
  saveButton <- ttkcheckbutton(saveFrame, variable=saveVar, text=gettext("Add to data frame",domain="R-RcmdrPlugin.TeachStat"),
                               command = function(){ 
                                 if(tclvalue(saveVar)==1){
                                   tk2state.set(nameEntry, state = "normal")
                                   tk2state.set(nameLabel, state = "normal")
                                 } else{
                                   tk2state.set(nameEntry, state = "disabled")
                                   tk2state.set(nameLabel, state = "disabled")
                                 }
                               })
  
  if(dialog.values$initial.save!=1){
    mostrar3 <- "disabled"
  }else {
    mostrar3<-"normal"
  }
  nameLabel <- labelRcmdr(saveFrame, text=gettext("Variable name:",domain="R-RcmdrPlugin.TeachStat"),state=mostrar3)
  nameEntry <- ttkentry(saveFrame, width="20", textvariable=nameVar,state=mostrar3)
  
  onOK <- function(){
    closeDialog()
    .activeDataSet <- ActiveDataSet()
    # Error check
    varValueVar <- getSelection(selectValueVariable)
    varDeflatVar <- getSelection(selectDeflatVariable)
    varBaseVar <- getSelection(selectBaseVariable)
    varBaseLev <- getSelection(selectBaseLevel)
    
    dialogNameF <- get(dialogName,mode="function")
    
    if(varValueVar==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){
      errorCondition(recall=dialogNameF, message=gettext("No current value variable selected",domain="R-RcmdrPlugin.TeachStat"))
      return()
    } else if(varDeflatVar==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){
      errorCondition(recall=dialogNameF, message=gettext("No deflactor selected",domain="R-RcmdrPlugin.TeachStat"))
      return()
    } else if(varBaseVar==gettext("<no variable selected>",domain="R-RcmdrPlugin.TeachStat")){
      errorCondition(recall=dialogNameF, message=gettext("No base variable selected",domain="R-RcmdrPlugin.TeachStat"))
      return()
    } else if(varBaseLev==gettext("<no level selected>",domain="R-RcmdrPlugin.TeachStat")){
      errorCondition(recall=dialogNameF, message=gettext("No base level selected",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    
    # Check that there are no repeated values in the base variable
    datasetactivo<-get(.activeDataSet, envir = env)
    vals <- datasetactivo[,varBaseVar]
    
    if(anyDuplicated(vals)!=0){
      errorCondition(recall=dialogNameF, message=gettext("'The base variable cannot have repeated values'",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    
    # command construction
    command <- paste('local({\n  .Deflator <- Deflat(',.activeDataSet,', "',varBaseVar,'", "',varValueVar,'", "',varDeflatVar,'", "',varBaseLev,'")', sep="")
    command <- paste(command,'\n  print(.Deflator)',sep="")
    saveval <- tclvalue(saveVar)
    nameval <- trim.blanks(tclvalue(nameVar))
    if(saveval==1){
      if (!is.valid.name(nameval)){
        errorCondition(recall=dialogNameF,
                       message=paste('"', nameval, '" ', gettextRcmdr("is not a valid name."), sep=""))
        return()
      }
      #Check if there exists that name in the data.frame
      if (is.element(nameval, Variables())) {
        if ("no" == tclvalue(checkReplace(nameval))){
          Deflation()
          return()
        }
      }
      # command <- paste(command,'\n  ',.activeDataSet,' <<- cbind(',.activeDataSet,', .Sindex)',sep="")
      command <- paste(command,'\n  ',.activeDataSet,'$',nameval,' <<- drop(as.matrix(.Deflator))',sep="")
    }
    command <- paste(command,'\n})', sep="")
    result <- doItAndPrint(command)
    #For refreshing the activedataset
    if (saveval==1 && class(result)[1] !=  "try-error") activeDataSet(.activeDataSet, flushModel=FALSE, flushDialogMemory=FALSE)
    tkfocus(CommanderWindow())
    putDialog(dialogName, list(initial.valvar=varValueVar,initial.defvar=varDeflatVar,initial.basvar=varBaseVar,
                               initial.baslevar=varBaseLev,initial.save=saveval,initial.name=nameval))#, resettable=FALSE)
    
  }
  OKCancelHelp(helpSubject="Deflat", reset = dialogName, apply = dialogName)
  tkgrid(getFrame(selectValueVariable),sticky="w")
  
  tkgrid(getFrame(selectDeflatVariable),sticky="w")
  
  tkgrid(getFrame(selectBaseVariable),sticky="w")
  
  tkgrid(getFrame(selectBaseLevel),sticky="w")
  
  tkgrid(varFrame,baseFrame,padx=6,sticky="nw")
  
  tkgrid(selectFrame, sticky="w")
  
  tkgrid(saveButton,nameLabel,nameEntry,padx=3,pady=6,sticky="w")
  # tkgrid(saveButton,pady=6,sticky="w")
  tkgrid(saveFrame, sticky="w")
  
  tkgrid(buttonsFrame, sticky="ew")
  dialogSuffix()
}

