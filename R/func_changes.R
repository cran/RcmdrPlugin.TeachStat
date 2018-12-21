# Small modification to several Rcmdr functions
## - numericToFactor --> add a check box for Make ordered factor
## - binVariable --> add an option to levels names: Range midpoints 
## - reorderFactor --> Reorder level factor for several factors at a time and convert to ordered factor
## - NewDataSet --> Automatically save the file with the data set and load
## - variablesDataSet --> show also the variable type betwen brackets


numericToFactor2 <- function(){
  # Author: J. Fox (Modified by M. A. Mosquera, 22 Oct 18)
  initializeDialog(title=gettextRcmdr("Convert Numeric Variables to Factors"))
  dataFrame <- tkframe(top)
  variableBox <- variableListBox(dataFrame, Numeric(), selectmode="multiple",
                                 title=gettextRcmdr("Variables (pick one or more)"))
  radioButtons(dataFrame, name="levels", buttons=c("names", "numbers"),
               labels=gettextRcmdr(c("Supply level names", "Use numbers")), title=gettextRcmdr("Factor Levels"))
  orderedFrame <- tkframe(top)
  orderedVariable <- tclVar("0")
  orderedCheckBox <- ttkcheckbutton(orderedFrame, variable=orderedVariable)
  factorNameFrame <- tkframe(top)
  factorName <- tclVar(gettextRcmdr("<same as variables>"))
  factorNameField <- ttkentry(factorNameFrame, width="20", textvariable=factorName)
  onOK <- function(){
    variables <- getSelection(variableBox)
    closeDialog()
    facname <- trim.blanks(tclvalue(factorName))
    .activeDataSet <- ActiveDataSet()
    cmd <- paste("apply(", .activeDataSet, "[c(", paste(
      paste('"', variables, '"', sep=""),
      collapse=","), ")], 2, function(x) sort(unique(x)))", sep="")
    levs <- eval(parse(text=cmd), envir=.GlobalEnv)
    sameLevels <- (length(variables) == 1) ||
      ((is.matrix(levs)) && (all(0 == apply(levs, 1, var))))
    if (length(variables) == 0) {
      errorCondition(recall=numericToFactor2, message=gettextRcmdr("You must select a variable."))}
    else command <- paste(.activeDataSet, " <- within(", .activeDataSet, ", {", sep="")
    for (name in variables){
      fname <- if (facname == gettextRcmdr("<same as variables>")) name
      else if (length(variables) == 1) facname
      else paste(facname, name, sep="")
      if (!is.valid.name(fname)){
        errorCondition(recall=numericToFactor2,
                       message=paste('"', fname, '" ', gettextRcmdr("is not a valid name."), sep=""))
        return()
      }
      if (is.element(fname, Variables())) {
        if ("no" == tclvalue(checkReplace(fname))){
          numericToFactor2()
          return()
        }
      }
      levelsType <- tclvalue(levelsVariable)
      env <- environment()
      if (((name == variables[1]) || (!sameLevels)) && (levelsType == "names")){
        values <- sort(unique(eval(parse(text=paste(.activeDataSet, "$", name, sep="")),
                                   envir=.GlobalEnv)))
        nvalues <- length(values)
        if (nvalues > 30) {
          errorCondition(recall=numericToFactor2,
                         message=sprintf(gettextRcmdr("Number of levels (%d) too large."), nvalues))
          return()
        }
        initializeDialog(subdialog,
                         title=paste(gettextRcmdr("Level Names for"),
                                     if(sameLevels && length(variables) > 1) "Factors" else fname))
        names <- rep("", nvalues)
        onOKsub <- function() {
          closeDialog(subdialog)
          for (i in 1:nvalues){
            names[i] <- eval(parse(text=paste("tclvalue(levelName", i, ")", sep="")))
          }
          if (length(unique(names)) != nvalues){
            errorCondition(recall=numericToFactor2,
                           message=gettextRcmdr("Levels names are not unique."))
            return()
          }
          if (any(names == "")){
            errorCondition(recall=numericToFactor2,
                           message=gettextRcmdr("A level name is empty."))
            return()
          }
          assign("labels", paste(paste("'", names, "'", sep=""), collapse=","),
                 envir=env)
        }
        subOKCancelHelp()
        tkgrid(labelRcmdr(subdialog, text=gettextRcmdr("Numeric value")), labelRcmdr(subdialog, text=gettextRcmdr("Level name")), sticky="w")
        for (i in 1:nvalues){
          valVar <- paste("levelName", i, sep="")
          assign(valVar, tclVar(""))
          assign(paste("entry", i, sep=""), ttkentry(subdialog, width="20",
                                                     textvariable=get(valVar)))
          tkgrid(labelRcmdr(subdialog, text=values[i]), get(paste("entry", i, sep="")), sticky="w")
        }
        tkgrid(subButtonsFrame, sticky="w", columnspan=2)
        dialogSuffix(subdialog, focus=entry1, onOK=onOKsub, force.wait=TRUE)
      }
      ordered <- tclvalue(orderedVariable)
      if (levelsType == "names"){
        if (!exists("labels", mode="character")) return()
        cmd <- paste("factor(", name,
                     ", labels=c(", labels, ")",if(ordered==1) ",ordered=TRUE" else "",")", sep="")
        command <- paste(command, "\n  ", fname, " <- ", cmd, sep="")
      }
      else{
        command <- paste(command, "\n  ", fname, " <- as.",if(ordered==1) "ordered" else "factor","(", name, ")", sep="")
      }
    }
    command <- paste(command, "\n})", sep="")
    result <- doItAndPrint(command)
    if (class(result)[1] !=  "try-error") activeDataSet(.activeDataSet, flushModel=FALSE, flushDialogMemory=FALSE)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="factor")
  tkgrid(getFrame(variableBox), labelRcmdr(dataFrame, text="  "), levelsFrame, sticky="nw")
  tkgrid(dataFrame, sticky="w")
  tkgrid(labelRcmdr(factorNameFrame,
                    text=gettextRcmdr("New variable name or prefix for multiple variables:  ")),
         factorNameField, sticky="w")
  tkgrid(factorNameFrame, sticky="w")
  tkgrid(orderedCheckBox, labelRcmdr(orderedFrame, text=gettextRcmdr("Make ordered factor")), sticky="w")
  tkgrid(orderedFrame, sticky="w")
  tkgrid(buttonsFrame, sticky="ew", columnspan=2)
  tkgrid.configure(numbersButton, sticky="w")
  tkgrid.configure(namesButton, sticky="w")
  dialogSuffix(preventGrabFocus=TRUE)
}

binVariable2 <- function () {
  # Author: Dan Putler (revision by J. Fox, 2 Feb 05) (Modified by M. A. Mosquera, 22 Oct 18)
  defaults <- list (initial.levels = "specify", initial.bins = "3", initial.varName = NULL, 
                    initial.newVar = "variable", initial.method = "intervals")
  dialog.values <- getDialog ("binVariable2", defaults)
  env <- environment()
  initializeDialog(title = gettextRcmdr("Bin a Numeric Variable"))
  variableFrame <- tkframe(top)
  variableBox <- variableListBox(variableFrame, Numeric(), 
                                 title = gettextRcmdr("Variable to bin (pick one)"), 
                                 initialSelection = varPosn (dialog.values$initial.varName, "numeric"))
  newVariableFrame <- tkframe(variableFrame)
  newVariableName <- tclVar(dialog.values$initial.newVar)
  newVariable <- ttkentry(newVariableFrame, width = "18", textvariable = newVariableName)
  binsFrame <- tkframe(top)
  binsVariable <- tclVar(dialog.values$initial.bins)
  slider <- tkscale(binsFrame, from = 2, to = 20, showvalue = TRUE, 
                    variable = binsVariable, resolution = 1, orient = "horizontal")
  optionsFrame <- tkframe(top)
  radioButtons(optionsFrame, name = "levels", buttons = c("specify", 
                                                          "numbers", "ranges", "midpoints"), labels = c(gettextRcmdr(c("Specify names","Numbers", "Ranges")),
                                                                                                        gettext("Ranges midpoints",domain="R-RcmdrPlugin.TeachStat")),
               title = gettextRcmdr("Level Names"),initialValue = dialog.values$initial.levels)
  radioButtons(optionsFrame, name = "method", buttons = c("intervals", 
                                                          "proportions", "natural"), labels = gettextRcmdr(c("Equal-width bins", 
                                                                                                             "Equal-count bins", "Natural breaks\n(from K-means clustering)")), 
               title = gettextRcmdr("Binning Method"), 
               initialValue = dialog.values$initial.method)
  onOK <- function() {
    levels <- tclvalue(levelsVariable)
    bins <- as.numeric(tclvalue(binsVariable))
    varName <- getSelection(variableBox)
    closeDialog()
    if (length(varName) == 0) {
      errorCondition(recall = binVariable2, message = gettextRcmdr("You must select a variable."))
      return()
    }
    newVar <- tclvalue(newVariableName)
    if (is.element(newVar, Variables())) {
      if ("no" == tclvalue(checkReplace(newVar))) {
        binVariable2()
        return()
      }
    }
    if (!is.valid.name(newVar)) {
      errorCondition(message = paste("\"", newVar, "\" ", 
                                     gettextRcmdr("is not a valid name."), sep = ""), 
                     recall = binVariable2)
      return()
    }
    method <- tclvalue(methodVariable)
    putDialog ("binVariable2", list (initial.levels = levels, initial.bins = bins, initial.varName = varName, 
                                    initial.newVar = newVar, initial.method = method))
    if (levels == "specify") {
      initializeDialog(subdialog, title = gettextRcmdr("Bin Names"))
      onOKsub <- function() {
        closeDialog(subdialog)
        level <- character(bins)
        for (i in 1:bins) {
          level[i] <- eval(parse(text = paste("tclvalue(levelName", 
                                              i, ")", sep = "")))
        }
        if (length(unique(level)) != length(level)) {
          errorCondition(window = subdialog, message = gettextRcmdr("Level names must be unique."), 
                         recall = onOK)
          return()
        }
        assign("levelNames", level, envir = env)
      }
      subOKCancelHelp()
      tkgrid(labelRcmdr(subdialog, text = gettextRcmdr("Bin"), 
                        fg = getRcmdr("title.color"), font="RcmdrTitleFont"), labelRcmdr(subdialog, text = gettextRcmdr("Name"), 
                                                                                         fg = getRcmdr("title.color"), font="RcmdrTitleFont"), sticky = "w")
      for (i in 1:bins) {
        valVar <- paste("levelName", i, sep = "")
        assign(valVar, tclVar(i))
        assign(paste("entry", i, sep = ""), ttkentry(subdialog, 
                                                     width = "20", textvariable = get(valVar)))
        tkgrid(labelRcmdr(subdialog, text = as.character(i)), 
               get(paste("entry", i, sep = "")), sticky = "w")
      }
      tkgrid(subButtonsFrame, sticky = "w", columnspan = 2)
      dialogSuffix(subdialog, focus = entry1, bindReturn = FALSE, force.wait=TRUE)
    }
    labels <- if (levels == "numbers") 
      "FALSE"
    else if (levels == "ranges" || levels=="midpoints") 
      "NULL"
    else {
      if (!exists("levelNames")) {
        onCancel()
        binVariable2()
        return()
      }
      paste("c('", paste(levelNames, collapse = "','"), 
            "')", sep = "")
    }
    .activeDataSet <- ActiveDataSet()
    command <- paste0(.activeDataSet, "$", newVar, " <- ", 
                     "as.ordered(with(", .activeDataSet, ", binVariable(", varName, ", bins=", 
                     bins, ", method=", "'", method, "', labels=", labels, 
                     ")))")
    
    if (levels=="midpoints"){
      command<-paste0(command,"\n","levels(",.activeDataSet, "$", newVar,")<-local({\n \t",
                     ".le <- read.table(text=gsub(\"[^-.0-9]\", \" \", levels(",
                     .activeDataSet, "$", newVar,")))\n \t",
                     "if(is.factor(.le$V1)){\n \t\t",
                     "levels(.le$V1)[1] <- min(",.activeDataSet, "$", newVar,")\n\t\t",
                     ".le$V1 <- as.numeric(levels(.le$V1)[.le$V1])\n \t",
                     "}\n \t",
                     "with(.le,(V1+V2)/2)\n","})\n",.activeDataSet, "$", newVar, " <- ", 
                     "as.numeric(levels(",.activeDataSet, "$", newVar,"))[",.activeDataSet, "$", newVar,"]")
    }
    logger(command)
    result <- justDoIt(command)
    if (class(result)[1] != "try-error") 
      activeDataSet(.activeDataSet, flushModel = FALSE, 
                    flushDialogMemory = FALSE)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "bin.var", reset = "binVariable2")
  tkgrid(labelRcmdr(newVariableFrame, text = gettextRcmdr("New variable name"), 
                    fg = getRcmdr("title.color"), font="RcmdrTitleFont"), sticky = "w")
  tkgrid(newVariable, sticky = "w")
  tkgrid(getFrame(variableBox), labelRcmdr(variableFrame, text = "    "), 
         newVariableFrame, sticky = "nw")
  tkgrid(variableFrame, sticky = "w")
  tkgrid(labelRcmdr(binsFrame, text = gettextRcmdr("Number of bins:")), 
         slider, sticky = "s")
  tkgrid(binsFrame, sticky = "w")
  tkgrid(levelsFrame, labelRcmdr(optionsFrame, text = "    "), 
         methodFrame, sticky = "nw")
  tkgrid(optionsFrame, sticky = "w")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix()
}


reorderFactors <- function(){
  # Author: J. Fox (Modified by M. A. Mosquera, 22 Oct 18)
  initializeDialog(title=gettextRcmdr("Reorder Factor Levels"))
  variableBox <- variableListBox(top, Factors(), selectmode="multiple", 
                                 title=gettext("Factor (pick one or more)",domain="R-RcmdrPlugin.TeachStat"))
  orderedFrame <- tkframe(top)
  orderedVariable <- tclVar("0")
  orderedCheckBox <- ttkcheckbutton(orderedFrame, variable=orderedVariable)
  factorName <- tclVar(gettextRcmdr("<same as original>"))
  factorNameField <- ttkentry(top, width="20", textvariable=factorName)
  onOK <- function(){
    variables <- getSelection(variableBox)
    closeDialog()
    
    facname <- trim.blanks(tclvalue(factorName))
    .activeDataSet <- ActiveDataSet()
    cmd <- paste("apply(", .activeDataSet, "[c(", paste(
      paste('"', variables, '"', sep=""),
      collapse=","), ")], 2, function(x) sort(unique(x)))", sep="")
    levs <- eval(parse(text=cmd), envir=.GlobalEnv)
    sameLevels <- (length(variables) == 1) ||
      ((is.matrix(levs)) && (all(1 == apply(levs, 1, function(x) length(unique(x))))))
    
    if (length(variables) == 0) {
      errorCondition(recall=reorderFactors, message=gettextRcmdr("You must select a variable."))
      return()
    }
    else command <- paste(.activeDataSet, " <- within(", .activeDataSet, ", {", sep="")
    # name <- trim.blanks(tclvalue(factorName))
    for (name in variables){
      fname <- if (facname == gettextRcmdr("<same as original>")) name
      else if (length(variables) == 1) facname
      else paste(facname, name, sep="")
      if (!is.valid.name(fname)){
        errorCondition(recall=reorderFactors,
                       message=paste('"', fname, '" ', gettextRcmdr("is not a valid name."), sep=""))
        return()
      }
      if (is.element(fname, Variables())) {
        if ("no" == tclvalue(checkReplace(fname))){
          reorderFactors()
          return()
        }
      }
      env <- environment()
      if ((name == variables[1]) || (!sameLevels)){
        old.levels <- eval(parse(text=paste("levels(", .activeDataSet, "$", name, ")",
                                            sep="")), envir=.GlobalEnv)
        nvalues <- length(old.levels)
        if (nvalues > 30) {
          errorCondition(recall=reorderFactors,
                         message=sprintf(gettextRcmdr("Number of levels (%d) too large."), nvalues))
          return()
        }
        initializeDialog(subdialog, title=paste(gettextRcmdr("Reorder Levels for"),
                                                if(sameLevels && length(variables) > 1) "Factors" else fname))
        order <- 1:nvalues
        onOKsub <- function() {
          closeDialog(subdialog)
          opt <- options(warn=-1)
          for (i in 1:nvalues){
            order[i] <- as.numeric(eval(parse(text=paste("tclvalue(levelOrder", i, ")", sep=""))))
          }
          options(opt)
          if (any(sort(order) != 1:nvalues) || any(is.na(order))){
            errorCondition(recall=reorderFactors,
                           message=paste(gettextRcmdr("Order of levels must include all integers from 1 to "), nvalues, sep=""))
            return()
          }
          assign("levels",old.levels[order(order)],envir=env)
        }
        subOKCancelHelp()
        tkgrid(labelRcmdr(subdialog, text=gettextRcmdr("Old Levels"), fg=getRcmdr("title.color"), font="RcmdrTitleFont"),
               labelRcmdr(subdialog, text=gettextRcmdr("New order"), fg=getRcmdr("title.color"), font="RcmdrTitleFont"), sticky="w")
        for (i in 1:nvalues){
          valVar <- paste("levelOrder", i, sep="")
          assign(valVar, tclVar(i))
          assign(paste("entry", i, sep=""), ttkentry(subdialog, width="2",
                                                     textvariable=get(valVar)))
          tkgrid(labelRcmdr(subdialog, text=old.levels[i]), get(paste("entry", i, sep="")), sticky="w")
        }
        tkgrid(subButtonsFrame, sticky="w", columnspan=2)
        dialogSuffix(subdialog, focus=entry1, onOK=onOKsub, force.wait=TRUE)
      }
      ordered <- tclvalue(orderedVariable)
      ordered <- if (ordered == "1") ", ordered=TRUE" else ""
      cmd <- paste("factor(", name,
                   ", levels=c(", paste(paste("'", levels, "'", sep=""), collapse=","), ")",
                   ordered, ")", sep="")
      command <- paste(command, "\n  ", fname, " <- ", cmd, sep="")
    }
    command <- paste(command, "\n})", sep="")
    result <- doItAndPrint(command)
    if (class(result)[1] !=  "try-error") activeDataSet(.activeDataSet, flushModel=FALSE, flushDialogMemory=FALSE)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="factor")
  tkgrid(getFrame(variableBox), sticky="nw")
  tkgrid(labelRcmdr(top, text=gettextRcmdr("New variable name or prefix for multiple variables:  ")), sticky="w")
  tkgrid(factorNameField, sticky="w")
  tkgrid(orderedCheckBox, labelRcmdr(orderedFrame, text=gettextRcmdr("Make ordered factor")), sticky="w")
  tkgrid(orderedFrame, sticky="w")
  tkgrid(buttonsFrame, sticky="w")
  dialogSuffix(preventGrabFocus=TRUE)
}


newDataSet2 <- function() {
  # Author: J. Fox (Modified by M. A. Mosquera, 22 Oct 18)
  initializeDialog(title=gettextRcmdr("New Data Set"))
  dsname <- tclVar("Dataset")
  entryDsname <- ttkentry(top, width="20", textvariable=dsname)
  onOK <- function(){
    dsnameValue <- trim.blanks(tclvalue(dsname))
    if (dsnameValue == "") {
      errorCondition(recall=newDataSet2,
                     message=gettextRcmdr("You must enter the name of a data set."))
      return()
    }
    if (!is.valid.name(dsnameValue)) {
      errorCondition(recall=newDataSet2,
                     message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
      return()
    }
    if (is.element(dsnameValue, listDataSets())) {
      if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
        newDataSet2()
        return()
      }
    }
    closeDialog()
    command <- paste("editDataset(dsname='", dsnameValue, "')", sep="")
    result <- justDoIt(command)
    if (class(result)[1] !=  "try-error"){
      if (!getRcmdr("dataset.modified")) return()
      .data <- try(get(dsnameValue, envir=.GlobalEnv), silent=TRUE)
      if (nrow(.data) == 0){
        errorCondition(recall=newDataSet2, message=gettextRcmdr("empty data set."))
        return()
      }
      # Automatically save
      # wdir <- getwd()
      # savefile <- paste(wdir, "/", dsnameValue,".RData", sep="")
      # Ask for a name to save
      savefile <- tclvalue(tkgetSaveFile(filetypes=
                                           gettextRcmdr('{"All Files" {"*"}} {"R Data Files" {".RData" ".rda" ".Rda" ".RDA"}}'),
                                         defaultextension=".RData", initialfile=paste(activeDataSet(), ".RData", sep="")))
      savefile <- removeRedundantExtension(savefile)
      if (savefile == "") return()
      savefile <- sub(".RData.RData$", ".RData", savefile)
      
      cmd <- paste("save(\"",activeDataSet(),"\",file=\"",savefile,"\")",sep="")
      eval(parse(text=cmd))
      
      
      
      command <- paste('load("',savefile, '")', sep="")
      doItAndPrint(command)
    }
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="editDataset")
  tkgrid(labelRcmdr(top, text=gettextRcmdr("Enter name for data set:")), entryDsname, sticky="e")
  tkgrid(buttonsFrame, columnspan="2", sticky="w")
  tkgrid.configure(entryDsname, sticky="w")
  dialogSuffix(focus=entryDsname)
}

variablesandtypesDataSet <- function(){
  # To ensure that menu name is included in pot file
  gettext("Variables and types in active data set", domain="R-RcmdrPlugin.TeachStat")
  
  doItAndPrint(paste0("listTypesVariables(\"", ActiveDataSet(), "\")"))
}