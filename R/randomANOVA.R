oneWayAnovaRE <- function () {
  # To ensure that menu name is included in pot file
  gettext("One-Way ANOVA with random effects...", domain="R-RcmdrPlugin.TeachStat")
  
  Library("multcomp")
  Library("abind")
  Library("lme4")
  dialogName <- "oneWayAnovaRE"
  defaults <- list(initial.group = NULL, initial.response = NULL, initial.varest = "mm")
  dialog.values <- getDialog(dialogName, defaults)
  initializeDialog(title = gettext("One-Way Analysis of Variance with random effects", domain="R-RcmdrPlugin.TeachStat"))
  UpdateModelNumber()
  modelName <- tclVar(paste("AoVREModel.", getRcmdr("modelNumber"), 
                            sep = ""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, Factors(), title = gettextRcmdr("Groups (pick one)"), 
                              initialSelection = varPosn(dialog.values$initial.group, "factor"))
  responseBox <- variableListBox(dataFrame, Numeric(), title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "numeric"))
  optionsFrame <- tkframe(top)
  varestCheckBox <- radioButtons(optionsFrame, name = "varest", buttons = c("mm", "ml", "reml"), 
                                 labels = gettext(c("Moments", "Maximum likelihood", 
                                                    "Restricted maximum likelihood (REML)"),
                                                  domain="R-RcmdrPlugin.TeachStat"), 
                                 initialValue = dialog.values$initial.varest, 
                                 title = gettext("Variance estimations:", domain="R-RcmdrPlugin.TeachStat"))
  
    onOK <- function() {
    modelValue <- trim.blanks(tclvalue(modelName))
    if (!is.valid.name(modelValue)) {
      UpdateModelNumber(-1)
      errorCondition(recall = dialogName, message = sprintf(gettextRcmdr("\"%s\" is not a valid name."), 
                                                             modelValue))
      return()
    }
    if (is.element(modelValue, listAOVModels())) {
      if ("no" == tclvalue(checkReplace(modelValue, type = gettextRcmdr("Model")))) {
        UpdateModelNumber(-1)
        tkdestroy(top)
        oneWayAnovaRE()
        return()
      }
    }
    group <- getSelection(groupBox)
    response <- getSelection(responseBox)
    closeDialog()
    if (length(group) == 0) {
      errorCondition(recall = dialogName, message = gettextRcmdr("You must select a groups factor."))
      return()
    }
    if (length(response) == 0) {
      errorCondition(recall = dialogName, message = gettextRcmdr("You must select a response variable."))
      return()
    }
    .activeDataSet <- ActiveDataSet()
    
    doItAndPrint(paste("with(", .activeDataSet, ", numSummary(",
                       response, ", groups=", group, 
                       ", statistics=c(\"mean\", \"sd\")))", sep = ""))
    
    varest <- tclvalue(varestVariable)
    
    if(varest=="mm"){
      command <- paste(modelValue, " <- aovremm(", response, " ~ ", 
                       group, ", data=", .activeDataSet, ")$model", sep = "")
    } else{
      reml <- (varest=="reml")
      command <- paste(modelValue, " <- aovreml(", response, " ~ ( 1 | ", 
                       group, " ), data=", .activeDataSet, ", REML=",reml,
                       ", Lconfint=TRUE)$model", sep = "")
    }
    
    doItAndPrint(command)
    # merMod is not a model that can be treated by Rcmdr for the moment
    if(varest=="mm"){
      activeModel(modelValue)
      putRcmdr("modelWithSubset", FALSE)
    }
    
    putDialog (dialogName, list (initial.group = group, initial.response = response, 
                                 initial.varest = varest))
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "oneWayAnovaRE", model = TRUE, reset = dialogName, apply = dialogName)
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model: ")), 
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w", columnspan = 2)
  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), getFrame(responseBox), sticky = "nw")
 
  tkgrid(varestFrame,sticky = "w")
  tkgrid(dataFrame,optionsFrame, padx="14",sticky = "w")
  tkgrid(buttonsFrame, columnspan=2, sticky = "w")
  dialogSuffix()
}