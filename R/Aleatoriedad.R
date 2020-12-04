# Functions taken from other packages:

# Function to be called by Rcmdr to test for randomness using runs.test from tseries packages
randomnessFTest <- function() {
  # Author: M. Munoz-Marquez (package RcmdrPlugin.UCA)
  # Add to Namespace: importFrom("tseries", tseries.runs.test = "runs.test")
  # To ensure that menu name is included in pot file
  gettext("Randomness test for two level factor...", domain="R-RcmdrPlugin.TeachStat")
  # Build dialog
  initializeDialog(title=gettext("Randomness test for two level factor", domain="R-RcmdrPlugin.TeachStat"))
  variablesBox <- variableListBox(top, TwoLevelFactors(), selectmode="single", initialSelection=NULL, title=gettextRcmdr("Variable (pick one)"))
  onOK <- function(){
    x <- getSelection(variablesBox)
    if (length(x) == 0) {
      errorCondition(recall=randomnessNTest, message=gettextRcmdr("No variables were selected."))
      return()
    }
    closeDialog()
    # Apply test
    doItAndPrint(paste("with(", ActiveDataSet(), ", twolevelfactor.runs.test(", x, "))", sep = ""))
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="Randomness test", reset = "randomnessFTest", apply = "randomnessFTest")
  tkgrid(getFrame(variablesBox), sticky="nw")
  tkgrid(buttonsFrame, sticky="w")
  dialogSuffix(rows=6, columns=1)
}

# Function to be called by Rcmdr to test for randomness using runs.test from randtest packages
randomnessNTest <- function() {
  # Author: M. Munoz-Marquez (package RcmdrPlugin.UCA)
  # Add to Namespace: importFrom("randtests", randtests.runs.test = "runs.test")
  # To ensure that menu name is included in pot file
  gettext("Randomness test for numeric variable...", domain="R-RcmdrPlugin.TeachStat")
  # Build dialog
  defaults <- list (initial.plot="0", initial.threshold="")
  dialog.values <- getDialog ("randomnessNTest", defaults)
  initializeDialog(title=gettext("Randomness test for numeric variable", domain="R-RcmdrPlugin.TeachStat"))
  
  variablesFrame<-tkframe(top)
  variablesBox <- variableListBox(variablesFrame, Numeric(), selectmode="single", initialSelection=NULL, title=gettextRcmdr("Variable (pick one)"))
  optionsFrame<-tkframe(top)
  
  plotVar<-tclVar(dialog.values$initial.plot)
  plotCheckBox<-ttkcheckbutton(optionsFrame, variable=plotVar,text=gettext("Show graphically",domain="R-RcmdrPlugin.TeachStat"))
  
  
  thresholdVar<-tclVar(dialog.values$initial.threshold)
  thresholdEntry<-ttkentry(optionsFrame,width="10",textvariable=thresholdVar)
  tkgrid(labelRcmdr(optionsFrame, text=gettext("Cut-point (median by default):",domain="R-RcmdrPlugin.TeachStat"),fg = getRcmdr("title.color"), font="RcmdrTitleFont"),thresholdEntry, sticky="nw")
  tkgrid(plotCheckBox, sticky="nw")
  
  
  
  onOK <- function(){
    x <- getSelection(variablesBox)
    var3<-tclvalue(plotVar)
    var4<-tclvalue(thresholdVar)
    if (length(x) == 0) {
      errorCondition(recall=randomnessNTest, message=gettextRcmdr("No variables were selected."))
      return()
    }
    
    if ((var4!="") && (!is.numeric(type.convert(var4)))){
      errorCondition(recall = randomnessNTest, message = gettext("Cut-point must be numeric or NULL",domain="R-RcmdrPlugin.TeachStat"))
      return()
    }
    
    putDialog ("randomnessNTest", list(initial.plotVar=var3,initial.threshold=var4))
    closeDialog()
    # Apply test
    draw_graph<-as.logical(as.numeric(var3))
    threshold<-if(var4=="") paste0("median(",x,",na.rm=TRUE)") else as.numeric(var4)
    
    doItAndPrint(paste("with(", ActiveDataSet(), ", numeric.runs.test(", x, ", threshold=",threshold,", plot=",draw_graph,"))", sep = ""))
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="Randomness test", reset = "randomnessNTest", apply = "randomnessNTest")
  tkgrid(getFrame(variablesBox), sticky="nw")
  tkgrid(variablesFrame,optionsFrame, sticky="nw")
  tkgrid(buttonsFrame,columnspan=2,sticky="we")
  #dialogSuffix(rows=6, columns=2)
  dialogSuffix()
}

numeric.runs.test <- function(...) randtests.runs.test(...)
twolevelfactor.runs.test <- function(...) tseries.runs.test(...)