library(shiny)
source('PageStructureFunctions.R')

 
shinyUI(pageWithSidebar(
  
  headerPanel("Rayleigh Benard LBM GUI"),
  
  sidebarPanel(
    Row(Span(class=12,
             accordionsetPanel(id="SimSetupAcc",
                               accordionPanel(head="Simulation Setup", collapsed=FALSE,
                                              numericInputRow(inputId="lxIn", 
                                                              label="Lx - number of grid points in x direction", 
                                                              value=100, 
                                                              min=4, 
                                                              max=10000, 
                                                              step=1),
                                              numericInputRow(inputId="lyIn", 
                                                              label="Ly - number of grid points in y direction", 
                                                              value=52, 
                                                              min=4, 
                                                              max=10000, 
                                                              step=1),
                                              numericInputRow(inputId="Nt0In", 
                                                              label="Nt0 - simulation length in characteristic time steps", 
                                                              value=50, 
                                                              min=1, 
                                                              max=10000),
                                              numericInputRow(inputId="betaIn", 
                                                              label="beta - coupling parameter between spatial and temporal step size", 
                                                              value=11.18, 
                                                              min=1E-6, 
                                                              max=1E6),
                                              Row(Span(class=c(12, "alert alert-warning"), 
                                                       textOutput(outputId="SimSetupTextOut")
                                                       ))),
                               accordionPanel(head="Simulation parameter", collapsed=TRUE,
                                              numericInputRow(inputId="PrIn", 
                                                              label="Pr - Prandtl number", 
                                                              value=1, 
                                                              min=1E-6, 
                                                              max=1E8),
                                              numericInputRow(inputId="RaIn", 
                                                              label="Ra - Rayleigh number", 
                                                              value=20000, 
                                                              min=1E-6, 
                                                              max=1E8),
                                              Row(Span(class=c(12, "alert alert-warning"), 
                                                       htmlOutput(outputId="SimParaTextOut")
                                              ))),
                               accordionPanel(head="Output options", collapsed=TRUE,
                                              numericInputRow(inputId="NoutIn", 
                                                              label="Nout - number of outputs", 
                                                              value=50, 
                                                              min=2, 
                                                              max=10000, 
                                                              step=1),
                                              Row(Span(class=c(12, "alert alert-warning"), 
                                                       textOutput(outputId="OutOptTextOut")
                                              )))
                               ),
             actionButtonAdv(inputId="runButton",label="Run Simulation", class="btn btn-block btn-info")
             )),
    modalBusy(id="SimModalBusy", title="Simulation running...",
              "Please wait until the simulation has finished!",
              progressBar(id="RbProgressBar", class="progress-striped active")),
    p(),
    accordionsetPanel(id="timeNavPanel", accordionPanel(head="Plot options", collapsed=FALSE,
             selectInput(inputId="PlotVarSelect", 
                         label="Select variable to be plotted", 
                         choices=c("Temperature"="temp",
                                   "Density" = "rho",
                                   "Velocity x-comp" = "ux",
                                   "Velocity y-comp" = "uy",
                                   "absol. Velocity" = "abs_u",
                                   "Vorticity" = "vort")),
             p(),
             Row(Span(class=6, "Click to change Plot type"),
                 Span(class=6, actionButtonAdv(inputId="PlotTypeButton", label="Type", class="btn btn-warning btn -xs btn-block"))),
             checkboxInput(inputId="plotVectors", 
                           label="Plot velocity vector field", 
                           value=FALSE),
             p(),
             Row("Choose time frame:"),
             Row(div(class="input-prepend input-append",
                     tags$head(tags$style(type="text/css",
                                          ".w20{width:20%;}")),
                     actionButtonAdv(inputId="timeFirstButton", label=tags$i(class="icon-fast-backward glyphicon-fast-backward"), class="btn w20"),
                     actionButtonAdv(inputId="timePrevButton", label=tags$i(class="icon-step-backward glyphicon-step-backward"), class="btn w20"),
                     numericInputSolo(inputId="timeCurrIn", class="w20", value=1, min=1, step=1),
                     actionButtonAdv(inputId="timeNextButton", label=tags$i(class="icon-step-forward glyphicon-step-forward"), class="btn w20"),
                     actionButtonAdv(inputId="timeLastButton", label=tags$i(class="icon-fast-forward glyphicon-fast-forward"), class="btn w20")
                 )),
             Row(Span(class=c(12, "alert alert-warning"),
                      textOutput(outputId="currTimeTextOut"))),
             downloadButton(outputId="SaveButton", label="Save Plot", class="btn btn-block btn-primary")))
  ),
  
  mainPanel(
    customJsMsg(),
    
    plotOutput(outputId="RBplot")
    
  )
  
))
