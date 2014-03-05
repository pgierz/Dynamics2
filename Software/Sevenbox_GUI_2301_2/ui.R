library(shiny)
source('PageStructureFunctions.R')


# Define UI for random distribution application 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Sevenbox"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the br()
  # element to introduce extra vertical spacing
  sidebarPanel(id="sidebar",
          
    tags$head(tags$script(
      HTML('
        Shiny.addCustomMessageHandler("jsCode",
         function(message) {
           console.log(message)
           eval(message.code);
         }
        );'))),
    p(h4("Parameters:")),
    accordionsetPanel(id="Parameter_accordion", 
                      accordionPanel(collapsed=TRUE,
                                     head=list("heat capacity and heat fluxes", span(class="caret", style="vertical-align:middle")), 
                                     numericInput("cpo", "spec. heat cap. water [J/K]", 4200),
                                     numericInput("Ks","tuning param. sens. heat flux", 2.5e13),
                                     numericInput("Kl","tuning param. lat. heat flux", 7.65e17)),
                      accordionPanel(collapsed=TRUE,
                                     head=list(HTML("solar CO<sub>2</sub> effect"), span(class="caret", style="vertical-align:middle")),
                                     numericInput("CFsl",HTML("solar CO<sub>2</sub> effect - south. lat."), 1),
                                     numericInput("CFml",HTML("solar CO<sub>2</sub> effect - mid. lat."), 1),
                                     numericInput("CFnl",HTML("solar CO<sub>2</sub> effect - north. lat."), 1)),
                      accordionPanel(collapsed=TRUE,
                                     head=list("additional fresh water fluxes", span(class="caret", style="vertical-align:middle")), 
                                     numericInput("F3","add. freshwater flux - south. lat. [Sv]", 0),
                                     numericInput("F4","add. freshwater flux - north. lat. [Sv]", 0))
    ),
    Row(modal(id="SpinUpModal", 
          label="Spin-Up", 
          linkLabel="Spin-Up",
          linkClass="btn span12",
          accept_button=actionButtonAdv(inputId="acceptSpinUp", label="Apply",class="btn-info span2 pull-right"),
          Row(Span(class=c(4, "well"),
                   Span(class=c(12, "alert alert-warning"),
                        radioButtons(inputId="initialCondType", 
                                label="Choose, which initial conditions should be used!", 
                                choices=c("default initial conditions"="default", 
                                          "initial conditions from Spin-Up run"="spinUp"))), 
                   p(),
                   conditionalPanel(condition="input.initialCondType == 'spinUp'",
                       Row(
                          numericInputRow(inputId="Nt_SpinUp", 
                                          label="Simulation length of Spin-Up [years]", 
                                          value=1000, 
                                          min=10, 
                                          step=1),
                          helpText("Run the Spin-Up until the system gets into steady state for a given set of parameters."),
                          p(),
                          actionButtonAdv(inputId="runSpinUp", 
                                              label="Run SpinUp", 
                                              class="btn-warning btn-block"),
                          p(),
                          Row(Span(class=c(12, "alert alert-danger"), 
                                  id="SpinUpInfo", 
                                  "Spin-Up is not valid anymore! Re-run it!")))
                     )
                   ),
              Span(class=8, 
                   conditionalPanel(condition="input.initialCondType == 'spinUp'",
                     tabsetPanel(
                       tabPanel("Temperature", 
                                plotOutput("SpinUp_Temp_plot", width="100%", height='auto')), 
                       tabPanel("Salinity", 
                                plotOutput("SpinUp_Sal_plot", width="100%", height='auto')),
                       tabPanel("Ocean Flux", 
                                plotOutput("SpinUp_Flux_plot", width="100%", height='auto')),
                       tabPanel("Atm. heat transp.", 
                                plotOutput("SpinUp_AtmHeatTransp_plot", width="100%", height='auto')),
                       tabPanel("P-E", 
                                plotOutput("SpinUp_PE_plot", width="100%", height='auto')),
                       tabPanel("Surface heat flux", 
                                plotOutput("SpinUp_SHF_plot", width="100%", height='auto'))
                     ) 
                   ),
                   conditionalPanel(condition="input.initialCondType == 'default'",
                                    Row(Span(class=3,""),
                                        Span(class=c(6,"alert alert-info"), "Use default initial conditions")))
                   
                   )))),
    modalBusy(id="SpinUpBusy", title="Spin-Up running...", "Please wait until the spin-up has finished!"),
    Row(Span(class=c(12,"alert alert-warning"),textOutput(outputId="InitInfo"))),
    accordionsetPanel(id="Perturb_accordion", 
                      accordionPanel(collapsed=FALSE,
                                     head=list("Perturbations", span(class="caret", style="vertical-align:middle")), 
                                     selectInput("pert_box", "Choose which box should be perturbed:", 
                                                 choices = c("south", "middle lat", "north", "deep")),
                                     textInput("pert_list", "Salinity Perturbations [PSU]","0.0; 0.1"),
                                     helpText("Separate multiple pertubations by semicolon!"))),
    numericInputRow(inputId="N_t", label="Simulation length [years]",value=1000, min=10, step=1),
    Row(actionButtonAdv("plot_button","Start Simulation",class="btn-warning span12")),
    modalBusy(id="SimBusy", title="Simulation running...", "Please wait until the simulation has finished!"),
    p(),
    conditionalPanel(condition='input.plot_button > 0',
                     Row(downloadButton(outputId='downloadPDF', label='Save results as PDF', class="btn-info pull-right")))


  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Temperature", 
                plotOutput("Temp_plot", width="100%", height='auto')), 
      tabPanel("Salinity", 
               plotOutput("Sal_plot", width="100%", height='auto')),
      tabPanel("Ocean Flow", 
               plotOutput("Flux_plot", width="100%", height='auto')),
      tabPanel("Atm. heat transp.", 
               plotOutput("AtmHeatTransp_plot", width="100%", height='auto')),
      tabPanel("P-E", 
               plotOutput("PE_plot", width="100%", height='auto')),
      tabPanel("Surface heat flux", 
               plotOutput("SHF_plot", width="100%", height='auto'))
    ) 

  )
))
