## ui.R Project "22_cor plot/ui.R"

library(shiny)
#tabPanelInfo <- source("info.r")$value
shinyUI(pageWithSidebar(
  headerPanel("Correlation and Composite analysis of time series"
  ),
  
  
  sidebarPanel(
    tags$head(
      tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }"),
      tags$style(type="text/css", "select { max-width: 200px; }"),
      tags$style(type="text/css", "textarea { max-width: 200px; }"),
      #tags$style(type="text/css", ".jslider { max-width: 400px; }"),
      #tags$style(type='text/css', ".well { max-width: 350px; }"),
      tags$style(type='text/css', ".span4 { max-width: 350px; }")
    ),  
    wellPanel(
#       conditionalPanel( # Tab 1 only, part 1
#         condition="input.tsp=='info'",
#         helpText("Information")  
#       ),  
      conditionalPanel( # Tab 2 only, part 1
        condition="input.tsp=='ts'",
        uiOutput("ts.Var"),
        uiOutput("ts.Choose")
      ),
      conditionalPanel( # Tab 3 only, part 1
        condition="input.tsp=='field'",
        uiOutput("field.Var"),
        uiOutput("field.Choose"),
        uiOutput("field.Season"),
        uiOutput("field.Area"),
#         uiOutput("field.Area.text"),
        uiOutput("field.Area.latlon")
      ),
      conditionalPanel( # Tab 4 only, part 1
        condition="input.tsp=='cor'",
        uiOutput("cor.Title"),
        uiOutput("cor.Sigtest"),
        uiOutput("cor.Lag"),
        uiOutput("cor.Filter.bol"),
        uiOutput("cor.Filter.type"),
        uiOutput("cor.Filter.attr"),
        uiOutput("cor.Compute")
      ),  
      conditionalPanel ( # Tab 5 only, part 1
        condition="input.tsp=='comp'",
        uiOutput("comp.Title"),
        uiOutput("comp.Lag"),
        uiOutput("comp.Upperlim"),
        uiOutput("comp.Lowerlim"),
        uiOutput("comp.Show")
    )),

#     conditionalPanel( # Tab 1 only, part 2
#       condition="input.tsp=='info'",
#       wellPanel(
#         helpText("1. Choose time series (upload is possible)"),
#         helpText("2. Choose gridded data set, season and area"),
#         helpText("3. Analyse data with correlation or composite analysis")
#       )  
#     ),
    conditionalPanel( # Tab 2 only, part 2 & 3
      condition="input.tsp=='ts'",
      uiOutput("ts.Plot"),
      uiOutput("ts.Plot.dl"),
      uiOutput("ts.Upload")
    ),
    conditionalPanel( # Tab 3 only, part 2 & 3
      condition="input.tsp=='field'",
      uiOutput("field.Show"),
      wellPanel( selectInput('field.show', 'Show', choices = c("Summary", "Plot", "Histogram"), multiple=T)),  
      uiOutput("field.Plot.dl")
    ),
    conditionalPanel( # Tab 4 only, part 2 & 3
      condition="input.tsp=='cor'",
      wellPanel(
        uiOutput("cor.Main"),
        uiOutput("cor.Zlim"),
        uiOutput("cor.Custom.zlim"),
        uiOutput("cor.Shift")
      ),  
      uiOutput("cor.Dl")
    ),
    conditionalPanel ( # Tab 5 only, part 2 
      condition="input.tsp=='comp'",
      wellPanel(
        uiOutput("comp.Shift"),
        uiOutput("comp.pm.Plotoptions"),
        uiOutput("comp.p.Plotoptions"),
        uiOutput("comp.m.Plotoptions")
        #uiOutput("comp.Main"),
      )
    )
    
    
    
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Info",value="info",
        h4("Information"),
        p(style="text-align:justify",'This tool is designed for correlation and composite analysis of time series with gridded data sets.'),
        p(style="text-align:justify",em('2. tab:'),'Choose time series.'),
        p(style="text-align:justify",em('3. tab:'),'Choose gridded data set, season and area.'),
        p(style="text-align:justify",em('4. tab:'),'Correlation analysis.'),
        p(style="text-align:justify",em('5. tab:'),'Composite analysis.'),
        br()
      ),
      
      tabPanel(
        "Choose TS",
        uiOutput("ts.Main.warning"),
        tableOutput("contents"),
        plotOutput("tsPlot"),
        uiOutput("ts.Plot.slider"),
        tableOutput("tsSummary"),
        #wellPanel(uiOutput("ts.Plot.window")),
        #verbatimTextOutput("info"),
        value="ts"
      ),
      
      tabPanel(
        "Choose Field",
        #h4("Choose Field"),
        tableOutput("fieldSummary"),
        uiOutput("field.Plot.slider"),
        uiOutput("fieldWarning"),
        uiOutput("field.Plot"),
        uiOutput("field.Hist"),
        value="field"
      ),      
      tabPanel(
        "Field Correlation",
        uiOutput("cor.Main.warning"),
        uiOutput("cor.Plot.slider"),
        uiOutput("cor.Transfer"),
        plotOutput("corPlot",height="auto"), #,height="auto"),
        value ="cor"
      ),  
      
      tabPanel(
        "Composite Analysis",
        uiOutput("ts.comp.Plot.slider"),
        plotOutput("ts.comp.Plot"),
        uiOutput("comp.pm.Plot"),
        uiOutput("comp.p.Plot"),
        uiOutput("comp.m.Plot"),
        #plotOutput("comp.p.Plot"),
        #plotOutput("comp.m.Plot"),
        #plotOutput("comp.Plot",height="auto"),
        value = "comp"
      ),
      id="tsp"
  ))
))
        