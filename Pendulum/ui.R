# ui.R

shinyUI(fluidPage(
    titlePanel("Foucault's Pendulum"),

    sidebarLayout(
        sidebarPanel(h3("Parameters"),
                     br(),
                     h5("Choose the following:"),
                     br(),
                      sliderInput("lat",
                                  label = "Latitude",
                                  min = -90,
                                  max = 90,
                                  value = 49),
                     numericInput("tday",
                                  label = "Length of 1 Day (s)",
                                  value = 86400),
                     numericInput("tmax",
                                  label = "Simulation Time (s)",
                                  value = 200),
                     br(),
                     #actionButton("runbutton",
                     #             label = "Run Simulation"),
                     br(),
                     #h3("Plot"),
                     #actionButton("plotbutton", label = "Make Plot"),
                     br()
                     
                     ),
        mainPanel(
            plotOutput("plot1")
            )
        )

    ))
