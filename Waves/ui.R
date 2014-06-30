library(shiny)
##library(shinyIncubator)

## Define the UI for application shallow_water_wave
shinyUI(navbarPage("Shallow Water Waves: ",
                   tabPanel("1D",
                            ##progressInit(),
                            sidebarLayout(
                                sidebarPanel(h3("Parameters"),
                                             helpText("Set up the parameters that control the wave"),
                                             hr(),
                                             numericInput("gravity", label = "Gravity (m/s^2)", value = 9.81),
                                             hr(),
                                             numericInput("depth", label = "Water Depth (m)", value = 1e3),
                                             hr(),
                                             actionButton("goButton", "Go!"),
                                             hr(),
                                        # Animation with custom interval (in ms) to control speed, plus looping
                                             sliderInput("animation", "Animation:", 1, 1000, 10, step = 10, 
                                                         animate=animationOptions(interval=500, loop=FALSE))
                                             ),
                                mainPanel(
                                    plotOutput("plot1")
                                    )
                                )
                            ),
                   tabPanel("2D",
                            sidebarLayout(
                                sidebarPanel(h3("Parameters"),
                                             helpText("Set up the parameters that control the wave"),
                                             hr(),
                                             numericInput("gravity2", label = "Gravity (m/s^2)", value = 0.10),
                                             hr(),
                                             numericInput("depth2", label = "Water Depth (m)", value = 1e3),
                                             hr(),
                                             actionButton("goButton2", "Go!"),
                                             hr(),
                                        # Animation with custom interval (in ms) to control speed, plus looping
                                             sliderInput("animation2", "Animation:", 1, 1000, 10, step = 10, 
                                                         animate=animationOptions(interval=500, loop=FALSE))
                                             ),
                                mainPanel(
                                    plotOutput("plot2")
                                    )
                                )
                            )
                   )
        )
