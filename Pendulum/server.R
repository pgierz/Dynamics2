# server.R

shinyServer(function(input, output) {
    # Initial Conditions
    g = 9.81 # m/s**2, acceleration due to gravity
    L = 67 # length of pendulum string
    initial_x = L/100 # initial x coordinate
    initial_y = 0 # initial y
    initial_u = 0 # initial u
    initial_v = 0 # initial v

    # set up vectors for x, x_d, x_dd, and y, y_d, y_dd
    x = vector() # x+x_d*t
    x_d = vector() # x_d + x_dd*t
    x_dd = vector() # 2*Omega*phi*y_d-(g/L)*x
    y = vector() # y+y_d*t
    y_d = vector() # y_d + y_dd*t
    y_dd = vector() # -2*Omega*phi*x_d-(g/L)*y

    
    output$plot1 = renderPlot({
        lat = input$lat # latitude as defined by user
        tday = input$tday # tday as defined by user
        tmax = input$tmax # tmax as defined by user

        # Definitions based on User Variables
        Omega = 2*pi/tday
        phi = lat/180 * pi

        # Define acceleration
        a_x <- function(yd, r){
            ax = 2*Omega*phi*yd-(g/L)*r
            return(ax)
        }

        a_y <- function(xd, r){
            ay = -2*Omega*phi*xd-(g/L)*r
            return(ay)
        }
        
        # Initialize vectors
        x[1] = initial_x
        y[1] = initial_y
        x_d[1] = initial_u
        y_d[1] = initial_v
        x_dd[1] = a_x(y_d[1], x[1])
        y_dd[1] = a_y(x_d[1], y[1])
        
      # if(input$runbutton > 0){
           for (i in 2:tmax) {
               x_dd[i] =  a_x(y_d[i-1], x[i-1])
               y_dd[i] = a_y(x_d[i-1], y[i-1])
               x_d[i] = x_d[i-1] + x_dd[i]
               y_d[i] = y_d[i-1] + y_dd[i]
               x[i] = x[i-1] + x_d[i]
               y[i] = y[i-1] + y_d[i]
                                        
           }
       #}
       #if(input$plotbutton > 0){
           plot(x, y, xlim=c(-1,1), ylim=c(-1,1))
       #}
    })
    
})
