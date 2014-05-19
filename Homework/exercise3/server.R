library(shiny)
source('rb_lbm.R')
source('rb_plot_functions.R')


shinyServer(function(input, output, session) {
  
  
  RB <- reactiveValues(data=NULL,
                       length=NULL,
                       t_curr=NULL)
  
  observe({
    
    if(input$runButton > 0){
      
      showModal(id="SimModalBusy" ,session)
      
      lx=isolate(input$lxIn)
      ly=isolate(input$lyIn) 
      N_t0=isolate(input$Nt0In) 
      Pr=isolate(input$PrIn) 
      Ra=isolate(input$RaIn) 
      beta=isolate(input$betaIn)
      N_out=isolate(input$NoutIn)
      
      rb = rb_lbm(lx=lx, 
                  ly=ly, 
                  N_t0=N_t0, 
                  Pr=Pr, 
                  Ra=Ra, 
                  beta=beta, 
                  N_out=N_out,
                  ShinyData=list(id="RbProgressBar",
                                 session=session))
      
      RB$data <- rb
      
      RB$length = length(rb$time)
      
      RB$t_curr = 1
      updateNumericInput(session=session, inputId="timeCurrIn", value=1)
      
      hideModal(id="SimModalBusy" ,session)
    }
    
  })
  
  observe({
    
    if (is.null(RB$length)){
      hideElement(id="timeNavPanel", session)
    }
    else{
      showElement(id="timeNavPanel", session)
    }
    
  })
  
  observe({
    
    if(input$timePrevButton > 0){
      val = floor(isolate(input$timeCurrIn))
      val = val - 1
      if (val <= 0){
        val = 1
      }
      updateNumericInput(session=session, inputId="timeCurrIn", value=val)
    }
    
  })
  
  observe({
    
    if(input$timeNextButton > 0){
      val = floor(isolate(input$timeCurrIn))
      val = val + 1
      if (val > isolate(RB$length)){
        val = isolate(RB$length)
      }
      updateNumericInput(session=session, inputId="timeCurrIn", value=val)
    }
    
  })
  
  observe({
    
    if(input$timeFirstButton > 0){
      val = 1
      updateNumericInput(session=session, inputId="timeCurrIn", value=val)
    }
    
  })
  
  observe({
    
    if(input$timeLastButton > 0){
      val = isolate(RB$length)
      updateNumericInput(session=session, inputId="timeCurrIn", value=val)
    }
    
  })
  
  
  observe({
    
    data_len = RB$length
    
    if(!is.null(data_len)){
      updateNumericInput(session=session, inputId="timeCurrIn", max=data_len)
    }
    
  })
  
  
  observe({
    
    val = floor(input$timeCurrIn)
    data_len = isolate(RB$length)
    
    if (is.null(data_len)){
      data_len = 0
    }
    
    if ((is.numeric(val) && (val > 0) && (val <= data_len))){
      RB$t_curr = val
    }
    
  })
  
  
  observe({
    
    val = RB$t_curr
    data_len = RB$length
    
    if (is.null(val)){
      val = 0
    }
    
    if (is.null(data_len)){
      data_len = 0
    }
    
    if(val <= 1){
      disableButton(id="timePrevButton", session)
      disableButton(id="timeFirstButton", session)
    }
    else{
      enableButton(id="timePrevButton", session)
      enableButton(id="timeFirstButton", session)
    }
    
    if(val >= data_len){
      disableButton(id="timeNextButton", session)
      disableButton(id="timeLastButton", session)
    }
    else{
      enableButton(id="timeNextButton", session)
      enableButton(id="timeLastButton", session)
    }
    
  })
  
  output$currTimeTextOut <- renderText({
    if (!is.null(RB$data)){
      rb_time = RB$data$time[RB$t_curr]
      paste("Current timestep:", rb_time)
    }
  })
  
  output$SimSetupTextOut <- renderText({
    
    N_t0 = input$Nt0In
    delta_x = 1/(input$lyIn -2)
    delta_t = input$betaIn * delta_x * delta_x;
    Tmax = ceiling(N_t0 / delta_t) +1
    
    paste("number of timesteps:", Tmax)
    
  })
  
  output$SimParaTextOut <- renderUI({
    
    N_t0 = input$Nt0In
    delta_x = 1/(input$lyIn -2)
    delta_t = input$betaIn * delta_x * delta_x;
    Pr = input$PrIn
    Ra = input$RaIn
    
    nu = sqrt(Pr/Ra) * delta_t / (delta_x * delta_x);  #kinematic viscosity in lattice units
    k  = sqrt(1/(Ra*Pr)) * delta_t / (delta_x * delta_x);	#thermal diffusivity in lattice units
    
    HTML(paste("&nu;:", formatC(nu, format="g"), "- viscosity [lattice units] <br>", 
               "&alpha;:", formatC(k, format="g"), "- therm. diffusivity [lattice units] <br>"))
    
  })
  
  output$OutOptTextOut <- renderText({
    
    N_t0 = input$Nt0In
    delta_x = 1/(input$lyIn -2)
    delta_t = input$betaIn * delta_x * delta_x;
    Tmax = ceiling(N_t0 / delta_t) +1
    
    out_freq = floor(Tmax/(input$NoutIn-1))
    
    paste("output every", out_freq, "timesteps")
    
  })
  
  observe({
    
    val = !as.logical(input$PlotTypeButton %% 2)
    
    if(val){
      setProp(id="PlotTypeButton", session=session, prop="innerHTML", value="Filled Contour")
    }
    else{
      setProp(id="PlotTypeButton", session=session, prop="innerHTML", value="Contour")
    }
    
  })
  
  
  output$SaveButton <- downloadHandler(
    filename=function(){
      RBdata = isolate(RB$data)
      t_idx = isolate(RB$t_curr)
      plot_type = isolate(input$PlotVarSelect)
      CurTime = RBdata$time[t_idx]
      
      switch(plot_type,
             temp = {var_name="Temperature"},
             rho = {var_name="Density"},
             ux = {var_name="x-Comp. Velocity"},
             uy = {var_name="y-Comp. Velocity"},
             abs_u = {var_name="absol. Velocity"},
             vort = {var_name="Vorticity"})
      
      paste(var_name,"_", CurTime,".png", sep="")
    },
    content=function(fileName){
      png(filename=fileName, width=1024, height=500)
      
      RBdata = isolate(RB$data)
      t_idx = isolate(RB$t_curr)
      plot_type = isolate(input$PlotVarSelect)
      plotVectors = isolate(input$plotVectors)
      filledCont = !as.logical(isolate(input$PlotTypeButton %% 2))
      
      
      if(!is.null(RBdata)){
        plotRBdata(RBdata=RBdata, 
                   t_idx=t_idx, 
                   plot_type=plot_type, 
                   plotVectors=plotVectors, 
                   filledCont=filledCont)
      }
      
      dev.off()
    },
    contentType="image/png"
    
  )
  

  
  output$RBplot <- renderPlot({
    
    RBdata = RB$data
    t_idx = RB$t_curr
    plot_type = input$PlotVarSelect
    plotVectors = input$plotVectors
    filledCont = !as.logical(input$PlotTypeButton %% 2)
    
  
    if(!is.null(RBdata)){
      plotRBdata(RBdata=RBdata, 
                 t_idx=t_idx, 
                 plot_type=plot_type, 
                 plotVectors=plotVectors, 
                 filledCont=filledCont)
    }
    
  })

  
})
