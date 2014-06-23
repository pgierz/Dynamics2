library(shiny)
source('sevenbox.r')
source('sevenbox_mc.R')
source('sevenbox_plot_func.R')

showModal <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').modal('show')"
                                             ,sep="")))
}

hideModal <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').modal('hide')"
                                             ,sep="")))
}

disableButton <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('disabled',true)"
                                             ,sep="")))
}

enableButton <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('disabled',false)"
                                             ,sep="")))
}

hideElement <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('style','display: none;')"
                                             ,sep="")))
}

showElement <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('style','display: block;')"
                                             ,sep="")))
}


shinyServer(function(input, output, session) {
  
  Sim <- reactiveValues(Data=new('list'), running=FALSE)
  SpinUp <- reactiveValues(Data=new('list'), running=FALSE, valid=FALSE, message="No Spin-Up run now!")
  InitialCond <- reactiveValues(useDefault=TRUE,
                                default=new('list'),
                                SpinUp=new('list'))

  
  getInitialCond <- reactive({
    
    input$plot_button
    
    if(isolate(InitialCond$useDefault)){
      #cat("Using default ICs \n")
      return(isolate(InitialCond$default))
    }
    else{
      #cat("Using SpinUp ICs \n")
      return(isolate(InitialCond$SpinUp))
    }
    
  })
  
  get_input <- reactive({
    #execute when plot_button is pressed
    input$plot_button
    
    isolate({input=list(N_t=input$N_t,
                 pert_flag=switch(input$pert_box,
                                    "south" = 'S',
                                    "middle lat" = 'M',
                                    "north" = 'N',
                                    "deep" = 'D'),
                 pert_box=input$pert_box,
                 pert_num=sort(as.numeric(strsplit(input$pert_list, '[;]')[[1]])),
                 cpo=input$cpo,
                 Ks=input$Ks,
                 Kl=input$Kl,
                 CFnl=input$CFnl,
                 CFml=input$CFml,
                 CFsl=input$CFsl,
                 F3=1e6*input$F3,
                 F4=1e6*input$F4)})   

    return(input)
  })
  
  get_inputSpinUp <- reactive({
    #execute when SpinUp button is pressed
    input$runSpinUp
    
    isolate({input=list(N_t=input$Nt_SpinUp,
                        cpo=input$cpo,
                        Ks=input$Ks,
                        Kl=input$Kl,
                        CFnl=input$CFnl,
                        CFml=input$CFml,
                        CFsl=input$CFsl,
                        F3=1e6*input$F3,
                        F4=1e6*input$F4)})   
    
    return(input)
  })
  
  observe({
    input$Nt_SpinUp
    input$cpo
    input$Ks
    input$Kl
    input$CFnl
    input$CFml
    input$CFsl
    input$F3
    input$F4
    
    SpinUp$valid <- FALSE
    SpinUp$message <- "Parameter changed!"
  })
  
  observe({
    
    if((!SpinUp$valid) && (isolate(input$runSpinUp) > 0)){
      showElement("SpinUpInfo", session)
    }
    else{
      hideElement("SpinUpInfo", session)
    }
    
  })
  
  
observe({
  
  if(input$plot_button > 0){
    Sim$running <- TRUE
  }
  
})

observe({
  
  if(Sim$running){
    showModal("SimBusy", session)
  }
  else{
    hideModal("SimBusy", session)
  }
  
})

observe({
  
  if(SpinUp$running){
    showModal("SpinUpBusy", session)
    hideModal("SpinUpModal", session)
  }
  else{
    hideModal("SpinUpBusy", session)
    if (isolate(input$runSpinUp)>0)
      showModal("SpinUpModal", session)
  }
  
})

observe({
  if(input$runSpinUp > 0){
    SpinUp$running <- TRUE
  }
})



observe({
  
  if(SpinUp$running){
    
    disableButton("runSpinUp", session)
    
    SpinUp$Data <- sevenbox_wrapper(ncores=1,
                                 N=get_inputSpinUp()$N_t, 
                                 cpo=get_inputSpinUp()$cpo, 
                                 Ks=get_inputSpinUp()$Ks, 
                                 Kl=get_inputSpinUp()$Kl, 
                                 CFnl=get_inputSpinUp()$CFnl, 
                                 CFml=get_inputSpinUp()$CFml, 
                                 CFsl=get_inputSpinUp()$CFsl, 
                                 F3=get_inputSpinUp()$F3, 
                                 F4=get_inputSpinUp()$F4,
                                 Initial=InitialCond$default)
        
    SpinUp$running <- FALSE
    SpinUp$valid <- TRUE 
  }
  
})

observe({
  
  if(!SpinUp$valid){
    enableButton("runSpinUp", session)
    InitialCond$useDefault <- TRUE
    #updateRadioButtons(session=session, inputId="initialCondType", selected="default initial conditions")
  }
  
})

observe({    
    #no update at startup (plot_button == 0)
    if (Sim$running){
      
      if (exists('n_cores')) ncores = n_cores
      else ncores = 1
      
      #cat(paste("Using ", ncores, "processors \n"))
            
      Sim$Data <- sevenbox_wrapper(ncores=ncores,
                              flag=get_input()$pert_flag,
                              perturbation=get_input()$pert_num,
                              N=get_input()$N_t, 
                              cpo=get_input()$cpo, 
                              Ks=get_input()$Ks, 
                              Kl=get_input()$Kl, 
                              CFnl=get_input()$CFnl, 
                              CFml=get_input()$CFml, 
                              CFsl=get_input()$CFsl, 
                              F3=get_input()$F3, 
                              F4=get_input()$F4,
                              Initial=getInitialCond())
      
      Sim$running <- FALSE

    }

  })

observe({
  
  if(input$acceptSpinUp > 0){

    type = isolate(input$initialCondType)
    
    if(type=="default"){
      InitialCond$useDefault <- TRUE
      SpinUp$message <- ""
      disableButton("acceptSpinUp", session)
     }
    else if(type == "spinUp"){
      InitialCond$SpinUp <- isolate(InitFromSpinUp())
      InitialCond$useDefault <- FALSE
      disableButton("acceptSpinUp", session)
      }
  }
  
})

observe({
  
  type = input$initialCondType
  SUvalid = SpinUp$valid
  
  if(type=="default"){
    enableButton("acceptSpinUp", session)
    }
  else if(type=="spinUp")
    {
      if (SUvalid){
        enableButton("acceptSpinUp", session)
      }
      else{
        disableButton("acceptSpinUp", session)
        }
     }
  
})

InitFromSpinUp <- reactive({
  
  Data = SpinUp$Data
  
  Init_out = new('list')
  
  if (length(Data) > 0){
    max_idx = length(Data$time)
    
    Init_out=list(Tasl = Data$Tasl[max_idx],
                 Taml = Data$Taml[max_idx],
                 Tanl = Data$Tanl[max_idx],
                 Tsl = Data$Tsl[max_idx],
                 Tml = Data$Tml[max_idx],
                 Tnl = Data$Tnl[max_idx],
                 Td = Data$Td[max_idx],
                 Ssl = Data$Ssl[max_idx],
                 Sml = Data$Sml[max_idx],
                 Snl = Data$Snl[max_idx],
                 Sd = Data$Sd[max_idx])
  }
  
  return(Init_out)
  
})

  output$InitInfo<-renderText({
    
    if(InitialCond$useDefault){
      paste("Default Initial conditions used.", SpinUp$message)
    }
    else{
      "Using Initial conditions from Spin-Up run."
    }
  })
  
  output$Temp_plot <- renderPlot({
    data=Sim$Data
    if(isolate(input$plot_button > 0)) {
      plot_box_Temp(data=data, pert=isolate(get_input()$pert_num))
    }
  }, height=function() { session$clientData$output_Temp_plot_width / sqrt(2)})
  
  output$Sal_plot <- renderPlot({
    data=Sim$Data
    if(isolate(input$plot_button > 0)) {
      plot_box_Sal(data=data, pert=isolate(get_input()$pert_num))
    }
  }, height=function() { session$clientData$output_Sal_plot_width / sqrt(2)})
  
  output$Flux_plot <- renderPlot({
    data=Sim$Data
    if(isolate(input$plot_button > 0)) {
      plot_box_Flux(data=data, pert=isolate(get_input()$pert_num))
    }
  }, height=function() { session$clientData$output_Flux_plot_width / sqrt(2)})

  output$AtmHeatTransp_plot <- renderPlot({
    data=Sim$Data
    if(isolate(input$plot_button > 0)) {
      plot_box_AtmHeatTransp(data=data, pert=isolate(get_input()$pert_num))
    }
  }, height=function() { session$clientData$output_AtmHeatTransp_plot_width / sqrt(2)})

output$PE_plot <- renderPlot({
  data=Sim$Data
  if(isolate(input$plot_button > 0)) {
    plot_box_PE(data=data, pert=isolate(get_input()$pert_num))
  }
}, height=function() { session$clientData$output_PE_plot_width / sqrt(2)})

output$SHF_plot <- renderPlot({
  data=Sim$Data
  if(isolate(input$plot_button > 0)) {
    plot_box_SurfHeatFlux(data=data, pert=isolate(get_input()$pert_num))
  }
}, height=function() { session$clientData$output_SHF_plot_width / sqrt(2)})

output$SpinUp_Temp_plot <- renderPlot({
  data=SpinUp$Data
  if(isolate(input$runSpinUp > 0)) {
    plot_box_Temp(data=data)
  }
}, height=function() { session$clientData$output_SpinUp_Temp_plot_width / sqrt(2)})

output$SpinUp_Sal_plot <- renderPlot({
  data=SpinUp$Data
  if(isolate(input$runSpinUp > 0)) {
    plot_box_Sal(data=data)
  }
}, height=function() { session$clientData$output_SpinUp_Sal_plot_width / sqrt(2)})

output$SpinUp_Flux_plot <- renderPlot({
  data=SpinUp$Data
  if(isolate(input$runSpinUp > 0)) {
    plot_box_Flux(data=data)
  }
}, height=function() { session$clientData$output_SpinUp_Flux_plot_width / sqrt(2)})

output$SpinUp_AtmHeatTransp_plot <- renderPlot({
  data=SpinUp$Data
  if(isolate(input$runSpinUp > 0)) {
    plot_box_AtmHeatTransp(data=data)
  }
}, height=function() { session$clientData$output_SpinUp_AtmHeatTransp_plot_width / sqrt(2)})

output$SpinUp_PE_plot <- renderPlot({
  data=SpinUp$Data
  if(isolate(input$runSpinUp > 0)) {
    plot_box_PE(data=data)
  }
}, height=function() { session$clientData$output_SpinUp_PE_plot_width / sqrt(2)})

output$SpinUp_SHF_plot <- renderPlot({
  data=SpinUp$Data
  if(isolate(input$runSpinUp > 0)) {
    plot_box_SurfHeatFlux(data=data)
  }
}, height=function() { session$clientData$output_SpinUp_SHF_plot_width / sqrt(2)})

  
  output$downloadPDF <- downloadHandler(
    filename = 'sevenbox_results.pdf',
    content = function(con) {
      if(input$plot_button > 0) {
          temp <- tempfile()
          on.exit(unlink(temp))
          pdf(file=temp, paper="a4r", title="Sevenbox modell results", width=0, height=0)
          
          par("oma"=c(0,0,2,0))
          plot_box_Temp(data=Sim$Data, pert=get_input()$pert_num)
          title(main="Temperature", line=1, outer=TRUE, cex.main=1.5)
          
          par("oma"=c(0,0,2,0))
          plot_box_Sal(data=Sim$Data, pert=get_input()$pert_num)
          title(main="Salinity", line=1, outer=TRUE, cex.main=1.5)
          
          par("oma"=c(0,0,2,0))
          plot_box_Flux(data=Sim$Data, pert=get_input()$pert_num)
          title(main="Ocean Flow", line=1, outer=TRUE, cex.main=1.5)
          
          par("fig"=c(1/4,1,0,1/3), new=TRUE)
          par("mar" = c(0,0,1.5,1))
                    
          n_char_x= par("pin")[1]/par("cin")[1]
          n_char_y= par("pin")[2]/par("cin")[2]
          
          plot(1, type="n", axes=FALSE, xlab="", ylab="",xlim=c(0,n_char_x), ylim=c(n_char_y-1.5,-1.5), xaxs="i")
          box(which='plot')
          
          text(x=0.01*n_char_x, y=-1, labels="Parameter",adj=0, cex=1.4)
          
          lines(x=c(n_char_x/2,n_char_x/2),y=c(1, round(n_char_y-1.5)))
          
          line=2
          col=1
          
          print_parameter(name="Simulation length", val=get_input()$N_t, val_unit="years",line=line, col=col, nx=n_char_x)
          line = line + 1
          
          print_parameter(name="perturbed Box", val=get_input()$pert_box, line=line, col=col, nx=n_char_x)
          line = line + 1
          
          print_parameter(name="Perturbations", val=get_input()$pert_num, val_unit="PSU",line=line, col=col, nx=n_char_x)
          line = line + 1

          print_parameter(name="Specific heat capacity", val=get_input()$cpo, val_unit="J/K",line=line, col=col, nx=n_char_x)
          line = line + 1
          
          print_parameter(name="tun. Param. sens. heat flux", val=get_input()$Ks, line=line, col=col, nx=n_char_x)
          line = line + 1
          
          print_parameter(name="tun. Param. lat. heat flux", val=get_input()$Kl, line=line, col=col, nx=n_char_x)
          
          
          line = 2
          col=2
          
          print_parameter(name=expression(paste("sol. ", CO[2] , " effect - north")), val=get_input()$CFnl, line=line, col=col, nx=n_char_x)
          line = line + 1

          print_parameter(name=expression(paste("sol. ", CO[2] , " effect - mid lat.")), val=get_input()$CFml, line=line, col=col, nx=n_char_x)
          line = line + 1
          
          print_parameter(name=expression(paste("sol. ", CO[2] , " effect - south")), val=get_input()$CFsl, line=line, col=col, nx=n_char_x)
          line = line + 1
          
          print_parameter(name="F3 - add. freshwater flux", val=get_input()$F3/1e6, val_unit="Sv", line=line, col=col, nx=n_char_x)
          line = line + 1
          
          print_parameter(name="F4 - add. freshwater flux", val=get_input()$F4/1e6, val_unit="Sv", line=line, col=col, nx=n_char_x)
          line = line + 1
          
          par("oma"=c(0,0,2,0))
          plot_box_AtmHeatTransp(data=Sim$Data, pert=get_input()$pert_num)
          title(main="Atmosph. heat transport", line=1, outer=TRUE, cex.main=1.5)
          
          par("oma"=c(0,0,2,0))
          plot_box_PE(data=Sim$Data, pert=get_input()$pert_num)
          title(main="P-E", line=1, outer=TRUE, cex.main=1.5)
          
          par("oma"=c(0,0,2,0))
          plot_box_SurfHeatFlux(data=Sim$Data, pert=get_input()$pert_num)
          title(main="Surface heat flux", line=1, outer=TRUE, cex.main=1.5)
                    
                    
          dev.off()
          bytes <- readBin(temp, "raw", file.info(temp)$size)
          writeBin(bytes, con)
      }
    }
  )
  

})
