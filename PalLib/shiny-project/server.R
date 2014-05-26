## server.R Project "22_cor plot/server.R"
## runApp("t:/weinschmeckt/App-fieldcor")
library(shiny)
source("standard.shiny.R")
db.ts.c<-db.ts  ## custom data base of TS for this run

## Seasons for Field choice
seasons.print <- c("Annual", "DJF", "MAM", "JJA", "SON", "01 Jan", "02 Feb", "03 Mar", "04 Apr", "05 May", "06 Jun", "07 Jul", "08 Aug", "09 Sep", "10 Oct", "11 Nov", " 12 Dec", "Apr-Aug")
seasons.int <- c("annual", "DJF", "MAM", "JJA", "SON", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "AAT")

## Palette for plot
palette=colorRampPalette(c("darkblue","blue","steelblue", "green","white", "yellow","orange","red","brown"))
plot.bg="grey90"

### To save uploaded TS into data base db.ts
nr.ts<-1
save.ts2db <- function(ts1) {
  name.ts<<-paste("custom.ts.",nr.ts,sep="")
  print(paste("[write2db (func)]:  Name of saves custom ts: ",name.ts))
  assign(name.ts,ts1, envir = .GlobalEnv) 
  #browser()
  db.ts.c <<- rbind(db.ts.c,data.frame(ts.name=name.ts,variable=factor("Custom"),name=attr(ts1,"name"),location=NA, start=start(ts1)[1], end=end(ts1)[1], lat=attr(ts1,"lat"), lon=attr(ts1,"lon"),na.count=sum(is.na(ts1)),na.string=NA,sep=NA,dec=NA,path=NA,stringsAsFactors=FALSE)) 
  nr.ts<<-nr.ts+1
}







shinyServer(function(input, output) {
  
  
  
  re.val <- reactiveValues()
  re.val[["db"]] <- 0
  re.val[["upload"]] <- FALSE
  re.val[["cor.field"]] <- FALSE
  re.val[["fieldTstep.warning"]] <- FALSE
  re.val[["plot.command.ts"]] <- NULL
  re.val[["plot.command.field"]] <- NULL
  re.val[["plot.command.cor"]] <- NULL
  re.val[["result.shift"]] <- FALSE
  
  
  
  observe ({
    if (!is.null(input$ts.choose)&&!is.null(fieldInput())) re.val[["cor.field"]] <- TRUE
    else re.val[["cor.field"]] <- FALSE
  })

  observe ({
    if (!is.null(fieldArea()))  print(paste("fieldArea = ", fieldArea()))
    
  })
  
  observe ({
    if (re.val$cor.field){
      area <- fieldArea()
      area.db <-  subset(db.field,db.field$name==input$field.choose)$area
      if ((is.null(area)&& area.db %in% c("Global","Northern Hemisphere"))||(!is.null(area)&&area["lon1"]==0&&area["lon2"]==360)){
        re.val[["result.shift"]] <- TRUE      
      } else re.val[["result.shift"]] <- FALSE
    } else re.val[["result.shift"]] <- FALSE
  })
  


     
############### $$ TIME SERIES (tab 2) ##############################      
  
##############  __$ Side Panel ____Part 1 #################
  
  output$ts.Var <- renderUI({
    #print("[output$ts.Var (reUI)]: executed")
    print(paste("[output$ts.Var (reUI)] executed: re.val$db is: ",re.val$db))
    ts.vars <- c(levels(db.ts.c$variable), "Upload Time Series")
    if (re.val$db==0) sel = "GHD" else sel = "Custom"
    selectInput("ts.var","Choose Variable:", choices=ts.vars, selected=sel)
  })
   
  output$ts.Choose <- renderUI({
    if(length(input$ts.var)&&input$ts.var!="Upload Time Series"){
      selectInput("ts.choose", "Choose a time series:", choices = subset(db.ts.c, db.ts.c$variable==input$ts.var)$name)
    }
  })

#  
################ ____  Part 2  Choose TS #################
#                     


  output$ts.Plot<- renderUI({
    print("[output$ts.Choose (reUI)] executed ")
    if(!re.val$upload)
       wellPanel(
         textInput("ts.plot.title", "Title of plot", attr(tsInput(), "name")),
         textInput("ts.plot.xlab", "X-label", "Time"),
         textInput("ts.plot.ylab", "Y-label", isolate(input$ts.var))
    )
  })


     
#  
################ ____  Part 2 Upload      #################
#                       

  output$ts.Upload <- renderUI({
    if(re.val$upload)
      wellPanel(
        h5("Upload a custom Time Series"),
        helpText("You can upload your custom time series as plain text or .csv file.",
                "You have to specify seperator and NA string.",
                "The structure of the file should be: 1.Column Year 2.Column Data.",
                "One Header line is allowed (click Header checkbox)"),
        fileInput('read.ts', '', #'Upload a Time Series',
                 accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
        textInput("NAstring","String for NA values",""),
        tags$style(type="text/css", '#NAstring {width: 20px}'),
        radioButtons('sep', 'Separator', c( Tab='\t', Comma=',', Semicolon=';', Space = ''),'Tab'),
        radioButtons('dec', 'Decimal Point', c( Dot='.', Comma=','),'Dot'),
        checkboxInput('header', 'Header', FALSE),
        h5("Specify Data of Time Series"),
        textInput("upload.name","Name",paste("Custom TS",nr.ts)),
        div(class="row-fluid", div(class="span6", numericInput("lat","Latitude",0)), div(class="span6", numericInput("lon","Longitude",0))),
        tags$style(type="text/css", '#lat {width: 20px}'),
        tags$style(type="text/css", '#lon {width: 20px}'),
        radioButtons('preview', 'Preview', c(Show.Table='table',Show.Plot='plot'),'Show.Table'),
        actionButton("saveTsButton", "Save this TS")
    )                  
  })
  
#  
############### ____  Part 3 Download Plot   ##################
#  
  
  output$ts.Plot.dl <- renderUI({
    if(!re.val$upload)
      wellPanel(
        downloadButton("ts.plot.dl", "Download Graphic")
      )
  })  
  
  output$ts.plot.dl <- downloadHandler(
    filename <- function() {
      paste(input$ts.plot.title,".pdf",sep="")
    },  
    content <- function(file){
      pdf(file = file, width=11, height=8.5)
      eval(parse(text = re.val$plot.command.ts))
      dev.off()
    }  
  )   
  



############### __$ Reactive Functions  ################     
    
     

   tsInput <- reactive({
     sub.db <- subset(db.ts.c,db.ts.c$name==input$ts.choose)  
     if(dim(sub.db)[1]==1) eval(parse(text = sub.db$ts.name))
     else return(NULL)
   })     
   
     
  readTable <- reactive({
    inFile <- input$read.ts
    if (is.null(inFile)) return(NULL)
    read.csv(inFile$datapath, header=input$header, sep=input$sep, dec=input$dec, na.strings = input$NAstring)
  })     

     
  tsUpload <- reactive({
    temp<-readTable()
    if (!is.null(temp)&&dim(temp)[2]==2&&class(temp[,2])!="factor") pTs(temp[,2],temp[,1],lat=input$lat,lon=input$lon,name=input$upload.name)
    else NULL
  })

  observe({
    if (length(input$ts.var)){
      if (input$ts.var=="Upload Time Series") re.val[["upload"]] <- TRUE
      else re.val[["upload"]] <- FALSE
      print(paste("re.val$upload=",re.val$upload))
    }
  })
  
  observe({                           # Function to write database db.ts.custom when new TS is uploaded    
    if (!is.null(input$saveTsButton)&&input$saveTsButton!=0&&!is.null(isolate(tsUpload()))) {
      #browser()
      isolate(save.ts2db(tsUpload()))
      print("[write2db (obs)]: Now TS is saved")
      print(paste("[write2db (obs)]: Updated custom Database:"))
      print(db.ts.c)
      re.val[["db"]] <- isolate(re.val[["db"]] + 1) # perform update of reactive functions now (after new db is written)
      print(paste("[write2db (obs)]: Value of SaveTsButton:",input$saveTsButton))  
  }})

########## __$ Main Panel ########
  
  
  output$ts.Main.warning <- renderUI({
    if(length(input$ts.var)&&input$ts.var=="Upload Time Series"&&!is.null(readTable())){
      if(dim(readTable())[2]!=2) {
        warning("Choosen data ist not two dimensional as required")
        warning.text<-"Error: Choosen data ist not two dimensional as required (check Separator)"
      } else if(class(readTable()[,2])=="factor"){
        warning("Wrong Decimal Point or NA-value choosen")
        warning.text<-"Error: Wrong Decimal Point or NA-value choosen"
      } else return(NULL)
    } else return(NULL)
    h5(warning.text)  
  })   
  
  
  
  output$tsPlot <- renderPlot({
    if(!re.val$upload&&!is.null(tsInput())){
      t.time<-c(start(tsInput())[1],end(tsInput())[1])
      s.time<-input$ts.plot.slider
      print("[output$tsPlot (rePlot)]: input$ts.plot.slider:")
      print(s.time)
      if (!is.null(s.time)&&(s.time[1] < t.time[1] || s.time[2] > t.time[2])) return(NULL)
      re.val[["plot.command.ts"]] <<- paste("plot(window(tsInput(),",s.time[1],",",s.time[2],"),main=input$ts.plot.title,xlab=input$ts.plot.xlab,ylab=input$ts.plot.ylab)",sep="")
      plot(window(tsInput(),s.time[1],s.time[2]),main=input$ts.plot.title,xlab=input$ts.plot.xlab,ylab=input$ts.plot.ylab)
    } else if (is.null(tsUpload())||input$preview!="plot") return(NULL) else plot(tsUpload())    
  })
  
  
  output$ts.Plot.slider <- renderUI({
    print("[output$ts.PLot.slider (reUI)] executed ")
    if(!re.val$upload&&length(input$ts.choose)){
      min.max<-c(subset(db.ts.c,db.ts.c$name==input$ts.choose)$start,subset(db.ts.c,db.ts.c$name==input$ts.choose)$end)
      wellPanel(sliderInput("ts.plot.slider", "Time Window [Years]:", min = min.max[1] , max = min.max[2], value = min.max, step=1, format = "####0.#####"))   
    } else return(NULL)
  })     
  
  output$tsSummary <-   renderTable({
    if(!re.val$upload&&!is.null(input$ts.choose))
      with(subset(db.ts.c,db.ts.c$name==input$ts.choose), {
        ts.sum <- matrix(nrow = 6)
        colnames(ts.sum) <- c(paste("Summary of:",name))
        rownames(ts.sum) <- c("Variable", "Location", "Longitude", "Latitude", "Time Range", "Missing Values")
        ts.sum["Variable",] <- as.character(variable)
        if(!(location==""||is.na(location))) ts.sum["Location",] <- location
        if (lon!=0||lat!=0) {
          ts.sum["Longitude",] <- paste(lon, "degree")
          ts.sum["Latitude",] <- paste(lat, "degree")
        } 
        ts.sum["Time Range",] <- paste(start,"-",end)        
        ts <- eval(parse(text = ts.name))
        nr.na <- sum(is.na(ts))
{if (nr.na==0) ts.sum["Missing Values",] <- "No Missing Values"
 else if (nr.na < 10) ts.sum["Missing Values",] <- paste("Missing Years:", paste(time(ts)[is.na(ts)],collapse=", "))
 else  ts.sum["Missing Values",] <- paste("Number of Missing Values:" ,nr.na)}
        ts.sum
      })
  })
  
  
  output$contents <- renderTable({
    if(re.val$upload&&!is.null(input$preview)&&input$preview=="table")   readTable()
  })
  
  
############### ____________________ ###################
############### $$ CHOOSE FIELD (tab 3) ################     


##############  __$ Side Panel   #################
  
############ Part 1 Choose TS 
  
  output$field.Var <- renderUI({
    print("[output$field.Var (reUI)]: executed")
    field.vars <- c(levels(db.field$variable))
    selectInput("field.var","Choose Variable:", choices=field.vars, selected = "Land Temperature")
  })
  
  output$field.Choose <- renderUI({
    if(length(input$field.var)){
      print("[output$field.Choose (reUI)] executed")
      selectInput("field.choose", "Choose a data set:", choices = subset(db.field, db.field$variable==input$field.var)$name)
    }
  })
    
  output$field.Season <- renderUI({
    if(length(input$field.choose)){
      print("[output$field.Season (reUI)] executed")
      field.seasons <- seasons.print[as.logical(subset(db.field, db.field$name==input$field.choose)[,14:31])]
      selectInput("field.season", "Choose season(s):", choices = field.seasons, multiple = T)
    }
  })
  
  output$field.Area <- renderUI({ 
    selectInput("field.area", "Select Area", choices = c("Entire Area",db.area$name, "Custom"))
  })
  
#   output$field.Area <- renderUI({
#     if (length(input$field.choose)){
#       area <- subset(db.field,db.field$name==input$field.choose)$area
#       choices <- c(db.area$name, "Custom")
#       if (area == "Global") choices <- c("Global", choices)
#        else if (area == "Northern Hemisphere") choices <- c("Northern Hemisphere", "Europe", "Custom")
#        else if (area == "Europe") choices <- c("Europe", "Custom")
#       selectInput("field.area", "Select Area", choices = choices, selected = area)
#     }   
#   })
  
#   output$field.Area.text <- renderUI({
#     if (length(input$field.area)&&input$field.area!="Custom"){
#       with(subset(db.area,db.area$name==input$field.area), helpText(paste("Latitude:", lat1, "to", lat2, "   ", "Longitude:", lon1, "to", lon2 )) )
#     }  
#   })
#   
  output$field.Area.latlon <- renderUI({
    if (length(input$field.area)&&input$field.area=="Custom")        
      wellPanel(
        sliderInput("field.area.lat", "Latitude:", min = -90 , max = 90, value = c(-90,90), step=1),
        sliderInput("field.area.lon", "Longitude:", min = -180 , max = 180, value = c(-180,180), step=1)
      )
    
  })

  
########### Part 2 Download Plot  
  
  
  output$field.Plot.dl <- renderUI({
    if(("Plot" %in% input$field.show)&&!is.null(re.val$plot.command.field))  wellPanel(downloadButton("field.plot.dl", "Download Plot Graphic"))
  })  
  
  output$field.plot.dl <- downloadHandler(
    filename <- function() {
      paste(attr(fieldTstep(),"name")," ",input$field.plot.slider,".pdf",sep="")
    },  
    content <- function(file){
      pdf(file = file, width=11, height=8.5)
      eval(parse(text = re.val$plot.command.field))
      dev.off()
    }  
  )   
  
  

  
############### __$ Reactive Functions ###############     
 

fieldInput <- reactive ({
  dataset <- subset(db.field,db.field$name==isolate(input$field.choose))
  area <- dataset$area
  name.field <- dataset$name.field
  seasons <- input$field.season
  nr.s <- length(seasons)
  #browser()
  if (length(name.field)&&nr.s&&!sum(!(seasons %in% seasons.print[as.logical(dataset[14:31])]))){
    field <- read.field(name.field,seasons.int[which(seasons.print==input$field.season[1])])
    if (nr.s>1){  # If several seasons than add them to one field
      for (season in input$field.season[2:nr.s])  field <- field + read.field(name.field,seasons.int[which(seasons.print==season)]) 
      field <- field/nr.s
      attr(field,"name") = paste(dataset$name,paste(seasons,collapse="+"))
    }
  field  
  } else NULL
})
  
fieldArea <- reactive ({
  if (!is.null(input$field.choose)&&!is.null(input$field.area)) {
    print("[fieldArea (re)] executed")
#     #area <- subset(db.field,db.field$name==input$field.choose)$area
    if (input$field.area=="Entire Area") return(NULL)
    else if (input$field.area!="Custom") {
      area <- with(subset(db.area,db.area$name==input$field.area),{
        if (lon1==-180&&lon2==180) {lon1 <- 0;   lon2 <- 360}
        else {
          if (lon1 < 0) lon1 <- lon1 + 360
          if (lon2 < 0) lon2 <- lon2 + 360
        }  
        return(c(lat1=lat1, lat2=lat2, lon1=lon1, lon2=lon2))   
      })
      
    } else if (!is.null(input$field.area.lon)) {
      #browser()
      lon <- input$field.area.lon
      lat <- input$field.area.lat
      if (length(lat)&&length(lon)){
        if (lon[1]==-180&&lon[2]==180) {lon[1] <- 0; lon[2] <- 360}
        else {
          if (lon[1] < 0) lon[1] <- lon[1] + 360
          if (lon[2] < 0) lon[2] <- lon[2] + 360
        }  
        area <- c(lat1=lat[1], lat2=lat[2], lon1=lon[1], lon2=lon[2])
      }  
    } else return(NULL)
    return(area)
  }    
})  

 #############  
fieldTstep <- reactive ({
  print("[fieldTstep (re)] executed")
  tstep <- input$field.plot.slider
  if(!is.null(tstep)&&!is.null(isolate(input$field.season)))  {
    field <- window(fieldInput(),tstep,tstep)
    ## Select Area of field
    if (input$field.area!="Entire Area"){
      area <- fieldArea()
      field <- selspace(field,lat1=area["lat1"], lat2=area["lat2"], lon1=area["lon1"], lon2=area["lon2"])
    }
    if (dim(field)[2]==sum(is.na(field))){ 
      re.val[["fieldTstep.warning"]] <- TRUE
      NULL
    } else {
      re.val[["fieldTstep.warning"]] <- FALSE
      field
    }  
  }  
})  
  

     
############# __$ Main Panel  #######


  output$fieldSummary <-   renderTable({
    if(length(input$field.choose)&&("Summary" %in% input$field.show)&&!is.null(input$field.season)){
      dataset <- subset(db.field,db.field$name==input$field.choose)  
      with(dataset, {
        f.sum <- matrix(nrow = 6)
        colnames(f.sum) <- c(paste("Summary of:",name))
        rownames(f.sum) <- c("Variable", "Type", "Area", "Resolution", "Time Range", "Available Seasons")
        f.sum["Variable",] <- as.character(variable)
        f.sum["Type",] <- type
        f.sum["Area",] <- area
        f.sum["Resolution",] <- resolution
        #f.sum["Time Range",] <- paste(start,"-",end)
        f.sum["Time Range",] <- paste(start(fieldInput())[1],"-", end(fieldInput())[1])
        f.sum["Available Seasons",] <- paste(seasons.print[as.logical(dataset[14:31])],collapse=", ")
        f.sum
      })
    }
  })
  
  output$field.Plot.slider <- renderUI({
    print("[output$field.Plot.slider (reUI)] executed ")
    if(!is.null(fieldInput())&&sum(c("Histogram","Plot") %in% input$field.show)){  # Only show slider if Histogram or Plot is choosen
      min.max <- c(start(fieldInput())[1],end(fieldInput())[1])
      wellPanel(sliderInput("field.plot.slider", "Year:", min = min.max[1] , max = min.max[2], value = min.max[1], step=1, format = "####0.#####"))   
    } else return(NULL)
  })     
  
  output$fieldWarning <- renderUI({
    if (re.val$fieldTstep.warning) h5("Warning: Selected Area has only NA Values for this time step")
  })
  
  
  
  output$fieldHist <- renderPlot({
    #browser()
    field <- fieldTstep()
    if(!is.null(field)&&("Histogram" %in% input$field.show)) {
      print("[output$fieldHist (rePlot)] executed")
      temp <- matrix(field,length(attr(field,"lon")),length(attr(field,"lat")))  ## Convert pField to matrix, because hist produces an error if the pField does not have missing values (for whatever reason) 
      hist(temp, main=paste("Histogram of", attr(field,"name"),start(field)[1]), xlab= "")
    } else return(NULL)
  })  
  

  
  output$fieldPlot  <- renderPlot({
    if(!is.null(fieldTstep())&&("Plot" %in% input$field.show)) {
      print("[output$fieldPlot (rePlot)] executed")
      if (input$field.area=="Entire Area") {  if (subset(db.field,db.field$name==isolate(input$field.choose))$area %in% c("Global","Northern Hemisphere")) shift = T else shift = F} 
      else {if (fieldArea()["lon1"]==0 && fieldArea()["lon2"]==360) shift = T else shift = F}
      re.val[["plot.command.field"]] <<- paste("plot(fieldTstep(),main=paste(attr(fieldTstep(),'name'),start(fieldTstep())[1]),shift=",shift,",palette=palette,set.bg=plot.bg)",sep="")
      print(re.val$plot.command.field)
      plot(fieldTstep(),main=paste(attr(fieldTstep(),"name"),start(fieldTstep())[1]),shift=shift,palette=palette,set.bg=plot.bg )
    } else return(NULL)
  })  
  
  output$field.Plot <- renderUI({
    if (!is.null(fieldTstep())&&("Plot" %in% input$field.show)) plotOutput("fieldPlot")
  })
            
            
  output$field.Hist <- renderUI({
    if (!is.null(fieldTstep())&&("Histogram" %in% input$field.show)) plotOutput("fieldHist")
  })
  


############### ____________________ ###################
############### $$ PLOT CORRELATION (tab 3) ################   

####### __$ Side Panel ########  
  
  output$cor.Title <- renderUI({
    if (re.val$cor.field) h5("Correlation of",input$ts.choose,"and", attr(fieldInput(),"name"))    
    else  h5("Error: No Time series or no Field choosen")   
  })
  

  output$cor.Sigtest <- renderUI({
    if (re.val$cor.field) selectInput("cor.sigtest", "Test Significance", choices = c("No", "Contour" = "contour", "Shaded" = "siglines"))
  })
  
  output$cor.Lag <- renderUI({
    if (re.val$cor.field) sliderInput("cor.lag", "Lag of Time Series (e.g. lag = 1: TS 1900 to 2000 -> 1899 to 1999):", min = -5 , max = 5, value = 0, step = 1)
  })
  
  
  output$cor.Filter.bol <- renderUI({
    print("cor.Filter.bol (reUI) executed")
    if (re.val$cor.field) {
      if (sum(is.na(tsInput()))) helpText("No filter possible (Time series has NA values)")
      else  checkboxInput("cor.filter.bol","Use filter", FALSE)
    }  
  })    
  
  output$cor.Filter.type <- renderUI({
    if (length(input$cor.filter.bol)&&input$cor.filter.bol) 
      selectInput("cor.filter.type", "Type of Filter", choices = c("Lowpass" = "lowpass", "Highpass" = "highpass", "Bandpass" = "bandpass"))
  })
  
  output$cor.Filter.attr <- renderUI({
    if (length(input$cor.filter.bol)&&input$cor.filter.bol&&length(input$cor.filter.type)){
      if (input$cor.filter.type!="bandpass")
        wellPanel(        
            numericInput("lim1","Cutoff Frequency (1/x)",10),
            tags$style(type="text/css", '#lim1 {width: 20px}'),
            sliderInput("cor.filter.n","Length of filter (+1)", min = 2 , max = 40, value = 8, step = 2),
            checkboxInput("cor.filter.transfer","Show Transfer Function", FALSE)
        )
      else 
        wellPanel(        
          div(class="row-fluid", div(class="span6", numericInput("lim1","Upper Limit (1/x)",10)), div(class="span6", numericInput("lim2","Lower Limit (1/x)",5))),
          tags$style(type="text/css", '#lim1 {width: 20px}'),
          tags$style(type="text/css", '#lim2 {width: 20px}'),
          sliderInput("cor.filter.n","Length of filter (+1)", min = 2 , max = 40, value = 8, step = 2),
          checkboxInput("cor.filter.transfer","Show Transer Function", FALSE)
        )
    }  
  })
  
  
  output$cor.Compute <- renderUI({
    if (re.val$cor.field)  actionButton("cor.compute", "Compute Correlation")
    
  })
  
#### ____ Part 2
  
  output$cor.Main <- renderUI({
    if (re.val$cor.field&&!is.null(corResult())) {
      #browser()
      textInput("cor.title","Title of Plot",paste("Correlation of",input$ts.choose,"and", attr(fieldInput(),"name"), isolate(input$cor.plot.slider)[1], "-", isolate(input$cor.plot.slider)[2]))  #textInput("cor.title","Title of Plot","Correlation map")
    }
  })  
  
  output$cor.Zlim <- renderUI({
    if (re.val$cor.field) selectInput("cor.zlim", "Limit of Colour bar", choices = c("Automatic", "Custom"))
  })
  
  output$cor.Custom.zlim <- renderUI({
    if (length(input$cor.zlim)&&input$cor.zlim=="Custom")  sliderInput("cor.custom.zlim", "", min = -1 , max = 1, value = c(-1,1), step=0.05)
  })
  
  output$cor.Shift <- renderUI({
    if (re.val$result.shift) checkboxInput("cor.shift", "Shift correlation map?", TRUE)
  })  

#   output$cor.Shift <- renderUI({
#     if (re.val$cor.field){
#       area <- fieldArea()
#       area.db <-  subset(db.field,db.field$name==input$field.choose)$area
#       if ((is.null(area)&& area.db %in% c("Global","Northern Hemisphere"))||(!is.null(area)&&area["lon1"]==0&&area["lon2"]==360)){
#         re.val[["cor.shift"]] <- TRUE
#         checkboxInput("cor.shift", "Shift correlation map?", TRUE)       
#       } else {re.val[["cor.shift"]] <- FALSE; return(NULL)}
#     }
#   })
#   
#### _ Part 3  
  
  output$cor.Dl <- renderUI({
    if(!is.null(re.val$plot.command.cor))  wellPanel(downloadButton("cor.dl", "Download Plot Graphic"))
  })    
  
  output$cor.dl <- downloadHandler(
    filename <- function() {  paste(input$cor.title, ".pdf", sep="") },  
    content <- function(file){
      pdf(file = file, width=corSize()[1]/100, height=corSize()[2]/100)
      eval(parse(text = re.val$plot.command.cor))
      dev.off()
    }  
  )     
  
  

  ####### __$ Reactive Functions ########  
  
  
  corSize <- reactive({
    if(!is.null(corResult())){
      lat <- attr(corResult(),"lat")
      lon <- attr(corResult(),"lon")
      ratio <- min(2,min(diff(lon))*(length(lon)-1)/(min(diff(lat))*(length(lat)-1))*6/5)
      print(paste("Ratio=", ratio))
      width <- 1000
      height <- round(width/ratio)
      browser()
      c(width, height)
    } else (c("auto","auto"))
   
})
  
  corResult <- reactive({
    if (length(input$cor.compute)&&input$cor.compute>0){
      print(paste("[corResult (re)] executed"))
      area <- isolate(fieldArea())
      lag.ts <- isolate(input$cor.lag) 
      t.range <- isolate(input$cor.plot.slider)
      if ((t.range[2] - t.range[1])<30)  return(NULL)
      ts1 <- window(lag(isolate(tsInput()),lag.ts), t.range[1], t.range[2])
      field <- window(isolate(fieldInput()), t.range[1], t.range[2])
      b.sig_test <- isolate(input$cor.sigtest)=="No"
      b.filt <- isolate(input$cor.filter.bol)
      if (is.null(b.filt)) b.filt=FALSE
      
      if (!is.null(area)) {
        lon1 <- area["lon1"]; lon2 <- area["lon2"]; lat1 <- area["lat1"]; lat2 <- area["lat2"] 
        if (!(lon1==0&&lon2==360)){
          if (lon1 < -175&& lon2 > 175) {lon1<- 0; lon2 <- 360}
          else if (lon1 < -175) {lon1 <- -180; lon2 <- lon2-5}
          else if (lon2 > 175)  {lon2 <- 180; lon1 <- lon1-5}
          else {lon1 <- lon1-5; lon2 <- lon2+5}
        }
        if (lat1 < -85&& lat2 > 85) {lat1<- -90; lat2 <- 90}
        else if (lat1 < -85) {lat1 <- -90; lat2 <- lat2+5}
        else if (lat2 > 85)  { lat2 <- 90; lat1 <- lat1-5}
        else {lat1 <- lat1-5; lat2 <- lat2+5}
        print("Given area:")
        print(area)
        print("Selected area:")
        print(c(lat1=lat1, lat2=lat2, lon1=lon1, lon2=lon2))
        field<-selspace(field,lat1=lat1, lat2=lat2, lon1=lon1, lon2=lon2)
      }
      
      if (!b.filt) {
        if (b.sig_test) result<-cor.pTs(ts1,field)
        else   result<-cortest.pTs(ts1,field) 
      } else {
          filt=list(filter=isolate(input$cor.filter.type), x1= 1/isolate(input$lim1), x2= 1/isolate(input$lim2), y= (isolate(input$cor.filter.n)+1), method=2)
          if (b.sig_test) result<-cor.filter(ts1,field,filt)
          else result<-cor.filter(ts1,field,filt,plot_sig="Yes Yoah")
      }
      #browser()
      result 
    }
  })  
  
  
  
####### __$ Main Panel ########
  
  output$cor.Main.warning <- renderUI({
    t.range <- input$cor.plot.slider
    if (length(t.range)&&(t.range[2] - t.range[1])<30) h5("Warning: Minimum Time Range has to be at least 30 years")  
  })   
  
  output$cor.Plot.slider <- renderUI({
    print("[output$ts.PLot.slider (reUI)] executed ")
    if(re.val$cor.field&&length(input$cor.lag)){
      ts1 <- lag(tsInput(),input$cor.lag)
      min.max <- c(max(start(fieldInput())[1],start(ts1)[1]), min(end(fieldInput())[1],end(ts1)[1]))
      wellPanel(sliderInput("cor.plot.slider", "Time Window [Years]:", min = min.max[1] , max = min.max[2], value = min.max, step=1, format = "####0.#####"))   
    } else return(NULL)
  })     
  
  output$corTransfer <- renderPlot({
    if (length(input$cor.filter.transfer)&&input$cor.filter.transfer) {
      #filt=list(filter=input$cor.filter.type, x1= 1/input$lim1, x2= 1/input$lim2, y= (input$cor.filter.n+1), method=2)
      filt <- input$cor.filter.type
      if (filt!="bandpass") get.transfer(get(input$cor.filter.type)(1/input$lim1, input$cor.filter.n+1))
      else if (!is.null(input$lim2))  get.transfer(get(input$cor.filter.type)(1/input$lim1, 1/input$lim2, input$cor.filter.n+1))
      else return(NULL)
    }
  })
  output$cor.Transfer <- renderUI({
      if (length(input$cor.filter.transfer)&&input$cor.filter.transfer) plotOutput("corTransfer")
  })
  

  output$corPlot <- renderPlot({
    print(paste("[corPlot (rePlot)] executed"))
    result <- corResult()
    area <- isolate(fieldArea())
    sigtest <- isolate(input$cor.sigtest)
    
    if (!is.null(result)) {
      if (!is.null(area)) {
        xlim<-c(if (area["lon1"]>area["lon2"]) area["lon1"]-360 else area["lon1"],area["lon2"])
        ylim<-c(area["lat1"],area["lat2"])
      } else  { xlim<-NULL ;  ylim<-NULL}
      
      if (input$cor.zlim=="Custom"&&length(input$cor.custom.zlim)) zlim=input$cor.custom.zlim
      else {
        if (sum(is.na(result))>0)
          lim<-max(abs(range(result[1,][!is.na(result[1,])])))
        else
          lim<-max(abs(range(result[1,])))
        zlim=c(-lim,lim)
      }  
      
      if (!isolate(re.val$result.shift)) shift = F else shift = input$cor.shift
      if (shift) xlim<-NULL
      #browser()  
      point<-c(lat=attr(tsInput(),"lat"),lon=attr(tsInput(),"lon"))    
      if (sigtest=="No"){
        re.val[["plot.command.cor"]] <- paste('plot(corResult(), main=input$cor.title, palette=palette, zlim=', deparse(zlim), ', xlim=', deparse(xlim), ', ylim=', deparse(ylim),', shift=', shift,', set.bg=plot.bg, FUN=points(', point["lon"], ',', point["lat"], ', pch=21, cex=1, col="black", bg="red", lwd=2))',sep='')
        print(re.val$plot.command.cor)
        #browser()
        plot(result, main=input$cor.title, palette=palette, zlim=zlim, xlim=xlim, ylim=ylim, shift = shift, set.bg=plot.bg, FUN=points(point["lon"], point["lat"], pch=21, cex=1, col="black", bg="red", lwd=2))
      }else{
        re.val[["plot.command.cor"]] <- paste('plot.sig(corResult()[1,], corResult()[2,], main=input$cor.title, plot_sig="',sigtest,'", palette=palette, zlim=', deparse(zlim), ', xlim=', deparse(xlim), ', ylim=', deparse(ylim),', shift=', shift,', set.bg=plot.bg, FUN= "points(', point["lon"], ',', point["lat"], ', pch=21, cex=1, col=\'black\', bg=\'red\', lwd=2)")',sep='')
        print(re.val$plot.command.cor)
        plot.sig(result[1,], result[2,], main=input$cor.title, plot_sig=sigtest, palette=palette, zlim=zlim, xlim=xlim, ylim=ylim, shift = shift, set.bg=plot.bg, FUN=paste("points(",point["lon"],",",point["lat"],",pch=21,cex=1,col=\"black\",bg=\"red\",lwd=2)",sep=""))
      }
      
    }
  }, height= function(){corSize()[2]}, width = function(){corSize()[1]} # height=750, width=1000 
  )

############### ____________________ ###################
############### $$ COMPOSITE (tab 4) ################   

####### __$ Side Panel ########  
  
  output$comp.Title <- renderUI({
    if (re.val$cor.field) h5("Composite analysis of",input$ts.choose,"and", attr(fieldInput(),"name"))    
    else  h5("Error: No Time series or no Field choosen")   
  })
  
  output$comp.Lag <- renderUI({
    if (re.val$cor.field) sliderInput("comp.lag", "Lag of Time Series (e.g. lag = 1: TS 1900 to 2000 -> 1899 to 1999):", min = -5 , max = 5, value = 0, step = 1)
  })


  output$comp.Upperlim<- renderUI({
    if (re.val$cor.field)  sliderInput("comp.upperlim", "Upper Limit (1 = standard deviation)", min = 0 , max = 3, value = 1, step=0.05)
  })
  
  output$comp.Lowerlim<- renderUI({
    if (re.val$cor.field)  sliderInput("comp.lowerlim", "Lower Limit", min = -3 , max = 0, value = -1, step=0.05)
  })


  output$comp.Show <- renderUI({ 
    if  (re.val$cor.field) selectInput('comp.show', 'Show composites', choices = c("Comp+ - Comp-"="comp.pm", "Comp+"="comp.p", "Comp-"="comp.m"), multiple=T)
  })
#### Part 2

  output$comp.Shift <- renderUI({
    if (re.val$result.shift) checkboxInput("comp.shift", "Shift correlation map?", TRUE)
  })

  output$comp.pm.Plotoptions <- renderUI({
    if ("comp.pm" %in% input$comp.show){
      result <- compResult()$plus - compResult()$minus
      zlim <- c(min(result,na.rm=T),max(result,na.rm=T))+c(-0.025,0.025)
      max.zlim<-ceiling(max(abs(max(result,na.rm=T)+0.025),abs(min(result,na.rm=T)-0.025)))
      #browser()
      wellPanel(
        h5("Comp+ - Comp-"),
        textInput('comp.pm.title','Title of plot', paste("Composite +- of",input$ts.choose,"and", attr(fieldInput(),"name"), isolate(input$ts.comp.plot.slider)[1], "-", isolate(input$ts.comp.plot.slider)[2])),
        sliderInput("comp.pm.zlim", "Limit of Colour bar", min = -max.zlim , max = max.zlim, value = zlim, step=0.05)
      )
    }
  })

  output$comp.p.Plotoptions <- renderUI({
    if ("comp.p" %in% input$comp.show){
      result <- compResult()$plus
      zlim <- c(min(result,na.rm=T),max(result,na.rm=T))+c(-0.025,0.025)
      max.zlim<-ceiling(max(abs(max(result,na.rm=T)+0.025),abs(min(result,na.rm=T)-0.025)))
      
      wellPanel(
        h5("Comp+"),
        textInput('comp.p.title','Title of plot', paste("Composite+ of",input$ts.choose,"and", attr(fieldInput(),"name"), isolate(input$ts.comp.plot.slider)[1], "-", isolate(input$ts.comp.plot.slider)[2])),
        sliderInput("comp.p.zlim", "Limit of Colour bar", min = -max.zlim , max = max.zlim, value = zlim, step=0.05)
      )
    }
  })

  output$comp.m.Plotoptions <- renderUI({
    if ("comp.m" %in% input$comp.show){
      result <- compResult()$minus
      zlim <- c(min(result,na.rm=T),max(result,na.rm=T))+c(-0.025,0.025)
      max.zlim<-ceiling(max(abs(max(result,na.rm=T)+0.025),abs(min(result,na.rm=T)-0.025)))
      #browser()
      wellPanel(
        h5("Comp-"),
        textInput('comp.m.title','Title of plot', paste("Composite+ of",input$ts.choose,"and", attr(fieldInput(),"name"), isolate(input$ts.comp.plot.slider)[1], "-", isolate(input$ts.comp.plot.slider)[2])),
        sliderInput("comp.m.zlim", "Limit of Colour bar", min = -max.zlim , max = max.zlim, value = zlim, step=0.05)
      )
    }
  })


#### part 3


####### __$ Reactive Functions ########  
  tsComp <- reactive ({
    if(re.val$cor.field&&!is.null(input$ts.comp.plot.slider)) scale(window(tsInput(),input$ts.comp.plot.slider[1],input$ts.comp.plot.slider[2])) 
    else return(NULL)
  })


  compResult <- reactive({
    #browser()
    ts <- tsComp()
    field <- fieldInput()
    area <- fieldArea()
    sp <- input$comp.upperlim
    sm <- input$comp.lowerlim
    anomaly=T
    if(!is.null(ts)&&!is.null(sp)) {
      
      if (!is.null(area)) {
        lon1 <- area["lon1"]; lon2 <- area["lon2"]; lat1 <- area["lat1"]; lat2 <- area["lat2"] 
        if (!(lon1==0&&lon2==360)){
          if (lon1 < -175&& lon2 > 175) {lon1<- 0; lon2 <- 360}
          else if (lon1 < -175) {lon1 <- -180; lon2 <- lon2-5}
          else if (lon2 > 175)  {lon2 <- 180; lon1 <- lon1-5}
          else {lon1 <- lon1-5; lon2 <- lon2+5}
        }
        if (lat1 < -85&& lat2 > 85) {lat1<- -90; lat2 <- 90}
        else if (lat1 < -85) {lat1 <- -90; lat2 <- lat2+5}
        else if (lat2 > 85)  { lat2 <- 90; lat1 <- lat1-5}
        else {lat1 <- lat1-5; lat2 <- lat2+5}
        print("Given area:")
        print(area)
        print("Selected area:")
        print(c(lat1=lat1, lat2=lat2, lon1=lon1, lon2=lon2))
        field<-selspace(field,lat1=lat1, lat2=lat2, lon1=lon1, lon2=lon2)
      }
      
      temp<-attributes(field)
      if (anomaly) field<-scale(field,scale=FALSE)
      ts<-scale(ts)
      #bring both on the same time basis
      start<-max(start(ts)[1],start(field)[1])
      end<-min(end(ts)[1],end(field)[1])
      print(paste("Common time period: ",start,end))
      ts<-window(ts,start,end)
      field<-unclass(window(field,start,end))
      
      ts[is.na(ts)]<-0
      index.plus <- ts>sp
      index.minus <- ts<sm
      
      
      field.plus<-field[index.plus,]
      field.minus<-field[index.minus,]
      
      field.plus<-pField(colMeans(field.plus),9999,temp$lat,temp$lon,paste("+ Compos.",getname(ts),temp$name),temp$history,date=FALSE)
      field.minus<-pField(colMeans(field.minus),9999,temp$lat,temp$lon,paste("- Compos.",getname(ts),temp$name),temp$history,date=FALSE)
      list(plus=field.plus,minus=field.minus)
      
    }
  })

####### __$ Main Panel ########

  output$ts.comp.Plot.slider <- renderUI({
    print("[output$comp.PLot.slider (reUI)] executed ")
    if(re.val$cor.field&&length(input$comp.lag)){
      ts1 <- lag(tsInput(),input$comp.lag)
      min.max <- c(max(start(fieldInput())[1],start(ts1)[1]), min(end(fieldInput())[1],end(ts1)[1]))
      wellPanel(sliderInput("ts.comp.plot.slider", "Time Window [Years]:", min = min.max[1] , max = min.max[2], value = min.max, step=1, format = "####0.#####"))   
    } else return(NULL)
  })     

  output$ts.comp.Plot <- renderPlot({
    print("[output$ts.comp.PLot (rePlot)] executed ")
    if(!is.null(tsComp())) {
      plot(tsComp(),main=isolate(input$ts.plot.title),xlab=isolate(input$ts.plot.xlab),ylab=isolate(input$ts.plot.ylab))
      abline(h=c(input$comp.upperlim,input$comp.lowerlim),col=c("red","blue"))
    }  
    else return(NULL)  
  })


  output$comp.pm.Plot<- renderUI({  if ("comp.pm" %in% input$comp.show)  plotOutput("comp.pm.plot")  })
  
  output$comp.pm.plot <- renderPlot({
    print(paste("[comp.pm.Plot (rePlot)] executed"))
    result <- compResult()$plus - compResult()$minus
    area <- isolate(fieldArea())
    
    if ("comp.pm" %in% input$comp.show){
      if (!is.null(result)) {
        if (!is.null(area)) {
          xlim<-c(if (area["lon1"]>area["lon2"]) area["lon1"]-360 else area["lon1"],area["lon2"])
          ylim<-c(area["lat1"],area["lat2"])
        } else  { xlim<-NULL ;  ylim<-NULL}
        
        
        if (!isolate(re.val$result.shift)) shift = F else shift = input$comp.shift
        if (shift) xlim<-NULL
        #browser()  
        point<-c(lat=attr(tsInput(),"lat"),lon=attr(tsInput(),"lon"))    
        
        plot(result,main=input$comp.pm.title,  palette=palette, xlim=xlim, ylim=ylim,zlim=input$comp.pm.zlim, shift = shift, set.bg=plot.bg, FUN=points(point["lon"], point["lat"], pch=21, cex=1, col="black", bg="red", lwd=2))
        
        #     if (sigtest=="No"){
        #       re.val[["plot.command.cor"]] <- paste('plot(corResult(), main=input$cor.title, palette=palette, zlim=', deparse(zlim), ', xlim=', deparse(xlim), ', ylim=', deparse(ylim),', shift=', shift,', set.bg=plot.bg, FUN=points(', point["lon"], ',', point["lat"], ', pch=21, cex=1, col="black", bg="red", lwd=2))',sep='')
        #       print(re.val$plot.command.cor)
        #       #browser()
        #       plot(result, main=input$cor.title, palette=palette, zlim=zlim, xlim=xlim, ylim=ylim, shift = shift, set.bg=plot.bg, FUN=points(point["lon"], point["lat"], pch=21, cex=1, col="black", bg="red", lwd=2))
        #     }else{
        #       re.val[["plot.command.cor"]] <- paste('plot.sig(corResult()[1,], corResult()[2,], main=input$cor.title, plot_sig="',sigtest,'", palette=palette, zlim=', deparse(zlim), ', xlim=', deparse(xlim), ', ylim=', deparse(ylim),', shift=', shift,', set.bg=plot.bg, FUN= "points(', point["lon"], ',', point["lat"], ', pch=21, cex=1, col=\'black\', bg=\'red\', lwd=2)")',sep='')
        #       print(re.val$plot.command.cor)
        #       plot.sig(result[1,], result[2,], main=input$cor.title, plot_sig=sigtest, palette=palette, zlim=zlim, xlim=xlim, ylim=ylim, shift = shift, set.bg=plot.bg, FUN=paste("points(",point["lon"],",",point["lat"],",pch=21,cex=1,col=\"black\",bg=\"red\",lwd=2)",sep=""))
        #     }
        #     
      }
    } else return(NULL)
  } #, height= function(){corSize()[2]}, width = function(){corSize()[1]} # height=750, width=1000 
  )



  output$comp.p.Plot<- renderUI({  if ("comp.p" %in% input$comp.show)  plotOutput("comp.p.plot")  })

  output$comp.p.plot <- renderPlot({
    print(paste("[comp.p.Plot (rePlot)] executed"))
    result <- compResult()$plus
    #result <- comp.p.Result()
    area <- isolate(fieldArea())
    
    if ("comp.p" %in% input$comp.show){
      if (!is.null(result)) {
        if (!is.null(area)) {
          xlim<-c(if (area["lon1"]>area["lon2"]) area["lon1"]-360 else area["lon1"],area["lon2"])
          ylim<-c(area["lat1"],area["lat2"])
        } else  { xlim<-NULL ;  ylim<-NULL}
        
        
        if (!isolate(re.val$result.shift)) shift = F else shift = input$comp.shift
        if (shift) xlim<-NULL
        #browser()  
        point<-c(lat=attr(tsInput(),"lat"),lon=attr(tsInput(),"lon"))    
        
        plot(result, main=input$comp.p.title, palette=palette, xlim=xlim, ylim=ylim, zlim=input$comp.p.zlim, shift = shift, set.bg=plot.bg, FUN=points(point["lon"], point["lat"], pch=21, cex=1, col="black", bg="red", lwd=2))

  #     
      }
    } else return(NULL)
  } #, height= function(){corSize()[2]}, width = function(){corSize()[1]} # height=750, width=1000 
  )

  output$comp.m.Plot<- renderUI({ if ("comp.m" %in% input$comp.show)  plotOutput("comp.m.plot")  })
  
  output$comp.m.plot <- renderPlot({
    print(paste("[comp.m.Plot (rePlot)] executed"))
    result <- compResult()$minus
    #result <- comp.m.Result()
    area <- isolate(fieldArea())
    if ("comp.m" %in% input$comp.show){
      if (!is.null(result)) {
        if (!is.null(area)) {
          xlim<-c(if (area["lon1"]>area["lon2"]) area["lon1"]-360 else area["lon1"],area["lon2"])
          ylim<-c(area["lat1"],area["lat2"])
        } else  { xlim<-NULL ;  ylim<-NULL}
        
               
        if (!isolate(re.val$result.shift)) shift = F else shift = input$comp.shift
        if (shift) xlim<-NULL
        #browser()  
        point<-c(lat=attr(tsInput(),"lat"),lon=attr(tsInput(),"lon"))    
        
        #plot(result, palette=palette, xlim=xlim, ylim=ylim, shift = shift, set.bg=plot.bg, FUN=points(point["lon"], point["lat"], pch=21, cex=1, col="black", bg="red", lwd=2))
        plot(result, main=input$comp.m.title, palette=palette, xlim=xlim, ylim=ylim, zlim=input$comp.m.zlim, shift = shift, set.bg=plot.bg, FUN=points(point["lon"], point["lat"], pch=21, cex=1, col="black", bg="red", lwd=2))
        
        
      }
    } else return(NULL)
  } #, height= function(){corSize()[2]}, width = function(){corSize()[1]} # height=750, width=1000 
  )

})
