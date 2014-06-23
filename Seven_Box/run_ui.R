run_ui <- function(ncores) {
  
  if (!require(shiny)){
    pass=FALSE
    while (!pass){
      switch(readline("For the user interface the package 'shiny' is needed. \n Would you like to install it? (y/n):"), 
             'n'={pass=TRUE
                  stop("'shiny' needed!")},
             'y'={pass=TRUE
                  install.packages('shiny', dependencies=TRUE)
                  require(shiny)})
    }
  }
  
  tryCatch(
    expr={
          if (!missing(ncores) && (Sys.info()['sysname']!="Windows")){
            n_cores <<- ncores
            if (ncores > 1){    
              if (!(require('doMC') && require('foreach'))){
                pass=FALSE
                while (!pass){
                  switch(readline("For parallel computing the packages 'foreach' and 'doMC' are required. \n Would you like to install them? (y/n):"), 
                         'n'={pass=TRUE
                              n_cores<<-1},
                         'y'={pass=TRUE
                              install.packages('foreach', dependencies=TRUE)
                              install.packages('doMC', dependencies=TRUE)})
                }
              }
            }
          }
          else {
            n_cores <<- 1
          }
          
          runApp(".")
          
        }, 
  interrupt=function(ex){ 
                          cat("Sevenbox modell stopped! \n")
                          rm(n_cores, envir=globalenv())
                        }
  
  )
  
}