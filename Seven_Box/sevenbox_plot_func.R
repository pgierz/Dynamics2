## Plots

plot_box_results<-function(x ,y, xlab="", ylab="", main="", col, yRange=range(y, na.rm=TRUE)){
  
  N_pert = dim(y)[2]
  
  if (missing(col)){
    col = array("black", N_pert)
  }
  
  for (i in 1:N_pert){
    plot(x=x,
         y=y[,i], 
         col=col[i], 
         type='l', 
         xlim=c(0, round(max(x, na.rm=TRUE)+0.5)), 
         ylim=yRange, 
         xlab="", 
         ylab="", 
         main="", 
         xaxs="i",
         lwd=3)
    par(new=TRUE)
  }
  
  title(xlab=xlab, line=2)
  title(ylab=ylab, line=2)
  title(main=main)
  
}


addLegend <- function(pert, col, pos=c(0,1/3,0,1/3), lwd=3, title="Perturbation [PSU]"){
  if(length(pert) > 0){
    par(new=TRUE)
    par("fig"=pos)
    plot(1, type="n", axes=FALSE, xlab="", ylab="")
    par(new=TRUE)
    par("xpd"=NA)
    legend("topleft","(x,y)",legend=pert, col=col, lwd=lwd, title=title)
  }
}

plot_box_Temp<-function(data, pert=NULL){
  
  if(length(pert) > 0){
    N_pert = length(pert);
    #     col=terrain.colors(N_pert+1); # color map for plots
    #     col=col[-(N_pert+1)]
    col=rainbow(N_pert)
  }
  else{
    col='black'
  }
  
  xlab="time [years]"
  ylab=expression(paste("Temperature [",~degree~C,"]"))
  
  rangeT = computePlotRange(data=data, var_names=c("Tasl", "Taml", "Tanl", "Tsl", "Tml", "Tnl", "Td"))
  
  plot_mar = c(3,3,2,1)
  
  par("fig"=c(0,1/3,2/3,1))
  par("mar"=plot_mar)
  plot_box_results(x=data$time, y=data$Tasl, xlab=xlab, ylab=ylab, main="Atmosphere south. lat.", col=col, yRange=rangeT$Tasl)
  par(new=TRUE)
  
  par("fig"=c(1/3,2/3,2/3,1))
  par("mar"=plot_mar)
  plot_box_results(x=data$time, y=data$Taml, xlab=xlab, ylab=ylab, main="Atmosphere mid. lat.", col=col, yRange=rangeT$Taml)
  par(new=TRUE)
  
  par("fig"=c(2/3,1,2/3,1))
  par("mar"=plot_mar)
  plot_box_results(x=data$time, y=data$Tanl, xlab=xlab, ylab=ylab, main="Atmosphere north. lat.", col=col, yRange=rangeT$Tanl)
  par(new=TRUE)
  
  par("fig"=c(0,1/3,1/3,2/3))
  par("mar"=plot_mar)
  plot_box_results(x=data$time, y=data$Tsl, xlab=xlab, ylab=ylab, main="Ocean south. lat.", col=col, yRange=rangeT$Tsl)
  par(new=TRUE)
  
  par("fig"=c(1/3,2/3,1/3,2/3))
  par("mar"=plot_mar)
  plot_box_results(x=data$time, y=data$Tml, xlab=xlab, ylab=ylab, main="Ocean mid. lat.", col=col, yRange=rangeT$Tml)
  par(new=TRUE)
  
  par("fig"=c(2/3,1,1/3,2/3))
  par("mar"=plot_mar)
  plot_box_results(x=data$time, y=data$Tnl, xlab=xlab, ylab=ylab, main="Ocean north. lat.", col=col, yRange=rangeT$Tnl)
  par(new=TRUE)
  
  par("fig"=c(1/3,2/3,0,1/3))
  par("mar"=plot_mar)
  plot_box_results(x=data$time, y=data$Td, xlab=xlab, ylab=ylab, main="Deep Ocean", col=col, yRange=rangeT$Td)

  addLegend(pert=pert, col=col, lwd=3)
}

plot_box_Sal<-function(data, pert=NULL){
  
  if(length(pert) > 0){
    N_pert = length(pert);
    #     col=terrain.colors(N_pert+1); # color map for plots
    #     col=col[-(N_pert+1)]
    col=rainbow(N_pert)
  }
  else{
    col='black'
  }
  
  plot_mar = c(3,3,2,1)
  
  xlab="time [years]"
  ylab="Salinity [PSU]"
  
  rangeSal = computePlotRange(data=data, var_names=c("Ssl", "Sml", "Snl", "Sd"))  
  
  par("fig"=c(0,1/3,1/2,1))
  par("mar"=plot_mar)
  plot_box_results(x=data$time, y=data$Ssl, xlab=xlab, ylab=ylab, main="Ocean south. lat.", col=col, yRange=rangeSal$Ssl)
  par(new=TRUE)
  
  par("fig"=c(1/3,2/3,1/2,1))
  par("mar"=plot_mar)
  plot_box_results(x=data$time, y=data$Sml, xlab=xlab, ylab=ylab, main="Ocean mid. lat.", col=col, yRange=rangeSal$Sml)
  par(new=TRUE)
  
  par("fig"=c(2/3,1,1/2,1))
  par("mar"=plot_mar)
  plot_box_results(x=data$time, y=data$Snl, xlab=xlab, ylab=ylab, main="Ocean north. lat.", col=col, yRange=rangeSal$Snl)
  par(new=TRUE)
  
  par("fig"=c(1/3,2/3,0,1/2))
  par("mar"=plot_mar)
  plot_box_results(x=data$time, y=data$Sd, xlab=xlab, ylab=ylab, main="Deep Ocean", col=col, yRange=rangeSal$Sd)

  addLegend(pert=pert, col=col, lwd=3)

}

plot_box_Flux<-function(data, pert=NULL){
  
  if(length(pert) > 0){
    N_pert = length(pert);
    #     col=terrain.colors(N_pert+1); # color map for plots
    #     col=col[-(N_pert+1)]
    col=rainbow(N_pert)
  }
  else{
    col='black'
  }
  
  plot_mar = c(3,3,2,1)
  
  xlab="time [years]"
  ylab=expression(phi~ "[Sv]")
  main="Overturning Flow"
  
  
  par("fig"=c(0,1,1/3,1))
  par("mar"=plot_mar)
  plot_box_results(x=data$time, y=data$phi, xlab=xlab, ylab=ylab, main=main, col=col)

  addLegend(pert=pert, col=col, lwd=3)
}

plot_box_AtmHeatTransp<-function(data, pert=NULL){
  
  if(length(pert) > 0){
    N_pert = length(pert);
    #     col=terrain.colors(N_pert+1); # color map for plots
    #     col=col[-(N_pert+1)]
    col=rainbow(N_pert)
  }
  else{
    col='black'
  }
  
  xlab="time [years]"
  ylab="heat transp. [PW]"
  ylabTglob=expression(paste("Temperature [",~degree~C,"]"))
  
  rangeAHF = computePlotRange(data=data, var_names=c("Fs30S", "Fs50N", "Fl30S", "Fl50N"))
  
  plot_mar = c(3,3,2,1)
  
  par("fig"=c(0,1/2,2/3,1))
  par("mar"=plot_mar)
  plot_box_results(x=data$time, y=data$Fs30S, xlab=xlab, ylab=ylab, main="Sens. heat flux south. lat.", col=col, yRange=rangeAHF$Fs30S)
  par(new=TRUE)
  
  par("fig"=c(1/2,1,2/3,1))
  par("mar"=plot_mar)
  plot_box_results(x=data$time, y=data$Fs50N, xlab=xlab, ylab=ylab, main="Sens. heat flux north. lat.", col=col, yRange=rangeAHF$Fs50N)
  par(new=TRUE)
  
  par("fig"=c(0,1/2,1/3,2/3))
  par("mar"=plot_mar)
  plot_box_results(x=data$time, y=data$Fl30S, xlab=xlab, ylab=ylab, main="Lat. heat flux south. lat.", col=col, yRange=rangeAHF$Fl30S)
  par(new=TRUE)
  
  par("fig"=c(1/2,1,1/3,2/3))
  par("mar"=plot_mar)
  plot_box_results(x=data$time, y=data$Fl50N, xlab=xlab, ylab=ylab, main="Lat. heat flux north. lat.", col=col, yRange=rangeAHF$Fl50N)
  par(new=TRUE)
  
  par("fig"=c(1/4,3/4,0,1/3))
  par("mar"=plot_mar)
  plot_box_results(x=data$time, y=data$Tglob, xlab=xlab, ylab=ylabTglob, main="Atm. glob. mean Temp.", col=col)

  addLegend(pert=pert, col=col, lwd=3)

}

plot_box_PE<-function(data, pert=NULL){
  
  if(length(pert) > 0){
    N_pert = length(pert);
    #     col=terrain.colors(N_pert+1); # color map for plots
    #     col=col[-(N_pert+1)]
    col=rainbow(N_pert)
  }
  else{
    col='black'
  }
  
  xlab="time [years]"
  ylab="P-E [Sv]"
  

  rangePE = computePlotRange(data=data, var_names=c("FWFAS", "FWFAM", "FWFAN"))
  
  plot_mar = c(3,3,2,1)
  
  par("fig"=c(0,1/3,1/3,1))
  par("mar"=plot_mar)
  plot_box_results(x=data$time, y=data$FWFAS, xlab=xlab, ylab=ylab, main="south. lat.", col=col, yRange=rangePE$FWFAS)
  par(new=TRUE)
  
  par("fig"=c(1/3,2/3,1/3,1))
  par("mar"=plot_mar)
  plot_box_results(x=data$time, y=data$FWFAM, xlab=xlab, ylab=ylab, main="mid. lat.", col=col, yRange=rangePE$FWFAM)
  par(new=TRUE)
  
  par("fig"=c(2/3,1,1/3,1))
  par("mar"=plot_mar)
  plot_box_results(x=data$time, y=data$FWFAN, xlab=xlab, ylab=ylab, main="north. lat.", col=col, yRange=rangePE$FWFAN)

  addLegend(pert=pert, col=col, lwd=3)
}


plot_box_SurfHeatFlux<-function(data, pert=NULL){
  
  if(length(pert) > 0){
    N_pert = length(pert);
#     col=terrain.colors(N_pert+1); # color map for plots
#     col=col[-(N_pert+1)]
    col=rainbow(N_pert)
  }
  else{
    col='black'
  }
  
  xlab="time [years]"
  ylab=expression(paste("Flux density [",W/m^2,"]"))
  
  
  rangeSHF = computePlotRange(data=data, var_names=c("Hfsl", "Hfml", "Hfnl"))
  
  plot_mar = c(3,3.5,2,1)
  
  par("fig"=c(0,1/3,1/3,1))
  par("mar"=plot_mar)
  plot_box_results(x=data$time, y=data$Hfsl, xlab=xlab, ylab=ylab, main="south. lat.", col=col, yRange=rangeSHF$Hfsl)
  par(new=TRUE)
  
  par("fig"=c(1/3,2/3,1/3,1))
  par("mar"=plot_mar)
  plot_box_results(x=data$time, y=data$Hfml, xlab=xlab, ylab=ylab, main="mid. lat.", col=col, yRange=rangeSHF$Hfml)
  par(new=TRUE)
  
  par("fig"=c(2/3,1,1/3,1))
  par("mar"=plot_mar)
  plot_box_results(x=data$time, y=data$Hfnl, xlab=xlab, ylab=ylab, main="north. lat.", col=col, yRange=rangeSHF$Hfnl)

  addLegend(pert=pert, col=col, lwd=3)
}

computePlotRange<-function(data, var_names){
  
  var_range = new('list')
  var_diff = new('list')
  for (name in var_names){
    var_range[[name]] = range(data[[name]])
    var_diff[[name]] = diff(var_range[[name]])
  }
  
  max_diff = max(unlist(var_diff)) * 1.04 #stretch range by 4% 
  
  var_plot_range = new('list')
  
  for(name in var_names){
    
    var_dev = (max_diff - var_diff[[name]])/2
    
    var_plot_range[[name]] = c(var_range[[name]][1] - var_dev, var_range[[name]][2] + var_dev)
    
  }
  
  return(var_plot_range)
  
}


print_parameter<-function(name, val, val_unit="", line=1, col=1, nx, ncol=2){
  
  if (val_unit != ""){
    val_unit = paste("[",val_unit,"]")
  }
  
  if (length(val)==1){
    val_txt = paste(val, val_unit, sep=" ")
  }
  else{
    val_txt = paste(val, collapse="; ")
    val_txt = paste (val_txt, val_unit, sep=" ")
  }
  
  x_name = (col - 1) * nx/ncol + 0.02 * nx
  x_val  = col * nx/ncol - 0.02 * nx
  
  text(labels=name, x=x_name, y=line, adj=0)
  text(labels=val_txt, x=x_val, y=line, adj=1)
  
}
