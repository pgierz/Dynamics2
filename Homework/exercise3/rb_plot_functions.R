
plot_field <- function(data, filename='./out.png', title="", zlim=NULL) {
  Size = dim(data);
  png(filename, width=1024, height=(1024/Size[1] * Size[2]));

  if (is.null(zlim)) {
    filled.contour(data, main=title, xlab="x", ylab="y", x=c(1:Size[1]), y=c(1:Size[2]));
  }
  else {
    filled.contour(data, main=title, xlab="x", ylab="y", x=c(1:Size[1]), y=c(1:Size[2]), zlim=zlim);
  }
  dev.off();
}


plot_vec_field <-function(ux, uy, nx=10, ny=floor(dim(ux)[2]/(dim(ux)[1]/nx)), scale=1,  title="", uMax=NULL, ...){
  
  if (is.null(uMax))
    uMax = max(sqrt(ux^2+uy^2))
  
  Orig = list(x=getCellCenter(dim(ux)[1]),
              y=getCellCenter(dim(ux)[2]))

  Arr = expand.grid(x=getCellCenter(nx), 
                    y=getCellCenter(ny))

  UxArr = interpolSurf_Fields(append(Orig, list(z=ux)), Arr)
  UyArr = interpolSurf_Fields(append(Orig, list(z=uy)), Arr)
  absUArr = sqrt(UxArr^2 + UyArr^2)

  if (uMax > 0){
    dCellX = abs(Arr$x[2] - Arr$x[1])
    dCellY = abs(Arr$y[1 + nx] - Arr$y[1])
    dCell = min(dCellX, dCellY)
    
    LenArr = absUArr/uMax * dCell
    LenArrX = UxArr/uMax * dCell
    LenArrY = UyArr/uMax * dCell
    LenArrIn = LenArr * par("pin")[2]/(par("usr")[4]-par("usr")[3])
    LenArrHatIn = LenArrIn * scale
    
    hasntLen = ((LenArrIn < 1/72) & (absUArr > 0))
    
    for(i in which(hasntLen)){
      minScale = 1/(72 * LenArrIn[i])
      LenArrX[i] = minScale * LenArrX[i]
      LenArrY[i] = minScale * LenArrY[i]
      LenArrIn[i] = 1/72
    }

    hasLen = (LenArrIn >= 1/72)

    Dot = Arr[!hasLen,]
    
    if (length(Dot$x) < length(Arr$x)){
      Arr = Arr[hasLen,]
      UxArr = UxArr[hasLen]
      UyArr = UyArr[hasLen]
      absUArr = absUArr[hasLen]
      LenArr = LenArr[hasLen]
      LenArrX = LenArrX[hasLen]
      LenArrY = LenArrY[hasLen]
      LenArrIn = LenArrIn[hasLen]
      LenArrHatIn = mean(LenArrHatIn[hasLen])
      if(LenArrHatIn < 3/72)
        LenArrHatIn = 3/72
      
      arrows(x0=Arr$x, y0=Arr$y, x1=Arr$x+LenArrX, y1=Arr$y+LenArrY, length=LenArrHatIn, ...)  
    }
    
  }
  else{
    Dot = Arr
  }

  points(x=Dot$x, y=Dot$y, cex=0.1, ...)

}

getCellCenter <- function(nCells, Min=0, Max=1){
  
  len = Max - Min
  
  ds = len/nCells
  
  xMin = Min + ds/2
  xMax = Max - ds/2
  
  return(seq(from=xMin, to=xMax, by=ds))
  
}


interpolSurf_Fields <- function (obj, loc) 
{
  x <- obj$x
  y <- obj$y
  z <- obj$z
  nx <- length(x)
  ny <- length(y)
  lx <- approx(x, 1:nx, loc[, 1])$y
  ly <- approx(y, 1:ny, loc[, 2])$y
  lx1 <- floor(lx)
  ly1 <- floor(ly)
  ex <- lx - lx1
  ey <- ly - ly1
  ex[lx1 == nx] <- 1
  ey[ly1 == ny] <- 1
  lx1[lx1 == nx] <- nx - 1
  ly1[ly1 == ny] <- ny - 1
  
  return(z[cbind(lx1, ly1)] * (1 - ex) * (1 - ey) + 
           z[cbind(lx1 +1, ly1)] * ex * (1 - ey) + 
           z[cbind(lx1, ly1 + 1)] * (1 - ex) * ey + 
           z[cbind(lx1 + 1, ly1 + 1)] * ex * ey)
}


plotRBdata <- function(RBdata, t_idx=1, plot_type, plotVectors=FALSE, filledCont=TRUE){
  CurTime = RBdata$time[t_idx]
  switch(plot_type,
         temp = {plot_data = RBdata$T[t_idx,,]
                 var_name="Temperature"},
         rho = {plot_data = RBdata$rho[t_idx,,]
                var_name="Density"},
         ux = {plot_data = RBdata$u[t_idx,,,1]
               var_name="x-Comp. Velocity"},
         uy = {plot_data = RBdata$u[t_idx,,,2]
               var_name="y-Comp. Velocity"},
         abs_u = {plot_data = sqrt(RBdata$u[t_idx,,,1]^2+RBdata$u[t_idx,,,2]^2)
                  var_name="absol. Velocity"},
         vort = {plot_data = compute_vorticity_2d(RBdata$u[t_idx,,,1], RBdata$u[t_idx,,,2])
                 var_name="Vorticity"})
  
  dimVar = dim(plot_data)
  
  labX = axisTicks(usr=c(0,dimVar[1]), nint=4, log=FALSE)
  posX = labX/dimVar[1]
  
  labY = axisTicks(usr=c(0,dimVar[2]), nint=4, log=FALSE)
  posY = labY/dimVar[2]
  
  if (filledCont){  
    par("mar"=c(4,4,2,1))
    filled.contour(z=plot_data, 
                   main=paste(var_name,"- frame", t_idx, "( timestep:", CurTime,")"), 
                   plot.axes={
                     axis(side=1, labels=labX, at=posX)
                     axis(side=2, labels=labY, at=posY)
                     mtext(text="x", side=1, line=3)
                     mtext(text="y", side=2, line=3)
                     if(plotVectors){
                       par("new"=TRUE)
                       plot_vec_field(ux=RBdata$u[t_idx,,,1], 
                                      uy=RBdata$u[t_idx,,,2], 
                                      nx=15,
                                      uMax=max(sqrt(RBdata$u[,,,1]^2 + RBdata$u[,,,2]^2)))
                     }
                   }
    )
  }
  else{
    par("mar"=c(4,4,2,2))
    contour(z=plot_data,
            main=paste(var_name,"- frame", t_idx, "( timestep:", CurTime,")"),
            xaxs="i",
            yaxs="i",
            xaxt="n",
            yaxt="n"
    )
    axis(side=1, labels=labX, at=posX)
    axis(side=2, labels=labY, at=posY)
    mtext(text="x", side=1, line=3)
    mtext(text="y", side=2, line=3)
    
    if(plotVectors){
      par("new"=TRUE)
      plot_vec_field(ux=RBdata$u[t_idx,,,1], 
                     uy=RBdata$u[t_idx,,,2], 
                     nx=15,
                     uMax=max(sqrt(RBdata$u[,,,1]^2 + RBdata$u[,,,2]^2)),
                     col="red")
    }
  }
}


compute_vorticity_2d <- function(Ax, Ay) {
  
  Size = dim(Ax);
  
  vort = array(0, Size);
  
  for (i in 1:Size[1]){
    for (j in 1:Size[2]){
      
      if( (i==1) | (i==Size[1]) | (j==1) | (j==Size[2]) ){
        vort[i,j] = NA;
      }
      else {
        
        vort[i,j] = (Ax[i, j-1] - Ax[i, j+1] + Ay[i+1, j] - Ay[i-1, j]) / 2 ;
        
      }
      
    }
  }
  
  return(vort);
  
}
