
rb_lbm <- function(lx=100, ly=52, N_t0=50, Pr=1, Ra=20000, beta=11.18, N_out=50, ShinyData=NULL){
  
  T_top  = 0;		#dimensionless Temperature: Cooling at the top wall
  T_bot	= 1;		#dimensionless Temperature: Heating at the bottom wall
  
  #Compute simulation parameter
  T0 = (T_top + T_bot)/2;
  
  delta_x = 1/(ly -2);		#Size of a cell in lattice units
  delta_t = beta * delta_x * delta_x;
  max_t = ceiling(N_t0 / delta_t) +1 ;
  
  out_freq = floor(max_t/(N_out-1));
  
  g_y = beta^2 * delta_x^3;
  g = c(0, g_y);
  
  nu = sqrt(Pr/Ra) * delta_t / (delta_x * delta_x);	#kinematic viscosity in lattice units
  k  = sqrt(1/(Ra*Pr)) * delta_t / (delta_x * delta_x);	#thermal diffusivity in lattice units
  
  omega_fl = 1/(3*nu + 0.5);	#relaxation parameter for fluid lattice
  omega_T  = 1/(2*k + 0.5);	#relaxation parameter for temperature lattice
  
  #D2Q9 lattice parameter (for the fluid lattice)
  w_fl = c(4/9, 1/9, 1/9, 1/9, 1/9, 1/36, 1/36, 1/36, 1/36);	#lattice weights
  cx_fl = c(0, 1, 0, -1, 0, 1, -1, -1, 1);			#x-component of lattice velocity
  cy_fl = c(0, 0, 1, 0, -1, 1, 1, -1, -1);			#y-component of lattice velocity
  opp_fl = c(1, 4, 5, 2, 3, 8, 9, 6, 7);				#index of opposite element
  
  #D2Q5 lattice parameter (for the temperature lattice)
  w_T = c(1/3, 1/6, 1/6, 1/6, 1/6);	#lattice weights
  cx_T = c(0, 1, 0, -1, 0);		#x-component of lattice velocity
  cy_T = c(0, 0, 1, 0, -1);		#y-component of lattice velocity
  opp_T = c(1, 4, 5, 2, 3);		#index of opposite element
  
  idxRangeFluid = 1:9
  idxRangeTemp = 1:5
  
  
  #Initial conditions
  rho = array(1, c(lx, ly));		#Set rho=1 on the whole lattice
  ux = array(0, c(lx, ly));		#Set ux=0 on the whole lattice
  uy = array(0, c(lx, ly));		#Set uy=0 on the whole lattice
  
  #Initialize T with a linear vertical temperature gradient
  T = array(0, c(lx, ly));
  
  for (y in 1:ly)	{
    T[,y] = (T_top - T_bot)/(ly-1) * (y-1) + T_bot;
  }
  
  #Set small trigger to break symmetry
  T[lx/2+1, 1] = 1.1 * T_bot;   # original
  
  #Initalize lattices with equilibrium functions
  
  flIn = array(0, c(9, lx, ly));
  
  for (i in idxRangeFluid){
    cu_fl = 3* (cx_fl[i] * ux + cy_fl[i] * uy);
    flIn[i,,] = rho * w_fl[i] * (1 + cu_fl + 0.5 * cu_fl^2 - 1.5 * (ux^2 + uy^2));
  }
  
  TIn = array(0, c(5, lx, ly));
  
  for (i in idxRangeTemp){
    cu_T = 3* (cx_T[i] * ux + cy_T[i] * uy);
    TIn[i,,] = T * w_T[i] * (1 + cu_T);
  }
  
  # Pre-declare temporary arrays
  cu_fl = array(0, c(lx, ly));
  flEq = array(0, c(lx, ly));
  force = array(0, c(lx, ly));
  cu_T = array(0, c(lx, ly));
  TEq = array(0, c(lx, ly));
  flOut = array(0, c(9, lx, ly));
  TOut = array(0, c(5, lx, ly));
  
  Out=new('list')
  
  Out$time = array(NA, dim=N_out)
  Out$T = array(NA, dim=c(N_out, lx, ly))
  Out$rho = array(NA, dim=c(N_out, lx, ly))
  Out$u = array(NA, dim=c(N_out, lx, ly, 2))
  
  Nwrite=1
  
  Out$time[Nwrite] = 0
  Out$T[Nwrite,,] = T
  Out$rho[Nwrite,,] = rho
  Out$u[Nwrite,,,1] = ux
  Out$u[Nwrite,,,2] = uy
  
  Nwrite = Nwrite + 1
  
  # Main Loop
  for (time_step in 1:max_t){
    #Compute macroscopic values
    rho = colSums(flIn, dims=1); 
    T = colSums(TIn, dims=1);
    ux = colSums( cx_fl*flIn, dims=1 ) / rho;
    uy = colSums( cy_fl*flIn, dims=1 ) / rho;
    
    #Collision Step
    #Fluid
    for (i in idxRangeFluid){
      cu_fl = 3* (cx_fl[i] * ux + cy_fl[i] * uy);
      flEq= rho * w_fl[i] * (1 + cu_fl + 0.5 * cu_fl^2 - 1.5 * (ux^2 + uy^2));
      force = 3* w_fl[i]* rho * (T-T0)* (cx_fl[i] * g[1] + cy_fl[i] * g[2])/(T_bot - T_top);
      
      flOut[i,,] = (1.-omega_fl)*flIn[i,,] + omega_fl*flEq + force;
    }
    
    #Temperature
    for (i in idxRangeTemp){
      cu_T = 3* (cx_T[i] * ux + cy_T[i] * uy);
      TEq = T * w_T[i] * (1 + cu_T);
      
      TOut[i,,] = (1.-omega_T)*TIn[i,,] + omega_T*TEq;
    }
    
    #"Bounce Back" Boundary Conditions for Fluid
    for (i in idxRangeFluid){
      flOut[i,,1] = flIn[opp_fl[i],,1];
      flOut[i,,ly] = flIn[opp_fl[i],,ly];
    }
    
    #Streaming Step
    #Fluid
    for (i in idxRangeFluid){
      flIn[i,,] = circshift(flOut[i,,], c(cx_fl[i], cy_fl[i]));
    }
    
    #Temperature
    for (i in idxRangeTemp){
      TIn[i,,] = circshift(TOut[i,,], c(cx_T[i], cy_T[i]));
    }
    
    #"Constant Temperature" Boundary Condition
    TIn[5,,ly] = T_top - TIn[1,,ly] - TIn[2,,ly] - TIn[3,,ly] - TIn[4,,ly];	#Top Wall
    TIn[3,,1] = T_bot - TIn[1,,1] - TIn[2,,1] - TIn[4,,1] - TIn[5,,1];	#Bottom Wall
    

    if ((time_step %% out_freq ==0) && (Nwrite <= N_out) ){
      
      Out$time[Nwrite] = time_step
      Out$T[Nwrite,,] = T
      Out$rho[Nwrite,,] = rho
      Out$u[Nwrite,,,1] = ux
      Out$u[Nwrite,,,2] = uy
      
      Nwrite = Nwrite + 1
      
      progress = (time_step/max_t)
      
      #Show progress
      if(is.null(ShinyData))
       cat("\r Progress: ", format(progress * 100, digits=1), "%")
      else{

        ShinyData$session$sendCustomMessage(type="updateProgress", list(id=ShinyData$id, 
                                                                   progress=progress,
                                                                   message=paste(round(progress*100), "%", sep="")))


      }
      
    }
    
    
  }
  
  return(Out)
  
}

circshift <- function(a, stepsize) {
  
  if(is.matrix(a) && length(stepsize)==2){
    n = nrow(a);
    m = ncol(a);
    
    srow = stepsize[1] %% n;
    scol = stepsize[2] %% m;
    
    a = a[( ((1:n) - srow -1) %% n) +1, ( ((1:m) -scol -1) %% m) +1];
  }
  else stop("a has to be a matrix and stepsize has to be 2 element vector");
  
  return(a)
}