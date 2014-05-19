#clear workspace
rm(list=ls());

#Load required functions
source('rb_plot_functions.R');
source('rb_functions.R');



#Parameter Definitions
###############################################################################

lx 	= 100;		#Number of horizontal cells
ly	= 52;		#Number of vertical cells
N_t0 = 50;		# origianlly set to 100

Pr   = 1;		#Prandtl number
Ra	= 20000;	#Rayleigh number
beta = 11.18;	#coupling parameter between dx and dt

#Output Parameter
N_out = 50;		#number of output
out_dir = "./out/";	#Output directory
save_T_img = 1;		#save image of temperature field (1=yes, 0=no)
save_usq_img = 0;	#save image of u^2 field (1=yes, 0=no)
save_ux_img = 0;	#save image of ux field (1=yes, 0=no)
save_uy_img = 0;	#save image of uy field (1=yes, 0=no)
save_rho_img = 0;	#save image of rho field (1=yes, 0=no)
save_u_vec_img = 1;	#save image of velocity vectors (1=yes, 0=no)
save_vorticity_img = 1; #save image of vorticity field (1=yes, 0=no)
save_T_data = 1;	#save data of temperature field (1=yes, 0=no)
save_usq_data = 0;	#save data of u^2 field (1=yes, 0=no)
save_ux_data = 1;	#save data of ux field (1=yes, 0=no)
save_uy_data = 1;	#save data of uy field (1=yes, 0=no)
save_rho_data = 0;	#save data of rho field (1=yes, 0=no)
save_vorticity_data = 0;#save data of vorticity field (1=yes, 0=no)
###############################################################################


T_top  = 0;		#dimensionless Temperature: Cooling at the top wall
T_bot	= 1;		#dimensionless Temperature: Heating at the bottom wall

#Compute simulation parameter
T0 = (T_top + T_bot)/2;

delta_x = 1/(ly -2);		#Size of a cell in lattice units
delta_t = beta * delta_x * delta_x;
max_t = round((N_t0 / delta_t) + 0.5) +1 ;

out_freq = round(max_t/N_out);

g_y = beta^2 * delta_x^3;
g = c(0, g_y);

#cat('dx', delta_x, '\ndt', delta_t, '\nmax_t', max_t, '\nout_freq', out_freq,'\ng_y', g_y);

nu = sqrt(Pr/Ra) * delta_t / (delta_x * delta_x);	#kinematic viscosity in lattice units
k  = sqrt(1/(Ra*Pr)) * delta_t / (delta_x * delta_x);	#thermal diffusivity in lattice units

omega_fl = 1/(3*nu + 0.5);	#relaxation parameter for fluid lattice
omega_T  = 1/(3*k + 0.5);	#relaxation parameter for temperature lattice

#cat('\nnu', nu, '\nk', k, '\nomega_fl', omega_fl, '\nomega_T', omega_T, '\n');


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

# Main Loop
for (time_step in 1:max_t){
  #Compute macroscopic values
  rho = colSums(flIn, dims=1); 
  T = colSums(TIn, dims=1);
  ux = colSums( cx_fl*flIn, dims=1 ) / rho;
  uy = colSums( cy_fl*flIn, dims=1 ) / rho;

  #Collision Step
  #Fluid momentum
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

  #Output (only every 'out_freq' steps)
  if (!file.exists(out_dir)) {
    dir.create(out_dir);
  }
  if (time_step %% out_freq ==0) {

    if (save_T_img == 1) {
      plot_field(data=T
                 , filename=paste(out_dir, "T", sprintf("%06.0f",time_step), ".png", sep="")
                 , title=paste("T \n time=", time_step), zlim=c(0, 1.1));
    }

    if (save_usq_img == 1) {
      usq = ux^2 + uy^2;
      plot_field(data=usq
                 , filename=paste(out_dir, "Usq", sprintf("%06.0f",time_step), ".png", sep="")
                 , title=paste("U^2 \n time=", time_step));
    }

    if (save_ux_img == 1) {
      plot_field(data=ux
                 , filename=paste(out_dir, "Ux", sprintf("%06.0f",time_step), ".png", sep="")
                 , title=paste("U_x \n time=", time_step));
    }

    if (save_uy_img == 1) {
      plot_field(data=uy
                 , filename=paste(out_dir, "Uy", sprintf("%06.0f",time_step), ".png", sep="")
                 , title=paste("U_y \n time=", time_step));
    }

    if (save_rho_img == 1) {
      plot_field(data=rho
                 , filename=paste(out_dir, "Rho", sprintf("%06.0f",time_step), ".png", sep="")
                 , title=paste("Rho \n time=", time_step), zlim=c(0, 1.1));
    }

    if (save_u_vec_img == 1) {
      plot_vec_field(ux, uy, ny=10, scale=2
                     , filename=paste(out_dir, "U_vec", sprintf("%06.0f",time_step), ".png", sep="")
                     , title=paste("U \n time=", time_step));
    }

    if (save_vorticity_img == 1) {
      plot_vorticity(ux, uy
                     , filename=paste(out_dir, "Vorticity", sprintf("%06.0f",time_step), ".png", sep="")
                     , title=paste("Vorticity \n time=", time_step));
    }

    if (save_T_data == 1) {
      write.table(T, paste(out_dir, "T_", sprintf("%06.0f",time_step), ".txt", sep=""));
    }

    if (save_usq_data == 1) {
      usq = ux^2 + uy^2;
      write.table(usq, paste(out_dir, "Usq_", sprintf("%06.0f",time_step), ".txt", sep=""));
    }

    if (save_ux_data == 1) {
      write.table(ux, paste(out_dir, "Ux_", sprintf("%06.0f",time_step), ".txt", sep=""));
    }

    if (save_uy_data == 1) {
      write.table(uy, paste(out_dir, "Uy_", sprintf("%06.0f",time_step), ".txt", sep=""));
    }

    if (save_rho_data == 1) {
      write.table(rho, paste(out_dir, "Rho_", sprintf("%06.0f",time_step), ".txt", sep=""));
    }

    if (save_vorticity_data == 1) {
      Vort = compute_vorticity_2d(ux, uy);
      write.table(Vort, paste(out_dir, "Vorticity_", sprintf("%06.0f",time_step), ".txt", sep=""));
    }

    #Show progress
    cat("\r Progress: ", format((time_step * 100/max_t), digits=1), "%");

  }


}

cat("\n Finished! \n");

