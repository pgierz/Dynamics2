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
