source('sevenbox.r')

Out=sevenbox_wrapper <-function(ncores=1, flag=1,perturbation=0,N=1000, cpo=4200, Ks=2.5e13, Kl=7.65e17, CFnl=1, CFml=1, CFsl=1, F3=0, F4=0, Initial=new('list')){
  
  if ((ncores>1) && (length(perturbation)>1) ){
    require(foreach)
    require(doMC)
    registerDoMC(ncores)

    N_pert = length(perturbation)
    
    Out = foreach(j=seq(1:N_pert), .combine='sevenbox_comb', .multicombine=TRUE, .inorder=TRUE) %dopar% {
      sevenbox(flag=flag,perturbation=perturbation[j],N=N, cpo=cpo, Ks=Ks, Kl=Kl, CFnl=CFnl, CFml=CFml, CFsl=CFsl, F3=F3, F4=F4, Initial=Initial)
    }
    
    return(Out)
    
  }
  else{
    Out = sevenbox(flag=flag,perturbation=perturbation,N=N, cpo=cpo, Ks=Ks, Kl=Kl, CFnl=CFnl, CFml=CFml, CFsl=CFsl, F3=F3, F4=F4, Initial=Initial)
  }
  
  return(Out)
  
}

Out = sevenbox_comb<-function(...){
  
  res_list = list(...)
  N_pert = length(res_list)
  
  N = length(res_list[[1]]$time)
  
  Out=list(time=array(NA, dim=N),
           Tglob=array(NA, dim=c(N, N_pert)),
           Tasl=array(NA, dim=c(N, N_pert)),
           Taml=array(NA, dim=c(N, N_pert)),
           Tanl=array(NA, dim=c(N, N_pert)),
           
           Tsl=array(NA, dim=c(N, N_pert)),
           Tml=array(NA, dim=c(N, N_pert)),
           Tnl=array(NA, dim=c(N, N_pert)),
           Td=array(NA, dim=c(N, N_pert)),
           
           Ssl=array(NA, dim=c(N, N_pert)),
           Sml=array(NA, dim=c(N, N_pert)),
           Snl=array(NA, dim=c(N, N_pert)),
           Sd=array(NA, dim=c(N, N_pert)),
           
           Fs30S=array(NA, dim=c(N, N_pert)),
           Fs50N=array(NA, dim=c(N, N_pert)),
           Fl30S=array(NA, dim=c(N, N_pert)),
           Fl50N=array(NA, dim=c(N, N_pert)),
           
           FWFAS=array(NA, dim=c(N, N_pert)),
           FWFAM=array(NA, dim=c(N, N_pert)),
           FWFAN=array(NA, dim=c(N, N_pert)),
           
           Hfnl=array(NA, dim=c(N, N_pert)),
           Hfml=array(NA, dim=c(N, N_pert)),
           Hfsl=array(NA, dim=c(N, N_pert)),
           
           phi=array(NA, dim=c(N, N_pert)))
  
  Out$time[] = res_list[[1]]$time
  
  for (i in 1:N_pert){
    Out$Tglob[,i] = res_list[[i]]$Tglob
    Out$Tasl[,i] = res_list[[i]]$Tasl
    Out$Taml[,i] = res_list[[i]]$Taml
    Out$Tanl[,i] = res_list[[i]]$Tanl
    
    Out$Tsl[,i] = res_list[[i]]$Tsl
    Out$Tml[,i] = res_list[[i]]$Tml
    Out$Tnl[,i] = res_list[[i]]$Tnl
    Out$Td[,i] = res_list[[i]]$Td
    
    Out$Ssl[,i] = res_list[[i]]$Ssl
    Out$Sml[,i] = res_list[[i]]$Sml
    Out$Snl[,i] = res_list[[i]]$Snl
    Out$Sd[,i] = res_list[[i]]$Sd
    
    Out$Fs30S[,i] = res_list[[i]]$Fs30S
    Out$Fs50N[,i] = res_list[[i]]$Fs50N
    Out$Fl30S[,i] = res_list[[i]]$Fl30S
    Out$Fl50N[,i] = res_list[[i]]$Fl50N
    
    Out$FWFAS[,i] = res_list[[i]]$FWFAS
    Out$FWFAM[,i] = res_list[[i]]$FWFAM
    Out$FWFAN[,i] = res_list[[i]]$FWFAN
    
    Out$Hfnl[,i] = res_list[[i]]$Hfnl
    Out$Hfml[,i] = res_list[[i]]$Hfml
    Out$Hfsl[,i] = res_list[[i]]$Hfsl
    
    Out$phi[,i] = res_list[[i]]$phi
  }
  
  return(Out)
  
}