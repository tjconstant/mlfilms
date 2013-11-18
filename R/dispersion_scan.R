dispersion_scan<-function(angle_range=seq(0,pi/2,,100),wavelength_range=seq(350e-9,850e-9,,100), polarisation="p",incident_medium.index=1+0i,exit_medium.index=1+0i,layers,dispersive.function="none",dispersive.layers=NA){
  
  #library(Biodem) #need Biodem for raising matrix to a power function (mtx.exp)
  mtx.exp<-Biodem::mtx.exp
  
  #initalize reflection/transmission varible
  #Reflection<-c()
  Reflection<-numeric(length(angle_range)*length(wavelength_range))
  #Transmission<-c()
  cum_angle<-numeric(length(angle_range)*length(wavelength_range))
  cum_wavelength<-numeric(length(angle_range)*length(wavelength_range))
  counting_variable<-0
  
  #prevent numerical instablity by adding an extra entry and exit medium
  layers$index<-c(incident_medium.index,layers$index,exit_medium.index)
  layers$thickness<-c(0,layers$thickness,0)
  
  pb<-txtProgressBar(min=0,max=(length(wavelength_range)*length(angle_range)),style=3)
  pb.counter<-0
  
  for(wavelength in wavelength_range){
    
    for(angle in angle_range){
      
      counting_variable<-counting_variable+1
      
      M<-matrix(c(1,0,0,1),nrow=2,ncol=2,byrow=TRUE)
      
      for(layer in 1:length(layers$index)){
        
        if(dispersive.function!="none") {
          disp.index<-match.fun(dispersive.function)
          layers$index[dispersive.layers+1]<-disp.index(wavelength)
        }
        
        L<-TMatrix(lambda0=wavelength,polarisation=polarisation,n0=incident_medium.index,n1=layers$index[layer],n2=exit_medium.index,d1=layers$thickness[layer],theta0=angle)
        if(layer==1) gamma0<-L$gamma0
        if(layer==length(layers$index)) gamma2<-L$gamma2
        M<-M%*%L$TMatrix
        
      }
      
      #repeat unit cells
      M<-mtx.exp(M,layers$repetitions)
      
      r<-rFromTMatrix(M=M,gamma0=gamma0,gamma2=gamma2)
      Reflection[counting_variable]<-r*Conj(r)
      
      #t<-tFromTMatrix(M=M,gamma0=gamma0,gamma2=gamma2)
      #Transmission<-c(Transmission,t*Conj(t))
      
      cum_angle[counting_variable]<-angle
      cum_wavelength[counting_variable]<-wavelength
      
      #pb.counter<-pb.counter+1
      setTxtProgressBar(pb=pb,value=counting_variable)
      
    }
  }
  return(data.frame(angle=cum_angle,wavelength=cum_wavelength,Reflection=Re(Reflection)))#,Transmission=Re(Transmission)))
}

