angle_scan<-function(angle_range=seq(0,pi/2,,500),wavelength=633e-9, polarisation="p",incident_medium.index=1+0i,exit_medium.index=1+0i,layers){

  #library(Biodem) #need Biodem for raising matrix to a power function (mtx.exp)
  mtx.exp<-Biodem::mtx.exp
  
  #if(is.vector(layers)) layers<-list(index=layers[1],thickness=layers[2],repetitions=layers[3] )
  
  #initalize reflection/transmisson varible
  Reflection<-numeric(length(angle_range))
  Transmission<-numeric(length(angle_range))
  r <- numeric(length(angle_range))
  t <- numeric(length(angle_range))
  
  #prevent numerical instablity by adding an extra entry and exit medium
  layers$index<-c(incident_medium.index,layers$index,exit_medium.index)
  layers$thickness<-c(0,layers$thickness,0)
  
  counting_variable<-0
  
  
  for(angle in angle_range){
    
    counting_variable<-counting_variable+1
    
    M<-matrix(c(1,0,0,1),nrow=2,ncol=2,byrow=TRUE)
    
    for(layer in 1:length(layers$index)){
      
      L<-TMatrix(lambda0=wavelength,polarisation=polarisation,n0=incident_medium.index,n1=layers$index[layer],n2=exit_medium.index,d1=layers$thickness[layer],theta0=angle)
      if(layer==1) gamma0<-L$gamma0
      if(layer==length(layers$index)) gamma2<-L$gamma2
      M<-M%*%L$TMatrix
      
    }
    
    #repeat unit cells
    M<-mtx.exp(M,layers$repetitions)
    
    r<-rFromTMatrix(M=M,gamma0=gamma0,gamma2=gamma2)
    Reflection[counting_variable]<-ReflectionCalc(r)
  
    t<-tFromTMatrix(M=M,gamma0=gamma0,gamma2=gamma2)
    Transmission[counting_variable]<-TransmissionCalc(t,angle,L$theta2,incident_medium.index,exit_medium.index,polarisation)

}
  return(data.frame(angle=angle_range,
                    Reflection=Re(Reflection),
                    Transmission=Re(Transmission),
                    Absorption=1-Re(Transmission)-Re(Reflection),
                    r = r,
                    t = t))
}


