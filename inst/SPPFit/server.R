library(shiny)

epsilon.im<-function(n,k){
  return(2*n*k)
}

epsilon.re<-function(n,k){
  return(n^2-k^2)
}

k<-function(espr,espi){
  return(Im(sqrt(espr+espi*(0+1i))))
}

n<-function(espr,espi){
  return(Re(sqrt(espr+espi*(0+1i))))
}

rindex<-function(espr,espi){
  return((sqrt(espr+espi*(0+1i))))
}

rFromTMatrix<- function(M,gamma0,gamma2){
  
  r<-((gamma0*M[1,1])+(gamma0*gamma2*M[1,2])-(M[2,1])-(gamma2*M[2,2]))/((gamma0*M[1,1])+(gamma0*gamma2*M[1,2])+(M[2,1])+(gamma2*M[2,2]))
  return(r)
  
}

tFromTMatrix<- function(M,gamma0,gamma2){
  #remember t*conj(t) does NOT in general equal T, and this needs fixing.
  t<-(2*gamma0)/((gamma0*M[1,1])+(gamma0*gamma2*M[1,2])+(M[2,1])+(gamma2*M[2,2]))
  return(t)
  
}

#transfer matrix thin film interference
#based on pedrotti pedrotti & pedrotti p.478

TMatrix<-function(lambda0=633e-9,polarisation='s',n0=complex(real=1),n1=complex(real=1.50),n2=complex(real=1.50),d1=40e-9,theta0=0){
  
  #light
  k0<-2*pi/lambda0
  c<-3e8
  
  #snell!
  theta1<-asin((n0/n1)*sin(theta0))
  theta2<-asin((n1/n2)*sin(theta1))
  
  #theta2<-Re(theta2)-abs(Im(theta2))*1i 
  #this is a bodge to make it work for metals. need permanant solution here.
  #for dielectrics this line makes no difference, and could even be removed.
  #not needed after using correct angles. fixed, but remains here as a testiment to stupidity
  
  #pedrotti notation
  #gammas for ease of polarisation switches
  
  if(polarisation=="s"){
    gamma0<-(n0/c)*cos(theta0)
    gamma1<-(n1/c)*cos(theta1)
    gamma2<-(n2/c)*cos(theta2)
  }else{
    gamma0<-(n0/c)/cos(theta0)
    gamma1<-(n1/c)/cos(theta1)
    gamma2<-(n2/c)/cos(theta2)
  }
  
  #phase difference that develops in one traversal of the film
  phase<-k0*n1*d1*cos(theta1)
  
  #assemble the transfer matrix
  M11<-cos(phase)
  M12<--1i*(sin(phase)/gamma1) #M12 and M21 corrected to be negative to account for refractive index definintion n=n+ik
  M21<--1i*(sin(phase)*gamma1) #see above
  M22<-cos(phase)
  
  M<-matrix(c(M11,M12,M21,M22),nrow=2,ncol=2,byrow=TRUE)
  
  return(list(TMatrix=M, gamma0=gamma0, gamma1=gamma1,gamma2=gamma2, theta0=theta0, theta1=theta1, theta2=theta2))
  
}

angle_scan<-function(angle_range=seq(0,pi/2,,500),wavelength=633e-9, polarisation="p",incident_medium.index=1+0i,exit_medium.index=1+0i,layers){
  
  #library(Biodem) #need Biodem for raising matrix to a power function (mtx.exp)
  mtx.exp<-function (X, n) 
  {
    if (n != round(n)) {
      n <- round(n)
      warning("rounding exponent `n' to", n)
    }
    phi <- diag(nrow = nrow(X))
    pot <- X
    while (n > 0) {
      if (n%%2) 
        phi <- phi %*% pot
      n <- n%/%2
      pot <- pot %*% pot
    }
    return(phi)
  }
  
  #if(is.vector(layers)) layers<-list(index=layers[1],thickness=layers[2],repetitions=layers[3] )
  
  #initalize reflection/transmisson varible
  Reflection<-numeric(length(angle_range))
  #Transmission<-c()
  
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
    Reflection[counting_variable]<-r*Conj(r)
    
    #t<-tFromTMatrix(M=M,gamma0=gamma0,gamma2=gamma2)
    #Transmission<-c(Transmission,t*Conj(t))
    
  }
  return(data.frame(angle=angle_range,Reflection=Re(Reflection)))#,Transmission=Re(Transmission)))
}




# Define server logic required to generate and plot
shinyServer(function(input, output) {
  
  # Function that generates a plot of the distribution. The function
  # is wrapped in a call to reactivePlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs change
  #  2) Its output type is a plot 
  #
  output$reflPlot <- renderPlot( {
    
    if(!is.null(input$datafile)){
      inFile<-input$datafile
      r_data<-read.csv(inFile$datapath)
      angles<-r_data$angle
    }else{
      angles<-c(seq(0,90,,500))*pi/180
    }
    
    # generate multi-layer reflection angle sweep and plot it
    layers<-vector(mode="list")
    
    layers$index<-c(rindex(input$epsiR,input$epsiI),input$overlayer_index+0i)
    layers$thickness<-c(input$thickness*1e-9,input$overlayer_thickness*1e-9)
    layers$repetitions<-1
    
    par(mar=c(4,4,1,1))
    #library(ggplot2)
    plot(NA,NA,xlim=range(angles),ylim=c(0,1),type='l',xlab="internal angle (degrees)",ylab="Reflectivity",xaxt='n',xaxs="i")
    grid()
    axis(side=1,at=seq(0,90,5)*pi/180,labels=seq(0,90,5))
    
    if(input$p.pol){
    lines(angle_scan(angle_range=angles,wavelength=input$wavelength*1e-9,layers=layers,incident_medium.index=1.5+0i,exit_medium.index=1+0i),lwd=2)
         }
    if(input$s.pol){
      lines(angle_scan(angle_range=angles,wavelength=input$wavelength*1e-9,layers=layers,incident_medium.index=1.5+0i,exit_medium.index=1+0i,polarisation="s"),col=2,lwd=2)
    }
    
    
    #plot data file if present
    if(!is.null(input$datafile)){
      lines(r_data$angle,r_data$Reflection,lwd=2,col=4)
    }
    
    #attempt fitting
    
    if(input$fit_flag==T){
      #first guess
      
#       layers$index<-c(rindex(input$epsiR,input$epsiI),input$overlayer_index+0i)
#       layers$thickness<-c(input$thickness*1e-9,input$overlayer_thickness*1e-9)
#       layers$repetitions<-1
#       R_plasmon_guess<-angle_scan(angle_range=angles,incident_medium.index=1.5+0i,exit_medium.index=1+0i,layers=layers)
#       
      rsquared<-function(parameters){ 
        layers<-vector(mode="list")
        layers$index<-parameters[1]+parameters[2]*(1i)
        layers$thickness<-Re(parameters[3])*1e-9
        layers$repetitions<-1
        #print(layers)
        R_plasmon_fit<-angle_scan(angle_range=angles,incident_medium.index=1.5+0i,exit_medium.index=1+0i,layers=layers)
        #points(R_plasmon_fit$angle*180/pi,R_plasmon_fit$R,col=2)
        return((sum(r_data$Reflection-R_plasmon_fit$R)^2)/length(R_plasmon_fit$R))
      }
      
      fitparams<-optim(par=c(n(input$epsiR,input$epsiI),k(input$epsiR,input$epsiI),input$thickness),fn=rsquared,method="L-BFGS-B",control=list(maxit=500))
      #fitparams<-optim(par=c(n(input$epsiR,input$epsiI),k(input$epsiR,input$epsiI),input$thickness),fn=rsquared,method="BFGS",control=list(maxit=500))
      
      layers<-vector(mode="list")
      layers$index<-fitparams$par[1]+fitparams$par[2]*(1i)
      layers$thickness<-Re(fitparams$par[3])*1e-9
      layers$repetitions<-1
      fittedR<-angle_scan(angle_range=angles,incident_medium.index=1.5+0i,exit_medium.index=1+0i,layers=layers)
      lines(fittedR[,1],fittedR[,2],col=2,lwd=2)
      
      output$fittedparameters<-renderTable({ 
        if (is.null(input$datafile)) {
          # User has not uploaded a file yet
          return(NULL)
        }
        data.frame(thickness=Re(fitparams$par[3]),epsilon_real=epsilon.re(fitparams$par[1],fitparams$par[2]),epsilon_imaginary=epsilon.im(fitparams$par[1],fitparams$par[2]),rsquared=1-fitparams$value)})
    }

    
  })
  
  output$filetable<-renderTable({ 
    if (is.null(input$datafile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    inFile<-input$datafile
    read.csv(inFile$datapath)})
  
  

})