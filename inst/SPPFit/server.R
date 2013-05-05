library(shiny)

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