ReflectionCalc <-function(r){
  return(r*Conj(r))
}

TransmissionCalc <-function(t,angle,theta2, incident_medium.index,exit_medium.index,polarisation){
  
  #remember t*conj(t) does NOT in general equal T.
  #correct phase conditions accounted for in scan files, and 
  #found originally at https://github.com/sbyrnes321/tmm
  
  if(polarisation=="s"){
    
    return(t*Conj(t)*(exit_medium.index*Re(cos(angle))/(incident_medium.index*Re(cos(theta2)))))
  
    } else if(polarisation=="p"){
      
    return(t*Conj(t)*(exit_medium.index/incident_medium.index)*Re(cos(angle)/cos(theta2)))
      } 
}