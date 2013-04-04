rFromTMatrix<- function(M,gamma0,gamma2){
  
  r<-((gamma0*M[1,1])+(gamma0*gamma2*M[1,2])-(M[2,1])-(gamma2*M[2,2]))/((gamma0*M[1,1])+(gamma0*gamma2*M[1,2])+(M[2,1])+(gamma2*M[2,2]))
  return(r)
  
}

tFromTMatrix<- function(M,gamma0,gamma2){
  #remember t*conj(t) does NOT in general equal T
  t<-(2*gamma0)/((gamma0*M[1,1])+(gamma0*gamma2*M[1,2])+(M[2,1])+(gamma2*M[2,2]))
  return(t)
  
}