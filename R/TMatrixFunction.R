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