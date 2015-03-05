.onAttach<-function(libname=find.package("mlfilms"),pkgname="mlfilms"){  
  
  this_release<-packageVersion("mlfilms")
  
  GET<-httr::GET
  content<-httr::content
  
  latest_release<-GET("https://api.github.com/repos/tjconstant/mlfilms/releases")
  
  latest_release_content<-content(latest_release)
  
  latest_release_number<-unlist(strsplit(latest_release_content[[1]]$tag_name,"-"))[1]
  
  if(this_release!=latest_release_number[1]){
    packageStartupMessage(paste("WARNING: mlfilms v",this_release," is not the current stable release (mlfilms v",latest_release_number,")",sep=""))
  }else{
    packageStartupMessage(paste("mlfilms v",this_release," '",latest_release_content[[1]]$name,"'",sep=""))
  }
  
}

