[![Build Status](https://travis-ci.org/tjconstant/mlfilms.svg?branch=master)](https://travis-ci.org/tjconstant/mlfilms) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/mlfilms)](https://cran.r-project.org/package=mlfilms)

mlfilms
=======

Calculation of reflection from multilayer films using the transfer matrix method.

This package uses the transfer matrix method outlined in [Pedrotti, Pedrotti and Pedrotti](http://www.amazon.com/Introduction-Optics-3rd-Frank-Pedrotti/dp/0131499335) to calculate the reflection and transmission of plane polarised light from an arbitary stack of thin films.

To Install
----------

From within an R session, use the devtools package like so:
```
install.packages('devtools')
library(devtools)

install_github('tjconstant/mlfilms')
```

The Stack
----------

All stacks are defined as a list of parameters, with the layers listed sequentially from top (incident side) to bottom (exit side). 
For Example:

```r
mystack <- list(index=c(1,1.5,1,1.5+0.5i),thickness=c(100e-9,50e-9,20e-9,100e-9), repetitions=1)

```
