
mlfilms <img src="man/figures/logo.png" align="right" />
========================================================

[![Build Status](https://travis-ci.org/tjconstant/mlfilms.svg?branch=master)](https://travis-ci.org/tjconstant/mlfilms) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/tjconstant/mlfilms?branch=master&svg=true)](https://ci.appveyor.com/project/tjconstant/mlfilms) [![codecov](https://codecov.io/gh/tjconstant/mlfilms/branch/master/graph/badge.svg)](https://codecov.io/gh/tjconstant/mlfilms)

Calculation of reflection from multilayer films using the transfer matrix method.

This package uses the transfer matrix method outlined in [Pedrotti, Pedrotti and Pedrotti](http://www.amazon.com/Introduction-Optics-3rd-Frank-Pedrotti/dp/0131499335) to calculate the reflection and transmission of plane polarised light from an arbitary stack of thin films.

To Install
----------

From within an R session, use the devtools package like so:

``` r
devtools::install_github('tjconstant/mlfilms')
```

The Layers
----------

All layers are defined as a list of parameters, with the layers listed sequentially from top (incident side) to bottom (exit side). The list must contain the refractive indexes as `index` and the thicknesses of the layers as `thickness`. You may also optionally include a number of repetitions of the stack under `repetitions`.

For Example:

``` r
mystack <- list(index = c(1, 1.5, 1, 1.5 + 0.5i),
                thickness = c(100e-9, 50e-9, 20e-9, 100e-9), 
                repetitions = 2)
```

Example
-------

As a quick example, here is how you'd calculate and plot the reflection from a thin silver film through a prism. This is a standard experiment in our lab which results in a reflectivity minima associated with a surface plasmon excitation.

We simply construct a stack with an appropriate refractive index and thickness (45 nm), and calculate the reflection (as a funciton of polar angle, setting the incident medium to glass.

``` r
layers <- list(index = 0.13+4i, 
               thickness = 45e-9)

plasmon <- angle_scan(layers,
                      incident_medium.index = 1.5 + 0i)
```

Plotting the result using `ggplot` shows a critical edge related to the onset of total internal reflection in the prism, and the reflectivity minima resulting from a surface plasmon excitation on the silver/air interface.

``` r
library(tidyverse)

plasmon %>%
  ggplot(aes(x = angle, y = Reflection))+
  geom_line() +
  theme_bw() +
  scale_x_continuous(expand=c(0,0)) +
  xlab(expression(polar~angle~theta~(degree))) +
  ylab("reflection")
```

![](https://i0.wp.com/i.imgur.com/V4zXdjE.png)
