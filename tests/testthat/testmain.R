context("main functions")

test_that("angle scan function works", {
  
  layers <- list(index=c(0.13+4i),thickness=c(45e-9),repetitions= 1)
  R_plasmon <- angle_scan(incident_medium.index = 1.5+0i,exit_medium.index = 1+0i,layers = layers)
  
  expect_equal(round(R_plasmon[1,3],2), 0.92)  
  })

test_that("angle scan function works for s-polarisation", {
  
  layers <- list(index=c(0.13+4i),thickness=c(45e-9),repetitions= 1)
  R_plasmon <- angle_scan(incident_medium.index = 1.5+0i,exit_medium.index = 1+0i,layers = layers, polarisation = "s")
  
  expect_equal(round(R_plasmon[1,3],2), 0.92)  
})

test_that("wavelength scan function works", {
  
  layers <- list(index=c(0.13+4i),thickness=c(45e-9),repetitions= 1)
  R_plasmon <- wavelength_scan(incident_medium.index = 1.5+0i,exit_medium.index = 1+0i,layers = layers)
  
  expect_equal(round(R_plasmon[1,3],2), 0.96)  
})

test_that("wavelength scan function works for s polarisation", {
  
  layers <- list(index=c(0.13+4i),thickness=c(45e-9),repetitions= 1)
  R_plasmon <- wavelength_scan(incident_medium.index = 1.5+0i,exit_medium.index = 1+0i,layers = layers, polarisation="s")
  
  expect_equal(round(R_plasmon[1,3],2), 0.96)  
})

test_that("dispersion scan function works", {
  
  layers <- list(index=c(0.13+4i),thickness=c(45e-9),repetitions= 1)
  R_plasmon <- dispersion_scan(incident_medium.index = 1.5+0i,exit_medium.index = 1+0i,layers = layers)
  
  expect_equal(round(R_plasmon[1,3],2), 0.96)  
})

test_that("dispersion scan function works without progress bar", {
  
  layers <- list(index=c(0.13+4i),thickness=c(45e-9),repetitions= 1)
  R_plasmon <- dispersion_scan(incident_medium.index = 1.5+0i,exit_medium.index = 1+0i,layers = layers, show.progress = FALSE)
  
  expect_equal(round(R_plasmon[1,3],2), 0.96)  
})

test_that("dispersion scan function works for s polarisation", {
  
  layers <- list(index=c(0.13+4i),thickness=c(45e-9),repetitions= 1)
  R_plasmon <- dispersion_scan(incident_medium.index = 1.5+0i,exit_medium.index = 1+0i,layers = layers, polarisation = "s")
  
  expect_equal(round(R_plasmon[1,3],2), 0.96)  
})

test_that("utility functions for index to epsilon work", {
  expect_equal(round(n(18,1) + k(18,1)*1i,2), 
               round(sqrt(epsilon.re(4.244276, 0.117806) + 1i*epsilon.im(4.244276, 0.117806)),2))
})

test_that("utility functions for rindex to  n and k", {
  expect_equal(n(18,1) + k(18,1)*1i, rindex(18,1))
})

test_that("A warning appears when using pi/2 max radians", {
  expect_warning(check_for_radians(pi/2))
})

test_that("normal completion when not using radians", {
  expect_error(check_for_radians(0), NA)
})

test_that("dispersion scan can accept dispersive materials", {
  fake_material <- function(wavelength){
    return(wavelength*1e8+0.1i)
  }
  
  stack <- list(index = c(1.33, fake_material, 1.2+0.1), 
                thickness = c(40e-9, 100e-9, 20e-9))
  result <- dispersion_scan(stack)
  
  expect_equal(round(result$Reflection[10],3), 0.458)
})


test_that("all top-level functions agree",{
  stack <- list(thickness = c(40e-9), index=c(1+0.1i), repetitions = 1)
  a <- angle_scan(stack, angles = 0, wavelength = 600e-9) 
  w <- wavelength_scan(stack, angle = 0, wavelengths = 600e-9) 
  d <- dispersion_scan(stack, angles = 0, wavelengths = 600e-9)
  
  expect_identical(a,w,d)
})

test_that("a missing repetition list item is defaulted to 1", {
  stack <- list(thickness = c(40e-9), index=c(1+0.1i))
  expect_warning(angle_scan(stack))
})

test_that("different length thicknesses/indexes error" ,{
  stack <- list(thickness = 1:2, index = 1)
  expect_error(wavelength_scan(stack))
  })