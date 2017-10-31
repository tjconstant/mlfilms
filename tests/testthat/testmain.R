context("String length")

test_that("angle scan function works", {
  
  layers <- list(index=c(0.13+4i),thickness=c(45e-9),repetitions= 1)
  R_plasmon <- angle_scan(incident_medium.index = 1.5+0i,exit_medium.index = 1+0i,layers = layers)
  
  expect_equal(round(R_plasmon[1,2],2), 0.92)  
  })

test_that("angle scan function works for s-polarisation", {
  
  layers <- list(index=c(0.13+4i),thickness=c(45e-9),repetitions= 1)
  R_plasmon <- angle_scan(incident_medium.index = 1.5+0i,exit_medium.index = 1+0i,layers = layers, polarisation = "s")
  
  expect_equal(round(R_plasmon[1,2],2), 0.92)  
})

test_that("wavelength scan function works", {
  
  layers <- list(index=c(0.13+4i),thickness=c(45e-9),repetitions= 1)
  R_plasmon <- wavelength_scan(incident_medium.index = 1.5+0i,exit_medium.index = 1+0i,layers = layers)
  
  expect_equal(round(R_plasmon[1,2],2), 0.96)  
})

test_that("dispersion scan function works", {
  
  layers <- list(index=c(0.13+4i),thickness=c(45e-9),repetitions= 1)
  R_plasmon <- dispersion_scan(incident_medium.index = 1.5+0i,exit_medium.index = 1+0i,layers = layers)
  
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