context("String length")

test_that("angle scan function works", {
  
  layers <- list(index=c(0.13+4i),thickness=c(45e-9),repetitions= 1)
  R_plasmon <- angle_scan(incident_medium.index = 1.5+0i,exit_medium.index = 1+0i,layers = layers)
  
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