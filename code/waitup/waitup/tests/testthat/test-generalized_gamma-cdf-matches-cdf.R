library(waitup)
library(flexsurv)
context("generalized Gamma cdf implementations match")

test_that("generalized Gamma density matches flexsurv gamma density", {
  f <- generalized_gamma_cdf_1
  g <- generalized_gamma_cdf_3
  expect_f_matches_g(f=f, g=g, domain=c(0,100), 
    arg_bounds=list(alpha=c(1.5, 5), beta=c(1,5), nu=c(1.5, 5)), 
    n_arg_points=20)
})

test_that("generalized Gamma exp(log density) matches flexsurv gamma density", {
  f <- generalized_gamma_lcdf_1
  g <- generalized_gamma_lcdf_3
  expect_f_matches_g(f=f, g=g, domain=c(0,100), arg_bounds=list(
    alpha=c(1.5, 5), beta=c(1,5), nu=c(1.5, 5)), n_arg_points=20)
})

test_that("generalized Gamma (Stan) density matches flexsurv gamma density", {
  f <- function(x, ...) sapply(x, generalized_gamma_cdf_1S, ...)
  g <- generalized_gamma_cdf_3
  expect_f_matches_g(f=f, g=g, domain=c(0,100), 
    arg_bounds=list(alpha=c(1.5, 5), beta=c(1,5), nu=c(1.5, 5)), 
    n_arg_points=20)
})

test_that("generalized Gamma (Stan) exp(log density) matches flexsurv gamma density", {
  f <- function(x, ...) sapply(x, generalized_gamma_lcdf_1S, ...)
  g <- generalized_gamma_lcdf_3
  expect_f_matches_g(f=f, g=g, domain=c(0,100), arg_bounds=list(
    alpha=c(1.5, 5), beta=c(1,5), nu=c(1.5, 5)), n_arg_points=20)
})

test_that("generalized Gamma (Lawless 1992) density matches flexsurv generalized gamma density", {
  f <- generalized_gamma_cdf_2
  g <- generalized_gamma_cdf_4
  expect_f_matches_g(f=f, g=g, domain=c(0,200), 
    arg_bounds=list(k=c(2, 2), mu=c(-3,3), sigma=c(1,3)), 
    n_arg_points=3)
})

test_that("generalized Gamma (Lawless 1992) exp(log density) matches flexsurv generalized gamma density", {
  f <- generalized_gamma_lcdf_2
  g <- generalized_gamma_lcdf_4
  expect_f_matches_g(f=f, g=g, domain=c(0,200), arg_bounds=list(
    k=c(2, 2), mu=c(-3, 3), sigma=c(1,3)), n_arg_points=3)
})

test_that("generalized Gamma (Stan, Lawless 1992) density matches flexsurv generalized gamma density", {
  f <- function(x, ...) sapply(x, generalized_gamma_cdf_2S, ...)
  g <- generalized_gamma_cdf_4
  expect_f_matches_g(f=f, g=g, domain=c(0,200), 
    arg_bounds=list(k=c(2, 2), mu=c(-3,3), sigma=c(1,3)), 
    n_arg_points=3)
})

test_that("generalized Gamma (Stan, Lawless 1992) exp(log density) matches flexsurv generalized gamma density", {
  f <- function(x, ...) sapply(x, generalized_gamma_lcdf_2S, ...)
  g <- generalized_gamma_lcdf_4
  expect_f_matches_g(f=f, g=g, domain=c(0,200), arg_bounds=list(
    k=c(2, 2), mu=c(-3, 3), sigma=c(1,3)), n_arg_points=3)
})



