context("Test base functions")

## simulation_generator is correct

test_that("simulation_generator works", {
  rule <- function(vec, ...){
    stats::rnorm(100, mean = vec[1])
  }
  criterion <- function(dat, vec, ...){
    mean(dat)
  }
  len <- 6
  paramMat <- matrix(1:len, nrow = len)

  trials <- 10
  res <- simulation_generator(rule, criterion, paramMat, trials = trials,
                              verbose = F, as_list = F)

  expect_true(is.list(res))
  expect_true(length(res) == len)
  expect_true(all(sapply(res, is.numeric)))
  expect_true(all(sapply(res, length) == trials))
})

test_that("simulation_generator works to return multiple things", {
  rule <- function(vec, ...){
    stats::rnorm(100, mean = vec[1])
  }
  criterion <- function(dat, vec, ...){
    c(mean(dat), stats::sd(dat))
  }
  len <- 6
  paramMat <- matrix(1:len, nrow = len)

  trials <- 10
  res <- simulation_generator(rule, criterion, paramMat, trials = trials,
                              verbose = F, as_list = F)

  expect_true(is.list(res))
  expect_true(length(res) == len)
  expect_true(all(sapply(res, is.numeric)))
  expect_true(all(sapply(res, is.matrix)))
  expect_true(all(sapply(res, ncol) == trials))
})

test_that("simulation_generator works to return multiple things as lists", {
  rule <- function(vec, ...){
    stats::rnorm(100, mean = vec[1])
  }
  criterion <- function(dat, vec, ...){
    c(mean(dat), stats::sd(dat))
  }
  len <- 6
  paramMat <- matrix(1:len, nrow = len)

  trials <- 10
  res <- simulation_generator(rule, criterion, paramMat, trials = trials,
                              verbose = F, as_list = T)

  expect_true(is.list(res))
  expect_true(length(res) == len)
  expect_true(all(sapply(res, is.list)))
  expect_true(all(sapply(res, length) == trials))
  expect_true(all(sapply(res, function(x){sapply(x, length)}) == 2))
})

test_that("simulation_generator works with cores for lists", {
  rule <- function(vec, ...){
    stats::rnorm(100, mean = vec[1])
  }
  criterion <- function(dat, vec, ...){
    c(mean(dat), stats::sd(dat))
  }
  len <- 6
  paramMat <- matrix(1:len, nrow = len)

  trials <- 10
  res1 <- simulation_generator(rule, criterion, paramMat, trials = trials,
                              verbose = F, as_list = T, cores = 2)

  res2 <- simulation_generator(rule, criterion, paramMat, trials = trials,
                               verbose = F, as_list = T, cores = NA)

  for(i in 1:len){
    for(j in 1:trials){
      expect_true(all(res1[[i]][[j]] == res2[[i]][[j]]))
    }
  }
})

test_that("simulation_generator works with cores for matrices", {
  rule <- function(vec, ...){
    stats::rnorm(100, mean = vec[1])
  }
  criterion <- function(dat, vec, ...){
    c(mean(dat), stats::sd(dat))
  }
  len <- 6
  paramMat <- matrix(1:len, nrow = len)

  trials <- 10
  res1 <- simulation_generator(rule, criterion, paramMat, trials = trials,
                               verbose = F, as_list = F, cores = 2)

  res2 <- simulation_generator(rule, criterion, paramMat, trials = trials,
                               verbose = F, as_list = F, cores = NA)

  for(i in 1:len){
    expect_true(all(res1[[i]] == res2[[i]]))
  }
})
