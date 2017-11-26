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
