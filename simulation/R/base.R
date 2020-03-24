#' Simulation setup
#'
#' This function applies \code{rule} (which takes in parameter settings and outputs
#' a synthetic dataset) and \code{criterion} (which takes a synthetic dataset and outputs
#' the results from estimators) to all the rows of \code{paramMat} (which is a matrix
#' that contains different parameter settings for each row). The distinction between
#' \code{rule} and \code{criterion} is only made by the user, as the user can design
#' exactly the same simulation that uses one but not the other.
#'
#' The input to \code{rule} must be a vector (a row from \code{paramMat}), while
#' the input to \code{criterion} must be first the output of \code{rule} and second
#' a vector (the same row from \code{paramMat}). Both these functions is allowed
#' to output lists.
#'
#' The output to \code{simulation_generator} is a list, one element for each
#' row of \code{paramMat}. Each element of the list is typically a list or
#' a matrix. This depends on how the user set up what \code{criterion} returns.
#'
#' The function has a \code{tryCatch} call, so if an error happens, the result for that
#' trial and row of \code{paramMat} will be an \code{NA}.
#'
#' The remaining inputs for \code{simulation_generator} are cosmetic.
#'
#' \code{as_list} is a boolean, where if \code{TRUE},
#' mandates that the \code{trials} results for each row of \code{paramMat}
#' is returned as a list. If \code{FALSE}, then the results are returned as a matrix.
#'
#' \code{filepath} is a filepath to the temporary save location. If set to not \code{NA},
#' \code{simulation_generator} will save the results of every row of \code{paramMat} there
#' as it runs the simulations.
#'
#' @param rule function
#' @param paramMat matrix
#' @param criterion function
#' @param trials number of trials for each row
#' @param cores number of cores
#' @param as_list boolean
#' @param filepath string
#' @param verbose boolean
#'
#' @return list
#' @export
simulation_generator <- function(rule, criterion, paramMat, trials = 10,
 cores = NA, as_list = T, filepath = NA, verbose = T){
  stopifnot(is.matrix(paramMat))

  if(!is.na(cores)) doMC::registerDoMC(cores = cores)
  if(length(trials) == 1 & nrow(paramMat) > 1){
    trials_vec <- rep(trials, nrow(paramMat))
  } else {
    stopifnot(length(trials) == nrow(paramMat))
    trials_vec <- trials
  }

  # function for each trial and row of paramMat
  fun <- function(y){
    if(verbose && trials_vec[x] > 10 && y %% floor(trials_vec[x]/10) == 0) cat("*")
    set.seed(y)
    tryCatch({
      dat <- rule(paramMat[x,])
      criterion(dat, paramMat[x,], y)
    }, error = function(e){
      NA
    })
  }

  # run the simulations
  res <- vector("list", nrow(paramMat))
  for(x in 1:nrow(paramMat)){
    if(verbose) cat(paste0("\n", Sys.time(), ": Row ", x, " started!\n"))

    if(is.na(cores)){
      if(as_list){
        vec <- lapply(1:trials_vec[x], fun)
      } else {
        vec <- sapply(1:trials_vec[x], fun)
      }
    } else {
      i <- 0 #debugging reasons
      vec <- foreach::"%dopar%"(foreach::foreach(i = 1:trials_vec[x]), fun(i))
      if(!as_list) vec <- .adjustFormat(vec)
    }

    res[[x]] <- vec
    if(!is.na(filepath)) save(res, file = filepath)
  }

  names(res) <- sapply(1:nrow(paramMat), function(x){
    paste0(paramMat[x,], collapse = "-")})

  res
}

.adjustFormat <- function(lis){
  len <- sapply(lis, length)
  if(length(unique(len)) != 1) return(lis)

  nrow <- unique(len)
  if(nrow == 1) return(as.numeric(unlist(lis)))

  vec <- as.numeric(unlist(lis))
  matrix(vec, nrow = nrow)
}
