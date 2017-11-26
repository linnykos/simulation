simulation_generator <- function(rule, paramMat, criterion, trials,
 cores = NA, as_list = T){

  if(!is.na(cores)) registerDoMC(cores = cores)

  res <- lapply(1:nrow(paramMat), function(x){
    cat(paste0("\nRow ", x, " started!\n"))

    fun <- function(y){
      if(trials > 10 && y %% floor(trials/10) == 0) cat("*")
      set.seed(y)
      criterion(rule(paramMat[x,]), paramMat[x,])
    }
    if(is.na(cores)){
      if(as_list){
        vec <- lapply(1:trials, fun)
      } else {
        vec <- sapply(1:trials, fun)
      }
    } else {
      vec <- foreach(trial = 1:trials) %dopar% fun(trial)
      if(!as_list) vec <- .adjustFormat(vec)
    }
  })

  names(res) <- sapply(1:nrow(paramMat), function(x){
    paste0(paramMat[x,], collapse = "-")})

  res
}

.adjustFormat <- function(lis){
  len <- sapply(lis, length)
  if(length(unique(len)) != 1) return(lis)

  ncol <- length(unique(len))
  if(ncol == 1) return(as.numeric(unlist(lis)))

  vec <- as.numeric(unlist(lis))
  matrix(vec, ncol = ncol, byrow = T)
}
