DEoptim <- function (fn, lower, upper, control = DEoptim.control(), ..., 
                     fnMap = NULL) 
{
  maxCore <- 36
  if (length(lower) != length(upper)) 
    stop("'lower' and 'upper' are not of same length")
  if (!is.vector(lower)) 
    lower <- as.vector(lower)
  if (!is.vector(upper)) 
    upper <- as.vector(upper)
  if (any(lower > upper)) 
    stop("'lower' > 'upper'")
  if (any(lower == "Inf")) 
    warning("you set a component of 'lower' to 'Inf'. May imply 'NaN' results", 
            immediate. = TRUE)
  if (any(lower == "-Inf")) 
    warning("you set a component of 'lower' to '-Inf'. May imply 'NaN' results", 
            immediate. = TRUE)
  if (any(upper == "Inf")) 
    warning("you set a component of 'upper' to 'Inf'. May imply 'NaN' results", 
            immediate. = TRUE)
  if (any(upper == "-Inf")) 
    warning("you set a component of 'upper' to '-Inf'. May imply 'NaN' results", 
            immediate. = TRUE)
  if (!is.null(names(lower))) 
    nam <- names(lower)
  else if (!is.null(names(upper)) & is.null(names(lower))) 
    nam <- names(upper)
  else nam <- paste("par", 1:length(lower), sep = "")
  ctrl <- do.call(DEoptim.control, as.list(control))
  ctrl$npar <- length(lower)
  if (is.na(ctrl$NP)) 
    ctrl$NP <- 10 * length(lower)
  if (ctrl$NP < 4) {
    warning("'NP' < 4; set to default value 10*length(lower)\n", 
            immediate. = TRUE)
    ctrl$NP <- 10 * length(lower)
  }
  if (ctrl$NP < 10 * length(lower)) 
    warning("For many problems it is best to set 'NP' (in 'control') to be at least ten times the length of the parameter vector. \n", 
            immediate. = TRUE)
  if (!is.null(ctrl$initialpop)) {
    ctrl$specinitialpop <- TRUE
    if (!identical(as.numeric(dim(ctrl$initialpop)), as.numeric(c(ctrl$NP, 
                                                                  ctrl$npar)))) 
      stop("Initial population is not a matrix with dim. NP x length(upper).")
  }
  else {
    ctrl$specinitialpop <- FALSE
    ctrl$initialpop <- 0
  }
  ctrl$trace <- as.numeric(ctrl$trace)
  ctrl$specinitialpop <- as.numeric(ctrl$specinitialpop)
  ctrl$initialpop <- as.numeric(ctrl$initialpop)
  if (!is.null(ctrl$cluster)) {
    if (!inherits(ctrl$cluster, "cluster")) 
      stop("cluster is not a 'cluster' class object")
    parallel::clusterExport(cl, ctrl$parVar)
    fnPop <- function(params, ...) {
      parallel::parApply(cl = ctrl$cluster, params, 1, 
                         fn, ...)
    }
  }
  else if (ctrl$parallelType == 2) {
    if (!foreach::getDoParRegistered()) {
      foreach::registerDoSEQ()
    }
    args <- ctrl$foreachArgs
    fnPop <- function(params, ...) {
      my_chunksize <- ceiling(NROW(params)/foreach::getDoParWorkers())
      my_iter <- iterators::iter(params, by = "row", chunksize = my_chunksize)
      args$i <- my_iter
      args$.combine <- c
      if (!is.null(args$.export)) 
        args$.export = c(args$.export, "fn")
      else args$.export = "fn"
      if (is.null(args$.errorhandling)) 
        args$.errorhandling = c("stop", "remove", "pass")
      if (is.null(args$.verbose)) 
        args$.verbose = FALSE
      if (is.null(args$.inorder)) 
        args$.inorder = TRUE
      if (is.null(args$.multicombine)) 
        args$.multicombine = FALSE
      foreach::"%dopar%"(do.call(foreach::foreach, args), 
                         apply(i, 1, fn, ...))
    }
  }
  else if (ctrl$parallelType == 1) {
    cl <- parallel::makeCluster(min(parallel::detectCores(),maxCore))
    packFn <- function(packages) {
      for (i in packages) library(i, character.only = TRUE)
    }
    parallel::clusterCall(cl, packFn, ctrl$packages)
    parallel::clusterExport(cl, ctrl$parVar)
    fnPop <- function(params, ...) {
      parallel::parApply(cl = cl, params, 1, fn, ...)
    }
  }
  else {
    fnPop <- function(params, ...) {
      apply(params, 1, fn, ...)
    }
  }
  if (is.null(fnMap)) {
    fnMapC <- function(params, ...) params
  }
  else {
    fnMapC <- function(params, ...) {
      mappedPop <- t(apply(params, 1, fnMap))
      if (all(dim(mappedPop) != dim(params))) 
        stop("mapping function did not return an object with ", 
             "dim NP x length(upper).")
      dups <- duplicated(mappedPop)
      np <- NCOL(mappedPop)
      tries <- 0
      while (tries < 5 && any(dups)) {
        nd <- sum(dups)
        newPop <- matrix(runif(nd * np), ncol = np)
        newPop <- rep(lower, each = nd) + newPop * rep(upper - 
                                                         lower, each = nd)
        mappedPop[dups, ] <- t(apply(newPop, 1, fnMap))
        dups <- duplicated(mappedPop)
        tries <- tries + 1
      }
      if (tries == 5) 
        warning("Could not remove ", sum(dups), " duplicates from the mapped ", 
                "population in 5 tries. Evaluating population with duplicates.", 
                call. = FALSE, immediate. = TRUE)
      storage.mode(mappedPop) <- "double"
      mappedPop
    }
  }
  outC <- .Call("DEoptimC", lower, upper, fnPop, ctrl, new.env(), 
                fnMapC, PACKAGE = "DEoptim")
  if (ctrl$parallelType == 1) 
    parallel::stopCluster(cl)
  if (length(outC$storepop) > 0) {
    nstorepop <- floor((outC$iter - ctrl$storepopfrom)/ctrl$storepopfreq)
    storepop <- list()
    cnt <- 1
    for (i in 1:nstorepop) {
      idx <- cnt:((cnt - 1) + (ctrl$NP * ctrl$npar))
      storepop[[i]] <- matrix(outC$storepop[idx], nrow = ctrl$NP, 
                              ncol = ctrl$npar, byrow = TRUE)
      cnt <- cnt + (ctrl$NP * ctrl$npar)
      dimnames(storepop[[i]]) <- list(1:ctrl$NP, nam)
    }
  }
  else {
    storepop = NULL
  }
  names(outC$bestmem) <- nam
  iter <- max(1, as.numeric(outC$iter))
  names(lower) <- names(upper) <- nam
  bestmemit <- matrix(outC$bestmemit[1:(iter * ctrl$npar)], 
                      nrow = iter, ncol = ctrl$npar, byrow = TRUE)
  dimnames(bestmemit) <- list(1:iter, nam)
  storepop <- as.list(storepop)
  outR <- list(optim = list(bestmem = outC$bestmem, bestval = outC$bestval, 
                            nfeval = outC$nfeval, iter = outC$iter), member = list(lower = lower, 
                                                                                   upper = upper, bestmemit = bestmemit, bestvalit = outC$bestvalit, 
                                                                                   pop = t(outC$pop), storepop = storepop))
  attr(outR, "class") <- "DEoptim"
  return(outR)
}
