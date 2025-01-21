unload_and_compile <- function(mod, tmb_int) {
  
  tmb_unload <- function(name) {
    ldll <- getLoadedDLLs()
    idx  <- grep(name, names(ldll))
    for (i in seq_along(idx))
      dyn.unload(unlist(ldll[[idx[i]]])$path)
    cat('Unload ', length(idx), "loaded versions.\n")
  }
  
  
  tmb_unload(mod)
  lapply(list.files("src/", pattern = "\\.o|\\.so", full.names = T), file.remove)
  
  
  TMB::compile(paste0("src/", mod, ".cpp"), flags = "-w")               # Compile the C++ file
  dyn.load(TMB::dynlib(paste0("src/", mod)))
  
  f <- parallel::mcparallel({TMB::MakeADFun(data = tmb_int$data,
                                            parameters = tmb_int$par,
                                            # DLL = "2023_11_03_all_levels_survey",
                                            DLL = mod,
                                            silent=0,
                                            checkParameterOrder=FALSE)
  })
  # #
  if(is.null(parallel::mccollect(f)[[1]])) {
    stop("TMB model is invalid. This is most likely an indexing error e.g. iterating over dimensions in an array that do not exist. Check mf model object")
  }
  
}

fit_model <- function(tmb, mod, sdreport = T) {
  
  obj <-  TMB::MakeADFun(data = tmb$data,
                         parameters = tmb$par,
                         DLL = mod,
                         random = tmb$random,
                         hessian = FALSE)
  
  
  check <- obj$report()
  obj$env$tracepar <- TRUE
  
  f <- stats::nlminb(obj$par, obj$fn, obj$gr)
  f$par.fixed <- f$par
  f$par.full <- obj$env$last.par
  
  fit <- c(f, obj = list(obj))
  
  if(sdreport) {
    fit$sdreport <- sdreport(fit$obj, fit$par)
    
    sd_report <- fit$sdreport
    sd_report <- summary(sd_report, "all")
    
    sd_report <- data.frame(sd_report, "hyper" = rownames(sd_report))
    
    fit$sdreport_clean <- sd_report
    
    fit
  }
  
}
