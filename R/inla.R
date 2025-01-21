sample_model <- function(inla_mod, inla_df, col) {
  df <- inla_df %>%
    filter(across(all_of(col), ~!is.na(.x)))

  samples <- inla.posterior.sample(1000, inla_mod)
  contents = inla_mod$misc$configs$contents
  effect = "Predictor"
  id.effect = which(contents$tag==effect)
  ind.effect = contents$start[id.effect]-1 + (1:contents$length[id.effect])

  ind.effect <- 1:(nrow(inla_df) - nrow(df))

  samples.effect = lapply(samples, function(x) x$latent[ind.effect])

  samples <- matrix(sapply(samples.effect, cbind), ncol=1000)
  mean <- rowMeans(samples)
  qtls <- apply(samples, 1, quantile, c(0.025, 0.5, 0.975))

  ident <- inla_df[ind.effect, ]
  samples <- ident %>% cbind(samples)

  art <- ident %>%
    mutate(
      mean = mean,
      lower = qtls[1,],
      upper = qtls[3,]
    )

}

 
 clean_inla <- function(inla_mod) {

  fixed <- inla_mod$summary.fixed %>%
    rownames_to_column("var") %>%
    select(var, mean, lower = `0.025quant`, upper = `0.975quant`) %>%
    mutate(across(-var, exp)) %>%
    mutate(txt = sprintf("%0.2f (%0.2f, %0.2f)", mean, lower, upper))
  out <- list()
  out$fixed <- fixed
  if(length(inla_mod$summary.random)) {
    random <- inla_mod$summary.random %>%
      lapply(mutate, ID = as.character(ID)) %>%
      bind_rows(.id = "var") %>%
      select(var, ID, mean, lower = `0.025quant`, upper = `0.975quant`) %>%
      mutate(across(-c(var, ID), exp)) %>%
      mutate(txt = sprintf("%0.2f (%0.2f, %0.2f)", mean, lower, upper))
    hyper <- inla_mod$internal.summary.hyperpar %>%
      rownames_to_column("var") %>%
      select(var, mean, lower = `0.025quant`, upper = `0.975quant`) %>%
      mutate(across(-var, exp)) %>%
      mutate(txt = sprintf("%0.2f (%0.2f, %0.2f)", mean, lower, upper))
    out$random <- random
    out$hyper <- hyper
  }

  out

}
