
getEstimateGlobal <- function(depvar,expvar, df=df_global_corrected){
  
  # set formula for base children
  basemodel <-
    formula(paste0(depvar,"~ ",expvar,"| 0 | 0 | union_code"))
  
  basemodel_fe <-
    formula(paste0(depvar,"~ time_bi *",expvar,"| 0 | 0 | union_code"))
  
  basemodel_control <-
    formula(paste0(depvar,"~ time_bi *",expvar," + hhm_sex + hhm_age+ hh_size +  marital_status_hhm+ literacy_hhm+ education_high +  hh_head_religion + hh_ethnic_group + nbr_female+ nbr_underfive+ nbr_yngchldrn_5_10+ nbr_teenager_10_20+ nbr_adults_20_65+ nbr_female_underfive+ nbr_female_yngchldrn_5_10+ nbr_female_teenager_10_20+ nbr_female_adults_20_65| 0 | 0 | union_code"))
  
  modbasemodel <- felm(basemodel, data = df)
  modbasemodel_fe <- felm(basemodel_fe, data = df)
  modbasemodel_control <- felm(basemodel_control, data = df)
  
  out <- tibble(
    model = c("basemodel","basemodel_fe","basemodel_control"),
    Results_base = list(modbasemodel,modbasemodel_fe,modbasemodel_control),
    outcome = depvar
  )
  
  out
}


getEstimateCross <- function(depvar,expvar, df=df_global_corrected){
  
  # set formula for base children
  # basemodel <-
  #   formula(paste0(depvar,"~ ",expvar,"*hhm_sex| 0 | 0 | union_code"))
  # 
  basemodel_fe <-
    formula(paste0(depvar,"~ time_bi *",expvar,"*hhm_sex| 0 | 0 | union_code"))
  
  basemodel_control <-
    formula(paste0(depvar,"~ time_bi *",expvar,"*hhm_sex + hhm_age+ hh_size +  marital_status_hhm+ literacy_hhm+ education_high +  hh_head_religion + hh_ethnic_group + nbr_female+ nbr_underfive+ nbr_yngchldrn_5_10+ nbr_teenager_10_20+ nbr_adults_20_65+ nbr_female_underfive+ nbr_female_yngchldrn_5_10+ nbr_female_teenager_10_20+ nbr_female_adults_20_65| 0 | 0 | union_code"))
  
  
  # Bind results from the two models
  temp_res <- bind_rows(
    getEstimatedf(basemodel_fe, depvar,expvar, df=df_global_corrected) %>% mutate(name = "(a) Baseline"),
    getEstimatedf(basemodel_control, depvar,expvar, df=df_global_corrected) %>% mutate(name = "(b) With controls")
  )
  
  # Return the results
  temp_res
}


# Function to estimate coefficients and confidence intervals for a given formula and data
getEstimatedf <- function(formula, depvar,expvar,df, R=1000) {
  
  # Fit the fixed effects model using the formula and input dataframe
  mod <- felm(formula, data = df, keepModel = TRUE, na.action = na.omit)
  
  #browser()
  # Extract the variance-covariance matrix of model coefficients
  vcmod <- vcov(mod)
  
  # Define quantiles for confidence intervals
  alpha <- c(.025, .05, .95, .975)
  
  # Simulate coefficients from a multivariate normal distribution
  modU <- MASS::mvrnorm(R, mu = coef(mod), Sigma = vcmod)
  
  # Create a matrix to select coefficients of interest
  matSelect <- matrix(0, nrow = 3, ncol = length(coef(mod)))
  colnames(matSelect) <- names(coef(mod))
  
  # Define the interaction term and the variable of interest
  crosvar1 <- paste0("time_bi1:",expvar,"1", sep = "")
  crosvar2 <- paste0("time_bi1:",expvar,"1:hhm_sex1", sep = "")
  
  # Set values in the matrix for the interaction term and variable of interest
  matSelect[, crosvar1] <- c(1, 1, 0)
  matSelect[, crosvar2] <- c(0, 1, 1)
  
  # Calculate the simulated coefficients and their confidence intervals
  coefs <- as.numeric(matSelect %*% coef(mod))
  modU_CI <- t(matSelect %*% t(modU))
  
  # Compute confidence intervals for the coefficients
  CI <- t(apply(modU_CI, 2, quantile, probs = alpha, na.rm = TRUE))
  colnames(CI) <- paste0("CI", parse_number(colnames(CI)))
  
  # Extract variables related to the number of events and treatments
  vect_temp_1 <- mod$model %>% pull(expvar)
  
  vect_temp_2 <- mod$model %>% filter(!!as.name(expvar) == "1") %>% pull("hhm_sex")
  #browser()
  # Create a tibble containing results
  out <- tibble(
    expvar = expvar,
    depvar = depvar,
    estimate = c("Female", "Male", "Gap"),
    pe = coefs,
    N = nrow(mod$model),
    Ntype = c(as.numeric(table(vect_temp_1)),nrow(mod$model)),
    Ntype_treat = c(as.numeric(table(vect_temp_2)),nrow(mod$model)),
    r.squared = summary(mod)$r.squared
  )
  
  # Bind confidence intervals to the output
  out <- bind_cols(out, as.tibble(CI))
  
  # Return the result
  out
}