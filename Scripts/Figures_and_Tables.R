  #'  ----------------------------
  #'  Nonbreeder turnover figures
  #'  ICFWRU
  #'  Ausband & Bassing
  #'  August 2023
  #'  ----------------------------
  #'  Plot results of nonbreeder turnover analyses
  #'  ----------------------------
  
  #'  Load libraries
  library(sf)
  library(terra)
  library(ggplot2)
  library(patchwork)
  #devtools::install_github("karthik/wesanderson")
  library(wesanderson)
  library(tidyverse)
  
  #'  Read in model outputs
  load("./Outputs/Model_outputs_for_SBB.RData")
  
  #'  Load spatial data
  gmu <- st_read("./Shapefiles/IDFG_Game_Management_Units/Game_Management_Units.shp")
  sa_gmu <- gmu[gmu$NAME == "4" | gmu$NAME == "28" | gmu$NAME == "33" | gmu$NAME == "34" | gmu$NAME == "35",] 
  usa <- st_read("./Shapefiles/tl_2012_us_state/tl_2012_us_state.shp")
  id <- usa[usa$NAME == "Idaho",] 
  gmu_wgs84 <- st_transform(gmu, "+proj=longlat +datum=WGS84 +no_defs")
  sa_gmu_wgs84 <- st_transform(sa_gmu, "+proj=longlat +datum=WGS84 +no_defs")
  id_wgs84 <- st_transform(id, "+proj=longlat +datum=WGS84 +no_defs")
  
  #'  Define projections
  gmu_proj <- crs(sa_gmu) 
  id_proj <- crs(id)
  wgs84 <- crs(sa_gmu_wgs84) 
  
  #'  Select color palette
  names(wes_palettes)
  pal1 <- wes_palette("AsteroidCity1", 3, type = "discrete") 
  # pal2 <- wes_palette("AsteroidCity2", 3, type = "discrete")
  # pal3 <- wes_palette("AsteroidCity3", 4, type = "discrete")
  # pal4 <- c("#FBA72A", "#CD7A5C", "#5785C1")
  
  
  #'  -------------------------------------
  ####  Format data for tables & plotting  ####
  #'  -------------------------------------
  #'  Function to extract model coefficients and standard errors
  mod_coeffs <- function(mod, mod_name, nonbreeder) {
    #'  Print model summary
    print(summary(mod))
    
    #'  Extract coefficients, standard errors, p-values, and overall R^2 of model
    betas <- round(mod$coefficients, 2)
    se <- round(sqrt(diag(vcov(mod))), 2)
    ci <- round(confint(mod, level = 0.95), 2)
    pval <- round(summary(mod)$coefficients[,4], 3) 
    r2 <- round(summary(mod)$r.squared, 3)
    
    #'  Create single data frame
    mod_df <- as.data.frame(cbind(betas, se, ci, pval, r2)) %>%
      mutate(model = mod_name,
             response = nonbreeder, 
             predictor = rownames(.), 
             #'  Update names of predictors
             predictor = ifelse(predictor == "(Intercept)", "Intercept", predictor),
             predictor = ifelse(predictor == "bf_status[T.2_inherit]", "BF inherits", predictor),
             predictor = ifelse(predictor == "bf_status[T.3_adopted]", "BF adopted", predictor),
             predictor = ifelse(predictor == "bm_status[T.2_inherit]", "BM inherits", predictor),
             predictor = ifelse(predictor == "bm_status[T.3_adopted]", "BM adopted", predictor),
             predictor = ifelse(predictor == "became_brdr", "Became breeder", predictor),
             predictor = ifelse(predictor == "Z.breeders", "Multiple breeders", predictor), # should this be number of breeders?
             predictor = ifelse(predictor == "Z.density", "Density", predictor),
             predictor = ifelse(predictor == "Z.harvest_rate", "Harvest rate", predictor),
             predictor = ifelse(predictor == "Z.X2yr_helpers_to_pup_r_t", "Relatedness to pups", predictor)) %>%
      #'  Reorganize for a cleaner results table
      relocate(model, .before = betas) %>%
      relocate(response, .after = model) %>%
      relocate(predictor, .after = response)
    row.names(mod_df) <- NULL
    
    return(mod_df)
  }
  NBF_BF <- mod_coeffs(mod_list[[1]], nonbreeder = "NB Female", mod_name = "NBF_BF")
  NBM_BF <- mod_coeffs(mod_list[[2]], nonbreeder = "NB male", mod_name = "NBM_BF")
  NBF_BM <- mod_coeffs(mod_list[[3]], nonbreeder = "NB Female", mod_name = "NBF_BM")
  NBM_BM <- mod_coeffs(mod_list[[4]], nonbreeder = "NB male", mod_name = "NBM_BM")
  
  #'  Bind result tables (note the order change here! NBF, NBF, NBM, NBM)
  results_df <- rbind(NBF_BF, NBF_BM, NBM_BF, NBM_BM) 
  
  #'  Use delta method to approximate standard errors of additive categorical effects
  #'  Necessary if want to plot comparison of nonbreeder responses to different breeder status
  #'  https://stats.stackexchange.com/questions/446676/measuring-standard-error-of-two-or-more-coefficients-combined
  delta <- function(mod) {
    #'  Vector of covariate values applied to each beta (intercept and inherit or adopt = 1),
    #'  while holding all other variables at their mean (holding became_brdr at 0) 
    x_inherit <- c(1, 1, 0, 0, 0, 0, 0, 0)
    x_adopt <- c(1, 0, 1, 0, 0, 0, 0, 0)
    
    #'  Calculate standard error for a linear combination of random variables where
    #'  vcov(mod) is the variance covariance matrix of the model coefficients (V)
    #'  SE = sqrt(x'Vx) which returns same result as deltamethod()
    delta_se_inherit <- c(sqrt(t(x_inherit) %*% vcov(mod) %*% x_inherit))
    delta_se_adopt <- c(sqrt(t(x_adopt) %*% vcov(mod) %*% x_adopt))
    
    #'  Snag original standard error of the model's intercept
    intercept_se = sqrt(diag(vcov(mod)))[1]
    #'  Bind intercept and adjusted standard errors together
    updated_se = c(intercept_se, delta_se_inherit, delta_se_adopt)
    print(updated_se)
    
    return(updated_se)
  }
  NBF_BF_delta <- delta(mod_list[[1]])
  NBM_BF_delta <- delta(mod_list[[2]])
  NBF_BM_delta <- delta(mod_list[[3]])
  NBM_BM_delta <- delta(mod_list[[4]])
  
  #'  Bind into single vector (note the order change here!)
  delta_SEs <- c(NBF_BF_delta, NBF_BM_delta, NBM_BF_delta, NBM_BM_delta) 
  delta_SEs <- as.data.frame(delta_SEs)

  #'  Focus on just breeder status effects
  breeder_effect <- results_df %>%
    filter(predictor == "Intercept" | predictor == "BF inherits" | predictor == "BF adopted" | 
             predictor == "BM inherits" | predictor == "BM adopted") %>%
    dplyr::select(-r2) %>%
    bind_cols(delta_SEs) %>%
    mutate(delta_SEs = round(delta_SEs, 2),
           #'  Change label for each intercept
           predictor = ifelse(model == "NBF_BF" & predictor == "Intercept", "No change in BF", predictor),
           predictor = ifelse(model == "NBF_BM" & predictor == "Intercept", "No change in BM", predictor),
           predictor = ifelse(model == "NBM_BF" & predictor == "Intercept", "No change in BF", predictor),
           predictor = ifelse(model == "NBM_BM" & predictor == "Intercept", "No change in BM", predictor),
           predictor = factor(predictor, levels = c("No change in BF", "BF inherits", "BF adopted",
                                                    "No change in BM", "BM inherits", "BM adopted")),
           #'  Calculate effect of breeder inheriting position (intercept + inherit)
           adj_betas = ifelse(predictor == "BF inherits", betas + lag(betas), betas),
           adj_betas = ifelse(predictor == "BM inherits", betas + lag(betas), adj_betas),
           #'  Calculate effect of breeding being adopted (intercept + adopt)
           adj_betas = ifelse(predictor == "BF adopted", betas + lag(betas, n = 2), adj_betas),
           adj_betas = ifelse(predictor == "BM adopted", betas + lag(betas, n = 2), adj_betas),
           #'  Calculate 95% confidence intervals using updated betas and SEs
           adj_lci = adj_betas - (delta_SEs * 1.96),
           adj_uci = adj_betas + (delta_SEs * 1.96)) %>%
    relocate(adj_betas, .after = pval) %>%
    relocate(delta_SEs, .after = adj_betas) %>%
    relocate(adj_lci, .before = delta_SEs) %>%
    relocate(adj_uci, .after = adj_lci)
  
  #'  ----------------
  ####  Plot results  ####
  #'  ----------------
  #'  Plot effect of breeder status on change in number of nonbreeders
  ggplot(breeder_effect, aes(x = predictor, y = adj_betas, group = model, color = predictor)) +
    geom_errorbar(aes(ymin = adj_lci, ymax = adj_uci, color = predictor), width = 0, position = position_dodge(width = 0.4)) +
    #scale_fill_manual(values = pal1) +
    geom_point(stat = "identity", aes(col = predictor), size = 2.5, position = position_dodge(width = 0.4)) +
    #scale_color_manual(value = pal1) +
    facet_wrap(~model, scales = "free_x")
    
  
  
  
  
  