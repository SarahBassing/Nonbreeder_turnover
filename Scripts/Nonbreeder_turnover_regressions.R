  #'  -------------------------------
  #'  Nonbreeder turnover regression
  #'  ICFWRU
  #'  Ausband & Bassing
  #'  August 2023
  #'  -------------------------------
  #'  Models to estimate the change in number on nonbreeders in a group in year t+1 
  #'  in response to breeder status, harvest rate, and other group/population dynamics 
  #'  in year t. Perform residual checks.
  #'  -------------------------------
  
  #'  Libraries
  library(tidyverse)
  
  #'  Load data
  dat <- read.csv("./Data/helper_data.csv")
  
  #'  Format data for regression analyses
  NB_status <- dat %>%
    transmute(change_in_nbfs_2_plus = change_in_nbfs_2_plus,
              change_in_nbms_2_plus = change_in_nbms_2_plus,
              bf_status = factor(bf_status, levels = c("1_no change", "2_inherit", "3_adopted")),
              bm_status = factor(bm_status, levels = c("1_no change", "2_inherit", "3_adopted")),
              became_brdr = factor(became_brdr, levels = c("0", "1")),
              Z.breeders = scale(breeders),
              Z.density = scale(density),
              Z.harvest_rate = scale(harvest_rate), 
              Z.X2yr_helpers_to_pup_r_t = scale(X2yr_helpers_to_pup_r_t), 
              year = year,
              pack = pack,
              individ_ID = individ_ID,
              sex = sex)

  
  ####  Fit models  ####
  #'  --------------
  #'  Nonbreeding female response to change in breeding female status
  NBF_BF <- lm(change_in_nbfs_2_plus ~ bf_status + became_brdr + Z.breeders + 
                 Z.density + Z.harvest_rate + Z.X2yr_helpers_to_pup_r_t, data = NB_status)
  summary(NBF_BF)
  
  #'  Nonbreeding male response to change in breeding female status
  NBM_BF <- lm(change_in_nbms_2_plus ~ bf_status + became_brdr + Z.breeders + 
                 Z.density + Z.harvest_rate + Z.X2yr_helpers_to_pup_r_t, data = NB_status)
  summary(NBM_BF)
  
  #'  Nonbreeding female response to change in breeding male status
  NBF_BM <- lm(change_in_nbfs_2_plus ~ bm_status + became_brdr + Z.breeders + 
                 Z.density + Z.harvest_rate + Z.X2yr_helpers_to_pup_r_t, data = NB_status)
  summary(NBF_BM)
  
  #'  Nonbreeding male response to change in breeding male status
  NBM_BM <- lm(change_in_nbms_2_plus ~ bm_status + became_brdr + Z.breeders + 
                 Z.density + Z.harvest_rate + Z.X2yr_helpers_to_pup_r_t, data = NB_status)
  summary(NBM_BM)
  
  mod_list <- list(NBF_BF, NBM_BF, NBF_BM, NBM_BM)
  save(mod_list, file = "./Outputs/helper_turnover_mod_list.R")
  
  ####  Residual checks  ####
  #'  -------------------
  #'  Function to perform residual checks
  residual_checks <- function(mod) {
    #'  Drop NAs from data set
    d <- NB_status[!is.na(NB_status$Z.X2yr_helpers_to_pup_r_t),]
    
    #'  Append predicted values and residuals to data set
    d$predicted <- predict(mod)
    d$residuals <- residuals(mod)
    
    #'  Regression: Plot response against a continuous variable even if coefficient 
    #'  is not significant just to visualize, depicting residual values based on 
    #'  color an size of points
    reg <- ggplot(d, aes(x = Z.harvest_rate, y = change_in_nbfs_2_plus)) +
      geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
      geom_segment(aes(xend = Z.harvest_rate, yend = predicted), alpha = 0.2) +
      geom_point(aes(color = abs(residuals), size = abs(residuals))) +
      scale_color_continuous(low = "green", high = "red") +
      guides(color = "none", size = "none") +
      theme_bw()
    plot(reg)
    
    #'  Plot residuals against fitted values
    #'  Any patterns that suggest data are not linear and a transformation is needed?
    plot(mod, which = 1, col = c("blue"))
    
    #'  QQ Plot
    plot(mod, which = 2, col = c("red"))
    
    #'  Scale - do the residuals have equal variance along the regression line (homoscedasticity)?
    plot(mod, which = 3, col = c("blue"))
    
    #'  Residuals vs leverage - any influential cases in the data?
    plot(mod, which = 5, col = c("blue"))
  }
  residual_checks(NBF_BF)
  residual_checks(NBM_BF)
  residual_checks(NBF_BM)
  residual_checks(NBM_BM)
  
  
  ####  Predict changes based on breeder status  ####
  #'  -------------------------------------------
  #'  Predict change in nonbreeder numbers based on breeder status
  #'  1. No change to breeder position
  newdat_nochange <- NB_status %>%
    mutate(bf_status = "1_no change",
           bm_status = "1_no change",
           became_brdr = "0",
           Z.breeders = Z.breeders,
           Z.density = Z.density,
           Z.harvest_rate = Z.harvest_rate,
           Z.X2yr_helpers_to_pup_r_t = Z.X2yr_helpers_to_pup_r_t)
  
  BFnochange_NBF<-predict(NBF_BF, newdata = newdat_nochange, se.fit=TRUE)
  BFnochange_NBM<-predict(NBM_BF, newdata = newdat_nochange, se.fit=TRUE)
  BMnochange_NBF<-predict(NBM_BF, newdata = newdat_nochange, se.fit=TRUE)
  BMnochange_NBM<-predict(NBM_BM, newdata = newdat_nochange, se.fit=TRUE)
  
  #'  2. Breeder inherits position
  newdat_inherit <- NB_status %>%
    mutate(bf_status = "2_inherit",
           bm_status = "2_inherit",
           became_brdr = "0",
           Z.breeders = Z.breeders,
           Z.density = Z.density,
           Z.harvest_rate = Z.harvest_rate,
           Z.X2yr_helpers_to_pup_r_t = Z.X2yr_helpers_to_pup_r_t)
  
  BFinherit_NBF<-predict(NBF_BF, newdata = newdat_inherit, se.fit=TRUE)
  BFinherit_NBM<-predict(NBM_BF, newdata = newdat_inherit, se.fit=TRUE)
  BMinherit_NBF<-predict(NBM_BF, newdata = newdat_inherit, se.fit=TRUE)
  BMinherit_NBM<-predict(NBM_BM, newdata = newdat_inherit, se.fit=TRUE)
  
  #'  3. Breeder adopted into position
  newdat_adopt <- NB_status %>%
    mutate(bf_status = "3_adopted",
           bm_status = "3_adopted",
           became_brdr = "0",
           Z.breeders = Z.breeders,
           Z.density = Z.density,
           Z.harvest_rate = Z.harvest_rate,
           Z.X2yr_helpers_to_pup_r_t = Z.X2yr_helpers_to_pup_r_t)
  
  BFadopt_NBF<-predict(NBF_BF, newdata = newdat_adopt, se.fit=TRUE)
  BFadopt_NBM<-predict(NBM_BF, newdata = newdat_adopt, se.fit=TRUE)
  BMadopt_NBF<-predict(NBM_BF, newdata = newdat_adopt, se.fit=TRUE)
  BMadopt_NBM<-predict(NBM_BM, newdata = newdat_adopt, se.fit=TRUE)
  
  
  