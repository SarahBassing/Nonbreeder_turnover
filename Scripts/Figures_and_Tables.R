  #'  ----------------------------
  #'  Nonbreeder turnover figures
  #'  ICFWRU
  #'  Ausband & Bassing
  #'  August 2023
  #'  ----------------------------
  #'  Plot results of nonbreeder turnover analyses
  #'  ----------------------------
  
  #'  Load libraries
  library(msm)
  library(sf)
  library(terra)
  library(tidyterra)
  library(ggspatial)
  library(ggplot2)
  library(grid)
  library(patchwork)
  library(cowplot)
  #devtools::install_github("karthik/wesanderson")
  library(wesanderson)
  library(tidyverse)
  
  #'  Read in model outputs & data
  load("./Outputs/Model_outputs_for_SBB.RData")
  dat <- read.csv("./Data/helper_data.csv")
  
  #'  Load spatial data
  gmu <- st_read("./Shapefiles/IDFG_Game_Management_Units/Game_Management_Units.shp")
  sa_gmu <- gmu[gmu$NAME == "4" | gmu$NAME == "28" | gmu$NAME == "33" | gmu$NAME == "34" | gmu$NAME == "35",] 
  usa <- st_read("./Shapefiles/tl_2012_us_state/tl_2012_us_state.shp")
  id <- usa[usa$NAME == "Idaho",] 
  gmu_wgs84 <- st_transform(gmu, "+proj=longlat +datum=WGS84 +no_defs")
  sa_gmu_wgs84 <- st_transform(sa_gmu, "+proj=longlat +datum=WGS84 +no_defs")
  usa_wgs84 <- st_transform(usa, "+proj=longlat +datum=WGS84 +no_defs")
  id_wgs84 <- st_transform(id, "+proj=longlat +datum=WGS84 +no_defs")
  
  #'  Reformat DEM raster for mapping
  dem <- rast("./Shapefiles/IDFG spatial data/Elevation__10m2.tif"); crs(dem)
  dem_low <- aggregate(dem, 100); res(dem_low)
  writeRaster(dem_low, file = "./Shapefiles/National Elevation Dataset (NED) NAD83/DEM_100m_res.tiff", overwrite = TRUE)
  dem_low_wgs84 <- project(dem_low, "+proj=longlat +datum=WGS84 +no_defs")
  #'  Crop low rez dem to extent of ID shapefile
  v <- vect(id_wgs84)
  dem_low_crop <- crop(dem_low_wgs84, v, mask = TRUE)
  
  #'  Define projections
  gmu_proj <- crs(sa_gmu) 
  id_proj <- crs(id)
  wgs84 <- crs(sa_gmu_wgs84) 
  
  #'  Spatial extent of study area GMUS
  st_bbox(sa_gmu_wgs84)
  
  #'  Select color palette
  names(wes_palettes)
  wes_palette("AsteroidCity1", 5, type = "discrete") 
  pal <- wes_palette("AsteroidCity2", 5, type = "discrete")
  wes_palette("AsteroidCity3", 4, type = "discrete")
  # pal4 <- c("#FBA72A", "#CD7A5C", "#5785C1")
  
  
  #'  --------------------------------
  ####  Maps within maps within maps  ####
  #'  --------------------------------
  #####  Map #1  #####
  #'  Plot USA with Idaho highlighted
  usa_map <- ggplot() +
    geom_sf(data = usa_wgs84, fill = "white", color = "gray50") +
    geom_sf(data = id_wgs84, fill = "gray20", color = "gray20") +
    #'  Constrain plot to the lower 48 
    coord_sf(xlim = c(-124.2, -68.2), ylim = c(25, 49)) +
    theme_minimal() +  
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y=element_blank(),
          #'  No margins around figure
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))
  
  #####  Map #2  #####
  #'  Text to label state of Idaho
  grob <- grobTree(textGrob("Idaho, USA", x = 0.45,  y = 0.95, hjust = 0,
                            gp = gpar(col = "black", fontsize = 18, fontface = "italic")))
  
  #'  Order GMUs geographically (N to S)
  sa_gmu_wgs84$NAME <- factor(sa_gmu_wgs84$NAME, levels = c("4", "28", "33", "34", "35"))
  
  #'  Plot study areas within Idaho
  ID_study_areas <- ggplot() +
    geom_sf(data = gmu_wgs84, fill = "gray95", color="gray50", size = 0.5) +
    # geom_sf(data = id_wgs84, fill = NA, color="gray20", size = 0.5) +
    geom_sf(data = sa_gmu_wgs84, aes(fill = NAME, color = NAME), size = 0.75, alpha = 0.7) +
    scale_fill_manual(values = wes_palette("AsteroidCity1", n = 5)) +
    scale_color_manual(values = wes_palette("AsteroidCity1", n = 5)) +
    labs(fill = "GMU", color = "GMU") +
    geom_rect(aes(xmin = -117.3, xmax = -113.15, ymin = 43.75, ymax = 48.25), color = "black", fill = NA, size = 0.5)  +
    #'  Get rid of lines and axis names
    # theme_bw() +
    # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    #       axis.title.x = element_text(size = 18), 
    #       axis.title.y = element_text(size = 18),
    #       axis.text.x = element_text(size = 16, angle = 45, hjust = 1, colour = "black"), 
    #       axis.text.y = element_text(size = 16, colour = "black"), 
    #       legend.title = element_text(size = 18),
    #       legend.text = element_text(size = 16)) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 16)) +
    theme(legend.justification = c(1, 0)) #+
    #' #'  Add north arrow
    #' annotation_north_arrow(location = "bl", which_north = "true", 
    #'                      pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
    #'                      style = north_arrow_fancy_orienteering(fill = c("gray20", "white",
    #'                                                                      line_col = "gray10"))) +
    #' #'  Add scale bar (be sure to double check the scale)
    #' annotation_scale(location = "bl", width_hint = 0.5, bar_cols = c("gray10", "white"))
  
  #####  Map #3  #####
  #'  Plot close up of study areas
  zoomed_in_gmus <- ggplot() +
    geom_spatraster(data = dem_low_crop, alpha = 0.75) +
    scale_fill_continuous(low = "gray90", high = "gray15", na.value = "transparent") +
    geom_sf(data = sa_gmu_wgs84, color = "black", fill = NA, size = 0.75, show.legend = FALSE) + #fill = NA, 
    #'  Constrain plot to study areas plus some room on the side & bottom
    coord_sf(xlim = c(-117.2, -113.25), ylim = c(43.75, 48.25), expand = TRUE) + #43.94041, 48.06903
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.title.x = element_text(size = 18), 
          axis.title.y = element_text(size = 18),
          axis.text.x = element_text(size = 16, angle = 45, hjust = 1, colour = "black"), 
          axis.text.y = element_text(size = 16, colour = "black"), 
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 16)) +
    labs(x = "Longitude", y = "Latitude", fill = "Elevation (m)") +
    theme(legend.justification = c(1, 0)) + 
    #'  Add north arrow
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering(fill = c("gray20", "white",
                                                                           line_col = "gray10"))) +
    #'  Add scale bar (be sure to double check the scale)
    annotation_scale(location = "bl", width_hint = 0.5, bar_cols = c("gray10", "white"))

  #'  Build plot with map of study areas and inset map of WA
  #'  https://upgo.lab.mcgill.ca/2019/12/13/making-beautiful-maps/
  #'  Requires "cowplot" package
  #'  Don't use png or other calls to save while plotting- formatting gets messed up
  #'  Use export option in Plot window and formatting holds
  tiff(file = "./Outputs/Figures/StudyAreaMap_v1.tiff",
       units = "in", width = 7, height = 9, res = 800, compress = 'lzw')
  StudyArea_Map <- ggdraw(ID_study_areas) + 
    draw_plot(
      {
        usa_map 
      },
      #'  Distance along a (0,1) x-axis to draw the left edge of the plot
      x = 0.40,
      #'  Distance along a (0,1) y-axis to draw the bottom edge of the plot
      y = 0.50,
      #'  Width & height of the plot expressed as proportion of the entire ggdraw object
      #'  THIS DEPENDS ON HOW BIG YOUR PLOT WINDOW IS TOO!!!!
      width = 0.55,
      height = 0.55) +
    theme(panel.background = element_rect(fill = "white", color = "black"))
  plot(StudyArea_Map)
  dev.off()
  
  tiff(file = "./Outputs/Figures/StudyAreaMap_v2.tiff",
       units = "in", width = 10, height = 11, res = 800, compress = 'lzw')
  Full_Map <- ggdraw(zoomed_in_gmus) +
    draw_plot(
      {
        StudyArea_Map #+
      },
      #'  Distance along a (0,1) x-axis to draw the left edge of the plot
      x = 0.60,
      #'  Distance along a (0,1) y-axis to draw the bottom edge of the plot
      y = 0.52,
      #'  Width & height of the plot expressed as proportion of the entire ggdraw object
      #'  THIS DEPENDS ON HOW BIG YOUR PLOT WINDOW IS TOO!!!!
      width = 0.37,
      height = 0.45) 
  plot(Full_Map)
  dev.off()
  
  
  
  
  
  
  
  
  
  
  
  
  
  
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
           #'  Add sex of breeder
           breeder_sex = ifelse(model == "NBF_BF" | model == "NBM_BF", "Female", "Male"),
           #'  Change predictor labels for breeder status
           predictor = ifelse(predictor == "Intercept", "No change", predictor),
           predictor = ifelse(predictor == "BF inherits", "Inherited", predictor),
           predictor = ifelse(predictor == "BM inherits", "Inherited", predictor),
           predictor = ifelse(predictor == "BF adopted", "Adopted", predictor),
           predictor = ifelse(predictor == "BM adopted", "Adopted", predictor),
           predictor = factor(predictor, levels = c("No change", "Inherited", "Adopted")),
           #'  Calculate effect of breeder inheriting position (intercept + inherit)
           adj_betas = ifelse(predictor == "Inherited", betas + lag(betas), betas),
           adj_betas = ifelse(predictor == "Inherited", betas + lag(betas), adj_betas),
           #'  Calculate effect of breeding being adopted (intercept + adopt)
           adj_betas = ifelse(predictor == "Adopted", betas + lag(betas, n = 2), adj_betas),
           adj_betas = ifelse(predictor == "Adopted", betas + lag(betas, n = 2), adj_betas),
           #' #'  Change label for each intercept
           #' predictor = ifelse(model == "NBF_BF" & predictor == "Intercept", "No change in BF", predictor),
           #' predictor = ifelse(model == "NBF_BM" & predictor == "Intercept", "No change in BM", predictor),
           #' predictor = ifelse(model == "NBM_BF" & predictor == "Intercept", "No change in BF", predictor),
           #' predictor = ifelse(model == "NBM_BM" & predictor == "Intercept", "No change in BM", predictor),
           #' predictor = factor(predictor, levels = c("No change in BF", "BF inherits", "BF adopted",
           #'                                          "No change in BM", "BM inherits", "BM adopted")),
           #' #'  Calculate effect of breeder inheriting position (intercept + inherit)
           #' adj_betas = ifelse(predictor == "BF inherits", betas + lag(betas), betas),
           #' adj_betas = ifelse(predictor == "BM inherits", betas + lag(betas), adj_betas),
           #' #'  Calculate effect of breeding being adopted (intercept + adopt)
           #' adj_betas = ifelse(predictor == "BF adopted", betas + lag(betas, n = 2), adj_betas),
           #' adj_betas = ifelse(predictor == "BM adopted", betas + lag(betas, n = 2), adj_betas),
           #'  Calculate 95% confidence intervals using updated betas and SEs
           adj_lci = adj_betas - (delta_SEs * 1.96),
           adj_uci = adj_betas + (delta_SEs * 1.96)) %>%
    relocate(adj_betas, .after = pval) %>%
    relocate(delta_SEs, .after = adj_betas) %>%
    relocate(adj_lci, .before = delta_SEs) %>%
    relocate(adj_uci, .after = adj_lci) #%>%
    # rename("Breeder status" = "predictor") %>%
    # rename("Change in number of nonbreeders" = "adj_betas")
  
  #'  ----------------
  ####  Plot results  ####
  #'  ----------------
  #'  New model labels
  model.labs <- c("Nonbreeding females", "Nonbreeding females", "Nonbreeding males", "Nonbreeding males")
  # model.labs <- c("Nonbreeding female - Breeding female", "Nonbreeding female - Breeding male",
  #                 "Nonbreeding male - Breeding female", "Nonbreeding male - Breeding male")
  names(model.labs) <- c("NBF_BF", "NBF_BM", "NBM_BF", "NBM_BM")
  
  #'  Plot effect of breeder status on change in number of nonbreeders
  nonbreeder_turnover_plot <- ggplot(breeder_effect, aes(x = predictor, y = adj_betas, group = model, color = predictor, shape = breeder_sex)) +
    geom_errorbar(aes(ymin = adj_lci, ymax = adj_uci, color = predictor), width = 0.1, position = position_dodge(width = 0.4)) +
    scale_fill_manual(values = wes_palette("AsteroidCity1", n = 3)) +
    geom_point(stat = "identity", aes(col = predictor), size = 2.5, position = position_dodge(width = 0.4)) +
    scale_color_manual(values = wes_palette("AsteroidCity1", n = 3)) +
    theme_bw() +
    ylab("Change in number of nonbreeders in pack") +
    xlab("Breeder status") +
    facet_wrap(~model, #scales = "free_x",
               labeller = labeller(model = model.labs)) +
    theme(strip.background = element_rect(fill = "white")) +
    labs(color = "Breeder status", shape = "Breeder sex")
  plot(nonbreeder_turnover_plot)
    
  #"  Save figure
  ggsave("./Outputs/Figures/Nonbreeder_turnover_figure.tiff", nonbreeder_turnover_plot, 
         units = "in", width = 7, height = 5, dpi = 600, device = 'tiff', compress = 'lzw')
  
  
  
  