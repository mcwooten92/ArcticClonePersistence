purrr::walk(c("beepr","sp","sf","tidyverse","terra","viridis"), library, character.only = T); Sys.sleep(0.5)

setwd('F:/DELTROZZO/caide'); Sys.sleep(0.5) # Change working directory accordingly

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      # The objects below can be imported into RStudio by uncommenting the lines

      # # Master dataframe
      # df_msrmts = readxl::read_excel("./drone_photos/measurements.xlsx") %>% as.data.frame(); Sys.sleep(0.5)
      # 
      # # RGB images: uncorrected brightness
      # x_files = list.files(path = "./drone_photos/", pattern = "\\.jpg$", full.names = T); Sys.sleep(0.5)
      # list_imgs_raw = list(); Sys.sleep(0.5)
      # for(x_img_i in seq_along(x_files)){
      #   x_img_path = x_files[x_img_i]
      #   x_img_name = tools::file_path_sans_ext(basename(x_img_path))
      #   list_imgs_raw[[x_img_name]] = terra::rast(x_img_path)}; Sys.sleep(0.5)
      # 
      # # Dataframe: RGB values for the paper sheet
      # load("./files/df_rgb_vals.RData"); Sys.sleep(0.5)
      # 
      # # RGB images: corrected brightness (using cntrl2113 as reference)
      # # Note: cases where image was lit but papers were in shadow used the uncorrected image
      # x_files = list.files(path = "./drone_photos/cor_brightness", pattern = "\\.tif$", full.names = T); Sys.sleep(0.5)
      # list_imgs_cor = list(); Sys.sleep(0.5)
      # for(x_i in seq_along(x_files)){
      #   x_img_i = x_files[x_i]
      #   x_img_name = tools::file_path_sans_ext(basename(x_img_i))
      #   list_imgs_cor[[x_img_name]] = terra::rast(x_img_i)}; Sys.sleep(0.5)
      # 
      # # RGBVI images
      # x_files = list.files(path = "./drone_photos/cor_brightness/rgbvi", pattern = "\\.tif$", full.names = T); Sys.sleep(0.5)
      # list_imgs_rgbvi = list(); Sys.sleep(0.5)
      # for(x_i in seq_along(x_files)){
      #   x_img_i = x_files[x_i]
      #   x_img_name = tools::file_path_sans_ext(basename(x_img_i))
      #   list_imgs_rgbvi[[x_img_name]] = terra::rast(x_img_i)}; Sys.sleep(0.5)
      # 
      # Various dataframe objects
      load("./files/df_circle_rgbvi.RData"); Sys.sleep(0.5)       # Dataframe: circular plot summaries
      load("./files/df_trnscts_rgbvi.RData"); Sys.sleep(0.5)      # Dataframe: transect metrics
      load("./files/df_imgs_rgbvi.RData"); Sys.sleep(0.5)         # Dataframe: image metrics
      load("./files/df_glob_rgbvi.RData"); Sys.sleep(0.5)         # Dataframe: global metrics
      load("./files/vals_control_rgbvi.RData"); Sys.sleep(0.5)      # Dataframe: control plots' summary metrics
      load("./files/df_crit_glob_rgbvi.RData"); Sys.sleep(0.5)    # Dataframe: critical distances
      load("./files/df_crit_imgs_rgbvi.RData"); Sys.sleep(0.5)
      load("./files/vals_critdist_inc_imgs.RData"); Sys.sleep(0.5)

      # Custom ggplot theme
      file.edit("./scripts/theme_custom.R"); source("./scripts/theme_custom.R")
      
      rm(list = ls(pattern = "^x_"))
      
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Dataframe: determining pixel distance #### 

# A paper sheet is 8.5 x 11 in. or 21.6 × 27.9 cm
# Measurements (pix): length = X pix; width = Y pix
# Conversion (m/pixel): 
# ..... length = 0.279 m / X pix
# ...... width = 0.216 m / Y pix

df_msrmts = readxl::read_excel("./drone_photos/measurements.xlsx") %>% as.data.frame(); Sys.sleep(0.5)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Images: radiometric normalization ####

      # Function to extract RGB values from all images
      fx_paper_rgb = function(img, img_name, msrmts, plot = F){
        
        # Coordinates: paper's centroid and radius for each image
        centroid = msrmts %>% 
          filter(image == img_name) %>%
          dplyr::select(centroid_x, centroid_y, radius)
        
        # Coordinates: all pixels in the image
        coords = xyFromCell(img, 1:ncell(img))
        
        # Identify pixels within the circle ((dist_x^2 + dist_y^2) <= r^2)
        circle_cells = which((coords[,1] - centroid$centroid_x)^2 + (coords[,2] - centroid$centroid_y)^2 <= centroid$radius^2)
        
        # Plot settings for plot = T (RGB image + circle)
        if(plot){
          plotRGB(img, r = 1, g = 2, b = 3, stretch = "lin")
          points(coords[circle_cells, 1], coords[circle_cells, 2], pch = 0, cex = 0.05, col = "red")}
        
        # Brighness values for pixels within the circle
        rgb_vals = terra::extract(img, circle_cells)
        
        # Median RGB brightness values for the reference paper
        return(data.frame(
          image = img_name,
          R = median(rgb_vals[[1]], na.rm = T),
          G = median(rgb_vals[[2]], na.rm = T),
          B = median(rgb_vals[[3]], na.rm = T)))
      }; Sys.sleep(0.5)

# Import raw images
x_files = list.files(path = "./drone_photos", pattern = "\\.jpg$", full.names = T); Sys.sleep(0.5)
list_imgs_raw = list(); Sys.sleep(0.5)

for(x_img_i in seq_along(x_files)){
  x_img_path = x_files[x_img_i]
  x_img_name = tools::file_path_sans_ext(basename(x_img_path))
  list_imgs_raw[[x_img_name]] = terra::rast(x_img_path)}; Sys.sleep(0.5)     

# Extract RGB values from the paper sheet across all images
df_rgb_vals = list(); Sys.sleep(0.5)

for(x_img_i in names(list_imgs_raw)){
  df_rgb_vals[[x_img_i]] = fx_paper_rgb(
    img = list_imgs_raw[[x_img_i]], img_name = x_img_i, msrmts = df_msrmts, plot = F)}; Sys.sleep(0.5); beep(1)

df_rgb_vals = do.call(what = rbind, args = df_rgb_vals); Sys.sleep(0.5)

# Plot a single image
fx_paper_rgb(img = list_imgs_raw$cntrl2113, img_name = "cntrl2113", msrmts = df_msrmts, plot = T)

# Define the reference image
# Evenly lit, non-saturated (i.e., not 255-255-255) white paper sheet
df_rgb_vals_ref = df_rgb_vals[df_rgb_vals$image == "cntrl2113", ]; Sys.sleep(0.5)

# Correction coefficients for the RGB channels of each image
df_rgb_vals$R_corr = round((df_rgb_vals_ref$R / df_rgb_vals$R), 3); Sys.sleep(0.5)
df_rgb_vals$G_corr = round((df_rgb_vals_ref$G / df_rgb_vals$G), 3); Sys.sleep(0.5)
df_rgb_vals$B_corr = round((df_rgb_vals_ref$B / df_rgb_vals$B), 3); Sys.sleep(0.5)

# save(df_rgb_vals, file = "./files/df_rgb_vals.RData", envir = .GlobalEnv); Sys.sleep(0.5)

# Correct the brightness of all images
x_list_imgs_cor = list(); Sys.sleep(0.5)

for(x_img_i in names(list_imgs_raw)){
  x_corrected = list_imgs_raw[[x_img_i]]
  x_coeff = df_rgb_vals[df_rgb_vals$image == x_img_i, ]
  x_corrected[[1]] = x_corrected[[1]] * (df_rgb_vals_ref$R / x_coeff$R)
  x_corrected[[2]] = x_corrected[[2]] * (df_rgb_vals_ref$G / x_coeff$G)
  x_corrected[[3]] = x_corrected[[3]] * (df_rgb_vals_ref$B / x_coeff$B)
  x_list_imgs_cor[[x_img_i]] = x_corrected}; Sys.sleep(0.5); beep(3)

# Revert correction in those cases where the paper is in the shadow
# Avoids unnecessary correction
list_imgs_cor = list(); Sys.sleep(0.5)

for(x_img_i in names(x_list_imgs_cor)){
  x_img_commt = df_msrmts$comments[df_msrmts$image == x_img_i]
  if(!is.na(x_img_commt) && x_img_commt == "paper in shadow"){
    list_imgs_cor[[x_img_i]] = list_imgs_raw[[x_img_i]]}
  else{ 
    list_imgs_cor[[x_img_i]] = x_list_imgs_cor[[x_img_i]]}}; Sys.sleep(0.5)

# Export the images with corrected brightness 
Sys.setenv(GDAL_PAM_ENABLED = "NO"); Sys.sleep(0.5)
for(x_img_i in names(list_imgs_cor)){
  writeRaster(
    x = list_imgs_cor[[x_img_i]],
    filename = paste0("./drone_photos/cor_brightness/", x_img_i, ".tif"),
    overwrite = T,
    datatype = "INT1U")}; Sys.sleep(0.5)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Images: calculating spectral indices #### 

# RGBVI
list_imgs_rgbvi = list(); Sys.sleep(0.5)

for(x_img_i in names(list_imgs_cor)){
  x_img = list_imgs_cor[[x_img_i]]
  x_R = x_img[[1]]
  x_G = x_img[[2]]
  x_B = x_img[[3]]
  list_imgs_rgbvi[[x_img_i]] = (x_G^2 - (x_R * x_B)) / (x_G^2 + (x_R * x_B))}; Sys.sleep(0.5); beep(1)

# Export the RGBVI images
Sys.setenv(GDAL_PAM_ENABLED = "NO"); Sys.sleep(0.5)

for(x_img_i in names(list_imgs_rgbvi)){
  writeRaster(
    x = list_imgs_rgbvi[[x_img_i]],
    filename = paste0("./drone_photos/cor_brightness/rgbvi/", x_img_i, ".tif"),
    overwrite = T,
    datatype = "FLT4S")}; Sys.sleep(0.5); beep(1)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Shapefiles: circle-wise metrics (control images) #### 

      # Create a 3.5-m circular plot (38.5 m^2)
      fx_make_circle = function(img_name, msrmts){
        
        # Skip if image is not a control image
        if(!startsWith(img_name, "cntrl")) return(NULL)
        
        # Obtain measurements from the current image
        msrmts_i = msrmts[msrmts$image == img_name, ]
        
        # Define the circle
        radius_px = 3.5 / msrmts_i$mtopix
        theta = seq(0, 2*pi, length.out = 200)
        
        # Define circle, with the paper in the middle
        circle_coords = cbind(
          msrmts_i$centroid_x + radius_px*cos(theta),
          msrmts_i$centroid_y + radius_px*sin(theta))
        
        return(terra::vect(circle_coords, type = "polygons"))
      }; Sys.sleep(0.5)

# Circles
shp_circle = list(); Sys.sleep(0.5)

for(x_img_i in names(list_imgs_cor)){
  shp_circle[[x_img_i]] = fx_make_circle(
    img_name = x_img_i, msrmts = df_msrmts)}; Sys.sleep(0.5)

x_img_i = 15; Sys.sleep(0.5)
plotRGB(list_imgs_cor[[x_img_i]]); Sys.sleep(0.5)
plot(shp_circle[[x_img_i]], add = T, border = "hotpink", lwd = 5); Sys.sleep(0.5)
plot(list_imgs_rgbvi[[x_img_i]], range = c(-0.5, 0.5), col = turbo(1000)); Sys.sleep(0.5)
plot(shp_circle[[x_img_i]], add = T, border = "black", lwd = 5); Sys.sleep(0.5)

# Dataframe: circular plot summary statistics: img + avg ####

      # Obtain summary statistics for the circular plot
      fx_circle_summary = function(img, poly){
      
        # Extract the index values
        pixels = terra::extract(img, poly)[,2]
        
        # Return a summary dataframe
        return(data.frame(
          mean = mean(pixels, na.rm = T),
          sd = sd(pixels, na.rm = T),
          n = sum(!is.na(pixels)),
          median = median(pixels, na.rm = T),
          q25 = quantile(pixels, 0.25, na.rm = T),
          q75 = quantile(pixels, 0.75, na.rm = T),
          row.names = NULL
        ))}; Sys.sleep(0.5)

# ..... RGBVI
x_names_cntrl = names(list_imgs_rgbvi)[startsWith(names(list_imgs_rgbvi), "cntrl")]; Sys.sleep(0.5)
df_circle_rgbvi = list(); Sys.sleep(0.5)

for(x_img_i in x_names_cntrl){
  df_circle_rgbvi[[x_img_i]] = fx_circle_summary(
    img = list_imgs_rgbvi[[x_img_i]], poly = shp_circle[[x_img_i]])}; Sys.sleep(0.5); beep(3)

df_circle_rgbvi = bind_rows(df_circle_rgbvi); Sys.sleep(0.5)
df_circle_rgbvi = df_circle_rgbvi %>% mutate(image = x_names_cntrl) %>% dplyr::select(image, everything()); Sys.sleep(0.5)

# save(df_circle_rgbvi, file = "./files/df_circle_rgbvi.RData", envir = .GlobalEnv); Sys.sleep(0.5)

rm(list = ls(pattern = "^x_")); Sys.sleep(0.5)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Shapefiles: transects for pixel-wise measurements (muskox carcass images) #### 

      # Riparian muskox images should NOT include those transect halves that overlap with creeks
      x_drop_riparian = list(
        muskx2103 = c("vrtcl_up", "hrztl_rt"),
        muskx2107 = c("vrtcl_up", "vrtcl_dn", "hrztl_lt")); Sys.sleep(0.5)

      # Create two 0.50 x 10 m transects centered in the paper sheet
      fx_make_transect = function(img_name, msrmts){
        
        # Skip if image is not a muskox image
        if(!startsWith(img_name, "muskx")) return(NULL)
        
        # Obtain measurements from the current image
        msrmts_i = msrmts[msrmts$image == img_name, ]
        centroid_x = msrmts_i$centroid_x; centroid_y = msrmts_i$centroid_y
        
        # Define transect, with the paper in the middle
        transect_len_px = 10 / msrmts_i$mtopix # length of the transect, in pixels
        transect_wid_px = 0.5 / msrmts_i$mtopix # width of the transect, in pixels
        
        # Bounding box
        make_box = function(xmin, xmax, ymin, ymax){
          matrix(c(
            xmin, ymin,
            xmax, ymin,
            xmax, ymax,
            xmin, ymax,
            xmin, ymin
          ), ncol = 2, byrow = T)}
        
        trnscts = list(
          
          # Vertical transects
          vrtcl_up = vect(make_box(xmin = centroid_x - transect_wid_px/2, xmax = centroid_x + transect_wid_px/2, ymin = centroid_y, ymax = centroid_y + transect_len_px/2), type = "polygons"),
      
          vrtcl_dn = vect(make_box(xmin = centroid_x - transect_wid_px/2, xmax = centroid_x + transect_wid_px/2, ymin = centroid_y - transect_len_px/2, ymax = centroid_y), type = "polygons"),
      
          # Horizontal transects
          hrztl_lt = vect(make_box(xmin = centroid_x - transect_len_px/2, xmax = centroid_x, ymin = centroid_y - transect_wid_px/2, ymax = centroid_y + transect_wid_px/2), type = "polygons"),
      
          hrztl_rt = vect(make_box(xmin = centroid_x, xmax = centroid_x + transect_len_px/2, ymin = centroid_y - transect_wid_px/2, ymax = centroid_y + transect_wid_px/2), type = "polygons"))
      
        # Drop transects that fall on water
        if(img_name %in% names(x_drop_riparian)){trnscts = trnscts[names(trnscts) %in% x_drop_riparian[[img_name]]]} 
        
        return(trnscts)}; Sys.sleep(0.5)

# Transects
shp_trnscts = list(); Sys.sleep(0.5)
for(x_img_i in names(list_imgs_cor)){
  shp_trnscts[[x_img_i]] = fx_make_transect(
    img_name = x_img_i, msrmts = df_msrmts)}; Sys.sleep(0.5)

x_img_i = "muskx2103"; Sys.sleep(0.5)
plotRGB(list_imgs_cor[[x_img_i]]); Sys.sleep(0.5)
plot(shp_trnscts[[x_img_i]]$vrtcl_up, add = T, border = "hotpink", lwd = 5); Sys.sleep(0.5)
plot(shp_trnscts[[x_img_i]]$vrtcl_dn, add = T, border = "hotpink", lwd = 5); Sys.sleep(0.5)
plot(shp_trnscts[[x_img_i]]$hrztl_lt, add = T, border = "hotpink", lwd = 5); Sys.sleep(0.5)
plot(shp_trnscts[[x_img_i]]$hrztl_rt, add = T, border = "hotpink", lwd = 5); Sys.sleep(0.5)
plot(list_imgs_rgbvi[[x_img_i]], range = c(-0.5, 0.5), col = turbo(1000)); Sys.sleep(0.5)
plot(shp_trnscts[[x_img_i]]$vrtcl_up, add = T, border = "black", lwd = 5); Sys.sleep(0.5)
plot(shp_trnscts[[x_img_i]]$vrtcl_dn, add = T, border = "black", lwd = 5); Sys.sleep(0.5)
plot(shp_trnscts[[x_img_i]]$hrztl_lt, add = T, border = "black", lwd = 5); Sys.sleep(0.5)
plot(shp_trnscts[[x_img_i]]$hrztl_rt, add = T, border = "black", lwd = 5); Sys.sleep(0.5)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Dataframe: transect-level VI profile (img + trnsct + dist + mean) ####

      # Extract VI values from the treatment images
      fx_transect_values = function(img_i, transects_i, mtopix, centroid_x, centroid_y, img_name){
        
        df_list = list()
      
        for(trn_half in names(transects_i)){
        
          # Mask pixel values that do not fall within the vertical/horizontal transect
          x_vals = mask(img_i, transects_i[[trn_half]])
          
          # Compile all pixel values and their coordinates
          x_df = as.data.frame(x_vals, xy = T, na.rm = T)
          
          # Skip empty transects
          if(nrow(x_df) == 0) next
      
          # Renaming column
          names(x_df)[3] = "rgbvi"
          
          # Scale coordinates
          if(trn_half == "vrtcl_up"){x_df$dist_m = (x_df$y - centroid_y) * mtopix}
          
          if(trn_half == "vrtcl_dn"){x_df$dist_m = (centroid_y - x_df$y) * mtopix}
          
          if(trn_half == "hrztl_rt"){x_df$dist_m = (x_df$x - centroid_x) * mtopix}
          
          if(trn_half == "hrztl_lt"){x_df$dist_m = (centroid_x - x_df$x) * mtopix}
          
          # Column for transect half  
          x_df$trnsct = trn_half
          
          # Compile result as a list of dataframes
          df_list[[trn_half]] = x_df}
        
        # Merge the dataframes in one; create transect and distance (in m) columns
        df_i = bind_rows(df_list)
        
        if(nrow(df_i) == 0) return(NULL)
        
        # For each step in the transect, get the average VI and more
        df_i %>% 
          group_by(trnsct, dist_m) %>%
          summarize(
            millimetric_mean = mean(rgbvi, na.rm = T),
            millimetric_pixels = sum(!is.na(rgbvi)),
            .groups = "drop") %>%
          
          # Record image name
          mutate(image = img_name) %>% 
          
          # Reorganize the columns
          dplyr::select(image, trnsct, dist_m, millimetric_mean, millimetric_pixels)}; Sys.sleep(0.5)

# ..... RGBVI
x_names_muskx = names(list_imgs_rgbvi)[startsWith(names(list_imgs_rgbvi), "muskx")]; Sys.sleep(0.5)
x_names_muskx = x_names_muskx[x_names_muskx != "muskx2109"] # Drop the blurry image

df_trnscts_rgbvi = list(); Sys.sleep(0.5)

for(x_img_i in x_names_muskx){
  df_trnscts_rgbvi[[x_img_i]] = fx_transect_values(
    img_i = list_imgs_rgbvi[[x_img_i]],
    transects_i = shp_trnscts[[x_img_i]],
    mtopix = df_msrmts$mtopix[df_msrmts$image == x_img_i],
    centroid_x = df_msrmts$centroid_x[df_msrmts$image == x_img_i],
    centroid_y = df_msrmts$centroid_y[df_msrmts$image == x_img_i],
    img_name = x_img_i)
  }; Sys.sleep(0.5); beep(3)

df_trnscts_rgbvi = bind_rows(df_trnscts_rgbvi); Sys.sleep(0.5)
df_trnscts_rgbvi = df_trnscts_rgbvi %>% mutate(image = as.factor(image), trnsct = as.factor(trnsct))

df_trnscts_rgbvi = df_trnscts_rgbvi %>%
  # Binning distances (0.01 m increments)
  mutate(dist_bin = round(dist_m, 2)) %>%
  group_by(image, trnsct, dist_bin) %>%
  summarize(
    cm_mean = mean(millimetric_mean, na.rm = T),
    cm_pixel_rows = n(),
    .groups = "drop")

# save(df_trnscts_rgbvi, file = "./files/df_trnscts_rgbvi.RData", envir = .GlobalEnv); Sys.sleep(0.5)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Dataframe: image-level VI profile (img + dist + mean) ####

# ..... RGBVI
df_imgs_rgbvi = df_trnscts_rgbvi %>%
  group_by(image, dist_bin) %>% 
  summarize( # Average, SD, no. of observations and more
    mean = mean(cm_mean, na.rm = T), 
    sd = sd(cm_mean, na.rm = T), 
    median = median(cm_mean, na.rm = T),
    q25 = quantile(cm_mean, 0.25, na.rm = T),
    q75 = quantile(cm_mean, 0.75, na.rm = T),
    n = n(), .groups = "drop") %>%
  mutate( # SE, CI and rolling average (20 cm or 10+10 cm)
    se = sd / sqrt(n), 
    lci = mean - 1.96 * se, 
    uci = mean + 1.96 * se,
    roll = zoo::rollmean(mean, k = 20, fill = NA, align = "center")); Sys.sleep(0.5)

# save(df_imgs_rgbvi, file = "./files/df_imgs_rgbvi.RData", envir = .GlobalEnv); Sys.sleep(0.5)

# # ..... Plot (RGBVI): lines for all ~20 muskox images together
# # plot_imgs_rgbvi =
# ggplot(df_imgs_rgbvi, aes(x = dist_bin)) +
#   geom_line(aes(y = mean, col = image), linewidth = 0.5, alpha = 0.5) +
#   geom_line(aes(y = roll, col = image), linewidth = 1) +
#   scale_x_continuous(limits = c(0, 5)) +
#   scale_y_continuous(limits = c(-0.1, 0.5), breaks = seq(-0.1, 0.5, 0.1)) +
#   labs(x = "Distance (m)", y = "RGBVI") +
#   # scale_color_viridis_d(option = "turbo") +
#   # scale_fill_viridis_d(option = "turbo") +
#   geom_hline(yintercept = median(df_circle_rgbvi$median), lty = 1, lwd = 1) +
#   geom_hline(yintercept = median(df_circle_rgbvi$q25), lty = 2, lwd = 0.75) +
#   geom_hline(yintercept = median(df_circle_rgbvi$q75), lty = 2, lwd = 0.75) +
#   # theme_custom() + theme(legend.position = "none")
#   theme_bw() + theme(legend.position = "none")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Dataframe: global-level VI profile (all cases) ####

# ..... RGBVI
df_glob_rgbvi = df_imgs_rgbvi %>%
  group_by(dist_bin) %>%
  rename(mean_img = mean, median_img = median) %>% 
  summarize(
    mean = mean(mean_img, na.rm = T),
    sd = sd(mean_img, na.rm = T),
    median = median(median_img, na.rm = T),
    q25 = quantile(median_img, 0.25, na.rm = T),
    q75 = quantile(median_img, 0.75, na.rm = T),
    n = n(),
    se = sd / sqrt(n),
    lci = mean - 1.96 * se,
    uci = mean + 1.96 * se,
    .groups = "drop") %>%
  mutate(roll = zoo::rollmean(mean, k = 20, fill = NA, align = "center"))

# save(df_glob_rgbvi, file = "./files/df_glob_rgbvi.RData", envir = .GlobalEnv); Sys.sleep(0.5)

# Metrics for the control image
# Note: there is not enough replication to estimate a CI from that single control circle.
# Observations (pixels) are not independent, and that leads to a severe underestimation of the SE
# Hence, I use the GLOBAL control metrics
vals_control_rgbvi = data.frame(
  mean = mean(df_circle_rgbvi$mean, na.rm = T),
  sd = sd(df_circle_rgbvi$mean, na.rm = T),
  lci = (mean(df_circle_rgbvi$mean, na.rm = T) - 1.96 * sd(df_circle_rgbvi$mean, na.rm = T) / sqrt(nrow(df_circle_rgbvi))),
  uci = (mean(df_circle_rgbvi$mean, na.rm = T) + 1.96 * sd(df_circle_rgbvi$mean, na.rm = T) / sqrt(nrow(df_circle_rgbvi))))

# save(vals_control_rgbvi, file = "./files/vals_control_rgbvi.RData", envir = .GlobalEnv); Sys.sleep(0.5)

ggplot(df_glob_rgbvi, aes(x = dist_bin)) + 
  geom_ribbon(aes(ymin = lci, ymax = uci), fill = "lightseagreen", alpha = 0.25, na.rm = T) +
  geom_line(aes(y = mean), linewidth = 3, col = "lightseagreen") +
  geom_hline(yintercept = vals_control_rgbvi$mean, lty = 1, lwd = 1.5) +
  geom_hline(yintercept = vals_control_rgbvi$lci, lty = 2, lwd = 0.75) +
  geom_hline(yintercept = vals_control_rgbvi$uci, lty = 2, lwd = 0.75) +
  labs(x = "Distance (m)", y = "RGBVI") +
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(0.025, 0.2), breaks = seq(0.025, 0.2, 0.05)) +
  theme_bw()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Dataframe: finding stabilization distance (global) ####
# Distance at which global transect CI overlaps with control-image reference envelope for at least 50 cm

df_crit_glob_rgbvi = df_glob_rgbvi %>% 
  arrange(dist_bin) %>% 
  dplyr::select(dist_bin, mean, sd, lci, uci) %>% 
  mutate(
    # logical: is the CI fully contained within the control's envelope?
    inside = (lci <= vals_control_rgbvi$uci & uci >= vals_control_rgbvi$lci),
    # logical: is the CI fully contained in the envelope for at least 50 cm?
    stable = zoo::rollapply(inside, 50, all, fill = NA, align = "left"),
    # logical: is this the bin where sustained overlap begins?
    start_stable = stable & !dplyr::lag(stable, default = F),
    # logical: has the CI been outside the envelope for at least one bin? 
    had_break = cumsum(!inside) > 0)

# save(df_crit_glob_rgbvi, file = "./files/df_crit_glob_rgbvi.RData", envir = .GlobalEnv); Sys.sleep(0.5)

x_crit_dist = which(df_crit_glob_rgbvi$start_stable & df_crit_glob_rgbvi$had_break)[1]

val_critdist_allimgs = if(!is.na(x_crit_dist)) df_crit_glob_rgbvi$dist_bin[x_crit_dist] else NA
val_critdist_allimgs
# 1.08 m

# plot_global_rgbvi =
ggplot(df_glob_rgbvi, aes(x = dist_bin)) + 
  geom_ribbon(aes(ymin = lci, ymax = uci), fill = "lightseagreen", alpha = 0.25, na.rm = T) +
  geom_line(aes(y = mean), linewidth = 3, col = "lightseagreen") +
  geom_hline(yintercept = vals_control_rgbvi$mean, lty = 1, lwd = 1.5) +
  geom_hline(yintercept = vals_control_rgbvi$lci, lty = 2, lwd = 0.75) +
  geom_hline(yintercept = vals_control_rgbvi$uci, lty = 2, lwd = 0.75) +
  geom_vline(xintercept = val_critdist_allimgs, lwd = 2, col = "hotpink") +
  labs(title = "(a)", x = "Distance (m)", y = "RGBVI") +
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(0.025, 0.2), breaks = seq(0.025, 0.2, 0.05)) +
  annotate(geom = "text", x = val_critdist_allimgs + 0.5, y = 0.15, label = paste0("— ", val_critdist_allimgs, " m"), size = 10, fontface = "bold") +  
  # theme_custom()
  theme_bw()

x_rgb = jpeg::readJPEG("./drone_photos/muskx2102.jpg") 
x_img_i = "muskx2102"; x_msrmts_i = df_msrmts[df_msrmts$image == x_img_i, ]
x_hex_matrix <- apply(x_rgb, 1:2, function(v) rgb(v[1], v[2], v[3]))
x_df_img <- reshape2::melt(x_hex_matrix)
colnames(x_df_img) <- c("y", "x", "hex_color")
x_theta = seq(0, 2*pi, length.out = 200)
x_radius_px = val_critdist_allimgs / x_msrmts_i$mtopix
x_df_circle = data.frame(
  x = df_msrmts$centroid_x[df_msrmts$image == x_img_i] + val_critdist_allimgs / df_msrmts$mtopix[df_msrmts$image == x_img_i] * cos(x_theta),
  y = -(df_msrmts$centroid_y[df_msrmts$image == x_img_i] + val_critdist_allimgs / df_msrmts$mtopix[df_msrmts$image == x_img_i] * sin(x_theta)))

plot_example_circle =
ggplot(x_df_img, aes(x = x, y = -y, fill = hex_color)) + 
  geom_raster() + 
  scale_fill_identity() + 
  geom_path(data = x_df_circle, aes(x = x, y = y), inherit.aes = F, color = "hotpink", linewidth = 2) +  coord_equal() + 
  labs(title = "(b)", x = "", y = "") +
  theme_custom_void()

ggsave("./figures/lineplot_global.png", 
       cowplot::plot_grid(
         cowplot::plot_grid(NULL,plot_global_rgbvi,NULL, nrow = 3, rel_heights = c(0.075, 1, 0.05)), 
         plot_example_circle, nrow = 1),
       width = 30, height = 15, units = "in", dpi = 300, bg = "white", limitsize = F)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Dataframe: finding stabilization distance (images) ####
# Distance at which transect's LCI overlaps with reference value (i.e. circle's quantile) for at least 50 cm

df_crit_imgs_rgbvi = data.frame(image = character(), x_critdist = numeric())

for(x_img in levels(df_imgs_rgbvi$image)){
  
  if(length(vals_control_rgbvi$mean) == 0) next
  
  x_df = df_imgs_rgbvi[df_imgs_rgbvi$image == x_img, ] %>% 
    arrange(dist_bin) %>% 
    # For each distance bin,
    mutate(
      # logical: is the CI fully contained within the control's envelope?
      inside = (lci <= vals_control_rgbvi$uci & uci >= vals_control_rgbvi$lci),
      # logical: is the CI fully contained in the envelope for at least 50 cm?
      stable = zoo::rollapply(inside, 50, all, fill = NA, align = "left"),
      # logical: is this the bin where sustained overlap begins?
      start_stable = stable & !dplyr::lag(stable, default = F),
      # logical: has the CI been outside the envelope for at least one bin? 
      had_break = cumsum(!inside) > 0)
  
  # First distance bin of sustained overlap (beyond 50 cm)
  x_critidx = which(x_df$start_stable & x_df$dist_bin > 0.5 & x_df$had_break)[1]
  x_critdist = if(!is.na(x_critidx)) x_df$dist_bin[x_critidx] else NA
  
  # Compile the dataframe
  df_crit_imgs_rgbvi = rbind(df_crit_imgs_rgbvi, data.frame(
    image = x_img, dist_crit = x_critdist))}

df_crit_imgs_rgbvi = df_crit_imgs_rgbvi %>% mutate(
  # Assign "0 m" manually for those cases that are clear flats
  dist_crit = ifelse(image %in% c(
    "muskx2104",
    "muskx2105",
    "muskx2108",
    "muskx2109",
    "muskx2114",
    "muskx2115",
    "muskx2121",
    "muskx2124",
    "muskx2125",
    "muskx2126B",
    "muskx2235",
    "muskx2366",
    "muskx2376"), 0, dist_crit),
  # Assign labels to cases: increase in greennes or flat 
  case = ifelse(dist_crit != 0, "increase", "flat")) %>% 
  # Drop those cases where the critical distance is NA
  filter(!is.na(dist_crit))

# Adding field data
x_fielddata = data.frame(
  image = c("muskx2101","muskx2102","muskx2103","muskx2104","muskx2105","muskx2106","muskx2107","muskx2108","muskx2110","muskx2113","muskx2115","muskx2116","muskx2119","muskx2121","muskx2124","muskx2125","muskx2126A","muskx2126B","muskx2235","muskx2240","muskx2361","muskx2362","muskx2366","muskx2368","muskx2376"),
  yrs_death = c(3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,">6",3,3,3,6,6,1),
  crcss_size = c(1,1,1,2,1,2,2,2,1,2,2,2,2,2,2,2,2,1,2,1,2,2,2,2,2),
  hab_moist = c(3,2,3,1,2,2,3,3,1,1,2,2,2,2,1,2,3,3,2,2,1,2,2,3,2))

df_crit_imgs_rgbvi = df_crit_imgs_rgbvi %>% 
  left_join(x_fielddata, by = "image") 

df_crit_imgs_rgbvi = df_crit_imgs_rgbvi %>% mutate(
  image = factor(image), 
  case = factor(case), 
  crcss_size = factor(crcss_size), 
  hab_moist = factor(hab_moist), 
  yrs_death = factor(yrs_death, levels = c("1", "3", "6", ">6")))

# save(df_crit_imgs_rgbvi, file = "./files/df_crit_imgs_rgbvi.RData", envir = .GlobalEnv); Sys.sleep(0.5)


plot_imgs_rgbvi_list = list()

for(x_img in levels(df_imgs_rgbvi$image)){
  
  x_imgcntrl = sub("^muskx", "cntrl", x_img)
  
  plot_imgs_rgbvi_list[[x_img]] =
    ggplot(df_imgs_rgbvi[df_imgs_rgbvi$image == x_img, ], aes(x = dist_bin)) +
    geom_ribbon(aes(ymin = lci, ymax = uci), fill = "lightseagreen", alpha = 0.4, na.rm = T) +
    geom_line(aes(y = mean), linewidth = 1, col = "lightseagreen") +
    geom_hline(yintercept = vals_control_rgbvi$mean, lty = 1, lwd = 1.5) +
    geom_hline(yintercept = vals_control_rgbvi$lci, lty = 2, lwd = 0.75) +
    geom_hline(yintercept = vals_control_rgbvi$uci, lty = 2, lwd = 0.75) +
    geom_vline(xintercept = df_crit_imgs_rgbvi$dist_crit[df_crit_imgs_rgbvi$image == x_img], col = "hotpink") +
    annotate(geom = "text", x = df_crit_imgs_rgbvi$dist_crit[df_crit_imgs_rgbvi$image == x_img] + 0.5, y = 0.4, label = paste0("— ", df_crit_imgs_rgbvi$dist_crit[df_crit_imgs_rgbvi$image == x_img], " m"), size = 5, fontface = "bold") +
    scale_x_continuous(limits = c(0, 5)) +
    scale_y_continuous(limits = c(-0.1, 0.5), breaks = seq(-0.1, 0.5, 0.1)) +
    labs(title = paste0("Image: ", x_img), x = "Distance (m)", y = "RGBVI") +
    theme_bw()}

ggsave("./figures/lineplot_images.png",
       patchwork::wrap_plots(plot_imgs_rgbvi_list, ncol = 4),
       width = 25, height = 30, units = "in", dpi = 300, bg = "white", limitsize = F)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Dataframe: global-level VI profile (only increases) ####

vals_critdist_inc_imgs = df_crit_imgs_rgbvi %>% filter(dist_crit > 0) %>% select(dist_crit) %>% 
  summarize(mean = mean(dist_crit, na.rm = T), sd = sd(dist_crit), n = n(), se = sd/sqrt(n), lci = mean - 1.96*se, uci = mean+1.96*se) %>% as.data.frame()

# save(vals_critdist_inc_imgs, file = "./files/vals_critdist_inc_imgs.RData", envir = .GlobalEnv); Sys.sleep(0.5)


df_imgs_inc_rgbvi = df_imgs_rgbvi %>% filter(image %in% df_crit_imgs_rgbvi$image[df_crit_imgs_rgbvi$case == "increase"])

# save(df_imgs_inc_rgbvi, file = "./files/df_imgs_inc_rgbvi.RData", envir = .GlobalEnv); Sys.sleep(0.5)

df_glob_inc_rgbvi = df_imgs_inc_rgbvi %>%
  group_by(dist_bin) %>%
  rename(mean_img = mean, median_img = median) %>% 
  summarize(
    mean = mean(mean_img, na.rm = T),
    sd = sd(mean_img, na.rm = T),
    median = median(median_img, na.rm = T),
    q25 = quantile(median_img, 0.25, na.rm = T),
    q75 = quantile(median_img, 0.75, na.rm = T),
    n = n(),
    se = sd / sqrt(n),
    lci = mean - 1.96 * se,
    uci = mean + 1.96 * se,
    .groups = "drop") %>%
  mutate(roll = zoo::rollmean(mean, k = 20, fill = NA, align = "center"))

# save(df_glob_inc_rgbvi, file = "./files/df_glob_inc_rgbvi.RData", envir = .GlobalEnv); Sys.sleep(0.5)

# plot_glob_inc_rgbvi =
  ggplot(df_glob_inc_rgbvi, aes(x = dist_bin)) + 
  geom_ribbon(aes(ymin = lci, ymax = uci), fill = "lightseagreen", alpha = 0.25, na.rm = T) +
  geom_line(aes(y = mean), linewidth = 3, col = "lightseagreen") +
  geom_hline(yintercept = vals_control_rgbvi$mean, lty = 1, lwd = 1.5) +
  geom_hline(yintercept = vals_control_rgbvi$lci, lty = 2, lwd = 0.75) +
  geom_hline(yintercept = vals_control_rgbvi$uci, lty = 2, lwd = 0.75) +
  geom_vline(xintercept = vals_critdist_inc_imgs$mean, lwd = 2, col = "hotpink") +
  labs(title = "(a)", x = "Distance (m)", y = "RGBVI") +
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, 0.05)) +
  annotate(geom = "text", x = vals_critdist_inc_imgs$mean + 0.75, y = 0.15, label = paste0("— ", round(vals_critdist_inc_imgs$mean, 1), " m ± ", round(vals_critdist_inc_imgs$se, 1)), size = 10, fontface = "bold") +  
  # theme_custom()
  theme_bw()

x_rgb = jpeg::readJPEG("./drone_photos/muskx2102.jpg") 
x_img_i = "muskx2102"; x_msrmts_i = df_msrmts[df_msrmts$image == x_img_i, ]
x_hex_matrix <- apply(x_rgb, 1:2, function(v) rgb(v[1], v[2], v[3])); Sys.sleep(0.5)
x_df_img <- reshape2::melt(x_hex_matrix)
colnames(x_df_img) <- c("y", "x", "hex_color")
x_theta = seq(0, 2*pi, length.out = 200)
x_radius_px = vals_critdist_inc_imgs$mean / x_msrmts_i$mtopix
x_df_circle = data.frame(
  x = df_msrmts$centroid_x[df_msrmts$image == x_img_i] + vals_critdist_inc_imgs$mean / df_msrmts$mtopix[df_msrmts$image == x_img_i] * cos(x_theta),
  y = -(df_msrmts$centroid_y[df_msrmts$image == x_img_i] + vals_critdist_inc_imgs$mean / df_msrmts$mtopix[df_msrmts$image == x_img_i] * sin(x_theta)))

plot_example_circle_inc =
  ggplot(x_df_img, aes(x = x, y = -y, fill = hex_color)) + 
  geom_raster() + 
  scale_fill_identity() + 
  geom_path(data = x_df_circle, aes(x = x, y = y), inherit.aes = F, color = "hotpink", linewidth = 2) +  coord_equal() + 
  labs(title = "(b)", x = "", y = "") +
  theme_custom_void()

ggsave("./figures/lineplot_global_inc.png", 
       cowplot::plot_grid(
         cowplot::plot_grid(NULL,plot_glob_inc_rgbvi,NULL, nrow = 3, rel_heights = c(0.075, 1, 0.05)), 
         plot_example_circle_inc, nrow = 1),
       width = 30, height = 15, units = "in", dpi = 300, bg = "white", limitsize = F)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_crit_imgs_rgbvi %>% pull(crcss_size) %>% table()

# plot_rgbvi_crcss_size1 =
ggplot(filter(df_crit_imgs_rgbvi, !is.na(crcss_size) & !is.na(case)), aes(crcss_size, fill = case)) +
  geom_bar(color = "black", position = position_dodge(width = 0.9), width = 0.75, lwd = 2, alpha = 0.75) +
  labs(x = "Carcass Size", y = "No. of Cases", col = "Greenness", fill = "Greenness", title = "(a)") +
  scale_fill_manual(values = c("white", "grey20"), labels = c("No Increase", "Increase")) +  
  scale_x_discrete(labels = c("1" = "Small\n(n = 7)", "2" = "Large\n(n = 18)")) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
  # theme_custom() + theme(legend.position = "none")
  theme_bw() + theme(legend.position = "left")

df_crit_imgs_rgbvi %>% pull(hab_moist) %>% table()

plot_rgbvi_hab_moist1 =
ggplot(filter(df_crit_imgs_rgbvi, !is.na(hab_moist) & !is.na(case)), aes(hab_moist, fill = case)) +
  geom_bar(color = "black", position = position_dodge(width = 0.9), width = 0.75, lwd = 2, alpha = 0.75) +
  labs(x = "Habitat Moisture Class", y = "No. of Cases", col = "Greenness", fill = "Greenness", title = "(b)") +
  scale_fill_manual(values = c("white", "grey20"), labels = c("No Increase", "Increase")) + 
  scale_x_discrete(labels = c("1" = "Dry \n(n = 5)", "2" = "Medium \n(n = 13)", "3" = "Wet \n(n = 7)")) +
  scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, 2)) +
  theme_custom() + theme(legend.position = "none")
  # theme_bw() + theme(legend.position = "none")

df_crit_imgs_rgbvi %>% filter(case == "increase") %>% pull(crcss_size) %>% table()

plot_rgbvi_crcss_size2 =
ggplot(filter(df_crit_imgs_rgbvi %>% filter(case == "increase"), !is.na(crcss_size)), aes(crcss_size, dist_crit)) +
  geom_boxplot(col = "black", fill = "grey80", alpha = 0.75, lwd = 2) +
  labs(x = "Carcass Size", y = "Distance (m)", title = "(c)") +
  scale_x_discrete(labels = c("1" = "Small \n(n = 5)", "2" = "Large \n(n = 9)")) +
  scale_y_continuous(limits = c(0, 3.5), breaks = seq(0, 3, 1)) +
  theme_custom()
  # theme_bw()

df_crit_imgs_rgbvi %>% filter(case == "increase") %>% pull(hab_moist) %>% table()
  
plot_rgbvi_hab_moist2 =
ggplot(filter(df_crit_imgs_rgbvi %>% filter(case == "increase"), !is.na(hab_moist)), aes(hab_moist, dist_crit)) +
  geom_boxplot(col = "black", fill = "grey80", alpha = 0.75, lwd = 2) +
  labs(x = "Habitat Moisture Class", y = "Distance (m)", title = "(d)") +
  scale_x_discrete(labels = c("1" = "Dry \n(n = 3)", "2" = "Medium \n(n = 6)", "3" = "Wet \n(n = 5)")) +
  scale_y_continuous(limits = c(0, 3.5), breaks = seq(0, 3, 1)) +
  theme_custom()
  # theme_bw()


ggsave("./figures/summary_rgbvi.png", cowplot::plot_grid(
  plot_rgbvi_crcss_size1, plot_rgbvi_hab_moist1,
  plot_rgbvi_crcss_size2, plot_rgbvi_hab_moist2, ncol = 2
), width = 30, height = 25, units = "in", dpi = 300, bg = "white", limitsize = F)