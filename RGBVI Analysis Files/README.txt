# Assessing distance of greenness from muskox carcasses images

The main purpose of this project component was to identify the distance of greenness, measured here by the RGBVI, associated with muskox carcasses. 

# Folder Structure

├── drone_photos/            # Drone images (contains JPG files)
├───── cor_brightness       # Drone images with radiometric calibration (contains TIF files)
├──────── rgbvi            # RGBVI images derived from the drone images with radiometric calibration (contains TIF files)
├── scripts/                 # R scripts necessary to generate the data (contains R files)
├───── study.R              # Main R code.
├───── theme_custom.R       # Settings for figure output in R. 
└── README.txt                # This documentation file

# Prerequisites

R and RStudio are necessary for running the R code. The following R packages are required: "beepr","tidyverse","progress","raster","sf","terra","viridis"
