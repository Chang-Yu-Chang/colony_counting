# Count PFU

library(tidyverse)
library(broom)
library(EBImage)

# Read an image
img <- readImage(here::here("data/raw/test3.jpeg"))
display(img, method = "raster", all = T)

# Grey scale
colorMode(img) = Grayscale
display(img, method = "raster", all = T)
img <- img[,,3]
display(img, method = "raster")

# Blur
img_bl <- gblur(img, sigma = 1)
display(img_bl, method = "raster")

# Global threshold
threshold <- otsu(img_bl)
img_th <- combine(mapply(function(frame, th) frame > th, getFrames(img), threshold, SIMPLIFY=FALSE))
display(img_th, method = "raster")
img_label = bwlabel(img_th)
display(img_label, method = "raster")

# Remove background
temp <- img_label %>% table()
index_rm <- names(temp)[which(temp <= 10)] # Pixel size for gating out background
img_clean <- rmObjects(img_label, index_rm)
display(img_clean, method = "raster")

# Watershed
img_ws <- watershed(distmap(img_clean), 1)
display(colorLabels(img_ws), method = "raster")

# Shape of objects
colony_shape <- computeFeatures.shape(img_ws) %>% as_tibble()

# Relative count by clustering on the shape and size of the colonies
colony_shape %>%
    as.matrix() %>%
    kmeans(2) %>%
    broom::tidy() %>%
    pull(size)










