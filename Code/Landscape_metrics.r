#Running landscape metrics on interpolated tracks

library(mlr)
library(tidyverse)
library(randomForest)
library(raster)
library(landscapemetrics)
library(future)
library(furrr)
library(sf)
library(terra)

#start with Rankin
#load in data

dat_rank <- read.csv('Data/rankin_hmm_dat_30.csv')

coord <- as.data.frame(cbind(dat_rank$x, dat_rank$y))
head(coord)
coord_sf <- st_as_sf(coord, coords = c("V1", "V2"), crs = "+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs")

rmap1 <- rast('Data/Rankin_map.tif')
crs(rmap1)
crs(coord_sf)
if (st_crs(coord_sf) != st_crs(rmap1)) {
  coord_sf <- st_transform(coord_sf, crs = st_crs(rmap1))
}
plot(rmap1)
# Get extent of the raster as an sf polygon
raster_bbox <- as.polygons(rmap1) |> st_as_sf()
crs(raster_bbox)
str(raster_bbox)
raster_bbox$savclass_2 <- 1  # assign same class
merged <- st_union(raster_bbox)
plot(merged)
crs(merged)
# Subset points inside the raster extent
coord_sf_in <- coord_sf[st_within(coord_sf, merged, sparse = FALSE), ]

plot(rmap1)
points(coord_sf_in)

# Apply the function

library(furrr)
plan(multisession)  # Use multisession to run on multiple cores
library(purrr)
coord[i,]
result_met <- 1:nrow(coord_sf_in) %>% future_map(
  function(i) {
    rmap_r <- rast("Data/Rankin_map.tif")
    sample_lsm(
      rmap_r,
      y = st_geometry(coord_sf_in[i, ]),  # Ensure geometry is passed
      size = 15,
      what = c(
        "lsm_c_pd", "lsm_c_ed",
        "lsm_c_division",
        "lsm_c_pland", "lsm_c_shape_mn"
      ),
      shape = "circle"
    )
  }
)
plan(sequential)
# coord_sf[i, ] %>% class()
# ?future_map
# 
# future::plan(multisession, workers = 9)
# result_met <- furrr::future_map(1:nrow(coor), function(i)(sample_lsm(map_dense, y = coor[i, ], size = 5, what = c("lsm_c_pd", "lsm_c_ed", 'lsm_c_area_mn', 'lsm_c_ai', 'lsm_c_para_mn', 'lsm_c_gyrate_mn', 'lsm_c_division', 'lsm_c_pland', 'lsm_c_shape_mn'), shape = 'circle')))
# future::plan(sequential)
# 
rt <-lapply(result_met, as.data.frame)

rt<- lapply(rt, function(x) if (is.null(x)) data.frame() else x)

all_columns <- unique(unlist(lapply(rt, colnames)))
rt <- lapply(rt, function(df) {
  df[setdiff(all_columns, colnames(df))] <- NA  # Add missing columns with NA
  df <- df[, all_columns]  # Reorder columns
  df
})

problematic <- which(sapply(rt, function(df) !inherits(try(bind_rows(df), silent = TRUE), "data.frame")))
## or
## is.na(Result) <- !lengths(Result)


met <- bind_rows(rt, .id = 'source')

met_slice <- met %>% slice(1:1000)

unique_sources <- unique(met$source)
unique_classes <- unique(met$class)
unique_metric <- unique(met$metric)



# Create a complete grid of all combinations of `source` and `class`
full_combinations <- expand.grid(source = unique_sources, class = unique_classes, metric = unique_metric)

# Perform a left join with the original dataset to add NAs for missing combinations
mettry <- full_combinations %>%
  left_join(met, by = c("source", "class", 'metric')) %>% distinct()

mettry1 <- mettry %>% dplyr::filter(class == 1) %>% pivot_wider(names_from = metric, values_from = value)


dat_rank_s <- st_as_sf(dat_rank, coords = c("x", "y"), crs = "+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs")

if (st_crs(dat_rank_s) != st_crs(rmap1)) {
  dat_rank_s <- st_transform(dat_rank_s, crs = st_crs(rmap1))
}

dat_rank_s1 <- dat_rank_s[st_within(dat_rank_s, merged, sparse = FALSE), ]
dat_all <- as.data.frame(dat_rank_s1)
names(mettry1)

metac <- mettry1 %>% dplyr::select(c(source, division, ed, pd, pland, shape_mn))

dat_all1 <- cbind(dat_all, metac)

classif <- extract(rmap1, coord_sf_in)
classif <- classif %>% dplyr::select(-ID)
dat_all2 <- cbind(dat_all1, classif)

names(dat_all1)
write.csv(dat_all, file = 'E:/FIU/PostDoc/CESI/Final_report/YAPS/johndat_yaps.csv')