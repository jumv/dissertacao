# Libraries ----

#install.load::install_load("sf", "geobr", "tidyr", "dplyr")
library(sf)
library(geobr)
library(tidyr)
library(dplyr)
# Options ----

options(scipen = 12)

# Directories ----

dirs <- paste0("data/", c("txt", "tar", "rdata", "csv"))

lapply(dirs, dir.create)

subdirs <- paste0("data/",  c("txt/", "tar/"), "udel")

lapply(subdirs, dir.create)


# Temperature -------------------------------------------------------------

# Terrestrial Air Temperature: 1900-2017 Gridded Monthly Time Series (1900 - 2017) (V 5.01 added 6/1/18)

## Download ----

url <- "http://climate.geog.udel.edu/~climate/html_pages/Global2017/air_temp_2017.tar.gz"

download.file(url = url, destfile = "data/tar/udel/temp_2017.tar.gz", method = "curl")

untar(tarfile = "data/tar/udel/temp_2017.tar.gz", 
      exdir   = "data/txt/udel/global_air_temp")    


## Prepare ----

# List files

files <- list.files(path = "data/txt/udel/global_air_temp/", full.names = TRUE)

# Read multiple files into list

temp <- lapply(files, function(i){read.table(i, header = FALSE)})

rm(files)


# List to table

temp_df <- data.frame(temp) # 1770 variables

rm(temp)

# Select columns (remove repeated coordinates column and year average columns)

temp_df <- temp_df %>% dplyr::select(-starts_with(c("V1.", "V2.", "V15"))) # 1418 variables


## Maps ----

# Download reference maps using geobr package

# View(list_geobr())

municipios <- read_municipality(simplified = FALSE)

# Validate geometries

unique(st_is_valid(municipios)) # TRUE FALSE

municipios <- st_make_valid(municipios)

unique(st_is_valid(municipios)) # TRUE

# Check for empty geometries

unique(st_is_empty(municipios))

# Save

save(municipios, file = "data/rdata/br_municipalities_IBGE_2010.RData")

gc()

# From table to sf object

temp_sf <- st_as_sf(temp_df,                    # data set
                    
                    crs = st_crs(municipios),   # define coordinate reference system equal to reference map
                    
                    coords = c("V1", "V2"),     # longitude and latitude
                    
                    dim = "XY",                 # indicate coordinates columns
                    
                    remove = TRUE,              # remove coordinates columns
                    
                    na.fail = TRUE,             # raise an error if coordinates contain missing values
                    
                    sf_column_name = "geometry")

# /!\ We could not find any indications of original CRS in the documentation for this data set

## Intersect ----

# Check CRS equivalence

all.equal(st_crs(municipios), st_crs(temp_sf)) # TRUE

# Transform CRS to projected CRS:  SAD69(96) / Brazil Polyconic

temp_sf    <- st_transform(temp_sf,    crs = 5530) 

municipios <- st_transform(municipios, crs = 5530)

gc()

# Crop to extent

temp_sf <- st_crop(temp_sf, sf::st_bbox(municipios))

# Intersection

temp_inters <- st_intersection(municipios, temp_sf)

# Unique intersected municipalities

length(unique(temp_inters$code_muni)) # 1472


## Buffers ----

# First buffer

# Select municipalities that do not intersected any points in first attempt

mun_diff <- municipios[!municipios$code_muni %in% 
                         
                         temp_inters$code_muni, ] # 4095 municípios

# /!\ The distance between two points is 55863 meters (0.5º) 

# Buffer of 27.9km (1/2 the distance between two points)

mun_buff_27km <- st_buffer(mun_diff, dist = 27931.5) 

# Intersection with buffer 27.9km

temp_inters_b27 <- st_intersection(temp_sf, mun_buff_27km)

# Municípios com um ou mais pontos de interseção

length(unique(temp_inters_b27$code_muni))

# /!\ 4024 municipalities intersect points with buffer 27.9km

# Second buffer       

# Select municipalities that do not intersected any points in first attempt

mun_diff <- municipios[!municipios$code_muni %in% 
                         
                         c(temp_inters$code_muni, temp_inters_b27$code_muni), ]

# /!\ 71 municipalities left

# Buffer of 55.8km (the distance between two points)

mun_buff_55km <- st_buffer(mun_diff, dist = 55863) 

# Intersection with buffer 27.9km

temp_inters_b55 <- st_intersection(temp_sf, mun_buff_55km)

# Municípios com um ou mais pontos de interseção

length(unique(temp_inters_b55$code_muni)) # 70


## Agreggate ----

# Remove geometries

st_geometry(temp_inters) <- NULL

st_geometry(temp_inters_b27) <- NULL

st_geometry(temp_inters_b55) <- NULL

# Aggregate for each iteration

temp_agg <- 
  
  temp_inters %>% 
  
  group_by(code_muni) %>% 
  
  summarise(across(where(is.numeric), 
                   
                   mean))  # average temperature 


temp_agg_27km <-
  
  temp_inters_b27 %>%
  
  group_by(code_muni) %>%
  
  summarise(across(where(is.numeric),
                   
                   mean))  # average temperature 


temp_agg_55km <-
  
  temp_inters_b55 %>%
  
  group_by(code_muni) %>%
  
  summarise(across(where(is.numeric),
                   
                   mean))  # average temperature 


## Join ----

# Bind tables by rows 

monthly_temp_br_mun <- rbind(temp_agg, temp_agg_27km, temp_agg_55km)

# As data frame

monthly_temp_br_mun <- data.frame(monthly_temp_br_mun)

# Confirm unique municipalities

length(unique(monthly_temp_br_mun$code_muni)) # 5566


## Panel ----

# Pivot (wide to long)

monthly_temp_br_mun_long <-
  
  pivot_longer(monthly_temp_br_mun, 
               
               cols = V3:V14.117,
               
               names_to = "data", # meses e anos da série
               
               values_to = "temp") # temperatura média


# Create vector of dates

auxs <- seq(from = as.Date("1900-01-01"), 
            to   = as.Date("2017-12-01"), by = "months") %>% 
  
  rep(times = 5566)  # number of unique municipalities

# Order data frame for matching

monthly_temp_br_mun_long <- monthly_temp_br_mun_long %>%
  
  arrange(code_muni, data) 

# Rename variables

monthly_temp_br_mun_long$data <- auxs

# Verify

length(unique(monthly_temp_br_mun_long$code_muni))

summary(monthly_temp_br_mun_long$temp)

## Export ----

save(monthly_temp_br_mun_long, 
     
     file = "data/rdata/udel_monthly_avg_air_temp_br_mun_long_buffer.RData")

write.csv2(monthly_temp_br_mun_long, 
           
           file = "data/csv/udel_monthly_avg_air_temp_br_mun_long_buffer.csv")

rm(list = ls())

gc()


# Precipitation --------------------------------

# Terrestrial Precipitation: 1900-2017 Gridded Monthly Time Series (V 5.01)

## Download ----

url <- "http://climate.geog.udel.edu/~climate/html_pages/Global2017/precip_2017.tar.gz"

download.file(url = url, destfile = "data/tar/udel/precip_2017.tar.gz", method = "curl")

untar(tarfile = "data/tar/udel/precip_2017.tar.gz", 
      
      exdir   = "data/txt/udel/global_precip")    


## Prepare ----

# List files

files <- list.files(path = "data/txt/udel/global_precip/", full.names = TRUE)

# Read multiple files into list

precip <- lapply(files, function(i){read.table(i, header = FALSE)})

rm(files)


# List to table

precip_df <- data.frame(precip) # 1770 variables

rm(precip)

# Select columns (remove repeated coordinates column and year average columns)

precip_df <- precip_df %>% 
  
  dplyr::select(-starts_with(c("V1.", "V2.", "V15"))) # 1418 variables

# Load reference maps

load("data/rdata/br_municipalities_IBGE_2010.RData") # Malha Municipal 2018 IBGE

# From table to sf object

precip_sf <- st_as_sf(precip_df,                    # data set
                      
                      crs = st_crs(municipios),   # define coordinate reference system equal to reference map
                      
                      coords = c("V1", "V2"),     # longitude and latitude
                      
                      dim = "XY",                 # indicate coordinates columns
                      
                      remove = TRUE,              # remove coordinates columns
                      
                      na.fail = TRUE,             # raise an error if coordinates contain missing values
                      
                      sf_column_name = "geometry")

# /!\ We could not find any indications of original CRS in the documentation for this data set

## Intersect ----

# Check CRS equivalence

all.equal(st_crs(municipios), st_crs(precip_sf)) # TRUE

# Transform CRS to projected CRS:  SAD69(96) / Brazil Polyconic

precip_sf  <- st_transform(precip_sf, crs = 5530) 

municipios <- st_transform(municipios, crs = 5530)

gc()

# Crop to extent

precip_sf <- st_crop(precip_sf, sf::st_bbox(municipios))

# Intersection

precip_inters <- st_intersection(municipios, precip_sf)

# Unique intersected municipalities

length(unique(precip_inters$code_muni)) # 1472


## Buffers ----

# First buffer

# Select municipalities that do not intersected any points in first atprecipt

mun_diff <- municipios[!municipios$code_muni %in% 
                         
                         precip_inters$code_muni, ] # 4095 municípios

# /!\ The distance between two points is 55863 meters (0.5º) 

# Buffer of 27.9km (1/2 the distance between two points)

mun_buff_27km <- st_buffer(mun_diff, dist = 27931.5) 

# Intersection with buffer 27.9km

precip_inters_b27 <- st_intersection(precip_sf, mun_buff_27km)

# Municípios com um ou mais pontos de interseção

length(unique(precip_inters_b27$code_muni))

# /!\ 4024 municipalities intersect points with buffer 27.9km

# Second buffer       

# Select municipalities that do not intersected any points in first atprecipt

mun_diff <- municipios[!municipios$code_muni %in% 
                         
                         c(precip_inters$code_muni, precip_inters_b27$code_muni), ]

# /!\ 71 municipalities left

# Buffer of 55.8km (the distance between two points)

mun_buff_55km <- st_buffer(mun_diff, dist = 55863) 

# Intersection with buffer 27.9km

precip_inters_b55 <- st_intersection(precip_sf, mun_buff_55km)

# Municípios com um ou mais pontos de interseção

length(unique(precip_inters_b55$code_muni)) # 70


## Agreggate ----

# Remove geometries

st_geometry(precip_inters) <- NULL

st_geometry(precip_inters_b27) <- NULL

st_geometry(precip_inters_b55) <- NULL

# Aggregate for each iteration

precip_agg <- 
  
  precip_inters %>% 
  
  group_by(code_muni) %>% 
  
  summarise(across(where(is.numeric), 
                   
                   mean))  # average preciperature 


precip_agg_27km <-
  
  precip_inters_b27 %>%
  
  group_by(code_muni) %>%
  
  summarise(across(where(is.numeric),
                   
                   mean))  # average preciperature 


precip_agg_55km <-
  
  precip_inters_b55 %>%
  
  group_by(code_muni) %>%
  
  summarise(across(where(is.numeric),
                   
                   mean))  # average preciperature 


## Join ----

# Bind tables by rows 

monthly_precip_br_mun <- rbind(precip_agg, precip_agg_27km, precip_agg_55km)

# As data frame

monthly_precip_br_mun <- data.frame(monthly_precip_br_mun)

# Confirm unique municipalities

length(unique(monthly_precip_br_mun$code_muni)) # 5566


## Panel ----

# Pivot (wide to long)

monthly_precip_br_mun_long <-
  
  pivot_longer(monthly_precip_br_mun, 
               
               cols = V3:V14.117,
               
               names_to = "data", # meses e anos da série
               
               values_to = "precip") # temperatura média

# Create vector of dates

auxs <- seq(from = as.Date("1900-01-01"), 
            to   = as.Date("2017-12-01"), by = "months") %>% 
  
  rep(times = 5566)  # number of unique municipalities

# Order data frame for matching

monthly_precip_br_mun_long <- monthly_precip_br_mun_long %>%
  
  arrange(code_muni, data) 

# Rename variables

monthly_precip_br_mun_long$data <- auxs


# Verify

length(unique(monthly_precip_br_mun_long$code_muni))

summary(monthly_precip_br_mun_long$precip)


## Export ----

save(monthly_precip_br_mun_long, 
     
     file = "data/rdata/udel_monthly_avg_precip_br_mun_long_buffer.RData")

write.csv2(monthly_precip_br_mun_long, 
           
           file = "data/csv/udel_monthly_avg_precip_br_mun_long_buffer.csv")

rm(list = ls())

gc()