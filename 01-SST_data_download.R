
# The packages we will use
library(dplyr) # A staple for modern data management in R
library(lubridate) # Useful functions for dealing with dates
library(ggplot2) # The preferred library for data visualisation
library(tidync) # For easily dealing with NetCDF data
library(rerddap) # For easily downloading subsets of data
library(doParallel) # For parallel processing
library(heatwaveR) # For detecting MHWs



# The information for the NOAA OISST data
rerddap::info(datasetid = "ncdcOisst21Agg_LonPM180", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

# Note that there is also a version with lon values from 0 yo 360
rerddap::info(datasetid = "ncdcOisst21Agg", url = "https://coastwatch.pfeg.noaa.gov/erddap/")


# This function downloads and prepares data based on user provided start and end dates
OISST_sub_dl <- function(time_df){
        OISST_dat <- griddap(datasetx = "ncdcOisst21Agg_LonPM180", 
                             url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                             time = c(time_df$start, time_df$end), 
                             zlev = c(0, 0),
                             
                             # Aquí cambias las coordenadas para la región del 
                             # Golfo de California 
                             latitude = c(22, 27),
                             longitude = c(-112, -109),
                             
                             
                             fields = "sst")$data %>% 
                mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
                dplyr::rename(t = time, temp = sst) %>% 
                #select(lon, lat, t, temp) %>% 
                na.omit()
}



# Date download range by start and end dates per year
# 
# la descarga va por intervalo de fechas, las puedes modificar si no quieres datos desde
# el '82
# dl_years <- data.frame(date_index = 1:5,
#                        start =as.character( as.Date(c("1982-01-01", "1990-01-01", 
#                                          "1998-01-01", "2006-01-01", "2014-01-01"))),
#                        end = as.character(as.Date(c("1989-12-31", "1997-12-31", 
#                                        "2005-12-31", "2013-12-31", "2022-09-30")))
# )

# Create a sequence of years from 1998 to 2023
years <- seq(from = 1998, to = 2023, by = 1)

# Create a data frame with start and end dates for each year
dl_years <- data.frame(date_index = 1:length(years),
                       start = as.character(as.Date(paste(years, "-01-01", sep = ""))),
                       end = as.character(as.Date(paste(years, "-12-31", sep = "")))
)

# Print the data frame
print(dl_years)

# Download all of the data with one nested request
# The time this takes will vary greatly based on connection speed
system.time(
        OISST_data <- dl_years %>% 
                group_by(date_index) %>% 
                group_modify(~OISST_sub_dl(.x)) %>% 
                ungroup() 
) # 636 seconds, ~127 seconds per batch

# OISST_data %>%
#         filter(t == "2023-8-01") %>%
#         ggplot(aes(x = longitude, y = latitude)) +
#         geom_tile(aes(fill = temp)) +
#         # borders() + # Activate this line to see the global map
#         scale_fill_viridis_c() +
#         coord_quickmap(expand = F) +
#         labs(x = NULL, y = NULL, fill = "SST (°C)") +
#         theme(legend.position = "bottom")
# 
# 
# # Save the data as an .Rds file because it has a much better compression rate than .RData

dir.create("data/")
saveRDS(OISST_data, "data/OISST_GoC.RDS")

