library(tidyverse)
library(sf)

reefs <- readRDS("data/tabla/coordinates_of_rocky_reefs_consistently_monitored.RDS") |> 
  mutate(Reef = recode(Reef,
                       "BALLENA" = "ESPIRITU_SANTO_BALLENA",
                       "ISLOTES_ESTE" ="ESPIRITU_SANTO_ISLOTES_ESTE",
                       "ISLOTES_NORTE" = "ESPIRITU_SANTO_ISLOTES_NORTE",
                       "PAILEBOTE" ="ESPIRITU_SANTO_PAILEBOTE",
                       "PARTIDA_NORESTE" = "ESPIRITU_SANTO_PARTIDA_NORESTE",
                       "PUNTA_LOBOS" = "ESPIRITU_SANTO_PUNTA_LOBOS"))

# ltem.sites <- readxl::read_excel("../ltem-program/data/lists/updates/ltem_monitoring_reefs_2023-12-13.xlsx") |> 
ltem.sites <- readRDS("data/tabla/ltem_historic_updated_2024-01-15.RDS") |> 
  filter(Reef %in% reefs$Reef) |> 
  # select(Reef,Protection_status, Latitude, Longitude)
  distinct(Reef, Protection_status, Protection_level, Fishery)

unique(ltem.sites$Protection_status)

reefs.sf <- merge(reefs, ltem.sites, by=c("Reef","Latitude", "Longitude"), all.x = T) |> 
  st_as_sf(coords=c("Longitude","Latitude"), crs=4326)

st_write(reefs.sf, "outputs/ltem_sites_protection.shp")