# install.packages("raster")


# Load required libraries
library(raster)
library(ggplot2)
library(rasterVis)
library(dplyr)
library(sp)



sitios <- readRDS("data/tabla/coordinates_of_rocky_reefs_consistently_monitored.RDS")

print(sitios, n = 43)

sst <- readRDS("data/OISST_GoC.RDS")

str(sst)


# Convertir la columna 't' a año para agrupar
sst_resumen <- sst %>%
  mutate(year = lubridate::year(t)) %>%
  group_by(latitude, longitude, year) %>%
  summarise(sst_promedio = mean(temp, na.rm = TRUE))

print(sst_resumen)



# Crear un objeto raster -----------------------

str(sst_raster)
# Inicializar una lista para almacenar los resultados
resultados_list <- list()

# Iterar sobre años y realizar el análisis para cada año
for (year in unique(sst_resumen$year)) {
  
  # Subconjunto de datos sst_resumen para el año actual
  sst_resumen_year <- subset(sst_resumen, year == year)
  
  # Crear un objeto raster a partir de los datos resumidos de SST
  sst_raster_year <- rasterFromXYZ(sst_resumen_year[, c("longitude", "latitude", "sst_promedio")])
  
  # Guardar el raster (ajusta la ruta y el nombre del archivo según tus necesidades)
  writeRaster(sst_raster_year, paste0("sst_raster_", year, ".tif"), format = "GTiff", overwrite = TRUE)
  
  # Almacenar el resultado en la lista
  resultados_list[[as.character(year)]] <- sst_raster_year
  
}

# Combinar todos los resultados en un solo RasterBrick
sst_raster_list <- brick(resultados_list)

# Mostrar el resumen del RasterBrick
print(sst_raster_list)

str(sitios)

# Extraer los valores de los puntos de monitoreo ----------------------
sst_points <- extract(sst_raster_list, sitios[, c("Longitude", "Latitude")])

# Agregar información de año a la tabla resultante
sst_points_resumen <- cbind(sitios, year = rep(colnames(sst_points), each = nrow(sitios)), values = as.vector(sst_points))

# Mostrar el resumen de los valores de SST en los puntos de monitoreo--------------------
print(sst_points_resumen)

# Exportar el objeto sst_points_resumen en formato RDS--------
saveRDS(sst_points_resumen, "sst_points_resumen.rds")






