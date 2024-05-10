library(ggplot2)
library(dplyr)
library(gridExtra)

# DATA-------------

fishdata <- readRDS("data/tabla/fish_productivity_data.RDS")
alldata <- readRDS("data/fishdata_prod-by-reef-allyears_1000.RDS")
matriz <- readRDS("data/tabla/reef_matrix_v1-2024-03-27.RDS")

# Generar la matriz de productividad y variables ambientales y humanas

