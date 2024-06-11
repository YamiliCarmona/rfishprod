library(ade4)
library(adespatial)
library(vegan)
library(gclus)
library(cluster)
library(pvclust)
library(RColorBrewer)
library(labdsv)
library(rioja)
library(indicspecies)
library(mvpart)
library(MVPARTwrap)
library(dendextend)
library(vegclust)
library(colorspace)
library(agricolae)
library(picante)


# cargar los datos
alldata <- readRDS("data/fishdata_prod-by-reef-allyears-sin-H-guentherii.RDS")
matriz <- readRDS("data/tabla/reef_matrix_v1-2024-03-27.RDS")


print(paste("Minimum biomass:", min(data$biom)))
print(paste("Maximum biomass:", max(data$biom)))
print(paste("Minimum production:", min(data$prod)))
print(paste("Maximum production:", max(data$prod)))
print(paste("Minimum turnover:", min(data$productivity)))
print(paste("Maximum turnover:", max(data$productivity)))

# data_management---------------
biom75 = quantile(data$log10biom, 0.95)
biom25 = quantile(data$log10biom, 0.25)
prod75 = quantile(data$log10prodb, 0.75)
prod25 = quantile(data$log10prodb, 0.25)
max_biom = max(data$log10biom)

str(data)
#Diving data into 3 classes for each biomass/productivity relationship
management = data %>% 
  
  mutate(class = ifelse(log10biom < biom25 & log10prod_b < prod25, "deadzone",
                        ifelse(log10prod_b > prod75, "partial",
                               ifelse(log10biom > biom75 & log10prod_b < prod75, "pristine", "transition")))) %>%
  mutate(class = as.factor(class))

range(management$productivity)
range(management$biom)
range(management$prod)


unique(management$region)
# plot by class--------------
#eje y = Biomass turnover, P/B × 100 (% per day)
# eje x <- log[standing biomass (g m–2)]
# filll = Class (Low biomass/turnover High turnover High biomass Mid-range)
# Class=c("deadzone"="Low biomass/turnover","partial"="High turnover","pristine"="High biomass", "transition" = "transition")


unique_points <- management[!duplicated(management$reef), ]

# ggplot(management, aes(x = log10biom, y = log10prodb, fill = Class)) +
#   geom_point(shape = 21, size = 3, alpha = 0.7) +
#   scale_fill_manual(values = c("deadzone" = "#ff9900", "partial" = "#3366cc", "pristine" = "#33cc33", "transition" = "#9900cc"),
#                     labels = c("deadzone" = "Low biomass/turnover", "partial" = "High turnover", "pristine" = "High biomass", "transition" = "Transition")) +
#   labs(x = "log(standing biomass (g m^-2))", y = "Biomass Turnover (P/B × 100 % per day)") +
#   theme_minimal() +
#   ggrepel::geom_text_repel(data = unique_points, aes(label = reef), box.padding = 0.5, point.padding = 0.5, size = 1.5, max.overlaps = 300)



# renombrar la columna 'prod' de 'matri' para evitar conflictos al fusionar los datos
names(matri)[which(names(matri) == "prod")] <- "prod_matri"

# fusionar los datos de 'alldata' y 'matri' por las columnas 'reef' y 'year', omitiendo la variable 'prod'
data <- merge(data, matri, by = c("reef", "year"), all.x = TRUE)

# eliminar la variable 'prod_matri' después de la fusión
data <- subset(data, select = -prod_matri)

# Renombrar la columna 'protection.x' a 'protection'
names(data)[which(names(data) == "protection.x")] <- "protection"

# Reemplazar valores NA en 'protection' con los valores de 'protection.y'
data$protection[is.na(data$protection)] <- data$protection.y

# Eliminar la columna 'protection.y' después de la fusión
data <- subset(data, select = -protection.y)


# definir los umbrales para dividir tus datos en clases de gestión
s_sup_biom <- 0.95
s_inf_biom <- 0.25
s_sup_prod <- 0.95
s_inf_prod <- 0.25


# función para crear las clases de gestión
data_management <- function(data, s_sup_biom, s_inf_biom, s_sup_prod, s_inf_prod) {
  biom75 <- quantile(data$log10biom, s_sup_biom)
  biom25 <- quantile(data$log10biom, s_inf_biom)
  prod75 <- quantile(data$log10prodb, s_sup_prod)
  prod25 <- quantile(data$log10prodb, s_inf_prod)
  
  management <- data %>%
    mutate(class = ifelse(log10biom < biom25 & log10prod_b < prod25, "deadzone",
                          ifelse(log10prod_b > prod75, "partial",
                                 ifelse(log10biom > biom75 & log10prod_b < prod75, "pristine", "transition")))) %>%
    mutate(class = as.factor(class))
  
  return(management)
}

model_prob <- function(data, resultados_modelo) {
  unique_points <- data[!duplicated(data$reef), ]
  
  var_probs <- pbmclapply(rownames(var_imp), function(i) {
    pd <- NULL
    plot_list <- list()
    for (p in 1:4) {
      tmp <- pdp::partial(resultados_modelo, pred.var = c(i), which.class = p, grid.resolution = 101 ,n.trees=1000, type = "classification", prob = TRUE)
      pd <- rbind(pd, cbind(tmp, class = levels(data$class)[p]))
    }
    
    pristine <- pd %>%
      filter(class == "pristine") %>%
      ggplot(aes(eval(parse(text = paste(i))), yhat)) +
      geom_point(colour = "#c1db60", size = 3, alpha = 0.7) +
      stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1) +
      theme_bw() +
      labs(x = var_imp[i, 3], y = "") +
      guides(fill = FALSE)
    
    partial <- pd %>%
      filter(class == "partial") %>%
      ggplot(aes(eval(parse(text = paste(i))), yhat)) +
      geom_point(colour = "#046c9a", size = 3, alpha = 0.7) +
      geom_smooth(se = FALSE) +
      theme_bw() +
      labs(x = var_imp[i, 3], y = "") +
      guides(fill = FALSE)
    
    deadzone <- pd %>%
      filter(class == "deadzone") %>%
      ggplot(aes(eval(parse(text = paste(i))), yhat)) +
      geom_point(colour = "#d69d4e", size = 3, alpha = 0.7) +
      geom_smooth(se = FALSE) +
      theme_bw() +
      labs(x = var_imp[i, 3], y = "") +
      guides(fill = FALSE)
    
    transition <- pd %>%
      filter(class == "transition") %>%
      ggplot(aes(eval(parse(text = paste(i))), yhat)) +
      geom_point(colour = "lightblue", size = 3, alpha = 0.7) +
      geom_smooth(se = FALSE) +
      theme_bw() +
      labs(x = var_imp[i, 3], y = "") +
      guides(fill = FALSE)
    
    plots <- ggarrange(pristine, partial, deadzone, transition, ncol = 1, nrow = 4)
    
    plot_list[[i]] <- plots
    
    return(plot_list)
  }, mc.cores = 6)
  
  var_probs_flat <- var_probs %>% flatten()
  
  setwd(here())
  
  save(var_probs_flat, file = "outputs/var_probs_flat.RData")
  
  load("outputs/var_probs_flat.RData")
  
  # for each group of covariates, three plots 
  plot1 <- var_probs_flat[1:3]
  plot2 <- var_probs_flat[4:6]
  plot3 <- var_probs_flat[7:9]
  plot4 <- var_probs_flat[10:12]
  
  bigplot <- plot_grid(plotlist = plot1, ncol = 3)
  
  ggsave("figures/figure4_1.pdf", height = 210, width = 297, units = "mm")
  ggsave("figures/figure4_1.png", height = 210, width = 297, units = "mm")
  
  bigplot2 <- plot_grid(plotlist = plot2, ncol = 3)
  
  ggsave("figures/figure4_2.pdf", height = 210, width = 297, units = "mm")
  ggsave("figures/figure4_2.png", height = 210, width = 297, units = "mm")
  
  bigplot3 <- plot_grid(plotlist = plot3, ncol = 3)
  
  ggsave("figures/figure4_3.pdf", height = 210, width = 297, units = "mm")
  ggsave("figures/figure4_3.png", height = 210, width = 297, units = "mm")
  
  bigplot4 <- plot_grid(plotlist = plot4, ncol = 3)
  
  ggsave("figures/figure4_4.pdf", height = 210, width = 297, units = "mm")
}


# seleccionar las variables relevantes para el análisis
data_cov <- matri %>%
  select(reef, prod_matri, mean_sst, mean_chla, human_gravity, protection, year, social_defi, edu_lag)




# llamar a la función data_management para crear las clases de gestión
data_gestion <- data_management(management, s_sup_biom, s_inf_biom, s_sup_prod, s_inf_prod)

# llamar a la función model_prob para visualizar la relación entre covariables y clases de gestión
model_prob(data_gestion, resultados_modelo)

# paso 3: modelado de clases de gestión
# selección de variables no correlacionadas
correlation_matrix <- cor(variables)
highly_correlated <- findCorrelation(correlation_matrix, cutoff = 0.7)
uncorrelated_variables <- variables[, -highly_correlated]

# transformación logarítmica
log_variables <- log(uncorrelated_variables + 1)  # sumamos 1 para evitar log(0)

# modelo de clasificación de bosques aleatorios
library(ranger)
model <- ranger(class ~ ., data = log_variables, num.trees = 1000, mtry = 3)

# evaluación del modelo
cross_val <- ranger::ranger.cv(data = log_variables, num.trees = 1000, mtry = 3, seed = 42)
print(cross_val)

# importancia de las variables
importance <- importance(model)
print(importance)


# seleccionar las variables relevantes para el análisis
data_cov <- matri %>%
  select(reef, prod_matri, mean_sst, mean_chla, human_gravity, protection, year, social_defi, edu_lag)



# llamar a la función data_management para crear las clases de gestión
data_gestion <- data_management(management, s_sup_biom, s_inf_biom, s_sup_prod, s_inf_prod)

# llamar a la función model_prob para visualizar la relación entre covariables y clases de gestión
model_prob(data_gestion, resultados_modelo)



# paso 3: modelado de clases de gestión
# selección de variables no correlacionadas
# (este es un ejemplo simplificado, asegúrate de aplicar la lógica adecuada)
correlation_matrix <- cor(variables)
highly_correlated <- findCorrelation(correlation_matrix, cutoff = 0.7)
uncorrelated_variables <- variables[, -highly_correlated]

# transformación logarítmica
log_variables <- log(uncorrelated_variables + 1)  # sumamos 1 para evitar log(0)

# modelo de clasificación de bosques aleatorios
library(ranger)
model <- ranger(class ~ ., data = log_variables, num.trees = 1000, mtry = 3)

# evaluación del modelo
cross_val <- ranger::ranger.cv(data = log_variables, num.trees = 1000, mtry = 3, seed = 42)
print(cross_val)

# importancia de las variables
importance <- importance(model)
print(importance)
