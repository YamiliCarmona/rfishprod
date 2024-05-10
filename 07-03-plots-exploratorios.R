library(ggplot2)
library(dplyr)
library(gridExtra)

# DATA-------------

fishdata <- readRDS("data/tabla/fish_productivity_data.RDS")
alldata <- readRDS("data/fishdata_prod-by-reef-allyears_1000.RDS")
data2010 <- readRDS("data/fishdata_product-by-reef-2010.RDS")
data2023 <- readRDS("data/fishdata_product-by-reef-2023.RDS")

data <- data2023

unique(fishdata$commercial)

data$Degree <- factor(data$Degree, levels = c(23, 24, 25, 26))


ggplot(data, aes(x = Degree, y = Prod, fill = Degree)) +
  geom_boxplot() +
  labs(title = " ", x = "Degree", y = "Productivity sum (g d^−1 ha^−1)") +
  theme_minimal() +
  theme(legend.position = " ")




# data_management---------------
biom75 = quantile(data$log10Biom,0.95)
biom25 = quantile(data$log10Biom,0.25)
prod75 = quantile(data$log10ProdB,0.75)
prod25 = quantile(data$log10ProdB,0.25)
max_biom = max(data$log10Biom)


#Diving data into 3 classes for each biomass/productivity relationship
management = data %>% 
  
  mutate(Class = ifelse(log10Biom < biom25 & log10ProdB < prod25,"deadzone",
                        ifelse(log10ProdB > prod75,"partial",
                               ifelse(log10Biom > biom75 & log10ProdB < prod75,"pristine","transition"))))%>%
  mutate(Class = as.factor(Class))

range(management$Productivity)
range(management$Biom)
range(management$Prod)


unique(management$Region)

unique_points <- management[!duplicated(management$Reef), ]

management2 <- management |> 
  # mutate(Protection_status = recode(Protection_status,
  #                                   "cabo pulmo" = "Fully protected",
  #                                   "sin proteccion" = "Not protected",
  #                                   "area protegida" = "Lightly protected")) |> 
  mutate(Class = recode(Class, "deadzone" = "Low biomass/turnover", 
                        "partial" = "High turnover", 
                        "pristine" = "High biomass", 
                        "transition" = "Mid-range"))


# Espíritu Santo
# Loreto
# CSL
# Corredor

unique(management2$Region)

# suma los datos por la columna Class y Protection_status
data_summarized <- management2 %>%
  group_by(Region, Class) %>%
  summarise(total_productivity = sum(Prod))

unique(data_summarized$Class)

data_summarized$Class <- factor(data_summarized$Class, 
                                levels = c("High biomass", "High turnover", "Mid-range", "Low biomass/turnover"))


group.colors <- setNames(
  c("#d69d4e", "#046c9a", "darkgrey", "#C1DB60"), 
  c("Low biomass/turnover", "High turnover", "Mid-range", "High biomass")
)




ggplot(data_summarized, aes(x = Class, y = total_productivity, fill = Class)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = group.colors) +
  labs(title = "All years",
       x = "Class",
       y = "Productivity",
       fill = " ") +
  theme_classic() +
  theme(legend.position = "none") +  # Para quitar la leyenda
  facet_wrap(~ Region, scales = "free", ncol = 2)  # Divide el gráfico por cada región



# ggsave("figs/data_class_status_portected_by_region.png", width = 12, height = 8, dpi=1000)


# ggsave("figs/alldata_class_status_portected_by_region.png",
# width = 12,     # Ancho en pulgadas
# height = 8,    # Altura en pulgadas
# dpi = 300)     # Resolución en píxeles por pulgada (dpi)
 

# Graficar productividad  y protección level -----------

data23 <- data |> 
  janitor::clean_names() %>%
  group_by(year, region, protection_level, reef,  depth2, transect)%>%
  summarise(biomass = sum(biom), product = sum(prod), turnover = sum(productivity))

ggplot(data23, aes(x = protection_level, y = product, fill = protection_level)) +
  geom_boxplot() +
  labs(title = "All years ", x = "Protection_level", y = "Productivity sum (g d^−1 ha^−1)") +
  theme_minimal() +
  theme(legend.position = " ")

# ggsave("figs/alldata_protection_level_prod.png", width = 8.5, height = 4.5, dpi=1000)

data23 <- data |> 
  janitor::clean_names() %>%
  group_by(year, region, protection_status, reef,  depth2, transect)%>%
  summarise(biomass = sum(biom), product = sum(prod), turnover = sum(productivity))


ggplot(data23, aes(x = protection_status, y = product, fill = protection_status)) +
  geom_boxplot() +
  labs(title = " ", x = "Protection_status", y = "Productivity (g d^−1 ha^−1)") +
  theme_minimal() +
  theme(legend.position = " ")

# ggsave("figs/data2023_Protection_status_prod.png", width = 8.5, height = 4.5, dpi=1000)

# niveles de Degree ordenados de sur a norte
data$Degree <- factor(data$Degree, levels = c(23, 24, 25, 26))


data23 <- data |> 
  janitor::clean_names() %>%
  group_by(year, degree, region, reef,  depth2, transect)%>%
  summarise(biomass = sum(biom), product = sum(prod), turnover = sum(productivity))

data23$degree <- factor(data23$degree, levels = c(23, 24, 25, 26))

ggplot(data23, aes(x = degree, y = product, fill = degree)) +
  geom_boxplot() +
  labs(title = " ", x = "Degree", y = "Productivity sum (g d^−1 ha^−1)") +
  theme_minimal() +
  theme(legend.position = " ")

# ggsave("figs/alldata_prod_degree.png", width = 8.5, height = 4.5, dpi=1000)


# Cabo Pulmo --------
# Antes y ahora
# Biom
# Prod
# Turnover

cp10 <- alldata |> 
  filter(Region == "Cabo Pulmo") 
  # janitor::clean_names() %>%
  # group_by(year, region, reef, depth2, transect)%>% 
  # summarise(biomass = sum(biom), product = sum(prod), turnover = sum(productivity))

cp23 <- data2023 |> 
  filter(Region == "Cabo Pulmo") 

cp10$Year <- factor(cp10$Year)
cp23$Year <- factor(cp23$Year)

plot1 <- ggplot(cp10, aes(x = Year, y = Biom, fill = Year)) +
  geom_boxplot() +
  labs(title = "Cabo Pulmo", x = " ", y = "Biomass (kg ha^−1)") +
  theme_minimal() +
  theme(legend.position = "none")

plot2 <- ggplot(cp10, aes(x = Year, y = Prod, fill = Year)) +
  geom_boxplot() +
  labs(title = " ", x = " ", y = "Productivity (g d^−1 ha^−1)") +
  theme_minimal() +
  theme(legend.position = "none")

plot3 <- ggplot(cp10, aes(x = Year, y = Productivity, fill = Year)) +
  geom_boxplot() +
  labs(title = " ", x = "Year", y = "Turnover (P/B × 100 % per day)") +
  theme_minimal() +
  theme(legend.position = "none")


plot4 <- ggplot(cp23, aes(x = Year, y = Biom, fill = Year)) +
  geom_boxplot() +
  labs(title = " ", x = " ", y = " ") +
  theme_minimal() +
  theme(legend.position = "none")

plot5 <- ggplot(cp23, aes(x = Year, y = Prod, fill = Year)) +
  geom_boxplot() +
  labs(title = " ", x = " ", y = " ") +
  theme_minimal() +
  theme(legend.position = "none")

plot6 <- ggplot(cp23, aes(x = Year, y = Productivity, fill = Year)) +
  geom_boxplot() +
  labs(title = " ", x = " ", y = " ") +
  theme_minimal() +
  theme(legend.position = "none")

# Mostrar las tres gráficas juntas
grid.arrange(plot1, plot2,  plot3,  ncol = 2)
grid.arrange(plot1, plot4, plot2, plot5, plot3, plot6, ncol = 1)

# Guardar 
# ggsave("figs/cabopulmo_plots_antes-ahora.png", arrangeGrob(plot1, plot4, plot2, plot5, plot3, plot6, ncol = 2), width = 10, height = 15, units = "in")
# ggsave("figs/cabopulmo_plots_allyears.png", arrangeGrob(plot1, plot2, plot3, ncol = 1), width = 10, height = 15, units = "in")






