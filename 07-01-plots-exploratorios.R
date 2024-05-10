

alldata <- readRDS("data/fish_datagr_prod-by-species-allyears.RDS") |> 
  janitor::clean_names() %>%
  # filter(region %in% c("Los Cabos", "Cabo Pulmo"), year > 2007) %>%
  group_by(year, region, protection_level, reef, diet, family, depth2, transect, habitat)%>%
  summarise(biomass = sum(biom), product = sum(prod), turnover = sum(productivity))

glimpse(alldata)

p1 <- alldata %>%
  ggplot(aes(x=year, y=biomass, col = region)) +
  # geom_point() +
  geom_smooth(method = "gam", method.args = list(Gamma(link = "log")))+
  facet_grid(region~. , scales = "free_y") +
  theme_bw() +
  theme(legend.position = " ")

p2 <- alldata %>%
  ggplot(aes(x=year, y=product, col = region)) +
  # geom_point() +
  geom_smooth(method = "gam", method.args = list(Gamma(link = "log")))+
  facet_grid(region~. , scales = "free_y") +
  theme_bw() +
  theme(legend.position = " ")

p3 <- alldata %>%
  ggplot(aes(x=year, y=turnover, col = region)) +
  # geom_point() +
  geom_smooth(method = "gam", method.args = list(Gamma(link = "log")))+
  facet_grid(region~. , scales = "free_y") +
  theme_bw() +
  theme(legend.position = " ")

library(patchwork)

p1 +p2 + p3

alldata %>%
  ggplot(aes(x=log1p(year), y=log1p(turnover), col = region)) +
  # geom_point() +
  geom_smooth(method = "gam", method.args = list(Gamma(link = "log")))+
  facet_grid(region~. , scales = "free_y") +
  theme_bw() +
  theme(legend.position = " ")


# buffer -------------------

alldata %>%
  mutate(protection_level = factor(protection_level)) |> #, levels = c (" ")
  mutate(protection_level = as.numeric(rev(protection_level))) |> 
  ggplot(aes(x=(protection_level), y=(biomass))) +
  # geom_point() +
  geom_smooth(method = "lm")+
  facet_grid(region~. , scales = "free_y") +
  theme_bw() +
  theme(legend.position = "top")



# Families -------------
# Serranidae, Labridae, Pomacentridae, Lutjanidae, Haemulidae y Scaridae.

# alldata <- readRDS("data/fish_data_productivity-by-species-allyears.RDS") |>
# alldata <- readRDS("data/fish_datagr_prod-by-species-allyears.RDS") |>
alldata<- data_with_prod2 |> 
  janitor::clean_names() %>%
  group_by(year, region, reef, habitat, family, depth2, transect)%>%
  summarise(biomass = sum(biom), product = sum(prod)#, turnover = sum(productivity)
            )

unique(alldata$habitat)


fam <- alldata %>%
  filter(region %in% c("Los Cabos", "Cabo Pulmo"), year > 2007) %>%
  bind_rows(alldata %>% filter(region != "Los Cabos" & region != "Cabo Pulmo")) |> 
  filter(family %in% c("Serranidae", "Labridae", "Pomacentridae", "Lutjanidae", "Haemulidae", "Scaridae")) 
# "PARED"       "BLOQUES"     "RISCO"       "CORAL NEGRO"


# Graficos
f1 <- ggplot(fam, aes(x = year, y = log1p(biomass), col = family)) +
  geom_smooth(method = "gam"
              # , method.args = list(Gamma(link = "log"))
              ) +
  facet_grid(region ~ . , scales = "free_y") +
  theme_bw() +
  theme(legend.position = " ")

f2 <- ggplot(fam, aes(x = year, y = log1p(product), col = family)) +
  geom_smooth(method = "gam") +
  facet_grid(region ~ . , scales = "free_y") +
  theme_bw() +
  theme(legend.position = " ")

f3 <- ggplot(fam, aes(x = year, y = log1p(turnover), col = family)) +
  geom_smooth(method = "gam") +
  facet_grid(region ~ . , scales = "free_y") +
  theme_bw() +
  theme(legend.position = " ")

# Muestra los gráficos combinados
f1 + f2 #+ f3

# Define un conjunto mínimo de datos para el gráfico de leyenda
legend_fam <- data.frame(Familia = unique(fam$family))

# Crea un gráfico para la leyenda
legend_plot <- ggplot(legend_fam, aes(x = Familia, y = 1, color = Familia)) +
  geom_line(size = 2) +
  scale_color_manual(values = c("#a30808", "#b57560", "#114a06", "#258f4e", "#6e1044", "#4585ad", "#fabf4c", "#f4777f", "#3e563e", "#a95fbc", "#708090")) +
  theme_void() +  # Elimina todos los elementos del gráfico excepto la línea
  theme(legend.position = "bottom", 
        legend.spacing.y = unit(0, "pt"))  # Elimina el espacio entre la leyenda y los gráficos


library(gridExtra)

# Combinar las gráficas en una sola fila
family <- grid.arrange(f1, f2, f3, nrow = 1)

# Guardar la imagen como un archivo PNG
ggsave("plot_family_spp.png", family, width = 10, height = 5, dpi = 300)



# Grupos domimantes------
# Chromis atrilobata
# Thalassoma lucasanum
# Paranthias colonus
# Stegastes rectifraenum
# Holacanthus passer
# Prionurus punctatus
# Bodianus diplotaenia
# Ophioblennius steindachneri
# Halichoeres dispilus
# Johnrandallia nigrirostris
# Cirrhitichthys oxycephalus


# Paredes 
# alimentadores de zooplancton y carnívoros
# Chromis atrilobata, Paranthias colonus, 
# Lutjanus argentiventris, Epinephelus panamensis, E. labriformis)
# alimentándose de gorgonias
# Johnrandallia nigrirostris, Pomacanthus zonipectus
# se refugian en grietas o invertebrados sésiles
# Apogon retrosella, Stegastes flavilatus
unique(alldata$diet)

alldata <- readRDS("data/fish_data_productivity-by-species-allyears.RDS") |>
  janitor::clean_names() %>%
  group_by(year, region, reef, diet, family, depth2, transect)%>% #family,
  summarise(biomass = sum(biom), product = sum(prod)
            , turnover = sum(productivity)
            )

Zoo <- alldata %>%
  filter(region %in% c("Los Cabos", "Cabo Pulmo"), year > 2007) %>%
  bind_rows(alldata %>% filter(region != "Los Cabos" & region != "Cabo Pulmo")) 

z1 <- ggplot(Zoo, aes(x = year, y = log1p(biomass), col = diet)) +
  geom_smooth(method = "gam") +
  facet_grid(region ~ . , scales = "free_y") +
  theme_bw() +
  scale_color_manual(values = c("#a30808", "#b57560", "#114a06", "#258f4e"))+
  theme(legend.position = " ")

z2 <- ggplot(Zoo, aes(x = year, y = log1p(product), col = diet)) +
  geom_smooth(method = "gam"
              # , method.args = list(Gamma(link = "log"))
              ) +
  facet_grid(region ~ . , scales = "free_y") +
  theme_bw() +
  scale_color_manual(values = c("#a30808", "#b57560", "#114a06", "#258f4e"))+
  theme(legend.position = " ")

z3 <- ggplot(Zoo, aes(x = year, y = log1p(turnover), col = diet)) +
  geom_smooth(method = "gam") + #, method.args = list(Gamma(link = "log")
  facet_grid(region ~ . , scales = "free_y") +
  theme_bw() +
  scale_color_manual(values = c("#a30808", "#b57560", "#114a06", "#258f4e"))+
  theme(legend.position = " ")
unique(Zoo$diet)

z1+z2+z3


diet_plot <- grid.arrange(z1, z2, z3, nrow = 1)


# Guardar la imagen como un archivo PNG
ggsave("diet_plot_tg.png", diet_plot, width = 10, height = 5, dpi = 300)


library(ggplot2)
library(patchwork)

# Define un conjunto mínimo de datos para el gráfico de leyenda
legend_data <- data.frame(diet = unique(Zoo$diet))

# Crea un gráfico para la leyenda
legend_plot <- ggplot(legend_data, aes(x = diet, y = 1, color = diet)) +
  geom_line(size = 2) +
  scale_color_manual(values = c("#a30808", "#b57560", "#114a06", "#258f4e")) +
  theme_void() +  # Elimina todos los elementos del gráfico excepto la línea
  theme(legend.position = "bottom", 
        legend.spacing.y = unit(0, "pt"))  # Elimina el espacio entre la leyenda y los gráficos

# Combina los gráficos y la leyenda
final_plot <- (z1 | z2 | z3) / legend_plot

# Muestra el gráfico final
final_plot

ggsave("dieta.png", legend_plot, width = 10, height = 5, dpi = 300)

