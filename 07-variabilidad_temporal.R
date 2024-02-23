library(readxl)
library(devtools)
library(rfishprod)
library(dplyr)
library(tidyverse)
library(ramify)
library(ggplot2)
library(openxlsx)
library(patchwork)
library(harrypotter)
library(ggpubr)
library(ggrepel)


# data-------------
data2010 <- readRDS("data/fishdata_productivity-by-reef-2010.RDS")
alldata <- readRDS("data/fishdata_productivity-by-reef-allyears.RDS")
data2023 <- readRDS("data/fishdata_productivity-by-reef-2023.RDS")

# data_management---------------
biom75 = quantile(data2010$log10Biom,0.95)
biom25 = quantile(data2010$log10Biom,0.25)
prod75 = quantile(data2010$log10ProdB,0.75)
prod25 = quantile(data2010$log10ProdB,0.25)
max_biom = max(data2010$log10Biom)


#Diving data into 3 classes for each biomass/productivity relationship
management = data2010 %>% 
  
  mutate(Class = ifelse(log10Biom < biom25 & log10ProdB < prod25,"deadzone",
                        ifelse(log10ProdB > prod75,"partial",
                               ifelse(log10Biom > biom75 & log10ProdB < prod75,"pristine","transition"))))%>%
  mutate(Class = as.factor(Class))

range(management$Productivity)
range(management$Biom)
range(management$Prod)


unique(management$Region)


# graficas ---------------

unique_points <- management[!duplicated(management$Reef), ]

ggplot(management, aes(x = log10Biom, y = log10ProdB, fill = Class)) +
  geom_segment(aes(x = biom75, xend=biom75, y =-0, yend=prod75),size = 0.5,colour = "black")+
  geom_segment(aes(x = biom25, xend=biom25, y =-0, yend=prod25),size = 0.5,colour = "black")+
  geom_segment(aes(x = 0, xend=biom25, y = prod25 , yend=prod25),size = 0.5,colour = "black")+
  geom_segment(aes(x = 0, xend=max_biom, y = prod75 , yend=prod75),size = 0.5,colour = "black")+
  # geom_segment(aes(x = 0, xend =3, y =-prod75, yend=prod75),size = 2,colour = "black")+
  geom_point(shape = 21, size = 3, alpha = 0.7) +
  scale_fill_manual(values = c("deadzone" = "#d69d4e", "partial" = "#046c9a", "pristine" = "#C1DB60", "transition" = "lightblue"),
                    labels = c("deadzone" = "Low biomass/turnover", "partial" = "High turnover", "pristine" = "High biomass", "transition" = "Mid-range")) +
  labs(x = "log(standing biomass (g m^-2))", y = "Biomass Turnover (P/B × 100 % per day)") +
  theme_classic() +
  ggrepel::geom_text_repel(data = unique_points, aes(label = Reef), box.padding = 0.5, point.padding = 0.5, size = 1.5, max.overlaps = 300)+

  ylim(0, 0.6) 

# ggsave("figs/ltem_2010_turnover_ajuste06.png", width = 8.5, height = 4.5, dpi=1000)


group.colors <- c(deadzone = "#d69d4e", partial = "#046c9a", pristine = "#C1DB60", transition = "lightblue")


management <- management |> 
  mutate(Protection_status = recode(Protection_status,
                                    "cabo pulmo" = "Fully protected",
                                    "sin proteccion" = "Not protected",
                                    "area protegida" = "Lightly protected")) |> 
  mutate(Class = recode(Class, "deadzone" = "Low biomass/turnover", 
                        "partial" = "High turnover", 
                        "pristine" = "High biomass", 
                        "transition" = "Mid-range"))

(ggplot(management,aes(log10Biom,log10ProdB))+
    # geom_mark_hull(aes(fill = Class), con.cap= 0, expand = 0, radius = 0, alpha = 0.3)+
    # scale_fill_manual(values=group.colors,labels=c("Low Productivity/Biomass reefs","Productive reefs","High Biomass reefs"))+
    ggnewscale::new_scale_fill() +
    geom_segment(aes(x = biom75, xend=biom75, y =-0, yend=prod75),size = 0.8,colour = "black")+
    geom_segment(aes(x = biom25, xend=biom25, y =-0, yend=prod25),size = 0.8,colour = "black")+
    geom_segment(aes(x = 0, xend=biom25, y = prod25 , yend=prod25),size = 0.8,colour = "black")+
    geom_segment(aes(x = 0, xend=max_biom, y = prod75 , yend=prod75),size = 0.8,colour = "black")+
    geom_point(size=3,alpha=0.4,shape=21,color="black",aes(fill=Protection_status))+
    scale_fill_manual(values = c("Fully protected" = "#258f4e", 
                                 "Not protected" = "#808080",   
                                 "Lightly protected" = "yellow2"))+  
    labs(x="Biomass (g/m²) - log scale",
         y = "Productivity (%/year) - log scale",
         fill = "Protection status")+
    theme_classic())+
  ggrepel::geom_text_repel(data = unique_points, aes(label = Reef), box.padding = 0.5, point.padding = 0.5, size = 2, max.overlaps = 300)
# ylim(0, 0.6)


# ggsave("figs/protect_status_2010_fig.png",height=210, width= 297,units="mm")


# suma los datos por la columna Class y Protection_status
data_summarized <- management %>%
  group_by(Class, Protection_status) %>%
  summarise(count = n())


data_summarized$Class <- factor(data_summarized$Class, 
                                levels = c("High biomass", "High turnover", "Mid-range", "Low biomass/turnover"))

# Graficar los datos reordenados
ggplot(data_summarized, aes(x = Class, y = count, fill = Protection_status)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("Fully protected" = "#258f4e", 
                               "Not protected" = "#808080",   
                               "Lightly protected" = "yellow2")) +  
  labs(title = "",
       x = "Class",
       y = "",
       fill = "Protection status") +
  theme_classic()


# ggsave("figs/ltem_2010_class_status_portected.png", width = 8.5, height = 4.5, dpi=1000)
  
#Todos los años -------------

biom75 = quantile(alldata$log10Biom,0.95)
biom25 = quantile(alldata$log10Biom,0.25)
prod75 = quantile(alldata$log10ProdB,0.75)
prod25 = quantile(alldata$log10ProdB,0.25)
max_biom = max(alldata$log10Biom)
  
  
#Diving data into 3 classes for each biomass/productivity relationship
allmanagement = alldata %>% 
    
    mutate(Class = ifelse(log10Biom < biom25 & log10ProdB < prod25,"deadzone",
                          ifelse(log10ProdB > prod75,"partial",
                                 ifelse(log10Biom > biom75 & log10ProdB < prod75,"pristine","transition"))))%>%
    mutate(Class = as.factor(Class))
  
  
unique_points <- allmanagement[!duplicated(allmanagement$Reef), ]
 
# means <- allmanagement |> 
#   group_by(Year, Reef, Class, Protection_status) |> 
#   summarise(mean_log10ProdB = mean(log10ProdB, na.rm = TRUE),
#             mean_log10Biom = mean(log10Biom, na.rm = TRUE),
#             mean_log10Prod = mean(log10Prod, na.rm = TRUE)) |> 
#   group_by(Reef, Class, Protection_status) %>%
#   summarise(mean_log10ProdB = mean(mean_log10ProdB),
#             mean_log10Biom = mean(mean_log10Biom),
#             mean_log10Prod = mean(mean_log10Prod))

# unique_points <- allmanagement[!duplicated(allmanagement$Reef), ]

ggplot(allmanagement, aes(x = log10Biom, y = log10ProdB, fill = Class)) +
  geom_segment(aes(x = biom75, xend = biom75, y = -0, yend = prod75), size = 0.8, colour = "black") +
  geom_segment(aes(x = biom25, xend = biom25, y = -0, yend = prod25), size = 0.8, colour = "black") +
  geom_segment(aes(x = 0, xend = biom25, y = prod25 , yend = prod25), size = 0.8, colour = "black") +
  geom_segment(aes(x = 0, xend = max_biom, y = prod75 , yend = prod75), size = 0.8, colour = "black") +
  geom_point(shape = 21, size = 3, alpha = 0.7) +
  scale_fill_manual(values = c("deadzone" = "#d69d4e", "partial" = "#046c9a", "pristine" = "#C1DB60", "transition" = "lightblue"),
                    labels = c("deadzone" = "Low biomass/turnover", "partial" = "High turnover", "pristine" = "High biomass", "transition" = "Mid-range")) +
  labs(x = "log(standing biomass (g m^-2))", y = "Biomass Turnover (P/B × 100 % per day)") +
  theme_classic() +
  ggrepel::geom_text_repel(data = unique_points, aes(label = Reef), box.padding = 0.5, point.padding = 0.5, size = 1.5, max.overlaps = 300)

# ggsave("figs/ltem_allyears_mean_class_reef.png", width = 8.5, height = 4.5, dpi=1000)


(ggplot(allmanagement,aes(mean_log10Biom,mean_log10ProdB))+
    # geom_mark_hull(aes(fill = Class), con.cap= 0, expand = 0, radius = 0, alpha = 0.3)+
    # scale_fill_manual(values=group.colors,labels=c("Low Productivity/Biomass reefs","Productive reefs","High Biomass reefs"))+
    ggnewscale::new_scale_fill() +
    geom_segment(aes(x = biom75, xend=biom75, y =-0, yend=prod75),size = 0.8,colour = "black")+
    geom_segment(aes(x = biom25, xend=biom25, y =-0, yend=prod25),size = 0.8,colour = "black")+
    geom_segment(aes(x = 0, xend=biom25, y = prod25 , yend=prod25),size = 0.8,colour = "black")+
    geom_segment(aes(x = 0, xend=max_biom, y = prod75 , yend=prod75),size = 0.8,colour = "black")+
    geom_point(size=3,alpha=0.4,shape=21,color="black",aes(fill=Protection_status))+
    scale_fill_manual(values = c("Fully protected" = "#258f4e", 
                                 "Not protected" = "#808080",   
                                 "Lightly protected" = "yellow2"))+  
    labs(x="Biomass (g/m²) - log scale",
         y = "Productivity (%/year) - log scale",
         fill = "Protection status")+
    theme_classic())+
  ggrepel::geom_text_repel(data = unique_points, aes(label = Reef), box.padding = 0.5, point.padding = 0.5, size = 2, max.overlaps = 300)
# ylim(0, 0.6)


# ggsave("figs/protect_status_2010_fig.png",height=210, width= 297,units="mm")


# suma los datos por la columna Class y Protection_status
data_summarized_all <- allmanagement |> 
  group_by(Reef, Class, Protection_status) |> 
    summarise(mean_log10ProdB = mean(log10ProdB, na.rm = TRUE),
              mean_log10Biom = mean(log10Biom, na.rm = TRUE),
              mean_log10Prod = mean(log10Prod, na.rm = TRUE)) |> 
  mutate(Protection_status = recode(Protection_status,
                                    "cabo pulmo" = "Fully protected",
                                    "sin proteccion" = "Not protected",
                                    "area protegida" = "Lightly protected")) |> 
  mutate(Class = recode(Class, "deadzone" = "Low biomass/turnover", 
                        "partial" = "High turnover", 
                        "pristine" = "High biomass", 
                        "transition" = "Mid-range")) |> 
  group_by(Class, Protection_status) |> 
  summarise(count = n())


data_summarized_all$Class <- factor(data_summarized_all$Class, 
                                levels = c("High biomass", "High turnover", "Mid-range", "Low biomass/turnover"))

# Graficar los datos reordenados
ggplot(data_summarized_all, aes(x = Class, y = count, fill = Protection_status)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("Fully protected" = "#258f4e", 
                               "Not protected" = "#808080",   
                               "Lightly protected" = "yellow2")) +  
  labs(title = "",
       x = "Class",
       y = "",
       fill = "Protection status") +
  theme_classic()



# Variabilidad Temporal  -----

mean_prod_by_year_reef <- allmanagement %>%
  group_by(Year, Class, Reef) %>%
  summarise(mean_log10ProdB = mean(log10ProdB, na.rm = TRUE),
            mean_log10Biom = mean(log10Biom, na.rm = TRUE),
            mean_log10Prod = mean(log10Prod, na.rm = TRUE)) %>%
  group_by(Year, Class) %>%
  summarise(mean_log10ProdB = mean(mean_log10ProdB),
            mean_log10Biom = mean(mean_log10Biom),
            mean_log10Prod = mean(mean_log10Prod))

  

ggplot(mean_prod_by_year_reef, aes(x = Year, y = mean_log10ProdB, group = Class, color = Class)) +
      geom_line() +
      labs(title = "Variabilidad Temporal",
           x = "Year",
           y = "Biomass Turnover (P/B × 100 % per day)") +
      scale_color_manual(values = c("deadzone" = "#d69d4e", 
                                    "partial" = "#046c9a", 
                                    "pristine" = "#C1DB60", 
                                    "transition" = "lightblue"),
                         labels = c("deadzone" = "Low biomass/turnover", 
                                    "partial" = "High turnover", 
                                    "pristine" = "High biomass", 
                                    "transition" = "Mid-range")) +
      theme_minimal()
    
# ggsave("figs/ltem_allyears_Variabilidad_Temporal.png", width = 8.5, height = 4.5, dpi=1000)
