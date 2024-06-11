# DATA-------------


alldata <- readRDS("data/fishdata_prod-by-reef-allyears_1000.RDS")
data2010 <- readRDS("data/fishdata_product-by-reef-2010.RDS")
data2023 <- readRDS("data/fishdata_product-by-reef-2023.RDS")
data <- data2010


ltem <- readRDS("data/ltem_historic_integrada_2024-05-24.RDS")

sspdata <- readRDS("data/fish_datagr_prod-by-species-herbivoros2.RDS") 

# alldata <- readRDS("data/fishdata_prod-by-reef-herbivoros.RDS")

fish <- sspdata |> 
  janitor::clean_names() |> 
  filter(label == "PEC") |> 
  mutate(
    a_ord = as.numeric(a),
    b_pen = as.numeric(b),
    quantity = as.numeric(quantity),
    size = as.numeric(size),
    area = as.numeric(area),
    month = as.numeric(month),
    biomass = (quantity * a_ord * (size^b_pen)) / (area * 100) # Fórmula para calcular la biomasa (ton/ha)
  ) |> 
  mutate(biomass = as.numeric(biomass))


unique(sspdata$Species)

# Sumar para cada transecto ---------
data <- sspdata |> 
  filter(Species %in% c("Calotomus carolinus", "Nicholsina denticulata", "Scarus compressus",
                        "Scarus ghobban", "Scarus perrico", "Scarus rubroviolaceus")) |> 
  #Sum for each transect
  group_by(Year,Region, Reef,Depth2,Transect) %>%
  mutate(
    Biom = sum(Biom)/Area,# (kg ha^−1) # porqué 500?
    Prod = sum(Prod)/Area,#g d^−1 ha^−1
    Productivity = (Prod/Biom)*100) %>%
  ungroup() |> 
  #Mean for each site
  group_by(Year, Reef) %>%
  mutate(Biom = mean(Biom),
         Prod = mean(Prod),
         Productivity = mean(Productivity)) %>% 
  ungroup() %>%
  #Transforming data
  mutate(
    log10ProdB = Productivity,# % per day
    log10Biom = log10(Biom+1), #(g m -2)
    log10Prod = log10(Prod+1), # (g m-2 d-1)
    Latitude = as.numeric(as.character(Latitude)),
    Longitude = as.numeric(as.character(Longitude)))


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
# plot by class--------------
#eje y = Biomass turnover, P/B × 100 (% per day)
# eje x <- log[standing biomass (g m–2)]
# filll = Class (Low biomass/turnover High turnover High biomass Mid-range)
# Class=c("deadzone"="Low biomass/turnover","partial"="High turnover","pristine"="High biomass", "transition" = "transition")


unique_points <- management[!duplicated(management$Reef), ]

ggplot(management, aes(x = log10Biom, y = log10ProdB, fill = Class)) +
  geom_point(shape = 21, size = 3, alpha = 0.7) +
  scale_fill_manual(values = c("deadzone" = "#FF9900", "partial" = "#3366CC", "pristine" = "#33CC33", "transition" = "#9900CC"),
                    labels = c("deadzone" = "Low biomass/turnover", "partial" = "High turnover", "pristine" = "High biomass", "transition" = "Transition")) +
  labs(x = "log(standing biomass (g m^-2))", y = "Biomass Turnover (P/B × 100 % per day)") +
  theme_minimal() +
  # geom_text(data = unique_points, aes(label = Reef), size = 2, vjust = 4, nudge_y = 0.1)
  ggrepel::geom_text_repel(data = unique_points, aes(label = Reef), box.padding = 0.5, point.padding = 0.5, size = 2, max.overlaps = 300)


# ggsave("figs/fish_prod_2023.png", width = 8.5, height = 4.5, dpi=1000)

avers<- management |> 
  filter(Class == "pristine") |>
  distinct(Region, Reef, Class, log10ProdB, log10Biom)

# figures ---------------

traits_trophic = data %>%
  dplyr::select(Species, TrophicLevelF, TrophicLevel,Functional_groups,Diet, MaxSizeTL )

RLS_prod_all_site = data%>% 
  # dplyr::left_join(transect_info, by = "Transect") %>%
  dplyr::select(c(Species, Genus,Family,Quantity, Size, MaxSizeTL,W, Kmax,Reef, Protection_level))

RLS_prod_all_site <- RLS_prod_all_site %>% distinct(Species, Genus,Family,Quantity, Size, MaxSizeTL,W, Kmax,Reef, .keep_all = TRUE)
traits_trophic <- traits_trophic %>% distinct(Species, TrophicLevelF, TrophicLevel,Functional_groups,Diet, MaxSizeTL, .keep_all = TRUE) |> 
  select(-MaxSizeTL)
str(traits_trophic)
RLS_prod_figures = management %>%
  # left_join(RLS_prod_all_site, by = "Reef") %>%
  # left_join(traits_trophic, by = "Species") |> 
  #Sum by Class
  group_by(Class) %>%
  dplyr::mutate(site_biom = sum(Biom)) %>%
  ungroup() %>%
  #Sum by Class and trophic group
  group_by(Class,TrophicLevelF) %>%
  dplyr::mutate(troph_biom = sum(Biom)) %>%
  ungroup() %>%
  #Proportional trophic groups
  dplyr::mutate(trophic_pyramid = troph_biom/site_biom,
                trophic_percentage = trophic_pyramid * 100) %>%
  #Average by class
  group_by(Class, TrophicLevelF) %>%
  dplyr::mutate(class_trophic = mean(trophic_percentage)) %>%
  ungroup() %>%
  group_by(Reef) %>%
  mutate(biom_by_site = sum(Biom)) %>%
  ungroup() %>%
  group_by(Reef,Diet) %>%
  mutate(diet_biom = sum(Biom),
         diet_size = mean(Size)) %>%
  ungroup() %>%
  mutate(relative_diet = (diet_biom/biom_by_site)*100)

group.colors.diet <- c(`Piscivoro` = "#a30808", `Carnivoro` ="#b57560", `Herbivoro` = "#114a06",`Zooplanctivoro` = "#258f4e")
(plot_biomass = ggplot(RLS_prod_figures %>% 
                         filter(Class != "transition") %>%
                         dplyr::select(Class, Reef, Diet, relative_diet) %>% distinct(), aes(Diet,relative_diet,fill=Diet))+
    geom_jitter(alpha  = 0.3, size = 0.5,color='black',pch=21) +
    geom_boxplot(alpha  = 0.8) +
    scale_fill_manual(values = group.colors.diet) +
    scale_color_manual(values = group.colors.diet) +
    ylim(0,100) +
    facet_wrap(~Class,labeller=labeller(Class=c("deadzone"="Low biomass/turnover","partial"="High turnover","pristine"="High biomass", "transition" = "transition")))+
    labs(x = "",
         y= "Relative biomass (%)") +
    theme_minimal()+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()))

RLS_prod_figures$Diet <- factor(RLS_prod_figures$Diet, levels=c("Piscivoro", 
                                                                "Carnivoro", 
                                                                "Herbivoro", 
                                                                "Zooplanctivoro"))

(diet_biomass = ggplot(RLS_prod_figures %>% 
                         filter(Class != "transition") %>%
                         dplyr::select(Class, Reef, Diet, relative_diet) %>% distinct(), aes(Diet,relative_diet,fill=Diet))+
    geom_jitter(alpha  = 0.3, size = 0.5,color='black',pch=21) +
    geom_boxplot(alpha  = 0.8) +
    scale_fill_manual(values = group.colors.diet) +
    scale_color_manual(values = group.colors.diet) +
    ylim(0,100) +
    facet_wrap(~Class,labeller=labeller(Class=c("deadzone"="Low biomass/turnover","partial"="High turnover","pristine"="High biomass", "transition" = "transition")))+
    labs(x = "",
         y= "Relative biomass (%)") +
    theme_minimal()+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()))

(diet_size = ggplot(RLS_prod_figures %>% 
                      filter(Class != "transition") %>%
                      dplyr::select(Class, Reef, Diet, diet_size) %>% distinct(), aes(Diet,diet_size,fill=Diet))+
    geom_jitter(alpha  = 0.3, size = 0.5,color='black',pch=21) +
    geom_boxplot(alpha  = 0.8) +
    scale_fill_manual(values = group.colors.diet) +
    scale_color_manual(values = group.colors.diet) +
    facet_wrap(~Class,labeller=labeller(Class=c("deadzone"=" ","partial"=" ","pristine"="", "transition" = " ")))+
    labs(x = "",
         y= "Mean fish size (cm)") +
    theme_minimal())

ggarrange(diet_biomass, diet_size, ncol = 1,common.legend = T)
# ggsave("figs/ltem_prod_diet_detail_2023_.png", width = 8.5, height = 4.5, dpi=1000)
# 
# ggsave("figs/diet_detail_2023.pdf",width =297,height  =210, units="mm",dpi=600)

RLS_prod_figures$TrophicLevelF <- factor(RLS_prod_figures$TrophicLevelF, levels=c("2-2.5", "2.5-3", "3-3.5", "3.5-4", "4-4.5"))
group.colors <- c(`2-2.5` = "#144D49", `2.5-3` = "#276C69", `3-3.5` = "#73C1C4", `3.5-4` = "lightgrey", `4-4.5` = "#BF8599")


#Trophic pyramids----------
RLS_prod_figures$TrophicLevelF <- factor(RLS_prod_figures$TrophicLevelF, levels=c("2-2.5", "2.5-3", "3-3.5", "3.5-4", "4-4.5"))
group.colors <- c(`2-2.5` = "#144D49", `2.5-3` = "#276C69", `3-3.5` = "#73C1C4", `3.5-4` = "lightgrey", `4-4.5` = "#BF8599")
group.colors.class <- c(deadzone = "#d69d4e", partial = "#046c9a", pristine = "#C1DB60")

protection = RLS_prod_figures %>% filter(Class == "partial") %>% dplyr::select(Class, TrophicLevelF, class_trophic) %>% distinct() %>% mutate(class_trophic = round(class_trophic,5)) %>%
  add_row(Class = "deadzone", TrophicLevelF = "4-4.5",class_trophic = 0)

protection$TrophicLevelF <- factor(protection$TrophicLevelF, levels=c("2-2.5", "2.5-3", "3-3.5", "3.5-4", "4-4.5"))

ggplot(protection)+
  geom_bar(aes(x = TrophicLevelF,y = class_trophic,fill=TrophicLevelF),stat='identity')+
  # scale_y_continuous(breaks = brks)+
  scale_fill_manual(values = group.colors)+
  coord_flip() +
  theme_void()+
  theme(legend.position="top")

# ggsave("figs/partial_trophic_2010.png", width = 8.5, height = 4.5, dpi=1000)
# ggsave("Figures/partial_trophic.pdf")

protection_pristine = RLS_prod_figures %>% filter(Class == "pristine") %>% dplyr::mutate(Diet = ifelse(Diet == "higher carnivore", "Higher carnivore",Diet)) %>%
  dplyr::mutate(individual_biomass = W * Quantity,
                total_biomass = sum(individual_biomass)) %>%
  group_by(Diet) %>%
  dplyr::mutate(trophic_group_biomass = sum(individual_biomass)) %>%
  ungroup() %>% 
  dplyr::mutate(relative_biomass = (trophic_group_biomass/total_biomass)*100) %>%
  dplyr::select(Class, Diet, relative_biomass) %>%
  distinct(.keep_all = T)

protection_deadzone= RLS_prod_figures %>% filter(Class == "deadzone")  %>% dplyr::mutate(Diet = ifelse(Diet == "higher carnivore", "Higher carnivore",Diet)) %>%
  dplyr::mutate(individual_biomass = W * Quantity,
                total_biomass = sum(individual_biomass)) %>%
  group_by(Diet) %>%
  dplyr::mutate(trophic_group_biomass = sum(individual_biomass)) %>%
  ungroup() %>% 
  dplyr::mutate(relative_biomass = (trophic_group_biomass/total_biomass)*100) %>%
  dplyr::select(Class, Diet, relative_biomass) %>%
  distinct(.keep_all = T)

protection_partial = RLS_prod_figures %>% filter(Class == "partial")  %>% dplyr::mutate(Diet = ifelse(Diet == "higher carnivore", "Higher carnivore",Diet)) %>%
  dplyr::mutate(individual_biomass = W * Quantity,
                total_biomass = sum(individual_biomass)) %>%
  group_by(Diet) %>%
  dplyr::mutate(trophic_group_biomass = sum(individual_biomass)) %>%
  ungroup() %>% 
  dplyr::mutate(relative_biomass = (trophic_group_biomass/total_biomass)*100) %>%
  dplyr::select(Class, Diet, relative_biomass) %>%
  distinct(.keep_all = T)

(pristine = ggplot(protection_pristine,aes(Diet, relative_biomass))+
    geom_bar(stat="identity",aes(fill = Class))+
    scale_fill_manual(values=group.colors.class,labels=c("Low Productivity/Biomass reefs","Productive reefs","High Biomass reefs"))+
    labs(x = "",
         y="Relative biomass (%)",
         fill = "Class")+
    ylim(0,50)+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=0.9))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()))

(deadzone = ggplot(protection_deadzone,aes(Diet, relative_biomass))+
    geom_bar(stat="identity",aes(fill = Class))+
    scale_fill_manual(values=group.colors.class,labels=c("Low Productivity/Biomass reefs","Productive reefs","High Biomass reefs"))+
    labs(x = "",
         y="Relative biomass (%)",
         fill = "Class")+
    theme_minimal()+
    ylim(0,50)+
    theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=0.9))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()))

(partial = ggplot(protection_partial,aes(Diet, relative_biomass))+
    geom_bar(stat="identity",aes(fill = Class))+
    scale_fill_manual(values=group.colors.class,labels=c("Low Productivity/Biomass reefs","Productive reefs","High Biomass reefs"))+
    labs(x = "",
         y="Relative biomass (%)",
         fill = "Class")+
    theme_minimal()+
    ylim(0,50)+
    theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=0.9))
)

ggarrange(pristine,deadzone,partial,ncol =1, common.legend = T)
# ggsave("figures/Diet_by_class_2010.png", width = 210, height = 297, units ="mm")

# 
# ggplot(RLS_prod_figures,aes(log10(site_biom+1),trophic_percentage,group = Diet, color = Diet))+
#   geom_point(shape = ".", alpha = 0.5)+
#   geom_smooth()+
#   theme_minimal()+
#   scale_color_hp_d(option = "LunaLovegood")+
#   labs(x = "Standing biomass (g/m^2) - log scale",
#        y = "Relative biomass (%)",
#        color = "Trophic position")

# ggsave("figures/deadzone_trophic.pdf")

# kplots-------------

data_prod = RLS_prod_figures

data_plot = data_prod %>%
  group_by(Species)%>%
  mutate(K = Kmax,
         # K = log10(Kmax),
         Family = as.factor(Family),
         Species = as.factor(Species))%>%
  distinct(K,.keep_all=T)

library(wesanderson)
library(ggpubr)
mycolors <- colorRampPalette(wes_palette("Darjeeling1", 27, type = "continuous"))(138)

(p = ggscatter(data_plot,x="MaxSizeTL",y="K",
               color="Family",
               palette = mycolors,
               size=3,alpha=0.6)+
    border()+ 
    labs(
      y="Kmax",
      x="Maximum observed size (cm)")+
    theme(legend.position = "none", legend.key.width=unit(3,"cm")) + stat_smooth(method = lm, formula = y ~ log(x),se=F,))

# ggsave("Figures/K_by_size.pdf",height=210,width=297, units = "mm")
# ggsave("Figures/K_by_size.png",height=210,width=297, units = "mm")

#Details for each Family and Genus 
unique(data_prod$Family)

data_Serranidae = data_prod %>%
  filter(Family == "Serranidae")%>%
  group_by(Species)%>%
  mutate(K_pred2 = mean(Kmax))%>%
  ungroup()

data_labridae = data_prod %>%
  filter(Family == "Labridae")%>%
  group_by(Species)%>%
  mutate(K_pred2 = mean(Kmax))%>%
  ungroup()

(p_Serranidae = ggscatter(data_Serranidae,x="MaxSizeTL",y="K_pred2",
                          palette = mycolors,
                          size=3,alpha=0.6)+
    border()+ stat_smooth(method = lm, formula = y ~ log(x)) +
    # add_fishape(family="Lutjanidae",option="Lutjanus_gibbus",
    #             xmin=140,xmax=160,ymin=0.75,ymax=1,fill="darkblue",alpha=0.7)+
    labs(title="Serranidae",
         y="Kmax",
         x="Maximum observed size (cm)"))

(p_labridae = ggscatter(data_labridae,x="MaxSizeTL",y="K_pred2",
                        palette = mycolors,
                        size=3,alpha=0.6)+
    # add_fishape(family="Labridae",option="Epibulus_insidiator",
    #             xmin=200,xmax=230,ymin=5,ymax=8,fill="darkblue",alpha=0.7)+
    border()+ stat_smooth(method = lm, formula = y ~ log(x)) +
    labs(title="Labridae",
         y="Kmax",
         x="Maximum observed size (cm)"))

(p_family = ggarrange(p_Serranidae,p_labridae))

serranidae = data_prod %>% filter(Family == "Labridae")
unique(serranidae$Genus)
unique(data_prod$Genus)
data_Epinephelus = data_prod %>%
  filter(Genus == "Epinephelus")%>%
  group_by(Species)%>%
  mutate(K_pred2 = mean(Kmax))%>%
  ungroup()

data_Scarus = data_prod %>%
  filter(Genus == "Scarus")%>%
  group_by(Species)%>%
  mutate(K_pred2 = mean(Kmax))%>%
  ungroup()

(p_Epinephelus = ggscatter(data_Epinephelus,x="MaxSizeTL",y="K_pred2",
                           palette = mycolors,
                           size=3,alpha=0.6)+
    border()+ stat_smooth(method = lm, formula = y ~ log(x)) +
    # add_fishape(family="Lutjanidae",option="Lutjanus_gibbus",
    #             xmin=140,xmax=160,ymin=0.8,ymax=1,fill="darkblue",alpha=0.7)+
    labs(title="Epinephelus",
         y="Kmax",
         x="Maximum observed size (cm)"))


(p_Scarus= ggscatter(data_Scarus,x="MaxSizeTL",y="K_pred2",
                     palette = mycolors,
                     size=3,alpha=0.6)+
    border()+ stat_smooth(method = lm, formula = y ~ log(x))+
    labs(title="Scarus",
         y="Kmax",
         x="Maximum observed size (cm)"))
# add_fishape(family="Labridae",option="Epibulus_insidiator",
#             xmin=190,xmax=220,ymin=3,ymax=4,fill="darkblue",alpha=0.7))

p_genus = ggarrange(p_Epinephelus,p_Scarus)

ggarrange(p_family,p_genus,ncol=1)
# 
# ggsave("Figures/K_by_size_details.png",height = 210,width=297,units="mm")
# ggsave("Figures/K_by_size_details.pdf",height = 210,width=297,units="mm")

K_by_family = ggplot(data_prod,aes(reorder(Family,-Kmax),Kmax,fill=Family,colour=Family))+
  geom_jitter(size = 0.1, alpha = 0.2)+
  geom_boxplot(alpha = 0.7)+
  scale_fill_viridis_d()+
  scale_color_viridis_d()+ 
  theme_bw()+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs( y = "Kmax",
        x = "")



group.colors <- c(deadzone = "#d69d4e", partial = "#046c9a", pristine = "#C1DB60")

library(ggforce)

#Plotting the classes------------

(ggplot(data_prod,aes(log10Biom,log10ProdB,colour=Class))+
   # geom_mark_hull(aes(fill = Class), con.cap= 0, expand = 0, radius = 0, con.size = 0)+
   geom_point(size=3,alpha=0.4)+
   scale_colour_manual(values=group.colors,labels=c("Low turnover/biomass reefs","High turnover","High Biomass reefs"))+
   scale_fill_manual(values=group.colors,labels=c("Low turnover/biomass reefs","Productive reefs","High Biomass reefs"))+
   labs(x="Biomass (g/m²) - log scale",
        y = "Productivity (%/year) - log scale")+
   theme_classic())+
# geom_text(data = unique_points, aes(label = Reef), size = 2, vjust = 4, nudge_y = 0.1)  
ggrepel::geom_text_repel(data = unique_points, aes(label = Reef), box.padding = 0.5, point.padding = 0.5, size = 2, max.overlaps = 300)

# ggsave("figs/protect_status_Class_2023_text.png",height=210, width= 297,units="mm")


library(ggnewscale)


(ggplot(RLS_prod_figures,aes(log10Biom,log10ProdB,colour=Class))+
    geom_mark_hull(aes(fill = Class), con.cap= 0, expand = 0, radius = 0, con.size = 0)+
    geom_point(size=3,alpha=0.4)+
    scale_colour_manual(values=group.colors,labels=c("Low turnover/biomass reefs","High turnover","High Biomass reefs"))+
    scale_fill_manual(values=group.colors,labels=c("Low turnover/biomass reefs","Productive reefs","High Biomass reefs"))+
    labs(x="Biomass (g/m²) - log scale",
         y = "Productivity (%/year) - log scale")+
    theme_classic())+
  # geom_text(data = unique_points, aes(label = Reef), size = 2, vjust = 4, nudge_y = 0.1)
  ggrepel::geom_text_repel(data = unique_points, aes(label = Reef), box.padding = 0.5, pcoint.padding = 0.5, size = 2, max.overlaps = 300)


(ggplot(RLS_prod_figures,aes(log10Biom,log10ProdB))+
    # geom_mark_hull(aes(fill = Class), con.cap= 0, expand = 0, radius = 0, alpha = 0.3)+
    scale_fill_manual(values=group.colors,labels=c("Low Productivity/Biomass reefs","Productive reefs","High Biomass reefs"))+
    ggnewscale::new_scale_fill() +
    geom_point(size=3,alpha=0.4,shape=21,color="black",aes(fill=ProtectionStatus))+
    scale_fill_viridis_d()+
    labs(x="Biomass (g/m²) - log scale",
         y = "Productivity (%/year) - log scale",
         fill = "Protection status")+
    theme_classic())+
  # geom_text(data = unique_points, aes(label = Reef), size = 2, vjust = 4, nudge_y = 0.1)
  ggrepel::geom_text_repel(data = unique_points, aes(label = Reef), box.padding = 0.5, point.padding = 0.5, size = 2, max.overlaps = 300)


# ggsave("figs/protect_status_2023_text.png",height=210, width= 297,units="mm")

data_family_genus = data_prod %>%
  dplyr::select(Family,Genus)%>%
  distinct(Genus, .keep_all = T)
