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


tabl <- read_excel("data/tabla/spp_parametros_20231201.xlsx")|> 
  mutate(sstmean = 27) |> 
  filter(!is.na (Linf), !is.na (LinfTL)) |>
  # mutate(LinfTL = ifelse(is.na(LinfTL), Linf, LinfTL))|>
  mutate(Kmax = K * Linf / LinfTL)
  # mutate(Kmax = K  / Linf)

fish <- readRDS("data/tabla/ltem_historic_20231109.RDS") |> 
  filter(Label == "PEC", Year==2022, Region%in% c("La Paz", "La Ventana", "Loreto", "Los Cabos", "Cabo Pulmo")) |> 
  mutate(
    A_ord = as.numeric(A_ord),
    B_pen= as.numeric(B_pen),
    Quantity = as.numeric(Quantity),
    Size=as.numeric(Size),
    Area= as.numeric(Area),
    Month= as.numeric(Month),
    Biomass = (Quantity * A_ord* (Size^B_pen))/(Area * 100)) |> 
    # Biomass = (A_ord * (Size^B_pen) * Quantity) / (1000 * Area),
    # Biomass = ((A_ord* (Size^B_pen))/1000) * Quantity) |>
  mutate(
    Biomass=as.numeric(Biomass),
    TrophicGroup = factor(TrophicGroup, 
                          levels = c("Piscivoro", 
                                     "Carnivoro", 
                                     "Herbivoro", 
                                     "Zooplanctivoro")), 
    Region = factor(Region),
    TrophicLevelF = cut(as.numeric(TrophicLevel), 
                        breaks = c(2, 2.5, 3, 3.5, 4, 4.6), 
                        labels = c("2-2.5", "2.5-3", "3-3.5", "3.5-4", "4-4.5"), 
                        right = FALSE))
  
  # #Remove sharks rays and seahorses
  # dplyr::filter(Taxa2 == "Actinopterygii" & Taxa2 != "Syngnathidae") |> 
  # filter(Family != "Anguillidae", Family != "Congridae", Family != "Muraenidae", Family != "Ophichthidae") |>  
  # #Removing small crypto 
  # filter(Family != "Chironemidae", Family != "Pinguipedidae", Family != "Chaenopsidae", Family != "Tripterygiidae",
  #        Family != "Gobiidae", Family != "Callionymidae", Family != "Blenniidae",
  #        Family != "Labrisomidae", Family != "Microdesmidae", Family != "Pholidichthyidae")

# merge ----------
merge_database <- merge(fish, tabl, by = c("Family", "Species", "A_ord", "B_pen"), all.x = TRUE)  |> 
  rename(Diet = TrophicGroup, a = "A_ord",
         b = "B_pen") |> 
  mutate(sstmean = 27) |> 
  filter(!Size == 0) |>
  filter(!is.na (Linf)) |>
  filter(!is.na (Size)) |>
  filter(!is.na (SpecCode)) |>
  filter(!is.na (MaxSizeTL))


# Formula from Morais and Bellwood (2018) #
fmod <- formula(~ sstmean + MaxSizeTL)
# fmod <- formula(~ sstmean + MaxSizeTL + Diet + Position + Method)



ltem_db <- merge_database |> 
  filter(Year==2022, Region%in% c("La Paz", "La Ventana", "Loreto", "Los Cabos", "Cabo Pulmo")) |> 
  distinct(Family, Species, SpecCode, MaxSizeTL, a, b, Diet, 
           LinfTL, K, O, Kmax, Longitude, Latitude, sstmean, Method) 

ltem_repdata <- merge_database |> 
  filter(Year==2022, Region%in% c("La Paz", "La Ventana", "Loreto", "Los Cabos", "Cabo Pulmo")) |> 
  select(-Kmax)

datagr <- rfishprod::predKmax(ltem_repdata,
                              dataset = ltem_db, 
                              fmod = fmod,
                              niter = 10,
                              return = 'pred')

datagr <- datagr$pred

# Predicting M/Z: the instantaneous mortality rate (Recommendation: see help file for) #
datagr$Md <- with (datagr, rfishprod::predM(Lmeas = Size,
                                            Lmax = MaxSizeTL,
                                            Kmax = Kmax,
                                            t = 1,
                                            method = 'Gislason'))

# Positioning your fish in their growth trajectory #
# aka. what's the size they're supposed to have on the next day? #
datagr$Size_nextday= with(datagr, rfishprod::applyVBGF (Lmeas = Size,
                                                        Lmax = MaxSizeTL,
                                                        Kmax = Kmax,
                                                        t = 1))

# Estimating gross somatic growth (g) #
datagr$somatic_growth = with(datagr, rfishprod::somaGain (a = a,
                                                          b = b,
                                                          Lmeas = Size,
                                                          Lmax = MaxSizeTL,
                                                          Kmax = Kmax,
                                                          t = 1))

range(datagr$somatic_growth)

# Applying stochastic mortality #
datagr$mortality = rfishprod::applyMstoch(datagr$Md,t=1)


#Alternatively, estimating per capita mass loss due to mortality #
datagr$soma_loss = with(datagr, rfishprod::somaLoss (M = Md,
                                                     Lmeas = Size,
                                                     a = a,
                                                     b = b,
                                                     t = 1))


datagr_prod = datagr %>%
  #Calculating biomass turnover-----------------
mutate(W = a*(Size^b),
       Biom = (W*Quantity),
       # Biom = ((W*Quantity)/(Area)), #Biomass = (a * (Size^b) * Quantity) / (1000 * Area),
       Prod = ifelse(mortality == T, (somatic_growth * Quantity),0))
        # Prod = ifelse(mortality == T, (somatic_growth * Biom),0))

# Productividad = Biomasa + Crecimiento Somático Total - Pérdidas debido a la Mortalidad ------

transect_info = ltem_repdata %>% 
  dplyr::select(Transect, Latitude, Longitude, IDReef, Depth, Region)

transect_site = transect_info %>% dplyr::select(Transect, IDReef) %>%
  dplyr::filter(Transect %in% datagr_prod$Transect) 

#' Calculating productivity------------------

data_with_prod2 =  datagr_prod |> 
  # filter(Year==2022, Region%in% c("La Paz", "La Ventana", "Loreto", "Los Cabos", "Cabo Pulmo")) |> 
  #Calculating production -----------------
  #Weight of fish at time of census
  mutate(W = a*Size^b,
         #Age of fish at time of census
         t = (1/Kmax)*log((MaxSizeTL)/((1-(Size/MaxSizeTL))*MaxSizeTL)),
         #Projected size one year later
         Ltx = MaxSizeTL * (1-exp(-Kmax*(t+365))),
         #Projected weight one year later
         Wtx = a*Ltx**b,
         #Production 
         Wgain = Wtx - W,
         #Biomass------ 
         # Biomass = (a * (Size^b) * Quantity) / (1000 * Area),
         Biom = (W*Quantity)/Area,
         #Individual Biomass 
         IndBiom = (W/Area),
         #Production-----------
         # Prod = (Wgain * Biom)/Area,
         Prod = (Wgain * Quantity)/Area,
         #Individual production 
         IndProd = (Wgain/Area)) %>%
  
  filter(!is.na(Prod))

# At the scale of the community (transect)---------
data_prod_brut = data_with_prod2 %>%
  #Sum for each transect
  group_by(IDReef,Depth,Transect) %>%
  # group_by(Transect) %>%
  mutate(Biom = sum(Biom)/500,# porqué 500?
         Prod = sum(Prod)/500,
  # mutate(Biom = sum(Biom)/1000,
  #        Prod = sum(Prod)/1000,
         Productivity = (Prod/Biom)*100) %>%
  ungroup() %>%
  #Mean for each site
  group_by(IDReef) %>%
  mutate(Biom = mean(Biom),
         Prod = mean(Prod),
         Productivity = mean(Productivity)) %>% 
  ungroup() %>%
  #joinin with transect data
  dplyr::select(IDReef, Biom, Prod, Productivity) %>%
  distinct(IDReef, .keep_all = T) %>%
  left_join(transect_info, by ="IDReef") %>%
  #Transforming data
  mutate(
    # log10ProdB = Productivity,
         log10ProdB = log10(Productivity+1),
         log10Biom = log10(Biom+1),
         log10Prod = log10(Prod+1),
         Latitude = as.numeric(as.character(Latitude)),
         Longitude = as.numeric(as.character(Longitude))) 
# dplyr::rename(site_code = IDReef)
# dplyr::select(IDReef, Biom, Prod, Productivity, Transect, Latitude, Longitude, Depth, Region, log10ProdB, log10Biom, log10Prod) %>%
# distinct(Transect, .keep_all = T)


print(paste("Minimum biomass:",min(data_prod_brut$Biom)))
print(paste("Maximum biomass:", max(data_prod_brut$Biom)))
print(paste("Minimum production:",min(data_prod_brut$Prod)))
print(paste("Maximum production:", max(data_prod_brut$Prod)))
print(paste("Minimum turnover:",min(data_prod_brut$Productivity)))
print(paste("Maximum turnover:", max(data_prod_brut$Productivity)))

# data_management---------------
biom75 = quantile(data_prod_brut$log10Biom,0.95)
biom25 = quantile(data_prod_brut$log10Biom,0.25)
prod75 = quantile(data_prod_brut$log10ProdB,0.75)
prod25 = quantile(data_prod_brut$log10ProdB,0.25)
max_biom = max(data_prod_brut$log10Biom)


#Diving data into 3 classes for each biomass/productivity relationship
management = data_prod_brut %>% 
  
  mutate(Class = ifelse(log10Biom < biom25 & log10ProdB < prod25,"deadzone",
                        ifelse(log10ProdB > prod75,"partial",
                               ifelse(log10Biom > biom75 & log10ProdB < prod75,"pristine","transition"))))%>%
  mutate(Class = as.factor(Class))


# plot by class--------------
#eje y = Biomass turnover, P/B × 100 (% per day)
# eje x <- log[standing biomass (g m–2)]
# filll = Class (Low biomass/turnover High turnover High biomass Mid-range)

ggplot(management, aes(x = log10Biom, y = log10ProdB, fill = Class)) +
  geom_point(shape = 21, size = 3, alpha = 0.7) +
  scale_fill_manual(values = c("#FF9900", "#3366CC", "#33CC33", "#9900CC")) +  # Colores personalizables
  labs(x = "log(standing biomass (g m^-2))", y = "Biomass Turnover (P/B × 100 % per day)") +
  theme_minimal()

# figures ---------------

traits_trophic = data_with_prod2 %>%
  dplyr::select(Species, TrophicLevelF, TrophicLevel,Functional_groups,Diet, MaxSizeTL )

RLS_prod_all_site = data_with_prod2 %>% 
  # dplyr::left_join(transect_info, by = "Transect") %>%
  dplyr::select(c(Species, Genus,Family,Quantity, Size, MaxSizeTL,W, Kmax,IDReef, Protection_level))

RLS_prod_all_site <- RLS_prod_all_site %>% distinct(Species, Genus,Family,Quantity, Size, MaxSizeTL,W, Kmax,IDReef, .keep_all = TRUE)
traits_trophic <- traits_trophic %>% distinct(Species, TrophicLevelF, TrophicLevel,Functional_groups,Diet, MaxSizeTL, .keep_all = TRUE) |> 
  select(-MaxSizeTL)

RLS_prod_figures = management %>%
  left_join(RLS_prod_all_site, by = "IDReef") %>%
  left_join(traits_trophic, by = "Species") |> 
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
  group_by(IDReef) %>%
  mutate(biom_by_site = sum(Biom)) %>%
  ungroup() %>%
  group_by(IDReef,Diet) %>%
  mutate(diet_biom = sum(Biom),
         diet_size = mean(Size)) %>%
  ungroup() %>%
  mutate(relative_diet = (diet_biom/biom_by_site)*100)

(plot_biomass = ggplot(management %>% 
                         # filter(Class != "transition") %>% 
                         dplyr::select(Class, IDReef, Diet, relative_diet) %>% distinct(), aes(Diet,relative_diet,fill=Diet))+
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
group.colors.diet <- c(`Piscivoro` = "#a30808", `Carnivoro` ="#b57560", `Herbivoro` = "#114a06",`Zooplanctivoro` = "#258f4e")
(diet_biomass = ggplot(RLS_prod_figures %>% 
                         # filter(Class != "transition") %>% 
                         dplyr::select(Class, IDReef, Diet, relative_diet) %>% distinct(), aes(Diet,relative_diet,fill=Diet))+
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
                      # filter(Class != "transition") %>% 
                      dplyr::select(Class, IDReef, Diet, diet_size) %>% distinct(), aes(Diet,diet_size,fill=Diet))+
    geom_jitter(alpha  = 0.3, size = 0.5,color='black',pch=21) +
    geom_boxplot(alpha  = 0.8) +
    scale_fill_manual(values = group.colors.diet) +
    scale_color_manual(values = group.colors.diet) +
    facet_wrap(~Class,labeller=labeller(Class=c("deadzone"=" ","partial"=" ","pristine"="", "transition" = " ")))+
    labs(x = "",
         y= "Mean fish size (cm)") +
    theme_minimal())

ggarrange(diet_biomass, diet_size, ncol = 1,common.legend = T)
# ggsave("Figures/diet_detail.pdf",width =297,height  =210, units="mm",dpi=600)

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
# ggsave("figures/Diet_by_class.png", width = 210, height = 297, units ="mm")


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
(ggplot(RLS_prod_figures,aes(log10Biom,log10ProdB,colour=Class))+
    geom_mark_hull(aes(fill = Class), con.cap= 0, expand = 0, radius = 0, con.size = 0)+
    geom_point(size=3,alpha=0.4)+
    scale_colour_manual(values=group.colors,labels=c("Low turnover/biomass reefs","High turnover","High Biomass reefs"))+
    scale_fill_manual(values=group.colors,labels=c("Low turnover/biomass reefs","Productive reefs","High Biomass reefs"))+
    labs(x="Biomass (g/m²) - log scale",
         y = "Productivity (%/year) - log scale")+
    theme_classic())

(ggplot(RLS_prod_figures,aes(log10Biom,log10ProdB))+
    geom_mark_hull(aes(fill = Class), con.cap= 0, expand = 0, radius = 0, alpha = 0.3)+
    scale_fill_manual(values=group.colors,labels=c("Low Productivity/Biomass reefs","Productive reefs","High Biomass reefs"))+
    ggnewscale::new_scale_fill() +
    geom_point(size=3,alpha=0.4,shape=21,color="black",aes(fill=Protection_level))+
    scale_fill_viridis_d()+
    labs(x="Biomass (g/m²) - log scale",
         y = "Productivity (%/year) - log scale",
         fill = "Protection status")+
    theme_classic())


# ggsave("Figures/prodB_biom.png",height=210, width= 297,units="mm")

data_family_genus = data_prod %>%
  dplyr::select(Family,Genus)%>%
  distinct(Genus, .keep_all = T)
