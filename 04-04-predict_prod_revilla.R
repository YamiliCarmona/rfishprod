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


# Data parametros, temperatura, peces

# sst_data <- readRDS("data/tabla/sst_for_reefs.RDS")  # añadir datos de temepratura para Revillagigedo, Islas Marias, Los Cabos

tabla <- read_excel("data/tabla/spp_parametros_20231201.xlsx")|> 
  select(-MaxSizeTL) |>
  filter(!is.na (Linf), !is.na (LinfTL))

t <- 1


# data fish ------------

fish <- readRDS("data/tabla/ltem_historic_20231109.RDS") |> 
  filter(Label == "PEC") |>
  filter(Region%in% c ("Revillagigedo","Los Cabos")) |> 
  mutate(
    A_ord = as.numeric(A_ord),
    B_pen= as.numeric(B_pen),
    Quantity = as.numeric(Quantity),
    Size=as.numeric(Size),
    Area= as.numeric(Area),
    Month= as.numeric(Month),
    Biomass = (Quantity * A_ord* (Size^B_pen))/(Area * 100)) |> 
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


unique(fish$Region)



sst_means <- data.frame(Region = c("Los Cabos", "Revillagigedo"),
                        sstmean = c(24.7, 24 ))


fish <- fish |> 
  filter((Region == "Los Cabos" & Year == 2022) |
           (Region == "Revillagigedo" & Year == 2023)
           # (Region == "Islas Marias" & Year == 2018)
         ) |> 
  left_join(sst_means, by = "Region") |> 
  mutate(Island = str_replace_all(Island, c("Clarión" = "Clarion", "Partida Revillagigedo" = "Roca Partida")))


# Tamaños maximos observados cada año ------------

TallaMax <- fish |> 
  # filter(!Year == 2023) |> 
  filter(!is.na(Size)) |> 
  group_by(Year, Region, Species) |> 
  arrange(Size) |> 
  mutate(MaxSizeTL = max(Size)) |>
  ungroup()

MaxSizesp <- TallaMax |> 
  distinct(Year, Region, Species, MaxSizeTL)


tibus <- fish |> 
  filter(Taxa2 == "Elasmobranchii") |> 
  distinct(Region, Species, Quantity, Size)

# merge ----------

merge_database <- merge(TallaMax, tabla, by = c("Family", "Species", "A_ord", "B_pen"), all.x = TRUE)  |> 
  rename(Diet = TrophicGroup, a = "A_ord",
         b = "B_pen") |> 
  #Remove sharks rays and seahorses
  dplyr::filter(Taxa2 == "Actinopterygii" & Taxa2 != "Syngnathidae") |>
  filter(Family != "Anguillidae", Family != "Congridae", Family != "Muraenidae", Family != "Ophichthidae") |>
  #Removing small crypto
  filter(Family != "Chironemidae", Family != "Pinguipedidae", Family != "Chaenopsidae", Family != "Tripterygiidae",
         Family != "Gobiidae", Family != "Callionymidae", Family != "Blenniidae",
         Family != "Labrisomidae", Family != "Microdesmidae", Family != "Pholidichthyidae") |>
  # filtering to keep fish larger than 5cm and fish of 5cm if Maxlength <25cm
  filter (Size != 5 | (Size==5 & MaxSizeTL < 25)) |> 
  filter(!Size == 0) |>
  filter(!is.na (Linf)) |>
  filter(!is.na (SpecCode)) |> 
  filter(!is.na(sstmean))

str(merge_database)

tallas <- merge_database |> 
  # filter(Reef == "ANEGADA") |> 
  distinct(Region, Species, Quantity, Size)

# Formula from Morais and Bellwood (2018) #--------
# fmod <- formula(~ sstmean + MaxSizeTL)
# fmod <- formula(~ sstmean + MaxSizeTL + Diet)
fmod <- formula(~ sstmean + MaxSizeTL + Diet + Method) # normalmente uso esta, pero a veces, con varios años marca que no crecen


ltem_db <- merge_database |> 
  distinct(Family, Species, SpecCode, MaxSizeTL, a, b, Diet, 
           LinfTL, K, Kmax, O, Longitude, Latitude, sstmean, Method) 

ltem_repdata <- merge_database |> 
  select(-Kmax)


# Predecir Kmax. Si  no me deja correr la función sin la columna Kmax, que datos debo poner? se calcula aparte?
# usé los datos de K
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



transect_info = ltem_repdata %>% 
  dplyr::select(Year, Transect, Latitude, Longitude, IDReef, Depth, Region)

transect_site = transect_info %>% dplyr::select(Transect, IDReef, Depth) %>%
  dplyr::filter(Transect %in% datagr_prod$Transect) 


# At the scale of the community (transect)---------
data_prod_brut = datagr_prod %>%
  #Sum for each transect
  group_by(Year,Reef,Depth,Transect) %>%
  mutate(Biom = sum(Biom)/250,# (kg ha^−1) # porqué 500?
         Prod = sum(Prod)/250,#g d^−1 ha^−1
         #Individual Biomass
         IndBiom = (W/Area),
         Productivity = (Prod/Biom)*100) %>%
  ungroup() %>%
  #Mean for each site
  group_by(Year, Reef) %>%
  mutate(Biom = mean(Biom),
         Prod = mean(Prod),
         Productivity = mean(Productivity)) %>% 
  ungroup() %>%
  #joinin with transect data
  # dplyr::select(IDReef, Reef, Biom, Prod, Productivity) %>%
  # distinct(IDReef, .keep_all = T) %>%
  # left_join(transect_info, by ="IDReef") %>%
  #Transforming data
  mutate(
    log10ProdB = Productivity,# % per day
    log10Biom = log10(Biom+1), #(g m -2)
    log10Prod = log10(Prod+1), # (g m-2 d-1)
    Latitude = as.numeric(as.character(Latitude)),
    Longitude = as.numeric(as.character(Longitude)))
# dplyr::select(Year, Degree, Region, IDReef, Reef, Habitat, Transect, Depth, Depth2, sstmean, Biom, Prod, Productivity, log10ProdB, log10Biom, log10Prod) %>%
# distinct(Year, Degree, Region, IDReef, Reef, Habitat, Transect, Depth, Depth2, sstmean, Biom, Prod, Productivity, log10ProdB, log10Biom, log10Prod, .keep_all = T)

str(data_prod_brut)

#' #' Calculating productivity------------------
#' 
# data_with_prod2 =  datagr |>
#   #Calculating production -----------------
#   #Weight of fish at time of census
#   mutate(W = a*Size^b,
#          #Age of fish at time of census
#          t = (1/Kmax)*log((MaxSizeTL)/((1-(Size/MaxSizeTL))*MaxSizeTL)),
#          #Projected size one year later
#          Ltx = MaxSizeTL * (1-exp(-Kmax*(t+365))),
#          #Projected weight one year later
#          Wtx = a*Ltx**b,
#          #Production
#          Wgain = Wtx - W,
#          #Biomass------
#          # Biomass = (a * (Size^b) * Quantity) / (1000 * Area),
#          Biom = (W*Quantity)/Area,
#          #Individual Biomass
#          IndBiom = (W/Area),
#          #Production-----------
#          # Prod = (Wgain * Biom)/Area,
#          Prod = (Wgain * Quantity)/Area,
#          #Individual production
#          IndProd = (Wgain/Area)) %>%
# 
#   filter(!is.na(Prod))

# saveRDS(data_prod_brut, "data/fishdata_productivity-by-reef-allyears.RDS")



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

range(management$Productivity)
range(management$Biom)
range(management$Prod)


unique(management$Region)
# plot by class--------------

unique_points <- management[!duplicated(management$Reef), ]

ggplot(management, aes(x = log10Biom, y = log10ProdB, fill = Class)) +
  geom_point(shape = 21, size = 3, alpha = 0.7) +
  scale_fill_manual(values = c("deadzone" = "#FF9900", "partial" = "#3366CC", "pristine" = "#33CC33", "transition" = "#9900CC"),
                    labels = c("deadzone" = "Low biomass/turnover", "partial" = "High turnover", "pristine" = "High biomass", "transition" = "Transition")) +
  labs(x = "log(standing biomass (g m^-2))", y = "Biomass Turnover (P/B × 100 % per day)") +
  theme_minimal() +
  ggrepel::geom_text_repel(data = unique_points, aes(label = Reef), box.padding = 0.5, point.padding = 0.5, size = 1.5, max.overlaps = 300)

management |> 
  filter(Class == "pristine") |>
  distinct(Reef, Class, log10ProdB, log10Biom)


# Figuras ---------------

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
  ggrepel::geom_text_repel(data = unique_points, aes(label = Reef), box.padding = 0.5, point.padding = 0.5, size = 1.5, max.overlaps = 300)

# ylim(0, 0.6) 

# ggsave("figs/ltem_prod_pnr-2023-csl.png", width = 8.5, height = 4.5, dpi=1000)

unique(management$Year)

group.colors <- c(deadzone = "#d69d4e", partial = "#046c9a", pristine = "#C1DB60", transition = "lightblue")

unique(fish$Protection_status)

management <- management |>
  # mutate(Protection_status = recode(Protection_status,
  #                                   "cabo pulmo" = "Fully protected",
  #                                   "sin proteccion" = "Not protected",
  #                                   "area protegida" = "Lightly protected")) |>
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
    # scale_fill_manual(values = c("Fully protected" = "#258f4e", 
    #                              "Not protected" = "#808080",   
    #                              "Lightly protected" = "yellow2"))+  
    labs(x="Biomass (g/m²) - log scale",
         y = "Productivity (%/year) - log scale",
         fill = "Protection status")+
    theme_classic())+
  ggrepel::geom_text_repel(data = unique_points, aes(label = Reef), box.padding = 0.5, point.padding = 0.5, size = 2, max.overlaps = 300)
# ylim(0, 0.6)



#  Class y Protection_status --------
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
