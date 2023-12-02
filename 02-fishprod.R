
install.packages("devtools")
library(devtools)
library(rfishprod)

devtools::install_github("renatoamorais/rfishprod")

# load data -----------------

tabla <- read_excel("data/tabla/spp_parametros_05092023.xlsx")|> 
  mutate(sstmean = 27) |> 
  filter(!is.na (Linf)) |>
  mutate(LinfTL = ifelse(is.na(LinfTL), Linf, LinfTL))|>
  mutate(Kmax = K * Linf / LinfTL)

fish <- readRDS("data/tabla/ltem_historic_20231109.RDS") |> 
  # filter(Year==2022, Region%in% c("La Paz", "La Ventana", "Loreto", "Los Cabos", "Cabo Pulmo")) |> 
  filter(Label == "PEC") |> 
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

merged_data <- merge(fish, tabla, by = c("Family", "Species", "A_ord", "B_pen"), all.x = TRUE) |>  
  #Remove sharks rays and seahorses
  # dplyr::filter(Taxa2 == "Actinopterygii" & Taxa2 != "Syngnathidae") |> 
  # filter(Family != "Anguillidae", Family != "Congridae", Family != "Muraenidae", Family != "Ophichthidae") |>  
  # #Removing small crypto 
  # filter(Family != "Chironemidae", Family != "Pinguipedidae", Family != "Chaenopsidae", Family != "Tripterygiidae",
  #        Family != "Gobiidae", Family != "Callionymidae", Family != "Blenniidae",
  #        Family != "Labrisomidae", Family != "Microdesmidae", Family != "Pholidichthyidae") |> 
  # mutate(Genus = word(Species,1)) |> 
  # mutate(Family = ifelse(Genus == "Rypticus", "Grammistidae", Family)) |> 
  rename(Diet = TrophicGroup) |> 
  filter(!Size == 0) |> 
  filter(!is.na (Linf)) |> 
  filter(!is.na (Size)) |> 
  filter(!is.na (SpecCode)) |> 
  filter(!is.na (MaxSizeTL)) 


ltem_db <- merged_data |> 
  filter(Year==2022, Region%in% c("La Paz", "La Ventana", "Loreto", "Los Cabos", "Cabo Pulmo")) |> 
  distinct(Family, Species, SpecCode, MaxSizeTL, a, b, Diet, 
           LinfTL, K, O, Kmax, Longitude, Latitude, sstmean, Method) 

ltem_repdata <- merged_data |> 
  filter(Year==2022, Region%in% c("La Paz", "La Ventana", "Loreto", "Los Cabos", "Cabo Pulmo")) |> 
  select(-Kmax)

# Predicting Kmax, the standardised VBGF parameter (Recommendation: use 100s to 1000s iterations) #
datagr <- predKmax(ltem_repdata,
                   dataset = ltem_db,
                   fmod = fmod,
                   niter = 10,
                   return = 'pred')

datagr <- datagr$pred

# Predicting M/Z: the instantaneous mortality rate (Recommendation: see help file for) #
# Using the Lorenzen equation
datagr$Md <- with (datagr, 
                   predM (Lmeas = Size, 
                          Lmax = MaxSizeTL, 
                          Kmax = Kmax, 
                          method = 'Lorenzen'))
# But also the Gislason one
datagr$Md <- with (datagr, 
                   predM (Lmeas = Size, 
                          Lmax = MaxSizeTL, 
                          Kmax = Kmax, 
                          method = 'Gislason'))


# Positioning your fish in their growth trajectory #
# aka. what's the size they're supposed to have on the next day? #
# now using L0, i.e., size at settlement, to constrain the curve #
# with(datagr, applyVBGF(Lmeas = Size,
#                        Lmax = MaxSizeTL,
#                        Kmax = Kmax,
#                        L0   = L0))


# Positioning your fish in their growth trajectory #
# aka. what's the size they're supposed to have on the next day? #
datagr$Size_nextday= with(datagr, rfishprod::applyVBGF (Lmeas = Size,
                                                        Lmax = MaxSizeTL,
                                                        Kmax = Kmax,
                                                        t = 1))

# Compare with their size on the previous day #
datagr$Size

# # Estimating gross somatic growth (g) #
# with(datagr, somaGain(a = a,
#                       b = b,
#                       Lmeas = Size,
#                       Lmax = MaxSizeTL,
#                       Kmax = Kmax))

# Estimating gross somatic growth (g) #
datagr$somatic_growth = with(datagr, rfishprod::somaGain (a = A_ord,
                                                          b = B_pen,
                                                          Lmeas = Size,
                                                          Lmax = MaxSizeTL,
                                                          Kmax = Kmax,
                                                          t = 1))


# Applying stochastic mortality #
applyMstoch(datagr$Md)

# 
# # Alternatively, estimating per capita mass loss due to mortality #
# with(datagr, somaLoss(M = Md,
#                       Lmeas = Size,
#                       a = a,
#                       b = b))


range(datagr$somatic_growth)

# Applying stochastic mortality #
datagr$mortality = rfishprod::applyMstoch(datagr$Md,t=1)


#Alternatively, estimating per capita mass loss due to mortality #
datagr$soma_loss = with(datagr, rfishprod::somaLoss (M = Md,
                                                     Lmeas = Size,
                                                     a = A_ord,
                                                     b = B_pen,
                                                     t = 1))


datagr_prod = datagr %>%
  #Calculating biomass turnover-----------------
mutate(W = A_ord*(Size^B_pen),
       Biom = (W*Quantity),
       Prod = ifelse(mortality == T, (somatic_growth * Quantity),0))