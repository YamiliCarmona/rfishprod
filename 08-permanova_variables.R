
merged_data <- readRDS("data/fishdata_productivity-by-reef-allyears.RDS") |> 
  filter(Habitat %in% c("BLOQUES", "PARED"))
  

merged_data |> 
  group_by(Habitat) |>
  summarise(n=n_distinct(Reef), Years = n_distinct(Year))

table <- merged_data >
  select(IDReef, Reef, Year, Habitat, Depth, Depth2, Degree, sstmean) %>%
  unique()

glimpse(merged_data)

# sample_data <- merged_data %>%
#   group_by(Degree, Depth2, Habitat, Reef, Transect, Species) %>%
#    summarise(sstmean= mean(sstmean),
#             Prod = mean(Prod, na.rm =T)
#             )%>%
#   pivot_wider(names_from=)

sample_data <- merged_data %>%
  group_by(Degree, Depth2, Habitat, Reef, Transect, Species) %>%
  summarise(sstmean = mean(sstmean),
             Prod = mean(Prod, na.rm=T)
             )

ggplot(sample_data, aes(x=Habitat, y=Prod)) +
  geom_boxplot() +
  facet_grid(Depth2~ Degree)

sample_data_matrix <- sample_data %>%
  pivot_wider(names_from = "Species", values_from ="Prod") %>%
  mutate_all(-replace_na(., 0)) %>%
  ungroup() %>%
  select(-c(Degree:sstmean))
unique(factor(env_data_matrix$Degree))

env_data_matrix <- sample_data %>%
  pivot_wider(names_from = "Species", values_from = "Prod") %>%
  mutate_all(-replace_na(., 0)) %>%
  ungroup() %>%
  select(Degree:sstmean) %>%
  mutate(Degree =  factor(Degree, Levels = c("23", "24", "25", "26"), ordered = T),
         Depth2 = factor(Depth2, Levels = c("Shallow", "Deep"), ordered = T),
         Habitat = factor(Habitat)) |> 
  select(Degree, Depth2, Habitat)
         
# Realizar PERMANOVA con 'Habitat', 'Depth' y 'Degree' como variables explicativas
         
permanova_result <- adonis2(sample_data_matrix~ ,
                            data = sample_data_matrix, method = "euclidean")

summary(permanova_result)
