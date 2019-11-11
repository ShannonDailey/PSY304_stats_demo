
library(tidyverse)

our_data <- read_csv("data_prep/our_data.csv") %>% 
  rename(gender = child_gender,
         age = child_age,
         soc_particip = social_participation) %>% 
  filter(sample == "1") %>% 
  dplyr::select(-sample, -observer, -child_num) 

our_data$cohort <- 7

our_data_clean <- our_data %>% 
  mutate(cohort = factor(cohort),
         gender = fct_recode(factor(gender),
                                    "male" = "m",
                                    "female" = "f")) %>% 
   dplyr::select(cohort, gender, age,
                 unoccupied, solitary, onlooker, parallel, associative, cooperative,
                 soc_particip, bossy, pos_affect, awkward)

summary(our_data_clean)
head(our_data_clean)
dim(our_data_clean)


spss <- read_csv("data_prep/spss_export.csv") %>% 
  mutate(Gender = fct_recode(factor(Gender),
                             "male" = "1",
                             "female" = "2"),
         Cohort = factor(Cohort)) %>% 
  na_if("#NULL!") %>% 
  mutate(pos_affect = as.numeric(degpos),
         bossy = as.numeric(bossy),
         awkward = as.numeric(socawk)) %>%
  rename(soc_particip = socparticip) %>% 
  dplyr::select(-Predom, -degpos, -socawk)

library(janitor)
spss_clean <- janitor::clean_names(spss, case = "snake") %>% 
  dplyr::select(cohort, gender, age,
                unoccupied, solitary, onlooker, parallel, associative, cooperative,
                soc_particip, bossy, pos_affect, awkward)

dim(spss_clean)
colnames(spss_clean)

dim(our_data_clean)
colnames(our_data_clean)

demo_data <- spss_clean %>%
  rbind(our_data_clean) %>% 
  tibble::rowid_to_column("child")

summary(demo_data)
dim(demo_data)
head(demo_data)

# Calculate predominant style
predom_style <- demo_data %>%
  dplyr::select(child,unoccupied:cooperative) %>% 
  pivot_longer(cols = unoccupied:cooperative, names_to = "type_play", values_to = "num_bins") %>% 
  group_by(child) %>%
  mutate(max = max(num_bins)) %>%
  filter(num_bins == max) %>% 
  dplyr::select(child,type_play) %>% 
  rename(predom = type_play)

exclude <- predom_style %>%
  mutate(is_duplicate = duplicated(child)) %>%
  filter(is_duplicate == TRUE) %>%
  pull(child)

predom_style <- predom_style %>%
  filter(!child %in% exclude) %>% 
  mutate(predom = factor(predom))

dim(predom_style)

data_final <- demo_data %>%
  left_join(predom_style) %>% 
  mutate(prop_unocc = unoccupied/30,
         prop_solitary = solitary/30,
         prop_onlooker = onlooker/30,
         prop_parallel = parallel/30,
         prop_assoc = associative/30,
         prop_coop = cooperative/30)

dim(data_final)

write_csv(data_final, "demo_data.csv")
