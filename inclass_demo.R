
##### SETUP #####
# Load the libraries we'll need
library(tidyverse)

# Read in the data
data <- read_csv("demo_data.csv") %>% 
  mutate(predom = factor(predom))

##### DESCRIPTIVES #####
# First things first -- let's take a look!
dim(data) # How many rows and columns?
View(data) # This opens a new tab with the data frame
summary(data) # This gives us a basic summary of the data frame

# Looks like we have some missing data!

# Who are the participants?
nrow(data)

# Ages
table(data$age)

# Gender
addmargins(table(data$age, data$gender), FUN = list(Total = sum), quiet = TRUE)

ggplot(data, aes(x= age, fill = gender))+
  geom_histogram(bins = 3, position = "dodge")

# Predominant play style
table(data$predom)

##### AGE DIFFERENCES #####
# Predominant style by age
table(data$predom, data$age)

ggplot(data %>% filter(!is.na(predom)), aes(x = age, fill = predom))+
  geom_bar(position = "fill")+
  labs(y = "Proportion of children", x = "Age (years)", fill = "Predominant play style")

age_predom_table <- table(data %>% dplyr::select(age, predom))
chisq.test(age_predom_table)


# Types of play by age
props_long <- data %>%
  dplyr::select(child,age,gender,prop_unocc:prop_coop) %>% 
  pivot_longer(cols = prop_unocc:prop_coop, names_to = "type_play",
               names_prefix = "prop_", values_to = "prop") %>% 
  mutate(type_play = factor(type_play))

ggplot(props_long, aes(x = age, y = prop, color = type_play))+
  stat_summary(geom = "pointrange")+
  geom_smooth(method = "lm")+
  theme_bw()

aov_age_coop <- aov(prop ~ age, data = props_long %>% filter(type_play == "coop"))
summary(aov_age_coop)

aov_age_solitary <- aov(prop ~ age, data = props_long %>% filter(type_play == "solitary"))
summary(aov_age_solitary)


##### GENDER DIFFERENCES #####
# Gender differences in predominant style
table(data$predom, data$gender)

ggplot(data %>% filter(!is.na(predom)), aes(x = gender, fill = predom))+
  geom_bar(position = "fill")+
  labs(y = "Proportion of children", x = "Gender", fill = "Predominant play style")

gender_predom_table <- table(data %>% dplyr::select(gender, predom))
chisq.test(gender_predom_table)

# Gender differences in types of play
props_gender <- props_long %>% group_by(gender,type_play) %>% summarise(mean_prop = mean(prop, na.rm = TRUE))

ggplot(props_long, aes(x = gender, y = prop, color = type_play))+
  stat_summary(geom = "pointrange")+
  theme_bw()

t.test(data = props_long %>% filter(type_play == "coop"), prop ~ gender)

t.test(data = props_long %>% filter(type_play == "solitary"), prop ~ gender)
