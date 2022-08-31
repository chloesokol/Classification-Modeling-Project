library(tidyverse)
library(skimr)
library(tidymodels)
library(corrplot)
library(RColorBrewer)
library(png)
tidymodels_prefer()

diabetes <- read_csv("data/diabetes_binary.csv")
diabetes <- diabetes %>%
  janitor::clean_names()
diabetes

set.seed(3012)

# Creating EDA data set
diabetes_initial_split <- diabetes %>%
  initial_split(prop = 0.50, strata = diabetes_binary)

# Creating the EDA set
diabetes_eda <- diabetes_initial_split %>% 
  testing()

# Creating testing/training set
diabetes_not_eda <- diabetes_initial_split %>% 
  training()

# EDA
skim_without_charts(diabetes_eda)

M <-cor(diabetes_eda)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))
# highest correlations between diabetes_binary, and 
# high_chol, age, high_bp, bmi, diff_walk, gen_hlth
diabetes_eda

diabetes <- diabetes %>%
  janitor::clean_names() %>%
  mutate(diabetes_binary = ifelse(diabetes_binary == 0, "No", "Yes"),
         diabetes_binary = factor(diabetes_binary, ordered = TRUE))

diabetes <- diabetes %>%
  janitor::clean_names() %>%
  mutate(high_chol = ifelse(high_chol == 0, "No", "Yes"),
         high_chol = factor(high_chol, ordered = TRUE))

diabetes <- diabetes %>%
  janitor::clean_names() %>%
  mutate(high_bp = ifelse(high_bp == 0, "No", "Yes"),
         high_bp = factor(high_bp, ordered = TRUE))

diabetes <- diabetes %>%
  janitor::clean_names() %>%
  mutate(diff_walk = if(diff_walk == 1) {diff_walk == "Excellent"},
         diff_walk = if(diff_walk == 2) {diff_walk == "Very Good"},
         diff_walk = if(diff_walk == 3) {diff_walk == "Good"},
         diff_walk = if(diff_walk == 4) {diff_walk == "Fair"},
         diff_walk = if(diff_walk == 5) {diff_walk == "Poor"},
         diff_walk = factor(diff_walk, ordered = TRUE))

diabetes <- diabetes %>%
  janitor::clean_names() %>%
  mutate(gen_hlth = factor(gen_hlth, ordered = TRUE))

diabetes_initial_split <- diabetes %>%
  initial_split(prop = 0.50, strata = diabetes_binary)

diabetes_eda <- diabetes_initial_split %>% 
  testing()

ggplot(diabetes_eda, aes(fill=high_chol, y=diabetes_binary, x=diabetes_binary)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Diabetes and High Cholesterol")

ggplot(diabetes_eda, aes(fill=high_bp, y=diabetes_binary, x=diabetes_binary)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Diabetes and High Blood Pressure")

ggplot(diabetes_eda, aes(fill=diff_walk, y=diabetes_binary, x=diabetes_binary)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Diabetes and Difficulty Walking")

ggplot(data = diabetes_eda, mapping = aes(x = factor(diabetes_binary), y = age)) +
  geom_boxplot() + 
  ggtitle("Diabetes and Age")

ggplot(data = diabetes_eda, mapping = aes(x = factor(diabetes_binary), y = bmi)) +
  geom_boxplot() + 
  ggtitle("Diabetes and Body Mass Index")

ggplot(diabetes_eda, aes(fill=gen_hlth, y=diabetes_binary, x=diabetes_binary)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Diabetes and State of Geeneral Health (1-5 Scale)")

