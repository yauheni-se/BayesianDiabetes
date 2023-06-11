# presets

# https://www.kaggle.com/datasets/mathchi/diabetes-data-set
library(dplyr)
library(stringr)
library(readr)
library(ROSE)

set.seed(82591)

# basic cleaning
data_clean <- read_csv("data/data.csv") %>% 
  `colnames<-`(str_to_lower(colnames(.)))

vapply(data_clean, function(x) {sum(is.na(x))}, integer(1))
table(data_clean$outcome)    # drop 50% of outcomes = 0 to make y balanced


# perform undersampling
data_undersampled <- ovun.sample(outcome~., data = data_clean, N=536, method = "under", seed = 82591)$data %>% 
  as_tibble()
table(data_undersampled$outcome)

data_undersampled <- data_undersampled %>% 
  rename(
    skin_thickness = skinthickness,
    diabetes_pedigree_func = diabetespedigreefunction,
    blood_pressure = bloodpressure,
    y = outcome
  ) %>% 
  select(y, everything())

# randomly sample data to be satisfy hometask requirements
data_y_0 <- data_undersampled %>% 
  arrange(y) %>% 
  filter(y == 0) %>% 
  sample_n(250)

data_y_1 <- data_undersampled %>% 
  arrange(y) %>% 
  filter(y != 0) %>% 
  sample_n(250)


data_final <- bind_rows(data_y_0, data_y_1)
data_final

write_csv(data_final, "data/data_cleared.csv")