# question 2

library(tidyverse)


# get names
var_names <- read_csv('data/biomarker-raw.csv', 
                      col_names = F, 
                      n_max = 2, 
                      col_select = -(1:2)) %>%
  t() %>%
  as_tibble() %>%
  rename(name = V1, 
         abbreviation = V2) %>%
  na.omit()


# read in data
biomarker_clean <- read_csv('data/biomarker-raw.csv', 
                            skip = 2,
                            col_select = -2L,
                            col_names = c('group', 
                                          'empty',
                                          pull(var_names, abbreviation),
                                          'ados'),
                            na = c('-', '')) %>%
  filter(!is.na(group)) %>%
  # log transform, center and scale, and trim
  mutate(across(.cols = -c(group, ados), 
                ~ trim(scale(log10(.x))[, 1], .at = 3))) %>%
  # reorder columns
  select(group, ados, everything())

library(dplyr)
temp <- biomarker_clean %>% 
  filter(if_any(CHIP:PLXB2, ~ . ==3))

temp %>% 
  count(group)

outlier_count <- temp %>% 
  rowwise() %>%
  mutate(outliers = sum(c_across(CHIP:PLXB2) == 3)) %>% 
  select(group, ados,outliers, everything())

outlier_count %>% 
  group_by(group) %>% 
  summarise(total_out = sum(outliers), avg_out = mean(outliers))

outlier_count %>% 
  ggplot(aes(x = group, y = outliers)) +
  geom_boxplot() +
  labs(title = 'Outliers per group',
       x = 'Group',
       y = 'Number of outliers') 

#About the same amount of people with at least one outlier in each group (ASD 74, TD 77)
#But TD have more individuals that have a higher amount of outliers (Boxplot)
#TD has a higher amount of total outliers as a group (ASD 813, TD 1161) and on average, an individual in TD would have more outliers (TD 15.1, ASD 11)