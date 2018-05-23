library(tidyverse)
library(stringr)

codes <- read_csv('~/Dropbox/relative_growth_rate/species_codes.csv')


labels <- expand.grid( species = 1:23, harvest = paste0('H', 1:4), treatment = 'C', rack = paste0('R', 1:10), germ = '' )

labels <- labels %>% merge(codes, by.x = 'species', by.y = 'experiment_id') %>% select( - current_name, -RGR_experiment, -drought)

labels$species <- str_pad(labels$species, width = 2, side = 'left', pad = '0')

labels %>% head

labels <- 
  labels %>% 
  mutate( rack_number = as.numeric(str_extract(rack, '\\d+'))) %>%
  mutate( harvest_number = as.numeric(str_extract(harvest, '\\d+'))) %>% 
  arrange( rack_number, harvest_number) %>% 
  select( - rack_number, -harvest_number)

labels <- labels %>% unite(label, c(species, harvest, treatment, rack, germ), sep = '-')


write_csv(labels, '~/Dropbox/relative_growth_rate/control_labels.csv')


drought_species <- codes %>% filter(drought == 1)
labels <- expand.grid( species = drought_species$experiment_id, harvest = paste0('H', 1:4), treatment = 'X', rep = 1:10, germ = '')

labels$species <- str_pad(labels$species, width = 2, side = 'left', pad = '0')
labels$rack <- head( paste0('R', sort(rep(12:17, 98))), 560)

labels <- 
  labels %>%
  mutate( experiment_id = as.numeric(str_extract(species, '\\d+'))) %>% 
  merge(codes, by = 'experiment_id') %>% 
  select( - current_name, -RGR_experiment, -drought, -experiment_id)

labels <- 
  labels %>% 
  mutate( rack_number = as.numeric(str_extract(rack, '\\d+'))) %>%
  mutate( harvest_number = as.numeric(str_extract(harvest, '\\d+'))) %>% 
  arrange(rack_number, rep, harvest_number) %>% 
  select( - rack_number, -harvest_number)


labels <- labels %>% unite(label, c(species, harvest, treatment, rack, germ), sep = '-')


write_csv(labels, '~/Dropbox/relative_growth_rate/drought_labels.csv')




# control floaters 
labels <- expand.grid( species = 1:23, harvest = paste0('H', '   '), treatment = 'C', rack = paste0('R', '   '), germ = '' )
labels$species <- str_pad(labels$species, width = 2, side = 'left', pad = '0')

labels <- labels %>% unite(label, c(species, harvest, treatment, rack, germ), sep = '-')

write_csv(labels, '~/Dropbox/projects/relative_growth_rate/floater_control_labels.csv')


# drought floaters 
drought_species <- codes %>% filter(drought == 1)
labels <- expand.grid( species = drought_species$experiment_id, harvest = 'H  ', treatment = 'X', rack = 'R  ',  germ = '')
labels$species <- str_pad(labels$species, width = 2, side = 'left', pad = '0')

labels <- labels %>% unite(label, c(species, harvest, treatment, rack, germ), sep = '-')

write_csv(labels, '~/Dropbox/projects/relative_growth_rate/drought_floater_labels.csv')
