rm(list = ls())
library(googlesheets)
library(googledrive)
library(tidyverse)
library(lubridate)
library(stringr)

g_ss <- gs_title('control_germination')

gdat <- gs_read(g_ss)

controls <- gdat %>% 
  separate(col = label, into = c('species', 'harvest', 'treatment', 'rack'), sep = '-') %>%
  mutate( gdate = ifelse(is.na(gdate), NA, paste(gdate, '/2018', sep = ''))) %>% 
  mutate( gdate = as.Date(gdate, '%m/%d/%Y')) %>% 
  mutate( hdate = as.Date(hdate)) %>% 
  mutate( hdate = as_date(ifelse(harvest == 'H1', gdate + 10, hdate))) %>% 
  arrange( hdate, as.numeric(str_extract(rack, '[0-9]+')))

g_ss <- gs_title('drought_germination')

gdat <- gs_read(g_ss)

droughts <- 
  gdat %>%
  separate(col = label, into = c('species', 'harvest', 'treatment', 'rack'), sep = '-') %>%
  mutate( gdate = ifelse(is.na(gdate), NA, paste(gdate, '/2018', sep = ''))) %>% 
  mutate( gdate = as.Date(gdate, '%m/%d/%Y')) %>% 
  mutate( hdate = as.Date(hdate)) %>% 
  mutate( hdate = as_date(ifelse(harvest == 'H1', gdate + 10, hdate))) %>% 
  arrange( hdate, as.numeric(str_extract(rack, '[0-9]+'))) %>% 
  select( -rep)



g_ss <- gs_title('floater_germination')
gdat <- gs_read(g_ss)

floaters <- 
  gdat %>%
  separate(col = label, into = c('species', 'treatment', 'rack', 'rep'), sep = '-') %>%
  mutate( gdate = ifelse(is.na(gdate), NA, paste(gdate, '/2018', sep = ''))) %>% 
  mutate( gdate = as.Date(gdate, '%m/%d/%Y'))  %>% 
  mutate( harvest = NA) %>% 
  select( - rep ) 

all_dat <- rbind( controls, droughts, floaters)

total_germination <- 
  all_dat %>% 
  group_by(species) %>% 
  summarise( total_germinants = sum( !is.na(gdate) )) 

species_list <- read_csv('species_codes.csv')

species_list <- species_list %>% 
  mutate( species = str_pad(experiment_id, width = 2, side = 'left', pad = '0'))

total_germination <- 
  merge( total_germination, species_list, by = 'species') %>% 
  mutate( total_required = RGR_experiment*40 + drought*40 ) %>% 
  mutate( missing = total_required - total_germinants ) %>% 
  select( species, current_code, total_germinants, total_required, missing)


germination_per_harvest <- 
  all_dat %>% 
  mutate( harvest = ifelse(is.na(harvest), 'floater', harvest)) %>% 
  mutate( treatment = ifelse( harvest == 'floater', 'C', treatment)) %>% 
  group_by( species, treatment, harvest ) %>% 
  summarise( total = sum( !is.na(gdate))) %>% 
  group_by( species ) %>% 
  filter( any(total < 10 & harvest != 'floater' )) %>% 
  spread( harvest, total ) %>% 
  merge( species_list %>% select( species, current_code), by = 'species') %>% 
  select( species, current_code, treatment, H1:H4, floater) %>% 
  group_by(species) %>% 
  mutate( enough = ifelse( n()*40 - (sum(H1) + sum(H2) + sum(H3) + sum(H4)) <= floater, T, F))
 
write_csv(germination_per_harvest, 'germination_per_harvest.csv')





