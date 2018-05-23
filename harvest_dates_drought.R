rm(list = ls())
library(googlesheets)
library(googledrive)
library(tidyverse)
library(lubridate)
library(stringr)

output_name <- 'total_germ_drought'
output_figure <- 'cumulative_germ_drought'
script_name <- 'drought_germination_progress.R'

g_ss <- gs_title('drought_germination')

gdat <- gs_read(g_ss)

is.na(gdat$gdate)

ifelse( is.na(gdat$gdate), NA, paste(gdat$gdate, '/2018', sep = ''))

stringr::str_extract('testing99', '[0-9]+')

rn <- sample( 1:20, 20)
rn
sort(rn)
sort( as.character( rn))

temp <- gdat %>% 
  separate(col = label, into = c('species', 'harvest', 'treatment', 'rack'), sep = '-') %>%
  mutate( gdate = ifelse(is.na(gdate), NA, paste(gdate, '/2018', sep = ''))) %>% 
  mutate( gdate = as.Date(gdate, '%m/%d/%Y')) %>% 
  mutate( hdate = as.Date(hdate)) %>% 
  mutate( hdate = as_date(ifelse(harvest == 'H1', gdate + 10, hdate))) %>% 
  arrange( hdate, as.numeric(str_extract(rack, '[0-9]+')))

harvest_5_2_H1 <- 
  temp %>% 
  filter( hdate == '2018-05-02')

write_csv(harvest_5_2_H1, 'drought_harvest1_2018-5-02.csv')


temp <- gdat %>% 
  separate(col = label, into = c('species', 'harvest', 'treatment', 'rack'), sep = '-') %>%
  mutate( gdate = ifelse(is.na(gdate), NA, paste(gdate, '/2018', sep = ''))) %>% 
  mutate( gdate = as.Date(gdate, '%m/%d/%Y')) %>% 
  mutate( hdate = as.Date(hdate)) %>% 
  mutate( hdate = as_date(ifelse(harvest == 'H2', gdate + 17, hdate))) %>% 
  arrange( hdate, as.numeric(str_extract(rack, '[0-9]+')))




harvest_5_12_H2 <- 
  temp %>% 
  filter( hdate == '2018-05-12')

write_csv(harvest_5_12_H2, 'drought_harvest2_2018-05-12.csv')

temp <- gdat %>% 
  separate(col = label, into = c('species', 'harvest', 'treatment', 'rack'), sep = '-') %>%
  mutate( gdate = ifelse(is.na(gdate), NA, paste(gdate, '/2018', sep = ''))) %>% 
  mutate( gdate = as.Date(gdate, '%m/%d/%Y')) %>% 
  mutate( hdate = as.Date(hdate)) %>% 
  mutate( hdate = as_date(ifelse(harvest == 'H3', gdate + 24, hdate))) %>% 
  arrange( hdate, as.numeric(str_extract(rack, '[0-9]+')))

harvest_5_14_H3 <- 
  temp %>% 
  filter( hdate == '2018-05-14')

write_csv(harvest_5_14_H3, 'drought_harvest3_2018-05-14.csv')

harvest_5_16_H3 <- 
  temp %>% 
  filter( hdate == '2018-05-16')

write_csv(harvest_5_16_H3, 'drought_harvest3_2018-05-16.csv')

