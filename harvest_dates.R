rm(list = ls())
library(googlesheets)
library(googledrive)
library(tidyverse)
library(lubridate)
library(stringr)

output_name <- 'total_germ_control'
output_figure <- 'cumulative_germ_control'
script_name <- 'control_germination_progress.R'

g_ss <- gs_title('control_germination')

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




temp <- gdat %>% 
  separate(col = label, into = c('species', 'harvest', 'treatment', 'rack'), sep = '-') %>%
  mutate( gdate = ifelse(is.na(gdate), NA, paste(gdate, '/2018', sep = ''))) %>% 
  mutate( gdate = as.Date(gdate, '%m/%d/%Y')) %>% 
  mutate( hdate = as.Date(hdate)) %>% 
  mutate( hdate = as_date(ifelse(harvest == 'H2', gdate + 17, hdate))) %>% 
  arrange( hdate, as.numeric(str_extract(rack, '[0-9]+')))



harvest_5_9_H2 <- 
  temp %>% 
  filter( hdate == '2018-05-09')

write_csv(harvest_5_9_H2, 'control_harvest_2018-5-9_H2.csv')

harvest_5_10_H2 <- 
  temp %>% 
  filter( hdate == '2018-05-10')

write_csv(harvest_5_10_H2, 'control_harvest_2018-5-10_H2.csv')

harvest_5_11_H2 <- 
  temp %>% 
  filter( hdate == '2018-05-11')

write_csv(harvest_5_11_H2, 'control_harvest_2018-5-11_H2.csv')

harvest_5_12_H2 <- 
  temp %>% 
  filter( hdate == '2018-05-12')

write_csv(harvest_5_12_H2, 'control_harvest_2018-5-12_H2.csv')

harvest_5_13_H2 <- 
  temp %>% 
  filter( hdate == '2018-05-13')

write_csv(harvest_5_13_H2, 'control_harvest_2018-5-13_H2.csv')

temp <- gdat %>% 
  separate(col = label, into = c('species', 'harvest', 'treatment', 'rack'), sep = '-') %>%
  mutate( gdate = ifelse(is.na(gdate), NA, paste(gdate, '/2018', sep = ''))) %>% 
  mutate( gdate = as.Date(gdate, '%m/%d/%Y')) %>% 
  mutate( hdate = as.Date(hdate)) %>% 
  mutate( hdate = as_date(ifelse(harvest == 'H3', gdate + 24, hdate))) %>% 
  arrange( hdate, as.numeric(str_extract(rack, '[0-9]+')))

harvest_5_11_H3 <- 
  temp %>% 
  filter( hdate == '2018-05-11')

write_csv(harvest_5_11_H3, 'control_harvest_2018-5-11_H3.csv')

harvest_5_11_H3 <- 
  temp %>% 
  filter( hdate == '2018-05-11')

write_csv(harvest_5_11_H3, 'control_harvest_2018-5-11_H3.csv')

harvest_5_12_H3 <- 
  temp %>% 
  filter( hdate == '2018-05-12')

write_csv(harvest_5_12_H3, 'control_harvest_2018-5-12_H3.csv')

harvest_5_13_H3 <- 
  temp %>% 
  filter( hdate == '2018-05-13')

write_csv(harvest_5_13_H3, 'control_harvest_2018-5-13_H3.csv')

harvest_5_14_H3 <- 
  temp %>% 
  filter( hdate == '2018-05-14')

write_csv(harvest_5_14_H3, 'control_harvest_2018-5-14_H3.csv')
