rm(list = ls())
library(googlesheets)
library(googledrive)
library(tidyverse)
library(lubridate)
library(stringr)

output_name <- ''

all_sheets <- gs_ls()
all_sheets$sheet_title

g_seeds <- gs_title('seed_weights')
seeds <- gs_read(g_seeds)

g_agb <- gs_title('harvest_data')
agb <- gs_read(g_agb)

g_species <- gs_title('RGR_species')
codes <- gs_read(g_species)


agb <- 
  agb %>% 
  mutate( germ_date = paste0(germ_date, '/2018'), 
          h_date = paste0(h_date, '/2018'), 
          germ_date = mdy( germ_date ), 
          h_date = mdy(h_date), 
          type = 'AGB') %>% 
  separate(label, c('experiment_id', 'harvest', 'treatment', 'rack', 'rep'))  %>% 
  mutate( n_days = h_date - germ_date) %>% 
  mutate( experiment_id = as.integer(experiment_id))


seeds <- 
  seeds %>%
  mutate( harvest = 'H0', 
          weight = g_per_seed, 
          n_days = 0) %>% 
  dplyr::select(experiment_id, harvest, n_days, weight)

seeds <- bind_rows(seeds %>% mutate( treatment = 'C'), seeds %>% mutate( treatment = 'X'))

growth_plot <- 
  bind_rows(seeds, agb) %>% 
  left_join(codes, by = 'experiment_id') %>%
  ggplot( aes( x = n_days, y = weight, color = treatment, linetype = treatment)) + 
  geom_point(alpha = 0.9) + 
  stat_summary(fun.y = 'mean', geom = 'line') + 
  scale_color_grey() + 
  facet_wrap(~current_code) + 
  theme_bw() + 
  ylab( 'Aboveground biomass (g per plant)')

growth_plot + scale_y_log10()

ggsave(growth_plot, filename = 'agb_growth.png', width = 8, height = 6)


# try testing differences in relative growth rate 


init_weights <- 
  bind_rows(seeds %>% 
            mutate( harvest = 'H1') %>% 
            rename( 'init_weight' = weight) %>% 
              dplyr::select( experiment_id, harvest, treatment, init_weight), 
          agb %>% 
            group_by( experiment_id, harvest, treatment ) %>% 
            summarise( init_weight = mean(weight, na.rm = T)) %>% 
            ungroup() %>% 
            mutate(harvest = ifelse(harvest == 'H1', 'H2', harvest), 
                   harvest = ifelse(harvest == 'H2', 'H3', harvest)))


all_dat <- 
  agb %>% 
  left_join(init_weights) 

rgr_dat <- 
  all_dat %>% 
  mutate( day_diff = ifelse(harvest == 'H2', n_days - 10, n_days)) %>% 
  mutate( rgr = (log(weight) - log(init_weight))/as.numeric(day_diff) ) %>% 
  left_join(codes)

lm(rgr ~ current_code, data = rgr_dat %>% filter(harvest == 'H1'))
lm(rgr ~ current_code, data = rgr_dat %>% filter(harvest == 'H2'))

rgr_dat %>% filter(harvest == 'H2') %>% dplyr::select(current_code, harvest, treatment, weight, init_weight, rgr)
rgr_dat %>% filter(harvest == 'H1') %>% dplyr::select(current_code, harvest, treatment, weight, init_weight, rgr)

vs <- sampsize$n
names(vs) <- sampsize$current_code

K <- multcomp::contrMat(vs, type = 'Tukey')

m1 <- aov( data = all_dat, log_growth ~ current_code, offset = n_days, contrasts = K)
summary(m1)
my_mct <- TukeyHSD(m1)

mcp_df <- 
  as.data.frame(my_mct$current_code) %>% 
  mutate( comp = row.names(.)) %>% 
  arrange(`p adj`)


K <- diag(length(vs))
m1 <- lm(data = all_dat, log_growth ~ 0  + current_code, offset = n_days)
test <- summary(m1)

mydf <- 
  data.frame( test$coefficients ) %>% 
  mutate( current_code = str_extract(row.names(.), '[A-Z]+'))

ggplot(mydf, 
       aes( x = current_code, 
            y= Estimate, 
            ymin = Estimate - Std..Error, 
            ymax = Estimate + Std..Error)) + 
  geom_point() + 
  geom_errorbar() 
