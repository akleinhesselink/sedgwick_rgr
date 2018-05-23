rm(list = ls())
library(googlesheets)
library(googledrive)
library(tidyverse)

output_name <- 'total_germ_control'
output_figure <- 'cumulative_germ_control'
script_name <- 'control_germination_progress.R'

g_ss <- gs_title('control_germination')

gdat <- gs_read(g_ss)

gdat <- 
  gdat %>% 
  separate(label, into =  c('species', 'harvest', 'treatment', 'rack')) %>% 
  mutate( gdate = ifelse(!is.na(gdate), paste0( gdate, '/2018'), gdate)) %>% 
  mutate( gdate = lubridate::mdy(gdate)) %>%
  mutate( germinated = !is.na(gdate))

germ_totals <- 
  gdat %>% 
  group_by( species, current_code, harvest, treatment) %>% 
  summarise( total_germ = sum(germinated))  %>% 
  arrange( species, harvest) %>% 
  mutate( low = ifelse( total_germ < 8, '***', ''))

write.csv(germ_totals, file = paste0(output_name, '.csv'), row.names = F)

# germ_totals <- gs_new(output_name,
#                        ws_title = output_name) ## create google spreadsheet

# germ_totals <- gs_title(output_name)
# 
# germ_totals %>% 
#   gs_edit_cells(ws = output_name, 
#                 input = control_totals)

# drive_mv(output_name, 'RGR_experiment/') # move file to RGR folder

# make a cumulative germination plot ------------------------------------ # 

cumulative_germination <- 
  gdat %>% 
  group_by(species) %>% 
  arrange( species, gdate ) %>% 
  mutate( total_germ = cumsum(germinated)/n())

gg_cumulative <- 
  cumulative_germination %>% 
  ggplot( aes( x = gdate, y = total_germ)) + 
  geom_step() + 
  facet_wrap(~current_code, ncol = 6) + 
  ylab('total germination prop.') + 
  theme(axis.text.x = element_text(angle = -65, hjust = 0)) + 
  ggtitle(paste0(output_figure, '  ', Sys.Date()))

gg_cumulative

fig_file <- paste0(output_figure, '.png')

ggsave(gg_cumulative, 
       filename = fig_file, height = 6, width = 8)

# drive_rm(fig_file) # remove old files 
# drive_rm(fig_file)
# drive_upload(fig_file, 'RGR_experiment/') # upload new files 
# drive_upload(script_name, 'RGR_experiment/')  # save this R script to google drive 

