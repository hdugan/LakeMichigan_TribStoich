# Load libraries
library(tidyverse)
library(patchwork)
source('Functions/loadMapdata.R')

# Load data
hydroid.order = read_csv('Data/hydroid_order2.csv')
raw = read_csv('Data/LMtribs_seasonal.csv') %>% 
  left_join(hydroid.order)

routeorder = raw %>% group_by(hydroid) %>%
  summarise(latitude = first(latitude), longitude = first(longitude), ag = first(ag)) %>%
  mutate(row = row_number()) %>%
  ungroup() %>%
  left_join(hydroid.order) %>%
  arrange(order)

# # Test plot route order
# ggplot(routeorder) +
#   geom_path(aes(x = longitude, y = latitude)) +
#   geom_point(aes(x = longitude, y = latitude, col = ag), alpha = 1) +
#   scale_color_viridis_c()

hydroid.levels = raw %>% arrange(order) %>%
  distinct(hydroid) %>%
  pull()

df = read_csv('Data/LMtribs_seasonal.csv') %>% 
  arrange(areakm2) %>% # arrange by area (could arrange by something else)
  group_by(hydroid, NAME, season) %>% 
  summarise_at(vars(srp_mgl:driftthickness_mean_meters), mean, na.rm = TRUE) %>%
  ungroup() %>% 
  
  mutate(hydroid = factor(hydroid, levels = hydroid.levels)) %>% 
  mutate(season = factor(season, levels = c('winter','spring','summer','fall'))) %>% 
  mutate(areaOrder = as.factor(row_number(areakm2))) %>% 
  mutate(agOrder = as.factor(row_number(ag))) %>% 
  mutate(np = (nitrate_mgl/14.01)/(srp_mgl/30.97)) %>% 
  mutate(np_group = case_when(np > 100 ~ '> 100',
                              np > 30 & np <= 100 ~ '30-100',
                              np > 10 & np <= 30 ~ '10-30',
                              np < 10 ~ '< 10')) %>% 
  mutate(np_group_value = case_when(np > 100 ~ 4,
                              np > 30 & np <= 100 ~ 3,
                              np > 10 & np <= 30 ~ 2,
                              np < 10 ~ 1)) %>% 
  mutate(np_group = factor(np_group, levels = c('< 10', '10-30','30-100','> 100'))) %>% 
  ungroup()

# plot(df$hydroid, as.numeric(df$areaOrder))
# ggplot(df) +
#   geom_bar(aes(x = hydroid, y = nitrate_mgl, fill=areakm2), stat="identity", color="white") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

df |> filter(season == 'winter') |> 
  group_by(np_group_value) |> tally()

df |> filter(season == 'fall') |> 
  group_by(np_group_value) |> tally()/102

##################### PLOTTING #######################################
plotseasonNP <- function(useseason, col) {
  ggplot(df %>% filter(season == useseason)) +
    geom_bar(aes(x = hydroid, y = -np_group_value, fill = np_group), width = 0.85, stat="identity", col = NA) +
    geom_hline(yintercept = c(-1,-2,-3), colour = "grey90", size = 0.2) +
    # geom_hline(yintercept = 0.2, colour = col, size = 1) +
    # geom_hline(yintercept = 0.2, colour = col, size = 1) +
    
    geom_segment(aes(x=1,xend=12,y=0.2,yend=0.2), col = '#89c2d9', size = 1) +
    geom_segment(aes(x=13,xend=22,y=0.2,yend=0.2), col = '#61a5c2', size = 1) +
    geom_segment(aes(x=23,xend=33,y=0.2,yend=0.2), col = '#468faf', size = 1) +
    geom_segment(aes(x=34,xend=59,y=0.2,yend=0.2), col = '#2a6f97', size = 1) +
    geom_segment(aes(x=60,xend=77,y=0.2,yend=0.2), col = '#014f86', size = 1) +
    geom_segment(aes(x=78,xend=90,y=0.2,yend=0.2), col = '#013a63', size = 1) +
    geom_segment(aes(x=91,xend=102,y=0.2,yend=0.2), col = '#012a4a', size = 1) +
    
    coord_polar("x", start=0) +
    # scale_fill_viridis_d('N:P ratio') +
    scale_fill_manual(values = c('#000814',"#720026","#ee964b","#f4d35e"), name = 'N:P ratio') +
    # scale_color_manual(values = c('#000814',"#720026","#ee964b","#f4d35e"), name = 'N:P ratio') +
    
    ylim(-5,0.2) +
    labs(title = toupper(useseason)) +    
    annotate('text',x = 0, y = -5, label = 'N:P', size = 4, color = col) +
    theme_void(base_size = 8) +
    theme(plot.title = element_text(colour = col)) +
    # theme(legend.position = "none") +
    theme(plot.margin = unit(c(0,0,0,0), "cm"),
          plot.title=element_text(vjust=-3, hjust = 0.1)) +
    NULL
}  

np1 = plotseasonNP('spring','forestgreen')
np2 = plotseasonNP('summer','red3')
np3 = plotseasonNP('fall','orange')
np4 = plotseasonNP('winter','lightblue3')

np.combo = (np4 + np1 + np2 + np3 + plot_layout(nrow = 2, guides = 'collect'))
# ggsave('Figures/Figure3_seasonNP_route.png',width = 5, height = 4, dpi = 500)


############## ############## MAPPING ############## ##############
loadMapdata()

#### Lat/Long Map ####
map1 = ggplot(raw.4269) +
  annotation_map_tile(type = world_gray, zoom = 9) + # Esri Basemap
  geom_sf(data = catchments.4269.notuse, fill = alpha('seagreen1',0.05), size = 0.2) +
  geom_sf(data = catchments.4269.use, fill = alpha('seagreen1',0.3), size = 0.2) +
  
  geom_sf(data = lakeMI.4269, size = 0.2, fill = alpha('lightsteelblue1',0.8)) +
  geom_sf(aes(fill = ag), alpha = 1, shape = 21,
          size = 2, stroke = 0.2) +
  scale_fill_gradient2(low = '#1b4332',mid = 'white', high = '#774936',midpoint = 50, na.value = "grey90",
                       name = 'Ag (%)') +
  theme_bw(base_size = 8) +
  annotation_scale(location = "tr", width_hint = 0.5, height = unit(0.05,'in')) + # Scale bar
  annotation_north_arrow(location = "br", which_north = "true",
                         # pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.3,'in'), width = unit(0.3,'in'),
                         style = north_arrow_nautical) + # North Arrow
  xlab('') + ylab('') +
  theme(legend.position = c(0.1,0.15))  +
  theme(legend.text=element_text(size=6), legend.title = element_text(size=6),
        legend.key.size = unit(0.6,"line"),
        text = element_text(size=10))

# ggsave(map1, filename = 'Figures/Map_Michigan_4269.png',width = 3, height = 4, dpi = 500)
# ggsave(map1, filename = 'Figures/Map_Michigan_4269.pdf',width = 3, height = 4)

fig3 = (map1 + np.combo) + 
  plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
  theme(plot.tag = element_text(size = 8))
ggsave(plot = fig3, filename = 'Figures/Figure3_seasonNP_route.png',width = 6.5, height = 4, dpi = 500)
