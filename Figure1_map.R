# Load libraries
library(ggspatial)
library(patchwork)
library(tidyverse)
library(sf)
source('Functions/loadMapdata.R')

############## ############## MAPPING ############## ##############
loadMapdata()

# Test plot of site order
# ggplot(raw.4269) +
#   geom_sf(data = lakeMI.4269, size = 0.2, fill = alpha('lightsteelblue1',0.8)) +
#   geom_sf(aes(fill = order), alpha = 1, shape = 21,
#           size = 2, stroke = 0.2) +
#   geom_sf_text(aes(label = order)) +
#   scale_fill_distiller(palette = 'RdYlBu', direction = -1, na.value = "grey90",
#                        name = 'Order')

#### Watershed Map ####
catchments.4269.use = catchments.4269 %>% filter(HydroID %in% raw.4269$hydroid) %>% 
  dplyr::select(HydroID) %>% 
  left_join(tribdata, by = c('HydroID' = 'hydroid'))


plotcatchment <- function(byvalue, highcolor, legendName) {
map2 = ggplot(raw.4269) +
  annotation_map_tile(type = world_gray, zoom = 9) + # Esri Basemap
  geom_sf(data = catchments.4269.use, aes_string(fill = byvalue), size = 0.2) +
  geom_sf(data = catchments.4269.notuse, fill = alpha('grey50',0.3), size = 0.2) +
  
  geom_sf(data = lakeMI.4269, size = 0.2, fill = alpha('lightsteelblue1',0.8)) +
  scale_fill_gradient(low = 'white', high = highcolor,na.value = "grey50",
                       name = legendName) +
  theme_bw(base_size = 8) +
  annotation_scale(location = "tr", width_hint = 0.5, height = unit(0.05,'in')) + # Scale bar
  annotation_north_arrow(location = "br", which_north = "true",
                         # pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         height = unit(0.3,'in'), width = unit(0.3,'in'),
                         style = north_arrow_nautical) + # North Arrow
  xlab('') + ylab('') +
  theme(legend.position = c(0.15,0.15))  +
  theme(legend.text=element_text(size=6), legend.title = element_text(size=6),
        legend.key.size = unit(0.6,"line"),
        text = element_text(size=10))
}
map2 = plotcatchment(byvalue = 'ag', highcolor = '#774936', legendName = 'Ag (%)')
map3 = plotcatchment(byvalue = 'wetland', highcolor = '#028090', legendName = 'Wetland (%)')
map4 = plotcatchment(byvalue = 'driftthickness_mean_meters', highcolor = '#9e2a2b', legendName = 'Drift (m)')
map5 = plotcatchment(byvalue = 'vert_cond_ksat_meters_day', highcolor = '#ffe66d', legendName = 'Sat. k (m/s)')

outputmap = map2 + map3 + map4 + map5 + plot_layout(ncol = 2) + 
  plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
  theme(plot.tag = element_text(size = 8))
ggsave(outputmap, filename = 'Figures/Figure1_CatchmentsMap.png',width = 6.5, height = 8, dpi = 500)
