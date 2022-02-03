# Load libraries
library(tidyverse)
library(patchwork)
library(ggcorrplot)

# Load data 
raw = read_csv('Data/LMtribs_seasonal.csv') %>% 
  mutate(season = factor(season, levels = c('winter','spring','summer','fall'))) 

## Random Forest covariance matrix, with only predictors selected ####
nutrient = raw %>% 
  group_by(hydroid) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  filter(!is.na(srp_mgl)) %>%
  filter(!is.na(driftthickness_mean_meters)) %>%
  rename(drift = driftthickness_mean_meters, k = vert_cond_ksat_meters_day) 

rf_cov <- nutrient  %>% 
  dplyr::select(urban, barren, forest, shrubland, herbaceous, ag, wetland, areakm2,
                k, drift)

#### PCA Plot of observations #### 

pca.plot = autoplot(prcomp(rf_cov, center = T, scale = T), data = rf_cov, alpha = 0.5, 
                    loadings = TRUE,  col = 'red4', width = 10, loadings.width = 0.2,
                    loadings.colour = 'black',loadings.label.colour = 'black',
                    loadings.label = TRUE, loadings.label.size = 2, loadings.label.repel=T) +
  
  theme_bw(base_size = 8); pca.plot

pca.plot$layers[[2]]$aes_params$size <- 0.4
pca.plot$layers[[2]]$geom_params$arrow$length <- unit(3, units = "points")

#### Correlation Matrix ####

# Compute a correlation matrix
corr <- round(cor(rf_cov), 2)
# Compute a matrix of correlation p-values
p.mat <- cor_pmat(rf_cov)

c.plot = ggcorrplot(corr, type = "full", hc.order = T,
                    lab = TRUE, p.mat = p.mat, insig = "blank",
                    outline.col = "white", tl.cex = 8, lab_size = 2,
                    ggtheme = ggplot2::theme_bw(base_size = 8),
                    colors = c("#6D9EC1", "grey95", "#E46726")); c.plot

#### Combine Figures ####
pca.plot + c.plot + 
  plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
  theme(plot.tag = element_text(size = 8))
ggsave('Figures/FigureS1.png',width = 7, height = 4, units = 'in', dpi = 500)

