# Load libraries
library(tidyverse)
library(patchwork)
library(ggfortify)
library(ggplot2)
library(ggbeeswarm)
library(broom)
options(scipen = 999)

# Load data 
raw = read_csv('Data/LMtribs_seasonal.csv') %>% 
  mutate(season = factor(season, levels = c('winter','spring','summer','fall'))) 

# Seasonal boxplots 
cv.n = ggplot(raw %>% filter(!is.na(nitrate_mgl)), aes(season, 
           (nitrate_mgl))) +
  geom_quasirandom(alpha = 1, fill ='#9381ff', size = 0.5, shape = 21, stroke = 0.2) +
  # geom_boxplot(fill = 'transparent', col = c('lightblue3','forestgreen', 'red3','orange')) +
  geom_boxplot(fill = c('lightblue3','forestgreen', 'red3','orange'), alpha = 0.5, outlier.shape = NA) +
  stat_summary(fun = mean, geom="point", shape="_", size=8, color="white") + # Mean
  scale_y_log10() +
  ylab(bquote('Nitrate'~(mg~L^-1))) +
  xlab('Season') +
  theme_bw(base_size = 8); cv.n

cv.p = ggplot(raw %>% filter(!is.na(srp_mgl)), aes(season, 
                                                   (srp_mgl))) +
  geom_quasirandom(alpha = 1, fill = '#70a288', size = 0.5, shape = 21, stroke = 0.2) +
  geom_boxplot(fill = c('lightblue3','forestgreen', 'red3','orange'), alpha = 0.5, outlier.shape = NA) +
  stat_summary(fun = mean, geom="point", shape="_", size=8, color="white") + # Mean
  scale_y_log10() +
  ylab(bquote('SRP'~(mg~L^-1))) +
  xlab('Season') +
  theme_bw(base_size = 8)

# combine plots for Figure 2
cv.n + cv.p  + 
  plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
  theme(plot.tag = element_text(size = 8))
ggsave('Figures/Figure2_Boxplots.png',width = 6.5, height = 3, units = 'in', dpi = 500)

##############################################################################################################
# Summary statistics of median 
raw |> group_by(season) |> 
  summarise(medianN = median(nitrate_mgl, na.rm = T), medianP = median(srp_mgl, na.rm = T))

# Compare differences between groups. 1) Test for normality
# The Shapiro-Wilk’s test or Shapiro test is a normality test in frequentist statistics. 
# The null hypothesis of Shapiro’s test is that the population is distributed normally.
raw %>% 
  nest(data = -season) %>% 
  mutate(
    shapiro = map(data, ~shapiro.test((.x$nitrate_mgl))),
    glanced = map(shapiro, glance)
  ) %>% 
  unnest(glanced) %>% 
  dplyr::select(season, W = statistic, p.value) 
# Not normally distributed (even when log)

# Nonparametric ANOVA: Kruskal-Wallis Test
kruskal.test((nitrate_mgl) ~ season, data = raw)
# We get a p-value that is much smaller than 0.05 so we can reject the null hypothesis and 
# conclude that there is at least one group statistically different from the other groups in the dataset
kruskal.test((srp_mgl) ~ season, data = raw)

# Paired wilcox test
pairwise.wilcox.test(log(raw$nitrate_mgl), raw$season, p.adjust.method="none")
pairwise.wilcox.test(log(raw$srp_mgl), raw$season, p.adjust.method="none")

##############################################################################################################


