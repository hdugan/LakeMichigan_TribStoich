# Load packages
library(randomForest)
library(ranger)
library(tidyverse)
library(patchwork)
# for partial dependence plots 
library(pdp)   
library(vip)   

options(scipen = 999)

# Functions 
# compare mean to median predictions
lm_eqn <- function(y, x){
  m <- lm(y ~ x);
  eq <- substitute(~~italic(r)^2~"="~r2, 
                   list(r2 = format(summary(m)$r.squared, digits = 2)))
  as.character(as.expression(eq));
}

# Load data
raw = read_csv('Data/LMtribs_seasonal.csv')

################## Nitrogen ###########################
## Nitrogen covariance matrix, with only predictors selected ####
n.df = raw %>% 
  filter(!is.na(nitrate_mgl)) %>% 
  filter(!is.na(driftthickness_mean_meters)) %>% 
  mutate(areakm2 = log(areakm2)) %>% 
  rename(drift = driftthickness_mean_meters, k = vert_cond_ksat_meters_day) %>% 
  mutate(value = 1) %>% #Add binary for seasons
  pivot_wider(names_from = season,
              values_from = value,
              values_fill = 0)

rf_cov <- n.df  %>% 
  dplyr::select(urban, barren, forest, herbaceous, ag, wetland, areakm2,
                k, drift, summer, fall, winter, spring) 

sapply(rf_cov, function(x) sum(is.na(x))) # Check if there are NA values

# RF model ####
rf_model.N = ranger(dependent.variable.name='nitrogen',
                    data=data.frame(nitrogen = scale(log(n.df$nitrate_mgl)),rf_cov), 
                    mtry = 4, quantreg = T, importance = 'permutation',
                    keep.inbag = TRUE)
rf_model.N

## option to save model the model 
# saveRDS(rf_model.N, "./filename.rds")

# Calculate oob quantiles
oob_quantiles <- predict(rf_model.N, type = 'quantiles', quantiles = c(0.05,0.50,0.95))

scaling.attributes = scale(log(n.df$nitrate_mgl))
unscale.model.pred.N =  exp(rf_model.N$predictions * attr(scaling.attributes, 'scaled:scale') + attr(scaling.attributes, 'scaled:center'))

plot.n.model = ggplot() + 
  geom_smooth(aes(x = n.df$nitrate_mgl, y = unscale.model.pred.N), method = 'lm', col = 'black') +
  geom_point(aes(x = n.df$nitrate_mgl, y = unscale.model.pred.N), shape = 21, fill = '#9381ff', alpha = 0.8, size = 1) +
  scale_y_log10() + scale_x_log10() +
  ylab(bquote('Modeled Nitrate'~(mg~L^-1))) +
  xlab(bquote('Observed Nitrate'~(mg~L^-1))) +
  annotate('text',x = 1, y = 0.01, label = lm_eqn(rf_model.N$predictions, log(n.df$nitrate_mgl)), parse = TRUE, size = 3) +
  theme_bw(base_size = 8)

summary(lm(oob_quantiles$predictions[,2] ~ log(n.df$nitrate_mgl))) # r2 of all observations (log transformed)

#variable importance
v<-as.numeric(rf_model.N$variable.importance)
w<-as.character(names(rf_model.N$variable.importance))
DF<-data.frame(w=w,v=as.numeric(v)) %>% arrange(v)
DF$w <- factor(DF$w, levels = DF$w)

# ggplot(DF, aes(x=w, y=v,fill=v))+
#   geom_bar(stat="identity", position="dodge") + coord_flip() +
#   ylab("Variable Importance") + xlab("")+
#   ggtitle("Information Value Summary")+
#   theme_bw() +
#   theme(legend.position="none")

# Variable importance plot (compare to randomForest::varImpPlot(boston_rf))
i.df = vi(rf_model.N) |> #importance
  mutate(group = 'Nitrate variable importance')

plot.n.imp = ggplot(i.df) +
  geom_col(aes(x = reorder(Variable, -Importance), y = Importance), fill = '#9381ff') +
  xlab('Variable') +
  theme_bw(base_size = 8) +
  facet_wrap(~group) +
  theme(strip.background = element_rect(fill=alpha("#9381ff",0.1))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  +
  theme(axis.title.x = element_blank())

partial.list = list()
for (i in 1:6) {
  varname = i.df$Variable[i]
  partial.list[[i]] = data.frame(pdp::partial(rf_model.N, pred.var = varname)) %>% rename(var = varname) %>% mutate(pred = varname) 
  
}

partial.df = do.call(rbind, partial.list) %>% 
  mutate(across(pred, factor, levels = i.df$Variable))

# Dataframe for x-axis rug 
rf_cov.long = rf_cov |> pivot_longer(cols = everything(), names_to = 'pred') |> 
  filter(pred %in% i.df$Variable[1:6])
rf_cov.long$pred = factor(rf_cov.long$pred, levels = i.df$Variable)

plot.n.pdp = partial.df %>% 
  ggplot(aes(x = var, y = yhat)) +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 0.2) +
  geom_point(data = rf_cov.long, aes(x = value, y = -0.43), shape = '|', color = "#9381ff", alpha = 0.8) +
  geom_line(size = 0.3) +
  geom_point(fill = '#9381ff', alpha = 0.8, shape = 21, size = 0.7, stroke = 0.2) +
  ylab('Feature Contribution') +
  ylim(c(-0.44, NA)) +
  facet_wrap(~pred, scales = 'free_x', ncol = 3) +
  theme_bw(base_size = 8) +
  theme(strip.background = element_rect(fill=alpha("#9381ff",0.1))) +
  theme(axis.title.x = element_blank())


################## Phosphorus ###########################
## Phosphorus covariance matrix, with only predictors selected ####
p.df = raw %>% 
  filter(!is.na(srp_mgl)) %>% 
  filter(!is.na(driftthickness_mean_meters)) %>% 
  mutate(areakm2 = log(areakm2)) %>% 
  rename(drift = driftthickness_mean_meters, k = vert_cond_ksat_meters_day) %>% 
  mutate(value = 1) %>% #Add binary for seasons
  pivot_wider(names_from = season,
              values_from = value,
              values_fill = 0)

rf_cov <- p.df  %>% 
  dplyr::select(urban, barren, forest, herbaceous, ag, wetland, areakm2,
                k, drift, summer, fall, winter, spring)

sapply(rf_cov, function(x) sum(is.na(x))) # Check if there are NA values

# RF model ####
rf_model.P = ranger(dependent.variable.name='phosphorus',
                    data=data.frame(phosphorus = scale(log(p.df$srp_mgl)),rf_cov), 
                    mtry = 4, quantreg = T, importance = 'permutation',
                    keep.inbag = TRUE)
rf_model.P

## option to save model the model 
# saveRDS(rf_model.P, "./filename.rds")

# Calculate oob quantiles
oob_quantiles <- predict(rf_model.P, type = 'quantiles', quantiles = c(0.05,0.50,0.95))

scaling.attributes = scale(log(p.df$srp_mgl))
unscale.model.pred.P =exp(rf_model.P$predictions * attr(scaling.attributes, 'scaled:scale') + attr(scaling.attributes, 'scaled:center'))

plot.p.model = ggplot() + 
  geom_smooth(aes(x = p.df$srp_mgl, y = unscale.model.pred.P), method = 'lm', col = 'black') +
  geom_point(aes(x = p.df$srp_mgl, y = unscale.model.pred.P), shape = 21, fill = '#70a288', alpha = 0.8, size = 1) +
  scale_y_log10() + scale_x_log10() +
  ylab(bquote('Modeled SRP'~(mg~L^-1))) +
  xlab(bquote('Observed SRP'~(mg~L^-1))) +
  annotate('text',x = 0.1, y = 0.003, label = lm_eqn(rf_model.P$predictions, log(p.df$srp_mgl)), parse = TRUE, size = 3, 
           hjust = 0, vjust = 0.5) +
  theme_bw(base_size = 8)

summary(lm(oob_quantiles$predictions[,2] ~ log(p.df$srp_mgl))) # r2 of all observations (log transformed)

#variable importance
v<-as.numeric(rf_model.P$variable.importance)
w<-as.character(names(rf_model.P$variable.importance))
DF<-data.frame(w=w,v=as.numeric(v)) %>% arrange(v)
DF$w <- factor(DF$w, levels = DF$w)

# ggplot(DF, aes(x=w, y=v,fill=v))+
#   geom_bar(stat="identity", position="dodge") + coord_flip() +
#   ylab("Variable Importance") + xlab("")+
#   ggtitle("Information Value Summary")+
#   theme_bw() +
#   theme(legend.position="none")

# Variable importance plot (compare to randomForest::varImpPlot(boston_rf))
i.df = vi(rf_model.P) |> #importance
  mutate(group = 'SRP variable importance')

plot.p.imp = ggplot(i.df) +
  geom_col(aes(x = reorder(Variable, -Importance), y = Importance), fill = '#70a288') +
  xlab('Variable') +
  theme_bw(base_size = 8) +
  facet_wrap(~group) +
  theme(strip.background = element_rect(fill=alpha("#70a288",0.1))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  +
  theme(axis.title.x = element_blank())

partial.list = list()
for (i in 1:6) {
  varname = i.df$Variable[i]
  partial.list[[i]] = data.frame(partial(rf_model.P, pred.var = varname)) %>% rename(var = varname) %>% mutate(pred = varname) 
  
}

partial.df = do.call(rbind, partial.list) %>% 
  mutate(across(pred, factor, levels = i.df$Variable))
partial.df$pred

# Dataframe for x-axis rug 
rf_cov.long = rf_cov |> pivot_longer(cols = everything(), names_to = 'pred') |> 
  filter(pred %in% i.df$Variable[1:6])
rf_cov.long$pred = factor(rf_cov.long$pred, levels = i.df$Variable)

plot.p.pdp = partial.df %>% 
  ggplot(aes(x = var, y = yhat)) +
  geom_hline(aes(yintercept = 0), linetype = 2, size = 0.2) +
  geom_point(data = rf_cov.long, aes(x = value, y = -0.2), shape = '|', color = "#70a288", alpha = 0.8) +
  geom_line(size = 0.3) +
  geom_point(fill = '#70a288', alpha = 0.8, shape = 21, size = 0.7, stroke = 0.2) +
  ylab('Feature Contribution') +
  ylim(c(-0.21, NA)) +
  facet_wrap(~pred, scales = 'free_x', ncol = 3) +
  theme_bw(base_size = 8) +
  theme(strip.background = element_rect(fill=alpha("#70a288",0.1))) +
  theme(axis.title.x = element_blank()); plot.p.pdp

################## Combine Figures ###########################
layout <- "
AAABBB
CCDDDD
EEFFFF
"
plot.n.model + plot.p.model + 
  plot.n.imp + plot.n.pdp +
  plot.p.imp + plot.p.pdp +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
  theme(plot.tag = element_text(size = 8))

ggsave('Figures/Figure4_RF.png',width = 6.5, height = 6, units = 'in', dpi = 500)






