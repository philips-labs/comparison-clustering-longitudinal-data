set.seed(1)
tsdata = longdata_cpap()

# Zero
mod0 = lmer(Value ~ 1 + (1 | Id), data=tsdata, REML=FALSE)
summary(mod0)
BIC(mod0)

# First
mod1 = lmer(Value ~ Time + (Time | Id), data=tsdata, REML=FALSE)
summary(mod1)
BIC(mod1)


# Second
mod2 = lmer(Value ~ poly(Time, 2, raw=TRUE) + (poly(Time, 2, raw=TRUE) | Id), data=tsdata, REML=FALSE)
summary(mod2)
BIC(mod2)

# Third
mod3 = lmer(Value ~ poly(Time, 3, raw=TRUE) + (poly(Time, 3, raw=TRUE) | Id), data=tsdata, REML=FALSE)
summary(mod3)
BIC(mod3)

# Fourth (RE with poly4 fails to converge)
mod4 = lmer(Value ~ poly(Time, 4, raw=TRUE) + (poly(Time, 3, raw=TRUE) | Id), data=tsdata, REML=FALSE)
summary(mod4)
BIC(mod4)

gcm_results = list('0'=mod0, '1'=mod1, '2'=mod2, '3'=mod3, '4'=mod4)
saveRDS(gcm_results, file.path(RESULTS_DIR, 'gcm.rds'))
gcm_results = readRDS(file.path(RESULTS_DIR, 'gcm.rds'))

# BIC
elbow_plot(names(gcm_results) %>% as.integer, sapply(gcm_results, BIC)) +
    labs(x = 'Polynomial order')
# ggsave(filename=file.path(FIG_DIR, 'gcm_bic.pdf'), width=FIG_TREND_W, height=FIG_TREND_H, units='cm')

sapply(gcm_results, BIC) %>% diff
