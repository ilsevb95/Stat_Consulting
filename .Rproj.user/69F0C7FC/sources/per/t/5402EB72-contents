# Extra

# Fit different random effects

We now compare a random intercept model to random intercept + slope



model_int <- nlme::lme(ndi ~ time_fct + hads_tot_cnt, method = "REML",
                       random = ~1|id, data = df_long)

model_int_slope <- nlme::lme(ndi ~ time_fct + hads_tot_cnt, method = "REML",
                             random = ~1+ time_fct|id, data = df_long)

summary(model_int_slope)


anova(model_int, model_int_slope)


