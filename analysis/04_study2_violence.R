##############
# Threats items
##############

load("data/study2_explicit.RData")

# Table 1: Explicit Dehumanization -> Legitimacy of Political Violence
model1 <- lm(endorse_violence_rescale ~ hum_blatant_rescale, data=data)
model2 <- lm(endorse_violence_rescale ~ ft_diff_rescale , data=data)
model3 <- lm(endorse_violence_rescale ~ hum_blatant_rescale + ft_diff_rescale + Republican, data=data)


stargazer(model1, model2, model3,
          dep.var.labels=c("Endorse Violence"),
          covariate.labels = c("Explicit Dehumanization","Affective Polarization","Republican"),
          title="Relationship Between Dehumanization and Support for Partisan Violence",
          header=FALSE, type='text')

rm(model1, model2, model3, data)
