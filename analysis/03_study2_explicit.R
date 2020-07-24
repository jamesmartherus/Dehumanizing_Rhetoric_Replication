library(ggplot2)
library(plyr)
library(stargazer)
library(gridExtra)
library(lmtest)
library(coefplot)
library(stringr)
library(psych)
library(foreign)
library(survey)

load("data/study2_explicit.RData")

# Figure 3: Treatment -> Explicit Dehumanization
model_in <- lm(hum_blatant_in ~ treat_binary, data=data)
model_out <- lm(hum_blatant_out ~ treat_binary, data=data)
model_treat <- lm(hum_blatant_rescale ~ treat_binary, data=data)
names(model_treat$coefficients) <- c("Intercept", "Dehumanizing Rhetoric","Negative Rhetoric")

pdf("figures/explicit_coefficients.pdf", width=9, height=9, pointsize=22)
coefplot(model_treat, coefficients=c("Dehumanizing Rhetoric","Negative Rhetoric")) +
  xlab("Explicit Dehumanization") +
  ggtitle("") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14), axis.text.y=element_text(size=14), axis.title.x=element_text(size=16), axis.title.y=element_text(size=16))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(angle = 90, hjust = .5)) +
  xlim(c(-.15,.05))
dev.off()

# Figure 4: Distribution of Explicit Dehumanization
pdf("figures/Explicit_dist.pdf", width=9, height=9, pointsize=22)
ggplot(data, aes(x = hum_blatant_rescale)) +  
  geom_density() +
  xlab("Blatant Dehumanization") +
  xlim(c(-.30,1.01)) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(text=element_text(size=20))
dev.off()

rm(model_in, model_out, model_treat, data)
