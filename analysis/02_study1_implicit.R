library(stargazer)
library(tidyverse)
library(coefplot)

load("data/study1_implicit.Rdata")

# Figure 1: Treatment -> Implicit Dehumanization
model1 <- lm(d_score ~ treatment, rips)
names(model1$coefficients) <- c("Intercept", "Dehumanizing Rhetoric","Negative Rhetoric")

pdf("figures/implicit_coefficients.pdf", width=6, height=6, pointsize=32)
coefplot(model1, coefficients=c("Dehumanizing Rhetoric","Negative Rhetoric")) +
  xlab("Implicit Dehumanization") +
  ggtitle("") +
  theme_bw() +
  theme(axis.text.x=element_text(size=14), axis.text.y=element_text(size=14), axis.title.x=element_text(size=16), axis.title.y=element_text(size=16))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.y = element_text(angle = 90, hjust = .5))
dev.off()

#Figure 5: Implicit Dehumanization Distribution
pdf("figures/dscore_distribution.pdf", width=6, height=6, pointsize=32)
ggplot(rips, aes(x = d_score)) +  
  geom_density() +
  xlab("Partisan Dehumanization IAT D Score") +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(text=element_text(size=14))
dev.off()

#Descriptive statistics for implicit dehumanization (used in supporting information)
mean(rips$d_score, na.rm=T)
nrow(rips[rips$d_score > 0 & !is.na(rips$d_score),]) / nrow(rips[!is.na(rips$d_score),])
nrow(rips[rips$d_score > 1 & !is.na(rips$d_score),]) / nrow(rips[!is.na(rips$d_score),])

rm(model1, rips)



