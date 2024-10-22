#Percent carbon data for gradient samples

setwd("~/Dropbox/Stanford/Lab/Projects/Vitousek_Mn Litter Decomp/Mn Incubation/R Script")
read.csv("EA_Gradient.csv")

library("colorspace")
library("ggplot2")
library("dplyr")
library(tidyselect)
library(multcomp)


#All carbon data
start_collect_site <- c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3)
start_decomp_site <- c(1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3)
start_carbon <- c(37.47908487792,37.11431161238,37.25736847846,38.61199517304,43.00243124844,42.73035894424,43.22482661047,38.98711958186,44.40085508031,34.98417627311,40.17217006021,39.7517244984,41.25506959784,43.31207753097,32.06063130386,43.59070971379,41.16075766163,39.59550376626,37.06521824951,34.42205361358,46.14364635059,39.33369371883,43.64235669256,42.27210406727,44.04122407029,36.59993982081,43.80306999625,43.54749685667)
end_collect_site <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3)
end_decomp_site <- c(1,1,1,1,1,2,2,2,3,3,3,3,3,1,1,2,2,2,3,3,3,3,3,1,1,1,1,1,2,2,2,3,3,3,3,3)
end_carbon <- c(35.466,42.064,31.176,34.076,26.155,44.459,51.964,43.7,47.305,43.288,44.505,45.916,30.404,29.332,32.814,41.177,34.591,32.805,46.047,43.378,31.311,43.864,33.302,32.144,28.183,37.683,33.028,34.263,41.282,42.38,41.688,36.769,35.453,40.584,29.281,45.9)
df.start.EA <- data.frame(start_collect_site,start_decomp_site,start_carbon)
df.end.EA <- data.frame(end_collect_site,end_decomp_site,end_carbon)
print(df.start.EA)
print(df.end.EA)

#Site Comparisons, % carbon before and after decomposition 
##Unpaired Two-Sample Wilcoxon Test--compare means of two nonparametric, unpaired, independent groups
###unpaired because the sample number for pre and post % C are not always equal (not true treatment pairs)
pre_11<-c(37.47908487792,37.11431161238,37.25736847846)			
post_11<-c(35.466,42.064,31.176,34.076,26.155)
pre_12<-c(38.61199517304,43.00243124844,42.73035894424)
post_12<-c(44.459,51.964,43.7)
pre_13<-c(43.22482661047,38.98711958186,44.40085508031)
post_13<-c(47.305,43.288,44.505,45.916,30.404)
pre_21<-c(34.98417627311,40.17217006021,39.7517244984)
post_21<-c(29.332,32.814)
pre_22<-c(41.25506959784,43.31207753097,32.06063130386,43.59070971379)
post_22<-c(41.177,34.591,32.805)
pre_23<-c(41.16075766163,39.59550376626,37.06521824951)
post_23<-c(46.047,43.378,31.311,43.864,33.302)
pre_31<-c(34.42205361358,46.14364635059,39.33369371883)
post_31<-c(32.144,28.183,37.683,33.028,34.263)
pre_32<-c(43.64235669256,42.27210406727,44.04122407029)
post_32<-c(41.282,42.38,41.688)
pre_33<-c(36.59993982081,43.80306999625,43.54749685667)
post_33<-c(36.769,35.453,40.584,29.281,45.9)

#Dataframes by site
df_11<-data.frame(group = (c("pre","pre","pre","post","post","post","post","post")),
                  PC.11 = c(pre_11,post_11))
print(df_11)

df_12<-data.frame(group = (c("pre","pre","pre","post","post","post")),
                  PC.12 = c(pre_12,post_12))
print(df_12)

df_13<-data.frame(group = (c("pre","pre","pre","post","post","post","post","post")),
                  PC.13 = c(pre_13,post_13))
print(df_13)

df_21<-data.frame(group = (c("pre","pre","pre","post","post")),
                  PC.21 = c(pre_21,post_21))
print(df_21)

df_22<-data.frame(group = (c("pre","pre","pre","pre","post","post","post")),
                  PC.22 = c(pre_22,post_22))
print(df_22)

df_23<-data.frame(group = (c("pre","pre","pre","post","post","post","post","post")),
                  PC.23 = c(pre_23,post_23))
print(df_23)

df_31<-data.frame(group = (c("pre","pre","pre","post","post","post","post","post")),
                  PC.31 = c(pre_31,post_31))
print(df_31)

df_32<-data.frame(group = (c("pre","pre","pre","post","post","post")),
                  PC.32 = c(pre_32,post_32))
print(df_32)

df_33<-data.frame(group = (c("pre","pre","pre","post","post","post","post","post")),
                  PC.33 = c(pre_33,post_33))
print(df_33)

##Site 11
boxplot(PC.11 ~ group, data = df_11, xlab = "Group", ylab = "% Carbon")

wilcox_11 <- wilcox.test(pre_11, post_11)
wilcox_11
###p-value=0.25, not significant

##Site 12
wilcox_12 <- wilcox.test(pre_12, post_12)
wilcox_12
###p-value=0.1, not significant

##Site 13
wilcox_13 <- wilcox.test(pre_13, post_13)
wilcox_13
###p-value=0.3929, not significant

##Site 21
wilcox_21 <- wilcox.test(pre_21, post_21)
wilcox_21
###p-value=0.2, not significant

##Site 22
wilcox_22 <- wilcox.test(pre_22, post_22)
wilcox_22
###p-value=0.4, not significant

##Site 23
wilcox_23 <- wilcox.test(pre_23, post_23)
wilcox_23
###p-value=0.7857, not significant

##Site 31
wilcox_31 <- wilcox.test(pre_31, post_31)
wilcox_31
###p-value=0.07143, not significant

##Site 32
wilcox_32 <- wilcox.test(pre_32, post_32)
wilcox_32
###p-value=0.2, not significant

##Site 33
wilcox_33 <- wilcox.test(pre_33, post_33)
wilcox_33
###p-value=0.5714, not significant

#Two-way ANOVA
gradient_data <- read.csv("EA_Gradient.csv")
gradient_data
gradient.aov <- aov(Pct_Carbon ~ Site + Treatment,data = gradient_data )
summary(gradient.aov)

two.way.plot <- ggplot(gradient_data, aes(x = Site, y = Pct_Carbon, group=Treatment)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))
two.way.plot

aov.gradient.factor = aov( Pct_Carbon ~ factor(Site) + factor(Treatment), data = gradient_data)
tukey.gradient<-TukeyHSD(aov.gradient.factor)
tukey.gradient
plot(tukey.gradient , las=1 , col="black")

##interactions
gradient.aov.interaction <- aov(Pct_Carbon ~ Site * Treatment,data = gradient_data )
summary(gradient.aov.interaction)
###no significant interaction between site and treatment effects 

##Site One-way ANOVA
gradient_site.aov <- aov(Pct_Carbon ~ Site ,data = gradient_data )
summary(gradient_site.aov)

aov.gradient.site.factor = aov( Pct_Carbon ~ factor(Site), data = gradient_data)
tukey.gradient.site<-TukeyHSD(aov.gradient.site.factor)
tukey.gradient.site
plot(tukey.gradient.site , las=1 , col="black")
###No significant diffs among sites

##Treatment One-way ANOVA
gradient_tx.aov <- aov(Pct_Carbon ~ Treatment,data = gradient_data )
summary(gradient_tx.aov)

aov.gradient.tx.factor = aov( Pct_Carbon ~ factor(Treatment), data = gradient_data)
tukey.gradient.tx<-TukeyHSD(aov.gradient.tx.factor)
tukey.gradient.tx
plot(tukey.gradient.tx , las=1 , col="black")
### Tx 2-1 p-value = 0.0006309, Tx 3-1 p-value = 0.0044894

#Gradient GLM
gradient_carbon.glm <- glm( Pct_Carbon ~ Site + Treatment, data = gradient_data)
summary(gradient_carbon.glm) 
plot(gradient_carbon.glm)

gradient_carbon.tx.glm <- glm( Pct_Carbon ~ Treatment, data = gradient_data)
summary(gradient_carbon.tx.glm) 
plot(gradient_carbon.tx.glm)

##boxplot labels from Tukey 
pctC<-c(37.47908487792,37.11431161238,37.25736847846,38.61199517304,43.00243124844,42.73035894424,43.22482661047,38.98711958186,44.40085508031,34.98417627311,40.17217006021,39.7517244984,41.25506959784,43.31207753097,32.06063130386,43.59070971379,41.16075766163,39.59550376626,37.06521824951,34.42205361358,46.14364635059,39.33369371883,43.64235669256,42.27210406727,44.04122407029,36.59993982081,43.80306999625,43.54749685667,35.466,42.064,31.176,34.076,26.155,44.459,51.964,43.7,47.305,43.288,44.505,45.916,30.404,29.332,32.814,41.177,34.591,32.805,46.047,43.378,31.311,43.864,33.302,32.144,28.183,37.683,33.028,34.263,41.282,42.38,41.688,36.769,35.453,40.584,29.281,45.9)
tx <- c(1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,1,1,2,2,2,3,3,3,3,3,1,1,2,2,2,3,3,3,3,3,1,1,1,1,1,2,2,2,3,3,3,3,3)
site <- c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3)
timesitetx <- c(011,011,011,012,012,012,013,013,013,021,021,021,022,022,022,022,023,023,023,031,031,031,032,032,032,033,033,033,111,111,111,111,111,112,112,112,113,113,113,113,113,121,121,122,122,122,123,123,123,123,123,131,131,131,131,131,132,132,132,133,133,133,133,133)
df.pctC <- data.frame(pctC,tx,site,timesitetx)
print(df.pctC)

anova <- aov(pctC ~ factor(timesitetx), data = df.pctC)
summary(anova)

tukey <- TukeyHSD(anova)
print(tukey)
plot(tukey , las=1 , col="purple")

cld <- multcompLetters4(anova, tukey)
print(cld)
###112   32  113   13  132   12   33   22   31  123   23   21  133   11  122  111  131  121 
###"a" "ab" "ab" "ab" "ab" "ab" "ab" "ab" "ab" "ab" "ab" "ab" "ab" "ab" "ab"  "b"  "b" "ab" 

