#Analyses are to test significance of treatment (control DIW vs. Mn) effect on Mn Concentration in 10-month incubated litter samples
#Results: 

#GLM to investigate treatment and climate effects on Mn Conc in 10-month incubated samples

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

Mn.data <- read.csv("ICPOES_MnIncubation_Table.csv", header = TRUE, colClasses = c("factor","factor","numeric","numeric","numeric","numeric","numeric"))
Mn.data
summary(Mn.data)  

tx <- c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3)
site <- c(1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3)
Mn <- c(0.033692575,0.029816129,0.031337924,0.302529748,0.260627389,0.284893548,0.387364333,0.29136991,0.398768433,0.23368125,0.267791284,0.349685185,0.733808824,0.702468121,0.732076119,0.403467568,0.3259575,0.664284653,1.36771875,0.800566946,0.710861233,1.172330619,1.534459276,0.870596939,1.655132813,1.4809275,1.1816775)
df.Mn <- data.frame(tx, site, Mn)
print(df.Mn)

Mnglm <- glm(Mn ~ site + tx)
summary(Mnglm)   

#Two-way ANOVA to test effects of treatment and site on % carbon output
two.way.Mn <- aov(Mn ~ tx + site, data = df.Mn)
summary(two.way.Mn)          
#Two-anova shows that site and tx are significant independent variables

#testing interactions
Mn.interaction <-aov(Mn ~ site*tx, data = df.Mn)
summary(Mn.interaction)         


#Tukey's pot-hoc test (#Tukey's requires categories as factors)
aov.tx.factor = aov(Mn ~ factor(tx), data = df.Mn)
tukey.tx<-TukeyHSD(aov.tx.factor)
tukey.tx

aov.site.factor = aov(Mn ~ factor(site), data = df.Mn)
tukey.site<-TukeyHSD(aov.site.factor)
tukey.site
#Tukey's confirms that sites do not vary significantly but treatments do; Mn Tx varies significantly from background and control treatments


