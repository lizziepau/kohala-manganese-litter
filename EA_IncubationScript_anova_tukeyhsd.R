#Analyses are to test significance of treatment (control DIW vs. Mn) effect on % carbon in 10-month incubated litter samples
#Results: only site 1 vs 3 showed signficant differences in %C; no signficant Tx effect found

#GLM to investigate treatment and climate effects on %Carbon in 10-month incubated samples

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

EA.data <- read.csv("EAIncubationTable.csv", header = TRUE, colClasses = c("numeric", "numeric", "numeric"))
EA.data
summary(EA.data)  

tx <- c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3)
site <- c(1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3)
carbon <- c(39.385,39.524,39.566,42.681,41.641,41.76,30.389,42.893,42.653,36.295,36.641,29.321,41.145,37.765,35.824,42.445,43.882,43.153,36.652,33.588, 37.216,  39.637,39.975,   38.134,  44.045, 43.695,36.292)
df.EA <- data.frame(tx, site, carbon)
print(df.EA)

EAglm <- glm(carbon ~ site + tx)
summary(EAglm)         

#Two-way ANOVA to test effects of treatment and site on % carbon output
two.way.EA <- aov(carbon ~ tx + site, data = df.EA)
summary(two.way.EA)          
#Two-anova shows that site is the only significant independent variable

#testing interactions
interaction <-aov(carbon ~ site*tx, data = df.EA)
summary(interaction)         
#there is little to no interaction between treatment and site

#Tukey's pot-hoc test (#Tukey's requires categories as factors)
aov.tx.factor = aov(carbon ~ factor(tx), data = df.EA)
tukey.tx<-TukeyHSD(aov.tx.factor)
tukey.tx

aov.site.factor = aov(carbon ~ factor(site), data = df.EA)
tukey.site<-TukeyHSD(aov.site.factor)
tukey.site
#Tukey's confirms that the only signfificant difference in %C lies bewteen sites 1 and 3
#Treatment has no signficant effect
         
          
