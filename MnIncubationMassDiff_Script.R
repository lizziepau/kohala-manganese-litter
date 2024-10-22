#testing difference in final incubation wet weights

tx <- c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2)
site <- c(1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3)
massdiff <- c(0.777133333,1.029533333,0.874333333,1.523533333,1.440133333,2.019433333,0.535733333,0.163633333,0.507533333,0.570333333,1.096833333,0.413633333,1.341133333,0.866033333,1.148933333,0.917933333,0.761033333,0.174833333)
df.mass <- data.frame(tx, site, massdiff)
print(df.mass)

massglm <- glm(massdiff ~ site + tx)
summary(massglm)   

#Two-way ANOVA to test effects of treatment and site on % carbon output
two.way.mass <- aov(massdiff ~ tx + site, data = df.mass)
summary(two.way.mass)          
#Two-anova shows no sigfnicant differences

#testing interactions
mass.interaction <-aov(massdiff ~ site*tx, data = df.mass)
summary(mass.interaction)         
#no sgnficant interaction effects

#Tukey's pot-hoc test (#Tukey's requires categories as factors)
aov.tx.factor = aov(massdiff ~ factor(tx), data = df.mass)
tukey.tx<-TukeyHSD(aov.tx.factor)
tukey.tx

aov.site.factor = aov(masdiff ~ factor(site), data = df.mass)
tukey.site<-TukeyHSD(aov.site.factor)
tukey.site
#Tukey's confirms no significant differences


