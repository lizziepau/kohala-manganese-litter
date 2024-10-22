#Incubation Experiment Mass

setwd("~/Dropbox/Stanford/Lab/Projects/Vitousek_Mn Litter Decomp/Mn Incubation/R Script")

library("colorspace")
library("ggplot2")
library("dplyr")
library(tidyselect)
library(multcomp)

site=c(rep(1, 3),rep(2,3),rep(3,3),rep(1, 3),
       rep(2,3),rep(3,3))
treatment = c(rep("C", 3),rep("C",3),rep("C",3),rep("Mn", 3),
              rep("Mn",3),rep("Mn",3))
st <- c(rep("CS1", 3),rep("CS2",3),rep("CS3",3),rep("Mn1", 3),
        rep("Mn2",3),rep("Mn3",3))
im = c(0.807165333333334,0.86482,0.807165333333334,0.812713333333334,
       0.740293333333334,0.828806666666667,0.839696666666667,0.968221666666667,
       0.873970000000001,0.807165333333334,0.884038222222223,0.807165333333334,
       0.756386666666667,0.732246666666667,0.708106666666667,0.831128333333334,
       0.925380000000001,0.942516666666667)
fm = c(0.3265,0.31,0.441,0.444,0.4503,0.4625,0.3067,0.3825,0.3053,0.3525,0.4083,
       0.3375,0.5345,0.4319,0.5574,0.3472,0.3268,0.3381)
delmass = c(0.480665333333334,0.554820000000001,0.366165333333334,
            0.368713333333334,0.289993333333334,0.366306666666667,
            0.532996666666667,0.585721666666667,0.568670000000001,
            0.454665333333334,0.475738222222223,0.469665333333334,
            0.221886666666668,0.300346666666667,0.150706666666667,
            0.483928333333334,0.598580000000001,0.604416666666668)

exp.mass.data <- data.frame(site, treatment,st,im,fm,delmass)
exp.mass.data

exp.del.mass.data <-data.frame(st,delmass, data = exp.mass.data)
exp.del.mass.data

#One-way ANOVA and Tukey's post-hoc: delta mass
delmass.aov.st <- aov(delmass ~ st, data = exp.del.mass.data)
summary(delmass.aov.st)

delmass.aov.factor = aov( delmass ~ factor(st), data = exp.del.mass.data)
tukey.delmass<-TukeyHSD(delmass.aov.factor)
tukey.delmass
plot(tukey.delmass , las=1 , col="black")

cld <- multcompLetters4(delmass.aov.factor, tukey.delmass)
print(cld)

##Paired Two-Sample Welch's T-test: Welch’s t-test: this test assumes that both groups of data are sampled from populations that follow a normal distribution, but it does not assume that those two populations have the same variance.
###paired because the sample number for each group are equal
###iniitial and final incubation (dry) litter masses, c = control, mn = Mn, i=initial, f=final

ic1<-c(0.807165333333334,0.86482,0.807165333333334)	
ic2<-c(0.812713333333334,0.740293333333334,0.828806666666667)
ic3<-c(0.839696666666667,0.968221666666667,0.873970000000001)
fc1<-c(0.3265,0.31,0.441)
fc2<-c(0.444,0.4503,0.4625)
fc3<-c(0.3067,0.3825,0.3053)
imn1<-c(0.807165333333334,0.884038222222223,0.807165333333334)
imn2<-c(0.756386666666667,0.732246666666667,0.708106666666667)
imn3<-c(0.831128333333334,0.925380000000001,0.942516666666667)
fmn1<-c(0.3525,0.4083,0.3375)
fmn2<-c(0.5345,0.4319,0.5574)
fmn3<-c(0.3472,0.3268,0.3381)

###Paired Two-Sample Welch's T-test by site.group

c1.df<-data.frame(group = (c(rep("initial", 3),rep("final",3))),
                   c1mass = c(ic1,fc1))
print(c1.df)
t_c1 <- t.test(ic1,fc1)
t_c1
##### *p-value = 0.002513

c2.df<-data.frame(group = (c(rep("initial", 3),rep("final",3))),
                  c2mass = c(ic2,fc2))
print(c2.df)
t_c2 <- t.test(ic2,fc2)
t_c2
##### *p-value = 0.004876

c3.df<-data.frame(group = (c(rep("initial", 3),rep("final",3))),
                  c3mass = c(ic3,fc3))
print(c3.df)
t_c3 <- t.test(ic3,fc3)
t_c3
##### *p-value = 0.0005639

mn1.df<-data.frame(group = (c(rep("initial", 3),rep("final",3))),
                  mn1mass = c(imn1,fmn1))
print(mn1.df)
t_mn1 <- t.test(imn1,fmn1)
t_mn1
##### *p-value = 0.000184

mn2.df<-data.frame(group = (c(rep("initial", 3),rep("final",3))),
                   mn1mass = c(imn2,fmn2))
print(mn2.df)
t_mn2 <- t.test(imn2,fmn2)
t_mn2
##### *p-value = 0.01879

mn3.df<-data.frame(group = (c(rep("initial", 3),rep("final",3))),
                   mn1mass = c(imn3,fmn3))
print(mn3.df)
t_mn3 <- t.test(imn3,fmn3)
t_mn3
##### *p-value = 0.003038

###Paired Two-Sample Welch's T-test by delta mass
delc1<-c(0.480665333333334,0.554820000000001,0.366165333333334)
delc2<-c(0.368713333333334,0.289993333333334,0.366306666666667)
delc3<-c(0.532996666666667,0.585721666666667,0.568670000000001)
delmn1<-c(0.454665333333334,0.475738222222223,0.469665333333334)
delmn2<-c(0.221886666666668,0.300346666666667,0.150706666666667)
delmn3<-c(0.483928333333334,0.598580000000001,0.604416666666668)

t_del1 <- t.test(delc1,delmn1)
t_del1
##### p-value = 0.9932

t_del2 <- t.test(delc2,delmn2)
t_del2
##### p-value = 0.09489

t_del3 <- t.test(delc3,delmn3)
t_del3
##### p-value = 0.9973




#Control vs. Mn treatments
##ANOVA

ic<-c(0.807165333333334,0.86482,0.807165333333334,0.812713333333334,0.740293333333334,0.828806666666667,0.839696666666667,0.968221666666667,0.873970000000001)
imn<-c(0.807165333333334,0.884038222222223,0.807165333333334,0.756386666666667,0.732246666666667,0.708106666666667,0.831128333333334,0.925380000000001,0.942516666666667)
fc<-c(0.3265,0.31,0.441,0.444,0.4503,0.4625,0.3067,0.3825,0.3053)
fmn<-c(0.3525,0.4083,0.3375,0.5345,0.4319,0.5574,0.3472,0.3268,0.3381)

ctrlmn.mass <- c(0.807165333333334,0.86482,0.807165333333334,0.812713333333334,0.740293333333334,0.828806666666667,
                 0.839696666666667,0.968221666666667,0.873970000000001,0.807165333333334,0.884038222222223,
                 0.807165333333334,0.756386666666667,0.732246666666667,0.708106666666667,0.831128333333334,
                 0.925380000000001,0.942516666666667,0.3265,0.31,0.441,0.444,0.4503,0.4625,0.3067,0.3825,0.3053,
                 0.3525,0.4083,0.3375,0.5345,0.4319,0.5574,0.3472,0.3268,0.3381)

ctrlmn.df<-data.frame(group = (c(rep("ic", 9),rep("imn",9),rep("fc", 9),rep("fmn",9))),
                  ctrlmn.mass )

ctrlmn.aov <- aov(ctrlmn.mass ~ group, data = ctrlmn.df)
summary(ctrlmn.aov)

tukey.ctrlmn<-TukeyHSD(ctrlmn.aov)
tukey.ctrlmn
plot(tukey.ctrlmn , las=1 , col="black")

cld <- multcompLetters4(ctrlmn.aov, tukey.ctrlmn)
print(cld)

##Paired Two-Sample Welch's T-test
delctrl<- c(0.480665333333334,0.554820000000001,0.366165333333334,0.368713333333334,0.289993333333334,
            0.366306666666667,0.532996666666667,0.585721666666667,0.568670000000001)
delmn<-c(0.454665333333334,0.475738222222223,0.469665333333334,0.221886666666668,0.300346666666667,
         0.150706666666667,0.483928333333334,0.598580000000001,0.604416666666668)

t_delctrlmn <- t.test(delctrl,delmn)
t_delctrlmn
