#Gradient Wet Mass
setwd("~/Dropbox/Stanford/Lab/Projects/Vitousek_Mn Litter Decomp/Mn Incubation/R Script")

library("colorspace")
library("ggplot2")
library("dplyr")
library(tidyselect)
library(multcomp)

wetmass.data <- data.frame(site=c(1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3),
                           treatment = c(1,1,1,1,1,2,2,2,3,3,3,3,1,1,2,2,2,3,3,3,3,1,1,1,1,1,2,2,2,3,3,3,3), 
                           st = c(11,11,11,11,11,12,12,12,13,13,13,13,21,21,22,22,22,23,23,23,23,31,31,31,31,
                                  31,32,32,32,33,33,33,33),
                           wetstart = c(0.93,1.02,0.89,0.91,0.93,0.96,1.09,0.89,0.88,0.83,0.85,0.84,0.67,0.61,
                                        0.74,0.63,0.7,0.52,0.32,0.75,0.5,1.17,1.08,0.98,1.1,0.92,1.11,1,1.07,1.07,
                                        1.16,1.01,1.06),
                           wetend = c(0.6522,0.3985,0.3448,0.1876,0.6097,0.5564,0.267900000000001,0.187000000000001,
                                      0.844200000000001,1.5315,0.878499999999999,1.491,0.5283,0.2046,0.0891999999999999,
                                      0.127700000000001,0.0493000000000006,1.5002,1.0441,0.7293,1.0104,0.492999999999999,
                                      0.355,0.3182,0.3672,0.3489,0.0616000000000003,0.105400000000001,0.227499999999999,
                                      1.0682,0.713200000000001,0.8569,0.611500000000001),
                           wetdelmass = c(0.2778,0.6215,0.5452,0.7224,0.3203,0.4036,0.8221,0.703,0.0358,-0.7015,-0.0285,
                                          -0.651,0.1417,0.4054,0.6508,0.5023,0.6507,-0.9802,-0.7241,0.0207,-0.5104,0.677,
                                          0.725,0.6618,0.7328,0.5711,1.0484,0.8946,0.8425,0.0018,0.4468,0.1531,0.4485))
wetmass.data

tstwetmass.data <-data.frame(tst = c(1.11,1.11,1.11,1.11,1.11,1.12,1.12,1.12,1.13,1.13,1.13,1.13,1.21,1.21,1.22,1.22,
                                       1.22,1.23,1.23,1.23,1.23,1.31,1.31,1.31,1.31,1.31,1.32,1.32,1.32,1.33,1.33,1.33,1.33,
                                       2.11,2.11,2.11,2.11,2.11,2.12,2.12,2.12,2.13,2.13,2.13,2.13,2.21,2.21,2.22,2.22,2.22,
                                       2.23,2.23,2.23,2.23,2.31,2.31,2.31,2.31,2.31,2.32,2.32,2.32,2.33,2.33,2.33,2.33),
                               wetmass = c(0.93,1.02,0.89,0.91,0.93,0.96,1.09,0.89,0.88,0.83,0.85,0.84,0.67,0.61,0.74,0.63,0.7,
                                           0.52,0.32,0.75,0.5,1.17,1.08,0.98,1.1,0.92,1.11,1,1.07,1.07,1.16,1.01,1.06,0.6522,0.3985,
                                           0.3448,0.1876,0.6097,0.5564,0.267900000000001,0.187000000000001,0.844200000000001,1.5315,
                                           0.878499999999999,1.491,0.5283,0.2046,0.0891999999999999,0.127700000000001,0.0493000000000006,
                                           1.5002,1.0441,0.7293,1.0104,0.492999999999999,0.355,0.3182,0.3672,0.3489,0.0616000000000003,
                                           0.105400000000001,0.227499999999999,1.0682,0.713200000000001,0.8569,0.611500000000001))
tstwetmass.data

#One-way ANOVA

##Wet Delta Mass
wetdelmass.aov.ST <- aov(wetdelmass ~ ST, data = wetmass.data)
summary(wetmass.aov.ST)

wetdelmass.aov.ST.factor = aov( wetdelmass ~ factor(ST), data = wetmass.data)
tukey.wetdelmass.gradient.ST<-TukeyHSD(wetdelmass.aov.ST.factor)
tukey.wetdelmass.gradient.ST
plot(tukey.wetdelmass.gradient.ST , las=1 , col="black")

##Wet time_site_treatment
tstwetmass.anova <- aov(wetmass ~ factor(tst), data = tstwetmass.data)
summary(tstwetmass.anova)

tstwetmass.tukey <- TukeyHSD(tstwetmass.anova)
print(tstwetmass.tukey)
plot(tstwetmass.tukey , las=1 , col="purple")

tstwetmass.cld <- multcompLetters4(tstwetmass.anova, tstwetmass.tukey)
print(tstwetmass.cld)



##boxplot from Tukey--troubleshooting 
cldwetdelmass <- multcompLetters4(wetdelmass.aov.ST.factor,tukey.wetdelmass.gradient.ST)
print(cldwetdelmass)

anova <- aov(mass ~ factor(ST), data = df.mass)
summary(anova)

tukey <- TukeyHSD(anova)
print(tukey)
plot(tukey , las=1 , col="purple")

cld <- multcompLetters4(anova, tukey)
print(cld)
###32    31    12    22    11    21    33    13    23 
###"a"  "ab"  "ab"  "ab"  "ab" "abc"   "b"  "cd"   "d" 

Tk <- group_by(df.mass, ST) %>%
  summarise(mean=mean(mass), quant = quantile(mass, probs = 0.75)) %>%
  arrange(desc(mean))
### extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$ST)
Tk$cld <- cld$Letters
print(Tk)
### boxplot
ggplot(df.mass, aes(ST, mass)) + 
  geom_boxplot(aes(fill = ST)) +
  labs(x="Site.Treatment", y="Mass (g)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(data = Tk, aes(x = ST, y = quant, label = cld), size = 3, vjust=-1, hjust =-1)

#Two-way ANOVA
gradient.mass.aov2 <- aov(mass_loss_g ~ Site + Treatment,data = mass_loss.data )
summary(gradient.mass.aov2)

two.way.plot <- ggplot(mass_loss.data, aes(x = Site, y = mass_loss_g, group=Treatment)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))
two.way.plot

aov.gradient.mass.factor = aov( mass_loss_g ~ factor(Site) + factor(Treatment), data = mass_loss.data)
tukey.mass.gradient<-TukeyHSD(aov.gradient.mass.factor)
tukey.mass.gradient
plot(tukey.mass.gradient , las=1 , col="black")

###Sites 2-3 p-value = 0.0000484, Sites 3-1 p-value = 0.0070439
###Treatments 2-3 p-value < 0.0000001, Treatments 3-1 p-value = 0.0000016

##interactions
gradient.mass.aov.interaction <- aov(mass_loss_g ~ Site * Treatment,data = mass_loss.data )
summary(gradient.mass.aov.interaction)
###potentially significant interaction effects between site and treatment effects

##Site One-way ANOVA
gradient.mass.site.aov <- aov(mass_loss_g ~ Site ,data = mass_loss.data )
summary(gradient.mass.site.aov)

gradient.mass.site.aov.factor = aov( mass_loss_g ~ factor(Site), data = mass_loss.data)
tukey.gradient.mass.site<-TukeyHSD(gradient.mass.site.aov.factor)
tukey.gradient.mass.site
plot(tukey.gradient.mass.site , las=1 , col="black")
###Sites 3-2 p-value = 0.0255675

##Treatment One-way ANOVA
gradient.mass.tx.aov <- aov(mass_loss_g ~ Treatment,data = mass_loss.data )
summary(gradient.mass.tx.aov)

aov.gradient.mass.tx.factor = aov( mass_loss_g ~ factor(Treatment), data = mass_loss.data)
tukey.gradient.mass.tx<-TukeyHSD(aov.gradient.mass.tx.factor)
tukey.gradient.mass.tx
plot(tukey.gradient.tx , las=1 , col="black")
### Tx 3-1 p-value = 0.0000193, Tx 3-2 p-value = 0.0000016

#Gradient GLM
gradient_mass.glm <- glm( mass_loss_g ~ Site + Treatment, data = mass_loss.data)
summary(gradient_mass.glm) 
plot(gradient_mass.glm)

gradient_mass.site.glm <- glm( mass_loss_g ~ Site , data = mass_loss.data)
summary(gradient_mass.site.glm) 
plot(gradient_mass.site.glm)

gradient_mass.tx.glm <- glm( mass_loss_g ~ Treatment, data = mass_loss.data)
summary(gradient_mass.tx.glm) 
plot(gradient_mass.tx.glm)


##Paired Two-Sample Welch's T-test: Welch’s t-test: this test assumes that both groups of data are sampled from populations that follow a normal distribution, but it does not assume that those two populations have the same variance.
###paired because the sample number for pre and post wet mass are equal
###wet masses, w=wet mass, i=initial, f=final
wi11<-c(0.93,1.02,0.89,0.91,0.93)			
wi12<-c(0.96,1.09,0.89)
wi13<-c(0.88,0.83,0.85,0.84)
wi21<-c(0.67,0.61)
wi22<-c(0.74,0.63,0.7)
wi23<-c(0.52,0.32,0.75,0.5)
wi31<-c(1.17,1.08,0.98,1.1,0.92)
wi32<-c(1.11,1,1.07)
wi33<-c(1.07,1.16,1.01,1.06)
wf11<-c(0.6522,0.3985,0.3448,0.1876,0.6097)			
wf12<-c(0.5564,0.267900000000001,0.187000000000001)
wf13<-c(0.844200000000001,1.5315,0.878499999999999,1.491)
wf21<-c(0.5283,0.2046)
wf22<-c(0.0891999999999999,0.127700000000001,0.0493000000000006)
wf23<-c(1.5002,1.0441,0.7293,1.0104)
wf31<-c(0.492999999999999,0.355,0.3182,0.3672,0.3489)
wf32<-c(0.0616000000000003,0.105400000000001,0.227499999999999)
wf33<-c(1.0682,0.713200000000001,0.8569,0.611500000000001)

###Dataframes by site, w=wet mass, i=initial, f=final

wdf_11<-data.frame(group = (c(rep("initial", 5),rep("final",5))),
                  wetmass11 = c(wi11,wf11))
print(wdf_11)
t_w11 <- t.test(wi11,wf11)
t_w11
##### *p-value = 0.003433

wdf_12<-data.frame(group = (c(rep("initial", 3),rep("final",3))),
                  wetmass12 = c(wi12,wf12))
print(wdf_12)
t_w12 <- t.test(wi12,wf12)
t_w12
##### *p-value = 0.01452

wdf_13<-data.frame(group = (c(rep("initial", 4),rep("final",4))),
                   wetmass13 = c(wi13,wf13))
print(wdf_13)
t_w13 <- t.test(wi13,wf13)
t_w13
##### p-value = 0.1714

wdf_21<-data.frame(group = (c(rep("initial", 2),rep("final",2))),
                   wetmass21 = c(wi21,wf21))
print(wdf_21)
t_w21 <- t.test(wi21,wf21)
t_w21
##### p-value = 0.3329

wdf_22<-data.frame(group = (c(rep("initial", 3),rep("final",3))),
                   wetmass22 = c(wi22,wf22))
print(wdf_22)
t_w22 <- t.test(wi22,wf22)
t_w22
##### *p-value = 0.0002125

wdf_23<-data.frame(group = (c(rep("initial", 4),rep("final",4))),
                   wetmass23 = c(wi23,wf23))
print(wdf_23)
t_w23 <- t.test(wi23,wf23)
t_w23
##### *p-value = 0.03241

wdf_31<-data.frame(group = (c(rep("initial", 5),rep("final",5))),
                   wetmass31 = c(wi31,wf31))
print(wdf_31)
t_w31 <- t.test(wi31,wf31)
t_w31
##### *p-value = 0.00000454

wdf_32<-data.frame(group = (c(rep("initial", 3),rep("final",3))),
                   wetmass32 = c(wi32,wf32))
print(wdf_32)
t_w32 <- t.test(wi32,wf32)
t_w32
##### *p-value = 0.000259

wdf_33<-data.frame(group = (c(rep("initial", 4),rep("final",4))),
                   wetmass33 = c(wi33,wf33))
print(wdf_33)
t_w33 <- t.test(wi33,wf33)
t_w33
##### p-value = 0.07177


###################################################################################################################################################
###################################################################################################################################################

#Litter Mass
ss<-c(1.1,1.1,1.1,1.1,1.1,1.2,1.2,1.2,1.3,1.3,1.3,1.3,2.1,2.1,2.2,2.2,2.2,2.3,2.3,2.3,2.3,3.1,3.1,3.1,3.1,3.1,3.2,3.2,3.2,3.3,3.3,3.3,3.3)
initialm<-c(0.819277372616575,0.898562279643985,0.784039636159948,0.801658504388261,0.819277372616575,0.845705674959045,0.960228318443082,0.784039636159948,0.775230202045791,0.731183031475007,0.748801899703321,0.739992465589164,0.434851159060847,0.395909264219577,0.480283369708995,0.408889895833333,0.454322106481481,0.337496421957672,0.207690105820106,0.486773685515873,0.324515790343915,0.476123230088495,0.439498366235534,0.398804073065577,0.447637224869526,0.374387497163603,0.451706654186521,0.406942931699569,0.435428936918539,0.435428936918539,0.4720538007715,0.411012361016564,0.431359507601543)
finalm<-c(0.616481650720748,0.376675770947896,0.3259167021903,0.177325908732309,0.576309203380006,0.526409725634767,0.25346003863687,0.176920594345259,0.273348442094039,0.495893318013529,0.284454639160878,0.482779586783004,0.501075563228312,0.194056521363832,0.0846033318946913,0.127700000000001,0.0493000000000006,0.349472523976465,0.243223745023215,0.169890889038819,0.235373309042675,0.469290289228526,0.337927084535754,0.302896896617682,0.34954035335642,0.332120450125422,0.0595849991739635,0.101952255080127,0.220058235585659,0.315746833808954,0.21081318280523,0.25328914238054,0.180751908700783)
delm<-c(0.202795721895827,0.521886508696089,0.458122933969648,0.624332595655952,0.242968169236569,0.319295949324277,0.706768279806212,0.607119041814689,0.501881759951752,0.235289713461479,0.464347260542443,0.25721287880616,-0.0662244041674653,0.201852742855744,0.395680037814303,0.281189895833332,0.405022106481481,-0.0119761020187934,-0.0355336392031095,0.316882796477054,0.0891424813012405,0.00683294085996961,0.10157128169978,0.0959071764478957,0.098096871513106,0.0422670470381817,0.392121655012558,0.304990676619442,0.21537070133288,0.119682103109584,0.26124061796627,0.157723218636024,0.25060759890076)

time <-c(rep("initial", 33), rep("final",33))
site<-c(1.1,1.1,1.1,1.1,1.1,1.2,1.2,1.2,1.3,1.3,1.3,1.3,2.1,2.1,2.2,2.2,2.2,2.3,2.3,2.3,2.3,3.1,3.1,3.1,3.1,3.1,3.2,3.2,3.2,3.3,3.3,3.3,3.3,
        1.1,1.1,1.1,1.1,1.1,1.2,1.2,1.2,1.3,1.3,1.3,1.3,2.1,2.1,2.2,2.2,2.2,2.3,2.3,2.3,2.3,3.1,3.1,3.1,3.1,3.1,3.2,3.2,3.2,3.3,3.3,3.3,3.3)
tss<-c(rep("i11",5),rep("f11",5), rep("i12",3),rep("f12",3),rep("i13",4),rep("f13",4),rep("i21",2),rep("f21",2),rep("i22",3),rep("f22",3),rep("i23",4),rep("f23",4),rep("i31",5),rep("f31",5),rep("i32",3),rep("f32",3),rep("i33",4),rep("f33",4))
littermass<-c(0.819277372616575,0.898562279643985,0.784039636159948,0.801658504388261,0.819277372616575,0.845705674959045,0.960228318443082,0.784039636159948,0.775230202045791,0.731183031475007,0.748801899703321,0.739992465589164,0.434851159060847,0.395909264219577,0.480283369708995,0.408889895833333,0.454322106481481,0.337496421957672,0.207690105820106,0.486773685515873,0.324515790343915,0.476123230088495,0.439498366235534,0.398804073065577,0.447637224869526,0.374387497163603,0.451706654186521,0.406942931699569,0.435428936918539,0.435428936918539,0.4720538007715,0.411012361016564,0.431359507601543,0.616481650720748,0.376675770947896,0.3259167021903,0.177325908732309,0.576309203380006,0.526409725634767,0.25346003863687,0.176920594345259,0.273348442094039,0.495893318013529,0.284454639160878,0.482779586783004,0.501075563228312,0.194056521363832,0.0846033318946913,0.127700000000001,0.0493000000000006,0.349472523976465,0.243223745023215,0.169890889038819,0.235373309042675,0.469290289228526,0.337927084535754,0.302896896617682,0.34954035335642,0.332120450125422,0.0595849991739635,0.101952255080127,0.220058235585659,0.315746833808954,0.21081318280523,0.25328914238054,0.180751908700783)


mass.data <- data.frame(time, site,littermass)
mass.data

delm.data <-data.frame(ss,delm, initialm,finalm)
delm.data

mass.tss.data <-data.frame(tss,littermass)
mass.tss.data

##One-way ANOVA and Tukey's post-hoc: litter mass
mass.aov <- aov(littermass ~ time + site, data = mass.data)
summary(mass.aov)

mass.aov.factor = aov( littermass ~ factor(time) + factor(site), data = mass.data)
tukey.mass<-TukeyHSD(mass.aov.factor)
tukey.mass
plot(tukey.mass , las=1 , col="black")

mass.cld <- multcompLetters4(mass.aov.factor, tukey.mass)
print(mass.cld)

mass.tss.aov<-aov(littermass ~ tss,data=mass.tss.data)
summary(mass.tss.aov)

mass.tss.aov.factor<-aov(littermass ~ factor(tss),data=mass.tss.data)
summary(mass.tss.aov.factor)
tukey.mass.tss<-TukeyHSD(mass.tss.aov.factor)
tukey.mass.tss
plot(tukey.mass.tss, las=1, col="purple")
tss.mass.cld<-multcompLetters4(mass.tss.aov.factor,tukey.mass.tss)
print(tss.mass.cld)


#One-way ANOVA and Tukey's post-hoc: delta mass
delm.aov <- aov(delm ~ ss, data = delm.data)
summary(delm.aov)

delm.aov.factor = aov( delm ~ factor(ss), data = delm.data)
tukey.delm<-TukeyHSD(delm.aov.factor)
tukey.delm
plot(tukey.delm , las=1 , col="black")

delm.cld <- multcompLetters4(delm.aov.factor, tukey.delm)
print(delm.cld)

##Paired Two-Sample Welch's T-test: Welch’s t-test: this test assumes that both groups of data are sampled from populations that follow a normal distribution, but it does not assume that those two populations have the same variance.
###paired because the sample number for each group are equal
###initial and final (dry) litter masses, i=initial, f=final

i11<-c(0.819277372616575,0.898562279643985,0.784039636159948,0.801658504388261,0.819277372616575)	
i12<-c(0.845705674959045,0.960228318443082,0.784039636159948)
i13<-c(0.775230202045791,0.731183031475007,0.748801899703321,0.739992465589164)
i21<-c(0.434851159060847,0.395909264219577)
i22<-c(0.480283369708995,0.408889895833333,0.454322106481481)
i23<-c(0.337496421957672,0.207690105820106,0.486773685515873,0.324515790343915)
i31<-c(0.476123230088495,0.439498366235534,0.398804073065577,0.447637224869526,0.374387497163603)
i32<-c(0.451706654186521,0.406942931699569,0.435428936918539)
i33<-c(0.435428936918539,0.4720538007715,0.411012361016564,0.431359507601543)
f11<-c(0.616481650720748,0.376675770947896,0.3259167021903,0.177325908732309,0.576309203380006)	
f12<-c(0.526409725634767,0.25346003863687,0.176920594345259)
f13<-c(0.273348442094039,0.495893318013529,0.284454639160878,0.482779586783004)
f21<-c(0.501075563228312,0.194056521363832)
f22<-c(0.0846033318946913,0.127700000000001,0.0493000000000006)
f23<-c(0.349472523976465,0.243223745023215,0.169890889038819,0.235373309042675)
f31<-c(0.469290289228526,0.337927084535754,0.302896896617682,0.34954035335642,0.332120450125422)
f32<-c(0.0595849991739635,0.101952255080127,0.220058235585659)
f33<-c(0.315746833808954,0.21081318280523,0.25328914238054,0.180751908700783)

###Paired Two-Sample Welch's T-test by site

t_s11 <- t.test(i11,f11)
t_s11
##### p-value = 0.006069

t_s12 <- t.test(i12,f12)
t_s12
##### p-value = 0.02066

t_s13 <- t.test(i13,f13)
t_s13
##### p-value = 0.008402

t_s21 <- t.test(i21,f21)
t_s21
##### p-value = 0.7352

t_s22 <- t.test(i22,f22)
t_s22
##### p-value = 0.0003165

t_s23 <- t.test(i23,f23)
t_s23
##### p-value = 0.2445

t_s31 <- t.test(i31,f31)
t_s31
##### p-value = 0.08381

t_s32 <- t.test(i32,f32)
t_s32
##### p-value = 0.01829

t_s33 <- t.test(i33,f33)
t_s33
##### p-value = 0.00322


#########################################################################################################################
#########################################################################################################################

# % ∆ Litter Mass

litter<-c(1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3)
site<-c(1,1,1,1,1,2,2,2,3,3,3,3,1,1,2,2,2,3,3,3,3,1,1,1,1,1,2,2,2,3,3,3,3)
pdelm<-c(24.7529992495883,58.0801710152871,58.4310936387645,77.8801188084912,29.6563993291536,37.7549730099352,73.6041904025668,77.4347384767718,64.7397068157707,32.1793180822087,62.0120302481097,34.7588510379458,
         0,50.984597001952,82.3847051073301,68.7690986494681,89.148668027227,0,0,65.098588092581,27.4693817538953,
         1.43512024370237,23.1107302103932,24.0486953181457,21.9143686143838,11.2896523944846,86.808917109873,74.9467929927349,49.4617337233083,27.4860242308551,55.3412804937302,38.374324861161,58.0971543421392,
         24.7529992495883,58.0801710152871,58.4310936387645,77.8801188084912,29.6563993291536,0,50.984597001952,1.43512024370237,23.1107302103932,24.0486953181457,21.9143686143838,11.2896523944846,
         37.7549730099352,73.6041904025668,77.4347384767718,82.3847051073301,68.7690986494681,89.148668027227,86.808917109873,74.9467929927349,49.4617337233083,
         64.7397068157707,32.1793180822087,62.0120302481097,34.7588510379458,0,0,65.098588092581,27.4693817538953,27.4860242308551,55.3412804937302,38.374324861161,58.0971543421392)

pdelm.df <-data.frame(litter,site,pdelm)
pdelm.df

#One-way ANOVA and Tukey's post-hoc: % delta mass
pdelm.aov <- aov(pdelm ~ site+litter, data = pdelm.df)
summary(pdelm.aov)

pdelm.aov.int <- aov(pdelm ~ site*litter, data = pdelm.df)
summary(pdelm.aov.int)

pdelm.aov.factor = aov( pdelm ~ factor(site+litter), data = pdelm.df)
tukey.pdelm<-TukeyHSD(pdelm.aov.factor)
tukey.pdelm
plot(tukey.pdelm , las=1 , col="black")

delm.cld <- multcompLetters4(delm.aov.factor, tukey.delm)
print(delm.cld)


