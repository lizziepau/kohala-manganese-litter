library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(DescTools)
library(Hmisc)

csm0<-c(0.655894451601439,0.65571477712802,0.655901644806255,0.655738716443418,0.655959207705211,0.655841716074879,0.656012000666704,0.655964005999)
shapiro.test(csm0)#normal
msm0<-c(0.655932820900164,0.655940016662039,0.65598800066663,0.655966405225854,0.656009600426685,0.655932820900164,0.656,0.655834526862739)
shapiro.test(msm0)#normal
hsm0<-c(0.660758526481792,0.661799242634565,0.662994057206331,0.655889656397841,0.663738074877001,0.664158486207969,0.664695033885123,0.662861141657502)
shapiro.test(hsm0)#not normal
csm0.5<-c(0.643329166058498,0.643851605734006,0.636043262831059,0.654039570303387,0.655122243687962,0.655355964220234,0.655532077987002)
shapiro.test(csm0.5)#normal
msm0.5<-c(0.654541253832141,0.65440444321022,0.654404088661741,0.655561580514689,0.65575687563869,0.655584,0.655482796522666)
shapiro.test(msm0.5)#not normal
hsm0.5<-c(0.66039070222005,0.661303512532065,0.662530186059427,0.663466198731689,0.663918624794564,0.664535051660926,0.662621213635909)
shapiro.test(hsm0.5)#normal
csm24<-c(0.631015186226416,0.622184254786925,0.627902971186142,0.632594763839851,0.638779043280182,0.642526245625728)
shapiro.test(csm24)#normal
msm24<-c(0.652573865908384,0.629099472368787,0.626837620132215,0.637330608255209,0.6324,0.636249542017787)
shapiro.test(msm24)#normal
hsm24<-c(0.644670213120397,0.61369743139846,0.643639433490697,0.646136898680763,0.64885679368959,0.644226731980405)
shapiro.test(hsm24)#not normal
csm72<-c(0.441076045361949,0.414203251890754,0.404660670456474,0.393437413189622,0.412356607232128)
shapiro.test(csm72)#normal
msm72<-c(0.375204158706181,0.340939516800888,0.331878006777401,0.351328,0.385657662458782)
shapiro.test(msm72)#normal
hsm72<-c(0.346956454138578,0.317762551499738,0.347465703971118,0.400580379957782,0.442639208237529)
shapiro.test(hsm72)#normal
csm120<-c(0.265242008668778,0.225093297275191,0.206766822098504,0.207814697550408)
shapiro.test(csm120)#normal
msm120<-c(0.163446039010086,0.122584171063594,0.111314260318871,0.181765979415782)
shapiro.test(msm120)#normal
hsm120<-c(0.132067123485445,0.107397083809925,0.133461149680643,0.25775067479756)
shapiro.test(hsm120)#normal

tx.time.sm.0<- c(rep("c0",8), rep("m0",8), rep("h0",8))
tx.time.sm.0.5<- c(rep("c0.5",7), rep("m0.5",7), rep("h0.5",7))
tx.time.sm.24<- c(rep("c24",6), rep("m24",6), rep("h24",6))
tx.time.sm.72<- c(rep("c72",5), rep("m72",5), rep("h72",5))
tx.time.sm.120<- c(rep("c120",4), rep("m120",4), rep("h120",4))

tx.time.sm <- c(rep("c0",8), rep("m0",8), rep("h0",8),rep("c0.5",7), rep("m0.5",7), rep("h0.5",7),rep("c24",6), rep("m24",6), rep("h24",6),
                rep("c72",5), rep("m72",5), rep("h72",5),rep("c120",4), rep("m120",4), rep("h120",4))
sm<-c(csm0,msm0,hsm0,csm0.5,msm0.5,hsm0.5,csm24,msm24,hsm24,csm72,msm72,hsm72,csm120,msm120,hsm120)
df.sm <- data.frame(sm,tx.time.sm)
print(df.sm)

smglm <- glm(sm ~ tx.time.sm)
summary(smglm)         

shapiro.test(sm)


#ANOVA
aov.sm <- aov(sm~tx.time.sm, data=df.sm)
summary(aov.sm)

#Tukey's pot-hoc test (#Tukey's requires categories as factors)
aov.factor.sm = aov(sm ~ factor(tx.time.sm), data = df.sm)
tukey.sm<-TukeyHSD(aov.factor.sm)
tukey.sm

tukey.cld.sm <- multcompLetters4(aov.factor.sm, tukey.sm)
print(tukey.cld.sm)

kw.sm<-kruskal.test(sm~factor(tx.time.sm), data=df.sm)
kw.sm
dunn.bon.sm<-dunnTest(sm~factor(tx.time.sm), data=df.sm, method="bonferroni")
dunn.bon.sm



#Time 0: H0-C0
sm.0 <- c(csm0,msm0,hsm0)
tx.time.sm.0 <- c(rep("c0",8), rep("m0",8), rep("h0",8))
df.sm.0 <- data.frame(sm.0,tx.time.sm.0)
print(df.sm.0)
shapiro.test(csm0)#normal
shapiro.test(msm0)#normal
shapiro.test(hsm0)#not normal

kw.sm.0<-kruskal.test(sm.0~tx.time.sm.0, data=df.sm.0)
kw.sm.0
db.sm.0<-dunnTest(sm.0~factor(tx.time.sm.0), data=df.sm.0, method="bonferroni")
db.sm.0

#Time 0.5: 
sm.0.5 <- c(csm0.5,msm0.5,hsm0.5)
tx.time.sm.0.5 <- c(rep("c0.5",7), rep("m0.5",7), rep("h0.5",7))
df.sm.0.5 <- data.frame(sm.0.5,tx.time.sm.0.5)
print(df.sm.0.5)
shapiro.test(csm0.5)#normal
shapiro.test(msm0.5)#not normal
shapiro.test(hsm0.5)#normal

kw.sm.0.5<-kruskal.test(sm.0.5~tx.time.sm.0.5, data=df.sm.0.5)
kw.sm.0.5
db.sm.0.5<-dunnTest(sm.0.5~factor(tx.time.sm.0.5), data=df.sm.0.5, method="bonferroni")
db.sm.0.5

#Time 24: 
sm.24 <- c(csm24,msm24,hsm24)
tx.time.sm.24 <- c(rep("c24",6), rep("m24",6), rep("h24",6))
df.sm.24 <- data.frame(sm.24,tx.time.sm.24)
print(df.sm.24)
shapiro.test(csm24)#normal
shapiro.test(msm24)#normal
shapiro.test(hsm24)#not normal

kw.sm.24<-kruskal.test(sm.24~tx.time.sm.24, data=df.sm.24)
kw.sm.24
db.sm.24<-dunnTest(sm.24~factor(tx.time.sm.24), data=df.sm.24, method="bonferroni")
db.sm.24

#Time 72: 
sm.72 <- c(csm72,msm72,hsm72)
tx.time.sm.72 <- c(rep("c72",5), rep("m72",5), rep("h72",5))
df.sm.72 <- data.frame(sm.72,tx.time.sm.72)
print(df.sm.72)
shapiro.test(csm72)#normal
shapiro.test(msm72)#normal
shapiro.test(hsm72)#normal

aov.sm.72 <- aov(sm.72~factor(tx.time.sm.72), data=df.sm.72)
summary(aov.sm.72)
tukey.sm.72<-TukeyHSD(aov.sm.72)
tukey.sm.72

tukey.cld.sm.72 <- multcompLetters4(aov.sm.72, tukey.sm.72)
print(tukey.cld.sm.72)

#Time 120: 
sm.120 <- c(csm120,msm120,hsm120)
tx.time.sm.120 <- c(rep("c120",4), rep("m120",4), rep("h120",4))
df.sm.120 <- data.frame(sm.120,tx.time.sm.120)
print(df.sm.120)
shapiro.test(csm120)#normal
shapiro.test(msm120)#normal
shapiro.test(hsm120)#normal

aov.sm.120 <- aov(sm.120~factor(tx.time.sm.120), data=df.sm.120)
summary(aov.sm.120)
tukey.sm.120<-TukeyHSD(aov.sm.120)
tukey.sm.120

tukey.cld.sm.120 <- multcompLetters4(aov.sm.120, tukey.sm.120)
print(tukey.cld.sm.120)

########################################################################################################

#control
sm.c<-c(csm0,csm0.5,csm24,csm72,csm120)
tx.time.sm.c<-c(rep("c0",8),rep("c0.5",7),rep("c24",6),rep("c72",5),rep("c120",4))
df.sm.c <-data.frame(sm.c,tx.time.sm.c)
df.sm.c
shapiro.test(csm0)
shapiro.test(csm0.5)
shapiro.test(csm24)
shapiro.test(csm72)
shapiro.test(csm120)

aov.sm.c <- aov(sm.c ~ tx.time.sm.c, data=df.sm.c)
summary(aov.sm.c)

aov.factor.sm.c = aov(sm.c ~ factor(tx.time.sm.c), data = df.sm.c)
tukey.sm.c<-TukeyHSD(aov.factor.sm.c)
tukey.sm.c

tukey.cld.sm.c <- multcompLetters4(aov.factor.sm.c, tukey.sm.c)
print(tukey.cld.sm.c)

#moderate Mn
sm.m<-c(msm0,msm0.5,msm24,msm72,msm120)
tx.time.sm.m<-c(rep("m0",8),rep("m0.5",7),rep("m24",6),rep("m72",5),rep("m120",4))
df.sm.m <-data.frame(sm.m,tx.time.sm.m)
df.sm.m
shapiro.test(msm0)
shapiro.test(msm0.5)#not normal
shapiro.test(msm24)
shapiro.test(msm72)
shapiro.test(msm120)

kw.sm.m<-kruskal.test(sm.m~tx.time.sm.m, data=df.sm.m)
kw.sm.m
db.sm.m<-dunnTest(sm.m~factor(tx.time.sm.m), data=df.sm.m, method="bonferroni")
db.sm.m


#high Mn
sm.h<-c(hsm0,hsm0.5,hsm24,hsm72,hsm120)
tx.time.sm.h<-c(rep("h0",8),rep("h0.5",7),rep("h24",6),rep("h72",5),rep("h120",4))
df.sm.h <-data.frame(sm.h,tx.time.sm.h)
df.sm.h
shapiro.test(hsm0)#not normal
shapiro.test(hsm0.5)
shapiro.test(hsm24)#not normal
shapiro.test(hsm72)
shapiro.test(hsm120)

kw.sm.h<-kruskal.test(sm.h~tx.time.sm.h, data=df.sm.h)
kw.sm.h
db.sm.h<-dunnTest(sm.h~factor(tx.time.sm.h), data=df.sm.h, method="bonferroni")
db.sm.h
