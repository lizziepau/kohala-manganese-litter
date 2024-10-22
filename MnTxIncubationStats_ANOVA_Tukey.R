
install.packages("rlang", dependencies = TRUE)
install.packages("tidyselect", dependencies = TRUE)
install.packages("tidyverse", dependencies = TRUE)

install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg"))
install.packages("https://cran.r-project.org/src/contrib/Archive/rlang/rlang_0.4.10.tar.gz", repos = NULL, type="source")

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(multcompView)

mn.data <- read.csv("MnTxIncubation_ANOVA_1.csv", header = TRUE, colClasses = c("factor", "numeric", "numeric", "numeric", "numeric", "numeric"))
summary(mn.data)

Mn2.aov <- aov(Mn2 ~ tx, data = mn.data)
summary(Mn2.aov)
tukey.Mn2<-TukeyHSD(Mn2.aov)
tukey.Mn2
plot(tukey.Mn2 , las=1 , col="black")

Mn3.aov <- aov(Mn3 ~ tx, data = mn.data)
summary(Mn3.aov)
tukey.Mn3<-TukeyHSD(Mn3.aov)
tukey.Mn3

Mn4.aov <- aov(Mn4 ~ tx, data = mn.data)
summary(Mn4.aov)
tukey.Mn4<-TukeyHSD(Mn4.aov)
tukey.Mn4

mn.data2 <- read.csv("MnTxIncubation_ANOVA_2.csv", header = TRUE)
summary(mn.data2)
mn.data2

nosite.aov <- aov(intensity ~ treatment, data = mn.data2)
summary(nosite.aov)
tukey.nosite<-TukeyHSD(nosite.aov)
tukey.nosite
plot(tukey.nosite , las=1 , col="black") 

#https://r-graph-gallery.com/84-tukey-test.html
# I need to group the treatments that are not different each other together.
generate_label_df <- function(tukey.nosite, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- tukey.nosite[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}

# Apply the function on my dataset
LABELS <- generate_label_df(tukey.nosite,"mn.data2$treatment")


# A panel of colors to draw each group with the same color :
my_colors <- c( 
  rgb(143,199,74,maxColorValue = 255),
  rgb(242,104,34,maxColorValue = 255), 
  rgb(111,145,202,maxColorValue = 255)
)

# Draw the basic boxplot
a <- boxplot(mn.data2$intensity ~ mn.data2$treatment , ylim=c(min(mn.data2$intensity) , 1.1*max(mn.data2$intensity)) , col=my_colors[as.numeric(LABELS[,1])] , ylab="intensity" , main="")

# I want to write the letter over each box. Over is how high I want to write it.
over <- 0.1*max( a$stats[nrow(a$stats),] )

#Add the labels
text( c(1:nlevels(mn.data2$treatment)) , a$stats[nrow(a$stats),]+over , LABELS[,1]  , col=my_colors[as.numeric(LABELS[,1])] )





three.way <- aov(intensity ~ Mn * site * tx, data = mn.data)
summary(three.way)
tukey.three.way<-TukeyHSD(three.way)



#Two-way ANOVA and Tukey's HSD

tx <- c(1, 1, 1,1, 1, 1, 2,2,2,2,2,2,2)
site <- c(1,1,2,2,3,3,1,1,2,2,2, 3,3)
Mn2 <- c(3660,4136,4597,5326,27405,20689,33369,20922,31550,52127,20281,14934,36556)
Mn3 <- c(1215,1228,1123,5322,28018,17344,22033,11286,18844,10020,14811,8570,46137)
Mn4 <- c(182,130,73,10091,6725,156,3348,3325,3784,1401,7738,1429,33977)
df.Mn <- data.frame(tx, site, Mn2,Mn3,Mn4)
print(df.Mn)

Mn2glm <- glm(Mn2 ~ site + tx)
summary(Mn2glm)   

Mn3glm <- glm(Mn3 ~ site + tx)
summary(Mn3glm)

Mn4glm <- glm(Mn4 ~ site + tx)
summary(Mn4glm)

#Two-way ANOVA to test effects of treatment and site on % carbon output
two.way.Mn2 <- aov(Mn2 ~ tx + site, data = df.Mn)
summary(two.way.Mn2)          

two.way.Mn3 <- aov(Mn3 ~ tx + site, data = df.Mn)
summary(two.way.Mn3)

two.way.Mn4 <- aov(Mn4 ~ tx + site, data = df.Mn)
summary(two.way.Mn4)

two.way.Mn <- aov(Mn2 + Mn3 + Mn4 ~ tx + site, data = df.Mn)
summary(two.way.Mn)

#Tukey's pot-hoc test (#Tukey's requires categories as factors)
aov.tx.factor = aov(Mn2 + Mn3 + Mn4 ~ factor(tx), data = df.Mn)
tukey.tx<-TukeyHSD(aov.tx.factor)
tukey.tx

aov.site.factor = aov(Mn2 + Mn3 + Mn4 ~ factor(site), data = df.Mn)
tukey.site<-TukeyHSD(aov.site.factor)
tukey.site
#Tukey's confirms that sites do not vary significantly but treatments do; Mn Tx varies significantly from background and control treatments

Mn2.aov.tx.factor = aov(Mn2 ~ factor(tx), data = df.Mn)
Mn2.tukey.tx<-TukeyHSD(Mn2.aov.tx.factor)
Mn2.tukey.tx

Mn3.aov.tx.factor = aov(Mn3 ~ factor(tx), data = df.Mn)
Mn3.tukey.tx<-TukeyHSD(Mn3.aov.tx.factor)
Mn3.tukey.tx

Mn4.aov.tx.factor = aov(Mn4 ~ factor(tx), data = df.Mn)
Mn4.tukey.tx<-TukeyHSD(Mn4.aov.tx.factor)
Mn4.tukey.tx
