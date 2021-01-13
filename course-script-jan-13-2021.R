#### loading libraries ####
library("multcomp")
library("car") # install.packages("car")
library("pgirmess") 
library("MASS")
library("tidyverse")

#### read in data ####
d <- read.csv("data/data-summary.csv", header = TRUE)
head(d)

#### first stats example ####
# co2flux with landuse

qplot(landuse, co2flux, data = d)

# test for equal variance
leveneTest(d$co2flux~d$landuse)
# p = 0.26 which is > 0.05

str(d$landuse)
#convert string into factor
d$landuse <- as.factor(d$landuse)

# test for normal distribution
shapiro.test(residuals(lm(d$co2flux~d$landuse)))
# p = 0.47 which is > 0.05
truehist(residuals(lm(d$co2flux~d$landuse)))

# finally, our global test
anova(aov(d$co2flux~d$landuse))
# p << 0.05
# interpretation: There are significant differnces 
# in co2 flux between our landuses 

# posthoc test (only if p for global
# test is < 0.05)

TukeyHSD(aov(d$co2flux~d$landuse))








