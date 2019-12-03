library(tidyverse)
library(readxl)

#### reading in data directly from excel
d<-read_xlsx("data/pract-data-summary.xlsx")%>%slice(2:1000)

head(d)
names(d)
d<-d%>%dplyr::rename(
  landuse = 'land use',
  bd = 'bulk density',
  ecec = 'Effective cation exchange capacity',
  n2fix = 'N2 fixation',
  co2flux = 'CO2 fluxes',
  totn = 'total N',
  n2oflux = 'N2O fluxes',
  ch4flux = 'CH4 fluxes',
  soiltemp = 'soil temp'
)
names(d)

d[[1]]<-as.factor(d[[1]])
levels(d[[1]])

library(plyr)
d$landuse<-mapvalues(d$landuse, from = levels(d[[1]]), 
              to = c("CAC", "CAT", "FL", "FU"))
detach(package:plyr)

class(d[[3]])<-"numeric" 
head(d)

for (i in list(4,5,6,7,8,9,10,11)) {
 class(d[[i]])<-"numeric" 
}
head(d)

unique(d$landuse)

#### stats ####
# Normality: Shapiro-Wilk test of model residuals 
# (package stats):
shapiro.test(residuals(lm(d$soiltemp~d$landuse)))
res<-residuals(lm(d$soiltemp~d$landuse))
library(MASS)
truehist(res)

# Homogeneity of variances: Levene's test 
# (package car; may need package lme4 as well)
library(car)
leveneTest(d$soiltemp~d$landuse)
plot(d$soiltemp~d$landuse)


# in case of parametric data 
# (both conditions hold with p > 0.05)
model1=aov(soiltemp  ~ landuse, data = d)
anova(model1)
# post-hoc test
TukeyHSD(model1)

library(agricolae)
result<-LSD.test(model1, "landuse", p.adj = "holm")

plot(result, variation = "SD")
# try variation = "SE" - what happens?

# in case on non-parametric data
# (one/both of the conditions do not hold with p < 0.05)

kruskal.test(SOC~landuse, data=d)

library(pgirmess)
kruskalmc(d$SOC~d$landuse, probs=0.05, cont=NULL) 


