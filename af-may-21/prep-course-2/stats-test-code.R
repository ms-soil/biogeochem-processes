# Script for basic statistics of multiple groups

library(tidyverse)
library(readxl)

d <- read_xlsx("data/data-summary-2021.xlsx") %>% 
  slice(2:100)

as_tibble(d)

names(d)
d <- d %>% dplyr::rename(
  landuse = "land use"
)
names(d)[1]

str(d[1])
d[[1]] <- as.factor(d[[1]])
str(d[1])

d[1]

d$landuse <- plyr::mapvalues(d$landuse,
                             from = levels(d[[1]]),
                             to = c("CAC", "CAT", "FL", "FU"))

d[1]

for (i in list(4,5,6,7,8,9,10,11)){
  d[[i]] <- as.numeric(d[[i]])
}
as_tibble(d)


# normality
mod <- lm(d$`soil temp`~ d$landuse)
summary(mod)
# plot(mod)

plot(d$`soil temp`~ d$landuse)
d$`soil temp`
residuals(mod)
shapiro.test(residuals(mod))

MASS::truehist(residuals(mod))

car::leveneTest(d$`soil temp`~d$landuse)
               
d$soiltemp <- d$`soil temp`
 
m1 <- aov(soiltemp~landuse, data = d)
anova(m1)

TukeyHSD(m1)


names(d)
kruskal.test(SOC ~ landuse, d = d)


#install.packages("pgrimess")
#pgirmess::kruskalmc(d$SOC~d$landuse, probs = 0.05, cont = NULL)

library(agricolae)

result <- agricolae::LSD.test(m1, landuse, p.adj = "holm")
result <- LSD.test(m1, "landuse", p.adj = "holm")
result

plot(result, variation = "SD")
