# variables
# mass_means: means of the mass in g per plot

# read in data
d <- read.csv("data/leaf-decomp-data-3.csv", h = T)
head(d)

# put in <- by typing Alt-Minus

# we now want to summarize the data by plot and day
library(tidyverse)

d_plotmeans <- d %>% group_by(day, Plot) %>% summarize(mass_means = mean(Mass_g))
head(d_plotmeans)


d_plotmeans <- d_plotmeans %>% mutate(
  soiltype = ifelse(str_detect(Plot, "C") == T, "Clay Acrisol", "Loam Acrisol")
)
head(d_plotmeans)

# just an example
animal <- "horse"
ifelse(animal == "bunny", print("yes"), print("no"))

# another way
# d_plotmeans$soiltype <- NA
# d_plotmeans$soiltype[d_plotmeans$Plot == "C1" | d_plotmeans$Plot == "C2" ] <- "Clay Acrisol"
# d_plotmeans$soiltype[d_plotmeans$Plot == "L1" | d_plotmeans$Plot == "L2" ] <- "Loam Acrisol"


# divide by clay and loam for fitting
d_clay <- d_plotmeans %>% filter(soiltype == "Clay Acrisol")
unique(d_clay$soiltype)

d_loam <- d_plotmeans %>% filter(soiltype == "Loam Acrisol")
unique(d_loam$soiltype)

# curve fitting for both clay and loam
#### clay ####
start_value_clay <- mean(d_clay$mass_means[d_clay$day == 0])
start_value_clay

x <- d_clay$day
y <- d_clay$mass_means

fit_clay <- nls(y ~ start_value_clay * 2.718^(-k * x), start=list(k = 0.005))
k_clay <- summary(fit_clay)$coefficients[1]
k_clay

e <- 2.718
decay_curve_clay <- function(x){start_value_clay * e^(-k_clay * x)}

#### loam ###
start_value_loam <- mean(d_loam$mass_means[d_loam$day == 0])
start_value_loam

x <- d_loam$day
y <- d_loam$mass_means

fit_loam <- nls(y ~ start_value_loam * 2.718^(-k * x), start=list(k = 0.005))
k_loam <- summary(fit_loam)$coefficients[1]
k_loam

e <- 2.718
decay_curve_loam <- function(x){start_value_loam * e^(-k_loam * x)}

#### plotting the two curves ###

p1 <- ggplot(data = d_plotmeans, aes(day, mass_means, col = soiltype)) +
  geom_point() +
  theme_bw()
p1

p2 <- p1 +
  stat_function(fun = decay_curve_clay, col = "pink") +
  stat_function(fun = decay_curve_loam, col = "lightblue")

p2

#### with real decay line and SE (standard error) ####

d_bars <- d_plotmeans %>% group_by(day) %>% summarize(overall_mean = mean(mass_means),
                                                     se = sd(mass_means)/sqrt(4))
head(d_bars)

p3 <- ggplot(d_bars, aes(day, overall_mean)) +
  geom_point() +
  geom_line() 

p3

p4 <- p3 +
  geom_errorbar(aes(
    ymin = overall_mean - se, ymax = overall_mean + se
  ), width = 1)
p4























