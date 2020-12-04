#### PT 1 ####
#### loading important libraries ####
# install.packages("tidyverse") # only needs to be done once
library("tidyverse") # loads the package for your session

# script handling: run 1 line: ctrl-enter / running whole script: ctrl-shft-enter

# read in data
d <- read.csv("data/van-straaten-soc.csv", header = T) # T means TRUE

#### inspecting data ###
d
# View(d)
names(d) # look at variables (columns)
str(d) # you want your number variables to be "num" for numeric

# this is the pipe: %>% # type ctrl-shift-M
# pipe helps you to configurate your dataset
d %>% select(Country, Landuse) %>% filter(Landuse == "Primary forest")
# just as an example; select is for variables and filter is for observations

names(d)

#### we will now create a dataset according to what we want to analyze ####
d2 <- d %>% 
  filter(d_Clay_50to100 < 20 & d_Clay_50to100 > -20) %>% # filter condition
  select(Country, Time.since.Deforestation, Landuse, SOC_prop_top10_C2) %>% 
  filter(Landuse == "Oil palm") %>% # choosing only oil palm
  mutate(time = Time.since.Deforestation, soc_pc = SOC_prop_top10_C2) %>% # remaming some variables
  select(Country, Landuse, time, soc_pc)

# consider transmute() (as another option)

d2

names(d2)
unique(d2$Landuse)

#### next will be plotting ####

# code here:
  # https://pad.gwdg.de/2vGQNUxAT6WFW07vMwCsGQ?edit

as_tibble(d2)
dim(d2) # dimensions of dataset

# basic plotting first:
plot(d2$time, d2$soc_pc)

# fitting decay curve
x <- d2$time
y <- d2$soc_pc

# we want to find the best fitting curve for our SOC loss curve
# a is the value of SOC that will be approached
# k is the decay rate to approach a
fit <- nls(y ~ a + (100 - a) * exp(-k * x), start=list(a = 60, k = 0.15), algorithm = "port")
xx <- seq(0,50,length=500)
lines(xx, predict(fit, data.frame(x=xx)), col = "red", lwd=2, lty=1)

summary(fit)
summary(fit)$coefficients
  
k <- summary(fit)$coefficients[2,1] # [row, column]
k
k <- round(k,2)
k

# We know turnover time (years) is 1/k

turnover_time <- round((1/k), 2) # rounding by 2 decimals
turnover_time

print(paste0("My turnover time is: ", turnover_time, " years :-)"))

#### make a nicer plot with ggplot ####

myplot <- ggplot(d2, aes(time, soc_pc, color = Country, shape = Country)) + # aes() is for aesthetics, like your x and y, color etc.
  geom_point(size = 3) +
  geom_hline(yintercept = 100, lty = 2) +
  annotate("text", label = "more than forest", x = 40, y = 105) + # add text
  annotate("text", label = "less than forest", x = 40, y = 95) + # add text
  xlab("Years since deforestation") +
  ylab("Carbon remaining [%]") +
  theme_classic() +
  ggtitle("Change in organic carbon")

myplot

# adding decay curve to the plot
fun1 <- function(x){60.4 + (100-60.4) * exp(-0.1296 * x)} 

myplot2 <- 
  myplot +
  stat_function(fun = fun1, xlim = c(0,45), col = "blue", size = 0.3) +
  theme(plot.title = element_text(hjust = 0.5)) # 0 is left allign, 0.5 is center, 1 is right allign

myplot2

ggsave("figs/carbon-plot-2020.png", plot = myplot2, width = 15, height = 7, units = "cm")
# ??ggsave if you need help


#### PT 2 Marcus prep: ####
library(tidyverse)
d <- read.csv("data/leaf-decomp-data-test.csv")
names(d)
head(d)

unique(d$Plot)

plot_choice = "C1"

d <- d %>% mutate(mass = Mass_g) %>% filter(Plot == plot_choice) %>% 
  select(day, Plot, mass)
  
head(d)
qplot(d$day, d$mass)

start_val <- mean(d$mass[d$day == 0])
start_val

x <- d$day
y <- d$mass

fit <- nls(y ~ start_val * 2.718^(-k * x), start=list(k=0.0001))
summary(fit)

k <- summary(fit)$coefficients[1]
k

p1 <- ggplot() +
  geom_point(aes(x,y)) +
  theme_bw()
p1

decay_line <- function(x){start_val * 2.718^(-k * x)}

p1 + stat_function(fun = decay_line) +
  annotate("text", label = plot_choice, x = 50, y = 10)

#### PT 2 Marcus prep: now as function overall ####  

my_own_function <- function(plot_choice){
  d <- read.csv("data/leaf-decomp-data-test.csv")
  d <- d %>% mutate(mass = Mass_g) %>% filter(Plot == plot_choice) %>% 
    select(day, Plot, mass)
  
  head(d)
  qplot(d$day, d$mass)
  
  start_val <- mean(d$mass[d$day == 0])
  start_val
  
  x <- d$day
  y <- d$mass
  
  fit <- nls(y ~ start_val * 2.718^(-k * x), start=list(k=0.0001))
  summary(fit)
  
  k <- summary(fit)$coefficients[1]
  k
  
  p1 <- ggplot() +
    geom_point(aes(x,y)) +
    theme_bw()
  p1
  
  decay_line <- function(x){start_val * 2.718^(-k * x)}
  
  p1 + stat_function(fun = decay_line) +
    annotate("text", label = plot_choice, x = 50, y = 10)
}

my_own_function("C1")




