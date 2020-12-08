#### load my fav. library ####
library(tidyverse)

# load in your data using read.csv(), remember your header
# alternative is read.csv2 (when you have "," in your orig. data)

d <- read.csv("data/leaf-decomp-data-3.csv", h = T)
head(d)
# give only variable names
names(d) # names(d[1:3]) if you only want to see some

# Find out what plots there are using unique()
unique(d$Plot)

plot_choice <- "C1"
plot_choice

d <- d %>% mutate(mass = Mass_g, plot = Plot) %>% filter(Plot == plot_choice) %>% 
  select(plot, day, mass) # day, Plot, mass
head(d)

#### fitting decay function ####

qplot(d$day, d$mass) # plot mass with time

start_val <- mean(d$mass[d$day == 0]) # getting mean of day 0
start_val

x <- d$day
y <- d$mass

fit <- nls(y ~ start_val * 2.718^(-k * x), start=list(k = 0.05))
# the higher your k, the faster will your leaves disappear
summary(fit)

k <- summary(fit)$coefficients[1]
k <- round(k, 3)

p1 <- ggplot() +
  geom_point(aes(x,y)) +
  theme_bw()
p1

e <- 2.718

# define our decay curve
decay_curve <- function(x){start_val * e^(-k * x)}

# add decay curve to plot

p1 + stat_function(fun = decay_curve) +
  annotate("text", x = 300, yo = 45, label = plot_choice, size = 10) +
  annotate("text", label = 
             paste0("y = ",start_val, "* e ^ (-",k," *x)"), x = 120, y = 5) +
  xlab("days") +
  ylab("mass remaining [g]") +
  ggtitle(paste0("Decomposition in plot ", plot_choice)) +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) # center title

# make your own function
add_two_numbers <- function(number1, number2){
  number1 + number2
}

add_two_numbers(number1 = 100, number2 = 50)
add_two_numbers(100, 50)

#### write your own decay-plotting function ####

plot_decay <- function(plot_choice){
  d <- read.csv("data/leaf-decomp-data-3.csv", h = T)

  d <- d %>% mutate(mass = Mass_g, plot = Plot) %>% filter(Plot == plot_choice) %>% 
    select(plot, day, mass) # day, Plot, mass
  head(d)
  
  start_val <- mean(d$mass[d$day == 0]) # getting mean of day 0
  start_val
  
  x <- d$day
  y <- d$mass
  
  fit <- nls(y ~ start_val * 2.718^(-k * x), start=list(k = 0.05))
  # the higher your k, the faster will your leaves disappear
  summary(fit)
  
  k <- summary(fit)$coefficients[1]
  k <- round(k, 3)
  
  p1 <- ggplot() +
    geom_point(aes(x,y)) +
    theme_bw()
  p1
  
  e <- 2.718
  
  # define our decay curve
  decay_curve <- function(x){start_val * e^(-k * x)}
  
  # add decay curve to plot
  
 p1 + stat_function(fun = decay_curve) +
    annotate("text", x = 300, y = 45, label = plot_choice, size = 10) +
    annotate("text", label = 
               paste0("y = ",start_val, "* e ^ (-",k," *x)"), x = 120, y = 5) +
    xlab("days") +
    ylab("mass remaining [g]") +
    ggtitle(paste0("Decomposition in plot ", plot_choice)) +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) # center title
 }

plot1 <- plot_decay(plot_choice = "C1")
plot2 <- plot_decay(plot_choice = "C2")
plot3 <- plot_decay(plot_choice = "L1")
plot4 <- plot_decay(plot_choice = "L2")

# plot together and save
# install.packages("gridExtra")
library(gridExtra)
all_together <- grid.arrange(plot1, plot2, plot3, plot4,  ncol = 2)
all_together

ggsave("figs/all-decompositions.png", plot = all_together,
       width = 20, height = 20, units = "cm")

########
########

#### prep for Wed Dec 12, 2020 ####

library(tidyverse)
# read in data again
d_raw <- read.csv("data/leaf-decomp-data-3.csv", h = T)
d_raw

names(d_raw)

d_plotmeans <- d_raw %>% group_by(day, Plot) %>% summarize(mass = mean(Mass_g))
as_tibble(d_plotmeans)

# shorten name
d <- d_plotmeans
names(d)

d <- d %>% mutate(
  soiltype = ifelse(str_detect(Plot, "C") == T, "Clay Acrisol", "Loam Acrisol" ))

qplot(d$day, d$mass, col = d$soiltype)


# divide into clay and loam for fitting
d_clay <- d %>% filter(soiltype == "Clay Acrisol")
unique(d_clay$soiltype)

d_loam<- d %>% filter(soiltype == "Loam Acrisol")
unique(d_loam$soiltype)

# fitting clay
start_val_clay <- mean(d_clay$mass[d_clay$day == 0]) # getting mean of day 0
start_val_clay

x <- d_clay$day
y <- d_clay$mass

fit_clay <- nls(y ~ start_val_clay * 2.718^(-k * x), start=list(k = 0.05))
k_clay <- summary(fit_clay)$coefficients[1]
k_clay <- round(k_clay, 5)
k_clay

e <- 2.718
decay_curve_clay <- function(x){start_val_clay * e^(-k_clay * x)}

# fitting loam
start_val_loam <- mean(d_loam$mass[d_loam$day == 0]) # getting mean of day 0
start_val_loam

x <- d_loam$day
y <- d_loam$mass

fit_loam <- nls(y ~ start_val_loam * 2.718^(-k * x), start=list(k = 0.05))
k_loam <- summary(fit_loam)$coefficients[1]
k_loam <- round(k_loam, 5)
k_loam

decay_curve_loam <- function(x){start_val_loam * e^(-k_loam * x)}

#### plotting with two curves

p1 <- ggplot(data = d, aes(day,mass, col = soiltype)) +
  geom_point() +
  theme_bw()
p1

p1 +
  stat_function(fun = decay_curve_clay, col = "red") +
  stat_function(fun = decay_curve_loam, col = "blue")


#### with line and SE ####

d_bars<-d_plotmeans%>%group_by(day)%>%summarize(mean_mass = mean(mass), se = sd(mass/sqrt(4)))
head(d_bars)

g2 <- ggplot(d_bars, aes(day, mean_mass)) +
  geom_line()

g2

g2 + 
  geom_errorbar(aes(ymin=mean_mass-se, ymax=mean_mass+se), colour="black", width=.1) 










