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
  annotate("text", x = 300, y = 45, label = plot_choice, size = 10) +
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










