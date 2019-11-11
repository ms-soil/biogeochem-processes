#### LOAD RELEVANT LIBRARIES ####
library("multcomp")
library("car")
library("pgirmess")
library("sciplot")
library("agricolae")
library("statmod") 
library("graphics")
library("tidyverse")

#### Oil palm: FIT DECOMPOSITION OF Clay Acrisol 1 ####
DATA=read.table("data/schmidt-nut-data.txt",h=T)
#View(DATA)
head(DATA)

DATA <- subset(DATA, Plot == "C1")
head(DATA)


#### part A: exponential
#1	50.81 (start mass)

xx <- seq(0,605, length=1500)
plot(DATA$Mass_g 
     ~ DATA$d)
y <- DATA$Mass_g
x <- DATA$d
fit <- nls(y ~ (50.81 * 2.718^(-k * x)), start=list(k=0.00001))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)

#### part A: exponential, using ggplot

#add fitted k from above
func1<- function(x) {50.81 * 2.718^(-0.0052707 * x)}
func1

dat1 = data.frame(x = DATA$d, y = DATA$Mass_g)
head(dat1)

g1 <- ggplot(dat1,aes(x,y)) +
  xlim(0,400) +
  ylim(0,60) +
  geom_point(show.legend = FALSE) +
  ggtitle("Leaf mass decline on Clay Aricsol 1") +
  xlab("days") +
  ylab("remaining mass in g")
g1

ggsave("figs/oil-palm-decomp.png", plot = g1, width = 10, height = 10)


g1 +
  stat_function(fun = func1, xlim=c(0,400), col = "red") 


#### part B: linear

xx <- seq(0,605, length=1500)
plot(DATA$Mass_g 
     ~ DATA$d)
y <- DATA$Mass_g
x <- DATA$d
fit <- nls(y ~ (a * x + b), start=list(a=1, b=1))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)

#### SOC DECAY ####
DATA=read.csv("data/van-straaten-soc.csv",h=T)
#View(DATA)
head(DATA)

DATA$t<-DATA$Time.since.Deforestation
DATA$soc_pc<-DATA$SOC_prop_top10_C2

DATA<-DATA%>%filter(t<50)
DATA<-DATA%>%group_by(t, Country)%>%summarize(soc_pc_mean = mean(soc_pc))
DATA

g2 <- ggplot(DATA,aes(t,soc_pc_mean)) +
  xlim(0,50) +
  ylim(0,150) +
  geom_point(show.legend = FALSE) 
g2

# group by t, give legend to country
