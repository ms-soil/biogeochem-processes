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

DATA<-DATA%>%filter(t<50)%>%filter(Country!="")%>%filter(Landuse == "Oil palm")
DATA<-DATA%>%filter(abs(d_Clay_50to100)<20)

y <- DATA$soc_pc #
x <- DATA$t

plot(y~x)

# from oliver: 
# fit2=nls(y~a+(b-a)*exp(-k*x),start=list(a=70, b=100, k=0.15), algorithm="port")
# summary(fit2)

# from oliver, adapted: 
fit2=nls(y~a+(100-a)*exp(-k*x),start=list(a=60, k=0.15), algorithm="port")
summary(fit2)

xx <- seq(0,605, length=1500)
lines(xx, predict(fit2, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit2)

g2 <- ggplot(DATA,aes(t,soc_pc, shape = Country, colour = Country)) +
    geom_hline(yintercept=100) +
  xlim(0,50) +
  ylim(0,150) +
  geom_point(show.legend = T, size = 3) +
  ggtitle("Change in organic carbon") +
  xlab("Years since deforestation") +
  ylab("Percent carbon remaining") +
  theme_bw()
g2


# Oliver ca.
func3<- function(x) {60.4+(100-60.4)*exp(-0.1296*x)}

g2 +
  stat_function(fun = func3, xlim=c(0,400), col = "blue", size = 1) 


  

