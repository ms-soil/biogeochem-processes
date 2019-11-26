#### LOAD RELEVANT LIBRARIES ####
library("multcomp")
library("car")
library("pgirmess")
library("sciplot")
library("agricolae")
library("statmod") 
library("graphics")
library("tidyverse")

#### SOC DECAY OIL PALM ####
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

g2 +
  stat_function(fun = func3, xlim=c(0,400), col = "blue", size = 1) 

#### Oil palm: FIT DECOMPOSITION OF Clay Acrisol 1 ####
DATA=read.table("data/leaf-decomp-data.txt",h=T)
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






#### Oil palm: FIT DECOMPOSITION OF all 4 landscapes ####
DATA=read.table("data/leaf-decomp-data.txt",h=T)
#View(DATA)
head(DATA)

# selecting data
names(DATA)
selection<-names(DATA)[1:5]
DATA<-DATA%>%select(selection)
head(DATA)

#group and summarize
DATA<-DATA%>%group_by(day, Time, Plot)%>%summarize(plot_mass_g = mean(Mass_g))
DATA<-data.frame(DATA)
head(DATA)

xx <- seq(0,400, length=800)
fit <- nls(y ~ (50 * exp(-k * x)), start=list(k=0.00001))
summary(fit)

k<-0.0048034


x<-DATA$day
y<-DATA$plot_mass_g
plot(x,y)
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)

func1<-func1<- function(x) {50 * exp(-k * x)}
func1

g1 <- ggplot(,aes(x,y)) +
  xlim(0,400) +
  ylim(0,60) +
  geom_point(show.legend = FALSE) +
  ggtitle("Leaf mass decline on all plots") +
  xlab("days") +
  ylab("remaining mass in g") +
  stat_function(fun = func1, xlim=c(0,400), col = "red") 
g1

DATA<-DATA%>%group_by(day,Time)%>%summarize(mean_mass_g = mean(plot_mass_g), 
                                            se_g = sd(plot_mass_g/sqrt(4)))
head(DATA)
x<-DATA$day
y<-DATA$mean_mass_g
z<-DATA$se_g

g2 <- ggplot(,aes(x,y)) +
  xlim(0,400) +
  ylim(0,60) +
  geom_line(show.legend = FALSE) +
  ggtitle("Leaf mass decline for area") +
  xlab("days") +
  ylab("remaining mass in g") +
  stat_function(fun = func1, xlim=c(0,400), col = "red")  +
  geom_errorbar(aes(ymin=y-z, ymax=y+z), colour="black", width=.1) 
g2


# pd <- position_dodge(0.1)
# , position=pd


