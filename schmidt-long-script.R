#### LOAD RELEVANT LIBRARIES ####
library("multcomp")
library("car")
library("pgirmess")
library("sciplot")
library("agricolae")
library("statmod") 
library("graphics")

DATA=read.table("data/Nut_data.txt",h=T)
#View(DATA)
head(DATA)

#### ERMITTLUNG VON EXP. DEKOMPOSITIONSRATEN ####
d1 <- subset(DATA, Subplot == "1")
d1
d2 <- subset(DATA, Subplot == "2")
d2
d3 <- subset(DATA, Subplot == "3")
d3
d4 <- subset(DATA, Subplot == "4")
d4
d5 <- subset(DATA, Subplot == "5")
d5
d6 <- subset(DATA, Subplot == "6")
d6
d7 <- subset(DATA, Subplot == "7")
d7
d8 <- subset(DATA, Subplot == "8")
d8
d9 <- subset(DATA, Subplot == "9")
d9
d10 <- subset(DATA, Subplot == "10")
d10
d11 <- subset(DATA, Subplot == "11")
d11
d12 <- subset(DATA, Subplot == "12")
d12
d13 <- subset(DATA, Subplot == "13")
d13
d14 <- subset(DATA, Subplot == "14")
d14
d15 <- subset(DATA, Subplot == "15")
d15
d16 <- subset(DATA, Subplot == "16")
d16

#1	50.81 (start mass)
xx <- seq(0,605, length=1500)
plot(d1$Mass_g 
     ~ d1$d)
y <- d1$Mass_g
x <- d1$d
fit <- nls(y ~ (50.81 * 2.718^(-k * x)), start=list(k=0.00001))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)


#2	52.41
xx <- seq(0,605, length=1500)
plot(d2$Mass_g 
     ~ d2$d)
y <- d2$Mass_g
x <- d2$d
fit <- nls(y ~ (52.41 * 2.718^(-k * x)), start=list(k=0.00001))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)

#3	48.41
xx <- seq(0,605, length=1500)
plot(d3$Mass_g 
     ~ d3$d)
y <- d3$Mass_g
x <- d3$d
fit <- nls(y ~ (48.41 * 2.718^(-k * x)), start=list(k=0.00001))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)

#4	50.89
xx <- seq(0,605, length=1500)
plot(d4$Mass_g 
     ~ d4$d)
y <- d4$Mass_g
x <- d4$d
fit <- nls(y ~ (50.89 * 2.718^(-k * x)), start=list(k=0.00001))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)

#5	47.51
xx <- seq(0,605, length=1500)
plot(d5$Mass_g 
     ~ d5$d)
y <- d5$Mass_g
x <- d5$d
fit <- nls(y ~ (47.51 * 2.718^(-k * x)), start=list(k=0.00001))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)

#6	51.19
xx <- seq(0,605, length=1500)
plot(d6$Mass_g 
     ~ d6$d)
y <- d6$Mass_g
x <- d6$d
fit <- nls(y ~ (51.19 * 2.718^(-k * x)), start=list(k=0.00001))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)

#7	51.72
xx <- seq(0,605, length=1500)
plot(d7$Mass_g 
     ~ d7$d)
y <- d7$Mass_g
x <- d7$d
fit <- nls(y ~ (51.72 * 2.718^(-k * x)), start=list(k=0.00001))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)

#8	48.56
xx <- seq(0,605, length=1500)
plot(d8$Mass_g 
     ~ d8$d)
y <- d8$Mass_g
x <- d8$d
fit <- nls(y ~ (48.56 * 2.718^(-k * x)), start=list(k=0.00001))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)

#9	48.45
xx <- seq(0,605, length=1500)
plot(d9$Mass_g 
     ~ d9$d)
y <- d9$Mass_g
x <- d9$d
fit <- nls(y ~ (48.45 * 2.718^(-k * x)), start=list(k=0.00001))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)

#10	49.65
xx <- seq(0,605, length=1500)
plot(d10$Mass_g 
     ~ d10$d)
y <- d10$Mass_g
x <- d10$d
fit <- nls(y ~ (49.65 * 2.718^(-k * x)), start=list(k=0.00001))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)

#11	51.12
xx <- seq(0,605, length=1500)
plot(d11$Mass_g 
     ~ d11$d)
y <- d11$Mass_g
x <- d11$d
fit <- nls(y ~ (51.12 * 2.718^(-k * x)), start=list(k=0.00001))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)

#12	49.72
xx <- seq(0,605, length=1500)
plot(d12$Mass_g 
     ~ d12$d)
y <- d12$Mass_g
x <- d12$d
fit <- nls(y ~ (49.72 * 2.718^(-k * x)), start=list(k=0.00001))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)

#13	50.24
xx <- seq(0,605, length=1500)
plot(d13$Mass_g 
     ~ d13$d)
y <- d13$Mass_g
x <- d13$d
fit <- nls(y ~ (50.24 * 2.718^(-k * x)), start=list(k=0.00001))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)

#14	50.41
xx <- seq(0,605, length=1500)
plot(d14$Mass_g 
     ~ d14$d)
y <- d14$Mass_g
x <- d14$d
fit <- nls(y ~ (50.41 * 2.718^(-k * x)), start=list(k=0.00001))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)

#15	52.8
xx <- seq(0,605, length=1500)
plot(d15$Mass_g 
     ~ d15$d)
y <- d15$Mass_g
x <- d15$d
fit <- nls(y ~ (52.8 * 2.718^(-k * x)), start=list(k=0.00001))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)

#16	50.69
xx <- seq(0,605, length=1500)
plot(d16$Mass_g 
     ~ d16$d)
y <- d16$Mass_g
x <- d16$d
fit <- nls(y ~ (50.69 * 2.718^(-k * x)), start=list(k=0.00001))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)



#### ERMITTLUNG VON UNTERSCHIEDEN IN DER DEKOMPOSITION ####
kDATA=read.table("data/k_data.txt",h=T)
summary(kDATA)

shapiro.test((kDATA$k))
bartlett.test(k~Location,data=kDATA)

m1=aov(k ~ Location, data=kDATA)
anova(m1)
## p = 0.002373 **

#TukeyHSD(m1)
#$Location
#$Location
#diff          lwr           upr     p adj
#C2-C1 -0.000254425 -0.000741461  2.326110e-04 0.4399465
#L1-C1 -0.000773825 -0.001260861 -2.867890e-04 0.0024282 !
#L2-C1 -0.000581400 -0.001068436 -9.436403e-05 0.0183210 !
#L1-C2 -0.000519400 -0.001006436 -3.236403e-05 0.0355030 !
#L2-C2 -0.000326975 -0.000814011  1.600610e-04 0.2435605
#L2-L1  0.000192425 -0.000294611  6.794610e-04 0.6540707

#C1 x      a	  	  
#C2 x x    ab	  
#L2   x x  bc	  
#L1     x  c	  	 


#### GRAFIK FÜR DIE VIER DEKOMPOSITIONSRATEN EXPONENTIELL
##für C1:
curve(50 * 2.718^(-0.00524965 * x), 0, 374, 
      col = "blue", xlab = "days", ylab = "remaining dry mass (g)",
      mtext ="clay 1"
)

##für C2:
curve(50 * 2.718^(-0.004995225 * x), 0, 374, col = "blue", add = TRUE)

##für L1:
curve(50 * 2.718^(-0.004475825 * x), 0, 374, col = "red", add = TRUE)

##für L2:
curve(50 * 2.718^(-0.00466825 * x), 0, 374, col = "red", add = TRUE)







#### TRYING LINEAR RELATIONSHIP OF DECOMPOSITION ####

#! same results with lm fit and nls fit


###LINEAR A, B
#1	50.81 (start mass)
xx <- seq(0,605, length=1500)
plot(d1$Mass_g 
     ~ d1$d)
y <- d1$Mass_g
x <- d1$d
fit <- nls(y ~ (a * x + b), start=list(a=1, b=1))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)

##non-linear and linear functions work similarly well
##would a linear function also lead to significant differences?


#2	52.41
xx <- seq(0,605, length=1500)
plot(d2$Mass_g 
     ~ d2$d)
y <- d2$Mass_g
x <- d2$d
fit <- nls(y ~ (a * x + b), start=list(a=1, b=1))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)




#3	48.41
xx <- seq(0,605, length=1500)
plot(d3$Mass_g 
     ~ d3$d)
y <- d3$Mass_g
x <- d3$d
fit <- nls(y ~ (a * x + b), start=list(a=1, b=1))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)




#4	50.89
xx <- seq(0,605, length=1500)
plot(d4$Mass_g 
     ~ d4$d)
y <- d4$Mass_g
x <- d4$d
fit <- nls(y ~ (a * x + b), start=list(a=1, b=1))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)



#5	47.51
xx <- seq(0,605, length=1500)
plot(d5$Mass_g 
     ~ d5$d)
y <- d5$Mass_g
x <- d5$d
fit <- nls(y ~ (a * x + b), start=list(a=1, b=1))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)





#6	51.19
xx <- seq(0,605, length=1500)
plot(d6$Mass_g 
     ~ d6$d)
y <- d6$Mass_g
x <- d6$d
fit <- nls(y ~ (a * x + b), start=list(a=1, b=1))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)



#7	51.72
xx <- seq(0,605, length=1500)
plot(d7$Mass_g 
     ~ d7$d)
y <- d7$Mass_g
x <- d7$d
fit <- nls(y ~ (a * x + b), start=list(a=1, b=1))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)



#8	48.56
xx <- seq(0,605, length=1500)
plot(d8$Mass_g 
     ~ d8$d)
y <- d8$Mass_g
x <- d8$d
fit <- nls(y ~ (a * x + b), start=list(a=1, b=1))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)


#9	48.45
xx <- seq(0,605, length=1500)
plot(d9$Mass_g 
     ~ d9$d)
y <- d9$Mass_g
x <- d9$d
fit <- nls(y ~ (a * x + b), start=list(a=1, b=1))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)


#10	49.65
xx <- seq(0,605, length=1500)
plot(d10$Mass_g 
     ~ d10$d)
y <- d10$Mass_g
x <- d10$d
fit <- nls(y ~ (a * x + b), start=list(a=1, b=1))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)


#11	51.12
xx <- seq(0,605, length=1500)
plot(d11$Mass_g 
     ~ d11$d)
y <- d11$Mass_g
x <- d11$d
fit <- nls(y ~ (a * x + b), start=list(a=1, b=1))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)


#12	49.72
xx <- seq(0,605, length=1500)
plot(d12$Mass_g 
     ~ d12$d)
y <- d12$Mass_g
x <- d12$d
fit <- nls(y ~ (a * x + b), start=list(a=1, b=1))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)


#13	50.24
xx <- seq(0,605, length=1500)
plot(d13$Mass_g 
     ~ d13$d)
y <- d13$Mass_g
x <- d13$d
fit <- nls(y ~ (a * x + b), start=list(a=1, b=1))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)



#14	50.41
xx <- seq(0,605, length=1500)
plot(d14$Mass_g 
     ~ d14$d)
y <- d14$Mass_g
x <- d14$d
fit <- nls(y ~ (a * x + b), start=list(a=1, b=1))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)



#15	52.8
xx <- seq(0,605, length=1500)
plot(d15$Mass_g 
     ~ d15$d)
y <- d15$Mass_g
x <- d15$d
fit <- nls(y ~ (a * x + b), start=list(a=1, b=1))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)


#16	50.69
xx <- seq(0,605, length=1500)
plot(d16$Mass_g 
     ~ d16$d)
y <- d16$Mass_g
x <- d16$d
fit <- nls(y ~ (a * x + b), start=list(a=1, b=1))
lines(xx, predict(fit, data.frame(x=xx)), col="red", lwd=2, lty=1)
summary(fit)









#### ERMITTLUNG VON UNTERSCHIEDEN IN DER DEKOMPOSITION LINEAR four groups ####
kDATA=read.table("data/k_data.txt",h=T)
summary(kDATA)

shapiro.test((kDATA$k_linear))
bartlett.test(k_linear~Location,data=kDATA)

m1=aov(k_linear ~ Location, data=kDATA)
anova(m1)
## p = 0.2783

## -> no difference when linear decomposition, where parameters are just as well






#### GRAFIK FÜR DIE VIER DEKOMPOSITIONSRATEN LINEAR ####
##für C1:
#curve(-0.1239235 * x + 45.77125975, 0, 374, 
#col = "blue", xlab = "days", ylab = "remaining dry mass (g)",
#mtext ="clay 1"
#)

## first curve added ##

##für C1:
curve(-0.1239235 * x + 45.77125975, 0, 374, col = "blue", add = TRUE)

##für C2:
curve(-0.12061825 * x + 45.44261575, 0, 374, col = "blue", add = TRUE)

##für L1:
curve(-0.12001375 * x + 47.06239525, 0, 374, col = "red", add = TRUE)

##für L2:
curve(-0.11612425 * x + 46.1424635, 0, 374, col = "red", add = TRUE)








#### NUTRIENT CHANGE DATA 4 groups####


D2=read.table("data/Nut_change_release_data.txt",h=T)
summary(DATA)

##N change

shapiro.test(D2$N_change) 
leveneTest(D2$N_change~D2$Plot)
#ok
m1=aov(D2$N_change~D2$Plot)
summary(m1)
#0.439


##P change

shapiro.test(D2$P_change) 
leveneTest(D2$P_change~D2$Plot)
#ok
m1=aov(D2$P_change~D2$Plot)
summary(m1)
#0.311

##K change

shapiro.test(D2$K_change) 
leveneTest(D2$K_change~D2$Plot)
#ok
m1=aov(D2$K_change~D2$Plot)
summary(m1)
#0.0186 *
TukeyHSD(m1)
#C2-C1  0.43100942 -1.09430749 1.956326 0.8350788
#L1-C1  1.59381345  0.06849653 3.119130 0.0396854 x
#L2-C1  1.56325316  0.03793624 3.088570 0.0440034 x
#L1-C2  1.16280402 -0.36251289 2.688121 0.1616317
#L2-C2  1.13224373 -0.39307318 2.657561 0.1773510
#L2-L1 -0.03056029 -1.55587720 1.494757 0.9999199

##Ca change

shapiro.test(D2$Ca_change) 
leveneTest(D2$Ca_change~D2$Plot)
#ok
m1=aov(D2$Ca_change~D2$Plot)
summary(m1)
#0.00583 **
TukeyHSD(m1)
#C2-C1 -3.047770 -5.878350 -0.2171908 0.0336626 x
#L1-C1 -1.698153 -4.528733  1.1324266 0.3280995
#L2-C1 -4.107834 -6.938414 -1.2772543 0.0048508 x
#L1-C2  1.349617 -1.480962  4.1801971 0.5138437
#L2-C2 -1.060064 -3.890643  1.7705162 0.6894327
#L2-L1 -2.409681 -5.240261  0.4208988 0.1055719



##Mg change

shapiro.test(D2$Mg_change) 
leveneTest(D2$Mg_change~D2$Plot)
#ok
m1=aov(D2$Mg_change~D2$Plot)
summary(m1)
#0.000353 ***
  TukeyHSD(m1)
#C2-C1  0.2192467 -0.4330791  0.8715726 0.7534923
#L1-C1 -0.8120096 -1.4643354 -0.1596837 0.0140487 x
#L2-C1 -0.9282709 -1.5805967 -0.2759450 0.0056012 x
#L1-C2 -1.0312563 -1.6835822 -0.3789304 0.0025261 x
#L2-C2 -1.1475176 -1.7998435 -0.4951917 0.0010578 x
#L2-L1 -0.1162613 -0.7685872  0.5360646 0.9503288



#NUTRIENT RELEASE DATA

#Same significance because all values are equally treated
#but test for Mg to be sure
#--> yields same result






#### PART II:  COMBINE BOTH CLAY AND BOTH LOAM ####


# NOTE (2 groups = differents stats than anova):
  
#-use shapiro.test for normality of each group
#-use Fisher's F-test (var.test(a,b) for variance comparison

#-if assumptions hold: use t.test(a,b, var.equal=TRUE)

#-if not, use Wilcoxon-Mann-Whitney test (wilcox.tes(y~x, data=...)


##### data for decomposition graphs Clay and Loam #####

kDATA=read.table("data/k_data.txt",h=T)
kDATA

kDATA_C <- subset(kDATA, CorL=="C")
kDATA_C

kDATA_L <- subset(kDATA, CorL=="L")
kDATA_L

#### GRAFIK FÜR DIE ZWEI DEKOMPOSITIONSRATEN EXPONENTIELL ####

##für C:
curve(50 * 2.718^(-0.005122438 * x), 0, 374, 
col = "blue", xlab = "days", ylab = "remaining dry mass (g)",
mtext ="clay 1"
)

##für L:
curve(50 * 2.718^(-0.004572038 * x), 0, 374, col = "red", add = TRUE)

#points(mclay ~ day, col="blue")
#points(mloam ~ day, col="red")

day<-c(
0,
36,
66,
99,
125,
157,
189,
221,
251,
281,
311,
340,
374)

mclay<-c(50.1875,
42.92375,
35.19625,
31.84375,
29.125,
25.00125,
20.53875,
18.03625,
14.47375,
10.77,
7.7325,
4.71,
2.7875)

mloam<-c(50.385,
43.6725,
36.55625,
32.77875,
30.57625,
27.205,
23.98625,
20.7,
17.84625,
11.94625,
9.4325,
6.65125,
4.82375)

MS_mean_point_y_error<-function(px, py, pySE, color, type)
{
  points(px, py, col=color, pch=type, cex=0.5)
  arrows(px			    , py-pySE, 	px			  , py+pySE	, code = 3, angle = 90, length = 0.01, col=color, lwd=1.5)
}


MS_mean_point_y_error(0, 50.19, 0.63, "blue", 1)
MS_mean_point_y_error(36, 42.92, 0.32, "blue", 1)
MS_mean_point_y_error(66, 35.20, 0.61, "blue", 1)
MS_mean_point_y_error(99, 31.84, 0.64, "blue", 1)
MS_mean_point_y_error(125, 29.13, 0.66, "blue", 1)
MS_mean_point_y_error(157, 25.00, 0.61, "blue", 1)
MS_mean_point_y_error(189, 20.54, 0.69, "blue", 1)
MS_mean_point_y_error(221, 18.04, 0.77, "blue", 1)
MS_mean_point_y_error(251, 14.47, 0.86, "blue", 1)
MS_mean_point_y_error(281, 10.77, 0.81, "blue", 1)
MS_mean_point_y_error(311, 7.73, 0.48, "blue", 1)
MS_mean_point_y_error(340, 4.71, 0.43, "blue", 1)
MS_mean_point_y_error(374, 2.29, 0.23, "blue", 1)

MS_mean_point_y_error(0, 50.39, 0.45, "red", 1)
MS_mean_point_y_error(36, 43.67, 0.60, "red", 1)
MS_mean_point_y_error(66, 36.56, 0.87, "red", 1)
MS_mean_point_y_error(99, 32.78, 0.67, "red", 1)
MS_mean_point_y_error(125, 30.58, 0.26, "red", 1)
MS_mean_point_y_error(157, 27.21, 0.76, "red", 1)
MS_mean_point_y_error(189, 23.99, 0.79, "red", 1)
MS_mean_point_y_error(221, 20.7, 0.50, "red", 1)
MS_mean_point_y_error(251, 17.85, 0.45, "red", 1)
MS_mean_point_y_error(281, 11.95, 0.39, "red", 1)
MS_mean_point_y_error(311, 9.43, 0.55, "red", 1)
MS_mean_point_y_error(340, 6.65, 0.28, "red", 1)
MS_mean_point_y_error(374, 4.82, 0.30, "red", 1)













#### DEKOMPOSITIONSRATEN EXPONENTIELL differences Clay and Loam ####

shapiro.test((kDATA_C$k))
#ok
shapiro.test((kDATA_L$k))
#ok
var.test(kDATA_C$k, kDATA_L$k)
#ok

t.test(kDATA_C$k, kDATA_L$k, var.equal=TRUE)
#p-value = 0.0005296





### checking if Wilcoxon Mann Whitney Test would also work)
### x<-kDATA$k
### x
### y<-kDATA$CorL
### y

### wilcox.test(x~y)
### works :-)






#DEKOMPOSITIONSRATEN Linear

shapiro.test((kDATA_C$k_linear))
#ok
shapiro.test((kDATA_L$k_linear))
#ok
var.test(kDATA_C$k_linear, kDATA_L$k_linear)
#ok

t.test(kDATA_C$k_linear, kDATA_L$klinear, var.equal=TRUE)
#p-value = 4.548e-11



#### GRAFIK FÜR DIE ZWEI DEKOMPOSITIONSRATEN LINEAR Clay and Loam ####
##für C:
curve(-0.122270875 * x + 45.60693775, 0, 374, 
col = "blue", xlab = "days", ylab = "remaining dry mass (g)",
mtext ="clay 1"
, add = TRUE
)


##für L:
curve(-0.118069 * x + 46.60242938, 0, 374, col = "red", add = TRUE)




#### Halfway betw. lin and exp? ####


curve(0.5*(-0.122270875 * x + 45.60693775)+
0.5*(50 * 2.718^(-0.005122438 * x))
, 0, 374, 
col = "blue", xlab = "days", ylab = "remaining dry mass (g)", add = TRUE)

curve(0.5*(-0.118069 * x + 46.60242938)+
0.5*(50 * 2.718^(-0.004572038 * x))
, 0, 374, 
col = "red", xlab = "days", ylab = "remaining dry mass (g)", add = TRUE)

day<-c(0,36,66,99,125,157,189,221,251,281,311,340,374)
mclay<-c(50.1875,42.92375,35.19625,31.84375,29.125,25.00125,20.53875,18.03625,14.47375,10.77,7.7325,4.71,2.7875)
mloam<-c(50.385,43.6725,36.55625,32.77875,30.57625,27.205,23.98625,20.7,17.84625,11.94625,9.4325,6.65125,4.82375)
day
mclay
mloam

points(mclay ~ day, col="blue")
points(mloam ~ day, col="red")

#...lines



mclay_pred_lin<- -0.122270875 * day + 45.60693775
mclay_pred_exp<- 50 * 2.718^(-0.005122438 * day)
mclay_pred_comb<- 0.5*(-0.122270875 * day + 45.60693775)+ 0.5*(50 * 2.718^(-0.005122438 * day))


mloam_pred_lin<- -0.118069 * day + 46.60242938
mloam_pred_exp<- 50 * 2.718^(-0.004572038 * day)
mloam_pred_comb<- 0.5*(-0.118069 * day + 46.60242938)+ 0.5*(50 * 2.718^(-0.004572038 * day))

points(mclay_pred_comb ~ day, col="blue")
points(mloam_pred_comb ~ day, col="red")

cor.test(mclay, mclay_pred_lin, method = "pearson")
#p-value = 7.324e-11
cor.test(mclay, mclay_pred_exp, method = "pearson")
#p-value = 1.203e-10
cor.test(mclay, mclay_pred_comb, method = "pearson")
#p-value = 5.488e-15

cor.test(mloam, mloam_pred_lin, method = "pearson")
#p-value = 2.32e-11
cor.test(mloam, mloam_pred_exp, method = "pearson")
#p-value = 4.459e-10
cor.test(mloam, mloam_pred_comb, method = "pearson")
#p-value = 4.072e-13




#### #NUTRIENT CHANGE DATA Clay and Loam ####


D2=read.table("data/Nut_change_release_data.txt",h=T)
D2

D2_C <- subset(D2, CorL=="C")
D2_C

D2_L <- subset(D2, CorL=="L")
D2_L




#N change

shapiro.test((D2_C$N_change))
#ok
shapiro.test((D2_L$N_change))
#ok
var.test(D2_C$N_change, D2_L$N_change)
#ok

t.test(D2_C$N_change, D2_L$N_change, var.equal=TRUE)
#p-value = 0.7544



#P change

shapiro.test((D2_C$P_change))
#ok
shapiro.test((sqrt(D2_L$P_change)))
#not transformable
var.test(D2_C$N_change, D2_L$P_change)
#ok

#not normal, transformation not tested

x<-D2$P_change
x
y<-D2$CorL
y
wilcox.test(x~y)
#p-value = 0.9591



#K change

shapiro.test((D2_C$K_change))
#ok
shapiro.test(sqrt(D2_L$K_change))
#ok
var.test(D2_C$K_change, sqrt(D2_L$K_change))
#not normal

x<-D2$K_change
x
y<-D2$CorL
y
wilcox.test(x~y)
#p-value = 0.01041



#Ca change

shapiro.test((D2_C$Ca_change))
#ok
shapiro.test((D2_L$Ca_change))
#ok
var.test(D2_C$Ca_change, D2_L$Ca_change)
#ok


t.test(D2_C$Ca_change, D2_L$Ca_change, var.equal=TRUE)
#p-value = 0.1744




#Mg change

shapiro.test((D2_C$Mg_change))
#ok
shapiro.test((D2_L$Mg_change))
#ok
var.test(D2_C$Mg_change, D2_L$Mg_change)
#ok

t.test(D2_C$Mg_change, D2_L$Mg_change, var.equal=TRUE)
#p-value = 1.459e-05



# Al change

shapiro.test((D2_C$Al_change))
#not ok
shapiro.test((D2_L$Al_change))
#ok
var.test(D2_C$Al_change, D2_L$Al_change)
#not ok

x<-D2$Al_change
x
y<-D2$CorL
y
wilcox.test(x~y)
#p-value 0.72



# Fe change

shapiro.test((D2_C$Fe_change))
#not ok
shapiro.test((D2_L$Fe_change))
#ok
var.test(D2_C$Fe_change, D2_L$Fe_change)
#ok

x<-D2$Fe_change
x
y<-D2$CorL
y
wilcox.test(x~y)
#p-value 0.51



# Mn Change

shapiro.test((D2_C$Mn_change))
#ok
shapiro.test((D2_L$Mn_change))
#ok
var.test(D2_C$Mn_change, D2_L$Mn_change)
#not ok

x<-D2$Mn_change
x
y<-D2$CorL
y
wilcox.test(x~y)
#p-value 0.0006216




# Na Change

shapiro.test((D2_C$Na_change))
#not ok
shapiro.test((D2_L$Na_change))
#ok
var.test(D2_C$Na_change, D2_L$Na_change)
#not ok

x<-D2$Na_change
x
y<-D2$CorL
y
wilcox.test(x~y)
#p-value 0.44




# S Change

shapiro.test((D2_C$S_change))
#ok
shapiro.test((D2_L$S_change))
#ok
var.test(D2_C$S_change, D2_L$S_change)
#ok

t.test(D2_C$S_change, D2_L$S_change, var.equal=TRUE)
#p-value = 0.67



## change and release have the same pattern since they are only area-calculated

























##### APPENDIX####
#'''''''''



# When to use:
#   Use ANOVA  to see if a parameter (e.g. soil nutrient) differs by treatment (e.g. tree species). 
# 
# How to use:
#   model1=aov(Nitrogen  ~ treatment, data=DATA)
# library("multcomp") # for following TukeyHSD function:
# TukeyHSD(model1) # Post hoc analysis
# 
# Assumptions and testing: 
#   Normality: Shapiro-Wilk test of model residuals (package stats):
#   shapiro.test(DATA$Nitrogen) 
# Homogeneity of variances: Levene's test (package car; may need package lme4 as well) 
# leveneTest(DATA$Nitrogen~DATA$treamtment)
# Both p-values of shapiro- and levene-tests need to be > 0.05 for assumption to be met.
# 
# If assumptions are not met, you can try to transform your data:
#   e.g. try shapiro.test(log(DATA$Nitrogen)) and shapiro.test(sqrt(DATA$Nitrogen))
# as well as leveneTest(log(DATA$Nitrogen)~DATA$treamtment) and leveneTest(sqrt(DATA$Nitrogen)~DATA$treamtment)
# 
# If assumptions cannot be met, even after transformation, use a non-parametric test:
#   Kruskal-Wallis H test 
# kruskal.test(Nitrogen~treatment, data=DATA
#              # p-value = 0.008936 ## meaning there is a significant difference, but doesn't say between which treatments, therefore, use following Post hoc test:
#              kruskalmc(DATA $Nitrogen~DATA $treatment, probs=0.05, cont=NULL) # Post hoc analysis
             
             
             
             
             
             
             
             
             
