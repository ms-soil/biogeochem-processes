#### loading libraries ####
library("multcomp")
library("car") # install.packages("car")
library("pgirmess") 
library("MASS")
library("tidyverse")

#### read in data ####
d <- read.csv("data/data-summary.csv", header = TRUE)
head(d)

#### first stats example ####
# co2flux with landuse

qplot(landuse, co2flux, data = d)

# test for equal variance
leveneTest(d$co2flux~d$landuse)
# p = 0.26 which is > 0.05

str(d$landuse)
#convert string into factor
d$landuse <- as.factor(d$landuse)

# test for normal distribution
shapiro.test(residuals(lm(d$co2flux~d$landuse)))
# p = 0.47 which is > 0.05
truehist(residuals(lm(d$co2flux~d$landuse)))

# finally, our global test
anova(aov(d$co2flux~d$landuse))
# p << 0.05
# interpretation: There are significant differnces 
# in co2 flux between our landuses 

# posthoc test (only if p for global
# test is < 0.05)

TukeyHSD(aov(d$co2flux~d$landuse))



#### showing and ordering means ####

means <- d %>% group_by(landuse) %>% summarize(m = mean(co2flux)) %>% arrange(-m)
means

#### function to test automatically: ####


MS_mg<-function(dataframe, parameter, treatment) {
  cat("\014") # clear console
  #test equality of variances
  p_equal_v<-leveneTest(dataframe[,parameter]~dataframe[,treatment])
  print("Equality of variance test returns:")
  print(p_equal_v[1,3])
  
  if(p_equal_v[1,3]<=0.05) {
    result1<-oneway.test(dataframe[,parameter]~dataframe[,treatment], data = dataframe, var.equal=FALSE)$p.value
    #summary1<-summary(oneway.test(dataframe[,parameter]~dataframe[,treatment], data = dataframe, var.equal=FALSE))
    print("The final one-way test returns:")
    print(result1)
    #print(summary1)
    print("Here are the test statistics:")
    print(oneway.test(dataframe[,parameter]~dataframe[,treatment], data = dataframe, var.equal=FALSE))
    
    if(result1<=0.05)
    {
      print("A post-hoc test is done:")
      y<-dataframe[,parameter]
      x<-dataframe[,treatment]
      pairwise.t.test(y, x, pool.sd=FALSE, paired=FALSE, p.adjust.method='holm')
    }
    else
    {
      print("no post-hoc test is done")
    }
  }
  else
  {
    print("Equal variances are assumed.")
    print("Please check if this is logical.")
    p_normality<-shapiro.test(residuals(lm(dataframe[,parameter]~dataframe[,treatment])))$p.value
    print("Normality of residuals returns:")
    print(p_normality)
    
    if(p_normality>=0.05)
    {
      model1<-aov(dataframe[,parameter]~dataframe[,treatment])
      result2<-summary(model1)[[1]][["Pr(>F)"]][[1]]
      print("The final ANOVA returns:")
      print(result2)
      print("Here are the test statistics:")
      print(summary(model1))
      if(result2<=0.05)
      {TukeyHSD(model1)}
      else
      {print("No post-hoc test is done")}
      
    }
    else
    {
      result3<-kruskal.test((dataframe[,parameter]~dataframe[,treatment]))$p.value
      print("The final Kruskal-Wallis test returns:")
      print(result3)
      print(kruskal.test((dataframe[,parameter]~dataframe[,treatment])))
      if(result3<=0.05)
      {kruskalmc(dataframe[,parameter]~dataframe[,treatment], probs=0.05, cont=NULL)}
      else
      {print("No post-hoc test is done")}
    }
    
  }
}

MS_mg(d, 'co2flux', 'landuse')
MS_mg(d, 'n2oflux', 'landuse')
MS_mg(d, 'ch4flux', 'landuse')
MS_mg(d, 'soc', 'landuse')








