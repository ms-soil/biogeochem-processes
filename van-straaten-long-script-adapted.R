
rm(list=ls(all=TRUE))

# ------ Opening libraries
library(RODBC)
#      library(stats)
#      library(Rcmdr)

# ------ Set working directory

#      setwd("P:\\1_REDD-ALERT\\6_Pan-tropic analysis\\Statistics\\2_Controls regulating SOC change\\Time after conversion\\By_Country -decay functions")
#      setwd("F:\\5_REDD-ALERT\\Workspace\\Statistics\\2_Controls regulating SOC change\\Time after conversion\\LU - Exp decay fn")

# ------ Accessing data from database
Data = read.csv("data/van-straaten-soc.csv", header = T, sep = ",")
Data<- subset(Data,Plot_no2!="P_39")
Data<- subset(Data,Plot_no2!="P_40")
(names(Data))

LU<- subset(Data,Rubber_1==1)
attach(LU)
(names(LU))
Indo<-subset(LU,Country=="Indonesia")
Cam<-subset(LU,Country=="Cameroon")
Peru<-subset(LU,Country=="Peru")

# Set graphical parameters and create a pdf file
pdf(file="Outputs\\Exponential decay - TSDefor - RU.pdf",paper="a4")
par(oma=c(0,1,0,0),mfrow=c(2,2),las=1, bg = "NA" ,mar=c(4,3,2,1), mgp=c(2,0.5,0),adj=0.5,pch=19,lwd=1)
maxi<-1.5
mini<-0
#Y positions 
y_pos1<-1.45
y_pos2<-1.35
y_pos3<-1.25
#X positions
x_pos1_factor<-0.05
x_pos2_factor<-0.07
x_pos3_factor<-0.7
x_pos4_factor<-0.85  

# ------ 0-10_C
# Plot data and prediction

#      maxi<-max(LU$SOC_prop_top100_C,na.rm=TRUE) # to set the upper limit
#      mini<- min(LU$SOC_prop_top100_C,na.rm=TRUE)-((max(LU$SOC_prop_top100_C,na.rm=TRUE)-min(LU$SOC_prop_top100_C,na.rm=T))*0.15)  # to set the lower limit

plot(SOC_prop_top10_C~TSDefor, main="Rubber: 0-10cm",ylim=c(mini,maxi),ylab="Proportional change",xlab="Years since deforestation",cex=0.1,col="white",
     cex.axis=1,cex.lab=1)
points(Indo$Age_no,Indo$SOC_prop_top10_C,pch=1)
points(Cam$Age_no,Cam$SOC_prop_top10_C,pch=22,bg="grey")
points(Peru$Age_no,Peru$SOC_prop_top10_C,pch=17,col="black")

abline(h=1,lwd=1)
points(0,1, pch=23, bg="black",cex=1.5)

# Calculate exponential fit parameters
Model_0to10=nls(SOC_prop_top10_C~a+(b-a)*exp(-k*TSDefor),start=list(a=0.7, b=1,k=0.15))
summary(Model_0to10)
RSS=sum(resid(Model_0to10)^2)                                                   # Calculate RSS, residual sum of squares
AdjSS=with(LU, sum((SOC_prop_top10_C-mean(SOC_prop_top10_C))^2))          # Calculate AdjSS, adjusted sum squares
(R2=1-(RSS/AdjSS))                                                              # Calculate R2
# Plot model on graph      
av=seq(0,100,0.1)
bv=predict(Model_0to10,list(TSDefor=av))
lines(av,bv)

#LABELS
#X positions            
x_pos1<-(((max(TSDefor,na.rm=TRUE)-min(TSDefor,na.rm=T))*x_pos1_factor)+min(TSDefor,na.rm=TRUE))
x_pos2<-(((max(TSDefor,na.rm=TRUE)-min(TSDefor,na.rm=T))*x_pos2_factor)+min(TSDefor,na.rm=TRUE))
x_pos3<-(((max(TSDefor,na.rm=TRUE)-min(TSDefor,na.rm=T))*x_pos3_factor)+min(TSDefor,na.rm=TRUE))
x_pos4<-(((max(TSDefor,na.rm=TRUE)-min(TSDefor,na.rm=T))*x_pos4_factor)+min(TSDefor,na.rm=TRUE))             

text(x_pos3,y_pos2,"R2 = ")
text(x_pos4,y_pos2,round(R2,4))      

#Legend
text(x_pos2,y_pos3,"Indonesia",pos=4)
points(x_pos1,y_pos3,pch=1)
text(x_pos2,y_pos2,"Cameroon",pos=4)
points(x_pos1,y_pos2,pch=22,bg="grey")
text(x_pos2,y_pos1,"Peru",pos=4)
points(x_pos1,y_pos1,pch=17,col="black")


# ------ 0-30_C
# Plot data and prediction
#      maxi<-max(LU$SOC_prop_top100_C,na.rm=TRUE) # to set the upper limit
#      mini<- min(LU$SOC_prop_top100_C,na.rm=TRUE)-((max(LU$SOC_prop_top100_C,na.rm=TRUE)-min(LU$SOC_prop_top100_C,na.rm=T))*0.15)  # to set the lower limit

plot(SOC_prop_top30_C~TSDefor, main="Rubber: 0-30cm",ylim=c(mini,maxi),ylab="Proportional change",xlab="Years since deforestation",cex=0.1,col="white",
     cex.axis=1,cex.lab=1)
points(Indo$Age_no,Indo$SOC_prop_top30_C,pch=1)
points(Cam$Age_no,Cam$SOC_prop_top30_C,pch=22,bg="grey")
points(Peru$Age_no,Peru$SOC_prop_top30_C,pch=17,col="black")

abline(h=1,lwd=1)
points(0,1, pch=23, bg="black",cex=1.5)

# Calculate exponential fit parameters
#      Model_0to30=nls(SOC_prop_top30_C~a+(b-a)*exp(-k*TSDefor),start=list(a=0.7, b=1,k=0.15))
#      summary(Model_0to30)
#      RSS=sum(resid(Model_0to30)^2)                                                   # Calculate RSS, residual sum of squares
#      AdjSS=with(LU, sum((SOC_prop_top30_C-mean(SOC_prop_top30_C))^2))          	# Calculate AdjSS, adjusted sum squares
#      (R2=1-(RSS/AdjSS))                                                              # Calculate R2
# Plot model on graph      
#      av=seq(0,100,0.1)
#      bv=predict(Model_0to30,list(TSDefor=av))
#      lines(av,bv)

#LABELS
#X positions      
x_pos1<-(((max(TSDefor,na.rm=TRUE)-min(TSDefor,na.rm=T))*x_pos1_factor)+min(TSDefor,na.rm=TRUE))
x_pos2<-(((max(TSDefor,na.rm=TRUE)-min(TSDefor,na.rm=T))*x_pos2_factor)+min(TSDefor,na.rm=TRUE))
x_pos3<-(((max(TSDefor,na.rm=TRUE)-min(TSDefor,na.rm=T))*x_pos3_factor)+min(TSDefor,na.rm=TRUE))
x_pos4<-(((max(TSDefor,na.rm=TRUE)-min(TSDefor,na.rm=T))*x_pos4_factor)+min(TSDefor,na.rm=TRUE))             


text(x_pos3,y_pos2,"R2 = ")
#      text(x_pos4,y_pos2,round(R2,4))      

#Legend
text(x_pos2,y_pos3,"Indonesia",pos=4)
points(x_pos1,y_pos3,pch=1)
text(x_pos2,y_pos2,"Cameroon",pos=4)
points(x_pos1,y_pos2,pch=22,bg="grey")
text(x_pos2,y_pos1,"Peru",pos=4)
points(x_pos1,y_pos1,pch=17,col="black")


# ------ 0-50_C
# Plot data and prediction
#      maxi<-max(LU$SOC_prop_top100_C,na.rm=TRUE) # to set the upper limit
#      mini<- min(LU$SOC_prop_top100_C,na.rm=TRUE)-((max(LU$SOC_prop_top100_C,na.rm=TRUE)-min(LU$SOC_prop_top100_C,na.rm=T))*0.15)  # to set the lower limit

plot(SOC_prop_top50_C~TSDefor, main="Rubber: 0-50cm",ylim=c(mini,maxi),ylab="Proportional change",xlab="Years since deforestation",cex=0.1,col="white",
     cex.axis=1,cex.lab=1)
points(Indo$Age_no,Indo$SOC_prop_top50_C,pch=1)
points(Cam$Age_no,Cam$SOC_prop_top50_C,pch=22,bg="grey")
points(Peru$Age_no,Peru$SOC_prop_top50_C,pch=17,col="black")

abline(h=1,lwd=1)
points(0,1, pch=23, bg="black",cex=1.5)

# Calculate exponential fit parameters
#      Model_0to50=nls(SOC_prop_top50_C~a+(b-a)*exp(-k*TSDefor),start=list(a=0.7, b=1,k=0.15))
#      summary(Model_0to50)
#      RSS=sum(resid(Model_0to50)^2)                                                   # Calculate RSS, residual sum of squares
#      AdjSS=with(LU, sum((SOC_prop_top50_C-mean(SOC_prop_top50_C))^2))          	# Calculate AdjSS, adjusted sum squares
#      (R2=1-(RSS/AdjSS))                                                              # Calculate R2
# Plot model on graph      
#      av=seq(0,100,0.1)
#      bv=predict(Model_0to50,list(TSDefor=av))
#      lines(av,bv)

#LABELS
#X positions
x_pos1<-(((max(TSDefor,na.rm=TRUE)-min(TSDefor,na.rm=T))*x_pos1_factor)+min(TSDefor,na.rm=TRUE))
x_pos2<-(((max(TSDefor,na.rm=TRUE)-min(TSDefor,na.rm=T))*x_pos2_factor)+min(TSDefor,na.rm=TRUE))
x_pos3<-(((max(TSDefor,na.rm=TRUE)-min(TSDefor,na.rm=T))*x_pos3_factor)+min(TSDefor,na.rm=TRUE))
x_pos4<-(((max(TSDefor,na.rm=TRUE)-min(TSDefor,na.rm=T))*x_pos4_factor)+min(TSDefor,na.rm=TRUE))             


text(x_pos3,y_pos2,"R2 = ")
#      text(x_pos4,y_pos2,round(R2,4))      

#Legend
text(x_pos2,y_pos3,"Indonesia",pos=4)
points(x_pos1,y_pos3,pch=1)
text(x_pos2,y_pos2,"Cameroon",pos=4)
points(x_pos1,y_pos2,pch=22,bg="grey")
text(x_pos2,y_pos1,"Peru",pos=4)
points(x_pos1,y_pos1,pch=17,col="black")

# ------ 0-100_C
# Plot data and prediction

#      maxi<-max(LU$SOC_prop_top100_C,na.rm=TRUE) # to set the upper limit
#      mini<- min(LU$SOC_prop_top100_C,na.rm=TRUE)-((max(LU$SOC_prop_top100_C,na.rm=TRUE)-min(LU$SOC_prop_top100_C,na.rm=T))*0.15)  # to set the lower limit

plot(SOC_prop_top100_C~TSDefor, main="Rubber: 0-100cm",ylim=c(mini,maxi),ylab="Proportional change",xlab="Years since deforestation",cex=0.1,col="white",
     cex.axis=1,cex.lab=1)
points(Indo$Age_no,Indo$SOC_prop_top100_C,pch=1)
points(Cam$Age_no,Cam$SOC_prop_top100_C,pch=22,bg="grey")
points(Peru$Age_no,Peru$SOC_prop_top100_C,pch=17,col="black")

abline(h=1,lwd=1)
points(0,1, pch=23, bg="black",cex=1.5)

# Calculate exponential fit parameters
#      Model_0to100=nls(SOC_prop_top100_C~a+(b-a)*exp(-k*TSDefor),start=list(a=0.7, b=1,k=0.16))
#      summary(Model_0to100)
#      RSS=sum(resid(Model_0to100)^2)                                                   # Calculate RSS, residual sum of squares
#      AdjSS=with(LU, sum((SOC_prop_top100_C-mean(SOC_prop_top100_C))^2))          	# Calculate AdjSS, adjusted sum squares
#      (R2=1-(RSS/AdjSS))                                                              # Calculate R2
#      # Plot model on graph      
#      av=seq(0,100,0.1)
#      bv=predict(Model_0to100,list(TSDefor=av))
#      lines(av,bv)

#LABELS
#X positions         
x_pos1<-(((max(TSDefor,na.rm=TRUE)-min(TSDefor,na.rm=T))*x_pos1_factor)+min(TSDefor,na.rm=TRUE))
x_pos2<-(((max(TSDefor,na.rm=TRUE)-min(TSDefor,na.rm=T))*x_pos2_factor)+min(TSDefor,na.rm=TRUE))
x_pos3<-(((max(TSDefor,na.rm=TRUE)-min(TSDefor,na.rm=T))*x_pos3_factor)+min(TSDefor,na.rm=TRUE))
x_pos4<-(((max(TSDefor,na.rm=TRUE)-min(TSDefor,na.rm=T))*x_pos4_factor)+min(TSDefor,na.rm=TRUE))             


text(x_pos3,y_pos2,"R2 = ")
#      text(x_pos4,y_pos2,round(R2,4))      

#Legend
text(x_pos2,y_pos3,"Indonesia",pos=4)
points(x_pos1,y_pos3,pch=1)
text(x_pos2,y_pos2,"Cameroon",pos=4)
points(x_pos1,y_pos2,pch=22,bg="grey")
text(x_pos2,y_pos1,"Peru",pos=4)
points(x_pos1,y_pos1,pch=17,col="black")


dev.off()    # turn off pdf
shell.exec(file.path(getwd(), paste("Outputs\\Exponential decay - TSDefor - RU.pdf")))