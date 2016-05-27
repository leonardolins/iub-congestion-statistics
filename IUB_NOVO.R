#setwd("/Users/edilsonsuame/R_wd")
setwd("C:/R")
library("ggplot2")
#install.packages("e1071")
library("e1071")
iubce02 <- read.table("BA03.csv", header=TRUE, sep=";")# 10:00 as 20:00 hs 
iubce02[complete.cases(iubce02),]
#iubce02[complete.cases(iubce02),]
sites<-unique(iubce02$SITE)
iubdata<-data.frame(SITE=character(),DELTA=double(),SK=double(),CURT=double(),PERDA=double(),FILTRO=double(),FATOR=integer(),stringsAsFactors = F)
for (i in sites){
  s <- iubce02[ which(iubce02$SITE == i), ]
  #q1<-quantile(s$LOAD,1/4)
  #q2<-quantile(s$LOAD,2/4)
  #q3<-quantile(s$LOAD,3/4)
  w1<-boxplot(s$LOAD)$stats[c(1),]
  w2<-boxplot(s$LOAD)$stats[c(5),]
  iqr<-IQR(s$LOAD)
  sk<-skewness(s$LOAD)
  curt<-kurtosis(s$LOAD)
  loss<-mean(s$PERDA)
  delta<-q2/w2
  #delta<-(q2 - w1)/(w2 - q2)
  #delta<-q2/((1.5*iqr + q3)-q2)
  #delta<- (q3-q2)/q3
  cong<- 1 + (0.01)^2 + (4*(delta/0.85)^3 - 3*(delta/0.85))^2
  #desvio<-round(sd(s$LOAD),2)
  #media<-round(mean(s$LOAD),2)
  #iubdata[nrow(iubdata)+1,]<-c(i,delta,cong,round(cong))
  iubdata[nrow(iubdata)+1,]<-c(i,delta,sk,curt,loss,cong,round(cong))
  out<-paste(i,delta,sk,curt,loss)
  print(out)
  
  
}
#iubdata[complete.cases(iubdata),]
iubdata<-transform(iubdata, DELTA = as.double(DELTA),SK=as.double(SK),CURT=as.double(CURT),PERDA=as.double(PERDA),FILTRO=as.double(FILTRO),FATOR=as.double(FATOR))
iubdata[complete.cases(iubdata),]
qplot(DELTA, log(FILTRO), data=iubdata, colour=DELTA, size=FATOR) + geom_vline(xintercept = 0.8,colour="red",size=1.2)
#print(iubdata[which((iubdata$DELTA >0.65 & iubdata$SK> 0.3) | (iubdata$PERDA) > 0),])
print(iubdata[which(((iubdata$DELTA > 0.69 & iubdata$SK< 0 )|(iubdata$DELTA > 0.80 ) ) | (iubdata$PERDA>10)),])
#print(iubdata[which((iubdata$DELTA > 0.7 & iubdata$SK<0 ) | (iubdata$PERDA>0)),])
#print(iubdata[which(iubdata$DELTA > 2.95),])
#write.table(iubdata[which(iubdata$DELTA > 2.95),],"BA01_IUB.csv",sep = ",")
#t<-iubce02[which(iubce02$SITE == "UPEVSA08"),]
#boxplot(t$LOAD,main="UCEMVA01",ylab="kBPS",col="gold",outline=F)
#print(iubdata[which(iubdata$DELTA > 2.449),])
#plot(iubdata$DELTA,iubdata$FILTRO,type = "p")
#hist(t$LOAD,main="UCEHZT03",xlab="kBPS",col="gold")
#boxplot(t$LOAD)$stats[c(1,5),]
# pdf("box.pdf")
# for (h in sites){
# 
#   t<-iubce02[which(iubce02$SITE == h),]
#   x<-skewness(t$LOAD)
#   y<-iubdata[which(iubdata$SITE == h),2]
#   o<-as.character(paste(x,y))
# 
#   #hist(t$LOAD,main=h,ylab="kBPS",xlab = o,col="gold")
#   boxplot(t$LOAD,main=h,ylab="kBPS",xlab = o, col="gold",outline=F)
# }
# dev.off()