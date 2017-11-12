library(energy)
library(minerva)
library(psych)
library(tseries)
library(forecast)
#library(ggplot2)

#lecture des fichiers
fileaddress<-readClipboard()
listenom<-list.files(fileaddress)
address<-paste(fileaddress,"\\",listenom,sep="")
#iteration sur chaque fichier
for(i in 1:length(address))
{

a<-read.csv(address[i],header = TRUE,sep = ",",dec=",")#,quote = TRUE,stringsAsFactors = FALSE)
a<-a[-c(length(a[,1])-1,length(a[,1])),1:2]
a<-as.data.frame(a)
a[,1]<-as.Date(a[,1],format="%d/%m/%Y")
a[,2]<-as.numeric(gsub(",",".",gsub("\\.","",as.character(a[,2]))))
a<-subset(a,a[,1]>as.Date("2016-08-31")&a[,1]<as.Date("2017-09-01"))
a<-a[order(a[,1]),]
colnames(a)<-c("Date",
               gsub(" Taux","",gsub(" Données","",gsub(" historiques - Investing.com.csv","",listenom[i]))))
if(i==1) {db<-a} else {db<-merge(db,a)}
}

EUROSTOXX<-db$`Euro Stoxx 50`
CAC<-db$`CAC 40`
DAX<-db$DAX
FTSE<-db$`FTSE 100`*db$`GBP EUR`
SMI<-db$SMI*db$`CHF EUR`
IBEX<-db$`IBEX 35`
FTSEMIB<-db$`FTSE MIB Net TR`
Date<-db$Date

"RA"<-function(x)
{
  RA<-exp(diff(log(x)))-1
  return(RA)
}
"RL"<-function(x)
{
  RL<-diff(log(x))
  return(RL)
}

"g"<-function(x,y)
{
  n=length(x)
  g<-2/n**2*(sum(abs(rank(x)+rank(y)-n-1))-sum(abs(rank(x)-rank(y))))
    return(g)
}

"beta"<-function(x,y)
{
  n=length(x)
  beta<-2/n*sum((((rank(x)-1/2-n/2)*(rank(y)-1/2-n/2))>=0)*1)-1
  return(beta)
}

"rhoN"<-function(x,y)
{
  n<-length(x)
  rhoN<-cor(qnorm((rank(x)-1/2)/n),qnorm((rank(y)-1/2)/n),method = "pearson")
  return(rhoN)
}

"rdc" <- function(x,y,k=20,s=1/6,f=sin) {
  x <- cbind(apply(as.matrix(x),2,function(u)rank(u)/length(u)),1)
  y <- cbind(apply(as.matrix(y),2,function(u)rank(u)/length(u)),1)
  x <- s/ncol(x)*x%*%matrix(rnorm(ncol(x)*k),ncol(x))
  y <- s/ncol(y)*y%*%matrix(rnorm(ncol(y)*k),ncol(y))
  cancor(cbind(f(x),1),cbind(f(y),1))$cor[1]
}


"GH"<-function(p,b){
  RLbstar<-sd(RL(p))/sd(RL(b))*RL(b)
  RLpstar<-sd(RL(b))/sd(RL(p))*RL(p)
  GH1<-prod(RL(p))-prod(RLbstar)
  GH2<-prod(RLpstar)-prod(RL(b))
  return(c(GH1,GH2))
}

"mesures"<-function(x,y)
{

  n=length(x)
  RAx<-RA(x)
  RAy<-RA(y)
  RLx<-RL(x)
  RLy<-RL(y)
  
  #xadf<-adf.test(x)$p.value
  #yadf<-adf.test(SMI)$p.value
  #RAxadf<-adf.test(RAx)$p.value
  #RAyadf<-adf.test(RAy)$p.value
  #RLxadf<-adf.test(RLx)$p.value
  #RLyadf<-adf.test(RLy)$p.value
  

  cor<-cor(x,y,method="pearson")
  corRA<-cor(RAx,RAy,method="pearson")
  corRL<-cor(RLx,RLy,method="pearson")
  
  tau<-cor(x,y,method="kendall")
  tauRA<-cor(RAx,RAy,method="kendall")
  tauRL<-cor(RLx,RLy,method="kendall")
  
  rho<-cor(x,y,method="spearman")
  rhoRA<-cor(RAx,RAy,method="spearman")
  rhoRL<-cor(RLx,RLy,method="spearman")
  
  g<-g(x,y)
  gRA<-g(RAx,RAy)
  gRL<-g(RLx,RLy)
  
  beta<-beta(x,y)
  betaRA<-beta(RAx,RAy)
  betaRL<-beta(RLx,RLy)
  
  rhoN<-rhoN(x,y)
  rhoNRA<-rhoN(RAx,RAy)
  rhoNRL<-rhoN(RLx,RLy)
  
  dcor<-dcor(x,y)
  dcorRA<-dcor(RAx,RAy)
  dcorRL<-dcor(RLx,RLy)
  
  mic<-mine(x,y)$MIC
  micRA<-mine(RAx,RAy)$MIC
  micRL<-mine(RLx,RLy)$MIC
  
  rdc<-rdc(x,y)
  rdcRA<-rdc(RAx,RAy)
  rdcRL<-rdc(RLx,RLy) 
  
  sRA<-mean(RAx-RAy)/sqrt(var(RAx-RAy))
  sRL<-mean(RLx-RLy)/sqrt(var(RLx-RLy))
  
  teRA<-sqrt(var(RAx-RAy))
  teRL<-sqrt(var(RLx-RLy))
  
  gh<-GH(x,y)
  
  return(rbind(  #paste(round(xadf,3),"/",round(yadf,3))
                 #,paste(round(RAxadf,3),"/",round(RAyadf,3))
                 #,paste(round(RLxadf,3),"/",round(RLyadf,3))
                 round(cor,3)
               ,round(corRA,3)
               ,round(corRL,3)
               ,round(tau,3)
               ,round(tauRA,3)
               ,round(tauRL,3)
               ,round(rho,3)
               ,round(rhoRA,3)
               ,round(rhoRL,3)
               ,round(g,3)
               ,round(gRA,3)
               ,round(gRL,3)
               ,round(beta,3)
               ,round(betaRA,3)
               ,round(betaRL,3)
               ,round(rhoN,3)
               ,round(rhoNRA,3)
               ,round(rhoNRL,3)
               ,round(dcor,3)
               ,round(dcorRA,3)
               ,round(dcorRL,3)
               ,round(mic,3)
               ,round(micRA,3)
               ,round(micRL,3)
               ,round(rdc,3)
               ,round(rdcRA,3)
               ,round(rdcRL,3)
               ,round(sRA,4)
               ,round(sRL,4)
               ,round(teRA,4)
               ,round(teRL,4)
               #,round(gh[1],2)
               #,round(gh[2],2)
               ))
  
  
}



figurepath<-"C:\\Users\\Simon\\Desktop\\Cours M2 IFM\\Memoire\\data_script\\figures\\"


dbmod<-as.data.frame(cbind(Date,EUROSTOXX,CAC,DAX,FTSE,SMI,IBEX,FTSEMIB))
names(dbmod)<-c("Date","EUROSTOXX","CAC","DAX","FTSE_in_EUR","SMI_in_EUR","IBEX","FTSE_MIB")

#rescaling at 100
plot(Date,dbmod[,2]*100/dbmod[1,2],ylim=c(90,130),type="n",xlab="",ylab="")
for(i in 2:length(dbmod))
{
  
  lines(Date,dbmod[,i]*100/dbmod[1,i],type="l",xlab="",ylab="")
  
}
#pairs.panels(dbmod[,-1])
plot(rank(FTSE)/n,rank(SMI)/n,xlab = "",ylab="")
hist(RA(CAC),xlab = "",main="")
lines(density(RA(CAC)))

for(i in 2:length(dbmod))
{
  pdf(paste(figurepath,"TS_",names(dbmod)[i],"_2017.pdf",sep=""))
  plot(Date,dbmod[,i],type="l",xlab="",ylab="",main="")#names(dbmod)[i])
  dev.off()
}

dbreturns<-Date[2:length(Date)]
for(i in 2: length(dbmod))
{
  dbreturns<-cbind(dbreturns,RL(dbmod[,i]))
  
}
dbreturns<-as.data.frame(dbreturns)
names(dbreturns)<-c("Date","EUROSTOXX","CAC","DAX","FTSE_in_EUR","SMI_in_EUR","IBEX","FTSE_MIB")

for(i in 2:length(dbmod))
{
  pdf(paste(figurepath,"TS_RL_",names(dbmod)[i],"_2017.pdf",sep=""))
  plot(dbreturns[,1],dbreturns[,i],type="l",xlab="",ylab="",main="")
  dev.off()
}

for(i in 2:length(dbmod))
{
  pdf(paste(figurepath,"TS_RL_ACF_",names(dbmod)[i],"_2017.pdf",sep=""))
  Acf(dbreturns[,i],main="")
  dev.off()
}

for (i in 2:length(dbmod)) {
  print(round(adf.test(dbmod[,i],k=5)$p.value,2))
}

for (i in 2:length(dbreturns)) {
  print(round(adf.test(dbreturns[,i],k=5)$p.value,2))
}

mesures(CAC,EUROSTOXX)
mesures(DAX,EUROSTOXX)
mesures(FTSE,EUROSTOXX)
mesures(SMI,EUROSTOXX)
mesures(FTSEMIB,EUROSTOXX)
mesures(IBEX,EUROSTOXX)

pairs.panels(dbreturns[,-1],hist.col = "darkgreen",cor=FALSE)
edit(pairs.panels)
Modif.pairs.panels(dbmod[,-1],hist.col = "white",cor=FALSE,ellipses = FALSE,density = FALSE)
Modif.pairs.panels(dbreturns[,-1],hist.col = "white",cor=FALSE,ellipses = FALSE,density = FALSE)

x=1:100
mine(x,sin(x**2))$MIC
mine(x,-1*x)$MIC

RiskFree<-read.csv("C:\\Users\\Simon\\Desktop\\Cours M2 IFM\\Memoire\\data_script\\data\\Used\\Germany 3-Month Historical Prices - Investing.com.csv",header = TRUE,sep = ";",dec=".")
RiskFree<-RiskFree[,1:2]


#RiskFree<-RiskFree[-c(length(RiskFree[,1])-1,length(RiskFree[,1])),1:2]
RiskFree[,1]<-as.Date(as.character(RiskFree[,1]),format = "%d/%m/%Y")
RiskFree[,2]<-as.numeric(as.character(RiskFree[,2]))
#RiskFree[,1]<-as.Date(RiskFree[,1],format="%b %d, %Y")
#RiskFree[,2]<-as.numeric(gsub(",",".",gsub("\\.","",as.character(a[,2]))))
#RiskFree<-subset(a,a[,1]>as.Date("2016-08-31")&a[,1]<as.Date("2017-09-01"))

names(RiskFree)[2]<-"RiskFreeRate"
RiskFree[,2]<-(1+RiskFree[,2])**(4/365)-1
RiskfreeVetor<-dbreturnsmod[length(dbreturnsmod)]
RiskfreeVetor<-(1+RiskfreeVetor/100)**(4/365)-1


"GH1"<-function(P,B,Rf)
{
  a=var(B)+var(Rf)-2*mean(B*Rf)+2*mean(B)*mean(Rf)
  b=-2*var(Rf)+2*mean(B*Rf)-2*mean(B)*mean(Rf)
  c=-1*var(P)+var(Rf)
  
  alpha1=(-b-sqrt(b**2-4*a*c))/(2*a)
  alpha2=(-b+sqrt(b**2-4*a*c))/(2*a)
  alpha1<-as.numeric(alpha1)
  alpha2<-as.numeric(alpha2)
  #print(var(B))
  #print(var(P))
  #print(alpha1)
  #print(alpha2)
  #print(var(alpha1*B+(1-alpha1)*Rf))
  #print(var(alpha2*B+(1-alpha2)*Rf))
  return(c(round(mean(P)-mean(alpha2*B+(1-alpha2)*Rf),4),round(alpha2,2)))
  
  
}
GH1(RL(CAC),RL(EUROSTOXX),RiskfreeVetor)
GH1(RL(DAX),RL(EUROSTOXX),RiskfreeVetor)
GH1(RL(FTSE),RL(EUROSTOXX),RiskfreeVetor)
GH1(RL(SMI),RL(EUROSTOXX),RiskfreeVetor)
GH1(RL(FTSEMIB),RL(EUROSTOXX),RiskfreeVetor)
GH1(RL(IBEX),RL(EUROSTOXX),RiskfreeVetor)



"GH2"<-function(P,B,Rf)
{
  a=var(P)+var(Rf)-2*mean(P*Rf)+2*mean(P)*mean(Rf)
  b=-2*var(Rf)+2*mean(P*Rf)-2*mean(P)*mean(Rf)
  c=-1*var(B)+var(Rf)
  
  alpha1=(-b-sqrt(b**2-4*a*c))/(2*a)
  alpha2=(-b+sqrt(b**2-4*a*c))/(2*a)
  alpha1<-as.numeric(alpha1)
  alpha2<-as.numeric(alpha2)
  #print(var(B))
  #print(var(P))
  #print(alpha1)
  #print(alpha2)
  #print(var(alpha1*P+(1-alpha1)*Rf))
  #print(var(alpha2*P+(1-alpha2)*Rf))
  return(c(round(-mean(B)+mean(alpha2*P+(1-alpha2)*Rf),4),round(alpha2,2)))
  
  
}

GH2(RL(CAC),RL(EUROSTOXX),RiskfreeVetor)
GH2(RL(DAX),RL(EUROSTOXX),RiskfreeVetor)
GH2(RL(FTSE),RL(EUROSTOXX),RiskfreeVetor)
GH2(RL(SMI),RL(EUROSTOXX),RiskfreeVetor)
GH2(RL(FTSEMIB),RL(EUROSTOXX),RiskfreeVetor)
GH2(RL(IBEX),RL(EUROSTOXX),RiskfreeVetor)

"Sharpe"<-function(x,f)
{
  a<-mean(x-f)/sd(x)
  return(round(a,3))
}
Sharpe(RA(CAC),RiskfreeVetor)
Sharpe(RA(DAX),RiskfreeVetor)
Sharpe(RA(FTSE),RiskfreeVetor)
Sharpe(RA(SMI),RiskfreeVetor)
Sharpe(RA(FTSEMIB),RiskfreeVetor)
Sharpe(RA(IBEX),RiskfreeVetor)

Sharpe(RL(CAC),RiskfreeVetor)
Sharpe(RL(DAX),RiskfreeVetor)
Sharpe(RL(FTSE),RiskfreeVetor)
Sharpe(RL(SMI),RiskfreeVetor)
Sharpe(RL(FTSEMIB),RiskfreeVetor)
Sharpe(RL(IBEX),RiskfreeVetor)