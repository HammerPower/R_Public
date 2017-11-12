SignConc<-function(x,y,a,b){
  x<-as.vector(x)
  y<-as.array(y)
  n1=0
  n2=0
  for(i in 1:length(x))
  {
    if((x[i]>a & y[i]>b) | (x[i]<=a & y[i]<=b)){
      n1=n1+1
    } else {
      n2=n2+1
    }
    mesure=((n1-n2)/(n1+n2))
  }
  return(c(n1,n2,mesure))
}
SignConc(RL(CAC),RL(EUROSTOXX),0,0)
SignConc(RL(CAC),RL(EUROSTOXX),mean(RL(CAC)),mean(RL(EUROSTOXX)))
SignConc(RL(CAC),RL(EUROSTOXX),median(RL(CAC)),median(RL(EUROSTOXX)))
SignConc(RL(CAC),RL(EUROSTOXX),sd(RL(CAC)),sd(RL(EUROSTOXX)))
SignConc(RL(CAC),RL(EUROSTOXX),var(RL(CAC)),var(RL(EUROSTOXX)))

SignConc(RL(DAX),RL(EUROSTOXX),0,0)
SignConc(RL(DAX),RL(EUROSTOXX),mean(RL(CAC)),mean(RL(EUROSTOXX)))
SignConc(RL(DAX),RL(EUROSTOXX),sd(RL(CAC)),sd(RL(EUROSTOXX)))
SignConc(RL(DAX),RL(EUROSTOXX),var(RL(CAC)),var(RL(EUROSTOXX)))


plot(RL(CAC),RL(EUROSTOXX))
cbind(RL(CAC),RL(EUROSTOXX))
cbind(RL(CAC),RL(EUROSTOXX),RL(CAC)>0 & RL(EUROSTOXX)>0)



GH1(RL(DAX),RL(EUROSTOXX),RiskfreeVetor)
GH1(RL(FTSE),RL(EUROSTOXX),RiskfreeVetor)
GH1(RL(SMI),RL(EUROSTOXX),RiskfreeVetor)
GH1(RL(FTSEMIB),RL(EUROSTOXX),RiskfreeVetor)
GH1(RL(IBEX),RL(EUROSTOXX),RiskfreeVetor)