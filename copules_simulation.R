install.packages("VineCopula")
library(VineCopula)
library(copula)
detach(name="package:copula",unload = TRUE)
?VineCopula
?copula
rCopula

BiCop(1)
family=as.array(c(0 
                  ,1 
                  ,2 
                  ,3 
                  ,4 
                  ,5 
                  ,6 
                  ,7 
                  ,8 
                  ,9 
                  ,10 
                  ,13 
                  ,14 
                  ,16 
                  ,17 
                  ,18 
                  ,19 
                  ,20 
                  ,23 
                  ,24 
                  ,26 
                  ,27 
                  ,28 
                  ,29 
                  ,30 
                  ,33 
                  ,34 
                  ,36 
                  ,37 
                  ,38 
                  ,39 
                  ,40 
                  ,104
                  ,114
                  ,124
                  ,134
                  ,204
                  ,214
                  ,224
                  ,234))


contour(BiCop(1,par=0.5))
a=BiCop(1,par=-0.5)
a=BiCop(0)
set.seed(1)
u1=runif(250,0,1)
u2=runif(250,0,1)
plot(u1,u2)
z=BiCopCDF(u1,u2,1,par=0.5)

z=as.matrix(z)
contour(z=z)
z=as.matrix(rnorm(250))
z=cbind(z,z=as.matrix(rnorm(250)))

?BiCopCDF
?contour

BiCopMetaContour(a,size=100)

?BiCopMetaContour
BiCopTau2Par(family = family_bis[5],0.5)

family_bis=c(0 ,
             1 ,
             2 ,
             3 ,
             4 ,
             5 ,
             6 ,
             13,
             14,
             16,
             23,
             24,
             26,
             33,
             34,
             36)

for(i in 1:length(family_bis))
{
  if(family_bis[i]!=23 & family_bis[i]!=24 & family_bis[i]!=26 & family_bis[i]!=33 & family_bis[i]!=34 & family_bis[i]!=36)
    {a=BiCopTau2Par(family = family_bis[i],0.5)
    
  }else{
    a=BiCopTau2Par(family = family_bis[i],-0.5)
  }
  print(a)
  #return(a)
if(i==1){param=a}else{param=cbind(b,a)}
  }

for (i in 1:length(param)) {
 contour( BiCop(family_bis[i],par = param[i],par2=2.1))
}
?BiCopMetaContour
plot(BiCop(family_bis[3],par = param[3],par2=2.1))
contour( BiCop(family_bis[3],par = param[3],par2=2.1))
BiCopChiPlot(u1,u2,BiCop(family_bis[3],par = param[3],par2=2.1),PLOT = TRUE)
BiCopLambda(BiCop(family_bis[3],par = param[3],par2=2.1))
#do not work#BiCopKPlot(u1,u2,BiCop(family_bis[3],par = param[3],par2=2.1),PLOT=TRUE,seq.default(0,1,10))

for(i in seq(from=2.1,to=5,by=0.20)){
#plot(BiCop(family_bis[3],par = param[3],par2=i))
contour( BiCop(family_bis[3],par = param[3],par2=i))
}

for(i in seq(from=,to=5,by=0.20)){
  contour( BiCop(family_bis[3],par = param[3],par2=i))
}
plot(BiCopSim(2500,BiCop(family_bis[3],par = param[3],par2=2.1)),xlab="u1",ylab="u2")
?BiCopCondSim
