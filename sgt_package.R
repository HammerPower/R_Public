library(sgt)



for( i in seq(from=-0.90,to=0.90,by = 0.1)){
x=rsgt(n=10000,mu = 0,sigma = 1,lambda = i,p = 5,q = Inf,mean.cent = FALSE,var.adj = FALSE)
hist(x)
}

sgt.mle(X=~RL(EUROSTOXX),start=start,method = "nlminb")
sgt.mle(X=~RL(EUROSTOXX),start=start,method = "Nelder-Mead")
sgt.mle(X=~RL(EUROSTOXX),start=start,method = "BFGS")
sgt.mle(X=~RL(EUROSTOXX),start=start,method = "CG")
sgt.mle(X=~RL(EUROSTOXX),start=start,method = "L-BFGS-B")
sgt.mle(X=~RL(EUROSTOXX),start=start,method = "nlm")
sgt.mle(X=~RL(EUROSTOXX),start=start,method = "spg")
sgt.mle(X=~RL(EUROSTOXX),start=start,method = "ucminf")
sgt.mle(X=~RL(EUROSTOXX),start=start,method = "newuoa")
sgt.mle(X=~RL(EUROSTOXX),start=start,method = "bobyqa")
sgt.mle(X=~RL(EUROSTOXX),start=start,method = "nmkb")
sgt.mle(X=~RL(EUROSTOXX),start=start,method = "hjkb")
sgt.mle(X=~RL(EUROSTOXX),start=start,method = "Rcgmin")
sgt.mle(X=~RL(EUROSTOXX),start=start,method = "Rvmmin")

set.seed(7900)
n = 1000
x = rsgt(n, mu = 2, sigma = 2, lambda = -0.25, p = 1.7, q = 7)
start = list(mu = 0, sigma = 1, lambda = 0, p = 2, q = 10)
result = sgt.mle(X.f = ~ x, start = start, method = "nlminb")
result$estimate[1]
print(result)
print(summary(result))



sgt.mle(X=~RL(EUROSTOXX),start=start,method = "nlminb")
sgt.mle(X=~RL(DAX),start=start,method = "nlminb")
sgt.mle(X=~RL(FTSE),start=start,method = "nlminb")
sgt.mle(X=~RL(FTSEMIB),start=start,method = "nlminb")
sgt.mle(X=~RL(SMI),start=start,method = "nlminb")
sgt.mle(X=~RL(IBEX),start=start,method = "nlminb")
sgt.mle(X=~RL(CAC),start=start,method = "nlminb")

hist(RL(EUROSTOXX))
hist(RL(DAX))
hist(RL(FTSE))
hist(RL(FTSEMIB))
hist(RL(SMI))
hist(RL(IBEX))
hist(RL(CAC))
mu=0.0005
sigma=0.008
lambda=0.05
p=1.5
q=5

x=rsgt(n=250,mu = mu,sigma = sigma,lambda = lambda,p = p,q = q,mean.cent = FALSE,var.adj = FALSE)
hist(x)
y=x
cor(x,y)
SignConc(x,y,0,0)
hist(x)
hist(dsgt(x,mu = mu,sigma = sigma,lambda = lambda,p = p,q = q,mean.cent = FALSE,var.adj = FALSE))
hist(psgt(x,mu = mu,sigma = sigma,lambda = lambda,p = p,q = q,mean.cent = FALSE,var.adj = FALSE))
hist(qsgt(runif(250,0,1),mu = mu,sigma = sigma,lambda = lambda,p = p,q = q,mean.cent = FALSE,var.adj = FALSE))

hist(psgt(runif(250,0,1),mu = mu,sigma = sigma,lambda = lambda,p = p,q = q,mean.cent = FALSE,var.adj = FALSE))
hist(qsgt(x,mu = mu,sigma = sigma,lambda = lambda,p = p,q = q,mean.cent = FALSE,var.adj = FALSE))
