## Exercice 1 
n<-100

X <- rep
X_g <- rnorm(n,0,1)
X_u <- runif(n,min=0,max=1)
X_exp <- rexp(n,rate=1)

hist(X_g)
X<-seq(-3,4,100)
curve(dnorm(X,0,1),from=-3,to=4,add=TRUE)
hist(X_u)

hist(X_exp)

qqmath(X_g,distribution=qnorm)
qqmath(X_u,distribution=qunif)
qqmath(X_exp,distribution=qexp)

graphics.off()
n_a=20
F_g <- rnorm(n=n1)
plot(sort(F_g),1:n1/n1,type="s",ylim=c(0,1),xlim=c(-2,2))
par(new=TRUE)
plot(pnorm,xlab="",ylab="",ylim=c(0,1),xlim=c(-2,2))

n_b=1000
F_g2<-rnorm(n=n2)
par(new=TRUE)
plot(sort(F_g2),1:n2/n2,type="s",ylim=c(0,1),xlim=c(-2,2))


x_lim=c(-3,3)
y_lim=c(0,1)
par(new=FALSE)
plot(dnorm,xlim=x_lim,ylim=y_lim,col='red')
par(new=TRUE)
plot(density(rnorm(n=3)),xlim=x_lim,ylim=y_lim,col='blue')
par(new=TRUE)
plot(density(rnorm(n=20)),xlim=x_lim,ylim=y_lim,col='blue')
par(new=TRUE)
plot(density(rnorm(n=5000)),xlim=x_lim,ylim=y_lim,col='blue')


?ks.test
ks.test(X_g,'pnorm')
ks.test(F_g,'pnorm')
ks.test(F_g2,'pnorm')


exp<-1
cpt<-1
n_rep<-70
while(exp<n_rep){
  if(ks.test(X_g,'pnorm')$p.value < 0.05){
    cpt<-cpt+1
  }
  exp<-exp+1
}
cat(cpt/n_rep * 100 , "%")

