# Normal(0,1)
N <- 10^5
df <- 4
X<- rep(rnorm(1),N)
alpha<-rep(0,N)
for(t in 2:N){
  Y <- rnorm(1)
  alpha[t]<- min(1, (dt(Y, df)*dnorm(X[t-1]))/(dt(X[t-1],df)*dnorm(Y)))
  if(runif(1)<alpha[t]){
    X[t] <- Y
  }else{
    X[t]=X[t-1]
  }
  print(t) 
}
mean(X)
mean(alpha[-1])

par(mfrow=c(2,2))
plot(X[5000:N], type="l", ylab="", xlab="")
##histogram
hist(X, breaks=100, border= "yellow4",freq=F, main="")
lines(seq(-10,10, by=0.1), dt(seq(-10,10, by=0.1),4),type="l", col="blue")
abline(v=mean(X), col="firebrick3", lty=2)
acf(X, main="",ylab="Autocorrelation")



#####################################################################################################
#t with v=2 degrees of freedom.
N <- 10^4
df <- 4
X<- rep(rnorm(1),N)
alpha.p<-rep(0,N)
for(s in 2:N){
  y <- rt(1,2)
  alpha.p[s] <- min(1, (dt(y,df)*dt(X[s-1],2))/(dt(X[s-1],df)*dt(y,2)))
  if(runif(1)<alpha.p[s]){
    X[s] <- y
  }else{
    X[s]=X[s-1]
  }
  print(s)
}
mean(X)
mean(alpha.p[-1])

par(mfrow=c(2,2))
plot(X, type="l", ylab="", xlab="")
##histogram
hist(X[4500:N], breaks=100, border="green",freq=F, main="")
lines(seq(-5,5, by=.01), dt(seq(-5,5, by=.01),4),type="l", col="blue")
abline(v=mean(X), col="firebrick3", lty=2)
acf(X, main="",ylab="Autocorrelation")


