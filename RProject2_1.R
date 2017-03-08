
x<- seq(-8,8,0.05)

y<-dgamma(x,20,13)
plot(x,y,type = 'l')

gam <- dgamma(x,shape = 20,rate = 13)

#Alpha - Shape parameter
#Beta - Rate parameter (Inverse of scale)

curve(dgamma(x, shape=20, rate=13),from=0, to=4, main="Gamma distribution", col='blue')


curve(dnorm(x,m=10,sd=2),from=0,to=20,main="Normal distribution")

curve(dexp(x,rate = 5.2),main="Exponential Distribution")


x<-seq(-8,6,0.01)
y<-dnorm(x,-1,1.5)
plot(x,y,type="l")
polygon(c(x[x>4],4),c(y[x>4],y[x==-4]),col="honeydew2")

c(x[x>4],4)
c(y[x>4],y[x==-4])

dnorm(x,mean = 2,sd = 1.5)

x<-seq(-8,6,0.01)
y<-dnorm(x,mean = -1.5,sd = sqrt(3))
plot(x,y,type="l",col='brown',main="Gaussian Density Function")


polygon(c(x[x<4],4),c(y[x<4],y[x==-4]),col="honeydew2")

auc(c(x[x<4],4),c(y[x<4],y[x==-4]))


cord.x <- c(-6)
cord.y <- c(0)

cord.x <- c(cord.x,-6) 
cord.y <- c(cord.y,dnorm(-6))

cord.x <- c(cord.x,-4,-4)
cord.y <- c(cord.y,dnorm(-4),0)

curve(dnorm(x,-1.5,sqrt(3)),xlim=c(-8,6),main='Normal Density')

polygon(cord.x,cord.y,col = "blue",density = 2)

x<-1

cord.x <- c(-4,seq(-4,-2,0.01),-2) 
cord.y <- c(0,dnorm(seq(-4,-2,0.01)),0) 
curve(dnorm(x,0,1.5),xlim=c(-4,6),main='Standard Normal') 
polygon(cord.x,cord.y,col='skyblue')

cord.x <- c(-6,seq(-6,-2,0.01),-2) 
cord.y <- c(0,dnorm(seq(-6,-2,0.01)),0) 
curve(dnorm(x,-1.5,sqrt(3)),xlim=c(-8,6),main='Normal Distribution') 
polygon(cord.x,cord.y,col='skyblue')


require(MESS)
auc(x,y, type = 'spline')

f<-function(x,y) {
  z<-1.38*exp(-0.6*(2*x+0.3*y))
}
y<-x<-seq(0,3,length=50)
z<-outer(x,y,f)
persp(x,y,z)
persp(x,y,z,theta=45,phi=30,expand=0.6,ltheta=120,shade=0.75,ticktype="det
      ailed",xlab="X",ylab="Y",zlab="f(x,y)",col="lightskyblue")