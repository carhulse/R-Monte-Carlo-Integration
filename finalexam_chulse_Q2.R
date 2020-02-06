##STAT 462 Final Exam##
#Question 2

#Consider the following estimator (1)...

#(a)
#Calculate the integrals a(0) and a(4) using Monte Carlo integration based on a Cauchy
#distribution.

#First we need to define a(x) in r
#Let's break it into 2 parts, f1 as the numerator and f2 as the denominator:
set.seed(0)
f1=function(theta){ theta/(1+theta*theta)*exp(-(x-theta)^2/2)}
f2=function(theta){ 1/(1+theta*theta)*exp(-(x-theta)^2/2)}

#Cauchy distribution has no mean! Thus, if you take a
#sample x1, x2, . . . , xn from the Cauchy distribution,
#then the average x does not tend to a particular number.


#Hence f1 and f2 are both expectations under the Cauchy distribution.
#They can therefore be approximated directly by
numit=10^4
cauch=rcauchy(numit)
I=mean(cauch*dnorm(cauch,mean=x))/mean(dnorm(cauch,mean=x))

#a(0)
x=0
est_0=mean(cauch*dnorm(cauch,mean=x))/mean(dnorm(cauch,mean=x))
est_0
#=0.001614005

#a(4)
x=4
est_4=mean(cauch*dnorm(cauch,mean=x))/mean(dnorm(cauch,mean=x))
est_4
#=3.421435

#(b)
#Plot the standard error of the estimates in part (a) as the number of samples used for
#Monte Carlo integration increases from 1 to 10000(=10^4).

x1=dnorm(cauch,mean=x)
estint2=cumsum(x1)/(1:numit)
esterr2=sqrt(cumsum((x1-estint2)^2))/(1:numit)
x1=cauch*x1
estint1=cumsum(x1)/(1:numit)
esterr1=sqrt(cumsum((x1-estint1)^2))/(1:numit)
par(mfrow=c(1,2))
plot(estint1,type="l",xlab="iteration",ylab="",col="gold")
lines(estint1-2*esterr1,lty=2,lwd=2)
lines(estint1+2*esterr1,lty=2,lwd=2)
plot(estint2,type="l",xlab="iteration",ylab="",col="gold")
lines(estint2-2*esterr2,lty=2,lwd=2)
lines(estint2+2*esterr2,lty=2,lwd=2)

esterr1

esterr2


#(c)
#Repeat part (a) using the standard normal distribution, and compare the two approaches.

#A similar implementation applies for the normal simulation, replacing
#dnorm with dcauchy
set.seed(0)
numit=10^4
nor=rnorm(numit)
I=mean(nor*dnorm(nor,mean=x))/mean(dnorm(nor,mean=x)) #????????? Unused argument mean=x

#a(0)
x=0
n.est_0 = mean(nor*dnorm(nor,mean=x))/mean(dnorm(nor,mean=x))
n.est_0
# = 0.006018831

#a(4)
x=4
n.est_4 = mean(nor*dnorm(nor,mean=x))/mean(dnorm(nor,mean=x))
n.est_4
# = 1.956114

x1n=dnorm(nor)
estint2n=cumsum(x1n)/(1:numit)
esterr2n=sqrt(cumsum((x1n-estint2n)^2))/(1:numit)
x1n=nor*x1n
estint1n=cumsum(x1n)/(1:numit)
esterr1n=sqrt(cumsum((x1n-estint1n)^2))/(1:numit)
par(mfrow=c(1,2))
plot(estint1n,type="l",xlab="iteration",ylab="",col="gold")
lines(estint1n-2*esterr1n,lty=2,lwd=2)
lines(estint1n+2*esterr1n,lty=2,lwd=2)
plot(estint2n,type="l",xlab="iteration",ylab="",col="gold")
lines(estint2n-2*esterr2n,lty=2,lwd=2)
lines(estint2n+2*esterr2n,lty=2,lwd=2)

esterr1n

esterr2n

##compare large sample size
mean(esterr1[6000:10000]) # = 0.003178698
mean(esterr1n[6000:10000]) # = 0.001975513

mean(esterr2[6000:10000]) # = 0.0008297403
mean(esterr2n[6000:10000]) # = 0.001244682
