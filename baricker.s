# Program to implement a power of a BA study design aimed
# at detecting a shift in the Ricker-a as in Hinrichsen (2001).
#Input definitions
#var is the error variance
#S1 is the sample mean of the spawner counts during the Before period
#S2 is the sample mean of the spawner counts during the After period
#varS1 is the sample variance of the spawner counts during the Before period
#varS2 is the sample variance of the spawner counts during the After period
#n1 is the number of Before years
#n2 is the number of After years
#theta is the true effect size
#alpha is the probability of a Type I error (assuming a two-sided alternative hypothesis)
#
baricker<-function(var=0.25,S1=100,S2=100,varS1=10,varS2=10,n1=10,n2=10,theta=log(2.0),alpha=0.05){
se<-var*(1/n1+1/n2+(S1-S2)*(S1-S2)/(n1*varS1+n2*varS2))
se<-sqrt(se)
cv<-se/theta
delta<-theta
N<-n1+n2
q<-qt(p=1-alpha/2,df=N-3)
power<-1-pt(q,ncp=delta/se,df=N-3)+pt(-q,ncp=delta/se,df=N-3)
return(list(var=var,S1=S1,S2=S2,varS1=varS1,varS2=varS2,n1=n1,n2=n2,theta=theta, alpha=alpha,se=se,cv=cv,power=power))
}
#outputs
#se -- standard error
#cv -- coefficient of variation
#power -- probability of rejecting the null hypothesis of no effect