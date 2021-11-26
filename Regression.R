'regression analysis cheat sheet for online statistic exams'

######Multiple linear regression################################################

#Set length and values of vectors
x1 <- c(23,33,73,123,113,126,120,137,142,162,200,242,250,261)
y <- c(260,294,344,372,293,411,400,413,438,465,500,512,536,526)
x0 <- rep(1,NROW(x1))

X <- matrix(c(x0,x1),byrow=FALSE,ncol=2)
Y <- matrix(y,byrow=FALSE, ncol=1)

##Beta Dach - Estimate Coefficients
XtX <- t(X)%*%X
XtY <- t(X)%*%Y
b.dach <- solve(XtX)%*%XtY

##SSres - Residual Standard Error
YtY <- t(Y)%*%Y       #Summe von y^2
tb.tx.y <- t(b.dach)%*%t(X)%*%Y
# t(b.dach)%*% XtY alternativ
SSres <- YtY - tb.tx.y
paste('SSRes =',YtY,'-',tb.tx.y,'=', SSres)
SSr <- matrix(sum((t(b.dach)%*%t(X)-mean(Y))^2))
SSt <- SSr + SSres
paste('SSt =',SSr,'+',SSres,'=', SSt)
#RSE <- sqrt(SSres/(n-k-1))
#RSE <- sqrt(deviance(lm.model)/df.residual(lm.model))
#(1/10)*sum(y)**2 --> für Berechnung vom y für SSt Formel

#variance 
k <- ncol(X)-1
p <- k+1
sigma.quad <- SSres/(nrow(X)-p)
#RSE
sigma <- sqrt(sigma.quad)

##R-Value and R² value
#k and n with Degrees of freedom
k <- ncol(X)-1
n <- nrow(X)

R  <- 1-SSres/SSt
paste('R.Quadrat = 1 - ',SSres,'/',SSt,'=',R)
R.adju <- 1-(SSres/(n-k-1))/(SSt/(n-1))
paste('R.adjusted = 1- (',SSres,'/',n,'-',k,'- 1 ) / (',SSt,'/',n,'- 1) =', R.adju)


# -> R² is low:
# (1) Linear regression is simply not suitable for the problem (prerequisites not met)
# (2) The variables that were included are the wrong variables 

##F-Test
F0 <- (nrow(X)-k-1)/k*R/(1-R)
paste('F0 = (',nrow(X),'-',k,'- 1) /',k,' ) * (',R,'/ (1-',R,')) =',F0)
F1 <- qf(1-0.05,df1=k,df2=nrow(X)-k-1)

#Rejection or Acceptance of the model --> F-test?
if (F0 > F1) {
  paste('The regression model has to be rejected ', F0,'>',F1)
} else {
  paste('The regression model has to be rejected ', F0,'<',F1)
  }

##p-value of F-test
F0.pValue = 1-pf(F0,k,(nrow(X)-k-1))

##se(ßj) : Standard Errors
se.ß.0 = sigma*sqrt(solve(XtX)[1,1])
paste('Standard Error Beta.0 =',sigma,'* squareRootOf(',solve(XtX)[1,1],') = ',se.ß.0)
se.ß.1 = sigma*sqrt(solve(XtX)[2,2])
paste('Standard Error Beta.1 =',sigma,'* squareRootOf(',solve(XtX)[2,2],') = ',se.ß.1)
se.ß.2 = sigma*sqrt(solve(XtX)[3,3])
paste('Standard Error Beta.2 =',sigma,'* squareRootOf(',solve(XtX)[3,3],') = ',se.ß.2)

#t-statistics: How strong is the influence of the variables on the model ?
t.beta.0 = b.dach[1]/se.ß.0
paste('T-Statistik Beta 0 =',b.dach[1],'/',se.ß.0,'=', t.beta.0)
t.beta.1 = b.dach[2]/se.ß.1
paste('T-Statistik Beta 1 =',b.dach[2],'/',se.ß.1,'=', t.beta.1)
t.beta.2 = b.dach[3]/se.ß.2
paste('T-Statistik Beta 2 =',b.dach[3],'/',se.ß.2,'=', t.beta.2)


######Confidence and forecast intervals ########################################
alpha = 0.1

#(1-alpha) confidence interval for a regression coefficient
#Note that se.ß. and b. must be set for the regression coefficient 
paste("Confidence interval for the regression coefficient lower limit:", b.dach[2]- qt(1-alpha/2,n-k-1)*se.ß.1
      ,"upper limit", b.dach[2]+ qt(1-alpha/2,n-k-1)*se.ß.1)
paste('Confidence interval values for regression coefficients: ',b.dach[2],'-',qt(1-alpha/2,n-k-1),'*',se.ß.1)


#(1-alpha) confidence interval for the expected value
#(y from the regression model) 
y0 = t(X[1,])%*%b.dach
y0=b.dach[1] + b.dach[2]*1/19

X0.vektor = matrix(X[1,],byrow=FALSE, ncol=1)
paste("(1-alpha) confidence interval lower limit ",y0-qt(1-alpha/2,n-k-1)*sigma*sqrt(t(X[1,])%*%solve(XtX)%*%X0.vektor),
       "Obergrenze", y0+qt(1-alpha/2,n-k-1)*sigma*sqrt(t(X[1,])%*%solve(XtX)%*%X0.vektor))
paste('(1-alpha) confidence interval value calculation: ',y0,'+/-',qt(1-alpha/2,n-k-1),'*',sigma,'* squareRootOf(',t(X[1,])%*%solve(XtX)%*%X0.vektor,')')


#Prognosis interval for value y0 at point x0
#Significantly higher than the confidence interval, therefore higher inaccuracy, for a specific point from the distribution 

Set # y0 value correctly 
y0=b.dach[1] + b.dach[2]*290000
alpha = 0.05
paste("Forecast interval lower limit:",y0-qt(1-alpha/2,n-k-1)*sigma*sqrt(1+t(X[1,])%*%solve(XtX)%*%X0.vektor),
"Upper limit:", y0+qt(1-alpha/2,n-k-1)*sigma*sqrt(1+t(X[1,])%*%solve(XtX)%*%X0.vektor))
paste('Forecast interval value calculation: ',y0,'+/-',qt(1-alpha/2,n-k-1),'*',sigma,'* squareRootOf(1+',t(X[1,])%*%solve(XtX)%*%X0.vektor,')')

######Regression model with R built-in Functions################################
lm.frame <- data.frame(cbind(c(x1,y)))
lm.model <- lm(y ~ x1, data=lm.frame)
summary(lm.model)


#residuen = Y-Y. roof
#st.residuen = sum (residues ^ 2) / (n-k-1)
#qnorm (((1: 5) -0.5) / 5) 

#Standardized residuals for Kolmogoroff Smirnov test 
Resdiuals.standardized = lm.model$residuals / (SSres/(n-k-1))

######predict-function##########################################################

#Confidence intervals for the expected value 
predict(lm.model,newdata=data.frame(X[1,]),interval="confidence")

#Prognosis intervals for the value y0 at a new point x0 
predict(lm.model,newdata=data.frame(X[1,]),interval="pred")

######simple linear regression #################################################

#Set vector values
x <- c(31,19,33,17,23,29,41,18,18,15)
y<- c(36,36,53,34,36,40,63,18,22,41)

x <- x1
y.mean <- mean(y)
x.mean <- mean(x)
y.mean.quadrat <- mean(y)^2
x.mean.quadrat <- mean(x)^2


x.quadrat <- sum(x^2)
xy <- sum(x*y)

beta1.dach <- (xy-length(x)*mean(x)*mean(y))/(x.quadrat-length(x)*mean(x)^2)
beta0.dach <- mean(y)-beta1.dach*mean(x)
y.dach <- beta0.dach + beta1.dach*x


SST <- sum((y-mean(y))^2)
SSRES <- sum((y-y.dach)^2)
R.quadrat <- 1-(SSRES/SST)

#further values for the modell to estimate the R-values
k <- NCOL(x)
n <- NROW(x)
R.adju <- 1-(SSRES/(n-k-1))/(SST/(n-1))
F0 <- (n-k-1)/k*R.quadrat/(1-R.quadrat)
F1 <- qf(1-0.05,df1=k,df2=n-k-1)


#Rejection or Acceptance of F-test?
if (F0 > F1) {
  paste('The regression model has to be rejected ', F0,'>',F1)
} else {
  paste('The regression model has to be rejected ', F0,'<',F1)
}

##p-value of F-test
F0.pValue = 1-pf(F0,k,(n-k-1))

#regression estimated with built-in r-functions (just for control)
lm.model = lm(y ~ x)
summary(lm.model)
lm.model$fitted.values
y0=b.dach[0] + b.dach[1]*20

paste("Forecast interval lower limit: ",y0-qt(1-alpha/2,n-k-1)*sigma*sqrt(1+t(X[1,])%*%solve(XtX)%*%X0.vektor),
      "upper limit:", y0+qt(1-alpha/2,n-k-1)*sigma*sqrt(1+t(X[1,])%*%solve(XtX)%*%X0.vektor))

#standardized residuals for Kolmogoroff Smirnov test 
res.st = lm.model$residuals / sqrt((SSRES/(n-k-1)))



