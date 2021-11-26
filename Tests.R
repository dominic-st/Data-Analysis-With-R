'hypothesis test cheatsheet for online exams'

######cheatsheet for confidence intervals#######################################
# Two-sided KI for µ, if R² is known 
Gu = mean(x) - qnorm(1-alpha/2) * sigma/sqrt(length(x))
Go = mean(x) + qnorm(1-alpha/2) * sigma/sqrt(length(x))

# One-sided confidence interval [Gunder;Gupper[ for µ with known R² 
Gu = mean(x) - qnorm(1-alpha) * sigma/sqrt(length(x))
Go = mean(x) + qnorm(1-alpha) * sigma/sqrt(length(x))

#Second case: R² is unknown 
Gu = mean(x) - qt(1-alpha/2, length(x)-1) * sd(x)/sqrt(length(x))
Go = mean(x) - qt(1-alpha/2, length(x)-1) * sd(x)/sqrt(length(x))

#Confidence interval for variance 
Gu = (length(x)-1)*var(x)/qchisq(1-alpha/2, length(x)-1)
Go = (length(x)-1)*var(x)/qchisq(alpha/2, length(x)-1)

#Cheatsheet for hypothesis tests in R###########################################
'Commands for the distributions '

'Standard normal distribution'

#density
dnorm(x=2,mean=5,sd=3)
#distribution 
pnorm(x=1.764,mean=287.72,sd=23.06985)
#quantile 
qnorm(p=0.95,mean=2,sd=9)

't-distribution'

#denisty
dt(x=3,df=16)
#distribution
pt(q=1.764,df=10)
#Quantile
qt(p=0.95,df=16)

'Wilcox-distribution'

#Quantile
qwilcox(0.975,8,8)

#Chi-Quadrat
qchisq(p=0.95,df=3)

######one sample tests##########################################################

'one sample expectation tests'

#Read in vector and specify hypothesis values that are known
#set vectors
x <- c()
mü.o = 0
sigma = 0
alpha = 0.05

#variance known
#testatistics V
one-sample.varKnown.V  = sqrt(length(x))*(mean(x)-mü.0)/sigma

#hypothesis pair rejection and associated p-value: 
paste('Hypothesis pairs and associated p-values for one-sample test with known variance or unknown and N(0.1) assumption ')

paste('mü = mü.0')
paste("Rejection of the null hypothesis ?:", abs(one-sample.varKnown.V ) > qnorm(1-alpha/2),"with critical value ", qnorm(1-alpha/2))
paste('p-Value: 2*(1-qnorm(|v|)) = ',2*(1-qnorm(abs(v))))

paste('mü >= mü0')
paste("Rejection of the null hypothesis ?:", one-sample.varKnown.V  < qnorm(alpha), "with critical value ", qnorm(alpha))
paste('p-Value: qnorm(v) = ',qnorm(one-sample.varKnown.V ))

paste('mü <= mü.0')
paste("Rejection of the null hypothesis ?:", v > qnorm(1-alpha/2),"with critical value ",qnorm(1-alpha/2))
paste('p-Value: 1-qnorm(v) = ',1-qnorm(one-sample.varKnown.V ))


#Variance unknown -> sample variance is used
#Teststatistic 
x = fill
mü.0 = 425
alpha = 0.05
sample.VarUnknown.V  = sqrt(length(x))*(mean(x)-mü.0)/sd(x)

# Hypothesis pair rejection and associated p-value: 
paste('Hypothesis pairs and associated p-values for one-shot test (expected value) with unknown variance and assumption of normal distribution ')

paste('mü = mü.0')
paste("Rejection of the null hypothesis  ?:", abs(sample.VarUnknown.V ) > qt(p=1-alpha/2,df=length(x)-1),
      "with criticial value", qt(p=1-alpha/2,df=length(x)-1))
paste('p-Value: 2*(1-qnorm(|v|)) = ',2*(1-pt(q=abs(sample.VarUnknown.V ),df=length(x)-1)))

paste('mü >= mü0')
paste("Rejection of the null hypothesis  ?:", sample.VarUnknown.V  < qt(p=alpha,df=length(x)-1), 
      "with criticial value", qt(p=alpha,df=length(x)-1))
paste('p-Value: qnorm(v) = ',pt(q=sample.VarUnknown.V , df = length(x)-1))

paste('mü <= mü.0')
paste("Rejection of the null hypothesis  ?:", sample.VarUnknown.V  > qt(p=1-alpha,df=length(x)-1),
      "with criticial value",qt(p=1-alpha,df=length(x)-1))
paste('p-Value: 1-qnorm(v) = ',1-pt(q=sample.VarUnknown.V , df = length(x)))

## One-sample proportional value test -> proportional value test for dichotomous Xi 
'exact values'
n = 0
p0 = 0 #between 0 and 1
alpha = 0
oneSample.proportionalValue.approximate   = 0  # is known about the problem ~ Bin (n, p0) 

# Hypothesis pair rejection and associated p-value: 
paste('Hypothesis pairs and associated p-values for one-shot test (proportional value) with known variance or unknown and N (0.1) assumption ')
paste('mü = mü.0')
paste("Rejection of the null hypothesis  ?:", 
      qbinom(alpha/2,n,p0) < abs(oneSample.proportionalValue.approximate  ) < qbinom(1-alpha/2,n,p0),
      "with critical values", qbinom(alpha/2,n,p0), qbinom(1-alpha/2,n,p0) )
paste('p-Value: p*) = ', binom.test(oneSample.proportionalValue.approximate  ,n,p0)$p.Value)

paste('mü >= mü0')
paste("Rejection of the null hypothesis  ?:", oneSample.proportionalValue.approximate   < qbinom(alpha,n,p0), 
      "with criticial value", qbinom(alpha,n,p0))
paste('p-Value: F(v) = ',binom.test(oneSample.proportionalValue.approximate  ,n,p0)$p.Value)

paste('mü <= mü.0')
paste("Rejection of the null hypothesis  ?:", oneSample.proportionalValue.approximate   > qbinom(1-alpha/2,n,p0),
      "with criticial value",qbinom(1-alpha/2,n,p0))
paste('p-Value: 1-F(v-1) = ',binom.test(oneSample.proportionalValue.approximate  ,n,p0)$p.Value)


'approximate '
n = 0
p0 = 0
x.EW = 0
oneSample.proportionalValue.approximate.v = sqrt(n)*(x.EW-p0)/sqrt(p0*(1-p0))

# Hypothesis pair rejection and associated p-value: 
paste('mü = mü.0')
paste("Rejection of the null hypothesis  ?:", abs(oneSample.proportionalValue.approximate.v) > qnorm(1-alpha/2),
      "with criticial value: ", qnorm(1-alpha/2))
paste('p-Value: 2*(1-qnorm(|v|)) ', 2*(1-qnorm(abs(oneSample.proportionalValue.approximate.v))))

paste('mü >= mü0')
paste("Rejection of the null hypothesis  ?:", oneSample.proportionalValue.approximate.v < qnorm(alpha), 
      "with criticial value:", qnorm(alpha))
paste('p-Value: qnorm(v) = ',qnorm(oneSample.proportionalValue.approximate.v))

paste('mü <= mü.0')
paste("Rejection of the null hypothesis  ?:", oneSample.proportionalValue.approximate.v > qnorm(1-alpha/2),
      "with criticial value",qnorm(1-alpha/2))
paste('p-Value: 1-qnorm(v) = ',1-qnorm(oneSample.proportionalValue.approximate.v))

paste('Validity of the normal approximation :', n*p0*dbinom(x.EW,n,p0))

##One-sample variance tests (via chi-square) 
#set vector
x = c()
n= 0
sigma = 0
mü = 0
alpha = 0.05
#expected value mü unknown
oneSample.varianceTest.müUnknown.v = (1/sigma^2)*sum((x-mean(x))^2)

#expected value mü known
oneSample.varianceTest.müKnown.v = (1/sigma^2)*sum((x-mü)^2)


#set v before executing hypothesis tests
V = oneSample.varianceTest.müUnknown.v
# Hypothesis pair rejection and associated p-value: 
paste('mü = mü.0')
paste("Rejection of the null hypothesis  ?:", v < qchisq(p=alpha/2,df=n-1) | v > qchisq(p=1-alpha/2,df=n-1),
      "with criticial value: ", v < qchisq(p=alpha/2,df=n-1), v > qchisq(p=1-alpha/2,df=n-1))
paste('p-Value: 2*min(pchisq(v,n-1),1-pchisq(v,n-1))', 2*min(pchisq(v,n-1),1-pchisq(v,n-1)))

paste('mü >= mü0')
paste("Rejection of the null hypothesis  ?:", 0 < v < qchisq(p=alpha,df=n-1), 
      "with criticial value:", qchisq(p=alpha,df=n-1))
paste('p-Value: pchisq(v,n-1) =',pchisq(v,n-1))

paste('mü <= mü.0')
paste("Rejection of the null hypothesis  ?:", v > qchisq(p=1-alpha,df=n-1),
      "with criticial value",qchisq(p=1-alpha,df=n-1))
paste('p-Value: 1-pchisq(v,n-1 =',1-pchisq(v,n-1))


######two-sample-tests##########################################################


# set vector values
x1 <- c(13,15,17,18,22,23,24,28,30,31)
x2 <- c(16,19,21,25,26,32,33,34,35,37)

#Variants the same? Which test should be taken? 
paste ('If TRUE use t-test, if false use Welch-test :',var(x1) == var(x2))

##welch-test
x1 =c(13,15,17,18,22,23,24,28,30,31)
x2 = c(16,19,21,25,26,32,33,34,35,37)
mean.x1 = mean(x1)
mean.x2 = mean(x2)
alpha = 0.05
twoSample.welch.v = (mean.x1-mean.x2-0)/sqrt(var(x1)/length(x1)+ var(x2)/length(x2))
welch.k.upperLimit = (var(x1)/length(x1)+var(x2)/length(x2))^2
welch.k.lowerLimit = (var(x1)/length(x1))^2/(length(x1)-1) + (var(x2)/length(x2))^2/(length(x2)-1)
twoSample.welch.k = (welch.k.upperLimit/welch.k.lowerLimit)

#Hypothesis pair rejection and associated p-value: 
paste('Hypothesis pairs and associated p-values for two-sample test (Welch test) with unknown variance and assumption of normal distribution ')

paste('mü = mü.0')
paste("Rejection of the null hypothesis  ?:", abs(twoSample.welch.v) > qt(p=1-alpha/2,df=twoSample.welch.k),
      "with criticial value", qt(p=1-alpha/2,df=twoSample.welch.k))
paste('p-Value: 2*(1-F(|v|)) = ',2*(1-pt(q=abs(twoSample.welch.v),df=twoSample.welch.k)))

paste('mü >= mü0')
paste("Rejection of the null hypothesis  ?:", twoSample.welch.v < qt(p=alpha,df=twoSample.welch.k), 
      "with criticial value", qt(p=alpha,df=twoSample.welch.k))
paste('p-Value: F(v) = ',pt(q=twoSample.welch.v, df = twoSample.welch.k))

paste('mü <= mü.0')
paste("Rejection of the null hypothesis  ?:", twoSample.welch.v > qt(p=1-alpha,df=twoSample.welch.k),
      "with criticial value",qt(p=1-alpha,df=twoSample.welch.k))
paste('p-Value: 1-F(|v|) = ',1-pt(q=abs(twoSample.welch.v), df = twoSample.welch.k))


#two.sided null hypothesis
#less = null hypothesis, less than
#greater= null hypothesis, greater than 
summary(t.test(x1,x2, var.equal = FALSE, alternative = "two.sided"))


##t-test
alpha = 0.05
twoSample.Ttest.v = (mean.x1-mean.x2-0)/sqrt((1/length(x1)+1/length(x2))*((length(x1)-1)*var(x1)+(length(x2)-1)*var(x2))/(length(x1)+length(x2)-2))

# Hypothesis pair rejection and associated p-value: 
paste('Hypothesis pairs and associated p-values for two-sample test (T-test) with known variance and assumption of normal distribution')
paste('mü = mü.0')
paste("Rejection of the null hypothesis  ?:", abs(twoSample.Ttest.v) > qt(p=1-alpha/2,df=length(x1)+length(x2)-2),
      "with criticial value", qt(p=1-alpha/2,df=length(x1)+length(x2)-2))
paste('p-Value: 2*(1-qnorm(|v|)) = ',2*(1-pt(p=abs(twoSample.Ttest.v),df=length(x1)+length(x2)-2)))

paste('mü >= mü0')
paste("Rejection of the null hypothesis  ?:", twoSample.Ttest.v < qt(p=alpha,df=length(x1)+length(x2)-2), 
      "with criticial value", qt(p=alpha,df=length(x1)+length(x2)-2))
paste('p-Value: qnorm(v) = ',pt(p=twoSample.Ttest.v, df = length(x1)+length(x2)-2))

paste('mü <= mü.0')
paste("Rejection of the null hypothesis  ?:", twoSample.Ttest.v > qt(p=1-alpha,df=length(x1)+length(x2)-2),
      "with criticial value",qt(p=1-alpha,df=length(x1)+length(x2)-2))
paste('p-Value: 1-qnorm(v) = ',1-pt(p=abs(twoSample.Ttest.v), df = length(x1)+length(x2)-2))


#two.sided Nullhypothese: =
#less Nullhyopthese: kleiner als
#greater Nullhypothese: größer als
t.test(x1,x2, var.equal = TRUE, alternative = "two.sided")

##Gauss test (normal distribution with known variances)
#known for very large samples and variance due to sample variance
#set vectors
x1 = c()
x2 = c()
alpha = 0.05
sigma1.quad = 0
sigma2.quad = 0
twoSample.gaussTest.v = (mean(x1)-mean(x2)-0)/(sigma1.quad/length(x1)+sigma2.quad/length(x2))

#Hypothesis pair rejection and associated p-value:
paste('mü.1 -mü.0 = sigma0 = 0')
paste("Rejection of the null hypothesis  ?:", abs(twoSample.gaussTest.v) > qnorm(1-alpha/2),
      "with criticial value: ", qnorm(1-alpha/2))
paste('p-Value: 2*(1-qnorm(|v|)) ', 2*(1-qnorm(abs(twoSample.gaussTest.v))))

paste('mü.1 -mü.0 >= sigma0 = 0')
paste("Rejection of the null hypothesis  ?:", twoSample.gaussTest.v < qnorm(alpha), 
      "with criticial value:", qnorm(alpha))
paste('p-Value: qnorm(v) = ',qnorm(twoSample.gaussTest.v))

paste('mü.1 -mü.0 <= sigma0 = 0')
paste("Rejection of the null hypothesis  ?:", twoSample.gaussTest.v > qnorm(1-alpha/2),
      "with criticial value",qnorm(1-alpha/2))
paste('p-Value: 1-qnorm(v) = ',1-qnorm(twoSample.gaussTest.v))


##approximate Gaussian test
#Same as Gaussian test only with sample variance, as correct variance is unknown
#set vectors
x1 = c()
x2 = c()
alpha = 0.05
twoSample.ApproxGaussTest.v = (mean(x1)-mean(x2)-0)/(var(x1)/length(x1)+var(x2)/length(x2))

#Hypothesis pair rejection and associated p-value: 
paste('mü.1 -mü.0 = sigma0 = 0')
paste("Rejection of the null hypothesis  ?:", abs(twoSample.ApproxGaussTest.v) > qnorm(1-alpha/2),
      "with criticial value: ", qnorm(1-alpha/2))
paste('p-Value: 2*(1-qnorm(|v|)) ', 2*(1-pnorm(abs(twoSample.ApproxGaussTest.v))))

paste('mü.1 -mü.0 >= sigma0 = 0')
paste("Rejection of the null hypothesis  ?:", twoSample.ApproxGaussTest.v < qnorm(alpha), 
      "with criticial value:", qnorm(alpha))
paste('p-Value: qnorm(v) = ',pnorm(twoSample.ApproxGaussTest.v))

paste('mü.1 -mü.0 <= sigma0 = 0')
paste("Rejection of the null hypothesis  ?:", twoSample.ApproxGaussTest.v > qnorm(1-alpha/2),
      "with criticial value",qnorm(1-alpha/2))
paste('p-Value: 1-qnorm(v) = ',1-pnorm(twoSample.ApproxGaussTest.v))

#t-test with connected sample
#Survey between the same feature carriers
#One-sample t-test for differences Di = X2i-X1i
#set vectors
x1 = c()
x2 = c()
alpha = 0.05
sigma.d.quad = var(x2-x1)
twoSample.Ttest.bound.v = sqrt(length(x))*sqt(sigma.d.quad)

#Hypothesis pair rejection and associated p-value: 
paste('Hypothesis pairs and associated p-values for two-sample test (T-test) with known variance and assumption of normal distribution')
paste('mü = mü.0')
paste("Rejection of the null hypothesis  ?:", abs(twoSample.Ttest.bound.v) > qt(p=1-alpha/2,df=length(x1)-1),
      "with criticial value", qt(p=1-alpha/2,df=length(x1)-1))
paste('p-Value: 2*(1-qnorm(|v|)) = ',2*(1-pt(p=abs(twoSample.Ttest.bound.v),df=length(x1)-1)))

paste('mü >= mü0')
paste("Rejection of the null hypothesis  ?:", twoSample.Ttest.bound.v < qt(p=alpha,df=length(x1)-1), 
      "with criticial value", qt(p=alpha,df=length(x1)-1))
paste('p-Value: qnorm(v) = ',pt(p=twoSample.Ttest.bound.v, df = length(x1)-1))

paste('mü <= mü.0')
paste("Rejection of the null hypothesis  ?:", twoSample.Ttest.bound.v > qt(p=1-alpha,df=length(x1)-1),
      "with criticial value",qt(p=1-alpha,df=length(x1)-1))
paste('p-Value: 1-qnorm(v) = ',1-pt(p=abs(twoSample.Ttest.bound.v), df = length(x1)-1))


##wilcoxon signed rank-test
#set vector values

x = c(12.2,11.0,13.4,12.6,10.8,11.2,13.1,11.7)
y = c(12.9,13.9,10.9,13.2,12.7,12.3,11.1)

#use the sorted order for the exam as a ranking template 
twoSample.wilcoxon.v = sort(cbind(x,y))

#Hypothesis pair rejection and associated p-value: 
paste('Hypothesis pairs and associated p-values for two-sample test (T-test) with known variance and assumption of normal distribution ')
paste('med(X) = med(Y)')
paste("Rejection of the null hypothesis  ?:", "w > w1-alpha/2(n1,n2)")

paste('med(X) >= med(Y)')
paste("Rejection of the null hypothesis  ?:", "w alpha(n1,n2)")

paste('med(X) <= med(Y)')
paste("Rejection of the null hypothesis  ?:","w 1-alpha(n1,n2)")


#If ties (same ranks exist) -> exact = FALSE, if no bindings = TRUE
#correct = TRUE if n <40 and or bindings exist 
wilcox.test(x1,x2,paired = TRUE)

#if test p <0.05 null hypothesis rejected 

######Distribution tests########################################################

##Kolmogorov-Smirnov-test
res.st = c(44,37,35,31,31,29,28)
res.st = as.vector(lm.model$residuals / sqrt((SSres/(n-k-1))))
n=length(res.st)
ks.xj = sort(res.st)
ks.xj.pnorm = sort(pnorm(res.st))
ks.Distance.upperLimit.measure = rep(1:n/n)
ks.Distance.lowerLimit.measure = rep((1:n-1)/n)
ks.Distance.upperLimit = abs(ks.xj.pnorm - ks.Distance.upperLimit.measure)
ks.Distance.lowerLimit = abs(ks.xj.pnorm - ks.Distance.lowerLimit.measure)

#highest value = Dn
ks.Dn = max(cbind(ks.Distance.upperLimit, ks.Distance.lowerLimit))
ks.v = ks.Dn * sqrt(n)

#verification
ks.test(res.st,pnorm)

##chi-squares independence test 
x <- c(65963,4000,2642,303)
y <- c(12813,647,359,42)

data <- data.frame(cbind(x,y))
chisq.test(data)


#Calculate chi-square (test statistic V) 
K = L = 0
chiSquared.independence  <- function(kt)
{
  
  chi_sum = 0
  
  for(i in 1:(nrow(kt)-1))
  {
    for (j in 2:(ncol(kt)-1)) {
      
      chi_sum = chi_sum + (kt[i,j]^2/(kt[nrow(kt),j]*kt[i,ncol(kt)]))
      
    }
  }
  n = kt[nrow(kt),ncol(kt)]
  K <- ncol(kt)-1
  L <- nrow(kt)-1
  
  chi_quadrat = n*(chi_sum-1)
  
  return(chi_quadrat)
}
distributionTests.chiSquared.independence.v  = chiSquared.independence(kt)
distributionTests.chiSquared.independence.df  = (K-1)*(L-1)

#Decision rule 
paste('Ho will be rejected',distributionTests.chiSquared.independence.v  
      > pchisq(q = v, df = distributionTests.chiSquared.independence.df ))

##Chi-square goodness-of-fit test 
data <- c(45,36,26,20,17,11,13,6,6)
n <- sum(data)
frequency.expected <- c(0.301,0.176,0.125,0.097,0.079,0.067,0.058,0.051,0.046)
frequency.expected <- round(frequency.expected/sum(frequency.expected)*n,2)# Bring to the same total amount
v.vector <- round((data-frequency.expected)^2 / frequency.expected,2)
v <- sum(v.vector)

paste('Ho will be rejected',v <= qchisq(p=0.95,df=length(data)-1))