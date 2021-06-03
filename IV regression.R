#Simulating Data

library(MASS)
set.seed(1234)
n=1000
Rho=matrix(c(1, 0.5, 0.5, 1), 2, 2, byrow = TRUE)
sims = mvrnorm(n,c(0,0), Rho)
e = sims[,1] 
v = sims[,2]
z = runif(n)
x = 0.5 + 0.8 * z + v
y = -0.3 + x + e





#Run IV regression

OLS1=lm(y~x)
OLS1



first_stage = lm(x~z)
reduced_form = lm (y~z)
first_stage 
reduced_form
0.7850/0.8192

#I found the betas of x regress on z and y regress on z. Finally I found the beta of 2SLS.






library(AER)
iv_results=ivreg(y~x|z)
summary(iv_results)
confint(iv_results,level = 0.95)

#95%confidence interval (0.7014, 1.215)





#Generate variables

library("readxl")
df=read_excel('cigarette.xlsx')
df$ravgprs = df$avgprs/df$cpi
df$rtax = df$tax/df$cpi
df$rtaxs = df$taxs/df$cpi
df$rtaxso = df$rtaxs-df$rtax
df$lpackpc = log(df$packpc)
df$lravgprs = log(df$ravgprs)
df$perinc = df$income/(df$pop*df$cpi)
df$lperinc = log(df$perinc)
df


#First stage regression:
df2 = subset(df, year==1995)
lravphat=lm(lravgprs~rtaxso, data=df2)
summary(lravphat)


#Second stage regression:
df2$lravphat=predict(lravphat)
OLS3=lm(lpackpc~lravphat, data= df2, year==1995)
summary(OLS3)


#IV regression function
iv2=ivreg(lpackpc~lravgprs|rtaxso, data=df2)
summary(iv2)

#AJR wants to find out the fudamental causes of the large differences in income per capita acroos countries.
#AJR's key theory is that they think colonial institutions are different for diferent purposes. The colonialism after hundred years can still affect the current economic performance. Therefore, they want to find out that whether the european settlement would give positive effect or negative effect to the countries.




#Regress loggdp on risk:

df3 = read.csv('ajr.csv')
ols = lm(loggdp~risk, data=df3)
summary(ols)



library(stargazer)
stargazer(ols, type = "text")

#We cannot interpret the results of "ols" causally, because we need to consider the mortality.






#Estimate the ??rst-stage regression of risk on logmort0:

first_stage2 = lm(risk~logmort0, data = df3)
summary(first_stage2)

#The risk factor has negtive effect towards the log mortality. The log mortality will decrease 43% for every increase in risk.






#Estimate the reduced-formregression:
```{r}
reduced_form2 = lm(loggdp~logmort0, data = df3)
summary(reduced_form2)
```
#The log mortality has negtive effect towards the log gdp. The log gdp decreases by 0.56% for every 1% increase of log mortality.


#IV regression:

iv=ivreg(loggdp~risk|logmort0, data=df3)
summary(iv)

#The r-squared is much lower than the 'ols'. That means the actual variation explained is that big. 

df3$first_stage2 = predict(first_stage2) 
OLS4=lm(loggdp~first_stage2, data= df3)
summary(OLS4)






#Including malaria as an additional regressor:

ols8 = lm(loggdp~risk+malaria, data=df3)
summary(ols8)



OLS7=lm(risk~logmort0 + malaria, data= df3)
summary(OLS7)


iv9=ivreg(loggdp~risk|+malaria, data=df3)
summary(iv9)



