##########################
# Plagioclase Prediction #
#  S.M. Morrison 2017    #
##########################

#Read in and attach plag data
plag=read.csv("/Users/smmorrison/Desktop/R/CheMin/Data/Plag_8.2017_avg.csv")

#Runs every permutation of parameter combinations
# library(glmulti)
# multi <- glmulti(K ~ a + b + c + alpha + beta + gamma +
#   I(a^2) + I(b^2) + I(c^2) + I(alpha^2) + I(beta^2) + I(gamma^2),
#   data = plag,level = 1, )
# print(multi)

#Linear models
options(digits=15)
lm_plag_Ca=lm(Ca ~ a + b + c + gamma + I(a^2) + I(b^2) + I(beta^2) + I(gamma^2), data = plag)
summary(lm_plag_Ca)

lm_plag_Na=lm(Na ~ a + b + c + gamma + I(a^2) + I(b^2) + I(beta^2) + I(gamma^2), data = plag)
summary(lm_plag_Na)

#You can use the equation below to calculate K, but the error is nearly as large as the magnitude
# lm_plag_K=lm(K ~ a + c + I(c^2) + I(alpha^2), data = plag)
# summary(lm_plag_K)

#non-linear model with lm coeff to start
# nls_plag <- nls (Ca ~ c0 + c1*a + c2*b + c3*c + c4*gamma + c5*I(a^2) + c6*I(b^2) + c7*I(beta^2) + c8*I(gamma^2), data = plag,
#                  start = list(c0 = summary(lm_plag_Ca)$coefficients[1,1],
#                               c1 = summary(lm_plag_Ca)$coefficients[2,1],
#                               c2 = summary(lm_plag_Ca)$coefficients[3,1],
#                               c3 = summary(lm_plag_Ca)$coefficients[4,1],
#                               c4 = summary(lm_plag_Ca)$coefficients[5,1],
#                               c5 = summary(lm_plag_Ca)$coefficients[6,1],
#                               c6 = summary(lm_plag_Ca)$coefficients[7,1],
#                               c7 = summary(lm_plag_Ca)$coefficients[8,1],
#                               c8 = summary(lm_plag_Ca)$coefficients[9,1]),
#                  control = nls.control(warnOnly = T))
# summary(nls_plag)
#nls does not converge, likely because it cannot improve upon the lm


#Compute RMSE on all data
plag_all=read.csv("/Users/smmorrison/Desktop/R/CheMin/Data/Plag_8.2017_all.csv")
plag_all$Ca_calc <- c(predict(lm_plag_Ca, newdata=plag_all))
plag_all$Na_calc <- c(predict(lm_plag_Na, newdata=plag_all))
#plag_all$K_calc <- (1-plag_all$Ca_calc-plag_all$Na_calc)
  
plag_all$Ca_all_ResSq <- (plag_all$Ca_calc-plag_all$Ca)^2
plag_all$Na_all_ResSq <- (plag_all$Na_calc-plag_all$Na)^2
# plag_all$K_all_ResSq <- (plag_all$K_calc-plag_all$K)^2

RMSE_Ca <- sqrt(mean(plag_all$Ca_all_ResSq))
RMSE_Na <- sqrt(mean(plag_all$Na_all_ResSq)) 
# RMSE_K <- sqrt(mean(plag_all$K_all_ResSq)) 

RMSE_Ca
RMSE_Na
# RMSE_K

#playing around            
cor(plag[,-1]) #correlation matrix
cov(plag[,-1]) #covariance matrix
sapply(plag,mean) #average of each parameter in plag
plot(plag$Ca,residuals(nls_plag))
plot(residuals(nls_plag))
hist(plag$a)
#end playing around

#Cross-validation for Ca
ratio <- 0.80 #Training on 80% of data
mse_Ca <- c()

for (i in 1:1000) {
  train.ind <- sample(1:nrow(plag),nrow(plag)*ratio)
  
  train <- plag[train.ind,]
  test <- plag[-train.ind,]
  
  lm_plag_Ca_train <- lm(Ca~a+b+c+gamma+I(a^2)+I(b^2)+I(beta^2)+I(gamma^2),data=train)

  predicted <- predict(lm_plag_Ca_train,test)
  
  mse_Ca <- c(mse_Ca,mean((predicted-test$Ca)^2))
  
}

#average rmse over all runs
rmse_Ca_test <- sqrt(mean(mse_Ca))
rmse_Ca_test

#Cross-validation for Na
mse_Na <- c()

for (i in 1:1000) {
  train.ind <- sample(1:nrow(plag),nrow(plag)*ratio)
  
  train <- plag[train.ind,]
  test <- plag[-train.ind,]
  
  lm_plag_Na_train <- lm(Na~a+b+c+gamma+I(a^2)+I(b^2)+I(beta^2)+I(gamma^2),data=train)
  
  predicted <- predict(lm_plag_Na_train,test)
  
  mse_Na <- c(mse_Na,mean((predicted-test$Na)^2))
  
}

#average rmse over all runs
rmse_Na_test <- sqrt(mean(mse_Na))
rmse_Na_test

#End cross validation
######################################################################################################

#Calculate Ca & Na of martian samples (based on Ca algorithm and difference)
mars_plag=read.csv("/Users/smmorrison/Desktop/R/CheMin/Data/Mars/mars_plag.csv")

mars_plag$Ca_calc <- c(predict(lm_plag_Ca, newdata = mars_plag))
mars_plag$Na_calc <- c(predict(lm_plag_Na, newdata = mars_plag))
# mars_plag$K_calc <- (1-mars_plag$Ca_calc+mars_plag$Na_calc)
mars_plag$sum <- (mars_plag$Ca_calc + mars_plag$Na_calc)