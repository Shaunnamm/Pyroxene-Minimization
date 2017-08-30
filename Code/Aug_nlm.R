#######################
# Augite Minimization #
# S.M. Morrison 2017  #
#######################

#Read in the earth augite dataset (uc params of minerals with identical compositions averaged to avoid weighting)
aug=read.csv("/Users/smmorrison/Desktop/R/CheMin/Data/Aug_8.2017_avg.csv")

#Run linear models to compute starting parameters for non-linear modeling
lm_a <- lm(a ~ Mg + Ca + I(Mg^2) + I(Mg*Ca) + I(Ca^3) + I(Mg*Ca^2), data = aug)
summary(lm_a)
lm_b <- lm(b ~ Mg + Ca + I(Mg^2) + I(Ca^2) + I(Mg*Ca) + I(Mg*Ca^2), data = aug)
summary(lm_b)
lm_beta <- lm(beta ~ Mg + I(Mg^2) + I(Ca^2) + I(Mg*Ca) + I(Mg^3) + I(Ca^3) + I(Mg^2*Ca) + I(Mg*Ca^2), data = aug)
summary(lm_beta)

#Non-linear modeling of uc params as a function of composition (Ca & Mg)
nls_a <- nls (a ~ c0 + c1*Mg + c2*Ca + c3*I(Mg^2) + c4*I(Mg*Ca) + c5*I(Ca^3) + c6*I(Mg*Ca^2), 
      data = aug, start = list(c0 = summary(lm_a)$coefficients[1,1], 
                               c1 = summary(lm_a)$coefficients[2,1], 
                               c2 = summary(lm_a)$coefficients[3,1], 
                               c3 = summary(lm_a)$coefficients[4,1], 
                               c4 = summary(lm_a)$coefficients[5,1], 
                               c5 = summary(lm_a)$coefficients[6,1],
                               c6 = summary(lm_a)$coefficients[7,1]), 
      control = nls.control(warnOnly = T))
summary(nls_a)
nls_b <- nls (b ~ c0 + c1*Mg + c2*Ca + c3*I(Mg^2) + c4*I(Ca^2) + c5*I(Mg*Ca) + c6*I(Mg*Ca^2), 
      data = aug, start = list(c0 = summary(lm_b)$coefficients[1,1], 
                               c1 = summary(lm_b)$coefficients[2,1], 
                               c2 = summary(lm_b)$coefficients[3,1], 
                               c3 = summary(lm_b)$coefficients[4,1], 
                               c4 = summary(lm_b)$coefficients[5,1], 
                               c5 = summary(lm_b)$coefficients[6,1],
                               c6 = summary(lm_b)$coefficients[7,1]), 
      control = nls.control(warnOnly = T))
summary(nls_b)
nls_beta <- nls (beta ~ c0 + c1*Mg + c2*I(Mg^2) + c3*I(Ca^2) + c4*I(Mg*Ca) + c5*I(Mg^3) + c6*I(Ca^3) + c7*I(Mg^2*Ca) + c8*I(Mg*Ca^2), 
      data = aug, start = list(c0 = summary(lm_beta)$coefficients[1,1], 
                               c1 = summary(lm_beta)$coefficients[2,1], 
                               c2 = summary(lm_beta)$coefficients[3,1], 
                               c3 = summary(lm_beta)$coefficients[4,1], 
                               c4 = summary(lm_beta)$coefficients[5,1], 
                               c5 = summary(lm_beta)$coefficients[6,1],
                               c6 = summary(lm_beta)$coefficients[7,1],
                               c7 = summary(lm_beta)$coefficients[8,1],
                               c8 = summary(lm_beta)$coefficients[9,1]), 
      control = nls.control(warnOnly = T))
summary(nls_beta)

#Storing nls coefficients 
{
c0_a <- summary(nls_a)$coefficients[1,1]
c1_a <- summary(nls_a)$coefficients[2,1]
c2_a <- summary(nls_a)$coefficients[3,1]
c3_a <- summary(nls_a)$coefficients[4,1]
c4_a <- summary(nls_a)$coefficients[5,1]
c5_a <- summary(nls_a)$coefficients[6,1]
c6_a <- summary(nls_a)$coefficients[7,1]
}

{
c0_b <- summary(nls_b)$coefficients[1,1]
c1_b <- summary(nls_b)$coefficients[2,1]
c2_b <- summary(nls_b)$coefficients[3,1]
c3_b <- summary(nls_b)$coefficients[4,1]
c4_b <- summary(nls_b)$coefficients[5,1]
c5_b <- summary(nls_b)$coefficients[6,1]
c6_b <- summary(nls_b)$coefficients[7,1]
}

{
c0_beta <- summary(nls_beta)$coefficients[1,1]
c1_beta <- summary(nls_beta)$coefficients[2,1]
c2_beta <- summary(nls_beta)$coefficients[3,1]
c3_beta <- summary(nls_beta)$coefficients[4,1]
c4_beta <- summary(nls_beta)$coefficients[5,1]
c5_beta <- summary(nls_beta)$coefficients[6,1]
c6_beta <- summary(nls_beta)$coefficients[7,1]
c7_beta <- summary(nls_beta)$coefficients[8,1]
c8_beta <- summary(nls_beta)$coefficients[9,1]
}

#Predicting uc params with nls models
a_calc <- predict(nls_a,aug)
b_calc <- predict(nls_b,aug)
beta_calc <- predict(nls_beta,aug)

aug$a_calc = a_calc
aug$b_calc = b_calc
aug$beta_calc = beta_calc

#Computing RMSE of calculated uc params
a_calc_ResSq <- (a_calc-aug$a)^2
b_calc_ResSq <- (b_calc-aug$b)^2
beta_calc_ResSq <- (beta_calc-aug$beta)^2

RMSE_a <- sqrt(mean(a_calc_ResSq))
RMSE_b <- sqrt(mean(b_calc_ResSq))  
RMSE_beta <- sqrt(mean(beta_calc_ResSq))

RMSE_a
RMSE_b
RMSE_beta
  
#Weighting a & b for minimization. 
weight_a <- a_calc/beta_calc
weight_b <- b_calc/beta_calc


#Minimization
  # par[1] = Mg
  # par[2] = Ca
Mg_calc<-c()
Ca_calc<-c()
sum_sq_error <- c()
aug_all=read.csv("/Users/smmorrison/Desktop/R/CheMin/Data/Aug_8.2017_all.csv")
for (i in 1:nrow(aug_all)) {
f <- function(par) { 
  (((aug_all$a[i]-(c0_a+c1_a*par[1]+c2_a*par[2]+c3_a*par[1]^2+c4_a*par[1]*par[2]+c5_a*par[2]^3+c6_a*par[1]*par[2]^2))
    /((c0_a+c1_a*par[1]+c2_a*par[2]+c3_a*par[1]^2+c4_a*par[1]*par[2]+c5_a*par[2]^3+c6_a*par[1]*par[2]^2)/(c0_beta+c1_beta*par[1]+c2_beta*par[1]^2+c3_beta*par[2]^2+c4_beta*par[1]*par[2]+c5_beta*par[1]^3+c6_beta*par[2]^3+c7_beta*par[1]^2*par[2]+c8_beta*par[1]*par[2]^2)))^2
  + ((aug_all$b[i]-(c0_b+c1_b*par[1]+c2_b*par[2]+c3_b*par[1]^2+c4_b*par[2]^2+c5_b*par[1]*par[2]+c6_b*par[1]*par[2]^2))
     /((c0_b+c1_b*par[1]+c2_b*par[2]+c3_b*par[1]^2+c4_b*par[2]^2+c5_b*par[1]*par[2]+c6_b*par[1]*par[2]^2)/(c0_beta+c1_beta*par[1]+c2_beta*par[1]^2+c3_beta*par[2]^2+c4_beta*par[1]*par[2]+c5_beta*par[1]^3+c6_beta*par[2]^3+c7_beta*par[1]^2*par[2]+c8_beta*par[1]*par[2]^2)))^2
  + (aug_all$beta[i]-(c0_beta+c1_beta*par[1]+c2_beta*par[1]^2+c3_beta*par[2]^2+c4_beta*par[1]*par[2]+c5_beta*par[1]^3+c6_beta*par[2]^3+c7_beta*par[1]^2*par[2]+c8_beta*par[1]*par[2]^2))^2)
}

min<-nlminb(start=c(2,1), objective=f, lower=c(0,0), upper=c(2,1), control=list(trace=TRUE)) #always set "start" to your upper bound

Mg_calc <- append(Mg_calc,min$par[1])
Ca_calc <- append(Ca_calc,min$par[2])
Fe_calc <- (2-Mg_calc-Ca_calc)
sum_sq_error <- append(sum_sq_error,min$objective)

}

#Computing RMSE of calculated composition
aug_all$Mg_calc = Mg_calc
aug_all$Ca_calc = Ca_calc
aug_all$Fe_calc = Fe_calc

Ca_ResSq <- (Ca_calc-aug_all$Ca)^2
Fe_ResSq <- (Fe_calc-aug_all$Fe)^2
Mg_ResSq <- (Mg_calc-aug_all$Mg)^2

RMSE_Ca <- sqrt(mean(Ca_ResSq))
RMSE_Fe <- sqrt(mean(Fe_ResSq))  
RMSE_Mg <- sqrt(mean(Mg_ResSq))

RMSE_Ca
RMSE_Fe
RMSE_Mg

##########################################################################################

#Calculate chemistry for your dataset (Mars, in our case)
mars_aug=read.delim("/Users/smmorrison/Desktop/R/CheMin/Data/Mars/mars_aug_2.txt")

# par[1] = Mg
# par[2] = Ca
mars_Mg_calc<-c()
mars_Ca_calc<-c()
mars_sum_sq_error <- c()
mars_a_calc <- c()
mars_b_calc <- c()
mars_beta_calc <- c()
for (i in 1:nrow(mars_aug)) {
  f <- function(par) { 
    (((mars_aug$a[i]-(c0_a+c1_a*par[1]+c2_a*par[2]+c3_a*par[1]^2+c4_a*par[1]*par[2]+c5_a*par[2]^3+c6_a*par[1]*par[2]^2))
      /((c0_a+c1_a*par[1]+c2_a*par[2]+c3_a*par[1]^2+c4_a*par[1]*par[2]+c5_a*par[2]^3+c6_a*par[1]*par[2]^2)/(c0_beta+c1_beta*par[1]+c2_beta*par[1]^2+c3_beta*par[2]^2+c4_beta*par[1]*par[2]+c5_beta*par[1]^3+c6_beta*par[2]^3+c7_beta*par[1]^2*par[2]+c8_beta*par[1]*par[2]^2)))^2
     + ((mars_aug$b[i]-(c0_b+c1_b*par[1]+c2_b*par[2]+c3_b*par[1]^2+c4_b*par[2]^2+c5_b*par[1]*par[2]+c6_b*par[1]*par[2]^2))
        /((c0_b+c1_b*par[1]+c2_b*par[2]+c3_b*par[1]^2+c4_b*par[2]^2+c5_b*par[1]*par[2]+c6_b*par[1]*par[2]^2)/(c0_beta+c1_beta*par[1]+c2_beta*par[1]^2+c3_beta*par[2]^2+c4_beta*par[1]*par[2]+c5_beta*par[1]^3+c6_beta*par[2]^3+c7_beta*par[1]^2*par[2]+c8_beta*par[1]*par[2]^2)))^2
     + (mars_aug$beta[i]-(c0_beta+c1_beta*par[1]+c2_beta*par[1]^2+c3_beta*par[2]^2+c4_beta*par[1]*par[2]+c5_beta*par[1]^3+c6_beta*par[2]^3+c7_beta*par[1]^2*par[2]+c8_beta*par[1]*par[2]^2))^2)
  }
  
  mars_min<-nlminb(start=c(2,1), objective=f, lower=c(0,0), upper=c(2,1), control=list(trace=TRUE)) #always set "start" to your upper bound

  print(mars_min$objective)
  mars_Mg_calc <- append(mars_Mg_calc,mars_min$par[1])
  mars_Ca_calc <- append(mars_Ca_calc,mars_min$par[2])
  mars_Fe_calc <- (2-mars_Mg_calc-mars_Ca_calc)
  mars_sum_sq_error <- append(mars_sum_sq_error,mars_min$objective)
  
  mars_a_calc <- append(mars_a_calc, (c0_a+c1_a*mars_min$par[1]+c2_a*mars_min$par[2]+c3_a*mars_min$par[1]^2+c4_a*mars_min$par[1]*mars_min$par[2]+c5_a*mars_min$par[2]^3+c6_a*mars_min$par[1]*mars_min$par[2]^2))
  mars_b_calc <- append(mars_b_calc, (c0_b+c1_b*mars_min$par[1]+c2_b*mars_min$par[2]+c3_b*mars_min$par[1]^2+c4_b*mars_min$par[2]^2+c5_b*mars_min$par[1]*mars_min$par[2]+c6_b*mars_min$par[1]*mars_min$par[2]^2))
  mars_beta_calc <- append(mars_beta_calc, (c0_beta+c1_beta*mars_min$par[1]+c2_beta*mars_min$par[1]^2+c3_beta*mars_min$par[2]^2+c4_beta*mars_min$par[1]*mars_min$par[2]+c5_beta*mars_min$par[1]^3+c6_beta*mars_min$par[2]^3+c7_beta*mars_min$par[1]^2*mars_min$par[2]+c8_beta*mars_min$par[1]*mars_min$par[2]^2))
}

mars_aug$Mg_calc = mars_Mg_calc
mars_aug$Ca_calc = mars_Ca_calc
mars_aug$Fe_calc = mars_Fe_calc
mars_aug$Sum_sq_err = mars_sum_sq_error

mars_aug$mars_a_calc = mars_a_calc
mars_aug$mars_b_calc = mars_b_calc
mars_aug$mars_beta_calc = mars_beta_calc

#Computing RMSE of calculated composition
mars_a_calc_ResSq <- (mars_a_calc-mars_aug$a)^2
mars_b_calc_ResSq <- (mars_b_calc-mars_aug$b)^2
mars_beta_calc_ResSq <- (mars_beta_calc-mars_aug$beta)^2

RMSE_mars_a <- sqrt(mean(mars_a_calc_ResSq))
RMSE_mars_b <- sqrt(mean(mars_b_calc_ResSq))
RMSE_mars_beta <- sqrt(mean(mars_beta_calc_ResSq))

RMSE_mars_a
RMSE_mars_b
RMSE_mars_beta

###########################################################################################
#Cross-validation for a
ratio <- 0.80 #Training on 80% of data
mse_a <- c()

for (i in 1:1000) {
  train.ind <- sample(1:nrow(aug),nrow(aug)*ratio)
  
  train <- aug[train.ind,]
  test <- aug[-train.ind,]
  
  lm_aug_a_train =lm(a ~ Mg + Ca + I(Mg^2) + I(Mg*Ca) + I(Ca^3) + I(Mg*Ca^2), data = train)
  
  predicted <- predict(lm_aug_a_train,test)
  
  mse_a <- c(mse_a,mean((predicted-test$a)^2))
}

#average rmse over all runs
rmse_a_test <- sqrt(mean(mse_a))
rmse_a_test

#Cross-validation for b
mse_b <- c()

for (i in 1:1000) {
  train.ind <- sample(1:nrow(aug),nrow(aug)*ratio)
  
  train <- aug[train.ind,]
  test <- aug[-train.ind,]
  
  lm_aug_b_train =lm(b ~ Mg + Ca + I(Mg^2) + I(Ca^2) + I(Mg*Ca) + I(Mg*Ca^2), data = train)
  
  predicted <- predict(lm_aug_b_train,test)
  
  mse_b <- c(mse_b,mean((predicted-test$b)^2))
  
}

#average rmse over all runs
rmse_b_test <- sqrt(mean(mse_b))
rmse_b_test

#Cross-validation for beta
mse_beta <- c()

for (i in 1:1000) {
  train.ind <- sample(1:nrow(aug),nrow(aug)*ratio)
  
  train <- aug[train.ind,]
  test <- aug[-train.ind,]
  
  lm_aug_beta_train =lm(beta ~ Mg + I(Mg^2) + I(Ca^2) + I(Mg*Ca) + I(Mg^3) + I(Ca^3) + I(Mg^2*Ca) + I(Mg*Ca^2), data = train)
  
  predicted <- predict(lm_aug_beta_train,test)
  
  mse_beta <- c(mse_beta,mean((predicted-test$beta)^2))
  
}

#average rmse over all runs
rmse_beta_test <- sqrt(mean(mse_beta))
rmse_beta_test

#End cross-validation
################################################################################