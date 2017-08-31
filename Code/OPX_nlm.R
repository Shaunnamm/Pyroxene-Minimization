#########################
# OPX Minimization.2017 #
#########################

#Read in earth opx dataset
opx=read.csv("/Users/smmorrison/Desktop/R/CheMin/Data/opx_8.2017_avg.csv")

#Run linear models to compute starting parameters for non-linear modeling
lm_a <- lm(a ~ Mg + Ca + I(Mg^2) + I(Ca^2), data = opx)
summary(lm_a)

lm_b <- lm(b ~ Mg + I(Mg^2), data = opx)
summary(lm_b)

#Run non-linear models
nls_a <- nls (a ~ c0 + c1*Mg + c2*Ca + c3*I(Mg^2) + c4*I(Ca^2), 
  data = opx, start = list(c0 = summary(lm_a)$coefficients[1,1], 
                           c1 = summary(lm_a)$coefficients[2,1], 
                           c2 = summary(lm_a)$coefficients[3,1], 
                           c3 = summary(lm_a)$coefficients[4,1], 
                           c4 = summary(lm_a)$coefficients[5,1]), 
  control = nls.control(warnOnly = T))
summary(nls_a)

nls_b <- nls (b ~ c0 + c1*Mg + c2*I(Mg^2), 
  data = opx, start = list(c0 = summary(lm_b)$coefficients[1,1], 
                           c1 = summary(lm_b)$coefficients[2,1], 
                           c2 = summary(lm_b)$coefficients[3,1]), 
  control = nls.control(warnOnly = T))
summary(nls_b)


#save nls coefficients
{
  c0_a <- summary(nls_a)$coefficients[1,1]
  c1_a <- summary(nls_a)$coefficients[2,1]
  c2_a <- summary(nls_a)$coefficients[3,1]
  c3_a <- summary(nls_a)$coefficients[4,1]
  c4_a <- summary(nls_a)$coefficients[5,1]
}

{
  c0_b <- summary(nls_b)$coefficients[1,1]
  c1_b <- summary(nls_b)$coefficients[2,1]
  c2_b <- summary(nls_b)$coefficients[3,1]
}

#Predicting uc params with nls models
a_calc <- predict(nls_a,opx)
b_calc <- predict(nls_b,opx)

opx$a_calc = a_calc
opx$b_calc = b_calc

#Computing RMSE of calculated uc params
a_calc_ResSq <- (a_calc-opx$a)^2
b_calc_ResSq <- (b_calc-opx$b)^2

RMSE_a <- sqrt(mean(a_calc_ResSq))
RMSE_b <- sqrt(mean(b_calc_ResSq))

RMSE_a
RMSE_b

#Weighting a for minimization. 
weight_a <- a_calc/b_calc

#minimization
  # par[1] = Mg
  # par[2] = Ca
Mg_calc<-c()
Ca_calc<-c()
a_calc_all <- c()
b_calc_all <- c()
sum_sq_error <- c()
opx_all=read.csv("/Users/smmorrison/Desktop/R/CheMin/Data/opx_8.2017_all.csv")
for (i in 1:nrow(opx_all)) {
  f <- function(par) { 
    (((((opx_all$a[i]-(c0_a+c1_a*par[1]+c2_a*par[2]+c3_a*par[1]^2+c4_a*par[2]^2)))/((c0_a+c1_a*par[1]+c2_a*par[2]+c3_a*par[1]^2+c4_a*par[2]^2)/(c0_b+c1_b*par[1]+c2_b*par[1]^2)))^2) 
     + ((opx_all$b[i]-(c0_b+c1_b*par[1]+c2_b*par[1]^2))^2))
    }
  min<-nlminb(start=c(2,1), objective=f, lower=c(0,0), upper=c(2,1), control=list(trace=TRUE)) #always set "start" to your upper bound
  
  
  Mg_calc <- append(Mg_calc,min$par[1])
  Ca_calc <- append(Ca_calc,min$par[2])
  Fe_calc <- (2-Mg_calc-Ca_calc)
  sum_sq_error <- append(sum_sq_error,min$objective)
  a_calc_all <- append(a_calc_all, (c0_a+c1_a*min$par[1]+c2_a*min$par[2]+c3_a*min$par[1]^2+c4_a*min$par[2]^2))
  b_calc_all <- append(b_calc_all, (c0_b+c1_b*min$par[1]+c2_b*min$par[1]^2))
}

opx_all$a_calc_all = a_calc_all
opx_all$b_calc_all = b_calc_all

a_calc_all_ResSq <- (a_calc_all-opx_all$a)^2
b_calc_all_ResSq <- (b_calc_all-opx_all$b)^2

opx_all$a_calc_all_ResSq <- a_calc_all_ResSq
opx_all$b_calc_all_ResSq <- b_calc_all_ResSq

RMSE_a <- sqrt(mean(a_calc_all_ResSq))
RMSE_b <- sqrt(mean(b_calc_all_ResSq))

RMSE_a
RMSE_b

#Computing RMSE of calculated composition
opx_all$Mg_calc = Mg_calc
opx_all$Ca_calc = Ca_calc
opx_all$Fe_calc = Fe_calc

Ca_ResSq <- (Ca_calc-opx_all$Ca)^2
Fe_ResSq <- (Fe_calc-opx_all$Fe)^2
Mg_ResSq <- (Mg_calc-opx_all$Mg)^2

RMSE_Ca <- sqrt(mean(Ca_ResSq))
RMSE_Fe <- sqrt(mean(Fe_ResSq))  
RMSE_Mg <- sqrt(mean(Mg_ResSq))

RMSE_Ca
RMSE_Fe
RMSE_Mg

##########################################################################################

#Calculate chemistry for your dataset (Mars, in our case)
mars_opx=read.csv("/Users/smmorrison/Desktop/R/CheMin/Data/Mars/mars_opx.csv")

# par[1] = Mg
# par[2] = Ca
mars_Mg_calc<-c()
mars_Ca_calc<-c()
mars_sum_sq_error <- c()
mars_a_calc <- c()
mars_b_calc <- c()
for (i in 1:nrow(mars_opx)) {
  f <- function(par) { 
    (((((mars_opx$a[i]-(c0_a+c1_a*par[1]+c2_a*par[2]+c3_a*par[1]^2+c4_a*par[2]^2)))/((c0_a+c1_a*par[1]+c2_a*par[2]+c3_a*par[1]^2+c4_a*par[2]^2)/(c0_b+c1_b*par[1]+c2_b*par[1]^2)))^2) 
     + ((mars_opx$b[i]-(c0_b+c1_b*par[1]+c2_b*par[1]^2))^2))
  }

  mars_min<-nlminb(start=c(2,1), objective=f, lower=c(0,0), upper=c(2,1), control=list(trace=TRUE)) #always set "start" to your upper bound
  
  print(mars_min$objective)
  mars_Mg_calc <- append(mars_Mg_calc,mars_min$par[1])
  mars_Ca_calc <- append(mars_Ca_calc,mars_min$par[2])
  mars_Fe_calc <- (2-mars_Mg_calc-mars_Ca_calc)
  mars_sum_sq_error <- append(mars_sum_sq_error,mars_min$objective)
  
  mars_a_calc <- append(mars_a_calc, (c0_a+c1_a*mars_min$par[1]+c2_a*mars_min$par[2]+c3_a*mars_min$par[1]^2+c4_a*mars_min$par[2]^2))
  mars_b_calc <- append(mars_b_calc, (c0_b+c1_b*mars_min$par[1]+c2_b*mars_min$par[1]^2))
}

mars_opx$Mg_calc = mars_Mg_calc
mars_opx$Ca_calc = mars_Ca_calc
mars_opx$Fe_calc = mars_Fe_calc
mars_opx$Sum_sq_err = mars_sum_sq_error

mars_opx$mars_a_calc = mars_a_calc
mars_opx$mars_b_calc = mars_b_calc

#Computing RMSE of calculated composition
mars_a_calc_ResSq <- (mars_a_calc-mars_opx$a)^2
mars_b_calc_ResSq <- (mars_b_calc-mars_opx$b)^2

RMSE_mars_a <- sqrt(mean(mars_a_calc_ResSq))
RMSE_mars_b <- sqrt(mean(mars_b_calc_ResSq))

RMSE_mars_a
RMSE_mars_b

###########################################################################################
#Cross-validation for a
ratio <- 0.80 #Training on 80% of data
mse_a <- c()

for (i in 1:1000) {
  train.ind <- sample(1:nrow(opx),nrow(opx)*ratio)
  
  train <- opx[train.ind,]
  test <- opx[-train.ind,]
  
  lm_opx_a_train =lm(a ~ Mg + Ca + I(Mg^2) + I(Ca^2), data = train)
  
  predicted <- predict(lm_opx_a_train,test)
  
  mse_a <- c(mse_a,mean((predicted-test$a)^2))
  
}

#average rmse over all runs
rmse_a_test <- sqrt(mean(mse_a))
rmse_a_test

#Cross-validation for b
mse_b <- c()

for (i in 1:1000) {
  train.ind <- sample(1:nrow(opx),nrow(opx)*ratio)
  
  train <- opx[train.ind,]
  test <- opx[-train.ind,]
  
  lm_opx_b_train =lm(b ~ Mg + I(Mg^2), data = train)
  
  predicted <- predict(lm_opx_b_train,test)
  
  mse_b <- c(mse_b,mean((predicted-test$b)^2))
  
}

#average rmse over all runs
rmse_b_test <- sqrt(mean(mse_b))
rmse_b_test
#End cross-validation
################################################################################

