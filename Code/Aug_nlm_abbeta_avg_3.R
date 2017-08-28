############################
# Augite Minimization.2017 #
############################

#Read in the earth augite dataset
aug=read.csv("/Users/smmorrison/Desktop/R/CheMin/Aug_8.2017_avg.csv")
plot(Mg ~ b, data=aug)

lm_a <- lm(a ~ Mg + Ca + I(Mg^2) + I(Mg*Ca) + I(Ca^3) + I(Mg*Ca^2), data = aug)
summary(lm_a)
lm_b <- lm(b ~ Mg + Ca + I(Mg^2) + I(Ca^2) + I(Mg*Ca) + I(Mg*Ca^2), data = aug)
summary(lm_b)
# lm_c <- lm(c ~ Mg + Ca + I(Mg^2) + I(Ca^2) + I(Mg^2*Ca) + I(Mg*Ca^2), data = aug)
# summary(lm_c)
lm_beta <- lm(beta ~ Mg + I(Mg^2) + I(Ca^2) + I(Mg*Ca) + I(Mg^3) + I(Ca^3) + I(Mg^2*Ca) + I(Mg*Ca^2), data = aug)
summary(lm_beta)

nls_a <- nls (a ~ c0 + c1*Mg + c2*Ca + c3*I(Mg^2) + c4*I(Mg*Ca) + c5*I(Ca^3) + c6*I(Mg*Ca^2), 
      data = aug, start = list(c0 = summary(lm_a)$coefficients[1,1], 
                               c1 = summary(lm_a)$coefficients[2,1], 
                               c2 = summary(lm_a)$coefficients[3,1], 
                               c3 = summary(lm_a)$coefficients[4,1], 
                               c4 = summary(lm_a)$coefficients[5,1], 
                               c5 = summary(lm_a)$coefficients[6,1],
                               c6 = summary(lm_a)$coefficients[7,1]), 
      control = nls.control(warnOnly = T))
nls_b <- nls (b ~ c0 + c1*Mg + c2*Ca + c3*I(Mg^2) + c4*I(Ca^2) + c5*I(Mg*Ca) + c6*I(Mg*Ca^2), 
      data = aug, start = list(c0 = summary(lm_b)$coefficients[1,1], 
                               c1 = summary(lm_b)$coefficients[2,1], 
                               c2 = summary(lm_b)$coefficients[3,1], 
                               c3 = summary(lm_b)$coefficients[4,1], 
                               c4 = summary(lm_b)$coefficients[5,1], 
                               c5 = summary(lm_b)$coefficients[6,1],
                               c6 = summary(lm_b)$coefficients[7,1]), 
      control = nls.control(warnOnly = T))
# nls_c <- nls (c ~ c0 + c1*Mg + c2*Ca + c3*I(Mg^2) + c4*I(Ca^2) + c5*I(Mg^2*Ca) + c6*I(Mg*Ca^2), 
#       data = aug, start = list(c0 = 5, c1 = 0.05, c2 = -0.1, c3 = -0.07, c4 = 0.08, c5 = 0.07, c6 = -0.07), 
#       control = nls.control(warnOnly = T))
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

# {
# c0_c <- summary(nls_c)$coefficients[1,1]
# c1_c <- summary(nls_c)$coefficients[2,1]
# c2_c <- summary(nls_c)$coefficients[3,1]
# c3_c <- summary(nls_c)$coefficients[4,1]
# c4_c <- summary(nls_c)$coefficients[5,1]
# c5_c <- summary(nls_c)$coefficients[6,1]
# c6_c <- summary(nls_c)$coefficients[7,1]
# }

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

a_calc <- predict(nls_a,aug)
b_calc <- predict(nls_b,aug)
# c_calc <- predict(nls_c,aug)
beta_calc <- predict(nls_beta,aug)

aug$a_calc = a_calc
aug$b_calc = b_calc
# aug$c_calc = c_calc
aug$beta_calc = beta_calc

a_calc_ResSq <- (a_calc-aug$a)^2
b_calc_ResSq <- (b_calc-aug$b)^2
# c_calc_ResSq <- (c_calc-aug$c)^2
beta_calc_ResSq <- (beta_calc-aug$beta)^2

RMSE_a <- sqrt(mean(a_calc_ResSq))
RMSE_b <- sqrt(mean(b_calc_ResSq))  
# RMSE_c <- sqrt(mean(c_calc_ResSq))  
RMSE_beta <- sqrt(mean(beta_calc_ResSq))

RMSE_a
RMSE_b
# RMSE_c
RMSE_beta
  
weight_a <- a_calc/beta_calc
weight_b <- b_calc/beta_calc
# weight_c <- c_calc/beta_calc


# par[1] = Mg
# par[2] = Ca
Mg_calc<-c()
Ca_calc<-c()
aug_all=read.csv("/Users/smmorrison/Desktop/R/CheMin/Aug_8.2017_all.csv")
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

}

aug_all$Mg_calc = Mg_calc
aug_all$Ca_calc = Ca_calc
aug_all$Fe_calc = Fe_calc
# aug_all$sumsqerr = min$objective ###This adds a column with the

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

#Calculate chemistry for martian samples
mars_aug=read.delim("/Users/smmorrison/Desktop/R/CheMin/mars_aug_2.txt")

# par[1] = Mg
# par[2] = Ca
mars_Mg_calc<-c()
mars_Ca_calc<-c()
for (i in 1:nrow(mars_aug)) {
  f <- function(par) { 
    (((mars_aug$a[i]-(c0_a+c1_a*par[1]+c2_a*par[1]^2+c3_a*par[1]*par[2]+c4_a*par[1]^3+c5_a*par[2]^3+c6_a*par[1]^2*par[2]+c7_a*par[1]*par[2]^2))/
        ((c0_a+c1_a*par[1]+c2_a*par[1]^2+c3_a*par[1]*par[2]+c4_a*par[1]^3+c5_a*par[2]^3+c6_a*par[1]^2*par[2]+c7_a*par[1]*par[2]^2)/(c0_beta+c1_beta*par[1]+c2_beta*par[2]+c3_beta*par[1]^2+c4_beta*par[1]*par[2]+c5_beta*par[1]^3+c6_beta*par[2]^3+c7_beta*par[1]^2*par[2])))^2
     + ((mars_aug$b[i]-(c0_b+c1_b*par[2]+c2_b*par[1]^2+c3_b*par[1]*par[2]+c4_b*par[1]^3+c5_b*par[2]^3+c6_b*par[1]^2*par[2]))/
          ((c0_b+c1_b*par[2]+c2_b*par[1]^2+c3_b*par[1]*par[2]+c4_b*par[1]^3+c5_b*par[2]^3+c6_b*par[1]^2*par[2])/(c0_beta+c1_beta*par[1]+c2_beta*par[2]+c3_beta*par[1]^2+c4_beta*par[1]*par[2]+c5_beta*par[1]^3+c6_beta*par[2]^3+c7_beta*par[1]^2*par[2])))^2
     + (mars_aug$beta[i]-(c0_beta+c1_beta*par[1]+c2_beta*par[2]+c3_beta*par[1]^2+c4_beta*par[1]*par[2]+c5_beta*par[1]^3+c6_beta*par[2]^3+c7_beta*par[1]^2*par[2]))^2)
  }
  
  mars_min<-nlminb(start=c(2,1), objective=f, lower=c(0,0), upper=c(2,1), control=list(trace=TRUE, iter.max=5000, step.min=100)) #always set "start" to your upper bound
  
  mars_Mg_calc <- append(mars_Mg_calc,mars_min$par[1])
  mars_Ca_calc <- append(mars_Ca_calc,mars_min$par[2])
  mars_Fe_calc <- (2-mars_Mg_calc-mars_Ca_calc)
  
}

mars_aug$Mg_calc = mars_Mg_calc
mars_aug$Ca_calc = mars_Ca_calc
mars_aug$Fe_calc = mars_Fe_calc

for (i in 1:nrow(mars_aug)){
mars_aug$mars_a_calc = (c0_a+c1_a*mars_aug$Mg_calc[i]+c2_a*mars_aug$Mg_calc[i]^2+c3_a*mars_aug$Mg_calc[i]*mars_aug$Ca_calc[i]+c4_a*mars_aug$Mg_calc[i]^3+c5_a*mars_aug$Ca_calc[i]^3+c6_a*mars_aug$Mg_calc[i]^2*mars_aug$Ca_calc[i]+c7_a*mars_aug$Mg_calc[i]*mars_aug$Ca_calc[i]^2)
mars_aug$mars_b_calc = (c0_b+c1_b*mars_aug$Ca_calc[i]+c2_b*mars_aug$Mg_calc[i]^2+c3_b*mars_aug$Mg_calc[i]*mars_aug$Ca_calc[i]+c4_b*mars_aug$Mg_calc[i]^3+c5_b*mars_aug$Ca_calc[i]^3+c6_b*mars_aug$Mg_calc[i]^2*mars_aug$Ca_calc[i])
mars_aug$mars_beta_calc = (c0_beta+c1_beta*mars_aug$Mg_calc[i]+c2_beta*mars_aug$Ca_calc[i]+c3_beta*mars_aug$Mg_calc[i]^2+c4_beta*mars_aug$Mg_calc[i]*mars_aug$Ca_calc[i]+c5_beta*mars_aug$Mg_calc[i]^3+c6_beta*mars_aug$Ca_calc[i]^3+c7_beta*mars_aug$Mg_calc[i]^2*mars_aug$Ca_calc[i])
}


# ##########################################################################################
# 
# #Cross-validation
# ratio <- 0.80 #Training on 80% of data
# RMSE_a_train <- c()
# RMSE_b_train <- c()
# RMSE_beta_train <- c()
# RMSE_Mg_train <- c()
# RMSE_Fe_train <- c()
# RMSE_Ca_train <- c()
# cross <- c()
# cross <- as.data.frame(cross)  
# 
# for (i in 1:1000) {
#   train.ind <- sample(1:nrow(aug),nrow(aug)*ratio)
#   
#   train <- aug[train.ind,]
#   test <- aug[-train.ind,]
#   
#   nls_a_train <- nls (a ~ c0 + c1*Mg + c2*I(Mg^2) + c3*I(Mg*Ca) + c4*I(Mg^3) + c5*I(Ca^3) + c6*I(Mg^2*Ca) + c7*I(Mg*Ca^2), 
#     data = train, start = list(c0 = 9, c1 = -0.5, c2 = 0.2, c3 = 0.4, c4 = -0.08, c5 = 0.03, c6 = -0.2, c7 = -0.3), 
#     control = nls.control(warnOnly = T))
#   
#   nls_b_train <- nls (b ~ c0 + c1*Ca + c2*I(Mg^2) + c3*I(Mg*Ca) + c4*I(Mg^3) + c5*I(Ca^3) + c6*I(Mg^2*Ca), 
#     data = train, start = list(c0 = 9, c1 = -0.08, c2 = -0.2, c3 = -0.08, c4 = 0.06, c5 = 0.02, c6 = 0.1), 
#     control = nls.control(warnOnly = T))
#   
#   nls_beta_train <- nls (beta ~ c0 + c1*Mg + c2*Ca + c3*I(Mg^2) + c4*I(Mg*Ca) + c5*I(Mg^3) + c6*I(Ca^3) + c7*I(Mg^2*Ca), 
#     data = train, start = list(c0 = 107, c1 = 11, c2 = -5, c3 = -11, c4 = -9, c5 = 2, c6 = 2, c7 = 5), 
#     control = nls.control(warnOnly = T))
#   
#   {
#     c0_a_train <- summary(nls_a_train)$coefficients[1,1]
#     c1_a_train <- summary(nls_a_train)$coefficients[2,1]
#     c2_a_train <- summary(nls_a_train)$coefficients[3,1]
#     c3_a_train <- summary(nls_a_train)$coefficients[4,1]
#     c4_a_train <- summary(nls_a_train)$coefficients[5,1]
#     c5_a_train <- summary(nls_a_train)$coefficients[6,1]
#     c6_a_train <- summary(nls_a_train)$coefficients[7,1]
#     c7_a_train <- summary(nls_a_train)$coefficients[8,1]
#   }
#   
#   {
#     c0_b_train <- summary(nls_b_train)$coefficients[1,1]
#     c1_b_train <- summary(nls_b_train)$coefficients[2,1]
#     c2_b_train <- summary(nls_b_train)$coefficients[3,1]
#     c3_b_train <- summary(nls_b_train)$coefficients[4,1]
#     c4_b_train <- summary(nls_b_train)$coefficients[5,1]
#     c5_b_train <- summary(nls_b_train)$coefficients[6,1]
#     c6_b_train <- summary(nls_b_train)$coefficients[7,1]
#   }
#   
#   {
#     c0_beta_train <- summary(nls_beta_train)$coefficients[1,1]
#     c1_beta_train <- summary(nls_beta_train)$coefficients[2,1]
#     c2_beta_train <- summary(nls_beta_train)$coefficients[3,1]
#     c3_beta_train <- summary(nls_beta_train)$coefficients[4,1]
#     c4_beta_train <- summary(nls_beta_train)$coefficients[5,1]
#     c5_beta_train <- summary(nls_beta_train)$coefficients[6,1]
#     c6_beta_train <- summary(nls_beta_train)$coefficients[7,1]
#     c7_beta_train <- summary(nls_beta_train)$coefficients[8,1]
#   }
#   
#   #Minimization
#   a_calc_train <- predict(nls_a_train,train)
#   b_calc_train <- predict(nls_b_train,train)
#   beta_calc_train <- predict(nls_beta_train,train)
#   
#   train$a_calc_train = a_calc_train
#   train$b_calc_train = b_calc_train
#   train$beta_calc_train = beta_calc_train
#   
#   #weights to scale a & b residuals to the beta magnitude
#   weight_a_train <- a_calc_train/beta_calc_train
#   weight_b_train <- b_calc_train/beta_calc_train
#   
#   # par[1] = Mg
#   # par[2] = Ca
#   Mg_calc_train<-c()
#   Ca_calc_train<-c()
#   for (i in 1:nrow(train)) {
#  i = 1
#        f <- function(par) { 
#       (((train$a[i]-(c0_a_train+c1_a_train*par[1]+c2_a_train*par[1]^2+c3_a_train*par[1]*par[2]+c4_a_train*par[1]^3+c5_a_train*par[2]^3+c6_a_train*par[1]^2*par[2]+c7_a_train*par[1]*par[2]^2))/weight_a_train[i])^2
#        + ((train$b[i]-(c0_b_train+c1_b_train*par[2]+c2_b_train*par[1]^2+c3_b_train*par[1]*par[2]+c4_b_train*par[1]^3+c5_b_train*par[2]^3+c6_b_train*par[1]^2*par[2]))/weight_b_train[i])^2
#        + (train$beta[i]-(c0_beta_train+c1_beta_train*par[1]+c2_beta_train*par[2]+c3_beta_train*par[1]^2+c4_beta_train*par[1]*par[2]+c5_beta_train*par[1]^3+c6_beta_train*par[2]^3+c7_beta_train*par[1]^2*par[2]))^2)
#     }
#     
#     min_train<-nlminb(start=c(2,1), objective=f, lower=c(0,0), upper=c(2,1), control=list(trace=TRUE, iter.max=5000, step.min=100)) #always set "start" to your upper bound
#     
#     Mg_calc_train <- append(Mg_calc_train,min_train$par[1])
#     Ca_calc_train <- append(Ca_calc_train,min_train$par[2])
#     Fe_calc_train <- (2-Mg_calc_train-Ca_calc_train)
#     
#   }
#   
#   cross = train
#   
#   cross$Mg_calc_train <- Mg_calc_train
#   cross$Ca_calc_train <- Ca_calc_train
#   cross$Fe_calc_train <- Fe_calc_train
#   
#   cross$Ca_ResSq <- (cross$Ca_calc-cross$Ca)^2
#   cross$Fe_ResSq <- (cross$Fe_calc-cross$Fe)^2
#   cross$Mg_ResSq <- (cross$Mg_calc-cross$Mg)^2
#   
#   RMSE_Ca_train <- c(RMSE_Ca_train,sqrt(mean(cross$Ca_ResSq)))
#   RMSE_Fe_train <- c(RMSE_Fe_train,sqrt(mean(cross$Fe_ResSq)))
#   RMSE_Mg_train <- c(RMSE_Mg_train,sqrt(mean(cross$Mg_ResSq)))
#   
#   cross$a_calc_train_ResSq <- (cross$a_calc_train-cross$a)^2
#   cross$b_calc_train_ResSq <- (cross$b_calc_train-cross$b)^2
#   cross$beta_calc_train_ResSq <- (cross$beta_calc_train-cross$beta)^2
#   
#   RMSE_a_train <- c(RMSE_a_train,sqrt(mean(cross$a_calc_train_ResSq)))
#   RMSE_b_train <- c(RMSE_b_train,sqrt(mean(cross$b_calc_train_ResSq)))
#   RMSE_beta_train <- c(RMSE_beta_train,sqrt(mean(cross$beta_calc_train_ResSq)))
#   
# }
# 
# RMSE_cross_Mg <- mean(RMSE_Mg_train)
# RMSE_cross_Ca <- mean(RMSE_Ca_train)
# RMSE_cross_Fe <- mean(RMSE_Fe_train)
# RMSE_cross_a <- mean(RMSE_a_train)
# RMSE_cross_b <- mean(RMSE_b_train)
# RMSE_cross_beta <- mean(RMSE_beta_train)
# 
# RMSE_cross_Mg 
# RMSE_cross_Ca 
# RMSE_cross_Fe 
# RMSE_cross_a 
# RMSE_cross_b 
# RMSE_cross_beta 
