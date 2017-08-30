###########################
# Final Olivine 5.18.2017 #
###########################

#library(glmulti)
#glmulti(Mg ~ a + b + c, data = oliv,level = 1)

#Read in and attach olivine data
oliv=read.csv("/Users/smmorrison/Desktop/R/CheMin/Data/Olivine_8.2017_avg.csv")
plot (oliv$Fe,oliv$b)


#Linear model
options(digits=10)

lm_oliv_Mg=lm(Mg ~ b, data = oliv)
summary(lm_oliv_Mg)
lm_oliv_Fe=lm(Fe ~ b, data = oliv)
summary(lm_oliv_Fe)

#Compute RMSE on averaged data
RMSE_Mg <- sqrt(mean(residuals(lm_oliv_Mg)^2))
RMSE_Mg
RMSE_Fe <- sqrt(mean(residuals(lm_oliv_Fe)^2))
RMSE_Fe

#Compute RMSE on all data
oliv_all=read.delim ("/Users/smmorrison/Desktop/R/CheMin/Data/Olivine_8.2017_all.txt")

oliv_all$Mg_calc <- c(predict(lm_oliv_Mg, newdata = oliv_all))
oliv_all$Fe_calc <- c(predict(lm_oliv_Fe, newdata = oliv_all))

oliv_all$Mg_all_ResSq <- (oliv_all$Mg_calc-oliv_all$Mg)^2
oliv_all$Fe_all_ResSq <- (oliv_all$Fe_calc-oliv_all$Fe)^2

RMSE_Mg <- sqrt(mean(oliv_all$Mg_all_ResSq))
RMSE_Fe <- sqrt(mean(oliv_all$Fe_all_ResSq)) 

RMSE_Mg
RMSE_Fe


#cor(oliv[,-1]) #correlation matrix
#cov(oliv[,-1]) #covariance matrix
#sapply(oliv,mean) #average of each parameter in z
#plot(Mg,residuals(lm_oliv_Mg))
#plot(residuals(lm_oliv_Mg))
#hist(oliv$a)

################################################################################
#Cross-validation for Mg
ratio <- 0.80 #Training on 80% of data
mse_Mg <- c()

for (i in 1:1000) {
  train.ind <- sample(1:nrow(oliv),nrow(oliv)*ratio)
  
  train <- oliv[train.ind,]
  test <- oliv[-train.ind,]
  
  lm_oliv_Mg_train =lm(Mg ~ b, data = train)

  predicted <- predict(lm_oliv_Mg_train,test)
  
  mse_Mg <- c(mse_Mg,mean((predicted-test$Mg)^2))
  
}

#average rmse over all runs
rmse_Mg_test <- sqrt(mean(mse_Mg))
rmse_Mg_test

#Cross-validation for Fe
mse_Fe <- c()

for (i in 1:1000) {
  train.ind <- sample(1:nrow(oliv),nrow(oliv)*ratio)
  
  train <- oliv[train.ind,]
  test <- oliv[-train.ind,]
  
  lm_oliv_Fe_train =lm(Fe ~ b, data = train)
  
  predicted <- predict(lm_oliv_Fe_train,test)
  
  mse_Fe <- c(mse_Fe,mean((predicted-test$Fe)^2))
  
}

#average rmse over all runs
rmse_Fe_test <- sqrt(mean(mse_Fe))
rmse_Fe_test
#End cross-validation
################################################################################

#Calculate Mg & Fe of martian samples
mars_oliv=read.delim("/Users/smmorrison/Desktop/R/CheMin/Data/Mars/mars_oliv.txt")

mars_oliv$Mg_calc <- c(predict(lm_oliv_Mg, newdata = mars_oliv))
mars_oliv$Fe_calc <- c(predict(lm_oliv_Fe, newdata = mars_oliv))
mars_oliv$sum <- mars_oliv$Mg_calc+mars_oliv$Fe_calc
