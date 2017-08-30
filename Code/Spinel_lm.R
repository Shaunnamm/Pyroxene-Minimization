###############################
#   Spinel RMSE calculation   #
#              &              #
#      Cross-validation       #
#     S.M. Morrison 2017      #
###############################


#Read in spinel data
spinel_Fe=read.delim("/Users/smmorrison/Desktop/R/CheMin/Data/Spinel/Spinel_Fe_avg.txt")
spinel_FeMg=read.delim("/Users/smmorrison/Desktop/R/CheMin/Data/Spinel/Spinel_FeMg_avg.txt")
spinel_FeTi=read.delim("/Users/smmorrison/Desktop/R/CheMin/Data/Spinel/Spinel_FeTi_avg.txt")
spinel_FeAl=read.delim("/Users/smmorrison/Desktop/R/CheMin/Data/Spinel/Spinel_FeAl.txt")
spinel_FeNi=read.delim("/Users/smmorrison/Desktop/R/CheMin/Data/Spinel/Spinel_FeNi.txt")
spinel_FeZn=read.delim("/Users/smmorrison/Desktop/R/CheMin/Data/Spinel/Spinel_FeZn.txt")
spinel_FeMgCr=read.delim("/Users/smmorrison/Desktop/R/CheMin/Data/Spinel/Spinel_FeMgCr.txt")
spinel_FeMnTi=read.delim("/Users/smmorrison/Desktop/R/CheMin/Data/Spinel/Spinel_FeMnTi.txt")
spinel_FeMgTi=read.delim("/Users/smmorrison/Desktop/R/CheMin/Data/Spinel/Spinel_FeMgTi.txt")
spinel_FeMgAl=read.delim("/Users/smmorrison/Desktop/R/CheMin/Data/Spinel/Spinel_FeMgAl.txt")
spinel_FeAlVac=read.delim("/Users/smmorrison/Desktop/R/CheMin/Data/Spinel/Spinel_FeAlVac_avg.txt")

#Linear models
lm_Fe <- lm(Fe ~ a, data = spinel_Fe)
lm_FeMg <- lm(Fe ~ a, data = spinel_FeMg)
lm_FeTi <- lm(Fe ~ a, data = spinel_FeTi)
lm_FeAl <- lm(Fe ~ a, data = spinel_FeAl)
lm_FeNi <- lm(Fe ~ a, data = spinel_FeNi)
lm_FeZn <- lm(Fe ~ a, data = spinel_FeZn)

lm_FeMgCr_Fe <- lm(Fe ~ a, data = spinel_FeMgCr)
lm_FeMgCr_Mg <- lm(Mg ~ a, data = spinel_FeMgCr)

lm_FeMnTi_Fe <- lm(Fe ~ a, data = spinel_FeMnTi)
lm_FeMnTi_Mn <- lm(Mn ~ a, data = spinel_FeMnTi)

lm_FeMgTi_Fe <- lm(Fe ~ a, data = spinel_FeMgTi)
lm_FeMgTi_Mg <- lm(Mg ~ a, data = spinel_FeMgTi)

lm_FeMgAl_Fe <- lm(Fe ~ a, data = spinel_FeMgAl)
lm_FeMgAl_Mg <- lm(Mg ~ a, data = spinel_FeMgAl)

lm_FeAlVac_Fe <- lm(Fe ~ a, data = spinel_FeAlVac)
lm_FeAlVac_Al <- lm(Al ~ a, data = spinel_FeAlVac)

#Add results from lm to your original dataframe
spinel_Fe$Fe_calc <- c(predict(lm_Fe, newdata = spinel_Fe))
spinel_FeMg$Fe_calc <- c(predict(lm_FeMg, newdata = spinel_FeMg))
spinel_FeMg$Mg_calc <- c(3 - c(predict(lm_FeMg, newdata = spinel_FeMg)))
spinel_FeTi$Fe_calc <- c(predict(lm_FeTi, newdata = spinel_FeTi))
spinel_FeTi$Ti_calc <- c(3 - c(predict(lm_FeTi, newdata = spinel_FeTi)))
spinel_FeAl$Fe_calc <- c(predict(lm_FeAl, newdata = spinel_FeAl))
spinel_FeAl$Al_calc <- c(3 - c(predict(lm_FeAl, newdata = spinel_FeAl)))
spinel_FeNi$Fe_calc <- c(predict(lm_FeNi, newdata = spinel_FeNi))
spinel_FeNi$Ni_calc <- c(3 - c(predict(lm_FeNi, newdata = spinel_FeNi)))
spinel_FeZn$Fe_calc <- c(predict(lm_FeZn, newdata = spinel_FeZn))
spinel_FeZn$Zn_calc <- c(3 - c(predict(lm_FeZn, newdata = spinel_FeZn)))
spinel_FeMgCr$Fe_calc <- c(predict(lm_FeMgCr_Fe, newdata = spinel_FeMgCr))
spinel_FeMgCr$Mg_calc <- c(predict(lm_FeMgCr_Mg, newdata = spinel_FeMgCr))
spinel_FeMgCr$Cr_calc <- c(3 - c(predict(lm_FeMgCr_Fe, newdata = spinel_FeMgCr)) 
                           - c(predict(lm_FeMgCr_Mg, newdata = spinel_FeMgCr)))
spinel_FeMnTi$Fe_calc <- c(predict(lm_FeMnTi_Fe, newdata = spinel_FeMnTi))
spinel_FeMnTi$Mn_calc <- c(predict(lm_FeMnTi_Mn, newdata = spinel_FeMnTi))
spinel_FeMnTi$Ti_calc <- c(3 - c(predict(lm_FeMnTi_Fe, newdata = spinel_FeMnTi)) 
                           - c(predict(lm_FeMnTi_Mn, newdata = spinel_FeMnTi)))
spinel_FeMgTi$Fe_calc <- c(predict(lm_FeMgTi_Fe, newdata = spinel_FeMgTi))
spinel_FeMgTi$Mg_calc <- c(predict(lm_FeMgTi_Mg, newdata = spinel_FeMgTi))
spinel_FeMgTi$Ti_calc <- c(3 - c(predict(lm_FeMgTi_Fe, newdata = spinel_FeMgTi)) 
                           - c(predict(lm_FeMgTi_Mg, newdata = spinel_FeMgTi)))
spinel_FeMgAl$Fe_calc <- c(predict(lm_FeMgAl_Fe, newdata = spinel_FeMgAl))
spinel_FeMgAl$Mg_calc <- c(predict(lm_FeMgAl_Mg, newdata = spinel_FeMgAl))
spinel_FeMgAl$Al_calc <- c(3 - c(predict(lm_FeMgAl_Fe, newdata = spinel_FeMgAl)) 
                           - c(predict(lm_FeMgAl_Mg, newdata = spinel_FeMgAl)))
spinel_FeAlVac$Fe_calc <- c(predict(lm_FeAlVac_Fe, newdata = spinel_FeAlVac))
spinel_FeAlVac$Al_calc <- c(predict(lm_FeAlVac_Al, newdata = spinel_FeAlVac))
spinel_FeAlVac$Vac_calc <- c(3 - c(predict(lm_FeAlVac_Fe, newdata = spinel_FeAlVac)) 
                           - c(predict(lm_FeAlVac_Al, newdata = spinel_FeAlVac)))

#Calculating errors
Spinel_Errors <- data.frame(1)

Spinel_Errors$RMSE_Fe <- sqrt(mean(residuals(lm_Fe)^2))
Spinel_Errors$RMSE_FeMg <- sqrt(mean(residuals(lm_FeMg)^2))
Spinel_Errors$RMSE_FeTi <- sqrt(mean(residuals(lm_FeTi)^2))
Spinel_Errors$RMSE_FeAl <- sqrt(mean(residuals(lm_FeAl)^2))
Spinel_Errors$RMSE_FeNi <- sqrt(mean(residuals(lm_FeNi)^2))
Spinel_Errors$RMSE_FeZn <- sqrt(mean(residuals(lm_FeZn)^2))
Spinel_Errors$RMSE_FeMgCr_Fe <- sqrt(mean(residuals(lm_FeMgCr_Fe)^2))
Spinel_Errors$RMSE_FeMgCr_Mg <- sqrt(mean(residuals(lm_FeMgCr_Mg)^2))
Spinel_Errors$RMSE_FeMnTi_Fe <- sqrt(mean(residuals(lm_FeMnTi_Fe)^2))
Spinel_Errors$RMSE_FeMnTi_Mn <- sqrt(mean(residuals(lm_FeMnTi_Mn)^2))
Spinel_Errors$RMSE_FeMgTi_Fe <- sqrt(mean(residuals(lm_FeMgTi_Fe)^2))
Spinel_Errors$RMSE_FeMgTi_Mg <- sqrt(mean(residuals(lm_FeMgTi_Mg)^2))
Spinel_Errors$RMSE_FeMgAl_Fe <- sqrt(mean(residuals(lm_FeMgAl_Fe)^2))
Spinel_Errors$RMSE_FeMgAl_Mg <- sqrt(mean(residuals(lm_FeMgAl_Mg)^2))
Spinel_Errors$RMSE_FeAlVac_Fe <- sqrt(mean(residuals(lm_FeAlVac_Fe)^2))
Spinel_Errors$RMSE_FeAlVac_Al <- sqrt(mean(residuals(lm_FeAlVac_Al)^2))

#Cross-validation
ratio <- 0.80 #Training on 80% of data
Spinel_Errors_Cross <- data.frame(1)

RMSE_Fe <- c()
for (i in 1:1000) {
  train.ind <- sample(1:nrow(spinel_Fe),nrow(spinel_Fe)*ratio)
  train <- spinel_Fe[train.ind,]
  test <- spinel_Fe[-train.ind,]
  lm_Fe_train=lm(Fe ~ a, data = train)
  predicted <- predict(lm_Fe_train,test)
  RMSE_Fe <- c(RMSE_Fe,sqrt(mean((predicted-test$Fe)^2)))
}

RMSE_FeMg <- c()
for (i in 1:1000) {
  train.ind <- sample(1:nrow(spinel_FeMg),nrow(spinel_FeMg)*ratio)
  train <- spinel_FeMg[train.ind,]
  test <- spinel_FeMg[-train.ind,]
  lm_FeMg_train=lm(Fe ~ a, data = train)
  predicted <- predict(lm_FeMg_train,test)
  RMSE_FeMg <- c(RMSE_FeMg,sqrt(mean((predicted-test$Fe)^2)))
}

RMSE_FeTi <- c()
for (i in 1:1000) {
  train.ind <- sample(1:nrow(spinel_FeTi),nrow(spinel_FeTi)*ratio)
  train <- spinel_FeTi[train.ind,]
  test <- spinel_FeTi[-train.ind,]
  lm_FeTi_train=lm(Fe ~ a, data = train)
  predicted <- predict(lm_FeTi_train,test)
  RMSE_FeTi <- c(RMSE_FeTi,sqrt(mean((predicted-test$Fe)^2)))
}

RMSE_FeAl <- c()
for (i in 1:1000) {
  train.ind <- sample(1:nrow(spinel_FeAl),nrow(spinel_FeAl)*ratio)
  train <- spinel_FeAl[train.ind,]
  test <- spinel_FeAl[-train.ind,]
  lm_FeAl_train=lm(Fe ~ a, data = train)
  predicted <- predict(lm_FeAl_train,test)
  RMSE_FeAl <- c(RMSE_FeAl,sqrt(mean((predicted-test$Fe)^2)))
}

RMSE_FeNi <- c()
for (i in 1:1000) {
  train.ind <- sample(1:nrow(spinel_FeNi),nrow(spinel_FeNi)*ratio)
  train <- spinel_FeNi[train.ind,]
  test <- spinel_FeNi[-train.ind,]
  lm_FeNi_train=lm(Fe ~ a, data = train)
  predicted <- predict(lm_FeNi_train,test)
  RMSE_FeNi <- c(RMSE_FeNi,sqrt(mean((predicted-test$Fe)^2)))
}

RMSE_FeZn <- c()
for (i in 1:1000) {
  train.ind <- sample(1:nrow(spinel_FeZn),nrow(spinel_FeZn)*ratio)
  train <- spinel_FeZn[train.ind,]
  test <- spinel_FeZn[-train.ind,]
  lm_FeZn_train=lm(Fe ~ a, data = train)
  predicted <- predict(lm_FeZn_train,test)
  RMSE_FeZn <- c(RMSE_FeZn,sqrt(mean((predicted-test$Fe)^2)))
}

RMSE_FeMgCr_Fe <- c()
for (i in 1:1000) {
  train.ind <- sample(1:nrow(spinel_FeMgCr),nrow(spinel_FeMgCr)*ratio)
  train <- spinel_FeMgCr[train.ind,]
  test <- spinel_FeMgCr[-train.ind,]
  lm_FeMgCr_Fe_train=lm(Fe ~ a, data = train)
  predicted <- predict(lm_FeMgCr_Fe_train,test)
  RMSE_FeMgCr_Fe <- c(RMSE_FeMgCr_Fe,sqrt(mean((predicted-test$Fe)^2)))
}

RMSE_FeMgCr_Mg <- c()
for (i in 1:1000) {
  train.ind <- sample(1:nrow(spinel_FeMgCr),nrow(spinel_FeMgCr)*ratio)
  train <- spinel_FeMgCr[train.ind,]
  test <- spinel_FeMgCr[-train.ind,]
  lm_FeMgCr_Mg_train=lm(Mg ~ a, data = train)
  predicted <- predict(lm_FeMgCr_Mg_train,test)
  RMSE_FeMgCr_Mg <- c(RMSE_FeMgCr_Mg,sqrt(mean((predicted-test$Mg)^2)))
}

RMSE_FeMnTi_Fe <- c()
for (i in 1:1000) {
  train.ind <- sample(1:nrow(spinel_FeMnTi),nrow(spinel_FeMnTi)*ratio)
  train <- spinel_FeMnTi[train.ind,]
  test <- spinel_FeMnTi[-train.ind,]
  lm_FeMnTi_Fe_train=lm(Fe ~ a, data = train)
  predicted <- predict(lm_FeMnTi_Fe_train,test)
  RMSE_FeMnTi_Fe <- c(RMSE_FeMnTi_Fe,sqrt(mean((predicted-test$Fe)^2)))
}

RMSE_FeMnTi_Mn <- c()
for (i in 1:1000) {
  train.ind <- sample(1:nrow(spinel_FeMnTi),nrow(spinel_FeMnTi)*ratio)
  train <- spinel_FeMnTi[train.ind,]
  test <- spinel_FeMnTi[-train.ind,]
  lm_FeMnTi_Mn_train=lm(Mn ~ a, data = train)
  predicted <- predict(lm_FeMnTi_Mn_train,test)
  RMSE_FeMnTi_Mn <- c(RMSE_FeMnTi_Mn,sqrt(mean((predicted-test$Mn)^2)))
}

RMSE_FeMgTi_Fe <- c()
for (i in 1:1000) {
  train.ind <- sample(1:nrow(spinel_FeMgTi),nrow(spinel_FeMgTi)*ratio)
  train <- spinel_FeMgTi[train.ind,]
  test <- spinel_FeMgTi[-train.ind,]
  lm_FeMgTi_Fe_train=lm(Fe ~ a, data = train)
  predicted <- predict(lm_FeMgTi_Fe_train,test)
  RMSE_FeMgTi_Fe <- c(RMSE_FeMgTi_Fe,sqrt(mean((predicted-test$Fe)^2)))
}

RMSE_FeMgTi_Mg <- c()
for (i in 1:1000) {
  train.ind <- sample(1:nrow(spinel_FeMgTi),nrow(spinel_FeMgTi)*ratio)
  train <- spinel_FeMgTi[train.ind,]
  test <- spinel_FeMgTi[-train.ind,]
  lm_FeMgTi_Mg_train=lm(Mg ~ a, data = train)
  predicted <- predict(lm_FeMgTi_Mg_train,test)
  RMSE_FeMgTi_Mg <- c(RMSE_FeMgTi_Mg,sqrt(mean((predicted-test$Mg)^2)))
}

RMSE_FeMgAl_Fe <- c()
for (i in 1:1000) {
  train.ind <- sample(1:nrow(spinel_FeMgAl),nrow(spinel_FeMgAl)*ratio)
  train <- spinel_FeMgAl[train.ind,]
  test <- spinel_FeMgAl[-train.ind,]
  lm_FeMgAl_Fe_train=lm(Fe ~ a, data = train)
  predicted <- predict(lm_FeMgAl_Fe_train,test)
  RMSE_FeMgAl_Fe <- c(RMSE_FeMgAl_Fe,sqrt(mean((predicted-test$Fe)^2)))
}

RMSE_FeMgAl_Mg <- c()
for (i in 1:1000) {
  train.ind <- sample(1:nrow(spinel_FeMgAl),nrow(spinel_FeMgAl)*ratio)
  train <- spinel_FeMgAl[train.ind,]
  test <- spinel_FeMgAl[-train.ind,]
  lm_FeMgAl_Mg_train=lm(Mg ~ a, data = train)
  predicted <- predict(lm_FeMgAl_Mg_train,test)
  RMSE_FeMgAl_Mg <- c(RMSE_FeMgAl_Mg,sqrt(mean((predicted-test$Mg)^2)))
}

RMSE_FeAlVac_Fe <- c()
for (i in 1:1000) {
  train.ind <- sample(1:nrow(spinel_FeAlVac),nrow(spinel_FeAlVac)*ratio)
  train <- spinel_FeAlVac[train.ind,]
  test <- spinel_FeAlVac[-train.ind,]
  lm_FeAlVac_Fe_train=lm(Fe ~ a, data = train)
  predicted <- predict(lm_FeAlVac_Fe_train,test)
  RMSE_FeAlVac_Fe <- c(RMSE_FeAlVac_Fe,sqrt(mean((predicted-test$Fe)^2)))
}

RMSE_FeAlVac_Al <- c()
for (i in 1:1000) {
  train.ind <- sample(1:nrow(spinel_FeAlVac),nrow(spinel_FeAlVac)*ratio)
  train <- spinel_FeAlVac[train.ind,]
  test <- spinel_FeAlVac[-train.ind,]
  lm_FeAlVac_Al_train=lm(Al ~ a, data = train)
  predicted <- predict(lm_FeAlVac_Al_train,test)
  RMSE_FeAlVac_Al <- c(RMSE_FeAlVac_Al,sqrt(mean((predicted-test$Al)^2)))
}

#average rmse over all runs
Spinel_Errors_Cross$RMSE_Fe <- (mean(RMSE_Fe))
Spinel_Errors_Cross$RMSE_FeMg <- (mean(RMSE_FeMg))
Spinel_Errors_Cross$RMSE_FeTi <- (mean(RMSE_FeTi))
Spinel_Errors_Cross$RMSE_FeAl <- (mean(RMSE_FeAl))
Spinel_Errors_Cross$RMSE_FeNi <- (mean(RMSE_FeNi))
Spinel_Errors_Cross$RMSE_FeZn <- (mean(RMSE_FeZn))
Spinel_Errors_Cross$RMSE_FeMgCr_Fe <- (mean(RMSE_FeMgCr_Fe))
Spinel_Errors_Cross$RMSE_FeMgCr_Mg <- (mean(RMSE_FeMgCr_Mg))
Spinel_Errors_Cross$RMSE_FeMnTi_Fe <- (mean(RMSE_FeMnTi_Fe))
Spinel_Errors_Cross$RMSE_FeMnTi_Mn <- (mean(RMSE_FeMnTi_Mn))
Spinel_Errors_Cross$RMSE_FeMgTi_Fe <- (mean(RMSE_FeMgTi_Fe))
Spinel_Errors_Cross$RMSE_FeMgTi_Mg <- (mean(RMSE_FeMgTi_Mg))
Spinel_Errors_Cross$RMSE_FeMgAl_Fe <- (mean(RMSE_FeMgAl_Fe))
Spinel_Errors_Cross$RMSE_FeMgAl_Mg <- (mean(RMSE_FeMgAl_Mg))
Spinel_Errors_Cross$RMSE_FeAlVac_Fe <- (mean(RMSE_FeAlVac_Fe))
Spinel_Errors_Cross$RMSE_FeAlVac_Al <- (mean(RMSE_FeAlVac_Al))

#End cross-validation
########################################################################################

#Add errors from cross validation to Spinel_Errors data frame
Spinel_Errors <- rbind(Spinel_Errors,Spinel_Errors_Cross)
row.names(Spinel_Errors) <- c("Spinel RMSE", "Spinel RMSE Cross")
########################################################################################

#Calculate spinel compositions for your data (Mars, in our case)
mars_spinel=read.csv("/Users/smmorrison/Desktop/R/CheMin/Data/Mars/mars_spinel.csv")

mars_spinel$Fe_Fe_calc <- c(predict(lm_Fe, newdata=mars_spinel))
mars_spinel$FeMg_Fe_calc <- c(predict(lm_FeMg, newdata=mars_spinel))
mars_spinel$FeMg_Mg_calc <- c(3 - c(predict(lm_FeMg, newdata=mars_spinel)))
mars_spinel$FeTi_Fe_calc <- c(predict(lm_FeTi, newdata=mars_spinel))
mars_spinel$FeTi_Ti_calc <- c(3 - c(predict(lm_FeTi, newdata=mars_spinel)))
mars_spinel$FeAl_Fe_calc <- c(predict(lm_FeAl, newdata=mars_spinel))
mars_spinel$FeAl_Al_calc <- c(3 - c(predict(lm_FeAl, newdata=mars_spinel)))
mars_spinel$FeNi_Fe_calc <- c(predict(lm_FeNi, newdata=mars_spinel))
mars_spinel$FeNi_Ni_calc <- c(3 - c(predict(lm_FeNi, newdata=mars_spinel)))
mars_spinel$FeZn_Fe_calc <- c(predict(lm_FeZn, newdata=mars_spinel))
mars_spinel$FeZn_Zn_calc <- c(3 - c(predict(lm_FeZn, newdata=mars_spinel)))
mars_spinel$FeMgCr_Fe_calc <- c(predict(lm_FeMgCr_Fe, newdata=mars_spinel))
mars_spinel$FeMgCr_Mg_calc <- c(predict(lm_FeMgCr_Mg, newdata=mars_spinel))
mars_spinel$FeMgCr_Cr_calc <- c(3 - c(predict(lm_FeMgCr_Fe, newdata=mars_spinel)) 
                           - c(predict(lm_FeMgCr_Mg, newdata=mars_spinel)))
mars_spinel$FeMnTi_Fe_calc <- c(predict(lm_FeMnTi_Fe, newdata=mars_spinel))
mars_spinel$FeMnTi_Mn_calc <- c(predict(lm_FeMnTi_Mn, newdata=mars_spinel))
mars_spinel$FeMnTi_Ti_calc <- c(3 - c(predict(lm_FeMnTi_Fe, newdata=mars_spinel)) 
                           - c(predict(lm_FeMnTi_Mn, newdata=mars_spinel)))
mars_spinel$FeMgTi_Fe_calc <- c(predict(lm_FeMgTi_Fe, newdata=mars_spinel))
mars_spinel$FeMgTi_Mg_calc <- c(predict(lm_FeMgTi_Mg, newdata=mars_spinel))
mars_spinel$FeMgTi_Ti_calc <- c(3 - c(predict(lm_FeMgTi_Fe, newdata=mars_spinel)) 
                           - c(predict(lm_FeMgTi_Mg, newdata=mars_spinel)))
mars_spinel$FeMgAl_Fe_calc <- c(predict(lm_FeMgAl_Fe, newdata=mars_spinel))
mars_spinel$FeMgAl_Mg_calc <- c(predict(lm_FeMgAl_Mg, newdata=mars_spinel))
mars_spinel$FeMgAl_Al_calc <- c(3 - c(predict(lm_FeMgAl_Fe, newdata=mars_spinel)) 
                           - c(predict(lm_FeMgAl_Mg, newdata=mars_spinel)))
mars_spinel$FeAlVac_Fe_calc <- c(predict(lm_FeAlVac_Fe, newdata=mars_spinel))
mars_spinel$FeAlVac_Al_calc <- c(predict(lm_FeAlVac_Al, newdata=mars_spinel))
mars_spinel$FeAlVac_Vac_calc <- c(3 - c(predict(lm_FeAlVac_Fe, newdata=mars_spinel)) 
                             - c(predict(lm_FeAlVac_Al, newdata=mars_spinel)))

