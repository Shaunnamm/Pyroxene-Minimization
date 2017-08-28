#########################
# OPX Minimization.2017 #
#########################

#Read in earth opx dataset
opx=read.csv("/Users/smmorrison/Desktop/R/CheMin/opx_8.2017_avg.csv")

plot(Mg ~ a, data = opx)

#Runs every permutation of parameter combinations
# library(glmulti)
# multi_a <- glmulti(a ~ Mg + Ca + I(Ca^2) + I(Mg^2), data = opx,level = 1, )
# print(multi_a)
# 
# multi_b <- glmulti(b ~ Mg + Ca + I(Ca^2) + I(Mg^2), data = opx,level = 1, )
# print(multi_b)
# 
# multi_c <- glmulti(c ~ Mg + Ca + I(Ca^2) + I(Mg^2), data = opx,level = 1, )
# print(multi_c)
# 


#lm_x <- lm(x ~ Mg + Ca + I(Ca^2) + I(Mg^2), data = opx)

#Run lm to determine which parameters are significant. Use these coeff as the starting parameters in the nls. 
lm_a <- lm(a ~ Mg + Ca + I(Mg^2) + I(Ca^2), data = opx)
summary(lm_a)

lm_b <- lm(b ~ Mg + I(Mg^2), data = opx)
summary(lm_b)

# lm_c <- lm(c ~ Mg + Ca + I(Ca^2) + I(Mg^2), data = opx)
# summary(lm_c)

#Running nls with starting parameters updated to be a little below the parameters of the lm. 
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



# nls_c <- nls (c ~ c0 + c1*Mg + c2*Ca + c3*I(Mg^2) + c4*I(Ca^2),
#   data = opx, start = list(c0 = summary(lm_c)$coefficients[1,1], 
#                            c1 = summary(lm_c)$coefficients[2,1], 
#                            c2 = summary(lm_c)$coefficients[3,1], 
#                            c3 = summary(lm_c)$coefficients[4,1], 
#                            c4 = summary(lm_c)$coefficients[5,1]),
#   control = nls.control(warnOnly = T))
# summary(nls_c)


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

# {
#   c0_c <- summary(nls_c)$coefficients[1,1]
#   c1_c <- summary(nls_c)$coefficients[2,1]
#   c2_c <- summary(nls_c)$coefficients[3,1]
#   c3_c <- summary(nls_c)$coefficients[4,1]
#   c4_c <- summary(nls_c)$coefficients[5,1]
# }


a_calc <- predict(nls_a,opx)
b_calc <- predict(nls_b,opx)
# c_calc <- predict(nls_c,opx)

opx$a_calc = a_calc
opx$b_calc = b_calc
# opx$c_calc = c_calc

a_calc_ResSq <- (a_calc-opx$a)^2
b_calc_ResSq <- (b_calc-opx$b)^2
# c_calc_ResSq <- (c_calc-opx$c)^2

RMSE_a <- sqrt(mean(a_calc_ResSq))
RMSE_b <- sqrt(mean(b_calc_ResSq))
# RMSE_c <- sqrt(mean(c_calc_ResSq))

RMSE_a
RMSE_b
# RMSE_c

weight_a <- a_calc/b_calc
# weight_c <- c_calc/b_calc

#minimization
# par[1] = Mg
# par[2] = Ca
Mg_calc<-c()
Ca_calc<-c()
opx_all=read.csv("/Users/smmorrison/Desktop/R/CheMin/opx_8.2017_all.csv")
for (i in 1:nrow(opx_all)) {
  f <- function(par) { 
    (((((opx_all$a[i]-(c0_a+c1_a*par[1]+c2_a*par[2]+c3_a*par[1]^2+c4_a*par[2]^2)))/((c0_a+c1_a*par[1]+c2_a*par[2]+c3_a*par[1]^2+c4_a*par[2]^2)/(c0_b+c1_b*par[1]+c2_b*par[1]^2)))^2) 
     + ((opx_all$b[i]-(c0_b+c1_b*par[1]+c2_b*par[1]^2))^2))
    }
  min<-nlminb(start=c(2,1), objective=f, lower=c(0,0), upper=c(2,1), control=list(trace=TRUE)) #always set "start" to your upper bound
  
  
  Mg_calc <- append(Mg_calc,min$par[1])
  Ca_calc <- append(Ca_calc,min$par[2])
  Fe_calc <- (2-Mg_calc-Ca_calc)
  
}
# (((((opx_all$a[i]-(c0_a+c1_a*par[1]+c2_a*par[2]+c3_a*par[1]^2+c4_a*par[2]^2)))/weight_a[i])^2) + ((opx_all$b[i]-(c0_b+c1_b*par[1]+c2_b*par[1]^2))^2))

opx_all$Mg_calc = Mg_calc
opx_all$Ca_calc = Ca_calc
opx_all$Fe_calc = Fe_calc
# opx_all$sumsqerr = min$objective ###This adds a column with the

Ca_ResSq <- (Ca_calc-opx_all$Ca)^2
Fe_ResSq <- (Fe_calc-opx_all$Fe)^2
Mg_ResSq <- (Mg_calc-opx_all$Mg)^2

RMSE_Ca <- sqrt(mean(Ca_ResSq))
RMSE_Fe <- sqrt(mean(Fe_ResSq))  
RMSE_Mg <- sqrt(mean(Mg_ResSq))

RMSE_Ca
RMSE_Fe
RMSE_Mg


