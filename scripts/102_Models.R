################################################################################L
#
##file name: 102_Models.R
## Author: Haley Holiman
## Updated 8/8/2025
## Output: Occupancy models for Part 1 of TN Marshbirds paper :comparision of survey methods & models
################################################################################

library(tidyverse)
library(unmarked)
library(AICcmodavg)
library(boot)

#source("./scripts/101_DataPrep.R") 

#setwd("C:/Users/SIU856584167/OneDrive - Southern Illinois University/Marshbirds/final_draft")
#1.Single season occupancy models ----------------------------------------------

#Point count data from 2022 and 2023
#For objective 1 we are not including site or observations covs - see objective 2 for landscape analysis 

## Least Bittern ---------------------------------------------------------------

#set up unmarked framework 
umf_lebi <- unmarkedFrameOccu(y = as.matrix(enc_hist_pc$LEBI),
                               siteCovs = NULL,
                               obsCovs = NULL)
summary(umf_lebi)

occu.m1_lebi <- occu(~1 ~ 1, data = umf_lebi)
summary(occu.m1_lebi)

# To get real estimate of occupancy (with 95% CI)
predict(occu.m1_lebi, 
        newdata = data.frame(site = 1),
        type = "state")

# To get real estimate of detection (with 95% CI)
predict(occu.m1_lebi, 
        newdata = data.frame(site = 1),
        type = "det")

# Equivalent to inverse logit
est_psi_lebi_m1 <- boot::inv.logit(coef(occu.m1_lebi)[1]) # Real estimate of occupancy 
est_p_lebi_m1 <-boot::inv.logit(coef(occu.m1_lebi)[2]) # Real estimate of detection 

print(est_psi_lebi_m1) #93% occupancy
print(est_p_lebi_m1) #6.8% detection prob
# Calculate confidence intervals
summary(occu.m1_lebi)
ci_psi_lebi_m1 <- confint(occu.m1_lebi, type = "state")
print(ci_psi_lebi_m1) #-21.82544 27.04583

ci_p_lebi_m1 <- confint(occu.m1_lebi, type = "det")
print(ci_p_lebi_m1) #-4.476044 -0.7341094

###calculate coefficent of variation -------------------------------------------

lebi_values <- as.data.frame(unlist(enc_hist_pc$`LEBI`))

sapply(lebi_values, function(x) sd(x, na.rm=T) / mean(x, na.rm=T) * 100)
# cv = 382.92 % 


## American Coot ---------------------------------------------------------------
umf_amco <- unmarkedFrameOccu( y = as.matrix(enc_hist_pc$AMCO),
                               siteCovs = NULL,
                               obsCovs = NULL)
summary(umf_amco)

occu.m1_amco <- occu(~1 ~ 1, data = umf_amco)
summary(occu.m1_amco)


# Equivalent to inverse logit
est_psi_amco_m1 <- boot::inv.logit(coef(occu.m1_amco)[1]) # Real estimate of occupancy (40%)
est_p_amco_m1 <- boot::inv.logit(coef(occu.m1_amco)[2]) # Real estimate of detection (22%)

print(est_psi_amco_m1)
print(est_p_amco_m1)

ci_psi_amco_m1 <- confint(occu.m1_amco, type = "state")
print(ci_psi_amco_m1) # -1.678984 0.8401528

ci_p_amco_m1 <- confint(occu.m1_amco, type = "det")
print(ci_p_amco_m1) # -2.322494 -0.2619546

###calculate coefficent of variation --------------------------------------------
amco_values <- unlist(enc_hist_pc$AMCO)

cv_amco_pc <- sd(amco_values) / mean(amco_values) *100
print(cv_amco_pc) #327.80

## Pied-billed Grebe -----------------------------------------------------------
umf_pbgr <- unmarkedFrameOccu(y = as.matrix(enc_hist_pc$PBGR),
                              siteCovs = NULL,
                              obsCovs = NULL)
summary(umf_pbgr)

occu.m1_pbgr <- occu(~1 ~ 1, data = umf_pbgr)
summary(occu.m1_pbgr)

est_psi_pbgr_m1 <- boot::inv.logit(coef(occu.m1_pbgr)[1]) # Real estimate of occupancy (99.5%)
est_p_pbgr_m1 <- boot::inv.logit(coef(occu.m1_pbgr)[2]) # Real estimate of detection (2.1%)

# Calculate confidence intervals

ci_psi_pbgr_m1 <- confint(occu.m1_pbgr, type = "state")
print(ci_psi_pbgr_m1) # -78.03573 88.65364

ci_p_pbgr_m1 <- confint(occu.m1_pbgr, type = "det")
print(ci_p_pbgr_m1) # -4.798994 -2.838914

###calculate coefficent of variation --------------------------------------------
pbgr_values <- unlist(enc_hist_pc$PBGR)

cv_pbgr_pc <- sd(pbgr_values) / mean(pbgr_values) *100
print(cv_pbgr_pc) #678.2077


##Sora -------------------------------------------------------------------------
umf_sora <- unmarkedFrameOccu( y = enc_hist_pc$SORA,
                               siteCovs = NULL,
                               obsCovs = NULL)
summary(umf_sora)

occu.m1_sora <- occu(~1 ~ 1, data = umf_sora, control = list(maxit = 300))
summary(occu.m1_sora)

est_psi_sora_m1 <- boot::inv.logit(coef(occu.m1_sora[1])) # Real estimate of occupancy (50%)
est_p_sora_m1 <- boot::inv.logit(coef(occu.m1_sora)[2]) # Real estimate of detection (9.4%)

ci_psi_sora_m1 <- confint(occu.m1_sora, type = "state")
print(ci_psi_sora_m1) # -3.467839 3.47501

ci_p_sora_m1 <- confint(occu.m1_sora, type = "det")
print(ci_p_sora_m1) # -4.262915 -0.272173

###calculate coefficent of variation -------------------------------------------
sora_values <- unlist(enc_hist_pc$SORA)

cv_sora_pc <- sd(sora_values) / mean(sora_values) *100
print(cv_sora_pc) #451.2176

#2.Dynamic Occu Models ARUs ---------------------------------------------------------
#dynamic or multistate models for aru survey method
# where the primary periods are the survey periods (3, separated by 2 weeks),
##and seconday is the number of recording days (5 days)
## Least Bittern ---------------------------------------------------------------

#unmarked framework

umf_lebi_aru <- unmarkedMultFrame(y = enc_hist_aru$`Least Bittern`,
                                  numPrimary = 3,
                                  obsCovs = NULL,
                                  siteCovs = NULL,
                                  yearlySiteCovs = NULL)
summary(umf_lebi_aru)

#null model

occu.m2_lebi <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
                       pformula = ~ 1, data = umf_lebi_aru)
summary(occu.m2_lebi)

#estimates
est_psi_lebi_m2 <- boot::inv.logit(coef(occu.m2_lebi[1])) # Real estimate of occupancy (19%)
est_p_lebi_m2 <- boot::inv.logit(coef(occu.m2_lebi)[4]) # Real estimate of detection (41%)

# Calculate confidence intervals

ci_psi_lebi_m2 <- confint(occu.m2_lebi, type = "psi")
print(ci_psi_lebi_m2) # -2.068167 -0.8404473

ci_p_lebi_m2 <- confint(occu.m2_lebi, type = "det")
print(ci_p_lebi_m2) # -0.6741443 -0.05984412

est_col_lebi_m2 <- boot::inv.logit(coef(occu.m2_lebi)[2]) #colonization
est_ext_lebi_m2 <- boot::inv.logit(coef(occu.m2_lebi)[3]) #extinction
ci_col_lebi_m2 <- confint(occu.m2_lebi, type = "col")
print(ci_col_lebi_m2) # -2.508792 -1.34356

ci_ext_lebi_m2 <- confint(occu.m2_lebi, type = "ext")
print(ci_ext_lebi_m2) # -0.8641797 0.6692032

###calculate coefficent of variation -------------------------------------------
lebi_values <- as.data.frame(unlist(enc_hist_aru$`Least Bittern`))

sapply(lebi_values, function(x) sd(x, na.rm=T) / mean(x, na.rm=T) * 100)
# cv = 329.42 % 


## American Coot ---------------------------------------------------------------

umf_amco_aru <- unmarkedMultFrame(y = enc_hist_aru$`American Coot`,
                                  numPrimary = 3,
                                  obsCovs = NULL,
                                  siteCovs = NULL,
                                  yearlySiteCovs = NULL)
summary(umf_amco_aru)


occu.m2_amco <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
                       pformula = ~ 1, data = umf_amco_aru)
summary(occu.m2_amco)

# estimates
est_psi_amco_m2 <- boot::inv.logit(coef(occu.m2_amco[1])) # Real estimate of occupancy (29%)
est_p_amco_m2 <- boot::inv.logit(coef(occu.m2_amco)[4]) # Real estimate of detection (39%)

# Calculate confidence intervals

ci_psi_amco_m2 <- confint(occu.m2_amco, type = "psi")
print(ci_psi_amco_m2) # psi(Int) -1.506165 -0.2890029

ci_p_amco_m2 <- confint(occu.m2_amco, type = "det")
print(ci_p_amco_m2) # -0.9104069 0.02268293

est_col_amco_m2 <- boot::inv.logit(coef(occu.m2_amco)[2]) #colonization
est_ext_amco_m2 <- boot::inv.logit(coef(occu.m2_amco)[3]) #extinction
ci_col_amco_m2 <- confint(occu.m2_amco, type = "col")
print(ci_col_amco_m2) # -5.767739 -2.26258

ci_ext_amco_m2 <- confint(occu.m2_amco, type = "ext")
print(ci_ext_amco_m2) # 0.5411393 3.490705

###calculate coefficent of variation -------------------------------------------
amco_values <- as.data.frame(unlist(enc_hist_aru$`American Coot`))

sapply(amco_values, function(x) sd(x, na.rm=T) / mean(x, na.rm=T) * 100)
# cv = 450.5692 higher than PC



## Pied-billed Grebe -----------------------------------------------------------

umf_pbgr_aru <- unmarkedMultFrame(y = enc_hist_aru$`Pied-billed Grebe`,
                                  numPrimary = 3)
summary(umf_pbgr_aru)

occu.m2_pbgr <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
                       pformula = ~ 1, data = umf_pbgr_aru)
summary(occu.m2_pbgr)

# Equivalent to inverse logit
est_psi_pbgr_m2 <- boot::inv.logit(coef(occu.m2_pbgr[1])) # Real estimate of occupancy (32%)
est_p_pbgr_m2 <- boot::inv.logit(coef(occu.m2_pbgr)[4]) # Real estimate of detection (35%)

# Calculate confidence intervals

ci_psi_pbgr_m2 <- confint(occu.m2_pbgr, type = "psi")
print(ci_psi_pbgr_m2) # psi(Int) -1.273134 -0.2046582

ci_p_pbgr_m2 <- confint(occu.m2_pbgr, type = "det")
print(ci_p_pbgr_m2) # -0.9410453 -0.2559701

est_col_pbgr_m2 <- boot::inv.logit(coef(occu.m2_pbgr)[2]) #colonization
est_ext_pbgr_m2 <- boot::inv.logit(coef(occu.m2_pbgr)[3]) #extinction
ci_col_pbgr_m2 <- confint(occu.m2_pbgr, type = "col")
print(ci_col_pbgr_m2) # -3.277614 -1.734332

ci_ext_pbgr_m2 <- confint(occu.m2_pbgr, type = "ext")
print(ci_ext_pbgr_m2) # 0.0973871 1.765576

###calculate coefficent of variation -------------------------------------------
pbgr_values <- as.data.frame(unlist(enc_hist_aru$`Pied-billed Grebe`))

sapply(pbgr_values, function(x) sd(x, na.rm=T) / mean(x, na.rm=T) * 100)
# cv = 371.748

## Sora ------------------------------------------------------------------------

umf_sora_aru <- unmarkedMultFrame(y = enc_hist_aru$Sora,
                                  numPrimary = 3)
summary(umf_sora_aru)



occu.m2_sora <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
                       pformula = ~ 1, data = umf_sora_aru)
summary(occu.m2_sora)

# Equivalent to inverse logit
est_psi_sora_m2 <- boot::inv.logit(coef(occu.m2_sora[1])) # Real estimate of occupancy (37%)
est_p_sora_m2 <- boot::inv.logit(coef(occu.m2_sora)[4]) # Real estimate of detection (46%)

# Calculate confidence intervals

ci_psi_sora_m2 <- confint(occu.m2_sora, type = "psi")
print(ci_psi_sora_m2) #-1.049537 -0.02516148

ci_p_sora_m2 <- confint(occu.m2_sora, type = "det")
print(ci_p_sora_m2) # -0.4712441 0.1365266

est_col_sora_m2 <- boot::inv.logit(coef(occu.m2_sora)[2]) #colonization
est_ext_sora_m2 <- boot::inv.logit(coef(occu.m2_sora)[3]) #extinction
ci_col_sora_m2 <- confint(occu.m2_sora, type = "col")
print(ci_col_sora_m2) # -2.235381 -1.113537

ci_ext_sora_m2 <- confint(occu.m2_sora, type = "ext")
print(ci_ext_sora_m2) # 1.471512 5.448621

###calculate coefficent of variation -------------------------------------------
sora_values <- as.data.frame(unlist(enc_hist_aru$Sora))

sapply(sora_values, function(x) sd(x, na.rm=T) / mean(x, na.rm=T) * 100)
# cv = 310.394
#6.Dynamic ARU + PC Occu Models ------------------------------------------------

## Least Bittern ---------------------------------------------------------------

umf_lebi_comb<- unmarkedMultFrame(y = enc_hist_comb$`Least Bittern`,
                                  numPrimary = 3)
summary(umf_lebi_comb)

occu.m3_lebi <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
                       pformula = ~  1, data = umf_lebi_comb)
summary(occu.m3_lebi)

# Equivalent to inverse logit
est_psi_lebi_m3 <- boot::inv.logit(coef(occu.m3_lebi[1])) # Real estimate of occupancy (20%)
est_p_lebi_m3 <- boot::inv.logit(coef(occu.m3_lebi)[4]) # Real estimate of detection (41%)

# Calculate confidence intervals

ci_psi_lebi_m3 <- confint(occu.m3_lebi, type = "psi")
print(ci_psi_lebi_m3) #-1.96219 -0.7681117

ci_p_lebi_m3 <- confint(occu.m3_lebi, type = "det")
print(ci_p_lebi_m3) # -0.6659256 -0.06736386

est_col_lebi_m3 <- boot::inv.logit(coef(occu.m3_lebi)[2]) #colonization
est_ext_lebi_m3 <- boot::inv.logit(coef(occu.m3_lebi)[3]) #extinction
ci_col_lebi_m3 <- confint(occu.m3_lebi, type = "col")
print(ci_col_lebi_m3) #-2.421391 -1.270133

ci_ext_lebi_m3 <- confint(occu.m3_lebi, type = "ext")
print(ci_ext_lebi_m3) #-0.9118449 0.6271194

###calculate coefficent of variation -------------------------------------------
lebi_values <- as.data.frame(unlist(enc_hist_comb$`Least Bittern`))

sapply(lebi_values, function(x) sd(x, na.rm=T) / mean(x, na.rm=T) * 100)
# cv = 316.83 % lower than pc + aru, but still high variability
## American Coot ---------------------------------------------------------------

umf_amco_comb<- unmarkedMultFrame(y = enc_hist_comb$`American Coot`,
                                  numPrimary = 3)
summary(umf_amco_comb)

occu.m3_amco <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
                       pformula = ~  1, data = umf_amco_comb)
summary(occu.m3_amco)


# Equivalent to inverse logit
est_psi_amco_m3 <- boot::inv.logit(coef(occu.m3_amco[1])) # Real estimate of occupancy (40%)
est_p_amco_m3 <- boot::inv.logit(coef(occu.m3_amco)[4]) # Real estimate of detection (35%)

# Calculate confidence intervals

ci_psi_amco_m3 <- confint(occu.m3_amco, type = "psi")
print(ci_psi_amco_m3) # psi(Int) -0.9904462 0.1402492

ci_p_amco_m3 <- confint(occu.m3_amco, type = "det")
print(ci_p_amco_m3) # -0.9948055 -0.2297532

est_col_amco_m3 <- boot::inv.logit(coef(occu.m3_amco)[2]) #colonization
est_ext_amco_m3 <- boot::inv.logit(coef(occu.m3_amco)[3]) #extinction
ci_col_amco_m3 <- confint(occu.m3_amco, type = "col")
print(ci_col_amco_m3) #-37.24468 19.75753

ci_ext_amco_m3 <- confint(occu.m3_amco, type = "ext")
print(ci_ext_amco_m3) #0.2224111 1.953407

###calculate coefficent of variation -------------------------------------------
amco_values <- as.data.frame(unlist(enc_hist_comb$`American Coot`))

sapply(amco_values, function(x) sd(x, na.rm=T) / mean(x, na.rm=T) * 100)
# 


## Pied-billed Grebe -----------------------------------------------------------

umf_pbgr_comb <- unmarkedMultFrame(y = enc_hist_comb$`Pied-billed Grebe`,
                                   numPrimary = 3)
summary(umf_pbgr_comb)

occu.m3_pbgr <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
                       pformula = ~ 1, data = umf_pbgr_comb)
summary(occu.m3_pbgr)

# Equivalent to inverse logit
est_psi_pbgr_m3 <- boot::inv.logit(coef(occu.m3_pbgr[1])) # Real estimate of occupancy (32%)
est_p_pbgr_m3 <- boot::inv.logit(coef(occu.m3_pbgr)[4]) # Real estimate of detection (36%)

# Calculate confidence intervals

ci_psi_pbgr_m3 <- confint(occu.m3_pbgr, type = "psi")
print(ci_psi_pbgr_m3) # psi(Int) -1.280372 -0.2176352

ci_p_pbgr_m3 <- confint(occu.m3_pbgr, type = "det")
print(ci_p_pbgr_m3) # -0.9077541 -0.2287076

est_col_pbgr_m3 <- boot::inv.logit(coef(occu.m3_pbgr)[2]) #colonization
est_ext_pbgr_m3 <- boot::inv.logit(coef(occu.m3_pbgr)[3]) #extinction
ci_col_pbgr_m3 <- confint(occu.m3_pbgr, type = "col")
print(ci_col_pbgr_m3) #-3.272091 -1.741239

ci_ext_pbgr_m3 <- confint(occu.m3_pbgr, type = "ext")
print(ci_ext_pbgr_m3) #0.1088215 1.772788

###calculate coefficent of variation -------------------------------------------
pbgr_values <- as.data.frame(unlist(enc_hist_comb$`Pied-billed Grebe`))

sapply(pbgr_values, function(x) sd(x, na.rm=T) / mean(x, na.rm=T) * 100)
#369.2161 

## Sora ------------------------------------------------------------------------

umf_sora_comb <- unmarkedMultFrame(y = enc_hist_comb$Sora,
                                   numPrimary = 3)
summary(umf_sora_comb)

occu.m3_sora <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
                       pformula = ~ 1, data = umf_sora_comb)
summary(occu.m3_sora)


# Equivalent to inverse logit
est_psi_sora_m3 <- boot::inv.logit(coef(occu.m3_sora[1])) # Real estimate of occupancy (44%)
est_p_sora_m3 <- boot::inv.logit(coef(occu.m3_sora)[4]) # Real estimate of detection (41%)

# Calculate confidence intervals

ci_psi_sora_m3 <- confint(occu.m3_sora, type = "psi")
print(ci_psi_sora_m3) # psi(Int) -0.7426101 0.292984

ci_p_sora_m3 <- confint(occu.m3_sora, type = "det")
print(ci_p_sora_m3) # -0.6740229 -0.09300575

est_col_sora_m3 <- boot::inv.logit(coef(occu.m3_sora)[2]) #colonization
est_ext_sora_m3 <- boot::inv.logit(coef(occu.m3_sora)[3]) #extinction
ci_col_sora_m3 <- confint(occu.m3_sora, type = "col")
print(ci_col_sora_m3) #-2.168589 -0.9978959

ci_ext_sora_m3 <- confint(occu.m3_sora, type = "ext")
print(ci_ext_sora_m3) #0.9065824 2.804292

###calculate coefficent of variation -------------------------------------------
sora_values <- as.data.frame(unlist(enc_hist_comb$Sora))

sapply(sora_values, function(x) sd(x, na.rm=T) / mean(x, na.rm=T) * 100)
# cv = 295.3445
