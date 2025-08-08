library(tidyverse)
library(unmarked)
library(AICcmodavg)
library(boot)

source("./scripts/101_DataPrep.R") 

#setwd("C:/Users/SIU856584167/OneDrive - Southern Illinois University/Marshbirds/final_draft")
#1.Point count data ----------------------------------------------

#Point count data from 2022 and 2023
#For part 1 we are not including site or observations covs - see part 2 for landscape analysis 
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
print(cv_sora_pc) 

#2. ARU data ------------------------------------------------------------------
## Least Bittern ---------------------------------------------------------------

#unmarked framework
#this is formatted for dynamic, modify for single season
y_lebi <- as.data.frame(enc_hist_aru$`Least Bittern`)

y <- y_lebi %>%
  mutate(R1 = if_else(
    Occ1_R1 == 1 | Occ2_R1 == 1 | Occ3_R1 == 1 | Occ4_R1 == 1 | Occ5_R1 == 1,
    1, 0
  )) %>% 
  mutate(R2 = if_else(
    Occ1_R2 == 1 | Occ2_R2 == 1 | Occ3_R2 == 1 | Occ4_R2 == 1 | Occ5_R2 == 1,
    1, 0
  )) %>% 
  mutate(R3 = if_else(
    Occ1_R3 == 1 | Occ2_R3 == 1 | Occ3_R3 == 1 | Occ4_R3 == 1 | Occ5_R3 == 1,
    1, 0
  )) %>% 
  select(R1,R2,R3)
          
umf_lebi_aru <- unmarkedFrameOccu(y = y,
                                  obsCovs = NULL,
                                  siteCovs = NULL)
summary(umf_lebi_aru)

#null model

occu.m2_lebi <- occu(~1 ~ 1, data = umf_lebi_aru)
summary(occu.m2_lebi)

#estimates
(est_psi_lebi_m2 <- boot::inv.logit(coef(occu.m2_lebi[1]))) # Real estimate of occupancy (62%)
(est_p_lebi_m2 <- boot::inv.logit(coef(occu.m2_lebi)[2])) # Real estimate of detection (46%)

# Calculate confidence intervals

ci_psi_lebi_m2 <- confint(occu.m2_lebi, type = "state")
print(ci_psi_lebi_m2) 

ci_p_lebi_m2 <- confint(occu.m2_lebi, type = "det")
print(ci_p_lebi_m2) # update this


###calculate coefficent of variation -------------------------------------------
lebi_values <- as.data.frame(unlist(y))
sapply(lebi_values, function(x) sd(x, na.rm=T) / mean(x, na.rm=T) * 100)
# cv = 206.1905


## American Coot ---------------------------------------------------------------
y_amco <- as.data.frame(enc_hist_aru$`American Coot`)

y <- y_amco %>%
  mutate(R1 = if_else(
    Occ1_R1 == 1 | Occ2_R1 == 1 | Occ3_R1 == 1 | Occ4_R1 == 1 | Occ5_R1 == 1,
    1, 0
  )) %>% 
  mutate(R2 = if_else(
    Occ1_R2 == 1 | Occ2_R2 == 1 | Occ3_R2 == 1 | Occ4_R2 == 1 | Occ5_R2 == 1,
    1, 0
  )) %>% 
  mutate(R3 = if_else(
    Occ1_R3 == 1 | Occ2_R3 == 1 | Occ3_R3 == 1 | Occ5_R3 == 1,
    1, 0
  )) %>% 
  select(R1,R2,R3)

umf_amco_aru <- unmarkedFrameOccu(y = y,
                                  obsCovs = NULL,
                                  siteCovs = NULL)
summary(umf_amco_aru)


occu.m2_amco <- occu(~1 ~ 1, data = umf_amco_aru)
summary(occu.m2_amco)

# estimates
(est_psi_amco_m2 <- boot::inv.logit(coef(occu.m2_amco[1]))) # Real estimate of occupancy (99%)
(est_p_amco_m2 <- boot::inv.logit(coef(occu.m2_amco)[2])) # Real estimate of detection (17.5%)

# Calculate confidence intervals

ci_psi_amco_m2 <- confint(occu.m2_amco, type = "state")
print(ci_psi_amco_m2) 

ci_p_amco_m2 <- confint(occu.m2_amco, type = "det")
print(ci_p_amco_m2) # 

###calculate coefficent of variation -------------------------------------------
amco_values <- as.data.frame(unlist(y))

sapply(amco_values, function(x) sd(x, na.rm=T) / mean(x, na.rm=T) * 100)
# cv = 217.43 



## Pied-billed Grebe -----------------------------------------------------------
y_pbgr <- as.data.frame(enc_hist_aru$`Pied-billed Grebe`)

y <- y_pbgr %>%
  mutate(R1 = if_else(
    Occ1_R1 == 1 | Occ2_R1 == 1 | Occ3_R1 == 1 | Occ4_R1 == 1 | Occ5_R1 == 1,
    1, 0
  )) %>% 
  mutate(R2 = if_else(
    Occ1_R2 == 1 | Occ2_R2 == 1 | Occ3_R2 == 1 | Occ4_R2 == 1 | Occ5_R2 == 1,
    1, 0
  )) %>% 
  mutate(R3 = if_else(
    Occ1_R3 == 1 | Occ2_R3 == 1 | Occ3_R3 == 1 | Occ4_R3 == 1 | Occ5_R3 == 1,
    1, 0
  )) %>% 
  select(R1,R2,R3)
umf_pbgr_aru <- unmarkedFrameOccu(y = y)
summary(umf_pbgr_aru)

occu.m2_pbgr <- occu(~1 ~ 1, data = umf_pbgr_aru)
summary(occu.m2_pbgr)

# Equivalent to inverse logit
(est_psi_pbgr_m2 <- boot::inv.logit(coef(occu.m2_pbgr[1]))) # Real estimate of occupancy (99%)
(est_p_pbgr_m2 <- boot::inv.logit(coef(occu.m2_pbgr)[2])) # Real estimate of detection (29%)

# Calculate confidence intervals

ci_psi_pbgr_m2 <- confint(occu.m2_pbgr, type = "state")
print(ci_psi_pbgr_m2) # psi(Int) 

ci_p_pbgr_m2 <- confint(occu.m2_pbgr, type = "det")
print(ci_p_pbgr_m2) # 

###calculate coefficent of variation -------------------------------------------
pbgr_values <- as.data.frame(unlist(y))

sapply(pbgr_values, function(x) sd(x, na.rm=T) / mean(x, na.rm=T) * 100)
# cv = 156.49

## Sora ------------------------------------------------------------------------
y_sora <- as.data.frame(enc_hist_aru$`Sora`)

y <- y_sora %>%
  mutate(R1 = if_else(
    Occ1_R1 == 1 | Occ2_R1 == 1 | Occ3_R1 == 1 | Occ4_R1 == 1 | Occ5_R1 == 1,
    1, 0
  )) %>% 
  mutate(R2 = if_else(
    Occ1_R2 == 1 | Occ2_R2 == 1 | Occ3_R2 == 1 | Occ4_R2 == 1 | Occ5_R2 == 1,
    1, 0
  )) %>% 
  mutate(R3 = if_else(
    Occ1_R3 == 1 | Occ2_R3 == 1 | Occ3_R3 == 1 | Occ4_R3 == 1 | Occ5_R3 == 1,
    1, 0
  )) %>% 
  select(R1,R2,R3)
umf_sora_aru <- unmarkedFrameOccu(y = y)
summary(umf_sora_aru)



occu.m2_sora <- occu(~1~1, data = umf_sora_aru)
summary(occu.m2_sora)

# Equivalent to inverse logit
(est_psi_sora_m2 <- boot::inv.logit(coef(occu.m2_sora[1]))) # Real estimate of occupancy (99%)
(est_p_sora_m2 <- boot::inv.logit(coef(occu.m2_sora)[2])) # Real estimate of detection (25%)

# Calculate confidence intervals

ci_psi_sora_m2 <- confint(occu.m2_sora, type = "state")
print(ci_psi_sora_m2) 

ci_p_sora_m2 <- confint(occu.m2_sora, type = "det")
print(ci_p_sora_m2) 



###calculate coefficent of variation -------------------------------------------
sora_values <- as.data.frame(unlist(y))

sapply(sora_values, function(x) sd(x, na.rm=T) / mean(x, na.rm=T) * 100)


#3. ARU+PC data ------------------------------------------------------------------
## Least Bittern ---------------------------------------------------------------

#unmarked framework

y_lebi <- as.data.frame(enc_hist_comb$`Least Bittern`)

y <- y_lebi %>%
  mutate(R1 = if_else(
    Occ1_R1 == 1 | Occ2_R1 == 1 | Occ3_R1 == 1 | Occ4_R1 == 1 | Occ5_R1 == 1,
    1, 0
  )) %>% 
  mutate(R2 = if_else(
    Occ1_R2 == 1 | Occ2_R2 == 1 | Occ3_R2 == 1 | Occ4_R2 == 1 | Occ5_R2 == 1,
    1, 0
  )) %>% 
  mutate(R3 = if_else(
    Occ1_R3 == 1 | Occ2_R3 == 1 | Occ3_R3 == 1 | Occ4_R3 == 1 | Occ5_R3 == 1,
    1, 0
  )) %>% 
  select(R1,R2,R3)
umf_lebi_comb <- unmarkedFrameOccu(y = y,
                                  obsCovs = NULL,
                                  siteCovs = NULL)
summary(umf_lebi_comb)

#null model

occu.m3_lebi <- occu(~1 ~ 1, data = umf_lebi_comb)
summary(occu.m3_lebi)

#estimates
(est_psi_lebi_m3 <- boot::inv.logit(coef(occu.m3_lebi[1]))) # Real estimate of occupancy (19%)
(est_p_lebi_m3 <- boot::inv.logit(coef(occu.m3_lebi)[2])) # Real estimate of detection (41%)

# Calculate confidence intervals

ci_psi_lebi_m3 <- confint(occu.m3_lebi, type = "state")
print(ci_psi_lebi_m3) 

ci_p_lebi_m3 <- confint(occu.m3_lebi, type = "det")
print(ci_p_lebi_m3) 


###calculate coefficent of variation -------------------------------------------
lebi_values <- as.data.frame(unlist(y))

sapply(lebi_values, function(x) sd(x, na.rm=T) / mean(x, na.rm=T) * 100)

## American Coot ---------------------------------------------------------------

y_amco <- as.data.frame(enc_hist_comb$`American Coot`)

y <- y_amco %>%
  mutate(R1 = if_else(
    Occ1_R1 == 1 | Occ2_R1 == 1 | Occ3_R1 == 1 | Occ4_R1 == 1 | Occ5_R1 == 1,
    1, 0
  )) %>% 
  mutate(R2 = if_else(
    Occ1_R2 == 1 | Occ2_R2 == 1 | Occ3_R2 == 1 | Occ4_R2 == 1 | Occ5_R2 == 1,
    1, 0
  )) %>% 
  mutate(R3 = if_else(
    Occ1_R3 == 1 | Occ2_R3 == 1 | Occ3_R3 == 1 | Occ5_R3 == 1,
    1, 0
  )) %>% 
  select(R1,R2,R3)


umf_amco_comb <- unmarkedFrameOccu(y = y,
                                  obsCovs = NULL,
                                  siteCovs = NULL)
summary(umf_amco_comb)


occu.m3_amco <- occu(~1 ~ 1, data = umf_amco_comb)
summary(occu.m3_amco)

# estimates
est_psi_amco_m3 <- boot::inv.logit(coef(occu.m3_amco[1])) # Real estimate of occupancy (29%)
est_p_amco_m3 <- boot::inv.logit(coef(occu.m3_amco)[2]) # Real estimate of detection (39%)

# Calculate confidence intervals

ci_psi_amco_m3 <- confint(occu.m3_amco, type = "state")
print(ci_psi_amco_m3) # psi(Int) -1.506165 -0.2890029

ci_p_amco_m3 <- confint(occu.m3_amco, type = "det")
print(ci_p_amco_m3) # -0.9104069 0.02268293

###calculate coefficent of variation -------------------------------------------
amco_values <- as.data.frame(unlist(y))

sapply(amco_values, function(x) sd(x, na.rm=T) / mean(x, na.rm=T) * 100)




## Pied-billed Grebe -----------------------------------------------------------
y_pbgr <- as.data.frame(enc_hist_comb$`Pied-billed Grebe`)

y <- y_pbgr %>%
  mutate(R1 = if_else(
    Occ1_R1 == 1 | Occ2_R1 == 1 | Occ3_R1 == 1 | Occ4_R1 == 1 | Occ5_R1 == 1,
    1, 0
  )) %>% 
  mutate(R2 = if_else(
    Occ1_R2 == 1 | Occ2_R2 == 1 | Occ3_R2 == 1 | Occ4_R2 == 1 | Occ5_R2 == 1,
    1, 0
  )) %>% 
  mutate(R3 = if_else(
    Occ1_R3 == 1 | Occ2_R3 == 1 | Occ3_R3 == 1 | Occ4_R3 == 1 | Occ5_R3 == 1,
    1, 0
  )) %>% 
  select(R1,R2,R3)
umf_pbgr_comb <- unmarkedFrameOccu(y = y)
summary(umf_pbgr_comb)

occu.m3_pbgr <- occu(~1 ~ 1, data = umf_pbgr_comb)
summary(occu.m3_pbgr)

# Equivalent to inverse logit
est_psi_pbgr_m3 <- boot::inv.logit(coef(occu.m3_pbgr[1])) # Real estimate of occupancy (32%)
est_p_pbgr_m3 <- boot::inv.logit(coef(occu.m3_pbgr)[2]) # Real estimate of detection (35%)

# Calculate confidence intervals

ci_psi_pbgr_m3 <- confint(occu.m3_pbgr, type = "state")
print(ci_psi_pbgr_m3) # psi(Int) -1.273134 -0.2046582

ci_p_pbgr_m3 <- confint(occu.m3_pbgr, type = "det")
print(ci_p_pbgr_m3) # -0.9410453 -0.2559701

###calculate coefficient of variation -------------------------------------------
pbgr_values <- as.data.frame(unlist(y))

sapply(pbgr_values, function(x) sd(x, na.rm=T) / mean(x, na.rm=T) * 100)
# cv = 371.748

## Sora ------------------------------------------------------------------------
y_sora <- as.data.frame(enc_hist_comb$`Sora`)

y <- y_sora %>%
  mutate(R1 = if_else(
    Occ1_R1 == 1 | Occ2_R1 == 1 | Occ3_R1 == 1 | Occ4_R1 == 1 | Occ5_R1 == 1,
    1, 0
  )) %>% 
  mutate(R2 = if_else(
    Occ1_R2 == 1 | Occ2_R2 == 1 | Occ3_R2 == 1 | Occ4_R2 == 1 | Occ5_R2 == 1,
    1, 0
  )) %>% 
  mutate(R3 = if_else(
    Occ1_R3 == 1 | Occ2_R3 == 1 | Occ3_R3 == 1 | Occ4_R3 == 1 | Occ5_R3 == 1,
    1, 0
  )) %>% 
  select(R1,R2,R3)
umf_sora_comb <- unmarkedFrameOccu(y = y)
summary(umf_sora_comb)

occu.m3_sora <- occu(~1~1, data = umf_sora_comb)
summary(occu.m3_sora)

# Equivalent to inverse logit
est_psi_sora_m3 <- boot::inv.logit(coef(occu.m3_sora[1])) # Real estimate of occupancy (37%)
est_p_sora_m3 <- boot::inv.logit(coef(occu.m3_sora)[2]) # Real estimate of detection (46%)

# Calculate confidence intervals

ci_psi_sora_m3 <- confint(occu.m3_sora, type = "state")
print(ci_psi_sora_m3) 

ci_p_sora_m3 <- confint(occu.m3_sora, type = "det")
print(ci_p_sora_m3) 



###calculate coefficent of variation -------------------------------------------
sora_values <- as.data.frame(unlist(y))

sapply(sora_values, function(x) sd(x, na.rm=T) / mean(x, na.rm=T) * 100)
