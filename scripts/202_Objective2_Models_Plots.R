################################################################################L
# Occupancy Models For MB Project - Combined Years
## file name: 202_Objective2_Models_Plots.R
### 
#### Updated 4/29/2026
## description: Dynamic occu Models and plots for objective 2 of TN Marshbirds paper

################################################################################L
#1. LOAD DATA ------------------------------------------------------------------
source("./scripts/201_Objective2_DataPrep.R") 

library(unmarked)
library(AICcmodavg)
library(rempsyc)
library(flextable)
library(ggpubr)
library(MuMIn)


yearlySiteCovs$bout <- as.matrix(yearlySiteCovs$bout)

#2. LEBI -----------------------------------------------------------------------


umf_lebi_comb <- unmarkedMultFrame(y = enc_hist_comb$`Least Bittern`,
                                   numPrimary = 3,
                                   obsCovs = obsCov,
                                   siteCovs = as.data.frame(sitecovs),
                                   yearlySiteCovs = yearlySiteCovs)
summary(umf_lebi_comb)



##2.A Top detection model ------------------------------------------------------

# Run models
m.null <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
                 pformula = ~ 1, data = umf_lebi_comb)
# d.1 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
#               pformula = ~ recording_day, data = umf_lebi_comb)
d.2 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
              pformula = ~ precip,data = umf_lebi_comb)
d.3 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
              pformula = ~ wind, data = umf_lebi_comb)
# d.4 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
#               pformula = ~ recording_day + precip,data = umf_lebi_comb)
d.5 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
              pformula = ~ precip + wind,data = umf_lebi_comb)
d.6 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
              pformula = ~ survey_day, data = umf_lebi_comb)
d.7 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
              pformula = ~ survey_day + precip, data = umf_lebi_comb)
d.8 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
              pformula = ~ bout,data = umf_lebi_comb)
# d.9 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
#               pformula = ~ bout + recording_day,data = umf_lebi_comb)
d.global <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
                   pformula = ~  precip + wind + bout + survey_day,
                   data = umf_lebi_comb)



# Model selection---YOU TEST THE SAME DETECTION VARS/MODS FOR ALL SPP?
models <- list(m.null, d.2, d.3, d.5,d.6,d.7,d.8,d.global)
modnames <- as.character(c("psi(.)gam(.)eps(.)p(.)", 
                           "psi(.)gam(.)eps(.)p(precip)", 
                           "psi(.)gam(.)eps(.)p(wind)",
                           "psi(.)gam(.)eps(.)p(precip + wind)",
                           "psi(.)gam(.)eps(.)p(survey_day)", 
                           "psi(.)gam(.)eps(.)p(survey_day + precip)", 
                           "psi(.)gam(.)eps(.)p(bout)",
                           "psi(.)gam(.)eps(.)p(precip + wind + bout + survey_day)"))

modsel_det <- aictab(models, modnames = modnames)
print(modsel_det)


# Top is survey day
summary(d.6)
confint(d.6, type = "psi", level = 0.85)
confint(d.6, type = "det",level = 0.85)

# Make table
modsel_det <- modsel_det[-5]
modsel_det <- modsel_det[-5]

table <- nice_table(
  modsel_det,
  title = c("Least Bittern", "Detection")
)

print(table, preview = "docx")

#confidence intervals 85%


##2.B Final Models ---------------------------------------------------------------

# I developed these a priori as they seem reasonable depictions of what could be 
# driving marsh bird occupancy in TN (e.g., local veg/water conditions, and 
# surrounding habitat availability)

# Set best detection model as null
m.null <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
                 pformula = ~ survey_day, data = umf_lebi_comb)

# Global includes all local scale variables and landscape variables 
m.global <- colext(psiformula = ~ 
                     
                     # local vegetation cover 
                     emveg + floatingveg + openwater  + waterdepth+
                     
                     # landscape cover
                     palus500 + emergent500 + openwater500+ag500,
                   gammaformula = ~ 1, epsilonformula = ~ 1, 
                   pformula = ~ survey_day, data = umf_lebi_comb)

m.local.veg <- colext(psiformula = ~ emveg + floatingveg + openwater +  waterdepth,
                      gammaformula = ~ 1, epsilonformula = ~ 1, 
                      pformula = ~ survey_day, data = umf_lebi_comb)

m.palus.500 <- colext(psiformula = ~ palus500, gammaformula = ~ 1, epsilonformula = ~ 1, 
                      pformula = ~ survey_day, data = umf_lebi_comb)

m.emergent.500 <- colext(psiformula = ~ emergent500, gammaformula = ~ 1, epsilonformula = ~ 1, 
                         pformula = ~ survey_day, data = umf_lebi_comb)

m.openwater.500 <- colext(psiformula = ~ openwater500, gammaformula = ~ 1, epsilonformula = ~ 1, 
                          pformula = ~ survey_day, data = umf_lebi_comb)

m.ag.500 <- colext(psiformula = ~ ag500, gammaformula = ~ 1, epsilonformula = ~ 1, 
                   pformula = ~ survey_day, data = umf_lebi_comb)

m.emveg <- colext(psiformula = ~ emveg,
                  gammaformula = ~ 1, epsilonformula = ~ 1, 
                  pformula = ~ survey_day, data = umf_lebi_comb)

m.floatveg <- colext(psiformula = ~ floatingveg,
                     gammaformula = ~ 1, epsilonformula = ~ 1, 
                     pformula = ~ survey_day, data = umf_lebi_comb)

m.openwater <- colext(psiformula = ~ openwater,
                      gammaformula = ~ 1, epsilonformula = ~ 1, 
                      pformula = ~ survey_day, data = umf_lebi_comb)

m.waterdepth <- colext(psiformula = ~  waterdepth,
                       gammaformula = ~ 1, epsilonformula = ~ 1, 
                       pformula = ~ survey_day, data = umf_lebi_comb)

m.landscape <- colext(psiformula = ~ palus500 + emergent500 +openwater500+ag500,
                      gammaformula = ~ 1, 
                      epsilonformula = ~ 1, 
                      pformula = ~ survey_day, 
                      data = umf_lebi_comb)

m.emveg.waterdepth <- colext(psiformula = ~  waterdepth+emveg,
                             gammaformula = ~ 1, epsilonformula = ~ 1,
                             pformula = ~ survey_day, data = umf_lebi_comb)

m.emveg.waterdepth.landscape <- colext(psiformula = ~  waterdepth+emveg+palus500 + emergent500 +openwater500+ag500,
                                       gammaformula = ~ 1, epsilonformula = ~ 1,
                                       pformula = ~ survey_day, data = umf_lebi_comb)



models <- list(m.null,
               m.palus.500, 
               m.emergent.500,
               m.openwater.500,
               m.ag.500,
               m.global,
               m.emveg, 
               m.floatveg,
               m.openwater,
               m.waterdepth,
               m.landscape,
               m.local.veg,
               m.emveg.waterdepth,
               m.emveg.waterdepth.landscape)

modnames <- as.character(c("m.null",
                           "m.palus.500", 
                           "m.emergent.500",
                           "m.openwater.500",
                           "m.ag.500",
                           "m.global",
                           "m.emveg", 
                           "m.floatveg",
                           "m.openwater",
                           "m.waterdepth",
                           "m.landscape",
                           "m.local.veg",
                           "m.emveg.waterdepth",
                           "m.emveg.waterdepth.landscape"))

modsel_site <- aictab(models, modnames = modnames)
modsel_site

#null is competitive so others are out
summary(m.final <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
                  pformula = ~ survey_day, data = umf_lebi_comb))
#### Find top occupancy model ---- m.null because it is competitive
# 85 % CI based on Arnold 2010 paper
# 85% confidence intervals 

confint(m.final, level = 0.85, type = "psi")
confint(m.final, level = 0.85, type = "det")
confint(m.final, level = 0.85, type = "col")
confint(m.final, level = 0.85, type = "ext")

#probability scale
plogis(-1.522304) #occu - 0.179
plogis(confint(m.final, level = 0.85, type = "psi"))


#odds ration of det prob
exp(0.00748)
exp(-0.01007166) #lower
exp(0.02503576)
### AF and I support setting the colonization/extinction to null and avoiding model selection completely- 
# That was not the focus of this paper. Rather, the use of dynamic models simply 
# incorporates more reality in that we likely do not meet population closure 
# assumptions for your focal species.

##2.C Plots --------------------------------------------------------------------
new_data1 <- data.frame(survey_day = 1:50)

lebi_det_preds <- predict(m.final,
                          type = "det",
                          newdata = new_data1,
                          level = .85)

(det_plot_lebi <- ggplot(lebi_det_preds %>% bind_cols(new_data1)) +  
    geom_ribbon(aes(x = survey_day, ymin = lower, ymax = upper), alpha = 0.5)+ 
    geom_line(aes(x = survey_day, y = Predicted), linewidth = 1, color = "#e1bce3") +
    labs( x = "Survey day", y = "Detection probability", title = "Least Bittern") +
    theme_pubr() +
    labs_pubr() +
    scale_fill_manual(values = "#CCC9DC") +
    theme(legend.position = "none") +
    theme(axis.title = element_text(size = 8),
          title = element_text(size = 10),
          axis.text = element_text(size = 8)) +
    scale_y_continuous(limits = c(0,1)) +
    scale_x_continuous(limits = c(0,50)))

# ggsave("Plots/det_lebi.png", plot = det_plot_lebi,
#        width = 3.5,
#        height = 3.5,
#        dpi = 300,
#        units = "in")




#3. COGA -----------------------------------------------------------------------

umf_coga_comb <- unmarkedMultFrame(y = (enc_hist_comb$`Common Gallinule`[-1,]),
                                   numPrimary = 3,
                                   obsCovs = obsCov,
                                   siteCovs = as.data.frame(sitecovs),
                                   yearlySiteCovs = yearlySiteCovs)
summary(umf_coga_comb)



##3.A Top detection model ------------------------------------------------------

#null model

m.null <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
                 pformula = ~ 1, data = umf_coga_comb)


## find top detection cov

# d.1 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
#               pformula = ~ recording_day, data = umf_coga_comb)
d.2 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
              pformula = ~ precip,data = umf_coga_comb)
d.3 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
              pformula = ~ wind, data = umf_coga_comb)
# d.4 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
#               pformula = ~ recording_day + precip,data = umf_coga_comb)
d.5 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
              pformula = ~ precip + wind,data = umf_coga_comb)
d.6 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
              pformula = ~ survey_day,data = umf_coga_comb)
d.7 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
              pformula = ~ survey_day + precip,data = umf_coga_comb)
d.8 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
              pformula = ~ bout,data = umf_coga_comb)
# d.9 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
#               pformula = ~ bout + recording_day,data = umf_coga_comb)
d.global <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
                   pformula = ~ precip + wind + bout + survey_day,data = umf_coga_comb)
summary(d.global)

##modified for small sample size


models <- list(m.null, d.2, d.3, d.5,d.6,d.7,d.8,d.global)
modnames <- as.character(c("psi(.)gam(.)eps(.)p(.)",
                           "psi(.)gam(.)eps(.)p(precip)", 
                           "psi(.)gam(.)eps(.)p(wind)",
                           "psi(.)gam(.)eps(.)p(precip+wind)",
                           "psi(.)gam(.)eps(.)p(survey_day)", 
                           "psi(.)gam(.)eps(.)p(survey_day+precip)", 
                           "psi(.)gam(.)eps(.)p(bout)",
                           "psi(.)gam(.)eps(.)p(global)"))

modsel_det <- aictab(models, modnames = modnames)
print(modsel_det)


confint(d.6, type = "det") 


#make table

modsel_det <- modsel_det[-5]
modsel_det <- modsel_det[-5]

table <- nice_table(
  modsel_det[1:9,],
  title = c("Common gallinule")
)

print(table, preview = "docx")

##3.B Final Models ---------------------------------------------------------------

# Set best detection model as null
m.null <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
                 pformula = ~ survey_day, data = umf_coga_comb)

# Global includes all local scale variables and landscape variables 
m.global <- colext(psiformula = ~ 
                     
                     # local vegetation cover 
                     emveg + floatingveg + openwater  + waterdepth+
                     
                     # landscape cover
                     palus500 + emergent500 + openwater500+ag500,
                   gammaformula = ~ 1, epsilonformula = ~ 1, 
                   pformula = ~ survey_day, data = umf_coga_comb)

m.local.veg <- colext(psiformula = ~ emveg + floatingveg + openwater +  waterdepth,
                      gammaformula = ~ 1, epsilonformula = ~ 1, 
                      pformula = ~ survey_day, data = umf_coga_comb)

m.palus.500 <- colext(psiformula = ~ palus500, gammaformula = ~ 1, epsilonformula = ~ 1, 
                      pformula = ~ survey_day, data = umf_coga_comb)

m.emergent.500 <- colext(psiformula = ~ emergent500, gammaformula = ~ 1, epsilonformula = ~ 1, 
                         pformula = ~ survey_day, data = umf_coga_comb)

m.openwater.500 <- colext(psiformula = ~ openwater500, gammaformula = ~ 1, epsilonformula = ~ 1, 
                          pformula = ~ survey_day, data = umf_coga_comb)

m.ag.500 <- colext(psiformula = ~ ag500, gammaformula = ~ 1, epsilonformula = ~ 1, 
                   pformula = ~ survey_day, data = umf_coga_comb)

m.emveg <- colext(psiformula = ~ emveg,
                  gammaformula = ~ 1, epsilonformula = ~ 1, 
                  pformula = ~ survey_day, data = umf_coga_comb)

m.floatveg <- colext(psiformula = ~ floatingveg,
                     gammaformula = ~ 1, epsilonformula = ~ 1, 
                     pformula = ~ survey_day, data = umf_coga_comb)

m.openwater <- colext(psiformula = ~ openwater,
                      gammaformula = ~ 1, epsilonformula = ~ 1, 
                      pformula = ~ survey_day, data = umf_coga_comb)

m.waterdepth <- colext(psiformula = ~  waterdepth,
                       gammaformula = ~ 1, epsilonformula = ~ 1, 
                       pformula = ~ survey_day, data = umf_coga_comb)

m.landscape <- colext(psiformula = ~ palus500 + emergent500 +openwater500+ag500,
                      gammaformula = ~ 1, 
                      epsilonformula = ~ 1, 
                      pformula = ~ survey_day, 
                      data = umf_coga_comb)

m.emveg.waterdepth <- colext(psiformula = ~  waterdepth+emveg,
                             gammaformula = ~ 1, epsilonformula = ~ 1,
                             pformula = ~ survey_day, data = umf_coga_comb)

m.emveg.waterdepth.landscape <- colext(psiformula = ~  waterdepth+emveg+palus500 + emergent500 +openwater500+ag500,
                                       gammaformula = ~ 1, epsilonformula = ~ 1,
                                       pformula = ~ survey_day, data = umf_coga_comb)



models <- list(m.null,
               m.palus.500, 
               m.emergent.500,
               m.openwater.500,
               m.ag.500,
               m.global,
               m.emveg, 
               m.floatveg,
               m.openwater,
               m.waterdepth,
               m.landscape,
               m.local.veg,
               m.emveg.waterdepth,
               m.emveg.waterdepth.landscape)

modnames <- as.character(c("m.null",
                           "m.palus.500", 
                           "m.emergent.500",
                           "m.openwater.500",
                           "m.ag.500",
                           "m.global",
                           "m.emveg", 
                           "m.floatveg",
                           "m.openwater",
                           "m.waterdepth",
                           "m.landscape",
                           "m.local.veg",
                           "m.emveg.waterdepth",
                           "m.emveg.waterdepth.landscape"))

modsel_site <- aictab(models, modnames = modnames)
modsel_site

#3 COMPETITIVE MODELS
#floating veg the best
#emergent 500
#ag 500



models_final <- list(m.ag.500, m.emergent.500, m.floatveg)
model_names <- c("m.ag500", "m.emergent500", "m.floatveg")

model_list <- model.sel(models_final, model_names)

avg_mod <- model.avg(model_list)
summary(avg_mod)

confint(avg_mod, level = 0.85)

summary(m.final.2 <- colext(psiformula = ~ floatingveg + emergent500,
                    gammaformula = ~ 1, epsilonformula = ~ 1, 
                    pformula = ~ survey_day, data = umf_coga_comb))
confint(m)


#manual model average
ms <- modSel(fl)

w <- ms@Full$AICwt
names(w) <- rownames(ms@Full)
w
##3.C Plots --------------------------------------------------------------------

new_data1 <- data.frame(survey_day = 1:50)

coga_det_preds <- predict(avg_mod,
                          type = "det",
                          newdata = new_data1,
                          level = .85)

#does not give upper and lower so manually
z_85 <- qnorm(0.925)  

coga_det_df <- data.frame(
  fit = coga_det_preds$fit,
  se = coga_det_preds$se.fit
) %>%
  mutate(
    lower = fit - z_85 * se,
    upper = fit + z_85 * se
  )

(det_plot_coga <- ggplot(coga_det_df %>% bind_cols(new_data1)) +  
    geom_ribbon(aes(x = survey_day, ymin = lower, ymax = upper), alpha = 0.5)+ 
    geom_line(aes(x = survey_day, y = fit), linewidth = 1, color = "#e1bce3") +
    labs( x = "Survey day", y = "Detection probability",title = "Common Gallinule") +
    theme_pubr() +
    labs_pubr() +
    scale_fill_manual(values = "#CCC9DC") +
    theme(legend.position = "none") +
    theme(axis.title = element_text(size = 8),
          title = element_text(size = 10),
          axis.text = element_text(size = 8)) +
    scale_y_continuous(limits = c(0,1)) +
    scale_x_continuous(limits = c(0,50)))

# ggsave("Plots/det_coga.png", plot = det_plot,
#        width = 3.5,
#        height = 3.5,
#        dpi = 300,
#        units = "in")


# avg_model 


new_data2 <- data.frame(
  floatingveg = seq(1:100), #- 9.10) / 17.2),  ## - mean / sd
  ag500 = 0,
  emergent500 = 0)


float_preds <- predict(avg_mod,
                       type = "psi",
                       newdata = new_data2,
                       level = 0.95)

float_df <- data.frame(
  fit = float_preds$fit,
  se = float_preds$se.fit
) %>%
  mutate(
    lower = fit - z_85 * se,
    upper = fit + z_85 * se
  ) %>%
  bind_cols(new_data2)

# new data for ag500
new_data3 <- data.frame(
  ag500 = seq((1:100 - 21.1) / 23.4),  
  floatingveg = 0,
  emergent500 = 0)

ag_preds <- predict(avg_mod,
                    type = "psi",
                    newdata = new_data3,
                    level = 0.85)

ag_df <- data.frame(
  fit = ag_preds$fit,
  se = ag_preds$se.fit
) %>%
  mutate(
    lower = fit - z_85 * se,
    upper = fit + z_85 * se
  ) %>%
  bind_cols(new_data3)



(float_plot <- ggplot(float_df) +
  geom_ribbon(aes(x = floatingveg, ymin = lower, ymax = upper), alpha = 0.5) +
  geom_line(aes(x = floatingveg, y = fit), color = "#e1bce3", linewidth = 1) +
  labs(x = "Percent floating aquatic vegetation",
       y = "Occupancy probability",
       title = "Common Gallinule") +
    theme_pubr() +
    labs_pubr() +
    scale_fill_manual(values = "#CCC9DC") +
    theme(legend.position = "none") +
    theme(axis.title = element_text(size = 8),
          title = element_text(size = 10),
          axis.text = element_text(size = 8))+
  scale_y_continuous(limits = c(0,1)))
   

(ag_plot <- ggplot(ag_df) +
  geom_ribbon(aes(x = ag500, ymin = lower, ymax = upper), alpha = 0.5) +
  geom_line(aes(x = ag500, y = fit),color = "#e1bce3", linewidth = 1) +
  labs(x = "Percent agriculture cover (500m)",
       y = "Occupancy probability",
       title = "Common Gallinule") +
    theme_pubr() +
    labs_pubr() +
    scale_fill_manual(values = "#CCC9DC") +
    theme(legend.position = "none") +
    theme(axis.title = element_text(size = 8),
          title = element_text(size = 10),
          axis.text = element_text(size = 8))+
    scale_y_continuous(limits = c(0,1)))

# ggsave("Plots/psi_em500_lebi.png", plot = psi1_plot,
#        width = 3.5,
#        height = 3.5,
#        dpi = 300,
#        units = "in")

#4. AMCO -----------------------------------------------------------------------

umf_amco_comb <- unmarkedMultFrame(y = enc_hist_comb$`American Coot`,
                                   numPrimary = 3,
                                   obsCovs = obsCov,
                                   siteCovs = as.data.frame(sitecovs),
                                   yearlySiteCovs = yearlySiteCovs)
summary(umf_amco_comb)

##4.A Top detection model ------------------------------------------------------

#null model

m.null <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
                 pformula = ~ 1, data = umf_amco_comb)
# d.1 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
#               pformula = ~ recording_day, data = umf_amco_comb)
d.2 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
              pformula = ~ precip,data = umf_amco_comb)
d.3 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
              pformula = ~ wind, data = umf_amco_comb)
# d.4 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
#               pformula = ~ recording_day + precip,data = umf_amco_comb)
d.5 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
              pformula = ~ precip + wind,data = umf_amco_comb)
d.6 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
              pformula = ~ survey_day,data = umf_amco_comb)
d.7 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
              pformula = ~ survey_day + precip,data = umf_amco_comb)
d.8 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
              pformula = ~ bout,data = umf_amco_comb)
# d.9 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
#               pformula = ~ bout + recording_day,data = umf_amco_comb)
d.global <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
                   pformula = ~ precip + wind + bout + survey_day,data = umf_amco_comb)



models <- list(m.null, d.2, d.3,d.5,d.6,d.7,d.8,d.global)
modnames <- as.character(c("psi(.)gam(.)eps(.)p(.)",
                           "psi(.)gam(.)eps(.)p(precip)", 
                           "psi(.)gam(.)eps(.)p(wind)", 
                           "psi(.)gam(.)eps(.)p(precip+wind)",
                           "psi(.)gam(.)eps(.)p(survey_day)", 
                           "psi(.)gam(.)eps(.)p(survey_day+precip)", 
                           "psi(.)gam(.)eps(.)p(bout)",
                           "psi(.)gam(.)eps(.)p(global)"))

modsel_det <- aictab(models, modnames = modnames)
print(modsel_det)

##top is survey day

#make table

modsel_det <- modsel_det[-5]
modsel_det <- modsel_det[-5]

table <- nice_table(
  modsel_det[1:9,],
  title = c("American Coot")
)

print(table, preview = "docx")

##4.B Final Models ---------------------------------------------------------------

# Set best detection model as null
m.null <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
                 pformula = ~ survey_day, data = umf_amco_comb)

# Global includes all local scale variables and landscape variables 
m.global <- colext(psiformula = ~ 
                     
                     # local vegetation cover 
                     emveg + floatingveg + openwater  + waterdepth+
                     
                     # landscape cover
                     palus500 + emergent500 + openwater500+ag500,
                   gammaformula = ~ 1, epsilonformula = ~ 1, 
                   pformula = ~ survey_day, data = umf_amco_comb)

m.local.veg <- colext(psiformula = ~ emveg + floatingveg + openwater +  waterdepth,
                      gammaformula = ~ 1, epsilonformula = ~ 1, 
                      pformula = ~ survey_day, data = umf_amco_comb)

m.palus.500 <- colext(psiformula = ~ palus500, gammaformula = ~ 1, epsilonformula = ~ 1, 
                      pformula = ~ survey_day, data = umf_amco_comb)

m.emergent.500 <- colext(psiformula = ~ emergent500, gammaformula = ~ 1, epsilonformula = ~ 1, 
                         pformula = ~ survey_day, data = umf_amco_comb)

m.openwater.500 <- colext(psiformula = ~ openwater500, gammaformula = ~ 1, epsilonformula = ~ 1, 
                          pformula = ~ survey_day, data = umf_amco_comb)

m.ag.500 <- colext(psiformula = ~ ag500, gammaformula = ~ 1, epsilonformula = ~ 1, 
                   pformula = ~ survey_day, data = umf_amco_comb)

m.emveg <- colext(psiformula = ~ emveg,
                  gammaformula = ~ 1, epsilonformula = ~ 1, 
                  pformula = ~ survey_day, data = umf_amco_comb)

m.floatveg <- colext(psiformula = ~ floatingveg,
                     gammaformula = ~ 1, epsilonformula = ~ 1, 
                     pformula = ~ survey_day, data = umf_amco_comb)

m.openwater <- colext(psiformula = ~ openwater,
                      gammaformula = ~ 1, epsilonformula = ~ 1, 
                      pformula = ~ survey_day, data = umf_amco_comb)

m.waterdepth <- colext(psiformula = ~  waterdepth,
                       gammaformula = ~ 1, epsilonformula = ~ 1, 
                       pformula = ~ survey_day, data = umf_amco_comb)

m.landscape <- colext(psiformula = ~ palus500 + emergent500 +openwater500+ag500,
                      gammaformula = ~ 1, 
                      epsilonformula = ~ 1, 
                      pformula = ~ survey_day, 
                      data = umf_amco_comb)

m.emveg.waterdepth <- colext(psiformula = ~  waterdepth+emveg,
                             gammaformula = ~ 1, epsilonformula = ~ 1,
                             pformula = ~ survey_day, data = umf_amco_comb)

m.emveg.waterdepth.landscape <- colext(psiformula = ~  waterdepth+emveg+palus500 + emergent500 +openwater500+ag500,
                                       gammaformula = ~ 1, epsilonformula = ~ 1,
                                       pformula = ~ survey_day, data = umf_amco_comb)



models <- list(m.null,
               m.palus.500, 
               m.emergent.500,
               m.openwater.500,
               m.ag.500,
               m.global,
               m.emveg, 
               m.floatveg,
               m.openwater,
               m.waterdepth,
               m.landscape,
               m.local.veg,
               m.emveg.waterdepth,
               m.emveg.waterdepth.landscape)

modnames <- as.character(c("m.null",
                           "m.palus.500", 
                           "m.emergent.500",
                           "m.openwater.500",
                           "m.ag.500",
                           "m.global",
                           "m.emveg", 
                           "m.floatveg",
                           "m.openwater",
                           "m.waterdepth",
                           "m.landscape",
                           "m.local.veg",
                           "m.emveg.waterdepth",
                           "m.emveg.waterdepth.landscape"))

modsel_site <- aictab(models, modnames = modnames)
modsel_site

#3 competitive models

#null is top
summary(m.final <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
                   pformula = ~ survey_day, data = umf_amco_comb))

# 85% confidence intervals 
confint(m.final, level = 0.85, type = "psi")
confint(m.final, level = 0.85, type = "det")
confint(m.final, level = 0.85, type = "col")
confint(m.final, level = 0.85, type = "ext")


#daily detection 
exp(-0.05) 

#weekly detection
#each additonal day reduces detection odds by 5%
-0.05*7
exp(-0.35) #each week, we were 0.7 times less likely to detect AMCO

#CIS
exp(-0.10*7)
exp(-0.01*7)

##4.C Plots --------------------------------------------------------------------
#not updated yet
new_data1 <- data.frame(survey_day = 1:50)

amco_det_preds <- predict(m.final,
                          type = "det",
                          newdata = new_data1,
                          level = .85)

(det_plot_amco <- ggplot(amco_det_preds %>% bind_cols(new_data1)) +  
    geom_ribbon(aes(x = survey_day, ymin = lower, ymax = upper), alpha = 0.5)+ 
    geom_line(aes(x = survey_day, y = Predicted), linewidth = 1, color = "#e1bce3") +
    labs( x = "Survey day", y = "Detection probability",title = "American Coot") +
    theme_pubr() +
    labs_pubr() +
    scale_fill_manual(values = "#CCC9DC") +
    theme(legend.position = "none") +
    theme(axis.title = element_text(size = 8),
          title = element_text(size = 10),
          axis.text = element_text(size = 8)) +
    scale_y_continuous(limits = c(0,1)) +
    scale_x_continuous(limits = c(0,50)))

# ggsave("Plots/det_amco.png", plot = det_plot,
#        width = 3.5,
#        height = 3.5,
#        dpi = 300,
#        units = "in")




#5. PBGR -----------------------------------------------------------------------

umf_pbgr_comb <- unmarkedMultFrame(y = enc_hist_comb$`Pied-billed Grebe`,
                                   numPrimary = 3,
                                   obsCovs = obsCov,
                                   siteCovs = as.data.frame(sitecovs),
                                   yearlySiteCovs = yearlySiteCovs)
summary(umf_pbgr_comb)



##5.A Top detection model ------------------------------------------------------

#null model

m.null <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
                 pformula = ~ 1, data = umf_pbgr_comb)
# d.1 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
#               pformula = ~ recording_day, data = umf_pbgr_comb)
d.2 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
              pformula = ~ precip,data = umf_pbgr_comb)
d.3 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
              pformula = ~ wind, data = umf_pbgr_comb)
# d.4 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
#               pformula = ~ recording_day + precip,data = umf_pbgr_comb)
d.5 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
              pformula = ~ precip + wind,data = umf_pbgr_comb)
d.6 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
              pformula = ~ survey_day,data = umf_pbgr_comb)
d.7 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
              pformula = ~ survey_day + precip,data = umf_pbgr_comb)
d.8 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
              pformula = ~ bout,data = umf_pbgr_comb)
# d.9 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
#               pformula = ~ bout + recording_day,data = umf_pbgr_comb)
d.global <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
                   pformula = ~ precip + wind + bout + survey_day,data = umf_pbgr_comb)
summary(d.global)




models <- list(m.null, d.2, d.3, d.5,d.6,d.7,d.8,d.global)
modnames <- as.character(c("psi(.)gam(.)eps(.)p(.)",
                           "psi(.)gam(.)eps(.)p(precip)", 
                           "psi(.)gam(.)eps(.)p(wind)",
                           "psi(.)gam(.)eps(.)p(precip+wind)",
                           "psi(.)gam(.)eps(.)p(survey_day)", 
                           "psi(.)gam(.)eps(.)p(survey_day+precip)", 
                           "psi(.)gam(.)eps(.)p(bout)",
                           "psi(.)gam(.)eps(.)p(global)"))

modsel_det <- aictab(models, modnames = modnames)
print(modsel_det)

##top is survey day

summary(d.6)
confint(d.6, type = "det") #survey day is sig

confint(d.7, type = "det") #no

modsel_det <- modsel_det[-5]
modsel_det <- modsel_det[-5]

table <- nice_table(
  modsel_det[1:9,],
  title = c("Pied-billed grebe")
)

 print(table, preview = "docx")

##5.B Occupancy -----------------------------------------------------------
# Set best detection model as null
m.null <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
                 pformula = ~ survey_day, data = umf_pbgr_comb)

# Global includes all local scale variables and landscape variables 
m.global <- colext(psiformula = ~ 
                     
                     # local vegetation cover 
                     emveg + floatingveg + openwater  + waterdepth+
                     
                     # landscape cover
                     palus500 + emergent500 + openwater500+ag500,
                   gammaformula = ~ 1, epsilonformula = ~ 1, 
                   pformula = ~ survey_day, data = umf_pbgr_comb)

m.local.veg <- colext(psiformula = ~ emveg + floatingveg + openwater +  waterdepth,
                      gammaformula = ~ 1, epsilonformula = ~ 1, 
                      pformula = ~ survey_day, data = umf_pbgr_comb)

m.palus.500 <- colext(psiformula = ~ palus500, gammaformula = ~ 1, epsilonformula = ~ 1, 
                      pformula = ~ survey_day, data = umf_pbgr_comb)

m.emergent.500 <- colext(psiformula = ~ emergent500, gammaformula = ~ 1, epsilonformula = ~ 1, 
                         pformula = ~ survey_day, data = umf_pbgr_comb)

m.openwater.500 <- colext(psiformula = ~ openwater500, gammaformula = ~ 1, epsilonformula = ~ 1, 
                          pformula = ~ survey_day, data = umf_pbgr_comb)

m.ag.500 <- colext(psiformula = ~ ag500, gammaformula = ~ 1, epsilonformula = ~ 1, 
                   pformula = ~ survey_day, data = umf_pbgr_comb)

m.emveg <- colext(psiformula = ~ emveg,
                  gammaformula = ~ 1, epsilonformula = ~ 1, 
                  pformula = ~ survey_day, data = umf_pbgr_comb)

m.floatveg <- colext(psiformula = ~ floatingveg,
                     gammaformula = ~ 1, epsilonformula = ~ 1, 
                     pformula = ~ survey_day, data = umf_pbgr_comb)

m.openwater <- colext(psiformula = ~ openwater,
                      gammaformula = ~ 1, epsilonformula = ~ 1, 
                      pformula = ~ survey_day, data = umf_pbgr_comb)

m.waterdepth <- colext(psiformula = ~  waterdepth,
                       gammaformula = ~ 1, epsilonformula = ~ 1, 
                       pformula = ~ survey_day, data = umf_pbgr_comb)

m.landscape <- colext(psiformula = ~ palus500 + emergent500 +openwater500+ag500,
                      gammaformula = ~ 1, 
                      epsilonformula = ~ 1, 
                      pformula = ~ survey_day, 
                      data = umf_pbgr_comb)

m.emveg.waterdepth <- colext(psiformula = ~  waterdepth+emveg,
                             gammaformula = ~ 1, epsilonformula = ~ 1,
                             pformula = ~ survey_day, data = umf_pbgr_comb)

m.emveg.waterdepth.landscape <- colext(psiformula = ~  waterdepth+emveg+palus500 + emergent500 +openwater500+ag500,
                                       gammaformula = ~ 1, epsilonformula = ~ 1,
                                       pformula = ~ survey_day, data = umf_pbgr_comb)



models <- list(m.null,
               m.palus.500, 
               m.emergent.500,
               m.openwater.500,
               m.ag.500,
               m.global,
               m.emveg, 
               m.floatveg,
               m.openwater,
               m.waterdepth,
               m.landscape,
               m.local.veg,
               m.emveg.waterdepth,
               m.emveg.waterdepth.landscape)

modnames <- as.character(c("m.null",
                           "m.palus.500", 
                           "m.emergent.500",
                           "m.openwater.500",
                           "m.ag.500",
                           "m.global",
                           "m.emveg", 
                           "m.floatveg",
                           "m.openwater",
                           "m.waterdepth",
                           "m.landscape",
                           "m.local.veg",
                           "m.emveg.waterdepth",
                           "m.emveg.waterdepth.landscape"))

modsel_site <- aictab(models, modnames = modnames)
modsel_site

#Null is the top
summary(m.final <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
                  pformula = ~ survey_day, data = umf_pbgr_comb))

# 85% confidence intervals 
confint(m.final, level = 0.85, type = "psi")
confint(m.final, level = 0.85, type = "det")
confint(m.final, level = 0.85, type = "col")
confint(m.final, level = 0.85, type = "ext")


#probability scale
plogis(-1.522304) #occu - 0.179

plogis(0.007482049)


#daily detection 
exp(-0.04) 

#weekly detection
#each additonal day reduces detection odds by 5%
-0.04*7
exp(-0.28) #each week, we were 0.76 times less likely to detect AMCO

#CIS
exp(-0.06*7)
exp(-0.02*7)
##5.C Plots --------------------------------------------------------------------

new_data1 <- data.frame(survey_day = 1:50)

pbgr_det_preds <- predict(m.final,
                          type = "det",
                          newdata = new_data1,
                          level = .85)

(det_plot_pbgr <- ggplot(pbgr_det_preds %>% bind_cols(new_data1)) +  
    geom_ribbon(aes(x = survey_day, ymin = lower, ymax = upper), alpha = 0.5)+ 
    geom_line(aes(x = survey_day, y = Predicted), linewidth = 1, color = "#e1bce3") +
    labs( x = "Survey day", y = "Detection probability",title = "Pied-billed Grebe") +
    theme_pubr() +
    labs_pubr() +
    scale_fill_manual(values = "#CCC9DC") +
    theme(legend.position = "none") +
    theme(axis.title = element_text(size = 8),
          title = element_text(size = 10),
          axis.text = element_text(size = 8)) +
    scale_y_continuous(limits = c(0,1)) +
    scale_x_continuous(limits = c(0,50)))



# ggsave("Plots/det_pbgr.png", plot = det_plot,
#        width = 3.5,
#        height = 3.5,
#        dpi = 300,
#        units = "in")



#6. SORA -----------------------------------------------------------------------
y <- enc_hist_comb$Sora[-1,]

umf_sora_comb <- unmarkedMultFrame(y = y,
                                   numPrimary = 3,
                                   obsCovs = obsCov,
                                   siteCovs = as.data.frame(sitecovs),
                                   yearlySiteCovs = yearlySiteCovs)
summary(umf_sora_comb)




##6.A Top detection model ------------------------------------------------------

#null model

m.null <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
                 pformula = ~ 1, data = umf_sora_comb)
# d.1 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
#               pformula = ~ recording_day, data = umf_sora_comb)
d.2 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
              pformula = ~ precip,data = umf_sora_comb)
d.3 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
              pformula = ~ wind, data = umf_sora_comb)
# d.4 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
#               pformula = ~ recording_day + precip,data = umf_sora_comb)
d.5 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
              pformula = ~ precip + wind,data = umf_sora_comb)
d.6 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
              pformula = ~ survey_day,data = umf_sora_comb)
d.7 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
              pformula = ~ survey_day + precip,data = umf_sora_comb)
d.8 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
              pformula = ~ bout,data = umf_sora_comb)
# d.9 <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
#               pformula = ~ bout + recording_day,data = umf_sora_comb)
d.global <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
                   pformula = ~ precip + wind + bout + survey_day,data = umf_sora_comb)


models <- list(m.null, d.2, d.3, d.5,d.6,d.7,d.8,d.global)
modnames <- as.character(c("psi(.)gam(.)eps(.)p(.)",
                           "psi(.)gam(.)eps(.)p(precip)", 
                           "psi(.)gam(.)eps(.)p(wind)",
                           "psi(.)gam(.)eps(.)p(precip+wind)",
                           "psi(.)gam(.)eps(.)p(survey_day)", 
                           "psi(.)gam(.)eps(.)p(survey_day+precip)", 
                           "psi(.)gam(.)eps(.)p(bout)",
                           "psi(.)gam(.)eps(.)p(global)"))

modsel_det <- aictab(models, modnames = modnames)
print(modsel_det)

##top are global, survey day, survey-day + precip
summary(d.7)
confint(d.7, type = "det") #no


modsel_det <- modsel_det[-5]
modsel_det <- modsel_det[-5]

table <- nice_table(
  modsel_det[1:9,],
  title = c("Sora")
)

print(table, preview = "docx")

##6.B Final Models ---------------------------------------------------------------

# Set best detection model as null
m.null <- colext(psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1, 
                 pformula = ~ survey_day, data = umf_sora_comb)

# Global includes all local scale variables and landscape variables 
m.global <- colext(psiformula = ~ 
                     
                     # local vegetation cover 
                     emveg + floatingveg + openwater  + waterdepth+
                     
                     # landscape cover
                     palus500 + emergent500 + openwater500+ag500,
                   gammaformula = ~ 1, epsilonformula = ~ 1, 
                   pformula = ~ survey_day, data = umf_sora_comb)

m.local.veg <- colext(psiformula = ~ emveg + floatingveg + openwater +  waterdepth,
                      gammaformula = ~ 1, epsilonformula = ~ 1, 
                      pformula = ~ survey_day, data = umf_sora_comb)

m.palus.500 <- colext(psiformula = ~ palus500, gammaformula = ~ 1, epsilonformula = ~ 1, 
                      pformula = ~ survey_day, data = umf_sora_comb)

m.emergent.500 <- colext(psiformula = ~ emergent500, gammaformula = ~ 1, epsilonformula = ~ 1, 
                         pformula = ~ survey_day, data = umf_sora_comb)

m.openwater.500 <- colext(psiformula = ~ openwater500, gammaformula = ~ 1, epsilonformula = ~ 1, 
                          pformula = ~ survey_day, data = umf_sora_comb)

m.ag.500 <- colext(psiformula = ~ ag500, gammaformula = ~ 1, epsilonformula = ~ 1, 
                   pformula = ~ survey_day, data = umf_sora_comb)

m.emveg <- colext(psiformula = ~ emveg,
                  gammaformula = ~ 1, epsilonformula = ~ 1, 
                  pformula = ~ survey_day, data = umf_sora_comb)

m.floatveg <- colext(psiformula = ~ floatingveg,
                     gammaformula = ~ 1, epsilonformula = ~ 1, 
                     pformula = ~ survey_day, data = umf_sora_comb)

m.openwater <- colext(psiformula = ~ openwater,
                      gammaformula = ~ 1, epsilonformula = ~ 1, 
                      pformula = ~ survey_day, data = umf_sora_comb)

m.waterdepth <- colext(psiformula = ~  waterdepth,
                       gammaformula = ~ 1, epsilonformula = ~ 1, 
                       pformula = ~ survey_day, data = umf_sora_comb)

m.landscape <- colext(psiformula = ~ palus500 + emergent500 +openwater500+ag500,
                      gammaformula = ~ 1, 
                      epsilonformula = ~ 1, 
                      pformula = ~ survey_day, 
                      data = umf_sora_comb)

m.emveg.waterdepth <- colext(psiformula = ~  waterdepth+emveg,
                             gammaformula = ~ 1, epsilonformula = ~ 1,
                             pformula = ~ survey_day, data = umf_sora_comb)

m.emveg.waterdepth.landscape <- colext(psiformula = ~  waterdepth+emveg+palus500 + emergent500 +openwater500+ag500,
                                       gammaformula = ~ 1, epsilonformula = ~ 1,
                                       pformula = ~ survey_day, data = umf_sora_comb)



models <- list(m.null,
               m.palus.500, 
               m.emergent.500,
               m.openwater.500,
               m.ag.500,
               m.global,
               m.emveg, 
               m.floatveg,
               m.openwater,
               m.waterdepth,
               m.landscape,
               m.local.veg,
               m.emveg.waterdepth,
               m.emveg.waterdepth.landscape)

modnames <- as.character(c("m.null",
                           "m.palus.500", 
                           "m.emergent.500",
                           "m.openwater.500",
                           "m.ag.500",
                           "m.global",
                           "m.emveg", 
                           "m.floatveg",
                           "m.openwater",
                           "m.waterdepth",
                           "m.landscape",
                           "m.local.veg",
                           "m.emveg.waterdepth",
                           "m.emveg.waterdepth.landscape"))

modsel_site <- aictab(models, modnames = modnames)
modsel_site

#top is openwater 500 
summary(m.final <- colext(psiformula = ~ openwater500, gammaformula = ~ 1, epsilonformula = ~ 1, 
                  pformula = ~ survey_day, data = umf_sora_comb))

confint(m.final, level = 0.85, type = "psi")
confint(m.final, level = 0.85, type = "det")
confint(m.final, level = 0.85, type = "col")
confint(m.final, level = 0.85, type = "ext")


##6.C Plots --------------------------------------------------------------------
new_data1 <- data.frame(survey_day = 1:50)

sora_det_preds <- predict(m.final,
                          type = "det",
                          newdata = new_data1,
                          level = .85)

(det_plot_sora <- ggplot(sora_det_preds %>% bind_cols(new_data1)) +  
    geom_ribbon(aes(x = survey_day, ymin = lower, ymax = upper), alpha = 0.5)+ 
    geom_line(aes(x = survey_day, y = Predicted), linewidth = 1, color = "#e1bce3") +
    labs( x = "Survey day", y = "Detection probability",title = "Sora") +
    theme_pubr() +
    labs_pubr() +
    scale_fill_manual(values = "#CCC9DC") +
    theme(legend.position = "none") +
    theme(axis.title = element_text(size = 8),
          title = element_text(size = 10),
          axis.text = element_text(size = 8)) +
    scale_y_continuous(limits = c(0,1)) +
    scale_x_continuous(limits = c(0,50)))

##Make plots for open water 

# ggsave("Plots/det_sora.png", plot = det_plot,
#        width = 3.5,
#        height = 3.5,
#        dpi = 300,
#        units = "in")



# OPENWATER 500
new_data2 <- data.frame(openwater500 = (1:100 - 17.24) / 18.16) #mean = 17.24, sd = 18.16

sora_psi1_preds <- predict(m.final,
                           type = "psi",
                           newdata = new_data2,
                           level = .85)

(psi1_plot_sora <- ggplot(sora_psi1_preds %>% bind_cols(new_data2) %>% 
                       mutate(openwater500 = 1:100)) +  
    geom_ribbon(aes(x = openwater500, ymin = lower, ymax = upper), alpha = 0.5)+ 
    geom_line(aes(x = openwater500, y = Predicted), linewidth = 1, color = "#e1bce3") +
    labs( x = "Percent open water (500 m)", y = "Occupancy probability", title = "Sora") +
    theme_pubr() +
    labs_pubr() +
    scale_fill_manual(values = "#CCC9DC") +
    theme(legend.position = "none") +
    theme(axis.title = element_text(size = 8),
          title = element_text(size = 10),
          axis.text = element_text(size = 8)) +
    scale_y_continuous(limits = c(0,1)) +
    scale_x_continuous(limits = c(0,100)))
# ggsave("Plots/psi_ow500_sora.png", plot = last_plot(),
#        width = 3.5,
#        height = 3.5,
#        dpi = 300,
#        units = "in")



### Detection Probability Plot -------------------------------------------------
library(patchwork)
(all_plot <- (det_plot_lebi + det_plot_coga + det_plot_amco + 
                plot_layout(axis_titles = "collect_y"))) / (det_plot_pbgr +
                                                              det_plot_sora + 
                                                              plot_spacer() + 
                                                              plot_layout(axis_titles = "collect_y")) 

#figure 2
# ggsave("Plots/detectionprobability.png", plot = last_plot(),
#        width = 8,
#        height = 8,
#        dpi = 300,
#        units = "in")


### Final Occupancy Plots ------------------------------------------------------
#figure 3