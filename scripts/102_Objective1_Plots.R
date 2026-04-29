################################################################################

#name: 102_Objective1_Plots.R
#date updated: 4/28/2026

#description: recreate Figure 2 in manuscript (objective 1)


################################################################################

source("./scripts/101_Objective 1.R")

library(tidyverse)
library(ggpubr)
library(patchwork)


#custom ggplot theme
theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = margin(0,0,0,0),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

# LEBI -------------------------------------------------------------------
# set up
est_psi_lebi_pc   <- plogis(coef(occu.m1_lebi_pc, type = "state"))
est_psi_lebi_aru  <- plogis(coef(occu.m1_lebi_aru, type = "state"))
est_psi_lebi_comb <- plogis(coef(occu.m1_lebi_comb, type = "state"))

ci_psi_lebi_pc   <- confint(occu.m1_lebi_pc, type = "state")
ci_psi_lebi_aru  <- confint(occu.m1_lebi_aru, type = "state")
ci_psi_lebi_comb <- confint(occu.m1_lebi_comb, type = "state")

est_p_lebi_pc   <- plogis(coef(occu.m1_lebi_pc, type = "det"))
est_p_lebi_aru  <- plogis(coef(occu.m1_lebi_aru, type = "det"))
est_p_lebi_comb <- plogis(coef(occu.m1_lebi_comb, type = "det"))

ci_p_lebi_pc   <- confint(occu.m1_lebi_pc, type = "det")
ci_p_lebi_aru  <- confint(occu.m1_lebi_aru, type = "det")
ci_p_lebi_comb <- confint(occu.m1_lebi_comb, type = "det")
#psi
aru.data.lebi <- data.frame(
  Method = c("PC","ARU","ARU + PC"),
  Estimate = c(est_psi_lebi_pc,
               est_psi_lebi_aru,
               est_psi_lebi_comb),
  low_ci = c(boot::inv.logit(ci_psi_lebi_pc[1]),
             boot::inv.logit(ci_psi_lebi_aru[1]),
             boot::inv.logit(ci_psi_lebi_comb[1])),
  high_ci = c(boot::inv.logit(ci_psi_lebi_pc[2]),
              boot::inv.logit(ci_psi_lebi_aru[2]),
              boot::inv.logit(ci_psi_lebi_comb[2])))

aru.data.lebi$Method <- factor(
  aru.data.lebi$Method,
  levels = c("PC","ARU","ARU + PC"))

psi.lebi <- ggplot(aru.data.lebi, aes(x = Method, y = Estimate, group = Method)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(
    aes(ymin = low_ci, ymax = high_ci),
    position = position_dodge(width = 0.2),
    width = 0.2) +
  labs(title = "Least Bittern",
       x = "Method", y = "Occupancy") +
  theme_Publication() +
  theme_pubr() +
  labs_pubr() +
  theme(axis.title = element_text(size = 12),
        title = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0,1))

psi.lebi

#detection
aru.data.lebi <- data.frame(
  Method = c("PC","ARU","ARU + PC"),
  Estimate = c(est_p_lebi_pc,
               est_p_lebi_aru,
               est_p_lebi_comb),
  low_ci = c(boot::inv.logit(ci_p_lebi_pc[1]),
             boot::inv.logit(ci_p_lebi_aru[1]),
             boot::inv.logit(ci_p_lebi_comb[1])),
  high_ci = c(boot::inv.logit(ci_p_lebi_pc[2]),
              boot::inv.logit(ci_p_lebi_aru[2]),
              boot::inv.logit(ci_p_lebi_comb[2])))

aru.data.lebi$Method <- factor(
  aru.data.lebi$Method,
  levels = c("PC","ARU","ARU + PC"))

p.lebi <- ggplot(aru.data.lebi, aes(x = Method, y = Estimate, group = Method)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(
    aes(ymin = low_ci, ymax = high_ci),
    position = position_dodge(width = 0.2),
    width = 0.2) +
  labs(title = "Least Bittern",
       x = "Method", y = "Detection Probability", tag = "A") +
  theme_Publication() +
  theme_pubr() +
  labs_pubr() +
  theme(axis.title = element_text(size = 12),
        title = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0,1))

p.lebi

# AMCO -------------------------------------------------------------------------
est_psi_amco_pc   <- plogis(coef(occu.m1_amco_pc, type = "state"))
est_psi_amco_aru  <- plogis(coef(occu.m1_amco_aru, type = "state"))
est_psi_amco_comb <- plogis(coef(occu.m1_amco_comb, type = "state"))

ci_psi_amco_pc   <- confint(occu.m1_amco_pc, type = "state")
ci_psi_amco_aru  <- confint(occu.m1_amco_aru, type = "state")
ci_psi_amco_comb <- confint(occu.m1_amco_comb, type = "state")

est_p_amco_pc   <- plogis(coef(occu.m1_amco_pc, type = "det"))
est_p_amco_aru  <- plogis(coef(occu.m1_amco_aru, type = "det"))
est_p_amco_comb <- plogis(coef(occu.m1_amco_comb, type = "det"))

ci_p_amco_pc   <- confint(occu.m1_amco_pc, type = "det")
ci_p_amco_aru  <- confint(occu.m1_amco_aru, type = "det")
ci_p_amco_comb <- confint(occu.m1_amco_comb, type = "det")
#psi
aru.data.amco <- data.frame(
  Method = c("PC","ARU","ARU + PC"),
  Estimate = c(est_psi_amco_pc,
               est_psi_amco_aru,
               est_psi_amco_comb),
  low_ci = c(boot::inv.logit(ci_psi_amco_pc[1]),
             boot::inv.logit(ci_psi_amco_aru[1]),
             boot::inv.logit(ci_psi_amco_comb[1])),
  high_ci = c(boot::inv.logit(ci_psi_amco_pc[2]),
              boot::inv.logit(ci_psi_amco_aru[2]),
              boot::inv.logit(ci_psi_amco_comb[2])))

aru.data.amco$Method <- factor(
  aru.data.amco$Method,
  levels = c("PC","ARU","ARU + PC"))

psi.amco <- ggplot(aru.data.amco, aes(x = Method, y = Estimate, group = Method)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(
    aes(ymin = low_ci, ymax = high_ci),
    position = position_dodge(width = 0.2),
    width = 0.2) +
  labs(title = "American Coot",
       x = "Method", y = "Occupancy") +
  theme_Publication() +
  theme_pubr() +
  labs_pubr() +
  theme(axis.title = element_text(size = 12),
        title = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0,1))

psi.amco

#detection
aru.data.amco <- data.frame(
  Method = c("PC","ARU","ARU + PC"),
  Estimate = c(est_p_amco_pc,
               est_p_amco_aru,
               est_p_amco_comb),
  low_ci = c(boot::inv.logit(ci_p_amco_pc[1]),
             boot::inv.logit(ci_p_amco_aru[1]),
             boot::inv.logit(ci_p_amco_comb[1])),
  high_ci = c(boot::inv.logit(ci_p_amco_pc[2]),
              boot::inv.logit(ci_p_amco_aru[2]),
              boot::inv.logit(ci_p_amco_comb[2])))

aru.data.amco$Method <- factor(
  aru.data.amco$Method,
  levels = c("PC","ARU","ARU + PC"))

p.amco <- ggplot(aru.data.amco, aes(x = Method, y = Estimate, group = Method)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(
    aes(ymin = low_ci, ymax = high_ci),
    position = position_dodge(width = 0.2),
    width = 0.2) +
  labs(title = "American Coot",
       x = "Method", y = "Detection Probability", tag = "B") +
  theme_Publication() +
  theme_pubr() +
  labs_pubr() +
  theme(axis.title = element_text(size = 12),
        title = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0,1))

p.amco

# PBGR -------------------------------------------------------------------------
est_psi_pbgr_pc   <- plogis(coef(occu.m1_pbgr_pc, type = "state"))
est_psi_pbgr_aru  <- plogis(coef(occu.m1_pbgr_aru, type = "state"))
est_psi_pbgr_comb <- plogis(coef(occu.m1_pbgr_comb, type = "state"))

ci_psi_pbgr_pc   <- confint(occu.m1_pbgr_pc, type = "state")
ci_psi_pbgr_aru  <- confint(occu.m1_pbgr_aru, type = "state")
ci_psi_pbgr_comb <- confint(occu.m1_pbgr_comb, type = "state")

est_p_pbgr_pc   <- plogis(coef(occu.m1_pbgr_pc, type = "det"))
est_p_pbgr_aru  <- plogis(coef(occu.m1_pbgr_aru, type = "det"))
est_p_pbgr_comb <- plogis(coef(occu.m1_pbgr_comb, type = "det"))

ci_p_pbgr_pc   <- confint(occu.m1_pbgr_pc, type = "det")
ci_p_pbgr_aru  <- confint(occu.m1_pbgr_aru, type = "det")
ci_p_pbgr_comb <- confint(occu.m1_pbgr_comb, type = "det")
#psi
aru.data.pbgr <- data.frame(
  Method = c("PC","ARU","ARU + PC"),
  Estimate = c(est_psi_pbgr_pc,
               est_psi_pbgr_aru,
               est_psi_pbgr_comb),
  low_ci = c(boot::inv.logit(ci_psi_pbgr_pc[1]),
             boot::inv.logit(ci_psi_pbgr_aru[1]),
             boot::inv.logit(ci_psi_pbgr_comb[1])),
  high_ci = c(boot::inv.logit(ci_psi_pbgr_pc[2]),
              boot::inv.logit(ci_psi_pbgr_aru[2]),
              boot::inv.logit(ci_psi_pbgr_comb[2])))

aru.data.pbgr$Method <- factor(
  aru.data.pbgr$Method,
  levels = c("PC","ARU","ARU + PC"))

psi.pbgr <- ggplot(aru.data.pbgr, aes(x = Method, y = Estimate, group = Method)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(
    aes(ymin = low_ci, ymax = high_ci),
    position = position_dodge(width = 0.2),
    width = 0.2) +
  labs(title = "Pied-billed Grebe",
       x = "Method", y = "Occupancy") +
  theme_Publication() +
  theme_pubr() +
  labs_pubr() +
  theme(axis.title = element_text(size = 12),
        title = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0,1))

psi.pbgr

#detection
aru.data.pbgr <- data.frame(
  Method = c("PC","ARU","ARU + PC"),
  Estimate = c(est_p_pbgr_pc,
               est_p_pbgr_aru,
               est_p_pbgr_comb),
  low_ci = c(boot::inv.logit(ci_p_pbgr_pc[1]),
             boot::inv.logit(ci_p_pbgr_aru[1]),
             boot::inv.logit(ci_p_pbgr_comb[1])),
  high_ci = c(boot::inv.logit(ci_p_pbgr_pc[2]),
              boot::inv.logit(ci_p_pbgr_aru[2]),
              boot::inv.logit(ci_p_pbgr_comb[2])))

aru.data.pbgr$Method <- factor(
  aru.data.pbgr$Method,
  levels = c("PC","ARU","ARU + PC"))

p.pbgr <- ggplot(aru.data.pbgr, aes(x = Method, y = Estimate, group = Method)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(
    aes(ymin = low_ci, ymax = high_ci),
    position = position_dodge(width = 0.2),
    width = 0.2) +
  labs(title = "Pied-billed Grebe",
       x = "Method", y = "Detection Probability", tag = "C") +
  theme_Publication() +
  theme_pubr() +
  labs_pubr() +
  theme(axis.title = element_text(size = 12),
        title = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0,1))

p.pbgr

# SORA -------------------------------------------------------------------------
est_psi_sora_pc   <- plogis(coef(occu.m1_sora_pc, type = "state"))
est_psi_sora_aru  <- plogis(coef(occu.m1_sora_aru, type = "state"))
est_psi_sora_comb <- plogis(coef(occu.m1_sora_comb, type = "state"))

ci_psi_sora_pc   <- confint(occu.m1_sora_pc, type = "state")
ci_psi_sora_aru  <- confint(occu.m1_sora_aru, type = "state")
ci_psi_sora_comb <- confint(occu.m1_sora_comb, type = "state")

est_p_sora_pc   <- plogis(coef(occu.m1_sora_pc, type = "det"))
est_p_sora_aru  <- plogis(coef(occu.m1_sora_aru, type = "det"))
est_p_sora_comb <- plogis(coef(occu.m1_sora_comb, type = "det"))

ci_p_sora_pc   <- confint(occu.m1_sora_pc, type = "det")
ci_p_sora_aru  <- confint(occu.m1_sora_aru, type = "det")
ci_p_sora_comb <- confint(occu.m1_sora_comb, type = "det")
#psi
aru.data.sora <- data.frame(
  Method = c("PC","ARU","ARU + PC"),
  Estimate = c(est_psi_sora_pc,
               est_psi_sora_aru,
               est_psi_sora_comb),
  low_ci = c(boot::inv.logit(ci_psi_sora_pc[1]),
             boot::inv.logit(ci_psi_sora_aru[1]),
             boot::inv.logit(ci_psi_sora_comb[1])),
  high_ci = c(boot::inv.logit(ci_psi_sora_pc[2]),
              boot::inv.logit(ci_psi_sora_aru[2]),
              boot::inv.logit(ci_psi_sora_comb[2])))

aru.data.sora$Method <- factor(
  aru.data.sora$Method,
  levels = c("PC","ARU","ARU + PC"))

psi.sora <- ggplot(aru.data.sora, aes(x = Method, y = Estimate, group = Method)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(
    aes(ymin = low_ci, ymax = high_ci),
    position = position_dodge(width = 0.2),
    width = 0.2) +
  labs(title = "Sora",
       x = "Method", y = "Occupancy") +
  theme_Publication() +
  theme_pubr() +
  labs_pubr() +
  theme(axis.title = element_text(size = 12),
        title = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0,1))

psi.sora

#detection
aru.data.sora <- data.frame(
  Method = c("PC","ARU","ARU + PC"),
  Estimate = c(est_p_sora_pc,
               est_p_sora_aru,
               est_p_sora_comb),
  low_ci = c(boot::inv.logit(ci_p_sora_pc[1]),
             boot::inv.logit(ci_p_sora_aru[1]),
             boot::inv.logit(ci_p_sora_comb[1])),
  high_ci = c(boot::inv.logit(ci_p_sora_pc[2]),
              boot::inv.logit(ci_p_sora_aru[2]),
              boot::inv.logit(ci_p_sora_comb[2])))

aru.data.sora$Method <- factor(
  aru.data.sora$Method,
  levels = c("PC","ARU","ARU + PC"))

p.sora <- ggplot(aru.data.sora, aes(x = Method, y = Estimate, group = Method)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(
    aes(ymin = low_ci, ymax = high_ci),
    position = position_dodge(width = 0.2),
    width = 0.2) +
  labs(title = "Sora",
       x = "Method", y = "Detection Probability", tag = "D") +
  theme_Publication() +
  theme_pubr() +
  labs_pubr() +
  theme(axis.title = element_text(size = 12),
        title = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0,1))

p.sora

# combine plots

(lebi <- (p.lebi / psi.lebi))

(amco <-  (p.amco / psi.amco))

(pbgr <-(p.pbgr / psi.pbgr))

(sora <-  (p.sora / psi.sora))

all <- (lebi | amco | pbgr | sora) +
  plot_layout(axis_titles = "collect",
              guides = "collect",
              axes = "collect")
all



ggsave("./Plots/Figure 2.png", plot = all,
       width = 12,
       height = 8,
       dpi = 300,
       units = "in")
