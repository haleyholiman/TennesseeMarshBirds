################################################################################L
#
##file name: 103_Figures.R
## Author: Haley Holiman
## Updated 8/8/2025
## Output: Figures for Part 1 of TN Marshbirds Paper
################################################################################
#setwd("C:/Users/SIU856584167/OneDrive - Southern Illinois University/Marshbirds/final_draft")
source("./scripts/102_Models.R")
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


#LEBI --------------------------------------------------------------------------

aru.data.lebi <- data.frame(
  Method = c("PC","ARU","ARU + PC"),
  Estimate = c(est_psi_lebi_m1,est_psi_lebi_m2, est_psi_lebi_m3),
  low_ci = c(boot::inv.logit(ci_psi_lebi_m1[1]),boot::inv.logit(ci_psi_lebi_m2[1]), boot::inv.logit(ci_psi_lebi_m3[1])),
  high_ci = c(boot::inv.logit(ci_psi_lebi_m1[2]),boot::inv.logit(ci_psi_lebi_m2[2]), boot::inv.logit(ci_psi_lebi_m3[2]))
)

aru.data.lebi$Method <- factor(aru.data.lebi$Method, levels = c("PC","ARU","ARU + PC"))

#occupancy plot
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
  theme(axis.title = element_text(size = 8),
        title = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  scale_y_continuous(limits = c(0,1))

psi.lebi

# ggsave("./Plots/ch1/psi_compare_lebi.png", plot = psi.lebi,
#        width = 3.5,
#        height = 3.5,
#        dpi = 300,
#        units = "in")

aru.data.lebi <- data.frame(
  Method = c("PC","ARU","ARU + PC"),
  Estimate = c(est_p_lebi_m1,est_p_lebi_m2, est_p_lebi_m3),
  low_ci = c(boot::inv.logit(ci_p_lebi_m1[1]),boot::inv.logit(ci_p_lebi_m2[1]), boot::inv.logit(ci_p_lebi_m3[1])),
  high_ci = c(boot::inv.logit(ci_p_lebi_m1[2]),boot::inv.logit(ci_p_lebi_m2[2]), boot::inv.logit(ci_p_lebi_m3[2]))
)

aru.data.lebi$Method <- factor(aru.data.lebi$Method, levels = c("PC","ARU","ARU + PC"))

#detection probability plot
p.lebi <- ggplot(aru.data.lebi, aes(x = Method, y = Estimate, group = Method)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(
    aes(ymin = low_ci, ymax = high_ci),
    position = position_dodge(width = 0.2),
    width = 0.2) +
  labs(title = "Least Bittern",x = "Method", y = "Detection Probability") +
  theme_Publication() +
  theme_pubr() +
  labs_pubr() +
  theme(axis.title = element_text(size = 8),
        title = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  scale_y_continuous(limits = c(0,1))

p.lebi

# ggsave("./Plots/ch1/p_compare_lebi.png", plot = p.lebi,
#        width = 3.5,
#        height = 3.5,
#        dpi = 300,
#        units = "in")




#AMCO --------------------------------------------------------------------------
aru.data.amco <- data.frame(
  Method = c("PC","ARU","ARU + PC"),
  Estimate = c(est_psi_amco_m1,est_psi_amco_m2, est_psi_amco_m3),
  low_ci = c(boot::inv.logit(ci_psi_amco_m1[1]),boot::inv.logit(ci_psi_amco_m2[1]), boot::inv.logit(ci_psi_amco_m3[1])),
  high_ci = c(boot::inv.logit(ci_psi_amco_m1[2]),boot::inv.logit(ci_psi_amco_m2[2]), boot::inv.logit(ci_psi_amco_m3[2]))
)

aru.data.amco$Method <- factor(aru.data.amco$Method, levels = c("PC","ARU","ARU + PC"))

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
  theme(axis.title = element_text(size = 8),
        title = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  scale_y_continuous(limits = c(0,1))

psi.amco

# ggsave("./Plots/ch1/psi_compare_amco.png", plot = psi.amco,
#        width = 3.5,
#        height = 3.5,
#        dpi = 300,
#        units = "in")

aru.data.amco <- data.frame(
  Method = c("PC","ARU","ARU + PC"),
  Estimate = c(est_p_amco_m1,est_p_amco_m2, est_p_amco_m3),
  low_ci = c(boot::inv.logit(ci_p_amco_m1[1]),boot::inv.logit(ci_p_amco_m2[1]), boot::inv.logit(ci_p_amco_m3[1])),
  high_ci = c(boot::inv.logit(ci_p_amco_m1[2]),boot::inv.logit(ci_p_amco_m2[2]), boot::inv.logit(ci_p_amco_m3[2]))
)

aru.data.amco$Method <- factor(aru.data.amco$Method, levels = c("PC","ARU","ARU + PC"))

p.amco <- ggplot(aru.data.amco, aes(x = Method, y = Estimate, group = Method)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(
    aes(ymin = low_ci, ymax = high_ci),
    position = position_dodge(width = 0.2),
    width = 0.2) +
  labs(title = "American Coot",x = "Method", y = "Detection Probability") +
  theme_Publication() +
  theme_pubr() +
  labs_pubr() +
  theme(axis.title = element_text(size = 8),
        title = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  scale_y_continuous(limits = c(0,1))

p.amco



#PBGR --------------------------------------------------------------------------
aru.data.pbgr <- data.frame(
  Method = c("PC","ARU","ARU + PC"),
  Estimate = c(est_psi_pbgr_m1,est_psi_pbgr_m2, est_psi_pbgr_m3),
  low_ci = c(boot::inv.logit(ci_psi_pbgr_m1[1]),boot::inv.logit(ci_psi_pbgr_m2[1]), boot::inv.logit(ci_psi_pbgr_m3[1])),
  high_ci = c(boot::inv.logit(ci_psi_pbgr_m1[2]),boot::inv.logit(ci_psi_pbgr_m2[2]), boot::inv.logit(ci_psi_pbgr_m3[2]))
)

aru.data.pbgr$Method <- factor(aru.data.pbgr$Method, levels = c("PC","ARU","ARU + PC"))

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
  theme(axis.title = element_text(size = 8),
        title = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  scale_y_continuous(limits = c(0,1))

psi.pbgr

# ggsave("./Plots/ch1/psi_compare_pbgr.png", plot = psi.pbgr,
#        width = 3.5,
#        height = 3.5,
#        dpi = 300,
#        units = "in")

aru.data.pbgr <- data.frame(
  Method = c("PC","ARU","ARU + PC"),
  Estimate = c(est_p_pbgr_m1,est_p_pbgr_m2, est_p_pbgr_m3),
  low_ci = c(boot::inv.logit(ci_p_pbgr_m1[1]),boot::inv.logit(ci_p_pbgr_m2[1]), boot::inv.logit(ci_p_pbgr_m3[1])),
  high_ci = c(boot::inv.logit(ci_p_pbgr_m1[2]),boot::inv.logit(ci_p_pbgr_m2[2]), boot::inv.logit(ci_p_pbgr_m3[2]))
)

aru.data.pbgr$Method <- factor(aru.data.pbgr$Method, levels = c("PC","ARU","ARU + PC"))

p.pbgr <- ggplot(aru.data.pbgr, aes(x = Method, y = Estimate, group = Method)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(
    aes(ymin = low_ci, ymax = high_ci),
    position = position_dodge(width = 0.2),
    width = 0.2) +
  labs(title = "Pied-billed Grebe",x = "Method", y = "Detection Probability") +
  theme_Publication() +
  theme_pubr() +
  labs_pubr() +
  theme(axis.title = element_text(size = 8),
        title = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  scale_y_continuous(limits = c(0,1))

p.pbgr



#SORA --------------------------------------------------------------------------

aru.data.sora <- data.frame(
  Method = c("PC","ARU","ARU + PC"),
  Estimate = c(est_psi_sora_m1,est_psi_sora_m2, est_psi_sora_m3),
  low_ci = c(boot::inv.logit(ci_psi_sora_m1[1]),boot::inv.logit(ci_psi_sora_m2[1]), boot::inv.logit(ci_psi_sora_m3[1])),
  high_ci = c(boot::inv.logit(ci_psi_sora_m1[2]),boot::inv.logit(ci_psi_sora_m2[2]), boot::inv.logit(ci_psi_sora_m3[2]))
)

aru.data.sora$Method <- factor(aru.data.sora$Method, levels = c("PC","ARU","ARU + PC"))

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
  theme(axis.title = element_text(size = 8),
        title = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  scale_y_continuous(limits = c(0,1))

psi.sora



aru.data.sora <- data.frame(
  Method = c("PC","ARU","ARU + PC"),
  Estimate = c(est_p_sora_m1,est_p_sora_m2, est_p_sora_m3),
  low_ci = c(boot::inv.logit(ci_p_sora_m1[1]),boot::inv.logit(ci_p_sora_m2[1]), boot::inv.logit(ci_p_sora_m3[1])),
  high_ci = c(boot::inv.logit(ci_p_sora_m1[2]),boot::inv.logit(ci_p_sora_m2[2]), boot::inv.logit(ci_p_sora_m3[2]))
)

aru.data.sora$Method <- factor(aru.data.sora$Method, levels = c("PC","ARU","ARU + PC"))

p.sora <- ggplot(aru.data.sora, aes(x = Method, y = Estimate, group = Method)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(
    aes(ymin = low_ci, ymax = high_ci),
    position = position_dodge(width = 0.2),
    width = 0.2) +
  labs(title = "Sora",x = "Method", y = "Detection Probability") +
  theme_Publication() +
  theme_pubr() +
  labs_pubr() +
  theme(axis.title = element_text(size = 8),
        title = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  scale_y_continuous(limits = c(0,1))

p.sora
# Combine plots ----------------------------------------------------------------


(lebi <- (psi.lebi / p.lebi))

(amco <-  (psi.amco / p.amco))

(pbgr <-(psi.pbgr / p.pbgr))

(sora <-  (psi.sora / p.sora))

all <- (lebi | amco | pbgr | sora) +
  plot_layout(axis_titles = "collect",
              guides = "collect",
              axes = "collect") 
all



ggsave("./Plots/objective1_final.png", plot = all,
       width = 8,
       height = 5,
       dpi = 300,
       units = "in")


#Just detection probability
det_all <- (p.lebi | p.amco | p.pbgr | p.sora) +
  plot_layout(axis_titles = "collect",
              guides = "collect",
              axes = "collect") 
det_all

ggsave("./Plots/objective1_detection.png", plot = det_all,
       width = 8,
       height = 5,
       dpi = 300,
       units = "in")
#Just occupancy probability