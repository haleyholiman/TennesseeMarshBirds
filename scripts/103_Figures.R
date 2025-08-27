################################################################################L
#
##file name: 103_Figures.R
## Author: Haley Holiman
## Updated 7/8/2025
## Output: Figures for Part 1 of TN Marshbirds Paper
################################################################################
#setwd("C:/Users/SIU856584167/OneDrive - Southern Illinois University/Marshbirds/final_draft")
#source("./scripts/102_Models.R")
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
  labs(x = "Method", y = "Detection Probability") +
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

##colonization olots

aru.data.lebi <- data.frame(
  Method = c("ARU","ARU + PC"),
  Estimate = c(est_col_lebi_m2, est_col_lebi_m3),
  low_ci = c(boot::inv.logit(ci_col_lebi_m2[1]), boot::inv.logit(ci_col_lebi_m3[1])),
  high_ci = c(boot::inv.logit(ci_col_lebi_m2[2]), boot::inv.logit(ci_col_lebi_m3[2]))
)

aru.data.lebi$Method <- factor(aru.data.lebi$Method, levels = c("ARU","ARU + PC"))

col.lebi <- ggplot(aru.data.lebi, aes(x = Method, y = Estimate, group = Method)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(
    aes(ymin = low_ci, ymax = high_ci),
    position = position_dodge(width = 0.2),
    width = 0.2) +
  labs(title = "Least Bittern",
       x = "Method", y = "Colonization") +
  theme_Publication() +
  theme_pubr() +
  labs_pubr() +
  theme(axis.title = element_text(size = 8),
        title = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  scale_y_continuous(limits = c(0,1))

col.lebi

# ggsave("./Plots/ch1/col_compare_lebi.png", plot = col.lebi,
#        width = 3.5,
#        height = 3.5,
#        dpi = 300,
#        units = "in")


#extinction plots
aru.data.lebi <- data.frame(
  Method = c("ARU","ARU + PC"),
  Estimate = c(est_ext_lebi_m2, est_ext_lebi_m3),
  low_ci = c(boot::inv.logit(ci_ext_lebi_m2[1]), boot::inv.logit(ci_ext_lebi_m3[1])),
  high_ci = c(boot::inv.logit(ci_ext_lebi_m2[2]), boot::inv.logit(ci_ext_lebi_m3[2]))
)

aru.data.lebi$Method <- factor(aru.data.lebi$Method, levels = c("ARU","ARU + PC"))

ext.lebi <- ggplot(aru.data.lebi, aes(x = Method, y = Estimate, group = Method)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(
    aes(ymin = low_ci, ymax = high_ci),
    position = position_dodge(width = 0.2),
    width = 0.2) +
  labs(x = "Method", y = "Extinction") +
  theme_Publication() +
  theme_pubr() +
  labs_pubr() +
  theme(axis.title = element_text(size = 8),
        title = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  scale_y_continuous(limits = c(0,1))

ext.lebi

# ggsave("./Plots/ch1/ext_compare_lebi.png", plot = ext.lebi,
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
  labs(x = "Method", y = "Detection Probability") +
  theme_Publication() +
  theme_pubr() +
  labs_pubr() +
  theme(axis.title = element_text(size = 8),
        title = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  scale_y_continuous(limits = c(0,1))

p.amco

# ggsave("./Plots/ch1/p_compare_amco.png", plot = p.amco,
#        width = 3.5,
#        height = 3.5,
#        dpi = 300,
#        units = "in")

##colonization from dynamic 

aru.data.amco <- data.frame(
  Method = c("ARU","ARU + PC"),
  Estimate = c(est_col_amco_m2, est_col_amco_m3),
  low_ci = c(boot::inv.logit(ci_col_amco_m2[1]), boot::inv.logit(ci_col_amco_m3[1])),
  high_ci = c(boot::inv.logit(ci_col_amco_m2[2]), boot::inv.logit(ci_col_amco_m3[2]))
)

aru.data.amco$Method <- factor(aru.data.amco$Method, levels = c("ARU","ARU + PC"))

col.amco <- ggplot(aru.data.amco, aes(x = Method, y = Estimate, group = Method)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(
    aes(ymin = low_ci, ymax = high_ci),
    position = position_dodge(width = 0.2),
    width = 0.2) +
  labs(title = "American Coot",
       x = "Method", y = "Colonization") +
  theme_Publication() +
  theme_pubr() +
  labs_pubr() +
  theme(axis.title = element_text(size = 8),
        title = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  scale_y_continuous(limits = c(0,1))

col.amco

# ggsave("./Plots/ch1/col_compare_amco.png", plot = col.amco,
#        width = 3.5,
#        height = 3.5,
#        dpi = 300,
#        units = "in")


#extinction 
aru.data.amco <- data.frame(
  Method = c("ARU","ARU + PC"),
  Estimate = c(est_ext_amco_m2, est_ext_amco_m3),
  low_ci = c(boot::inv.logit(ci_ext_amco_m2[1]), boot::inv.logit(ci_ext_amco_m3[1])),
  high_ci = c(boot::inv.logit(ci_ext_amco_m2[2]), boot::inv.logit(ci_ext_amco_m3[2]))
)

aru.data.amco$Method <- factor(aru.data.amco$Method, levels = c("ARU","ARU + PC"))

ext.amco <- ggplot(aru.data.amco, aes(x = Method, y = Estimate, group = Method)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(
    aes(ymin = low_ci, ymax = high_ci),
    position = position_dodge(width = 0.2),
    width = 0.2) +
  labs(x = "Method", y = "Extinction") +
  theme_Publication() +
  theme_pubr() +
  labs_pubr() +
  theme(axis.title = element_text(size = 8),
        title = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  scale_y_continuous(limits = c(0,1))

ext.amco

# ggsave("./Plots/ch1/ext_compare_amco.png", plot = ext.amco,
#        width = 3.5,
#        height = 3.5,
#        dpi = 300,
#        units = "in")

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
  labs(x = "Method", y = "Detection Probability") +
  theme_Publication() +
  theme_pubr() +
  labs_pubr() +
  theme(axis.title = element_text(size = 8),
        title = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  scale_y_continuous(limits = c(0,1))

p.pbgr

# ggsave("./Plots/ch1/p_compare_pbgr.png", plot = p.pbgr,
#        width = 3.5,
#        height = 3.5,
#        dpi = 300,
#        units = "in")

##colonization from dynamic 

aru.data.pbgr <- data.frame(
  Method = c("ARU","ARU + PC"),
  Estimate = c(est_col_pbgr_m2, est_col_pbgr_m3),
  low_ci = c(boot::inv.logit(ci_col_pbgr_m2[1]), boot::inv.logit(ci_col_pbgr_m3[1])),
  high_ci = c(boot::inv.logit(ci_col_pbgr_m2[2]), boot::inv.logit(ci_col_pbgr_m3[2]))
)

aru.data.pbgr$Method <- factor(aru.data.pbgr$Method, levels = c("ARU","ARU + PC"))

col.pbgr <- ggplot(aru.data.pbgr, aes(x = Method, y = Estimate, group = Method)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(
    aes(ymin = low_ci, ymax = high_ci),
    position = position_dodge(width = 0.2),
    width = 0.2) +
  labs(title = "Pied-billed Grebe",
       x = "Method", y = "Colonization") +
  theme_Publication() +
  theme_pubr() +
  labs_pubr() +
  theme(axis.title = element_text(size = 8),
        title = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  scale_y_continuous(limits = c(0,1))

col.pbgr

# ggsave("./Plots/ch1/col_compare_pbgr.png", plot = col.pbgr,
#        width = 3.5,
#        height = 3.5,
#        dpi = 300,
#        units = "in")


#extinction 
aru.data.pbgr <- data.frame(
  Method = c("ARU","ARU + PC"),
  Estimate = c(est_ext_pbgr_m2, est_ext_pbgr_m3),
  low_ci = c(boot::inv.logit(ci_ext_pbgr_m2[1]), boot::inv.logit(ci_ext_pbgr_m3[1])),
  high_ci = c(boot::inv.logit(ci_ext_pbgr_m2[2]), boot::inv.logit(ci_ext_pbgr_m3[2]))
)

aru.data.pbgr$Method <- factor(aru.data.pbgr$Method, levels = c("ARU","ARU + PC"))

ext.pbgr <- ggplot(aru.data.pbgr, aes(x = Method, y = Estimate, group = Method)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(
    aes(ymin = low_ci, ymax = high_ci),
    position = position_dodge(width = 0.2),
    width = 0.2) +
  labs(x = "Method", y = "Extinction") +
  theme_Publication() +
  theme_pubr() +
  labs_pubr() +
  theme(axis.title = element_text(size = 8),
        title = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  scale_y_continuous(limits = c(0,1))

ext.pbgr

# ggsave("./Plots/ch1/ext_compare_pbgr.png", plot = ext.pbgr,
#        width = 3.5,
#        height = 3.5,
#        dpi = 300,
#        units = "in")

#SORA --------------------------------------------------------------------------

aru.data.sora <- data.frame(
  Method = c("ARU","ARU + PC"),
  Estimate = c(est_psi_sora_m2, est_psi_sora_m3),
  low_ci = c(boot::inv.logit(ci_psi_sora_m2[1]), boot::inv.logit(ci_psi_sora_m3[1])),
  high_ci = c(boot::inv.logit(ci_psi_sora_m2[2]), boot::inv.logit(ci_psi_sora_m3[2]))
)

aru.data.sora$Method <- factor(aru.data.sora$Method, levels = c("ARU","ARU + PC"))

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

# ggsave("./Plots/ch1/psi_compare_sora.png", plot = psi.sora,
#        width = 3.5,
#        height = 3.5,
#        dpi = 300,
#        units = "in")

aru.data.sora <- data.frame(
  Method = c("ARU","ARU + PC"),
  Estimate = c(est_p_sora_m2, est_p_sora_m3),
  low_ci = c(boot::inv.logit(ci_p_sora_m2[1]), boot::inv.logit(ci_p_sora_m3[1])),
  high_ci = c(boot::inv.logit(ci_p_sora_m2[2]), boot::inv.logit(ci_p_sora_m3[2]))
)

aru.data.sora$Method <- factor(aru.data.sora$Method, levels = c("ARU","ARU + PC"))

p.sora <- ggplot(aru.data.sora, aes(x = Method, y = Estimate, group = Method)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(
    aes(ymin = low_ci, ymax = high_ci),
    position = position_dodge(width = 0.2),
    width = 0.2) +
  labs(x = "Method", y = "Detection Probability") +
  theme_Publication() +
  theme_pubr() +
  labs_pubr() +
  theme(axis.title = element_text(size = 8),
        title = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  scale_y_continuous(limits = c(0,1))

p.sora

# ggsave("./Plots/ch1/p_compare_sora.png", plot = p.sora,
#        width = 3.5,
#        height = 3.5,
#        dpi = 300,
#        units = "in")

##colonization from dynamic 

aru.data.sora <- data.frame(
  Method = c("ARU","ARU + PC"),
  Estimate = c(est_col_sora_m2, est_col_sora_m3),
  low_ci = c(boot::inv.logit(ci_col_sora_m2[1]), boot::inv.logit(ci_col_sora_m3[1])),
  high_ci = c(boot::inv.logit(ci_col_sora_m2[2]), boot::inv.logit(ci_col_sora_m3[2]))
)

aru.data.sora$Method <- factor(aru.data.sora$Method, levels = c("ARU","ARU + PC"))

col.sora <- ggplot(aru.data.sora, aes(x = Method, y = Estimate, group = Method)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(
    aes(ymin = low_ci, ymax = high_ci),
    position = position_dodge(width = 0.2),
    width = 0.2) +
  labs(title = "Sora",
       x = "Method", y = "Colonization") +
  theme_Publication() +
  theme_pubr() +
  labs_pubr() +
  theme(axis.title = element_text(size = 8),
        title = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  scale_y_continuous(limits = c(0,1))

col.sora

# ggsave("./Plots/ch1/col_compare_sora.png", plot = col.sora,
#        width = 3.5,
#        height = 3.5,
#        dpi = 300,
#        units = "in")


#extinction 
aru.data.sora <- data.frame(
  Method = c("ARU","ARU + PC"),
  Estimate = c(est_ext_sora_m2, est_ext_sora_m3),
  low_ci = c(boot::inv.logit(ci_ext_sora_m2[1]), boot::inv.logit(ci_ext_sora_m3[1])),
  high_ci = c(boot::inv.logit(ci_ext_sora_m2[2]), boot::inv.logit(ci_ext_sora_m3[2]))
)

aru.data.sora$Method <- factor(aru.data.sora$Method, levels = c("ARU","ARU + PC"))

ext.sora <- ggplot(aru.data.sora, aes(x = Method, y = Estimate, group = Method)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(
    aes(ymin = low_ci, ymax = high_ci),
    position = position_dodge(width = 0.2),
    width = 0.2) +
  labs(x = "Method", y = "Extinction") +
  theme_Publication() +
  theme_pubr() +
  labs_pubr() +
  theme(axis.title = element_text(size = 8),
        title = element_text(size = 10),
        axis.text = element_text(size = 8)) +
  scale_y_continuous(limits = c(0,1))

ext.sora

# ggsave("./Plots/ch1/ext_compare_sora.png", plot = ext.sora,
#        width = 3.5,
#        height = 7,
#        dpi = 300,
#        units = "in")


# Combine plots ----------------------------------------------------------------

psi<- (psi.lebi / psi.amco / psi.pbgr / psi.sora) + plot_annotation(tag_levels = "A")

psi

# ggsave("./Plots/ch1/psi_allspp.png", plot = psi,
#        width = 4.25,
#        height = 5.5,
#        dpi = 300,
#        units = "in")

p<- (p.lebi / p.amco / p.pbgr / p.sora) + plot_annotation(tag_levels = "A")

p

# ggsave("./Plots/ch1/p_allspp.png", plot = p,
#        width = 4.25,
#        height = 5.5,
#        dpi = 300,
#        units = "in")

col<- (col.lebi / col.amco / col.pbgr / col.sora) + plot_annotation(tag_levels = "A")

col

# ggsave("./Plots/ch1/col_allspp.png", plot = col,
#        width = 4.25,
#        height = 5.5,
#        dpi = 300,
#        units = "in")

ext<- (ext.lebi / ext.amco / ext.pbgr / ext.sora) + plot_annotation(tag_levels = "A")

ext

# ggsave("./Plots/ch1/ext_allspp.png", plot = ext,
#        width = 4.25,
#        height = 5.5,
#        dpi = 300,
#        units = "in")



lebi <- (psi.lebi / p.lebi / col.lebi / ext.lebi)

lebi

# ggsave("./Plots/ch1/lebi_all.png", plot = test,
#        width = 4.25,
#        height = 5.5,
#        dpi = 300,
#        units = "in")


amco <- (psi.amco / p.amco / col.amco / ext.amco)

amco

# ggsave("./Plots/ch1/amco_all.png", plot = amco,
#        width = 4.25,
#        height = 5.5,
#        dpi = 300,
#        units = "in")

pbgr <- (psi.pbgr / p.pbgr / col.pbgr / ext.pbgr)

pbgr

# ggsave("./Plots/ch1/pbgr_all.png", plot = pbgr,
#        width = 4.25,
#        height = 5.5,
#        dpi = 300,
#        units = "in")

sora <- (psi.sora / p.sora / col.sora / ext.sora)

sora

# ggsave("./Plots/ch1/sora_all.png", plot = sora,
#        width = 4.25,
#        height = 5.5,
#        dpi = 300,
#        units = "in")



# occupancy and det prob plots ------------------------------------------------
## Figure 2 in manuscript
(lebi <- (psi.lebi / p.lebi))

(amco <-  (psi.amco / p.amco))

(pbgr <-(psi.pbgr / p.pbgr))

(sora <-  (psi.sora / p.sora))

all <- (lebi | amco | pbgr | sora) +
  plot_layout(axis_titles = "collect",
              guides = "collect",
              axes = "collect") 
all

all2 <- (lebi + amco + pbgr + sora) +
  plot_layout(axis_titles = "collect",
              guides = "collect",
              axes = "collect")
all2

ggsave("./plots/Figure2.png", plot = all,
       width = 7,
       height = 6,
       dpi = 300,
       units = "in")



# alternative options
# 
# (ocu <- (psi.lebi | p.lebi) / (psi.amco | p.amco) / (psi.pbgr | p.pbgr) / 
#     (psi.sora | p.sora) +
#   plot_layout(axis_titles = "collect",
#               guides = "collect",
#               axes = "collect"))
# 
# ggsave("./plots/Figure2v2.png", plot = ocu,
#        width = 6,
#        height = 10,
#        dpi = 300)
# # colonization and extinction plots --------------------------------------------
# ## Figure 3 in manuscript
# (lebi <- (col.lebi / ext.lebi))
# 
# (amco <-  (col.amco / ext.amco))
# 
# (pbgr <-(col.pbgr / ext.pbgr))
# 
# (sora <-  (col.sora / ext.sora))
# 
# all <- (lebi | amco | pbgr | sora) +
#   plot_layout(axis_titles = "collect",
#               guides = "collect",
#               axes = "collect") 
# all
# 
# ggsave("./plots/Figure3.png", plot = all,
#        width = 7,
#        height = 6,
#        dpi = 300,
#        units = "in")
# 
# (col <- (col.lebi | ext.lebi) / (col.amco | ext.amco) / (col.pbgr | ext.pbgr) / 
#     (col.sora | ext.sora) +
#     plot_layout(axis_titles = "collect",
#                 guides = "collect",
#                 axes = "collect"))
# 
# ggsave("./plots/Figure3v2.png", plot = col,
#        width = 6,
#        height = 10,
#        dpi = 300)