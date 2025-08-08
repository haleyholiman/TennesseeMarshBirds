################################################################################L
# 
## file name: 101_DataPrep.R
## Author: Haley Holiman
## Updated 7/8/2025
## Output: Code to clean up PC and ARU data for Part 1 of TN Marshbirds Paper
################################################################################L
setwd("C:/Users/SIU856584167/OneDrive - Southern Illinois University/Marshbirds/final_draft")

#package version control
# library(renv)
# #init()
# restore() #download and install packages and versions used

library(tidyverse)

#1.Point Count Data ------------------------------------------------------------

#load 2022 point count data
pc1 <- read.csv("./data/pointcountdata_2022.csv")

#create a new column for unique site IDs
pc1 <- pc1 %>% 
  mutate(Site_id = paste0(pc1$SITE, pc1$POINT, sep = "")) %>% 
  mutate(across(everything(), ~ifelse(is.na(.), 0, .)))

#create encounter histories for each focal species

species <- c("LEBI","AMCO","PBGR","SORA")
enc_hist_1 <- list()
for(s in species){
  enc_hist_species <- pc1 %>% 
    filter(SPECIES_CODE == s) %>% 
    distinct(pick(BOUT, SITE, POINT), .keep_all = TRUE) %>%
    bind_rows(anti_join(expand_grid(BOUT = 1:3, Site_id = unique(pc1$Site_id)), .)) %>%
    mutate(detections = ifelse(is.na(SPECIES_CODE), 0, 1)) %>%
    pivot_wider(id_cols = Site_id,
                names_from = BOUT,
                names_prefix = "Occ",
                values_from = detections,
                values_fill = 0) %>%
    arrange(Site_id) %>%
    column_to_rownames("Site_id")
  
  enc_hist_1[[s]] <- enc_hist_species
}

rm(enc_hist_species,pc1)

#repeat for 2023 point count data
pc2 <- read.csv("./data/pointcountdata_2023.csv")

pc2 <- pc2 %>% 
  mutate(Site_id = paste0(pc2$SITE, pc2$POINT, sep = "")) %>% 
  mutate(across(everything(), ~ifelse(is.na(.), 0, .)))

species <- c("LEBI","AMCO","PBGR","SORA")
enc_hist_2 <- list()
for(s in species){
  enc_hist_species <- pc2 %>% 
    filter(SPECIES_CODE == s) %>% 
    distinct(pick(BOUT, SITE, POINT), .keep_all = TRUE) %>%
    bind_rows(anti_join(expand_grid(BOUT = 1:3, Site_id = unique(pc2$Site_id)), .)) %>%
    mutate(detections = ifelse(is.na(SPECIES_CODE), 0, 1)) %>%
    pivot_wider(id_cols = Site_id,
                names_from = BOUT,
                names_prefix = "Occ",
                values_from = detections,
                values_fill = 0) %>%
    arrange(Site_id) %>%
    column_to_rownames("Site_id")
  
  enc_hist_2[[s]] <- enc_hist_species
}

rm(enc_hist_species,pc2)

fixnames <- enc_hist_2 #will be used later to clean up col names

#combine both years
enc_hist_pc <- list()

for(s in species) {
  
  combo <- rbind(enc_hist_1[[s]], enc_hist_2[[s]])
  
  enc_hist_pc[[s]] <- combo
}

rm(enc_hist_1, enc_hist_2, combo)

print(enc_hist_pc) #Detection history for each survey point and species 
str(enc_hist_pc)
#2.ARU DATA --------------------------------------------------------------------

#aru detections from birdnet, detections column refers to manual validation
## where 1 = true positive, 0 - false positive
allrounds <- read.csv("./data/allrounds_standarizedrecordingdays_2022.csv")
allrounds$Detections <- as.numeric(allrounds$Detections)
allrounds$Detections <- replace_na(allrounds$Detections,0)

#make encounter history from aru detections

#give unique site id
allround <- allrounds %>% 
  mutate(Site_id = paste0(allrounds$study_area, allrounds$site, sep = ""))

species <- unique(allround$Common.Name)
enc_hist_aru <- list()
for(s in species){
  
  enc_hist <- allround %>% 
    filter(Common.Name == s) %>% 
    #mutate(Detections = ifelse(is.na(Detections), 0, Detections)) %>% 
    group_by(Date,Site_id,round,recording_day) %>% 
    summarise(count = max(as.numeric(Detections), na.rm = TRUE)) %>% 
    arrange(Site_id,Date,round,recording_day)
  
  enc_hist2 <- enc_hist %>% 
    pivot_wider(id_cols = Site_id,
                names_from = c("recording_day","round"),
                names_prefix = "Occ",
                values_from = count) %>% 
    mutate(across(everything(), ~ifelse(is.null(.), 0, .))) %>% 
    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>% 
    column_to_rownames("Site_id")
  
  enc_hist_aru[[s]] <- enc_hist2
  
}

#clean up and make sure column names match
enc_hist_aru <- enc_hist_aru[-6] #get rid of this 
enc_hist_aru <- enc_hist_aru[-7]
#format so they are all the same length 
## 41 sites, 15 columns 
enc_hist_aru$`Least Bittern`[39,] <- NA
enc_hist_aru$`Least Bittern`[40,] <- NA
enc_hist_aru$`Least Bittern`[41,] <- NA
enc_hist_aru$`Least Bittern` <- enc_hist_aru$`Least Bittern`[, -c(16:19)]

fix <- c("Occ1_R1","Occ2_R1","Occ3_R1","Occ4_R1","Occ5_R1",
         "Occ1_R2","Occ2_R2","Occ3_R2","Occ4_R2","Occ5_R2",
         "Occ1_R3","Occ2_R3","Occ3_R3","Occ4_R3","Occ5_R3")
enc_hist_aru$`Least Bittern` <- enc_hist_aru$`Least Bittern`[,fix]


enc_hist_aru$`Common Gallinule`[38,] <- NA
enc_hist_aru$`Common Gallinule`[39,] <- NA
enc_hist_aru$`Common Gallinule`[40,] <- NA
enc_hist_aru$`Common Gallinule`[41,] <- NA
enc_hist_aru$`Common Gallinule` <- enc_hist_aru$`Common Gallinule`[, -c(16:17)]
enc_hist_aru$`Common Gallinule` <- enc_hist_aru$`Common Gallinule`[,fix]

enc_hist_aru$`American Coot`[37,] <- NA
enc_hist_aru$`American Coot`[38,] <- NA
enc_hist_aru$`American Coot`[39,] <- NA
enc_hist_aru$`American Coot`[40,] <- NA
enc_hist_aru$`American Coot`[41,] <- NA

desired_order_american_coot <- c("Occ1_R1","Occ2_R1","Occ3_R1","Occ4_R1","Occ5_R1",
                                 "Occ1_R2","Occ2_R2","Occ3_R2","Occ4_R2","Occ5_R2",
                                 "Occ1_R3","Occ2_R3","Occ3_R3","Occ5_R3",
                                 "Occ6_R1","Occ7_R1","Occ6_R2")

# Subset and reorder columns for American Coot dataframe
enc_hist_aru$`American Coot` <- enc_hist_aru$`American Coot`[,desired_order_american_coot]
enc_hist_aru$`American Coot` <- enc_hist_aru$`American Coot`[,-c(16:17)]

enc_hist_aru$Sora[36,] <- NA
enc_hist_aru$Sora[37,] <- NA
enc_hist_aru$Sora[38,] <- NA
enc_hist_aru$Sora[39,] <- NA
enc_hist_aru$Sora[40,] <- NA
enc_hist_aru$Sora[41,] <- NA
enc_hist_aru$Sora <- enc_hist_aru$Sora[,fix]


enc_hist_aru$`Pied-billed Grebe`[41,] <- NA
desired_order <- c("Occ1_R1","Occ2_R1","Occ3_R1","Occ4_R1","Occ5_R1",
                   "Occ1_R2","Occ2_R2","Occ3_R2","Occ4_R2","Occ5_R2",
                   "Occ1_R3","Occ2_R3","Occ3_R3","Occ4_R3","Occ5_R3",
                   "Occ6_R1","Occ6_R2","Occ6_R3")

# Subset and reorder columns
enc_hist_aru$`Pied-billed Grebe` <- enc_hist_aru$`Pied-billed Grebe`[, desired_order]
enc_hist_aru$`Pied-billed Grebe` <- enc_hist_aru$`Pied-billed Grebe`[, -c(16:18)]

enc_hist_aru$`King Rail`[41,] <- NA
desired_order_king_rail <- c("Occ1_R1","Occ2_R1","Occ3_R1","Occ4_R1","Occ5_R1",
                             "Occ1_R2","Occ2_R2","Occ3_R2","Occ4_R2","Occ5_R2",
                             "Occ1_R3","Occ2_R3","Occ3_R3","Occ4_R3","Occ5_R3",
                             "Occ6_R1","Occ6_R2","Occ6_R3")
enc_hist_aru$`King Rail` <- enc_hist_aru$`King Rail`[, desired_order_king_rail]
enc_hist_aru$`King Rail` <- enc_hist_aru$`King Rail`[, -c(16:18)]

enc_hist_1 <- enc_hist_aru
rm(allround,allrounds,enc_hist, enc_hist2, enc_hist_aru)


#repeat for 2023 data
allrounds_23 <- read.csv("./data/allrounds_standarizedrecordingdays_2023.csv")

allrounds_23$Detections <- as.numeric(allrounds_23$Detections)
allrounds_23$Detections <- replace_na(allrounds_23$Detections,0)

unique(allrounds_23$Detections)

allround <- allrounds_23 %>% 
  mutate(Site_id = paste0(study_area,site, sep = ""))

species <- unique(allround$Common.Name)
enc_hist_aru <- list()
for(s in species){
  
  enc_hist <- allround %>% 
    filter(Common.Name == s) %>% 
    #mutate(Detections = ifelse(is.na(Detections), 0, Detections)) %>% 
    group_by(Date,Site_id,round,recording_day) %>% 
    summarise(count = max(as.numeric(Detections), na.rm = TRUE)) %>% 
    arrange(Site_id,Date,round,recording_day)
  
  enc_hist2 <- enc_hist %>% 
    pivot_wider(id_cols = Site_id,
                names_from = c("recording_day","round"),
                names_prefix = "Occ",
                values_from = count) %>% 
    mutate(across(everything(), ~ifelse(is.null(.), 0, .))) %>% 
    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>% 
    column_to_rownames("Site_id")
  
  enc_hist_aru[[s]] <- enc_hist2
  
}

enc_hist_aru_2023 <- enc_hist_aru

#clean up enc_hist for colext() format

##37 sites, 15 columns

fix <- c("Occ1_R1","Occ2_R1","Occ3_R1","Occ4_R1","Occ5_R1",
         "Occ1_R2","Occ2_R2","Occ3_R2","Occ4_R2","Occ5_R2",
         "Occ1_R3","Occ2_R3","Occ3_R3","Occ4_R3","Occ5_R3")
enc_hist_aru_2023$`American Coot` <- enc_hist_aru_2023$`American Coot`[,fix]

enc_hist_aru_2023 <- enc_hist_aru_2023[-1]
enc_hist_aru_2023 <- enc_hist_aru_2023[-c(7,8)] #remove AMBI and empty lists

# List of all sites in both data frames
all_sites <- union(rownames(fixnames$AMCO), rownames(enc_hist_aru_2023$`American Coot`))

# List of missing sites
missing_sites <- setdiff(all_sites, rownames(enc_hist_aru_2023$`American Coot`))

# Create a data frame with missing sites and NAs in OCC columns
missing_df <- tibble(Site_id = missing_sites)
for (i in 1:15) {
  missing_df[[paste0("OCC", i)]] <- NA_real_
}

missing_df <- as.data.frame(missing_df)
rownames(missing_df) <- missing_df$Site_id
missing_df <- missing_df[, -1]

colnames(missing_df) <- colnames(enc_hist_aru_2023$`American Coot`)
# Combine the missing_df with enc_hist_aru_2023
enc_hist_aru_2023$`American Coot` <- bind_rows(enc_hist_aru_2023$`American Coot`, missing_df)
enc_hist_aru_2023$`American Coot` <- enc_hist_aru_2023$`American Coot`[sort(names(enc_hist_aru_2023$`American Coot`))]

all_sites <- union(rownames(fixnames$SORA), rownames(enc_hist_aru_2023$Sora))

# List of missing sites
missing_sites <- setdiff(all_sites, rownames(enc_hist_aru_2023$Sora))

# Create a data frame with missing sites and NAs in OCC columns
missing_df <- tibble(Site_id = missing_sites)
for (i in 1:15) {
  missing_df[[paste0("OCC", i)]] <- NA_real_
}

missing_df <- as.data.frame(missing_df)
rownames(missing_df) <- missing_df$Site_id
missing_df <- missing_df[, -1]

colnames(missing_df) <- colnames(enc_hist_aru_2023$Sora)
# Combine the missing_df with enc_hist_aru_2023
enc_hist_aru_2023$Sora <- bind_rows(enc_hist_aru_2023$Sora, missing_df)
enc_hist_aru_2023$Sora <- enc_hist_aru_2023$Sora[sort(names(enc_hist_aru_2023$Sora))]

all_sites <- union(rownames(fixnames$PBGR), rownames(enc_hist_aru_2023$`Common Gallinule`))

# List of missing sites
missing_sites <- setdiff(all_sites, rownames(enc_hist_aru_2023$`Common Gallinule`))

# Create a data frame with missing sites and NAs in OCC columns
missing_df <- tibble(Site_id = missing_sites)
for (i in 1:15) {
  missing_df[[paste0("OCC", i)]] <- NA_real_
}

missing_df <- as.data.frame(missing_df)
rownames(missing_df) <- missing_df$Site_id
missing_df <- missing_df[, -1]

colnames(missing_df) <- colnames(enc_hist_aru_2023$`Common Gallinule`)
# Combine the missing_df with enc_hist_aru_2023
enc_hist_aru_2023$`Common Gallinule` <- bind_rows(enc_hist_aru_2023$`Common Gallinule`, missing_df)
enc_hist_aru_2023$`Common Gallinule` <- enc_hist_aru_2023$`Common Gallinule`[sort(names(enc_hist_aru_2023$`Common Gallinule`))]


fixnames <- c("Occ1_R1","Occ2_R1","Occ3_R1","Occ4_R1","Occ5_R1",
              "Occ1_R2","Occ2_R2","Occ3_R2","Occ4_R2","Occ5_R2",
              "Occ1_R3","Occ2_R3","Occ3_R3","Occ4_R3","Occ5_R3")

enc_hist_aru_2023$`Least Bittern` <- enc_hist_aru_2023$`Least Bittern`[,fixnames]
enc_hist_aru_2023$`American Coot` <- enc_hist_aru_2023$`American Coot`[,fixnames]
enc_hist_aru_2023$`Pied-billed Grebe` <- enc_hist_aru_2023$`Pied-billed Grebe`[,fixnames]
enc_hist_aru_2023$Sora <- enc_hist_aru_2023$Sora[,fixnames]

enc_hist_2 <- enc_hist_aru_2023

rm(enc_hist_aru_2023, fixnames, missing_df,allround,allrounds_23,
   enc_hist, enc_hist2)

colnames(enc_hist_2$`King Rail`) <- colnames(enc_hist_1$`King Rail`)
colnames(enc_hist_2$`Common Gallinule`) <- colnames(enc_hist_1$`Common Gallinule`)
colnames(enc_hist_2$`American Coot`) <- colnames(enc_hist_1$`American Coot`)
enc_hist_1 <- enc_hist_1[sort(names(enc_hist_1))]
enc_hist_2 <- enc_hist_2[sort(names(enc_hist_2))]

enc_hist_aru <- list()
for(s in species) {
  
  combo <- rbind(enc_hist_1[[s]], enc_hist_2[[s]])
  
  enc_hist_aru[[s]] <- combo
}

rm(enc_hist_1, enc_hist_2, combo)

#same format as pc encounter histories
print(enc_hist_aru)
str(enc_hist_aru)


### Re structure enc_hist_aru for single season occupancy

  enc_hist_aru_ss <- list()
  
  for (species in names(enc_hist_aru)) {
    
    df <- enc_hist_aru[[species]] 
    
    # Keep site names from rownames
    df$site <- rownames(df)
    
    df_long <- df %>%
      pivot_longer(-site, names_to = "Occ", values_to = "value") %>%
      filter(str_detect(Occ, "_R[123]$")) %>%
      mutate(round = str_extract(Occ, "R[123]"))
    
    df_summary <- df_long %>%
      group_by(site, round) %>%
      summarise(present = as.integer(any(value == 1)), .groups = "drop") %>%
      pivot_wider(names_from = round, values_from = present, values_fill = 0) %>%
      rename_with(~ str_replace_all(., "R", "R_"))
    
    enc_hist_aru_ss[[species]] <- df_summary
  }
  

#3 ARU + PC DATA ---------------------------------------------------------------
#Point Count surveys occured on the last ARU recording day - thus if a bird was detected on a 
##PC survey it was assigned a 1 on the last recording day (did this manually by looking at PC data.
#please excuse poor coding :( )

enc_hist_comb <- enc_hist_aru

#LEBI first
enc_hist_comb$`Least Bittern`["ReelfootA1", "Occ5_R3"] <- 1 
enc_hist_comb$`Least Bittern`["ReelfootA2","Occ5_R3"] <- 1
enc_hist_comb$`Least Bittern`["ReelfootB2","Occ5_R1"] <- 1
enc_hist_comb$`Least Bittern`["ReelfootB3","Occ5_R1"] <- 1
enc_hist_comb$`Least Bittern`["ReelfootC1","Occ5_R2"] <- 1
enc_hist_comb$`Least Bittern`["ReelfootD1","Occ5_R1"] <- 1
enc_hist_comb$`Least Bittern`["ReelfootD1","Occ5_R2"] <- 1
enc_hist_comb$`Least Bittern`["ReelfootD2","Occ5_R2"] <- 1
enc_hist_comb$`Least Bittern`["ReelfootD3","Occ5_R2"] <- 1

enc_hist_comb$`Least Bittern`["TigrettA1","Occ5_R3"] <- 1 
enc_hist_comb$`Least Bittern`["TigrettA2","Occ5_R3"] <- 1
enc_hist_comb$`Least Bittern`["TigrettA3","Occ5_R3"] <- 1

enc_hist_comb$`Least Bittern`["Tennessee NationalA1","Occ5_R1"] <- 1

# COGA
#none were detected on pc so don't change anything

#PBGR
enc_hist_comb$`Pied-billed Grebe`["HatchieA1", "Occ5_R1"] <- 1
enc_hist_comb$`Pied-billed Grebe`["Lake IsomA2","Occ5_R1"] <- 1
enc_hist_comb$`Pied-billed Grebe`["Lake IsomA3","Occ5_R1"] <- 1
enc_hist_comb$`Pied-billed Grebe`["Lower HatchieA2","Occ5_R1"] <- 1

enc_hist_comb$`American Coot`["HatchieA1", "Occ5_R1"] <- 1 #BOTH
enc_hist_comb$`American Coot`["HatchieA1", "Occ5_R2"] <- 1 #pc
enc_hist_comb$`American Coot`["Lake IsomA2", "Occ5_R1"] <- 1 #pc
enc_hist_comb$`American Coot`["Lake IsomA3", "Occ5_R1"] <- 1 #BOTH
enc_hist_comb$`American Coot`["ReelfootA1", "Occ5_R1"] <- 1 #PC
enc_hist_comb$`American Coot`["ReelfootA2", "Occ5_R1"] <- 1 #PC
enc_hist_comb$`American Coot`["ReelfootC1", "Occ5_R1"] <- 1 #PC
enc_hist_comb$`American Coot`["ReelfootC3", "Occ5_R1"] <- 1 #PC
enc_hist_comb$`American Coot`["ReelfootD1", "Occ5_R1"] <- 1 #PC
enc_hist_comb$`American Coot`["ReelfootD1", "Occ5_R2"] <- 1 #PC
enc_hist_comb$`American Coot`["ReelfootD1", "Occ5_R3"] <- 1 #PC
enc_hist_comb$`American Coot`["ReelfootD3", "Occ5_R1"] <- 1 #PC
enc_hist_comb$`American Coot`["ReelfootE1", "Occ5_R1"] <- 1 #PC
enc_hist_comb$`American Coot`["Tennessee NationalB2", "Occ5_R1"] <- 1 #PC

#sora
enc_hist_comb$Sora["GoochA2", "Occ5_R2"] <- 1 #PC
enc_hist_comb$Sora["HatchieA1", "Occ5_R2"] <- 1 #PC
enc_hist_comb$Sora["HatchieA1", "Occ5_R3"] <- 1 #PC
enc_hist_comb$Sora["Parker BranchA2", "Occ5_R3"] <- 1 #PC
enc_hist_comb$Sora["ReelfootA3", "Occ5_R1"] <- 1 #PC
enc_hist_comb$Sora["ReelfootB1", "Occ5_R1"] <- 1 #PC
enc_hist_comb$Sora["ReelfootD3", "Occ5_R1"] <- 1 #PC

enc_hist_comb$`Least Bittern`["HiwasseeC1","Occ5_R2"] <- 1 #BOTH
enc_hist_comb$`Least Bittern`["HiwasseeC2","Occ5_R2"] <- 1 #BOTH

enc_hist_comb$`American Coot`["Cordell HullA1","Occ5_R1"] <- 1 #pc
enc_hist_comb$`American Coot`["Cross CreeksB1","Occ5_R2"] <- 1 #pc
enc_hist_comb$`American Coot`["Cross CreeksC1","Occ5_R2"] <- 1 #pc

enc_hist_comb$`Pied-billed Grebe`["Cross CreeksA1","Occ5_R1"] <- 1 #BOTH

enc_hist_comb$Sora["Cowan MarshA2","Occ5_R1"] <- 1 #BOTH
enc_hist_comb$Sora["HiwasseeA1","Occ5_R1"] <- 1  #PC ONLY 
enc_hist_comb$Sora["Tennessee NationalC1","Occ5_R2"] <- 1 #BOTH
enc_hist_comb$Sora["Tennessee NationalC3","Occ5_R2"] <- 1 #BOTH


#sort names so everything matches
enc_hist_pc <- enc_hist_pc[sort(names(enc_hist_pc))]
enc_hist_aru <- enc_hist_aru[sort(names(enc_hist_aru))]
enc_hist_comb <- enc_hist_comb[sort(names(enc_hist_comb))]

species_pc <- c("LEBI", "AMCO", "PBGR", "SORA")

for (s in species_pc) {
  enc_hist_pc[[s]] <- enc_hist_pc[[s]][order(row.names(enc_hist_pc[[s]])), ]
}


species <- names(enc_hist_aru)

for (s in species) {
  enc_hist_aru[[s]] <- enc_hist_aru[[s]][order(row.names(enc_hist_aru[[s]])), ]
}

species <- names(enc_hist_aru)

for (s in species) {
  enc_hist_comb[[s]] <- enc_hist_comb[[s]][order(row.names(enc_hist_comb[[s]])), ]
}


#modify for single season occupancy
print(enc_hist_comb$`Least Bittern`)

enc_hist_comb_ss <- list()

for (species in names(enc_hist_comb)) {
  
  df <- enc_hist_comb[[species]] 
  
  # Keep site names from rownames
  df$site <- rownames(df)
  
  df_long <- df %>%
    pivot_longer(-site, names_to = "Occ", values_to = "value") %>%
    filter(str_detect(Occ, "_R[123]$")) %>%
    mutate(round = str_extract(Occ, "R[123]"))
  
  df_summary <- df_long %>%
    group_by(site, round) %>%
    summarise(present = as.integer(any(value == 1)), .groups = "drop") %>%
    pivot_wider(names_from = round, values_from = present, values_fill = 0) %>%
    rename_with(~ str_replace_all(., "R", "R_"))
  
  enc_hist_comb_ss[[species]] <- df_summary
}
