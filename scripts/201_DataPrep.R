################################################################################L
# 
## file name: 202_DataPrep.R
## Author: Haley Holiman
## Updated 12/18/2024
## Output: Code to clean up PC and ARU data for Part 2 of TN Marshbirds Paper
################################################################################L

source("./scripts/101_DataPrep.R")
rm(enc_hist_aru) ##just using combined data encounter history 

enc_hist_comb = rapply(enc_hist_comb, f = function(x) ifelse(is.na(x), 0, x), how = "replace")
select <- dplyr::select

#1. load data ------------------------------------------------------------------

##observation covs taken during point count surveys (temp, windspeed, etc)
sitecov_veg <- read.csv("./data/obsCovs_2022.csv")
#used as obs cov in single season
sitecov_veg <- sitecov_veg %>% 
  column_to_rownames("X") %>% 
  round()


#year (aka bout) as a cov
M <- 78 #number sites
J <- 5 #number secondary periods
T <- 3 #number primary periods

y <- array(NA, dim = c(M,J,T)) #fake detection histories

yy <- matrix(y, M, J*T)

survey_bout <- matrix(c('1','2','3'),
                      nrow(yy), T, byrow=TRUE)

rm(yy,y,T,J,M)


## spatial covs ----------------------------------------------------------------
#2022 sites
spat_covs <- read.csv("./data/spatcovs_2022_ally.csv") 

spat_covs <- spat_covs %>% 
  select(-"X") %>% 
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .))) %>% 
  column_to_rownames("points2022.site_id") %>% 
  mutate_all(as.numeric) 

#2023 sites
spat_covs2 <- read.csv("./data/spatcovs_2023_ally.csv")

spat_covs2 <- spat_covs2 %>% 
  select(-"X") %>% 
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .))) %>%  
  column_to_rownames("points2023.site_id") %>%
  mutate_all(as.numeric)

#combine both years
spat_covs <- rbind(spat_covs, spat_covs2) %>% 
  mutate_all(scale) %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), 0, .)))

rownames(enc_hist_pc$LEBI)

# points2022 <- read.csv("SpatialData/surverysites_2022.csv")
# points2023 <- read.csv("SpatialData/surverysites_2023.csv")
# 
# names(points2022) <- c("X","y","x","site_id","year")
# names(points2023) <- c("X","y","x","site_id","year")
# 
# sites_22 <- as.data.frame(points2022$site_id)
# colnames(sites_22) <- "site_id"
# sites_23 <- as.data.frame(points2023$site_id)
# colnames(sites_23) <- "site_id"
# 
# sites <- rbind(sites_22,sites_23)
# spat_covs_all <- cbind(sites,spat_covs) %>% 
#   arrange(site_id) %>%
#   column_to_rownames("site_id")
# spat_covs <-spat_covs_all

rm(sites_22, sites_23, spat_covs_all, points2022, points2023, enc_hist_pc,
   spat_covs2,spat_covs2) #keep sites for other covs

### yearly site covs -----------------------------------------------------------
#yearly referring to primary periods 
#data from 3 rounds of vegetation surveys at each point

#vegetation averaged for yearly site covs, created in ARU data prep script
#2022 sites
yearlyvegcovs1 <- read.csv("./data/yearlysitecovs.csv")

yearlyvegcovs1 <- yearlyvegcovs1 %>%
  mutate_all(~replace_na(.,0))

#2023 sites
yearlyvegcovs2 <- read.csv("./data/yearlysitecovs_23.csv")

yearlyvegcovs2 <- yearlyvegcovs2 %>%
  mutate_all(~replace_na(.,0))

yearlyvegcovs_all <- rbind(yearlyvegcovs1, yearlyvegcovs2) %>% 
  arrange(emergent.Site_id) %>% 
  column_to_rownames("emergent.Site_id")

yearlyvegcovs <- yearlyvegcovs_all %>% 
  select(-contains(".Site_id")) %>% 
  select(-"X") %>% 
  round() %>% scale()
  #mutate(across(where(is.numeric), ~ scale(.)))


rm(yearlyvegcovs_all, yearlyvegcovs2, yearlyvegcovs1)

####recording cov --------------------------------------------------------------
#recording days - numeric value 1:5

#recording day of detection for observation cov - 1:5
recording_cov1 <- read.csv("./data/recordingdaycov.csv")

recording_cov2 <- read.csv("./data/recordingdaycov_23.csv") %>% 
  select(-"X")

colnames(recording_cov2) <- colnames(recording_cov1)
recording_covall <- rbind(recording_cov1,recording_cov2) %>% 
  arrange(X) %>% 
  column_to_rownames("X")

rm(recording_cov1,recording_cov2)

##### survey day cov -----------------------------------------------------------
allrounds <- read.csv("./data/allrounds_standarizedrecordingdays_2022.csv")
allrounds$Detections <- as.numeric(allrounds$Detections)
allrounds$Detections <- replace_na(allrounds$Detections,0)

all_2022 <- allrounds %>% 
  mutate(Site_id = paste0(allrounds$study_area, allrounds$site, sep = ""))
day_values <- all_2022 %>% 
  mutate(Date = as.Date(Date)) %>% 
  mutate(day_in_window = as.integer(Date - min(Date)))

day_cov <- day_values %>%
  group_by(Site_id, Date,day_in_window, recording_day) %>%
  distinct(Date, Site_id, day_in_window) %>%
  mutate(Date = ymd(Date),
         bout = case_when(
           day(Date) <= 26 & month(Date) == 4 ~ 1,
           day(Date) >= 27 & month(Date) == 4 |
             day(Date) <14 & month(Date) == 5 ~ 2,
           day(Date) >= 14 & month(Date) == 5 ~ 3),
         day = min(day_in_window)) %>%
  arrange(Site_id, bout) %>%
  pivot_wider(id_cols = Site_id,
              names_from =  c("bout","recording_day"),
              names_prefix = "Occ",
              values_from = day,
              values_fn = list(day = ~ min(.)))
#fix mistakes due to NAs or incorrect entries
day_cov <- day_cov[, -17]
day_cov <- day_cov[, -20]
day_cov <- day_cov[, -19]
day_cov <- day_cov[, -18]
day_cov <- day_cov[, -17]
View(day_cov)
day_cov[9,2] <- 2
day_cov[9,"Occ1_1"] <- 2
day_cov[12,"Occ1_1"] <- 9
day_cov <- day_cov[, -1]

day_cov1 <- day_cov
#2023
allrounds <- read.csv("./data/allrounds_standarizedrecordingdays_2023.csv")

allrounds$Detections <- as.numeric(allrounds$Detections)
allrounds$Detections <- replace_na(allrounds$Detections,0)

all_2023 <- allrounds %>% 
  mutate(Site_id = paste0(allrounds$study_area, allrounds$site, sep = ""))
day_values <- all_2023 %>% 
  mutate(Date = as.Date(Date)) %>% 
  mutate(day_in_window = as.integer(Date - min(Date)))

day_cov2 <- day_values %>%
  group_by(Site_id, Date,day_in_window, recording_day) %>%
  distinct(Date, Site_id, day_in_window) %>%
  mutate(Date = ymd(Date),
         bout = case_when(
           day(Date) <= 26 & month(Date) == 4 ~ 1,
           day(Date) >= 27 & month(Date) == 4 |
             day(Date) <14 & month(Date) == 5 ~ 2,
           day(Date) >= 14 & month(Date) == 5 |
             day(Date) <= 3 & month(Date) == 6 ~ 3),
         day = min(day_in_window)) %>%
  arrange(Site_id, bout) %>%
  pivot_wider(id_cols = Site_id,
              names_from =  c("bout","recording_day"),
              names_prefix = "Occ",
              values_from = day,
              values_fn = list(day = ~ min(.)))

colnames(day_cov2)
order <- c("Site_id","Occ1_1","Occ1_2","Occ1_3","Occ1_4","Occ1_5",
           "Occ2_1","Occ2_2","Occ2_3","Occ2_4","Occ2_5",
           "Occ3_1","Occ3_2","Occ3_3","Occ3_4","Occ3_5")

day_cov2 <- day_cov2[,order]

day_cov2 <- day_cov2[,-1]

day_covall <- rbind(day_cov1, day_cov2)

day_cov <- cbind(data.frame(site_id = rownames(recording_covall)), day_covall) %>% 
  arrange(site_id) %>% 
  column_to_rownames("site_id")

rm(all_2022, all_2023, allrounds, day_cov1, day_cov2, day_covall,day_values,
   desired_order,desired_order_american_coot,desired_order_king_rail,fix,i,
   missing_sites, order, species, species_pc,s)

#####noaa cov ------------------------------------------------------------------
noaa_cov1 <- read.csv("./data/noaacovs2022.csv")

noaa_cov1 <- noaa_cov1 %>% 
  select(-1) %>% 
  mutate_all(~replace_na(.,0)) %>% 
  round()

noaa_cov2 <- read.csv("./data/noaacovs2023.csv")

noaa_cov2 <- noaa_cov2 %>% 
  select(-1) %>% 
  mutate_all(~replace_na(.,0)) %>% 
  round()


# Make sure they have the same column names and order
common_columns <- intersect(names(noaa_cov1), names(noaa_cov2))

# Select only the common columns in both data frames
noaa_cov1 <- noaa_cov1[, common_columns]
noaa_cov2 <- noaa_cov2[, common_columns]


noaa_cov <- rbind(noaa_cov1, noaa_cov2)

#2. Format COVS for r unmarked -------------------------------------------------
?unmarkedMultFrame

#observation covs
obsCov <- list(recording_day = recording_covall,
               precip = noaa_cov[1:78, 1:15],
               wind = noaa_cov[1:78, 16:30],
               tmax = noaa_cov[1:78, 31:45],
               survey_day = day_cov)

#site covs
sitecovs <- list(ag100 = spat_covs[1:78, "ag_100"],
                 ag224 = spat_covs[1:78, "ag_224"],
                 ag500 = spat_covs[1:78, "ag_500"],
                 develop100 = spat_covs[1:78, "dev_100"],
                 develop224 = spat_covs[1:78, "dev_224"],
                 develop500 = spat_covs[1:78, "dev_500"],
                 openwater100 = spat_covs[1:78, "openwat_100"],
                 openwater224 = spat_covs[1:78, "openwat_224"],
                 openwater500 = spat_covs[1:78, "openwat_500"],
                 emergent100 = spat_covs[1:78, "em_100"],
                 emergent224 = spat_covs[1:78, "em_224"],
                 emergent500 = spat_covs[1:78, "em_500"],
                 woody100 = spat_covs[1:78, "woody_100"],
                 woody224 = spat_covs[1:78, "woody_224"],
                 woody500 = spat_covs[1:78, "woody_500"],
                 artflood100 = spat_covs[1:78, "artfl_100"],
                 artflood224 = spat_covs[1:78, "artfl_224"],
                 artflood500 = spat_covs[1:78, "artfl_500"],
                 palus100 = spat_covs[1:78, "palus_100"],
                 palus224 = spat_covs[1:78, "palus_224"],
                 palus500 = spat_covs[1:78, "palus_500"],
                 emveg = yearlyvegcovs[1:78, "emergent.1"],
                 floatingveg = yearlyvegcovs[1:78, "floating.1"],
                 openwater = yearlyvegcovs[1:78, "openwater.1"],
                 woody = yearlyvegcovs[1:78, "woody.1"],
                 waterdepth = yearlyvegcovs[1:78, "water_depth.1"])

#"yearly" site covs
yearlySiteCovs <-  list(bout = survey_bout,
                        emergent_veg = yearlyvegcovs[1:78,1:3],
                        floating_veg = yearlyvegcovs[1:78,4:6],
                        openwater_veg = yearlyvegcovs[1:78, 7:9],
                        woody_veg = yearlyvegcovs[1:78,10:12],
                        water_depth = yearlyvegcovs[1:78,13:15])

rm(day_cov, noaa_cov, noaa_cov1, noaa_cov2, recording_covall, sites, 
   spat_covs, survey_bout, yearlyvegcovs)



