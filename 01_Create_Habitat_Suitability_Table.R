### Step1: build preliminary species x land use based on landcover information alone
## using existing crosswalk made in Maiorano et al 2013 and on habitat preferences documented in IUCN red list datasets (european and global)
## and equivalence table between Yue's land use classification and the globcover classification 

rm(list = ls())

library(data.table)
library(dplyr)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### A. Globcover x land use crosswalk ## ### ### ### ######
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

landcover_equiv_table <- read.csv("crosswalk_globcover_landsystem/equivalence_landcover_long.csv", sep = ";")
colnames(landcover_equiv_table) <- c("Dou2021_short", "Dou2021_full", "GlobCover")
### convert to wide format = binary matrix with 1 = equivalent classes, else 0
landcover_equiv_bin<- dcast(landcover_equiv_table, GlobCover~Dou2021_short, fill=0) 
rm(landcover_equiv_table)

rownames(landcover_equiv_bin) <- landcover_equiv_bin$GlobCover
landcover_equiv_bin$GlobCover<- NULL

landcover_equiv_bin[landcover_equiv_bin >= 1] <- 1 

# save(landcover_equiv_bin, file = "crosswalk_globcover_landsystem/01_equivalence_landcover_bin.RData")
# load("crosswalk_globcover_landsystem/01_equivalence_landcover_bin.RData")

## load globcover x species table (from Maiorano 2013)
spp_globcover_raw <-read.csv("data/globcover_crosswalk/20220405_species_globcover.csv", sep = ";") ## cleaned up version 
spp_globcover <- spp_globcover_raw
rownames(spp_globcover) <- spp_globcover[, 1]
spp_globcover[, 1] <- NULL
spp_globcover$SPPname <- NULL
colnames(spp_globcover) <- gsub("X", "", colnames(spp_globcover))

## prepare for matrix multiplication 
spp_globcover <- spp_globcover[, rownames(landcover_equiv_bin)]

# remove species (rows) which only have NA values 
spp_globcover <- spp_globcover[rowSums(is.na(spp_globcover)) != ncol(spp_globcover), ]
      
# only NAs left are in the 220 (permanent snow and ice category) 
### assume that these NA mean 0 (unsuitable)?? 
### anyway : permanent snow and ice is not an optimal habitat for any species. 
## it is only a secondary habitat for 3 species B221, B444, B445
## transform NAs to 0 

spp_globcover[is.na(spp_globcover)] <- 0


### matrix format 
Spp.Globcover <- as.matrix(spp_globcover)
landcover.Equivalence <- as.matrix(landcover_equiv_bin)

# optimal habitats 
Spp.Globcover.optimal <- Spp.Globcover

Spp.Globcover.optimal[Spp.Globcover.optimal ==1] <- 0
Spp.Globcover.optimal[Spp.Globcover.optimal ==2] <- 1
Spp.Dou2021.fromGC.optimal <- Spp.Globcover.optimal %*% landcover.Equivalence

# secondary
Spp.Globcover.secondary <- Spp.Globcover
Spp.Globcover.secondary[Spp.Globcover.secondary ==2] <- 0

Spp.Dou2021.fromGC.secondary <- Spp.Globcover.secondary %*% landcover.Equivalence

### divide by number of corresponding categories per land use class? 
# nclasses <- colSums(landcover.Equivalence)
# Spp.Dou2021.fromGC.opt.propclass <- Spp.Dou2021.fromGC.optimal/rep(nclasses, each = nrow(Spp.Dou2021.fromGC.optimal)) 

Spp_dou2021_opt <- as.data.frame(Spp.Dou2021.fromGC.optimal)
Spp_dou2021_opt$Sppname <- spp_globcover_raw$SPPname[match(rownames(Spp_dou2021_opt), spp_globcover_raw$ID)]

Spp.Dou2021.fromGC.optimal[Spp.Dou2021.fromGC.optimal>=1] <- 2
Spp.Dou2021.fromGC.secondary[Spp.Dou2021.fromGC.secondary>=1] <- 1
# sum both matrices to get final matrix
Spp.Dou2021.fromGC <- Spp.Dou2021.fromGC.optimal + Spp.Dou2021.fromGC.secondary
## if a land use class is noted as both 1 and 2 (in which case the sum == 3), then assign it to optimal (2)
Spp.Dou2021.fromGC[Spp.Dou2021.fromGC ==3] <- 2

species.landUse.globcover <- as.data.frame(Spp.Dou2021.fromGC)
species.landUse.globcover$Sppname <- spp_globcover_raw$SPPname[match(rownames(species.landUse.globcover), spp_globcover_raw$ID)]

# save(species.landUse.globcover, file = "01A_hab_suitability_from_GlobCover.RData")

# load("01A_hab_suitability_from_GlobCover.RData")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
#### B. habitat types from EEA (no intensity info)   ######
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 


# species x habitat column in IUCN threats table 
spp.habitat.threats <- read.csv("data/EEA_habitat_threats/spp_EEA_habitats_cleaned.csv", sep = ";")

spp.habitat.type.bin <- spp.habitat.threats %>%

  ### forests
  mutate(needle.forest = ifelse(grepl(pattern = "Spruce| spruce| pine|needle-lea|needle leav|conifer|Needle-leav", x = spp.habitat.threats$habitat),  
                                1, 0)) %>%
  mutate(broad.forest = ifelse(grepl(pattern = "oak|birch|beech|poplar|aspen|willow|broad-lea|deciduous|broad leav|broadlea", x = spp.habitat.threats$habitat),  
                               1, 0)) %>%
  ### cultivated areas
  mutate(cultivated = ifelse(grepl(pattern = "cultivated area|field|cropland|cultivated land|cultivated field|arable land", x = spp.habitat.threats$habitat),  
                             1, 0)) %>%
  ### permanent crops 
  mutate(permanent = ifelse(grepl(pattern = "vineyard|olive grove|orchard|fruit garden|permanent crop", x = spp.habitat.threats$habitat),  
                            1, 0)) %>%
  ### grasslands
  mutate(grassland = ifelse(grepl(pattern = "meadow|Meadow| tundra|grassland|grassy area|pastures|cattle", x = spp.habitat.threats$habitat),  
                           1, 0)) %>%
  mutate(agri.mosaic = ifelse(grepl(pattern = "field edge|hedgerow", x = spp.habitat.threats$habitat),  
                              1, 0)) %>%
  mutate(forest.mosaic = ifelse(grepl(pattern = "forest edge|mosaics of forest and open areas", x = spp.habitat.threats$habitat),  
                                1, 0)) %>%
  ### wetlands 
  mutate(wetlands = ifelse(grepl(pattern = " mires | fens |peatland|moorland| swamp|wetland|marsh|stream|flood-lands|water meadows|flooded meadow|peat bog|bogland| bog", x = spp.habitat.threats$habitat),  
                           1, 0)) %>%
  mutate(waterbodies = ifelse(grepl(pattern = "aquatic species|deep still waters| lake|permanent water| river", x = spp.habitat.threats$habitat),  
                              1, 0)) %>%
  mutate(shrub = ifelse(grepl(pattern = " garrigue | maquis| scrub| shrub|heathland| bushland|thicket", x = spp.habitat.threats$habitat),  
                              1, 0)) %>%
  mutate(bare = ifelse(grepl(pattern = "sparse vegetation|barren sand| desert|rocky areas|cliffs| scree|boulders", x = spp.habitat.threats$habitat),  
                        1, 0)) %>%
  mutate(mosaic.bare = ifelse(grepl(pattern = "semi-desert|arid area|sparse vegetation|rocky areas", x = spp.habitat.threats$habitat),  
                              1, 0))



## relate to land use classes using intermediate matrix

equiv.habitat.landUse <- read.csv("data/intermediate_matrices/equiv_habitattype_landUse_BIN.csv", sep = "\t")

rownames(equiv.habitat.landUse) <- equiv.habitat.landUse[, 1]
equiv.habitat.landUse[, 1] <- NULL
colnames(equiv.habitat.landUse) <- gsub(pattern = "X", replacement = "", x = colnames(equiv.habitat.landUse) )

### multiply matrices to obtain matrix of threats to species in terms of land use classes, where 1 = threat with medium intensity and 2 = threat with high intensity 

rownames(spp.habitat.type.bin) <- spp.habitat.type.bin$sppID
Spp.Hab.BIN <- as.matrix(spp.habitat.type.bin[, 6:17])
colnames(Spp.Hab.BIN) == rownames(equiv.habitat.landUse)
Hab.landUse <- as.matrix(equiv.habitat.landUse)

spp.landUse.hab <- Spp.Hab.BIN %*% Hab.landUse

spp.landUse.hab[spp.landUse.hab > 1] <-1
table(spp.landUse.hab)

# save(spp.landUse.hab, file = "01B_hab_suitability_fromEEA_landcover.RData")

# load("01B_hab_suitability_fromEEA_landcover.RData")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
##### A' sum both matrices = expert-based habitat knowledge ######
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

species.landUse.globcover$Sppname <- NULL
species.landUse.globcover<- as.matrix(species.landUse.globcover)

# match colnames and rownames 
spp.in.both <- intersect(rownames(species.landUse.globcover), rownames(spp.landUse.hab))
spp.globcov<- setdiff(rownames(species.landUse.globcover), rownames(spp.landUse.hab))
spp.EEA <- setdiff(rownames(spp.landUse.hab), rownames(species.landUse.globcover))

### add missing species to EEA dataset with 0 values
spp.landUse.hab.allspp <- rbind(spp.landUse.hab, 
                         matrix(nrow = length(spp.globcov), ncol = ncol(spp.landUse.hab), data = 0))

rownames(spp.landUse.hab.allspp) <- c(rownames(spp.landUse.hab), spp.globcov)

### add missing species in globcov dataset with 0
spp.globcov.allspp <- rbind(species.landUse.globcover, 
                                matrix(nrow = length(spp.EEA), ncol = ncol(species.landUse.globcover), data = 0))

rownames(spp.globcov.allspp) <- c(rownames(species.landUse.globcover), spp.EEA)

## match col and row names 
spp.globcov.allspp <- spp.globcov.allspp[rownames(spp.landUse.hab.allspp), ]
spp.globcov.allspp <- spp.globcov.allspp[,colnames(spp.landUse.hab.allspp)]

colnames(spp.globcov.allspp)==colnames(spp.landUse.hab.allspp)

# compare globcover only with matrix from globcover + EEA habitat data 
spp.habitats <- spp.globcov.allspp + 2*spp.landUse.hab.allspp
table(spp.habitats)

table(species.landUse.globcover)

spp.habitats[spp.habitats == 4] <- 2 # when both datasets agree 
spp.habitats[spp.habitats == 3] <- 1 # this is occasional habitat based on 2013 habitat table 

# save(spp.habitats, file = "01_combined_species_habitat_landUse_nointensity.RData")
# 


#### combine with habitat information from global IUCN ####
# use global data from  IUCN on species' habitat preferences to fill in habitat suitability table of land use classes

iucn_hab <- read.csv("../Data/BioticData/Spp_Habitats/IUCN/IUCN_Habitats_Preferences.csv") ## accessed with API 
# table(iucn_hab$season)
# table(iucn_hab$suitability, iucn_hab$majorimportance)
# summary(iucn_hab)
iucn_hab <- subset(iucn_hab, iucn_hab$suitability == "Suitable")

# load species old codes for correspondance 
taxo_codes <- read.csv("../Data/BioticData/Taxonomy/20220314_newtaxo_checkfile_loc.csv", sep = ";")

iucn_hab$SppID <- taxo_codes$Code_old[match(iucn_hab$code, taxo_codes$Code)]

## transform habitat types to codes 

hab_codes <- read.csv("crosswalk_IUCN_landsystem/IUCN_habitat_codes.csv", sep =";")
colnames(hab_codes) <- c("hab_code", "hab_name")

iucn_hab$habitat_code <- hab_codes$hab_code[match(iucn_hab$habitat, hab_codes$hab_name)]

iucn_hab <- subset(iucn_hab, !is.na(iucn_hab$habitat_code)) # remove habitats that aren't associated to code 

# table(iucn_hab$habitat)

### convert species x iucn habitats into binary matrix ==> for major importance & suitable separately. 
iucn_hab_bin<- dcast(iucn_hab, SppID~habitat_code, fill=0) 
# colSums(iucn_hab_bin[, -1]) # check if consistent # yes !


## load crosswalk matrix 
iucn_landuse_crosswalk <- read.csv("crosswalk_IUCN_landsystem/IUCN_landsystem_crosswalk.csv", sep = ";", header = T)

iucn_landuse_crosswalk[is.na(iucn_landuse_crosswalk)] <- 0

rownames(iucn_landuse_crosswalk) <- iucn_landuse_crosswalk[, 1]
iucn_landuse_crosswalk[, 1] <- NULL
colnames(iucn_landuse_crosswalk) <- gsub("X", "", x = colnames(iucn_landuse_crosswalk))


### matrix multiplication 

# get iucn habitats in same order
iucn_landuse_crosswalk <- iucn_landuse_crosswalk[colnames(iucn_hab_bin[-1]),]

# matrix format 
iucn_hab_bin <- iucn_hab_bin[complete.cases(iucn_hab_bin),]
rownames(iucn_hab_bin) <- iucn_hab_bin$SppID
iucn_hab_bin$SppID <- NULL

Spp.IUCN <- as.matrix(iucn_hab_bin)

IUCN.LandUse <- as.matrix(iucn_landuse_crosswalk)

# multiply!
Spp.LandUse.viaIUCN <- Spp.IUCN %*% IUCN.LandUse
Spp.LandUse.viaIUCN[Spp.LandUse.viaIUCN>1] <- 1


# save(Spp.LandUse.viaIUCN, file = "01C_habitatsuitability_fromglobalIUCN.RDATA")

## load habitat suitability based on crosswalk bw global iucn hab classification and land system classification
# load("01C_habitatsuitability_fromglobalIUCN.RDATA") # Spp.LandUse.viaIUCN

# matching species 
# species <- intersect(rownames(Spp.LandUse.viaIUCN), rownames(spp.habitats)) # 862 species 

# add missing species in IUCN 
spp.iucn.allspp <- rbind(Spp.LandUse.viaIUCN, 
                         matrix(nrow = length(setdiff( rownames(spp.habitats), rownames(Spp.LandUse.viaIUCN))), 
                         ncol = ncol(Spp.LandUse.viaIUCN), data = 0))


rownames(spp.iucn.allspp) <- c(rownames(Spp.LandUse.viaIUCN), setdiff( rownames(spp.habitats), rownames(Spp.LandUse.viaIUCN)))


## add missing species in spp.habitats 
spp.habitats.allspp <- rbind(spp.habitats, 
                             matrix(nrow = length(setdiff( rownames(Spp.LandUse.viaIUCN), rownames(spp.habitats))), 
                             ncol = ncol(spp.habitats), data = 0))


rownames(spp.habitats.allspp) <- c(rownames(spp.habitats), setdiff( rownames(Spp.LandUse.viaIUCN), rownames(spp.habitats)))

# match col & rownames
spp.habitats.allspp <- spp.habitats.allspp[rownames(spp.iucn.allspp), ]
spp.habitats.allspp <- spp.habitats.allspp[, colnames(spp.iucn.allspp)]

table.comp <- spp.habitats.allspp - 3*spp.iucn.allspp 
table(table.comp)

#### deal with forests: when type of forest specified, hab based on eea + globcover overrides global_iucn
habitats.landcover <- spp.habitats.allspp +2*spp.iucn.allspp # assume high scores for "suitable" habitat in IUCN global 
table(habitats.landcover)

# if forest habitat pref are all the same: leave as is. length(unique(x)) == 1
# if forest types are specified: length(unique(x)) != 1 : forest type preference overrides global IUCN data (no distinction in forest type)

for (i in 1:nrow(habitats.landcover)){
  if(length(unique(habitats.landcover[i, 4:9]))!=1){
    habitats.landcover[i, 4:9] <- spp.habitats.allspp[i, 4:9]
  } 
}

habitats.landcover[habitats.landcover >2] <- 2
# table(habitats.landcover)

# save(habitats.landcover, file = "01_combined_hab_suitability_from_LandCover.RData")



## include preference in levels of intensity ###########
##### species x habitats in EEA dataset ##

# spp.habitat.threats <- read.csv("data/EEA_habitat_threats/spp_EEA_habitats_cleaned.csv", sep = ";")

spp.habitats.LI.bin <- spp.habitat.threats %>%
  ### villages
  mutate(settlements.LI = ifelse(grepl(pattern = "village|rural|garden", x = spp.habitat.threats$habitat),  
                                 1, 0)) %>%
  mutate(settlements.MI = ifelse(grepl(pattern = "semi-urban|peri-urban|periurban|suburban|fringe of cities|outskirts|edges of town", x = spp.habitat.threats$habitat),  
                                 1, 0)) %>%
  ### forests
  mutate(needle.forest.LI = ifelse(grepl(pattern = "mature conifer|mature pine|mature spruce|decaying tree|rotting tree|decaying wood|dead wood|mature forest|old-growth|old tree|mature tree", x = spp.habitat.threats$habitat),  ## remove disturbance
                                   1, 0)) %>%
  mutate(broad.forest.LI = ifelse(grepl(pattern = "mature broad|mature deciduous forest|decaying tree|rotting tree|decaying wood|dead wood|mature forest|old-growth|old tree|mature tree", x = spp.habitat.threats$habitat),  ## remove disturbance
                                  1, 0)) %>%
  ### LI agriculture 
  mutate(agri.LI = ifelse(grepl(pattern = "extensively managed agri|low intensity agri|traditionally cultiv|traditional agri|traditional farm|traditionally farm", x = spp.habitat.threats$habitat),  
                          1, 0)) %>%
  ### arable crops 
  mutate(arable.LI = ifelse(grepl(pattern = "low-intensity arable cultiv|low intensity arable|small field", x = spp.habitat.threats$habitat),  
                            1, 0)) %>%
  
  ### permanent crops 
  mutate(permanent.LI = ifelse(grepl(pattern = "low intensity agri|traditionally cultiv|traditional agri|traditional farm|old orchard|agroforest", x = spp.habitat.threats$habitat), 
                               1, 0)) %>%
  ### grasslands
  mutate(grassland.LI = ifelse(grepl(pattern = "uncultivated grassland|moorland|steppe", x = spp.habitat.threats$habitat),
                               1, 0)) %>%
  ## mosaics
  mutate(forest.shrub.agri.mosaic = ifelse(grepl(pattern = "agroforest|sylvopastur|silvopastur|low intensity agri|traditionally cultiv|traditional agri|traditional farm|small field|forest edge|field edge|hedgerow|agroforest|silvopastur|sylvopastur", x = spp.habitat.threats$habitat),  ## remove disturbance
                                           1, 0)) %>%
  mutate(agri.mosaic.LI = ifelse(grepl(pattern = "mosaic of natural pastures, arable fields|low intensity agri|traditionally cultiv|traditional agri|traditional farm|field edge|hedgerow", x = spp.habitat.threats$habitat), 
                                 1, 0)) 

#### relate to land use classes

equiv.habitat.landUse <- read.csv("data/intermediate_matrices/equivalence_habitats_LowI_landUse_BIN.csv", sep = ";")

rownames(equiv.habitat.landUse) <- equiv.habitat.landUse[, 1]
equiv.habitat.landUse[, 1] <- NULL
colnames(equiv.habitat.landUse) <- gsub(pattern = "X", replacement = "", x = colnames(equiv.habitat.landUse) )

### multiply matrices to obtain matrix of threats to species in terms of land use classes, where 1 = threat with medium intensity and 2 = threat with high intensity 

rownames(spp.habitats.LI.bin) <- spp.habitats.LI.bin$sppID
Spp.Hab.BIN <- as.matrix(spp.habitats.LI.bin[, 6:15])
Hab.landUse <- as.matrix(equiv.habitat.landUse)
rownames(Hab.landUse) == colnames(Spp.Hab.BIN)

# multiply!
spp.landUse.LI.hab <- Spp.Hab.BIN %*% Hab.landUse

spp.landUse.LI.hab[spp.landUse.LI.hab>1] <-1
# save(spp.landUse.LI.hab, file = "02_species_lowintensity_hab_table.RDATA")

# load("02_species_lowintensity_hab_table.RDATA")


## create final habitat suitability matrixx 
# combine with habitat without intensity  #########
# load("01_combined_hab_suitability_from_LandCover.RData") ## habitats.landcover


# match colnames and rownames 
# sppnames <- intersect(rownames(spp.habitats), rownames(spp.landUse.LI.hab))
landUse_IDs <- intersect(colnames(habitats.landcover), colnames(spp.landUse.LI.hab))

# add missing species in EEA table 
LI.hab.allspp <- rbind(spp.landUse.LI.hab,  
                       matrix(nrow = length(setdiff(rownames(habitats.landcover), rownames(spp.landUse.LI.hab))), 
                              ncol = ncol(spp.landUse.LI.hab), data = 0))

rownames(LI.hab.allspp) <- c(rownames(spp.landUse.LI.hab), setdiff( rownames(habitats.landcover), rownames(spp.landUse.LI.hab)))


# match col and rownames
habitats.landcover <- habitats.landcover[rownames(LI.hab.allspp), ]
habitats.landcover <- habitats.landcover[, landUse_IDs]

LI.hab.allspp <- LI.hab.allspp[, landUse_IDs]



#combine 
spp.habitats.LI <- habitats.landcover + 3*LI.hab.allspp
# table(spp.habitats.LI)
# value of 5: low intensity suitable  + optimal land cover type 
# value of 4: low intensity suitable + secondary land cover type 
# values of 3: low intensity suitable but unsuitable land cover type 

## Case where Low Intensity is preferred + optimal land cover type ####
# change medium and high intensity habitat scores if LI is preferred

# grasslands
spp.habitats.LI[, "42"][which(spp.habitats.LI[, "41"] == 5)] <- 1
spp.habitats.LI[, "43"][which(spp.habitats.LI[, "41"] == 5)] <- 0

# croplands
spp.habitats.LI[, "32"][which(spp.habitats.LI[, "31"] == 5)] <- 1
spp.habitats.LI[, "33"][which(spp.habitats.LI[, "31"] == 5)] <- 0

#  perm croplands
spp.habitats.LI[, "35"][which(spp.habitats.LI[, "34"] == 5)] <- 1

# forests 
spp.habitats.LI[, "212"][which(spp.habitats.LI[, "211"] == 5)] <- 1
spp.habitats.LI[, "213"][which(spp.habitats.LI[, "211"] == 5)] <- 0
spp.habitats.LI[, "222"][which(spp.habitats.LI[, "221"] == 5)] <- 1
spp.habitats.LI[, "223"][which(spp.habitats.LI[, "221"] == 5)] <- 0

# agri mosaics
spp.habitats.LI[, "752"][which(spp.habitats.LI[, "751"] == 5)] <- 1
spp.habitats.LI[, "753"][which(spp.habitats.LI[, "751"] == 5)] <- 0

table(spp.habitats.LI)

## change LI to optimal habitat value = 2
spp.habitats.LI[spp.habitats.LI == 5] <- 2

## Case where Low Intensity is preferred + unsuitable land cover type ####
spp.habitats.LI[spp.habitats.LI == 3] <- 1

## Case where Low Intensity is preferred + secondary land cover type ####

# grasslands
spp.habitats.LI[, "42"][which(spp.habitats.LI[, "41"] == 4)] <- 0
spp.habitats.LI[, "43"][which(spp.habitats.LI[, "41"] == 4)] <- 0

# croplands
spp.habitats.LI[, "32"][which(spp.habitats.LI[, "31"] == 4)] <- 0
spp.habitats.LI[, "33"][which(spp.habitats.LI[, "31"] == 4)] <- 0

#  perm croplands
spp.habitats.LI[, "35"][which(spp.habitats.LI[, "34"] == 4)] <- 0


# forests 
spp.habitats.LI[, "212"][which(spp.habitats.LI[, "211"] == 4)] <- 0
spp.habitats.LI[, "213"][which(spp.habitats.LI[, "211"] == 4)] <- 0
spp.habitats.LI[, "222"][which(spp.habitats.LI[, "221"] == 4)] <- 0
spp.habitats.LI[, "223"][which(spp.habitats.LI[, "221"] == 4)] <- 0

# agri mosaics
spp.habitats.LI[, "752"][which(spp.habitats.LI[, "751"] == 4)] <- 0
spp.habitats.LI[, "753"][which(spp.habitats.LI[, "751"] == 4)] <- 0


spp.habitats.LI[spp.habitats.LI == 4] <- 1
table(spp.habitats.LI)

# save
# save(spp.habitats.LI, file = "02A_hab_suitability_fromEEA_LowIntensity.RDATA")


### step 2: make list of threats associated to different levels of land use intensity, for each land use class #####
# species x habitat column in IUCN threats table 
# load("../2021_threats_metaweb/2021-10-26_threats_correctednames.RData") # 935 species with ID in Maiorano taxonomy 

## from threats column ###
spp.habitat.threats.bin <- spp.habitat.threats %>%
  mutate(noMajorThreat = ifelse(grepl(pattern = "no major threat|no significant threat|no overall major threat|widespread species|adaptable species|adapts well|no known significant threat|no known threat|not believed to be significantly threatened|not thought to be any current|no serious threat", x = spp.habitat.threats$threats), 
                                1, 0)) %>%
  ### settlements 
  mutate(threats.urban = ifelse(grepl(pattern = "impervious|construction|infrastructure development|industrial development|commercial development|housing|urban", x = spp.habitat.threats$threats),  
                                1, 0)) %>%
  ### forests
  mutate(threats.forestry = ifelse(grepl(pattern = "logging|sylvi|silvi|wood harvesting|forestry|shortening of rotation time|commercial monocultures|felling|loss of old trees|removal of old tree|ntensive forest|ntense forest|ntensification of forest|fellin|loss of old trees|mature|oss of old mature|loss of old wood|forest management", x = spp.habitat.threats$threats), 
                                   1, 0)) %>%
  ### croplands
  mutate(threats.intensive.crops = ifelse(grepl(pattern = "large field|loss of hedge|removal of hedge|fertilizer|conversion of traditional agricultural habitats|irrigation|increased cultivation|abandonment of traditional agr|intensive agri|agricultural intensification|intensification of agri|intensive arable agriculture|agricultural development|fertilis|fertiliz|hedgerow|pesticid|herbicid|Pesticid|Herbicid|biocide|rodenticide|intensive farming|agrochemical", x = spp.habitat.threats$threats), 
                                          1, 0)) %>%
  ### permanent crops 
  mutate(threats.intensive.permanent.crops = ifelse(grepl(pattern = "loss of hedge|fertiliz|fertilis|conversion of traditional agricultural habitats|irrigation|increased cultivation|abandonment of traditional agr|intensive agr|agricultural intensification|agricultural development|fertilisation|hedgerow|pesticid|herbicid|Pesticid|biocide|rodenticide|intensive farming|agrochemical", x = spp.habitat.threats$threats),  
                                                    1, 0)) %>%
  ### grasslands 
  mutate(threats.intensive.grasslands = ifelse(grepl(pattern = "fertilis|fertiliz|mechanical mowing|frequent mowing|overgrazing|mechanization of farms|intensive grazin|overstocking of cattle|conversion of traditional agricultural habitats|overgrazing of habitat by cattle|livestock|cattle-grazin|loss of meadow|nitrogen fertilizer|intensive farmin|abandonment of traditional farming|intensive agri|agricultural intensification|intensification of agri|agricultural development|agrochemical", x = spp.habitat.threats$threats), 
                                               1, 0)) 
# colSums(spp.habitat.threats.bin[, 6:11])

### species with "no major threat" get 0 in other threat cats. 
spp.habitat.threats.bin[, 7:11][which(spp.habitat.threats.bin$noMajorThreat == 1), ] <- 0

#### relate threats to land use classes

equiv.threats.landUse <- read.csv("data/intermediate_matrices/equivalence_threats_landUse_BIN.csv", sep = ";")
rownames(equiv.threats.landUse) <- equiv.threats.landUse$X
equiv.threats.landUse$X <- NULL
colnames(equiv.threats.landUse) <- gsub(pattern = "X", replacement = "", x = colnames(equiv.threats.landUse) )
rownames(equiv.threats.landUse)


### multiply matrices to obtain matrix of threats to species in terms of land use classes, where 1 = threat with medium intensity and 2 = threat with high intensity 

rownames(spp.habitat.threats.bin) <- spp.habitat.threats.bin$sppID
Spp.Threats.BIN <- as.matrix(spp.habitat.threats.bin[7:11])
Threats.landUse <- as.matrix(equiv.threats.landUse)
colnames(Spp.Threats.BIN) == rownames(equiv.threats.landUse)

spp.landUse.threats <- Spp.Threats.BIN %*% Threats.landUse


table(spp.landUse.threats)
# value of 6: high intensity agri mosaics 
# value of 3: medium intensity agri mosaics
spp.landUse.threats[spp.landUse.threats == 6] <-2
spp.landUse.threats[spp.landUse.threats == 3] <-1

# value of 4: high intensity agri mosaics
spp.landUse.threats[spp.landUse.threats == 4] <-2
# value of 2 in medium agri mosaics ==> to 1 
spp.landUse.threats[, "752"][which(spp.landUse.threats[, "752"] == 2)] <- 1


save(spp.landUse.threats, file = "02B_species_threats_landUse.RData")
# load("02B_species_threats_landUse.RData")


#### COMBINE land use intensity tables #######
# add missing species to threats table 

spp.landUse.threats.allspp <- rbind(spp.landUse.threats,  
                                    matrix(nrow = length(setdiff(rownames(spp.habitats.LI), rownames(spp.landUse.threats))), 
                                           ncol = ncol(spp.landUse.threats), data = 0))

rownames(spp.landUse.threats.allspp) <- c(rownames(spp.landUse.threats), setdiff(rownames(spp.habitats.LI), rownames(spp.landUse.threats)))
spp.landUse.threats <- spp.landUse.threats.allspp
rm(spp.landUse.threats.allspp)

# match colnames and rownames 
landUse_IDs <- intersect(colnames(spp.landUse.threats), colnames(spp.habitats.LI))

spp.habitats.LI <- spp.habitats.LI[rownames(spp.landUse.threats), ]
spp.habitats.LI <- spp.habitats.LI[, landUse_IDs]

spp.landUse.threats <- spp.landUse.threats[, landUse_IDs]

table(spp.habitats.LI)
table(spp.landUse.threats)

# substract!
spp.landsystem.suitability <- spp.habitats.LI - spp.landUse.threats
table(spp.landsystem.suitability)

# all the negative values correspond to either: 
# habitats that are not suitable for the species according to the globcover crosswalk AND that represent a threat. 
# or secondary habitat that is associated to high intensity threat
# ==> change to 0 
spp.landsystem.suitability[spp.landsystem.suitability < 0] <- 0 


#### step 3: use information on avoided habitats ####
## set these to zero if not already the case

spp.habitat.avoided.bin <- spp.habitat.threats %>%
  mutate(anthropogenic_habitat = ifelse(grepl(pattern = "arable|cropland|plantation|urban|human|anthropogen|modified|cultivated|Cultivated", x = spp.habitat.threats$avoided_habitat), 
                                        1, 0)) %>%
  mutate(intensive_agri =  ifelse(grepl(pattern = "intensive arable|arable monocultu|intensive cultivation are avoided|intensively farm", x = spp.habitat.threats$avoided_habitat), 
                                  1, 0)) %>%
  mutate(intensive_grassland = ifelse(grepl(pattern = "heavily grazed", x = spp.habitat.threats$avoided_habitat), 
                                      1, 0)) %>%
  mutate(bare_areas = ifelse(grepl(pattern = "desert|arid", x = spp.habitat.threats$avoided_habitat), 
                             1, 0)) 


avoided_hab <- read.csv("data/intermediate_matrices/equivalence_avoidedhab_landUse_BIN.csv", sep = ";")
rownames(avoided_hab) <- avoided_hab[, 1]
avoided_hab[, 1] <- NULL
colnames(avoided_hab)  <- gsub(pattern = "X", replacement = "", x = colnames(avoided_hab) )
rownames(avoided_hab)


### multiply matrices to obtain matrix of threats to species in terms of land use classes, where 1 = threat with medium intensity and 2 = threat with high intensity 

rownames(spp.habitat.avoided.bin) <- spp.habitat.avoided.bin$sppID
Spp.avoidedHab.BIN <- as.matrix(spp.habitat.avoided.bin[6:9])
AvoidedHab <- as.matrix(avoided_hab)
colnames(Spp.avoidedHab.BIN) == rownames(AvoidedHab)

spp.landUse.avoided <- Spp.avoidedHab.BIN %*% AvoidedHab






##########################################  #
## Final habitat suitability matrix #######

#### combine with A&B ######


#### add missing species 
spp.landUse.avoided.allspp <-rbind(spp.landUse.avoided,  
                                   matrix(nrow = length(setdiff(rownames(spp.landsystem.suitability), rownames(spp.landUse.avoided))), 
                                          ncol = ncol(spp.landUse.avoided), data = 0))

rownames(spp.landUse.avoided.allspp) <- c(rownames(spp.landUse.avoided), setdiff(rownames(spp.landsystem.suitability), rownames(spp.landUse.avoided)))
spp.landUse.avoided <- spp.landUse.avoided.allspp
rm(spp.landUse.avoided.allspp)

# match col and rownames 
spp.landUse.avoided <- spp.landUse.avoided[rownames(spp.landsystem.suitability), ]
spp.landUse.avoided <- spp.landUse.avoided[, colnames(spp.landsystem.suitability)]

# table(spp.landsystem.suitability)
# table(spp.landUse.avoided)
spp.landUse.avoided[spp.landUse.avoided>1] <- 1

### substract 
spp.hab.suitability.final <- spp.landsystem.suitability-2*spp.landUse.avoided
# table(spp.hab.suitability.final)
spp.hab.suitability.final[spp.hab.suitability.final<0] <- 0

# save(spp.hab.suitability.final, file = "20221705_spp_habitat_suitability_final.RDATA")



### finalize for distribution filtering 
sppcodes <- read.csv("FILTERING_DISTRIBUTIONS/_2022/INPUT/ChecklistOK.csv")

spp.landsystem.suitability <- as.data.frame(spp.hab.suitability.final)

spp.landsystem.suitability$SppID <- rownames(spp.landsystem.suitability)
spp.landsystem.suitability$SppName <- sppcodes$SpeciesName[match(spp.landsystem.suitability$SppID, sppcodes$Code_old)]

spp.landsystem.suitability$Family <- sppcodes$Family[match(spp.landsystem.suitability$SppID, sppcodes$Code_old)]
spp.landsystem.suitability$Order <- sppcodes$Order[match(spp.landsystem.suitability$SppID, sppcodes$Code_old)]
spp.landsystem.suitability$Class <- sppcodes$Class[match(spp.landsystem.suitability$SppID, sppcodes$Code_old)]
spp.landsystem.suitability <- spp.landsystem.suitability[, c(34, 33, 32, 31, 30, 1:29)]

write.csv(spp.landsystem.suitability, file = "FILTERING_DISTRIBUTIONS/_2022/INPUT/HabitatsPreferences.csv", row.names = F)

# spp.landsystem.suitability$habitat <- spp.habitat.threats$habitat[match(spp.landsystem.suitability$SppID, spp.habitat.threats$sppID)]
# write.csv(spp.landsystem.suitability, file = "data/final_habitat_table/20220412_spp_landuse_with_habitatsentences.csv", row.names = F)


