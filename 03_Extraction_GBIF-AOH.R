## script adapted from Julien Renaud 
rm(list = ls())
library(data.table)
library(raster)
library(foreach)
library(viridis)

library(sf)
library(mapview)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggplot2)
library(plotly)

path2gbif <- "../Data/BioticData/GBIF_iNat/CLEAN/CLEAN/"
path2AOH <- "FILTERING_DISTRIBUTIONS/_2022/OUTPUT/current/"
path2Maiorano2013 <- "../2020_NCP/analysis/data_old_reso/data_1k/Tetrapods/"

## match with old code to compare also Maiorano 2013
specieslist <- fread("../Data/BioticData/Taxonomy/2022Taxonomy_ChecklistOK.csv")

# Amphibians 
specieslist.A=specieslist[grep("Amphibia",specieslist$Class),]

Amphibia=foreach (a=1:nrow(specieslist.A),.combine=rbind)%do%{
# Amphibia=foreach (a=1:10,.combine=rbind)%do%{
    
  luigi.code=as.character(specieslist.A[a,"Code"])
  old.code = as.character(specieslist.A[a,"Code_old"])
  
  luigi.name=as.character(specieslist.A[a,"SpeciesName"])
  luigi.name=gsub(" ","_",luigi.name)
  
  print(luigi.name)
  
  if (file.exists(paste0(path2AOH, "AMPHIBIANS/",luigi.code,".tif")))
  {
    distrib=raster(paste0(path2AOH, "AMPHIBIANS/",luigi.code,".tif"))
  }
  # else {next} 
  
  if (file.exists( paste0(path2Maiorano2013,old.code,".tif") ))
  {
    maiorano2013 = raster(paste0(path2Maiorano2013,old.code,".tif"))
  }
  
  if (file.exists(paste0(path2gbif, "Amphibia/",luigi.name,".csv")))
  {
    occurences=fread(paste0(path2gbif, "Amphibia/",luigi.name,".csv"))
    occurences=occurences[which(occurences$precision<1000&occurences$precision>0&occurences$Is_In_IUCN==TRUE)]    # Keep only OCCURRENCES THAT HAVE PRECISION <= 1km AND are within the IUCN range. 
    if (nrow(occurences)>0){
      occurences=st_as_sf(occurences,coords=c("longitude","latitude"),crs=4326)
      occurences=st_transform(occurences,crs=3035)
    }
  }
  

  if (exists("distrib") && exists("occurences") && nrow(occurences)>0){
    occurences$present_in_AOH=raster::extract(distrib,occurences) ## extracts the value of the raster for the coordinates of the observations
    absences=as.numeric(nrow(occurences[which(occurences$present_in_AOH==0),]))
    presences=as.numeric(nrow(occurences)-absences)
    df=data.frame(speciesname=luigi.name,presences=presences,absences=absences,ratio=presences/nrow(occurences)*100, 
                  presences_maiorano2013 = NA,absences_maiorano2013 = NA, ratio_maiorano2013=NA)
    
    if(exists("maiorano2013")){
      occurences$present_in_maiorano2013=raster::extract(maiorano2013,occurences)
      absences_maiorano2013=as.numeric(nrow(occurences[which(occurences$present_in_maiorano2013==0),]))
      presences_maiorano2013=as.numeric(nrow(occurences)-absences_maiorano2013)

      df=data.frame(speciesname=luigi.name,presences=presences,absences=absences,ratio=presences/nrow(occurences)*100,
                    presences_maiorano2013 = presences_maiorano2013,absences_maiorano2013 = absences_maiorano2013, ratio_maiorano2013=presences_maiorano2013/nrow(occurences)*100 )
      rm(maiorano2013)
      }
    
    rm(distrib)
    rm(occurences)
    return(df)
  }
}



## mammals 

specieslist.M=specieslist[grep("Mammalia",specieslist$Class),]
Mammalia=foreach (a=1:nrow(specieslist.M),.combine=rbind)%do%{
  
  luigi.code=as.character(specieslist.M[a,"Code"])
  old.code = as.character(specieslist.M[a,"Code_old"])
  
  luigi.name=as.character(specieslist.M[a,"SpeciesName"])
  luigi.name=gsub(" ","_",luigi.name)
  
  print(luigi.name)
  
  if (file.exists(paste0(path2AOH, "MAMMALS/",luigi.code,".tif")))
  {
    distrib=raster(paste0(path2AOH, "MAMMALS/",luigi.code,".tif"))
  }
  if (file.exists( paste0(path2Maiorano2013,old.code,".tif") ))
  { 
    maiorano2013 = raster(paste0(path2Maiorano2013,old.code,".tif"))
  }
  if (file.exists(paste0(path2gbif, "Mammalia/",luigi.name,".csv")))
  {
    occurences=fread(paste0(path2gbif, "Mammalia/",luigi.name,".csv"))
    occurences=occurences[which(occurences$precision<1000&occurences$precision>0&occurences$Is_In_IUCN==TRUE)]
    if (nrow(occurences)>0){
      occurences=st_as_sf(occurences,coords=c("longitude","latitude"),crs=4326)
      occurences=st_transform(occurences,crs=3035)
    }
  }
  
  if (exists("distrib") && exists("occurences") && nrow(occurences)>0){
    occurences$present_in_AOH=raster::extract(distrib,occurences) ## extracts the value of the raster for the coordinates of the observations
    absences=as.numeric(nrow(occurences[which(occurences$present_in_AOH==0),]))
    presences=as.numeric(nrow(occurences)-absences)
    df=data.frame(speciesname=luigi.name,presences=presences,absences=absences,ratio=presences/nrow(occurences)*100, 
                  presences_maiorano2013 = NA,absences_maiorano2013 = NA, ratio_maiorano2013=NA)
    
    if(exists("maiorano2013")){
      occurences$present_in_maiorano2013=raster::extract(maiorano2013,occurences)
      absences_maiorano2013=as.numeric(nrow(occurences[which(occurences$present_in_maiorano2013==0),]))
      presences_maiorano2013=as.numeric(nrow(occurences)-absences_maiorano2013)
      
      df=data.frame(speciesname=luigi.name,presences=presences,absences=absences,ratio=presences/nrow(occurences)*100,
                    presences_maiorano2013 = presences_maiorano2013,absences_maiorano2013 = absences_maiorano2013, ratio_maiorano2013=presences_maiorano2013/nrow(occurences)*100 )
      rm(maiorano2013)
    }
    
    rm(distrib)
    rm(occurences)
    return(df)
  }
}


# REPTILES 
specieslist.R=specieslist[grep("Reptilia",specieslist$Class),]
Reptilia=foreach (a=1:nrow(specieslist.R),.combine=rbind)%do%{
  
  luigi.code=as.character(specieslist.R[a,"Code"])
  old.code = as.character(specieslist.R[a,"Code_old"])
  
  luigi.name=as.character(specieslist.R[a,"SpeciesName"])
  luigi.name=gsub(" ","_",luigi.name)
  
  print(luigi.name)
  
  if (file.exists(paste0(path2AOH, "REPTILES/",luigi.code,".tif")))
  {
    distrib=raster(paste0(path2AOH, "REPTILES/",luigi.code,".tif"))
  }
  if (file.exists( paste0(path2Maiorano2013,old.code,".tif") ))
  { 
    maiorano2013 = raster(paste0(path2Maiorano2013,old.code,".tif"))
  }
  if (file.exists(paste0(path2gbif, "Reptilia/",luigi.name,".csv")))
  {
    occurences=fread(paste0(path2gbif, "Reptilia/",luigi.name,".csv"))
    occurences=occurences[which(occurences$precision<1000&occurences$precision>0&occurences$Is_In_IUCN==TRUE)]
    if (nrow(occurences)>0){
      occurences=st_as_sf(occurences,coords=c("longitude","latitude"),crs=4326)
      occurences=st_transform(occurences,crs=3035)
    }
  }
  
  if (exists("distrib") && exists("occurences") && nrow(occurences)>0){
    occurences$present_in_AOH=raster::extract(distrib,occurences) ## extracts the value of the raster for the coordinates of the observations
    absences=as.numeric(nrow(occurences[which(occurences$present_in_AOH==0),]))
    presences=as.numeric(nrow(occurences)-absences)
    df=data.frame(speciesname=luigi.name,presences=presences,absences=absences,ratio=presences/nrow(occurences)*100, 
                  presences_maiorano2013 = NA,absences_maiorano2013 = NA, ratio_maiorano2013=NA)
    
    if(exists("maiorano2013")){
      occurences$present_in_maiorano2013=raster::extract(maiorano2013,occurences)
      absences_maiorano2013=as.numeric(nrow(occurences[which(occurences$present_in_maiorano2013==0),]))
      presences_maiorano2013=as.numeric(nrow(occurences)-absences_maiorano2013)
      
      df=data.frame(speciesname=luigi.name,presences=presences,absences=absences,ratio=presences/nrow(occurences)*100,
                    presences_maiorano2013 = presences_maiorano2013,absences_maiorano2013 = absences_maiorano2013, ratio_maiorano2013=presences_maiorano2013/nrow(occurences)*100 )
      rm(maiorano2013)
    }
    
    rm(distrib)
    rm(occurences)
    return(df)
  }
}


# BIRDS 
specieslist.B=specieslist[grep("Aves",specieslist$Class),]
Aves=foreach (a=1:nrow(specieslist.B),.combine=rbind)%do%{
  
  luigi.code=as.character(specieslist.B[a,"Code"])
  old.code = as.character(specieslist.B[a,"Code_old"])
  
  luigi.name=as.character(specieslist.B[a,"SpeciesName"])
  luigi.name=gsub(" ","_",luigi.name)
  
  print(luigi.name)
  
  if (file.exists(paste0(path2AOH, "BIRDS/",luigi.code,".tif")))
  {
    distrib=raster(paste0(path2AOH, "BIRDS/",luigi.code,".tif"))
  }
  if (file.exists( paste0(path2Maiorano2013,old.code,".tif") ))
  { 
    maiorano2013 = raster(paste0(path2Maiorano2013,old.code,".tif"))
  }
  if (file.exists(paste0(path2gbif, "Aves/",luigi.name,".csv")))
  {
    occurences=fread(paste0(path2gbif, "Aves/",luigi.name,".csv"))
    occurences=occurences[which(occurences$precision<1000&occurences$precision>0&occurences$Is_In_IUCN==TRUE)]
    if (nrow(occurences)>0){
      occurences=st_as_sf(occurences,coords=c("longitude","latitude"),crs=4326)
      occurences=st_transform(occurences,crs=3035)
    }
  }
  
  if (exists("distrib") && exists("occurences") && nrow(occurences)>0){
    occurences$present_in_AOH=raster::extract(distrib,occurences) ## extracts the value of the raster for the coordinates of the observations
    absences=as.numeric(nrow(occurences[which(occurences$present_in_AOH==0),]))
    presences=as.numeric(nrow(occurences)-absences)
    df=data.frame(speciesname=luigi.name,presences=presences,absences=absences,ratio=presences/nrow(occurences)*100, 
                  presences_maiorano2013 = NA,absences_maiorano2013 = NA, ratio_maiorano2013=NA)
    
    if(exists("maiorano2013")){
      occurences$present_in_maiorano2013=raster::extract(maiorano2013,occurences)
      absences_maiorano2013=as.numeric(nrow(occurences[which(occurences$present_in_maiorano2013==0),]))
      presences_maiorano2013=as.numeric(nrow(occurences)-absences_maiorano2013)
      
      df=data.frame(speciesname=luigi.name,presences=presences,absences=absences,ratio=presences/nrow(occurences)*100,
                    presences_maiorano2013 = presences_maiorano2013,absences_maiorano2013 = absences_maiorano2013, ratio_maiorano2013=presences_maiorano2013/nrow(occurences)*100 )
      rm(maiorano2013)
    }
    
    rm(distrib)
    rm(occurences)
    return(df)
  }
}





Amphibia$Class="Amphibia"
Aves$Class="Aves"
Mammalia$Class="Mammalia"
Reptilia$Class="Reptilia"



AllClasses=rbind(Amphibia,Aves,Mammalia,Reptilia)

# save(AllClasses, file = "data/processed_data/Compare_gbif_AOH/perc_occ_AOHwithwater_Maiorano2013.RDATA")


# save(AllClasses, file = "data/processed_data/Compare_gbif_AOH/perc_occ_AOH_Maiorano2013.RDATA")


## without constraint by water distance (better)
load("data/processed_data/Compare_gbif_AOH/perc_occ_AOH_Maiorano2013.RDATA")



#### plot ####

pp = AllClasses[, c(1:4, 8) ] %>%
  ggplot( aes(x=Class, y=ratio, fill=Class)) +
  # geom_boxplot(varwidth = TRUE) +
  geom_boxplot(width = 0.5, outlier.size = 0.01, outlier.alpha = 0.1)+ 
  scale_fill_viridis(name="Classes",discrete = TRUE, alpha=0.6) +
  geom_jitter(aes(size=presences+absences),color="black",width = 0.24, alpha=0.1) +
  scale_size(name="Number of GBIF observations") +
  # ggtitle("Occurences GBIF dans la distribution filtrée") +
  xlab("Classes") +
  ylab("% GBIF observations overlapping with AOH")+
  theme_bw()

ppmaiorano2013 = AllClasses[, c(1, 5:8) ] %>%
  ggplot( aes(x=Class, y=ratio_maiorano2013, fill=Class)) +
  # geom_boxplot(varwidth = TRUE) +
  geom_boxplot(width = 0.5)+ 
  scale_fill_viridis(name="Classes",discrete = TRUE, alpha=0.6) +
  geom_jitter(aes(size=presences_maiorano2013+absences_maiorano2013),color="black", width = 0.26, alpha=0.5) +
  scale_size(name="Number of GBIF observations") +
  # ggtitle("Occurences GBIF dans la distribution filtrée") +
  xlab("Classes") +
  ylab("% agreement between GBIF observations and AOH map")+
  theme_bw()



## Statistical significance of the difference between maiorano 2013 and new aoh 
df <- AllClasses[, c("speciesname", "ratio", "ratio_maiorano2013", "Class")]
colnames(df) <- c("speciesname", "AOH_LULC", "AOH_LC", "Class")

df$nb.observations <- AllClasses$presences + AllClasses$absences


AllClasses.long <- gather(df, key = "AOHtype", value = "ratio", -c("speciesname", "Class", "nb.observations"))


t.test(formula = ratio ~ AOHtype,
       data = AllClasses.long,
       subset = Class == "Amphibia")

t.test(formula = ratio ~ AOHtype,
       data = AllClasses.long,
       subset = Class == "Aves")

t.test(formula = ratio ~ AOHtype,
       data = AllClasses.long,
       subset = Class == "Mammalia")

t.test(formula = ratio ~ AOHtype,
       data = AllClasses.long,
       subset = Class == "Reptilia")



ggplot( data = AllClasses.long, aes(x=Class, y=ratio, fill=AOHtype)) +
  # geom_boxplot(varwidth = TRUE) +
  geom_boxplot(width = 0.65, position = "dodge2", outlier.size = 0.01, outlier.alpha = 0.1)+ 
  scale_fill_manual(values = c("#9FA7BEFF", "#E7A79BFF")) +
  # scale_fill_manual(values = c("#278B9AFF", "#E75B64FF")) +
  
  # geom_jitter(aes(size=nb.observations, fill = AOHtype), color="black", width = 0.26, alpha=0.5) +
  geom_point(pch = 21, alpha = 0.15, position = position_jitterdodge(dodge.width =  0.69, jitter.width = 0.25),aes(size=nb.observations))+
  # geom_point(pch = 21, alpha = 0.2, position = position_dodge2(width = 0.76),aes(size=nb.observations))+
  
  scale_size(name="Number of GBIF observations") +
  # ggtitle("Occurences GBIF dans la distribution filtrée") +
  xlab("Classes") +
  ylab("% overlap GBIF observations and AOH map")+
  theme_bw()

  

