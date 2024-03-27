library(raster)
library(reshape2)

####Working directory
setwd("D:/FutureWeb/_2022/")

####Projections
ETRS89='+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs'

####Species raster distributions
distributions=list.files("INPUT/DISTRIBUTIONS",full.names = TRUE,recursive = TRUE)
distributions=distributions[grep(".tif$",distributions)]

#### Mask
mask=raster("INPUT/Mask1Km.tif")
crs(mask)=ETRS89

#### Habitats raster (w/ detailed forests types)
habitats=raster("INPUT/LandCover.tif")

#### get rid of 0 and .img na values
habitats[which(habitats[]==65535)]=NA
habitats[which(habitats[]==0)]=NA
crs(habitats)=ETRS89

#### just to be sure that it fits to the mask
habitats=projectRaster(from=habitats,to=mask,method='ngb')

#### Taxonomy to make the correspondance between old and new species codes
taxo=read.csv("INPUT/ChecklistOK.csv")
taxo=taxo[,c("Code","Code_old","SpeciesName")]

#### Correspondance beetween Landcover codes
habitats.codes.correspondance=read.csv("INPUT/LandUseCodesCorrespondance.csv",sep=";")

#### Merge the Landcover typo to apply it on the preference matrix
habitat.preferences=read.csv("INPUT/HabitatsPreferences.csv",stringsAsFactors = FALSE)
habitat.preferences=habitat.preferences[,c(5:34)]
habitat.preferences=merge(habitat.preferences,taxo,by.x="SppID",by.y="Code_old",all.x=TRUE)
habitat.preferences=habitat.preferences[!is.na(habitat.preferences$Code),]

###### Loop that filters the distributions by the habitats preference

for (i in 1:length(distributions)){
  
  path=distributions[i]
  
  #### Get the group of the species (Reptiles, Birds...)
  dir=dirname(path)
  group=basename(dir)
  
  #### Get the new code and the species name
  code=gsub(".tif","",basename(path))
  spname=taxo[which(taxo$Code==code),"SpeciesName"]
  
  if (!file.exists(paste0("OUTPUT/FILTERED/",group,"/",code,".tif"))){
    
    cat(group," --- ",code," --- ",spname,"\n")
    
    ###### get the habitats preference for the species and use it as a reclass table
    habitat.preferences.for.sp=habitat.preferences[which(habitat.preferences$Code==code),c(2:30)]
    
    if (length(habitat.preferences.for.sp>0)){
      ####format the habitats preferences table
      habitat.preferences.for.sp=melt(habitat.preferences.for.sp)
      habitat.preferences.for.sp$variable=gsub("X","",habitat.preferences.for.sp$variable)
      colnames(habitat.preferences.for.sp)=c("landcover_code","suitability")
      
      ####get Yue's landcover codes
      habitat.preferences.for.sp=merge(habitat.preferences.for.sp,habitats.codes.correspondance,by.x="landcover_code",by.y="Louise")
      habitat.preferences.for.sp=habitat.preferences.for.sp[,c("YueRaster","suitability")]
      colnames(habitat.preferences.for.sp)=c("landcover_code","suitability")
      habitat.preferences.for.sp$landcover_code=as.numeric(habitat.preferences.for.sp$landcover_code)
      
      ####Reclassify the habitat raster
      reclassed_hab=reclassify(habitats,habitat.preferences.for.sp)
      
      #### Get the species distribution raster
      spdistrib=raster(distributions[i])
      crs(spdistrib)=ETRS89
      
      ####Filter by the reclassed habitat raster
      spdistrib.filtered=spdistrib*reclassed_hab
      
      #### write filtered raster
      writeRaster(spdistrib.filtered,file=paste0("OUTPUT/FILTERED/",group,"/",code,".tif"),datatype='INT2U',overwrite=TRUE)
    }else{
      #### if no preferences have been found, write the raster as it is
      writeRaster(spdistrib.filtered,file=paste0("OUTPUT/UNFILTERED/",group,"/",code,".tif"),datatype='INT2U',overwrite=TRUE)
    }
  }
}
