## P Burke, University of British Columbia
## Date created: 2016 September 6
## Date last modified: 2017 July 17

## GNCE Occupancy Modeling
# 1) Camera Trap Image Management Using camtrapR
# 2) Image Metadata Tagging using digiKam
# 3) Check Species Identifications
# 4) Develop Detection History for individual Species using camtrapR



## ------------------------------------------------------------------------------------
## Part 1) Camera Trap Image Management Using camtrapR

#install.packages("camtrapR")
library(camtrapR)
library(dplyr)
library(tidyr)

Sys.getenv("PATH") # Check which directories are in PATH (not shown here)
Sys.which("exiftool") # Confirm the system can find ExifTool

setwd("C:/Users/prwb/Documents/_research_projects/prj_1601ubc_gnce_connectivity/methods/02_occupancy/")
ct <- read.csv("~/_research_projects/prj_1601ubc_gnce_connectivity/data/ct_2017-07jul15.csv", 
               header = TRUE, stringsAsFactors = FALSE, na.strings = "NA")
#rmwd4_tj20161121 <- file.path("wd4_tj20161121") # Set up empty working directory
names(ct)[2] <- "Station"
ct_tidy <- ct %>% 
  filter(id == "r") %>%
  select(Station,
         station_name,
         jurisdiction,
         y_proj_utm_n,
         x_proj_utm_e,
         elev,
         deploy_date,
         lastimg_date,
         Problem1_from,
         Problem1_to,
         samp_day,
         team,
         resample,
         avg_spc..m.,
         std_dead.10,
         cc_oc,
         vis_wt,
         topo_pos,
         TRI_STRM,
         fire_NAME,
         fire_YEAR,
         yrsinc_FIRE,
         zs_MEAN,
         landcover) %>%
  slice(1:46)

write.csv(ct_tidy, file = "~/_research_projects/prj_1601ubc_gnce_connectivity/methods/02_occupancy/rOutput/cttidy_2017-07jul15.csv")

## create station directories in wd_ct
#StationFolderCreate1 <- createStationFolders (inDir = dat_2016_pb_fin,
#                                              stations = as.character(ct$ct_id),
#                                              createinDir = TRUE)

## timeshift images?

## rename images to camtrapR convention
#wd_images_raw <- file.path("temp_t5") # TEMP raw image location in working directory
#wd_images_raw_renamed <- file.path("wd4_t5") # destination for renamed images to be copied to

#renaming.table2 <- imageRename(inDir = wd_images_raw,
#                               outDir = wd_images_raw_renamed,
#                               hasCameraFolders = FALSE,
#                               copyImages = TRUE
#                               )

## write copywright into image metadata
#wd_images_ID_copy <- file.path("wd4_t5")
#copyrightTagToAdd <- "P. Burke, Biodiversity Research Centre, University of British Columbia, (c2016)"
#addCopyrightTag(inDir = wd_images_ID_copy,
#                copyrightTag = copyrightTagToAdd,
#                askFirst = FALSE)

#exifTagNames(wd_images_ID_copy, returnMetadata = TRUE)

##getSpeciesImages collects all images of a focal species and moves NOT WORKING

#species_to_copy <- "American Black Bear" # = Ursus americanus = Uram
#specImagecopy <- getSpeciesImages(species = species_to_copy,
#                                  IDfrom = "directory",
#                                  inDir = wd_images_ID,
#                                  outDir = wd_images_species_copy,
#                                  createStationSubfolders = FALSE
#)


## ------------------------------------------------------------------------------------
## Part 2) Image Metadata Tagging using digiKam
#     - Hierarchical tags used to identify species in camera trap photos
#     - Double blind analysis available


# Species metadata tags in digiKam:
speciesNames = c("American Badger",
                 "American Black Bear",
                 "American Marten",
                 "Black-tailed Deer",
                 "Bobcat",
                 "Bushy-tailed Woodrat",
                 "Canada Lynx",
                 "Cougar",
                 "Coyote",
                 "Elk",
#                "Fisher",
                 "Golden manteled ground squirrel",
                 "Hoary Marmot",
                 "Moose",
                 "Mountain Goat",
                 "Mule Deer",
                 "Snowshoe Hare",
                 "Striped Skunk",
                 "White-tailed Deer"
#                "Western Spotted Skunk",
#                "Wolf")
#                 "Wolverine"
                  )

#checkNames1 <- checkSpeciesNames (speciesNames, searchtype = "common")

## Determine total JPG images in directory
img <- file.path("~/_research_projects/prj_1601ubc_gnce_connectivity/data/02c_img_analysis_camtrapR/dat_2016_pb_fin")
length(list.files(img, pattern = "JPG", recursive = TRUE))
## [1] 15907 on July 15


## ------------------------------------------------------------------------------------
## Part 3) Check Species Identifications and Camera Operation

#wd_images_wd2pb <- system.file("pictures/sample_images", package = "camtrapR")
# run check with 120 seconds (2 minutes) maximum time differnce
check.folders <- checkSpeciesIdentification(inDir = img,
                                            IDfrom = "metadata",
                                            metadataSpeciesTag = "Species_PB",
                                            #metadataSpeciesTagToCompare = "Species_TJ",
                                            metadataHierarchyDelimitor ="|",
                                            hasCameraFolders = FALSE,
                                            maxDeltaTime = 120    # 2minutes
                                            ) 
#Warning: removes records with missing species metadata tag


## Run recordTable to tabulate species records
rec.db.species.60noteam <- recordTable(inDir = img,
                                       IDfrom = "metadata",
                                       metadataSpeciesTag = c("Species_PB"),
                                       minDeltaTime = 60,                   # 1hour temporal independence between records
                                       deltaTimeComparedTo = "lastRecord",
                                       timeZone = "America/Vancouver",
                                       exclude = c("ê???Team", "ê???Other")
                                       )

## Add in functional groups: 
#Carnivore: American badger, American marten ,Bobcat, Canada lynx, Cougar, Coyote
#Herbivore: Black-tailed deer, Elk, Moose, Mountain, Goat, Mule Deer
#Omnivore: American black bear
#Medium Mammal: Bushy tailed woodrat, Chipmunk, Golden manteled ground squirrel, Hoary marmot, porcupine, snowshoe hare, squirrel, striped skunk
#Bird
#NA: Human, Team, Unknown


recTable.FG <- read.csv(file = "~/_research_projects/prj_1601ubc_gnce_connectivity/methods/02_occupancy/rOutput/recordTableFG.csv", 
                        header = TRUE, stringsAsFactors = FALSE) # FGs added in Excel: see above for species lists
fg <- recTable.FG$FunctionalGp
rec.db.species.60noteam[ , "FunctionalGroup"] <- fg

head(rec.db.species.60noteam)
str(rec.db.species.60noteam)

# Write Record table to file
write.csv(rec.db.species.60noteam, file = "~/_research_projects/prj_1601ubc_gnce_connectivity/methods/02_occupancy/rOutput/recordTable.csv", row.names=FALSE)

#list.files(file.path(img, "Canada Lynx"))

##Camera Operation   DATE ERROR IN SETUPCOL
#Convert ct_tidy data frame to characters
ct_camop <- data.frame(lapply(ct_tidy, as.character), stringsAsFactors=FALSE)

camop_noproblem <- cameraOperation(CTtable = ct_camop,           # Update with problems
                                   stationCol = "Station",
                                   setupCol = "deploy_date",
                                   retrievalCol = "lastimg_date",
                                   writecsv = FALSE,
                                   hasProblems = TRUE,    #TEMP     
                                   dateFormat = "%Y-%m-%d"
                                   )

#ct[,-which(colnames(ct_camop) %in% c("y_proj_utm_n", "x_proj_utm_e"))]
#camop_noproblem[, 1:5]
#camop_noproblem[, (ncol(camop_noproblem)-6):ncol(camop_noproblem)]

## Plot the Camera operation matrix
camopPlot <- function(camOp){
  which.tmp <- grep(as.Date(colnames(camOp)), pattern = "01$")
  label.tmp <- format(as.Date(colnames(camOp))[which.tmp], "%Yâ€%m")
  at.tmp <- which.tmp / ncol(camOp)
  image(t(as.matrix(camOp)), xaxt = "n", yaxt = "n", col = c("red", "grey70"))
  axis(1, at = at.tmp, labels = label.tmp)
  axis(2, at = seq(from = 0, to = 1, length.out = nrow(camOp)), labels = rownames(camOp), las = 1)
  abline(v = at.tmp, col = rgb(0,0,0, 0.2))
  box()
}
camopPlot(camOp = camop_noproblem)


## ------------------------------------------------------------------------------------
## Part 4) Develop Detection Matrix for individual Species using camtrapR
#     - Use camera operation matrix and record table from above
#     - Write DH to csv

## Species detection maps
Mapstest1 <- detectionMaps(CTtable = ct_camop,
                           recordTable = rec.db.species.60noteam,
                           Xcol = "x_proj_utm_e",
                           Ycol = "y_proj_utm_n",
                           stationCol = "Station",
                           speciesCol = "FunctionalGroup",
                           printLabels = TRUE,
                           #richnessPlot = TRUE, # by setting this argument TRUE
                           speciesPlots = FALSE,
                           addLegend = TRUE
)

## Detection Histories
##  7 day ocassion length
##  

## Mule deer detection history (without trapping effort)
DetHist_Odhe <- detectionHistory(recordTable = rec.db.species.60noteam,
                                 camOp = camop_noproblem,
                                 stationCol = "Station",
                                 speciesCol = "Species",
                                 recordDateTimeCol = "DateTimeOriginal",
                                 species = "Mule Deer",
                                 occasionLength = 7,
                                 day1 = "station",
                                 timeZone = "America/Vancouver",
                                 includeEffort = FALSE,
                                 #scaleEffort = TRUE,
                                 writecsv = TRUE,
                                 outDir = "rDetHist"
                                 )

DH_Odhe_df <- as.data.frame(DetHist_Odhe)
write.csv(DetHist_Odhe, file = "~/_research_projects/prj_1601ubc_gnce_connectivity/methods/02_occupancy/rDetHist/DetHist_Odhe.csv", 
        row.names=TRUE, na = "NA")


## American Black Bear detection history (without trapping effort)
DetHist_Uram <- detectionHistory(recordTable = rec.db.species.60noteam,
                                 camOp = camop_noproblem,
                                 stationCol = "Station",
                                 speciesCol = "Species",
                                 recordDateTimeCol = "DateTimeOriginal",
                                 species = "American Black Bear",
                                 occasionLength = 7,                     # 7 day ocassion
                                 day1 = "station",
                                 timeZone = "America/Vancouver",
                                 includeEffort = FALSE,
                                 writecsv = TRUE,
                                 outDir = "rDetHist"
)

DH_Uram_df <- as.data.frame(DetHist_Uram)
write.csv(DetHist_Uram, file = "~/_research_projects/prj_1601ubc_gnce_connectivity/methods/02_occupancy/rDetHist/DetHist_Uram.csv", 
          row.names=TRUE, na = "NA")

## Coyote detection history (without trapping effort)
DetHist_Cala <- detectionHistory(recordTable = rec.db.species.60noteam,
                                 camOp = camop_noproblem,
                                 stationCol = "Station",
                                 speciesCol = "Species",
                                 recordDateTimeCol = "DateTimeOriginal",
                                 species = "Coyote",
                                 occasionLength = 7,
                                 day1 = "station",
                                 timeZone = "America/Vancouver",
                                 includeEffort = FALSE,
                                 writecsv = TRUE,
                                 outDir = "rDetHist"
)

DH_Cala_df <- as.data.frame(DetHist_Cala)
write.csv(DetHist_Cala, file = "~/_research_projects/prj_1601ubc_gnce_connectivity/methods/02_occupancy/rDetHist/DetHist_Cala.csv", 
          row.names=TRUE, na = "NA")

## Bobcat detection history (without trapping effort)
DetHist_Feru <- detectionHistory(recordTable = rec.db.species.60noteam,
                                 camOp = camop_noproblem,
                                 stationCol = "Station",
                                 speciesCol = "Species",
                                 recordDateTimeCol = "DateTimeOriginal",
                                 species = "Bobcat",
                                 occasionLength = 7,
                                 day1 = "station",
                                 timeZone = "America/Vancouver",
                                 includeEffort = FALSE,
                                 writecsv = TRUE,
                                 outDir = "rDetHist"
)

DH_Feru_df <- as.data.frame(DetHist_Feru)
write.csv(DetHist_Feru, file = "~/_research_projects/prj_1601ubc_gnce_connectivity/methods/02_occupancy/rDetHist/DetHist_Feru.csv", 
          row.names=TRUE, na = "NA")

## Cougar detection history (without trapping effort)
DetHist_Feco <- detectionHistory(recordTable = rec.db.species.60noteam,
                                 camOp = camop_noproblem,
                                 stationCol = "Station",
                                 speciesCol = "Species",
                                 recordDateTimeCol = "DateTimeOriginal",
                                 species = "Cougar",
                                 occasionLength = 7,
                                 day1 = "station",
                                 timeZone = "America/Vancouver",
                                 includeEffort = FALSE,
                                 writecsv = TRUE,
                                 outDir = "rDetHist"
)

DH_Feco_df <- as.data.frame(DetHist_Feco)
write.csv(DetHist_Feco, file = "~/_research_projects/prj_1601ubc_gnce_connectivity/methods/02_occupancy/rDetHist/DetHist_Feco.csv", 
          row.names=TRUE, na = "NA")

# Carnivore detection history
DetHist_Carniv <- detectionHistory(recordTable = rec.db.species.60noteam,
                                 camOp = camop_noproblem,
                                 stationCol = "Station",
                                 speciesCol = "FunctionalGroup",
                                 recordDateTimeCol = "DateTimeOriginal",
                                 species = "Carnivore",
                                 occasionLength = 7,
                                 day1 = "station",
                                 timeZone = "America/Vancouver",
                                 includeEffort = FALSE,
                                 writecsv = TRUE,
                                 outDir = "rDetHist"
)

DH_Carnivore_df <- as.data.frame(DetHist_Carniv)
write.csv(DetHist_Carniv, file = "~/_research_projects/prj_1601ubc_gnce_connectivity/methods/02_occupancy/rDetHist/DetHist_Carniv.csv", 
          row.names=TRUE, na = "NA")

# Herbivore detection history
DetHist_Herbiv <- detectionHistory(recordTable = rec.db.species.60noteam,
                                   camOp = camop_noproblem,
                                   stationCol = "Station",
                                   speciesCol = "FunctionalGroup",
                                   recordDateTimeCol = "DateTimeOriginal",
                                   species = "Herbivore",                   # Ungulates
                                   occasionLength = 7,
                                   day1 = "station",
                                   timeZone = "America/Vancouver",
                                   includeEffort = FALSE,
                                   writecsv = TRUE,
                                   outDir = "rDetHist"
)

DH_Herbivore_df <- as.data.frame(DetHist_Herbiv)
write.csv(DetHist_Herbiv, file = "~/_research_projects/prj_1601ubc_gnce_connectivity/methods/02_occupancy/rDetHist/DetHist_Herbiv.csv", 
          row.names=TRUE, na = "NA")

