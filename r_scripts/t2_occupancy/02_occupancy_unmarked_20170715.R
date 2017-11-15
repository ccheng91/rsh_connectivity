## P Burke, University of British Columbia
## Date created: 2016 December 10
## Date last modified: 2017 July 13

## GNCE Occupancy Modeling
# Follows 01_analysis_camtrapR_YYYYMMDD.R

## NOTES:
# Modeled species and functional groups
# EDIT: Only use 5 months data (summer)


## ------------------------------------------------------------------------------------
## Section 02: Occupancy Models using unmarked
#          Part 1) Set up unmarked frame containing detection history and covariates (from csv in local directory)
#                   - use code in 01_analysis_camtrapR_YYYYMMDD.R to write .csv to local directory
#          Part 2) Single-season species-specific occupancy models and Model selection using AIC


#install.packages("unmarked")
library(unmarked)
library(dplyr)
library(tidyr)

#Set working Directory on local machine
setwd("~/_research_projects/prj_1601ubc_gnce_connectivity/methods/02_occupancy")
ct_tidy <- read.csv("~/_research_projects/prj_1601ubc_gnce_connectivity/methods/02_occupancy/rOutput/cttidy_2017-07jul15.csv",
                     header = TRUE, stringsAsFactors = FALSE)

#??? Remove NAs prior to analysis


## ------------------------------------------------------------------------------------
##      Part 1) Set up unmarked frame containing detection history and covariates

# list of stations in alphabetical order
stations <- c("CT-3-DEGU1-0501", "CT-27-COCR2-0625", "CT-28-REPE2-0830", "CT-115-LAMC-0501", "CT-115-WHCK-0714",
              "CT-133-WUDH-0815", "CT-283-STRF-0824", "CT-286-SHFI-0824", "CT-287-RAME1-0826", "CT-287-MCGM-0623",
              "CT-288-SOPA-0624", "CT-305-HZCK-0830", "CT-306-FMCK-0830", "CT-450-LSTR1-0828", "CT-474-FLCR-0829", 
              "CT-479-COCR1-0622", "CT-483-SIRI-0625", "CT-539-BUOR-0622", "CT-540-RLTR-0624", "CT-541-RBFI1-0826",
              "CT-544-RECK-0831", "CT-544-REPE1-0831", "CT-565-HIME-0624", "CT-566-SOCR2-0623", "CT-570-BORI-0624", 
              "CT-570-RAME2-0623", "CT-672-EAPA-0621", "CT-675-GRCR-0621", "CT-755-MTHA-0621", "CT-761-WFMR1-0908", 
              "CT-1168-PAIB-0712", "CT-1168-PARI2-0712", "CT-1168-PARI3-0712", "CT-1240-PAFC-0504", "CT-1262-POCK-0410", 
              "CT-1374-SUMC-0521", "CT-1390-SOWC-0520", "CT-1465-EIMC-0523", "CT-1501-CORI-0714", "CT-1596-WIWE-0522", 
              "CT-1597-LODM-0523", "CT-1598-PARI1-0430", "CT-1599-COOC-0522", "CT-1600-PETC-0522", "CT-1602-HEOC-0522", 
              "CT-1605-BOCK-0421" ) # make list of stations for row.names
effort <- read.csv(file = "~/_research_projects/prj_1601ubc_gnce_connectivity/methods/02_occupancy/rOutput/ObsCovs_effort_scaled.csv",
                   header = TRUE, row.names = stations,  stringsAsFactors = FALSE, na.strings = "NA")
# check that row names match
effort <- effort[ -c(1)] # remove redundant column
visib <- read.csv(file = "~/_research_projects/prj_1601ubc_gnce_connectivity/methods/02_occupancy/rOutput/ObsCovs_vist_wt.csv", 
                  header = TRUE, row.names = stations, stringsAsFactors = FALSE, na.strings = "NA")
visib <- visib[ -c(1)] # remove redundant column
visib.z <- scale(visib) # standardize

# set up site and observation covariate dataframes
siteCovs <- ct_tidy[ ,c("cc_oc","TRI_STRM","yrsinc_FIRE","zs_MEAN","landcover","elev")]

cc_oc.z <- scale(siteCovs$cc_oc)
TRI_STRM.z <- scale(siteCovs$TRI_STRM)
yrsinc_FIRE.z <- scale(siteCovs$yrsinc_FIRE)
zs_MEAN.z <- scale(siteCovs$zs_MEAN)
landcover <- siteCovs$landcover
elev.z <- scale(siteCovs$elev)

siteCovs <- data.frame(cc_oc=cc_oc.z,TRI_STRM=TRI_STRM.z,yrsinc_FIRE=yrsinc_FIRE.z,zs_MEAN=zs_MEAN.z,
                       landcover=landcover,elev=elev.z)

obsCovs <- list(effort=effort[,c("o1", "o2", "o3", "o4", "o5", "o6", "o7", "o8", "o9", "o10",
                                 "o11", "o12", "o13", "o14", "o15", "o16", "o17", "o18", "o19", "o20",
                                 "o21", "o22", "o23")],
                  visib=visib.z[,c("o1", "o2", "o3", "o4", "o5", "o6", "o7", "o8", "o9", "o10",
                                 "o11", "o12", "o13", "o14", "o15", "o16", "o17", "o18", "o19", "o20",
                                 "o21", "o22", "o23")])


# Mule deer
DH_Odhe_df <- read.csv(file = "~/_research_projects/prj_1601ubc_gnce_connectivity/methods/02_occupancy/rDetHist/DetHist_Odhe.csv", 
                       header = TRUE, row.names = stations, stringsAsFactors = FALSE, na.strings = "NA")
DH_Odhe_df <- DH_Odhe_df[ -c(1)] # remove redundant row
umf_Odhe <- unmarkedFrameOccu(y = DH_Odhe_df, siteCovs = siteCovs, obsCovs = obsCovs) # unmarked frame object
summary(umf_Odhe) 
#str(DH_Uram_df)


# American black bear
DH_Uram_df <- read.csv(file = "~/_research_projects/prj_1601ubc_gnce_connectivity/methods/02_occupancy/rDetHist/DetHist_Uram.csv", 
                       header = TRUE, row.names = stations, stringsAsFactors = FALSE, na.strings = "NA")
DH_Uram_df <- DH_Uram_df[ -c(1)] # remove redundant row
umf_Uram <- unmarkedFrameOccu(y = DH_Uram_df, siteCovs = siteCovs, obsCovs = obsCovs) # unmarked frame object
summary(umf_Uram)


# Coyote
DH_Cala_df <- read.csv(file = "~/_research_projects/prj_1601ubc_gnce_connectivity/methods/02_occupancy/rDetHist/DetHist_Cala.csv", 
                       header = TRUE, row.names = stations, stringsAsFactors = FALSE, na.strings = "NA")
DH_Cala_df <- DH_Cala_df[ -c(1)] # remove redundant row
umf_Cala <- unmarkedFrameOccu(y = DH_Cala_df, siteCovs = siteCovs, obsCovs = obsCovs) # unmarked frame object
summary(umf_Cala) 


# Cougar
DH_Feco_df <- read.csv(file = "~/_research_projects/prj_1601ubc_gnce_connectivity/methods/02_occupancy/rDetHist/DetHist_Feco.csv", 
                       header = TRUE, row.names = stations, stringsAsFactors = FALSE, na.strings = "NA")
DH_Feco_df <- DH_Feco_df[ -c(1)] # remove redundant row
umf_Feco <- unmarkedFrameOccu(y = DH_Feco_df, siteCovs = siteCovs, obsCovs = obsCovs) # unmarked frame object
summary(umf_Feco) 


# Bobcat
DH_Feru_df <- read.csv(file = "~/_research_projects/prj_1601ubc_gnce_connectivity/methods/02_occupancy/rDetHist/DetHist_Feru.csv", 
                       header = TRUE, row.names = stations, stringsAsFactors = FALSE, na.strings = "NA")
DH_Feru_df <- DH_Feru_df[ -c(1)] # remove redundant row
umf_Feru <- unmarkedFrameOccu(y = DH_Feru_df, siteCovs = siteCovs, obsCovs = obsCovs) # unmarked frame object
summary(umf_Feru)


# Carnivore FG
DH_Carnivore_df <- read.csv(file = "~/_research_projects/prj_1601ubc_gnce_connectivity/methods/02_occupancy/rDetHist/DetHist_Carniv.csv", 
                       header = TRUE, row.names = stations, stringsAsFactors = FALSE, na.strings = "NA")
DH_Carnivore_df <- DH_Carnivore_df[ -c(1)] # remove redundant row 
umf_Carnivore <- unmarkedFrameOccu(y = DH_Carnivore_df, siteCovs = siteCovs, obsCovs = obsCovs) # unmarked frame object
summary(umf_Carnivore)


# Herbivore FG
DH_Herbivore_df <- read.csv(file = "~/_research_projects/prj_1601ubc_gnce_connectivity/methods/02_occupancy/rDetHist/DetHist_Herbiv.csv", 
                            header = TRUE, row.names = stations, stringsAsFactors = FALSE, na.strings = "NA")
DH_Herbivore_df <-  DH_Herbivore_df[ -c(1)] # remove redundant row
umf_Herbivore <- unmarkedFrameOccu(y =  DH_Herbivore_df, siteCovs = siteCovs, obsCovs = obsCovs)  # unmarked frame object
summary(umf_Herbivore) 



## ------------------------------------------------------------------------------------
##      Part 2)  Single-season species-specific occupancy models and Model selection using AIC

#   >ADD knownOcc=numeric() for a priori incidentals
sp_ap <- c(1) # Species presence dectectd a priori at site row numbers


# Species: Mule Deer
# detectability covariates:
#   visib: visibility of the site
#   effort: number of days camera operational (out of 7d ocassion length)
# occupancy covariates:
#   TRI_STRM: topographic rugedness 
#   yrsinc_FIRE: years since fire
#   zs_MEAN: severity within 500m
#   landcover: class (CC model)

fm1 <- occu(~ 1 ~ 1, umf_Odhe, knownOcc=sp_ap)         # null nodel
fm2 <- occu(~ 1 ~ yrsinc_FIRE, umf_Odhe, knownOcc=sp_ap)     # occupancy modeled with ...
fm3 <- occu(~ 1 ~ TRI_STRM, umf_Odhe, knownOcc=sp_ap)  # occupancy modeled with ...
fm4 <- occu(~ 1 ~ yrsinc_FIRE, umf_Odhe, knownOcc=sp_ap) # occupancy modeled with year since fire
fm5 <- occu(~ 1 ~ zs_MEAN, umf_Odhe, knownOcc=sp_ap)   # occupancy modeled with fire severity as occupancy covariate
fm6 <- occu(~ effort ~ 1, umf_Odhe, knownOcc=sp_ap)     # occupancy modeled with trapping effort as detectability covariate
fm7 <- occu(~ visib ~ 1, umf_Odhe, knownOcc=sp_ap)  # occupancy modeled with camera visibility as detectability covariate
fm8 <- occu(~ 1 ~ zs_MEAN + yrsinc_FIRE, umf_Odhe, knownOcc=sp_ap) # occupancy modeled with ...
fm9 <- occu(~ 1 ~ yrsinc_FIRE + zs_MEAN, umf_Odhe, knownOcc=sp_ap)   # occupancy modeled with ...

## Model Selection
fm_Odhe <- fitList('psi(.)p(.)' = fm1,
                   'psi(yrsinc.fire)p(.)' = fm2,
                   'psi(TRI)p(.)' = fm3,
                   'psi(yrsinc.fire2)p(.)' = fm4,
                   'psi(severity)p(.)' = fm5,
                   'psi(.)p(effort)' = fm6,
                   'psi(T.)p(visibility)' = fm7,
                   'psi(severity + year)p(.)' = fm8,
                   'psi(year + severity)p(.)' = fm9
)
modSel(fm_Odhe)
Odhe_modSel <- modSel(fm_Odhe)

# Species: Coyote
fm11 <- occu(~ 1 ~ 1, umf_Cala, knownOcc=sp_ap)         # null nodel
fm12 <- occu(~ 1 ~ yrsinc_FIRE, umf_Cala, knownOcc=sp_ap)     # occupancy modeled with ...
fm13 <- occu(~ 1 ~ TRI_STRM, umf_Cala, knownOcc=sp_ap)  # occupancy modeled with ...
fm14 <- occu(~ 1 ~ landcover, umf_Cala, knownOcc=sp_ap) # occupancy modeled with ...
fm15 <- occu(~ 1 ~ zs_MEAN, umf_Cala, knownOcc=sp_ap)   # occupancy modeled with ...
fm16 <- occu(~ effort ~ 1, umf_Cala, knownOcc=sp_ap)     # occupancy modeled with ...
fm17 <- occu(~ visib ~ 1, umf_Cala, knownOcc=sp_ap)  # occupancy modeled with ...
fm18 <- occu(~ 1 ~ zs_MEAN + yrsinc_FIRE, umf_Cala, knownOcc=sp_ap) # occupancy modeled with ...
fm19 <- occu(~ 1 ~ yrsinc_FIRE + zs_MEAN, umf_Cala, knownOcc=sp_ap)   # occupancy modeled with ...

## Model Selection
fm_Cala <- fitList('psi(.)p(.)' = fm11,
                  'psi(yrsinc.fire)p(.)' = fm12,
                  'psi(TRI)p(.)' = fm13,
                  'psi(landcover)p(.)' = fm14,
                  'psi(severity)p(.)' = fm15,
                  'psi(.)p(effort)' = fm16,
                  'psi(.)p(visibility)' = fm17,
                  'psi(severity + year)p(.)' = fm18,
                  'psi(year + severity)p(.)' = fm19
)
modSel(fm_Cala)
Cala_modSel <- modSel(fm_Cala)

# Species: American Black Bear
fm21 <- occu(~ 1 ~ 1, umf_Uram, knownOcc=sp_ap) # null nodel
fm22 <- occu(~ 1 ~ yrsinc_FIRE, umf_Uram, knownOcc=sp_ap) # occupancy modeled with visability covariate
fm23 <- occu(~ 1 ~ landcover, umf_Uram, knownOcc=sp_ap) # occupancy modeled with elevation
fm24 <- occu(~ effort ~ 1, umf_Uram, knownOcc=sp_ap) # occupancy modeled with visability covariate
fm25 <- occu(~ visib ~ 1, umf_Uram, knownOcc=sp_ap) # occupancy modeled with elevation, canopy cover, visability covariate

## Model Selection
fm_Uram <- fitList('psi(.)p(.)' = fm21,
                   'psi(yrsinc.fire)p(.)' = fm22,
                   'psi(landcover)p(.)' = fm23,
                   'psi(.)p(effort)' = fm24,
                   'psi(.)p(visibility)' = fm25
                   )
modSel(fm_Uram)
Uram_modSel <- modSel(fm_Uram)

# Species: Cougar
#fm31 <- occu(~ 1 ~ 1, umf_Feco, knownOcc=sp_ap)         # null nodel
#fm32 <- occu(~ 1 ~ cc_oc, umf_Feco, knownOcc=sp_ap)     # occupancy modeled with ...
#fm33 <- occu(~ 1 ~ TRI_STRM, umf_Feco, knownOcc=sp_ap)  # occupancy modeled with ...
#fm34 <- occu(~ 1 ~ yrsinc_FIRE, umf_Feco, knownOcc=sp_ap) # occupancy modeled with ...
#fm35 <- occu(~ 1 ~ zs_MEAN, umf_Feco, knownOcc=sp_ap)   # occupancy modeled with ...
#fm36 <- occu(~ 1 ~ landcover, umf_Feco, knownOcc=sp_ap)     # occupancy modeled with ...
#fm37 <- occu(~ effort ~ 1, umf_Feco, knownOcc=sp_ap)  # occupancy modeled with ...
#fm38 <- occu(~ visib ~ 1, umf_Feco, knownOcc=sp_ap) # occupancy modeled with ...
#fm39 <- occu(~ 1 ~ yrsinc_FIRE + zs_MEAN, umf_Feco, knownOcc=sp_ap)   # occupancy modeled with ...

## Model Selection
#fm_Feco <- fitList('psi(.)p(.)' = fm31,
#                   'psi(cc)p(.)' = fm32,
#                   'psi(TRI)p(.)' = fm33,
#                   'psi(yrsinc.fire)p(.)' = fm34,
#                   'psi(severity)p(.)' = fm35,
#                   'psi(landcover)p(.)' = fm36,
#                   'psi(.)p(effort)' = fm37,
#                   'psi(.)p(visibility)' = fm38,
#                   'psi(yrsinc.fire + severity)p(.)' = fm39
#)
#modSel(fm_Feco)
#Feco_modSel <- modSel(fm_Feco)

# Species: Bobcat
fm41 <- occu(~ 1 ~ 1, umf_Feru, knownOcc=sp_ap)         # null nodel
fm42 <- occu(~ 1 ~ cc_oc, umf_Feru, knownOcc=sp_ap)     # occupancy modeled with ...
fm43 <- occu(~ 1 ~ TRI_STRM, umf_Feru, knownOcc=sp_ap)  # occupancy modeled with ...
fm44 <- occu(~ 1 ~ yrsinc_FIRE, umf_Feru, knownOcc=sp_ap) # occupancy modeled with ...
fm45 <- occu(~ 1 ~ zs_MEAN, umf_Feru, knownOcc=sp_ap)   # occupancy modeled with ...
fm46 <- occu(~ 1 ~ cc_oc + yrsinc_FIRE, umf_Feru, knownOcc=sp_ap)     # occupancy modeled with ...
fm47 <- occu(~ effort ~ 1, umf_Feru, knownOcc=sp_ap)  # occupancy modeled with ...
fm48 <- occu(~ visib ~ 1, umf_Feru, knownOcc=sp_ap) # occupancy modeled with ...
#fm49 <- occu(~ 1 ~ yrsinc_FIRE + zs_MEAN, umf_Feru, knownOcc=sp_ap)   # occupancy modeled with ...

## Model Selection
fm_Feru <- fitList('psi(.)p(.)' = fm41,
                   'psi(cc)p(.)' = fm42,
                   'psi(TRI)p(.)' = fm43,
                   'psi(yrsinc.fire)p(.)' = fm44,
                   'psi(severity)p(.)' = fm45,
                   'psi(cc + year)p(.)' = fm46,
                   'psi(.)p(effort)' = fm47,
                   'psi(.)p(visibility)' = fm48
                   #'psi(year + severity)p(.)' = fm49
)
modSel(fm_Feru)
Feru_modSel <- modSel(fm_Feru)

###########################################
# Species: FG: Carnivores
# detectability covariates:
#   vis_wt: visibility of the site
# occupancy covariates:
#   TRI_STRM: topographic rugedness 
#   yrsinc_FIRE: years since fire
#   zs_MEAN: severity within 500m
#   landcover: class (CC model)

fm101 <- occu(~ 1 ~ 1, umf_Carnivore, knownOcc=sp_ap)         # null nodel
fm102 <- occu(~ 1 ~ yrsinc_FIRE, umf_Carnivore, knownOcc=sp_ap)     # occupancy modeled with year since fire
fm103 <- occu(~ 1 ~ TRI_STRM, umf_Carnivore, knownOcc=sp_ap)  # occupancy modeled with topographic ruggedness
fm104 <- occu(~ 1 ~ cc_oc, umf_Carnivore, knownOcc=sp_ap) # occupancy modeled with canopy closure
fm105 <- occu(~ 1 ~ zs_MEAN, umf_Carnivore, knownOcc=sp_ap)   # occupancy modeled with fire severity
fm106 <- occu(~ 1 ~ landcover, umf_Carnivore, knownOcc=sp_ap)     # occupancy modeled with ...
fm107 <- occu(~ effort ~ 1, umf_Carnivore, knownOcc=sp_ap)  # occupancy modeled with ...
fm108 <- occu(~ visib ~ 1, umf_Carnivore, knownOcc=sp_ap) # occupancy modeled with ...
fm109 <- occu(~ 1 ~ yrsinc_FIRE + zs_MEAN, umf_Carnivore, knownOcc=sp_ap)   # occupancy modeled with ...
#fm110 <- occu(~ 1 ~ elev, umf_Carnivore, knownOcc=sp_ap)   # 
fm111 <- occu(~ visib ~ yrsinc_FIRE + TRI_STRM, umf_Carnivore, knownOcc=sp_ap)   # 
fm112 <- occu(~ 1 ~ yrsinc_FIRE + TRI_STRM, umf_Carnivore, knownOcc=sp_ap)   # 

## Model Selection
fm_Carniv <- fitList('psi(.)p(.)' = fm101,
                     'psi(yrsinc.fire)p(.)' = fm102,
                     'psi(TRI)p(.)' = fm103,
                     'psi(can.cov)p(.)' = fm104, 
                     'psi(severity)p(.)' = fm105,
                     'psi(landcover)p(.)' = fm106,
                     'psi(.)p(effort)' = fm107,
                     'psi(.)p(visibility)' = fm108,
                     'psi(yrsinc.fire + severity)p(.)' = fm109,
      #               'psi(elevation)p(.)' = fm110,
                     'psi(yrsinc.fire + TRI)p(visibility)' = fm111,
                     'psi(yrsinc.fire + TRI)p(.)' = fm112
)
modSel(fm_Carniv)
Carnivore_modSel <- modSel(fm_Carniv)

###########################################
# Species: FG: Herbivores
# detectability covariates:
#   vis_wt: visibility of the site
# occupancy covariates:
#   TRI_STRM: topographic rugedness 
#   yrsinc_FIRE: years since fire
#   zs_MEAN: severity within 500m
#   landcover: class (CC model)

fm201 <- occu(~ 1 ~ 1, umf_Herbivore, knownOcc=sp_ap)         # null nodel
fm202 <- occu(~ 1 ~ yrsinc_FIRE, umf_Herbivore, knownOcc=sp_ap)     # occupancy modeled with year since fire
fm203 <- occu(~ 1 ~ TRI_STRM, umf_Herbivore, knownOcc=sp_ap)  # occupancy modeled with topographic ruggedness
fm204 <- occu(~ 1 ~ cc_oc, umf_Herbivore, knownOcc=sp_ap) # occupancy modeled with canopy closure
fm205 <- occu(~ 1 ~ zs_MEAN, umf_Herbivore, knownOcc=sp_ap)   # occupancy modeled with fire severity
fm206 <- occu(~ 1 ~ landcover, umf_Herbivore, knownOcc=sp_ap)     # occupancy modeled with ...
fm207 <- occu(~ effort ~ 1, umf_Herbivore, knownOcc=sp_ap)  # occupancy modeled with ...
fm208 <- occu(~ visib ~ 1, umf_Herbivore, knownOcc=sp_ap) # occupancy modeled with ...
fm209 <- occu(~ 1 ~ zs_MEAN + cc_oc, umf_Herbivore, knownOcc=sp_ap)   # occupancy modeled with ...
fm210 <- occu(~ 1 ~ zs_MEAN + yrsinc_FIRE, umf_Herbivore, knownOcc=sp_ap)   # 

## Model Selection
fm_Herbiv <- fitList('psi(.)p(.)' = fm201,
                     'psi(yrsinc.fire)p(.)' = fm202,
                     'psi(TRI)p(.)' = fm203,
                     'psi(can.cov)p(.)' = fm204,
                     'psi(severity)p(.)' = fm205,
                     'psi(landcover)p(.)' = fm206,
                     'psi(.)p(effort)' = fm207,
         #            'psi(.)p(visibility)' = fm208,
                     'psi(severity + can.cov)p(.)' = fm209,
                     'psi(severity + yrsinc.FIRE)p(.)' = fm210
)
modSel(fm_Herbiv)
Herbivore_modSel <- modSel(fm_Herbiv)
Herbivore_topmodel_coef <- coef(fm205) #fm205 is top model
vcov(fm205)
confint(fm205, type="state", level = 0.95)

Cands <- list(fm201,fm202,fm203,fm204,fm205,fm206,fm207,fm208,fm209,fm210)
Modnames <- c("psi(.)p(.)","psi(yrsinc.fire)p(.)","psi(TRI)p(.)","psi(can.cov)p(.)","psi(severity)p(.)",
              "psi(landcover)p(.)","psi(.)p(effort)","psi(.)p(visibility)","psi(severity + can.cov)p(.)",
              "psi(severity + yrsinc.FIRE)p(.)"
              )

## ------------------------------------------------------------------------------------
## Part 6) Model Selection


predict(fm_Herbiv, type='det')


#nPars    AIC delta  AICwt cumltvWt
#psi(.)p(fire)          3 191.15  0.00 0.7657     0.77
#psi(.)p(Elev+Fire)     4 193.57  2.41 0.2289     0.99
#psi(.)p(.)             2 201.08  9.93 0.0053     1.00

## Parametric bootstrap to check adequacy of model fit
#chisq <- function(fms) {
#  umf <- getData(fms)
#  y <- getY(umf)
#  y[y>1] <- 1
#  sr <- fms@sitesRemoved
#  if(length(sr)>0)
#    y <- y[-sr,,drop=FALSE]
#  fv <- fitted(fms, na.rm=TRUE)
#  y[is.na(fv)] <- NA
#  sum((y-fv)^2/(fv*(1-fv)), na.rm=TRUE)
#}
#pb <- parboot(fm1, statistic=chisq, nsim=100)
#pb


##Start HERE >>>>>>>>>>>>>>>>>>>
#confint(fm, type='det', method = 'normal')
#confint(fm, type='det', method = 'profile')

#lc <- linearComb(fm['det'],c(1,0.5)) # estimate detection effect at obsvars=0.5
#btlc <- backTransform(lc) # transform this to probability (0 to 1) scale and get confidence limits
#confint(btlc, level = 0.9)

# Empirical Bayes estimates of proportion of sites occupied
#re <- ranef(fm)
#sum(bup(re, stat="mode"))


