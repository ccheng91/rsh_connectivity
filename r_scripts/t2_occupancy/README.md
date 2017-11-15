# Patrick Burke UBC MSc Thesis Project
Date created: ___2017-Nov-15___   
Last modified: ___2017-Nov-15___   


### Task2: Occupancy Models

Code for this task can be found in the Markdown document [ghd02_occupancy.md](https://github.com/burkeprw/rsh_connectivity/blob/master/r_scripts/t2_occupancy/ghd02_occupancy.md).  

I use occupancy models to predict occupancy across the study area using quantitative camera trap data. Here I provide results from occupancy models for the following species and functional groups:
- Mule deer  
- American Black Bear   
- Coyote   
- Cougar  
- Bobcat  
- Herbivores   
- Carnivores   
  
## Methods
I fit the fit the [MacKenzie et al. (2002)](https://www.uvm.edu/rsenr/vtcfwru/spreadsheets/occupancy/Occupancy%20Exercises/Exercise3/MacKenzie%20et%20al.%20single-season.pdf) model to camera trap data using the `unmarked:occu()` function. I used data from 5 months of sampling during the snow-free season when wildfire occurrs in the study region (May 15 to Octber 15). I assume that the population is closed during this period, with no immigration or emigration, to satiisfy the statistical framework. To satisfy the assumption of site independence, a minimum distance of 1000 m was maintained between camera locations.  

#### Observation covariates:   
 - **Terrain Ruggedness Index (TRI_STRM):** Terrain Ruggedness Index (TRI) is the difference between the value of a cell and the mean of an 8-cell neighborhood of surrounding cells. I use a digital elevation model xxx I used ArcGIS (the Riley et al. (1999) METHODS)?? 
 - **Years Since Fire (yrsinc_FIRE):**   
 - **Fire Severity (zs_MEAN):** A mean of fire severity values within 500m of the camera location. USGS provides fire severity models across the US portion of the study area using LANDFIRE methods, which are based on Landsat spectral imagery.   
 - **Landcover:** class (USGS model)  
 
#### Detectability covariates:
 - **Visibility (visib):** A metric of general visibility at each camera site. The linear distance (in meters) from the camera sensor to the furthest point of detection was recorded during camera deployment. Local vegetation and terrain features and camera deployment location dictate the visibility at each site. Visibility is assumed constant across all sample occassions at each site.     
 - **Effort (effort):** The number of days the camera was operational during each occassion (out of 7d ocassion length).  Effort is reduced by camera malfunctions, loss of power, or other     


**Table 2-1:** TABLE

Species|Code|Provincial<br>Status|Federal<br>Status|WNS<br>Impacts              
-----------------------------|----|------------|----------------|----------   
Big Brown Bat	               |EPFU|Not At Risk |      	         |Low          
Silver-haired Bat            |LANO|Not At Risk |      	         |Low          
Hoary Bat	                   |LACI|Not At Risk |                |Very Low     



## Model Results

I first modeled occupancy for MYLU, the endangered little brown myotis. In the coming weeks, I will add observation covariates to the model and add estimate occupancy for other species.   

- **Occupancy covariates:** Elevation   
- **Detectability covariates:** Gap Light (GLA), Leaf Area index (LAI.4)   


**Table 2-2:** Preliminary model selection for MYLU

Model|nPars|AIC|delta|AICwt|CumltvWt              
----------------------------------------|-|------|-----|-------|---------  
psi(leaf area index)p(.)                |3|161.11|0.00 |7.3e-01|     0.73
psi(leaf area index + gap light)p(.)    |4|163.10|1.99 |2.7e-01|     1.00
psi(.)p(.)                              |2|172.29|11.17|2.7e-03|     1.00
psi(gap light)p(.)                      |3|174.02|12.91|1.1e-03|     1.00
psi(.)p(elevation)                      |3|194.69|33.58|3.7e-08|     1.00


## Top model results MYLU:

**Call:**
occu(formula = ~leafarea + gaplight ~ 1, data = myluUMF)

**Occupancy:**  

Estimate |SE   |z  |P(>z)   
---------|------|-----|---------  
2.33     |0.697 |3.34 |0.000834  

**Detection:**  

covariate|Estimate|SE|z|P(>z)  
------------|------------|--------|-------|--------  
(Intercept) |0.951432   | 0.3851 | 2.470 | 0.01349  
leafarea    |-1.481091|0.4508|-3.286|0.00102  
gaplight    |-0.000835|0.0082|-0.102|0.91891  

**AIC:** 163.102


