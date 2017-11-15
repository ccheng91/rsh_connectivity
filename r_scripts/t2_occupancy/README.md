# Patrick Burke UBC MSc Thesis Project
Date created: ___2017-Nov-15___   
Last modified: ___2017-Nov-15___   


### Task2: Occupancy MOdels

I will use occupancy models to predict occupancy across the study area using quantitative camera trap data.  
  
## Methods
I will use `unmarked` to estimate species occupancy.  

Observation covariates:   
1. Elevation   
2. Landcover     

Detectability covariates:   
1. Gap light   
2. Leaf area index   
3. Microphone height   
4. Equipment type (recording unit and microphone)     


**Table 2-1:** Bat Species List for BC

Species|Code|Provincial<br>Status|Federal<br>Status|WNS<br>Impacts              
-----------------------------|----|------------|----------------|----------   
Big Brown Bat	               |EPFU|Not At Risk |      	         |Low          
Silver-haired Bat            |LANO|Not At Risk |      	         |Low          
Hoary Bat	                   |LACI|Not At Risk |                |Very Low     
California Myotis            |MYCA|Not At Risk |                |**High**     
Long-eared Myotis	(Keen's)   |MYEV|Not At Risk |Endangered      |**Severe**   
Little Brown Myotis	         |MYLU|Not At Risk |      	         |**Severe**   
Long-legged Myotis	         |MYVO|Not At Risk |      	         |**High**     
Yuma Myotis                  |MYYU|Not At Risk |      	         |**Severe**   
Western Small-footed Myotis  |MYCI|Blue        |                |**Severe**   
Fringed Myotis	             |MYTH|Blue        |Data Deficient  |Unknown        
Northern Myotis       	     |MYSE|Blue	      |Endangered      |**Severe** 
Eastern Red Bat	             |LABO|Unknown     |          	     |Low	       
Townsend's Big-eared Bat	   |COTO|Blue        |          	     |Unknown    
Pallid Bat            	     |ANPA|Red         |Threatened	     |Unknown    
Spotted Bat           	     |EUMA|Blue        |Special Concern |Unknown     

\* WNS Impacts are unknown for all species in the West. However, these qualitative impact categories have been assigned given the magnitude of population response for similar species in the east, species with similar physiology, expert opinion, and disease modeling.


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


