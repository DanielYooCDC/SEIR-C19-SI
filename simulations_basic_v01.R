############################################
##### Init  ####
############################################
setwd("C:/Users/opp8/OneDrive - CDC/GitHub/SEIR-C19-SI")

library(dplyr)
library(tidyr)

rm(list=ls())


last_date = Sys.Date()



################################################
##### model data, functions and parameters  ####
################################################


# model functions
source("functions_model_01.R")

# data for modeling 
source("data_model_01.R")

# model parameters
source("model_parameters_01.R")






############################################
##### Effective R tunning ####
############################################


Rt = c( 3.335, 1.450, 1.363, 0.580, 0.290, 2.400, 0.928, 0.957, 1.595, 1.725,  
         1.100, 1.000, 1.000, 1.015, 1.100, 1.044, 1.218, 1.102, 1.015, 0.957,
         1.057, 1.157, 1.257, 1.300, 1.257, 1.200, 1.300, 1.000, 1.000, 1.000, 
         2.000, 2.000, 2.000, 2.500, 2.750, 2.750, 3.000, 2.850, 1.700, 2.000, 
         #3.000, 2.500, 2.300, 2.100, 2.900, 4.500, 4.700, 3.700, 2.500, 2.000
         #3.000, 2.500, 2.300, 2.100, 2.900, 4.650, 4.700, 4.000, 3.700, 3.700 
         3.000, 2.500, 2.300, 2.100, 2.900, 4.650, 4.800, 4.800, 4.800, 4.800 
)



timeR = c( 15,  22,  30,  65, 100, 130, 160, 183, 208, 240,  
           241, 253, 263, 278, 290, 300, 312, 322, 332, 349, 
           359, 369, 379, 389, 404, 419, 429, 439, 449, 490, 
           494, 501, 508, 515, 522, 529, 536, 565, 573, 593, 
           610, 620, 630, 665, 670, 680, 695, 700, 720, 770
           #610, 615, 630, 655, 675, 685, 695, 705, 740, 770
)

timeR = c(15, diff(timeR))



# convert effR to time series
Rfun = Bt_rect_time(duration_time, Rt, timeR)

# compute beta parameters (time series) 
Bfun = 1/param$D_infectious * Rfun



#library(plotly)
#ggplotly(ggplot(data.frame(t= seq(1, duration_time), Rf =Bfun), aes(x=t, y=Rf)) + geom_line())



############################################
##### Model run  ####
############################################

start.time <- Sys.time()
pdat =  compute_model_V01(Bfun, duration_time, w, param)
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken

## join model projections
spdat = joining_model_projections(pdat)




############################################
##### Preocessing results  ####
############################################


### Projection: daily cases: smoothed average

odat_scen = spdat[spdat$groups=="infected: group",]
#7-day smoothing average
odat_scen$number = ma(odat_scen$number, n = 7, side = 1)
odat_scen$number = round(odat_scen$number, digits = 2)
odat_scen$groups = "infected"


### Projection: hospitalisations 
hspdat_scen = spdat[spdat$groups=="hospitalization",]
hspdat_scen$groups = "hosp"
hspdat_scen$number = round(hspdat_scen$number, digits = 0)
  
  
### Projection: hosp IN 
hspindat_scen = spdat[spdat$groups=="hosp in",]
hspindat_scen$groups = "hosp in"
hspindat_scen$number = round(hspindat_scen$number, digits = 0)

  
  
  
### Projection: intensive care
ispdat_scen = spdat[spdat$groups=="icu",]
ispdat_scen$groups = "icu" 
ispdat_scen$number = round(ispdat_scen$number, digits = 0)

  
  
### Projection: intensive care IN

icuindat_scen = spdat[spdat$groups=="icu in",]
icuindat_scen$groups = "icu in" 
icuindat_scen$number = round(icuindat_scen$number, digits = 0)


### Projection: daily deaths: smoothed average

udspdat_scen = spdat[spdat$groups=="death: daily",]
udspdat_scen$groups = "death"
udspdat_scen$number = round(udspdat_scen$number, digits = 2)



### Projection: vaccination

cpdat_scen = spdat[spdat$groups=="vaccinated",]
cpdat_scen$groups = "vaccinated"
cpdat_scen$number = round(cpdat_scen$number, digits = 2)



#########################################################################
###### Export to CSV  #####################
#########################################################################
  
  
  
tdat = data.frame(
  
  day = odat_scen[odat_scen$days >= 1 & odat_scen$days <= (duration_time - 30), "days" ],
  
  OKU = round( odat_scen[odat_scen$days >= 1 & odat_scen$days <= (duration_time - 30), "number"], 2),
  HOS = round( hspdat_scen[hspdat_scen$days >= 1 & hspdat_scen$days <= (duration_time - 30), "number"], 2),
  HOSin = round( hspindat_scen[hspindat_scen$days >= 1 & hspindat_scen$days <= (duration_time - 30), "number"], 2),
  ICU = round( ispdat_scen[ispdat_scen$days >= 1 & ispdat_scen$days <= (duration_time - 30), "number"], 2),
  ICUin = round( icuindat_scen[icuindat_scen$days >= 1 & icuindat_scen$days <= (duration_time - 30), "number"], 2),
  DSM = round( udspdat_scen[udspdat_scen$days >= 1 & udspdat_scen$days <= (duration_time - 30), "number"], 2),
  
  VACC = round( cpdat_scen[cpdat_scen$days >= 1 & cpdat_scen$days <= (duration_time - 30), "number"], 2),
  VACC.rate = round( cpdat_scen[cpdat_scen$days >= 1 & cpdat_scen$days <= (duration_time - 30), "number"]/Npop, 5)
  
)

file_name_csv = "epimod_latest.csv"
write.csv(tdat, file = file_name_csv, row.names = FALSE)


  
  





