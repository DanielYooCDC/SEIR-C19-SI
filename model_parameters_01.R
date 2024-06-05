############################################
##### Model parameters ####
############################################

duration_time = 880

param = list()



param$N = 2100000
param$zacetno_stevilo = 25  # Initial number


#### All parameters below are time series #####

### durations in states ####
D_incubation =  rep(5.2, duration_time)  # duration of latent period
param$D_incubation = D_incubation

D_infectious =  rep(2.9, duration_time)  # infectious period
param$D_infectious = D_infectious

D_recovery_mild = rep(12, duration_time)  # duration of mild illness to recovery
param$D_recovery_mild = D_recovery_mild

D_recovery_hosp = rep(12, duration_time)  # duration of non-ICU hospitalization to recovery
param$D_recovery_hosp = D_recovery_hosp

D_recovery_ICU = rep(14, duration_time)  # duration of ICU hospitalization to recovery
param$D_recovery_ICU  = D_recovery_ICU

D_death = rep(14, duration_time)  # duration of the disease before death in nursing/retirement home
param$D_death = D_death





#### waning parameters ####
D_waning_R = rep(730, duration_time)  # 2 years waning immunity 
D_waning_R[720:duration_time] = 365   # 1 year waning immunity
param$D_waning_R = D_waning_R

D_waning_V = rep(730, duration_time)
D_waning_V[720:duration_time] = 365

param$D_waning_V = D_waning_V



#### rate of reported daily cases #####
w_inf_daily_t =  rep(0.4, duration_time)
w_inf_daily_t[671:duration_time] = rep(0.4, duration_time-670)
w_inf_daily_t[691:duration_time] = rep(0.53, duration_time-690)
w_inf_daily_t[710:duration_time] = 0.60
w_inf_daily_t[720:duration_time] = 0.45
w_inf_daily_t[730:duration_time] = 0.60
#w_inf_daily_t[701:duration_time] = rep(0.80, duration_time-700)


param$w_inf_daily_t = w_inf_daily_t




### risk factors for vaccinated ###
# transmission
faktor_kuznosti_t = rep(5, duration_time)
faktor_kuznosti_t[661:duration_time] = rep(2.0, duration_time-660)
param$faktor_kuznosti_cep = faktor_kuznosti_t

# hospital admission
faktor_hosp_t = rep(10, duration_time)
faktor_hosp_t[661:duration_time] = rep(20.0, duration_time-660)
param$faktor_hosp_cep = faktor_hosp_t

# icu admission
faktor_icu_t = rep(5, duration_time)
faktor_icu_t[661:duration_time] = rep(5, duration_time-660)
param$faktor_icu_cep = faktor_icu_t

# death
faktor_smrti_t = rep(10, duration_time)
faktor_smrti_t[661:duration_time] = rep(20.0, duration_time-660)
param$faktor_smrti_cep = faktor_smrti_t




##### Death rates:  asymptomatic ####

# time function
p_fatal_a_t = rep(0, duration_time)
p_fatal_a_t[1:40]    = rep(10.0/100, 40)
p_fatal_a_t[41:100]  = rep(0.5/100, 60)
p_fatal_a_t[101:215] = rep(2.5/100, 115)
p_fatal_a_t[216:275] = rep(5.1/100, 60)
p_fatal_a_t[276:300] = rep(2.8/100, 25)
p_fatal_a_t[301:350] = rep(1.4/100, 50)
p_fatal_a_t[351:390] = rep(0.8/100, 40)
p_fatal_a_t[391:duration_time] = rep(0.1/100, duration_time-390)
p_fatal_a_t[611:duration_time] = rep(0.20/100, duration_time-610)
p_fatal_a_t[631:duration_time] = rep(0.01/100, duration_time-630)
p_fatal_a_t[671:duration_time] = rep(0.35/100, duration_time-670)

# for each group
p_fatal_a_t1 = 0.0 * p_fatal_a_t
p_fatal_a_t2 = 0.0 * p_fatal_a_t
p_fatal_a_t3 = 0.0 * p_fatal_a_t
p_fatal_a_t4 = 0.8 * p_fatal_a_t
p_fatal_a_t5 = 2.0 * p_fatal_a_t

param$p_fatal_a1 = p_fatal_a_t1
param$p_fatal_a2 = p_fatal_a_t2
param$p_fatal_a3 = p_fatal_a_t3
param$p_fatal_a4 = p_fatal_a_t4
param$p_fatal_a5 = p_fatal_a_t5



##### Death rates:  symptomatic ####

# time function
p_fatal_s_t = rep(20/100, duration_time)
p_fatal_s_t[601:duration_time] = rep(30/100, duration_time-600)
p_fatal_s_t[621:duration_time] = rep(32/100, duration_time-620)
p_fatal_s_t[641:duration_time] = rep(20/100, duration_time-640)
p_fatal_s_t[661:duration_time] = rep(25/100, duration_time-660)

#for each group
p_fatal_s_t1 = 0.0 * p_fatal_s_t
p_fatal_s_t2 = 0.0 * p_fatal_s_t
p_fatal_s_t3 = 0.0 * p_fatal_s_t
p_fatal_s_t4 = 0.8 * p_fatal_s_t
p_fatal_s_t5 = 2.0 * p_fatal_s_t

# tuning for different corona waves 
p_fatal_s_t3[370:duration_time] = 1.0 * p_fatal_s_t[370:duration_time]
p_fatal_s_t4[370:duration_time] = 1.75 * p_fatal_s_t[370:duration_time]
p_fatal_s_t5[370:duration_time] = 4.0 * p_fatal_s_t[370:duration_time]

p_fatal_s_t3[430:duration_time] = 1.0 * p_fatal_s_t[430:duration_time]
p_fatal_s_t4[430:duration_time] = 1.75 * p_fatal_s_t[430:duration_time]
p_fatal_s_t5[430:duration_time] = 2.0 * p_fatal_s_t[430:duration_time]

p_fatal_s_t3[500:duration_time] = 2.0 * p_fatal_s_t[500:duration_time]
p_fatal_s_t4[500:duration_time] = 3.5 * p_fatal_s_t[500:duration_time]
p_fatal_s_t5[500:duration_time] = 4.0 * p_fatal_s_t[500:duration_time]


param$p_fatal_s1 = p_fatal_s_t1
param$p_fatal_s2 = p_fatal_s_t2
param$p_fatal_s3 = p_fatal_s_t3
param$p_fatal_s4 = p_fatal_s_t4
param$p_fatal_s5 = p_fatal_s_t5


##### HOSP addmission rates ####

p_hosp_t = rep(10/100, duration_time)
p_hosp_t[601:duration_time] = rep(9/100, duration_time-600)
p_hosp_t[611:duration_time] = rep(9/100, duration_time-610)
p_hosp_t[641:duration_time] = rep(8.5/100, duration_time-640)

# Omicron
# 670 = 2.1.2022
p_hosp_t[661:duration_time] = rep(10.5/100, duration_time-660)
p_hosp_t[671:duration_time] = rep(5.5/100, duration_time-670)
p_hosp_t[681:duration_time] = rep(3.5/100, duration_time-680)
p_hosp_t[686:duration_time] = rep(2.0/100, duration_time-685)
p_hosp_t[691:duration_time] = rep(2.5/100, duration_time-690)
p_hosp_t[704:duration_time] = rep(3.5/100, duration_time-703)
p_hosp_t[711:duration_time] = rep(4.5/100, duration_time-710)
p_hosp_t[721:duration_time] = rep(5.0/100, duration_time-720)
p_hosp_t[721:duration_time] = 4.5/100

p_hosp_t1 = rep(0, duration_time)
p_hosp_t2 = rep(0, duration_time)
p_hosp_t3 = 0.1 * p_hosp_t
p_hosp_t4 = 1.0 * p_hosp_t
p_hosp_t5 = 1.5 * p_hosp_t


p_hosp_t3[320:duration_time] = 0.1*p_hosp_t[320:duration_time]
p_hosp_t4[320:duration_time] = 1.0*p_hosp_t[320:duration_time]
p_hosp_t5[320:duration_time] = 1.2*p_hosp_t[320:duration_time]


# alpha

p_hosp_t3[390:duration_time] = 0.3*p_hosp_t[390:duration_time]
p_hosp_t4[390:duration_time] = 1.0*p_hosp_t[390:duration_time]
p_hosp_t5[390:duration_time] = 1.5*p_hosp_t[390:duration_time]

p_hosp_t3[430:duration_time] = 0.3*p_hosp_t[430:duration_time]
p_hosp_t4[430:duration_time] = 1.0*p_hosp_t[430:duration_time]
p_hosp_t5[430:duration_time] = 1.5*p_hosp_t[430:duration_time]

p_hosp_t3[550:duration_time] = 0.4*p_hosp_t[550:duration_time]
p_hosp_t4[550:duration_time] = 1.0*p_hosp_t[550:duration_time]
p_hosp_t5[550:duration_time] = 1.3*p_hosp_t[550:duration_time]


param$p_hosp1 = p_hosp_t1
param$p_hosp2 = p_hosp_t2
param$p_hosp3 = p_hosp_t3
param$p_hosp4 = p_hosp_t4
param$p_hosp5 = p_hosp_t5


##### ICU addmission rates ####

p_icu_t = rep(15/100, duration_time)
p_icu_t[381:duration_time] = rep(23/100, duration_time-380)
p_icu_t[501:duration_time] = rep(25/100, duration_time-500)
p_icu_t[601:duration_time] = rep(25/100, duration_time-600)
p_icu_t[611:duration_time] = rep(21/100, duration_time-610)
p_icu_t[621:duration_time] = rep(25/100, duration_time-620)
p_icu_t[641:duration_time] = rep(30/100, duration_time-640)
p_icu_t[665:duration_time] = rep(40/100, duration_time-664)


# Omicron
# 670 = 2.1.2022
p_icu_t[671:duration_time] = rep(30/100, duration_time-670)
p_icu_t[681:duration_time] = rep(25/100, duration_time-680)
p_icu_t[691:duration_time] = rep(15/100, duration_time-690)
p_icu_t[701:duration_time] = rep(15/100, duration_time-700)
p_icu_t[725:duration_time] =  25/100

p_icu_t1 = 0.0 * p_icu_t
p_icu_t2 = 0.0 * p_icu_t
p_icu_t3 = 0.0 * p_icu_t
p_icu_t4 = 0.9 * p_icu_t
p_icu_t5 = 1.3 * p_icu_t

# Alpha
p_icu_t3[400:duration_time] = 1.0 * p_icu_t[400:duration_time]
p_icu_t4[400:duration_time] = 1.0 * p_icu_t[400:duration_time]
p_icu_t5[400:duration_time] = 1.0 * p_icu_t[400:duration_time]

p_icu_t3[445:duration_time] = 0.9 * p_icu_t[445:duration_time]
p_icu_t4[445:duration_time] = 1.0 * p_icu_t[445:duration_time]
p_icu_t5[445:duration_time] = 1.0 * p_icu_t[445:duration_time]

p_icu_t3[550:duration_time] = 1.0 * p_icu_t[550:duration_time]
p_icu_t4[550:duration_time] = 1.0 * p_icu_t[550:duration_time]
p_icu_t5[550:duration_time] = 1.0 * p_icu_t[550:duration_time]

p_icu_t3[600:duration_time] = 1.0 * p_icu_t[600:duration_time]
p_icu_t4[600:duration_time] = 1.0 * p_icu_t[600:duration_time]
p_icu_t5[600:duration_time] = 1.0 * p_icu_t[600:duration_time]


param$p_ICU1 = p_icu_t1
param$p_ICU2 = p_icu_t2
param$p_ICU3 = p_icu_t3
param$p_ICU4 = p_icu_t4
param$p_ICU5 = p_icu_t5




#### shifting parameters ####
param$premik_hosp = 6
param$premik_icu  = 5
param$premik_smrti = 0



############################################
##### Mixing matrices ####
############################################

# Mixing matrix, after assembly it is transformed
mix_mat1 = c(1.00, 1.00, 1.00, 1.00, 1.00,
             1.00, 1.15, 1.15, 1.00, 1.00,
             1.00, 1.15, 1.15, 1.00, 1.00,
             1.00, 1.00, 1.00, 1.00, 1.00,
             1.00, 1.00, 1.00, 1.00, 1.00)

mix_mat2 = c(1.50, 1.25, 1.20, 1.05, 1.05,
             1.00, 1.15, 1.15, 1.00, 1.00,
             1.00, 1.15, 1.15, 1.00, 1.00,
             1.00, 1.00, 1.00, 1.00, 1.00,
             1.00, 1.00, 1.00, 1.00, 1.00)


# Increasing mixing from September 1, 2021
mix_mat3 = c(1.75, 1.30, 1.25, 1.05, 1.05,
             1.00, 1.20, 1.20, 1.00, 1.00,
             1.00, 1.20, 1.20, 1.00, 1.00,
             1.00, 1.00, 1.00, 1.00, 1.00,
             1.00, 1.00, 1.00, 1.00, 1.00)


# "Increasing mixing from October 1, 2021"
mix_mat4 = c(3.00, 1.30, 1.25, 1.05, 1.05,
             1.00, 1.20, 1.20, 1.00, 1.00,
             1.00, 1.20, 1.20, 1.00, 1.00,
             1.00, 1.00, 1.00, 1.00, 1.00,
             1.00, 1.00, 1.00, 1.00, 1.00)



# "Increasing mixing from January 1, 2022"
mix_mat5 = c(3.00, 1.30, 1.25, 1.05, 1.05,
             1.00, 1.50, 1.50, 1.00, 1.00,
             1.00, 1.50, 1.50, 1.00, 1.00,
             1.00, 1.00, 1.00, 1.00, 1.00,
             1.00, 1.00, 1.00, 1.00, 1.00)


mix_mat5 = mix_mat4

cas_spr1 = as.numeric(as.Date("2021-02-01") - as.Date("2020-03-04"))
cas_spr2 = as.numeric(as.Date("2021-09-01") - as.Date("2021-02-01"))
cas_spr3 = as.numeric(as.Date("2021-10-10") - as.Date("2021-09-01"))
cas_spr4 = as.numeric(as.Date("2022-01-01") - as.Date("2021-10-01"))

# sestavimo matrike po času
m1 = array(mix_mat1, dim=c(5,5,cas_spr1))
m2 = array(mix_mat2, dim=c(5,5,cas_spr2))
m3 = array(mix_mat3, dim=c(5,5,cas_spr3))
m4 = array(mix_mat4, dim=c(5,5,cas_spr4))
m5 = array(mix_mat5, dim=c(5,5,duration_time-cas_spr4))

mix_array = abind::abind(m1, m2, m3, m4, m5)

param$MixMat = mix_array






###################################################################
### Vaccination ####
###################################################################


date_diff_vacc = 300

# vaccination rates are computed from data for each age group

# age group 5

vdat = vaccdat$cep5.2nd
vdat[is.na(vdat)] = 0
#"Daily increase in proportion"
koef_vdat = diff(vdat) / N5 
zac_date_diff = date_diff_vacc  # vax was not administered until day 300

uw = c(0.0, w5 * koef_vdat, 0.0)  # equivalent to daily number of dose administered divided by Total population
casV = c(zac_date_diff, rep(1, length(koef_vdat)), 10)   #how often vaccination date was updated 

vfun5 = Bt_rect_time(duration_time, uw, casV)


# age group 4

vdat = vaccdat$cep4.2nd
vdat[is.na(vdat)] = 0
#"Daily increase in proportion"
koef_vdat = diff(vdat) / N4 
zac_date_diff = date_diff_vacc

uw = c(0.0, w4 * koef_vdat, 0.0) # equivalent to daily number of dose administered divided by Total population
casV = c(zac_date_diff, rep(1, length(koef_vdat)), 10)

vfun4 = Bt_rect_time(duration_time, uw, casV)


# age group 3

vdat = vaccdat$cep3.2nd
vdat[is.na(vdat)] = 0
#"Daily increase in proportion"
koef_vdat = diff(vdat) / N3 
zac_date_diff = date_diff_vacc

#scenarij cepljenja
uw = c(0.0, w3 * koef_vdat, w3 * 0.02/100, 0.0 ) # equivalent to daily number of dose administered divided by Total population
casV = c(zac_date_diff, rep(1, length(koef_vdat)), 100, 10)

vfun3 = Bt_rect_time(duration_time, uw, casV)



# age group 2

vdat = vaccdat$cep2.2nd
#"Daily increase in proportion"
koef_vdat = diff(vdat) / N2 
zac_date_diff = date_diff_vacc

uw = c(0.0, w2 * koef_vdat, w2* 0.03/100, 0.0)# equivalent to daily number of dose administered divided by Total population
casV = c(zac_date_diff, rep(1, length(koef_vdat)), 100, 10)

vfun2 = Bt_rect_time(duration_time, uw, casV)


# age group 1

vdat = vaccdat$cep1.2nd
vdat[is.na(vdat)] = 0
#"Daily increase in proportion"
koef_vdat = diff(vdat) / N1 
zac_date_diff = date_diff_vacc

uw = c(0.0, w1 * koef_vdat, w1*0.06/100, 0.0) # equivalent to daily number of dose administered divided by Total population
casV = c(zac_date_diff, rep(1, length(koef_vdat)), 100, 10)

vfun1 = Bt_rect_time(duration_time, uw, casV)


param$vaccfun1 = vfun1
param$vaccfun2 = vfun2
param$vaccfun3 = vfun3
param$vaccfun4 = vfun4
param$vaccfun5 = vfun5




###### calibration parameterts ######

param$calibrate_infected = TRUE
param$calibrate_hosp     = TRUE
param$calibrate_icu      = TRUE
param$calibrate_death    = TRUE

param$calib_tolerance    = 0.20


###### confidence intervals ######
## obsolete
## confidence intervals are computed from model simulations

param$compute_CI = FALSE
param$CL = 0.20
param$CU = 0.20







