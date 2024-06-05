library(deSolve)
library(imputeTS)


##### model ######


seir_vacc_nonvacc_waning_5grps_V01 = function(current_timepoint, state_values, parameters)
{
  
  # local varaibles
  S1 = state_values [1]          # susceptibles 1
  S2 = state_values [2]          # susceptibles 2
  S3 = state_values [3]          # susceptibles 3
  S4 = state_values [4]          # susceptibles 4
  S5 = state_values [5]          # susceptibles 5
  
  
  E1 = state_values [6]          # exposed 1
  E2 = state_values [7]          # exposed 2
  E3 = state_values [8]          # exposed 3
  E4 = state_values [9]          # exposed 4
  E5 = state_values [10]          # exposed 5
  
  
  I1 = state_values [11]          # infectious 1
  I2 = state_values [12]          # infectious 2
  I3 = state_values [13]          # infectious 3
  I4 = state_values [14]          # infectious 4
  I5 = state_values [15]          # infectious 5
  
  
  Mild1   = state_values [16]       # Recovering mild
  Mild2   = state_values [17]       # Recovering mild
  Mild3   = state_values [18]       # Recovering mild
  Mild4   = state_values [19]       # Recovering mild
  Mild5   = state_values [20]       # Recovering mild
  
  
  Hosp1   = state_values [21]       # recovering hospital
  Hosp2   = state_values [22]       # recovering hospital
  Hosp3   = state_values [23]       # recovering hospital
  Hosp4   = state_values [24]       # recovering hospital
  Hosp5   = state_values [25]       # recovering hospital
  
  
  ICU1    = state_values [26]        # recovering ICU
  ICU2    = state_values [27]        # recovering ICU
  ICU3    = state_values [28]        # recovering ICU
  ICU4    = state_values [29]        # recovering ICU
  ICU5    = state_values [30]        # recovering ICU
  
  
  Dead_s1 = state_values [31]     # dying symp
  Dead_s2 = state_values [32]     # dying symp
  Dead_s3 = state_values [33]     # dying symp
  Dead_s4 = state_values [34]     # dying symp
  Dead_s5 = state_values [35]     # dying symp
  
  
  Dead_a1 = state_values [36]     # dying asymp
  Dead_a2 = state_values [37]     # dying asymp
  Dead_a3 = state_values [38]     # dying asymp
  Dead_a4 = state_values [39]     # dying asymp
  Dead_a5 = state_values [40]     # dying asymp
  
  
  Rmild1 = state_values [41]        # recovered mild
  Rmild2 = state_values [42]        # recovered mild
  Rmild3 = state_values [43]        # recovered mild
  Rmild4 = state_values [44]        # recovered mild
  Rmild5 = state_values [45]        # recovered mild
  
  
  Rhosp1 = state_values [46]       # recovered hospital
  Rhosp2 = state_values [47]       # recovered hospital
  Rhosp3 = state_values [48]       # recovered hospital
  Rhosp4 = state_values [49]       # recovered hospital
  Rhosp5 = state_values [50]       # recovered hospital
  
  
  Ricu1 = state_values [51]        # recovered ICU
  Ricu2 = state_values [52]        # recovered ICU
  Ricu3 = state_values [53]        # recovered ICU
  Ricu4 = state_values [54]        # recovered ICU
  Ricu5 = state_values [55]        # recovered ICU
  
  
  Rdead_s1 = state_values [56]     # dead
  Rdead_s2 = state_values [57]     # dead
  Rdead_s3 = state_values [58]     # dead
  Rdead_s4 = state_values [59]     # dead
  Rdead_s5 = state_values [60]     # dead
  
  
  Rdead_a1 = state_values [61]     # dead
  Rdead_a2 = state_values [62]     # dead
  Rdead_a3 = state_values [63]     # dead
  Rdead_a4 = state_values [64]     # dead
  Rdead_a5 = state_values [65]     # dead
  
  V1 = state_values [66]           # vaccinated
  V2 = state_values [67]           # vaccinated
  V3 = state_values [68]           # vaccinated
  V4 = state_values [69]           # vaccinated
  V5 = state_values [70]           # vaccinated
  
  Sv1 = state_values [71]           # suseptible vaccinated
  Sv2 = state_values [72]           # suseptible vaccinated
  Sv3 = state_values [73]           # suseptible vaccinated
  Sv4 = state_values [74]           # suseptible vaccinated
  Sv5 = state_values [75]           # suseptible vaccinated
  
  Ev1 = state_values [76]          # exposed 1 vacc
  Ev2 = state_values [77]          # exposed 2
  Ev3 = state_values [78]          # exposed 3
  Ev4 = state_values [79]          # exposed 4
  Ev5 = state_values [80]          # exposed 5
  
  
  Iv1 = state_values [81]          # infectious 1 vacc
  Iv2 = state_values [82]          # infectious 2
  Iv3 = state_values [83]          # infectious 3
  Iv4 = state_values [84]          # infectious 4
  Iv5 = state_values [85]          # infectious 5
  
  Mild1v   = state_values [86]       # Recovering mild vacc
  Mild2v   = state_values [87]       # Recovering mild
  Mild3v   = state_values [88]       # Recovering mild
  Mild4v   = state_values [89]       # Recovering mild
  Mild5v   = state_values [90]       # Recovering mild
  
  
  Hosp1v   = state_values [91]       # recovering hospital vacc
  Hosp2v   = state_values [92]       # recovering hospital
  Hosp3v   = state_values [93]       # recovering hospital
  Hosp4v   = state_values [94]       # recovering hospital
  Hosp5v   = state_values [95]       # recovering hospital
  
  
  ICU1v    = state_values [96]        # recovering ICU vacc
  ICU2v    = state_values [97]        # recovering ICU
  ICU3v    = state_values [98]        # recovering ICU
  ICU4v    = state_values [99]        # recovering ICU
  ICU5v    = state_values [100]        # recovering ICU
  
  
  Dead_s1v = state_values [101]     # dying symp vacc
  Dead_s2v = state_values [102]     # dying symp
  Dead_s3v = state_values [103]     # dying symp
  Dead_s4v = state_values [104]     # dying symp
  Dead_s5v = state_values [105]     # dying symp
  
  
  Dead_a1v = state_values [106]     # dying asymp vacc
  Dead_a2v = state_values [107]     # dying asymp
  Dead_a3v = state_values [108]     # dying asymp
  Dead_a4v = state_values [109]     # dying asymp
  Dead_a5v = state_values [110]     # dying asymp
  
  
  Rmild1v = state_values [111]        # recovered mild vacc
  Rmild2v = state_values [112]        # recovered mild
  Rmild3v = state_values [113]        # recovered mild
  Rmild4v = state_values [114]        # recovered mild
  Rmild5v = state_values [115]        # recovered mild
  
  
  Rhosp1v = state_values [116]       # recovered hospital vacc
  Rhosp2v = state_values [117]       # recovered hospital
  Rhosp3v = state_values [118]       # recovered hospital
  Rhosp4v = state_values [119]       # recovered hospital
  Rhosp5v = state_values [120]       # recovered hospital
  
  
  Ricu1v = state_values [121]        # recovered ICU vacc
  Ricu2v = state_values [122]        # recovered ICU
  Ricu3v = state_values [123]        # recovered ICU
  Ricu4v = state_values [124]        # recovered ICU
  Ricu5v = state_values [125]        # recovered ICU
  
  
  Rdead_s1v = state_values [126]     # dead vacc
  Rdead_s2v = state_values [127]     # dead
  Rdead_s3v = state_values [128]     # dead
  Rdead_s4v = state_values [129]     # dead
  Rdead_s5v = state_values [130]     # dead
  
  
  Rdead_a1v = state_values [131]     # dead vacc
  Rdead_a2v = state_values [132]     # dead
  Rdead_a3v = state_values [133]     # dead
  Rdead_a4v = state_values [134]     # dead
  Rdead_a5v = state_values [135]     # dead
  
  #Auxiliary VACC for drawing
  Vp1 = state_values [136]           # vaccinated
  Vp2 = state_values [137]           # vaccinated
  Vp3 = state_values [138]           # vaccinated
  Vp4 = state_values [139]           # vaccinated
  Vp5 = state_values [140]           # vaccinated
  
  #Other auxiliary parameters
  Ein1 = state_values [141]
  Ein2 = state_values [142]
  Ein3 = state_values [143]
  Ein4 = state_values [144]
  Ein5 = state_values [145]
  
  Einv1 = state_values [146]
  Einv2 = state_values [147]
  Einv3 = state_values [148]
  Einv4 = state_values [149]
  Einv5 = state_values [150]
  
  
  Hosp1in = state_values [151]
  Hosp2in = state_values [152]
  Hosp3in = state_values [153]
  Hosp4in = state_values [154]
  Hosp5in = state_values [155]
  
  Hosp1inv = state_values [156]
  Hosp2inv = state_values [157]
  Hosp3inv = state_values [158]
  Hosp4inv = state_values [159]
  Hosp5inv = state_values [160]
  
  ICU1in = state_values [161]
  ICU1in = state_values [162]
  ICU1in = state_values [163]
  ICU1in = state_values [164]
  ICU1in = state_values [165]
  
  ICU1inv = state_values [166]
  ICU1inv = state_values [167]
  ICU1inv = state_values [168]
  ICU1inv = state_values [169]
  ICU1inv = state_values [170]
  
 
  
  
  
  
  # parameters
  
  # asymptomatic death rate (Pz)
  p_fatal_a_tm1 = parameters[["p_fatal_a1"]]
  p_fatal_a_tm2 = parameters[["p_fatal_a2"]]
  p_fatal_a_tm3 = parameters[["p_fatal_a3"]]
  p_fatal_a_tm4 = parameters[["p_fatal_a4"]]
  p_fatal_a_tm5 = parameters[["p_fatal_a5"]]
  
  p_fatal_a_t1 = p_fatal_a_tm1[current_timepoint]
  p_fatal_a_t2 = p_fatal_a_tm2[current_timepoint]
  p_fatal_a_t3 = p_fatal_a_tm3[current_timepoint]
  p_fatal_a_t4 = p_fatal_a_tm4[current_timepoint]
  p_fatal_a_t5 = p_fatal_a_tm5[current_timepoint]
  
  # symptomatic death rate (1-Pr)
  p_fatal_s_tm1 = parameters[["p_fatal_s1"]]
  p_fatal_s_tm2 = parameters[["p_fatal_s2"]]
  p_fatal_s_tm3 = parameters[["p_fatal_s3"]]
  p_fatal_s_tm4 = parameters[["p_fatal_s4"]]
  p_fatal_s_tm5 = parameters[["p_fatal_s5"]]
  
  p_fatal_s_t1 = p_fatal_s_tm1[current_timepoint]
  p_fatal_s_t2 = p_fatal_s_tm2[current_timepoint]
  p_fatal_s_t3 = p_fatal_s_tm3[current_timepoint]
  p_fatal_s_t4 = p_fatal_s_tm4[current_timepoint]
  p_fatal_s_t5 = p_fatal_s_tm5[current_timepoint]
  
  # Proportion of hospitalization requiring ICU (Pc)
  p_icu_tm1 = parameters[["p_ICU1"]]
  p_icu_tm2 = parameters[["p_ICU2"]]
  p_icu_tm3 = parameters[["p_ICU3"]]
  p_icu_tm4 = parameters[["p_ICU4"]]
  p_icu_tm5 = parameters[["p_ICU5"]]
  
  p_icu_t1 = p_icu_tm1[current_timepoint]
  p_icu_t2 = p_icu_tm2[current_timepoint]
  p_icu_t3 = p_icu_tm3[current_timepoint]
  p_icu_t4 = p_icu_tm4[current_timepoint]
  p_icu_t5 = p_icu_tm5[current_timepoint]
  
  # Proportion of infections requiring hospitalization (Ph)
  p_hosp_tm1 = parameters[["p_hosp1"]]
  p_hosp_tm2 = parameters[["p_hosp2"]]
  p_hosp_tm3 = parameters[["p_hosp3"]]
  p_hosp_tm4 = parameters[["p_hosp4"]]
  p_hosp_tm5 = parameters[["p_hosp5"]]
  
  p_hosp_t1 = p_hosp_tm1[current_timepoint]
  p_hosp_t2 = p_hosp_tm2[current_timepoint]
  p_hosp_t3 = p_hosp_tm3[current_timepoint]
  p_hosp_t4 = p_hosp_tm4[current_timepoint]
  p_hosp_t5 = p_hosp_tm5[current_timepoint]
  
  
  # daily transmission coefficients (Rt times 1/infectious period)
  Bt = parameters[["Bfun"]]  # Bt is function of time
  beta  = Bt[current_timepoint]
  
  # a time-varying mixing matrix is added
  mix_mat_t = parameters[["MixMat"]]
  mix_mat = mix_mat_t[,,current_timepoint]
  
  # daily transmission coefficient combined with contact matrix
  beta_11 = mix_mat[1,1] * beta
  beta_21 = mix_mat[2,1] * beta
  beta_31 = mix_mat[3,1] * beta
  beta_41 = mix_mat[4,1] * beta
  beta_51 = mix_mat[5,1] * beta
  
  beta_12 = mix_mat[1,2] * beta
  beta_22 = mix_mat[2,2] * beta
  beta_32 = mix_mat[3,2] * beta
  beta_42 = mix_mat[4,2] * beta
  beta_52 = mix_mat[5,2] * beta
  
  beta_13 = mix_mat[1,3] * beta
  beta_23 = mix_mat[2,3] * beta
  beta_33 = mix_mat[3,3] * beta
  beta_43 = mix_mat[4,3] * beta
  beta_53 = mix_mat[5,3] * beta
  
  beta_14 = mix_mat[1,4] * beta
  beta_24 = mix_mat[2,4] * beta
  beta_34 = mix_mat[3,4] * beta
  beta_44 = mix_mat[4,4] * beta
  beta_54 = mix_mat[5,4] * beta
  
  beta_15 = mix_mat[1,5] * beta
  beta_25 = mix_mat[2,5] * beta
  beta_35 = mix_mat[3,5] * beta
  beta_45 = mix_mat[4,5] * beta
  beta_55 = mix_mat[5,5] * beta
  
  
  # daily vaccination rate by age group
  ut1 = parameters[["vaccfun1"]]
  u1  = ut1[current_timepoint]
  
  ut2 = parameters[["vaccfun2"]]
  u2  = ut2[current_timepoint]
  
  ut3 = parameters[["vaccfun3"]]
  u3  = ut3[current_timepoint]
  
  ut4 = parameters[["vaccfun4"]]
  u4  = ut4[current_timepoint]
  
  ut5 = parameters[["vaccfun5"]]
  u5  = ut5[current_timepoint]
  
  
  # latent period (alpha)
  at      = 1/parameters[["D_incubation"]]
  a       = at[current_timepoint]
  
  # infectious period (gamma)
  gt      = 1/parameters[["D_infectious"]]
  g       = gt[current_timepoint]
  
  # duration of asymp infection (gammaM)
  gMt     = 1/parameters[["D_recovery_mild"]]
  gM       = gMt[current_timepoint]
  
  # duration of non-ICU hosp infection (gammaH)
  gHOSPt  = 1/parameters[["D_recovery_hosp"]]
  gHOSP   = gHOSPt[current_timepoint]
  
  # duration of ICU hosp infection (gammaC)
  gICUt   = 1/parameters[["D_recovery_ICU"]]
  gICU    = gICUt[current_timepoint]
  
  # duration of disease before death in nursing home infection (gammaZ)
  gFt     = 1/parameters[["D_death"]]
  gF      = gFt[current_timepoint]
  
  # duration of natural waning immmunity (wfR)
  wrt     = 1/parameters[["D_waning_R"]]
  wr      = wrt[current_timepoint]
  
  # duration of vax waning immmunity (wfV)
  wvt      = 1/parameters[["D_waning_V"]]
  wv       = wvt[current_timepoint]
  
  #pv     = parameters[["stopnja_imunosti_cep"]]
  
  # reduced risk of infection (fIV)
  facIvacc_t = parameters[["faktor_kuznosti_cep"]]
  facIvacc  = facIvacc_t[current_timepoint]
  
  # reduced risk of hospitalization (fHV)
  facHvacc_t = parameters[["faktor_hosp_cep"]]
  facHvacc = facHvacc_t[current_timepoint]
  
  # reduced risk of ICU admission (?)
  facICUvacc_t = parameters[["faktor_icu_cep"]]
  facICUvacc = facICUvacc_t[current_timepoint]
  
  # reduced risk of death (fDV)
  facDvacc_t = parameters[["faktor_smrti_cep"]]
  facDvacc = facDvacc_t[current_timepoint]

  # asymp symp death rate among vaccinated
  p_fatal_a_t1v = 1/facDvacc * p_fatal_a_tm1[current_timepoint]
  p_fatal_a_t2v = 1/facDvacc * p_fatal_a_tm2[current_timepoint]
  p_fatal_a_t3v = 1/facDvacc * p_fatal_a_tm3[current_timepoint]
  p_fatal_a_t4v = 1/facDvacc * p_fatal_a_tm4[current_timepoint]
  p_fatal_a_t5v = 1/facDvacc * p_fatal_a_tm5[current_timepoint]
  
  p_fatal_s_t1v = 1/facDvacc * p_fatal_s_tm1[current_timepoint]
  p_fatal_s_t2v = 1/facDvacc * p_fatal_s_tm2[current_timepoint]
  p_fatal_s_t3v = 1/facDvacc * p_fatal_s_tm3[current_timepoint]
  p_fatal_s_t4v = 1/facDvacc * p_fatal_s_tm4[current_timepoint]
  p_fatal_s_t5v = 1/facDvacc * p_fatal_s_tm5[current_timepoint]
  
  # proportion hospitalizations requiring ICU among vaccinated (pCV)
  p_icu_t1v = 1/facICUvacc * p_icu_tm1[current_timepoint]
  p_icu_t2v = 1/facICUvacc * p_icu_tm2[current_timepoint]
  p_icu_t3v = 1/facICUvacc * p_icu_tm3[current_timepoint]
  p_icu_t4v = 1/facICUvacc * p_icu_tm4[current_timepoint]
  p_icu_t5v = 1/facICUvacc * p_icu_tm5[current_timepoint]
  
  
  # proportion infections requiring hospitalizations among vaccinated (pHV)
  p_hosp_t1v = 1/facHvacc * p_hosp_tm1[current_timepoint]
  p_hosp_t2v = 1/facHvacc * p_hosp_tm2[current_timepoint]
  p_hosp_t3v = 1/facHvacc * p_hosp_tm3[current_timepoint]
  p_hosp_t4v = 1/facHvacc * p_hosp_tm4[current_timepoint]
  p_hosp_t5v = 1/facHvacc * p_hosp_tm5[current_timepoint]
  
  
  
  # proportion asymp infection 
  p_mild1 = 1 - p_hosp_t1 - p_fatal_a_t1
  p_mild2 = 1 - p_hosp_t2 - p_fatal_a_t2
  p_mild3 = 1 - p_hosp_t3 - p_fatal_a_t3
  p_mild4 = 1 - p_hosp_t4 - p_fatal_a_t4
  p_mild5 = 1 - p_hosp_t5 - p_fatal_a_t5
  
  # proportion asymp infection among vaccinated
  p_mild1v = 1 - p_hosp_t1v - p_fatal_a_t1v
  p_mild2v = 1 - p_hosp_t2v - p_fatal_a_t2v
  p_mild3v = 1 - p_hosp_t3v - p_fatal_a_t3v
  p_mild4v = 1 - p_hosp_t4v - p_fatal_a_t4v
  p_mild5v = 1 - p_hosp_t5v - p_fatal_a_t5v
  
  
  with (
    as.list (parameters),
    {
      # daily force of infection 
      lam1 = (beta_11*I1 + beta_12*I2 + beta_13*I3 + beta_14*I4 + beta_15*I5 + 
                beta_11*Iv1 + beta_12*Iv2 + beta_13*Iv3 + beta_14*Iv4 + beta_15*Iv5)
      
      lam2 = (beta_21*I1 + beta_22*I2 + beta_23*I3 + beta_24*I4 + beta_25*I5 +
                beta_21*Iv1 + beta_22*Iv2 + beta_23*Iv3 + beta_24*Iv4 + beta_25*Iv5)
      
      lam3 = (beta_31*I1 + beta_32*I2 + beta_33*I3 + beta_34*I4 + beta_35*I5 +
                beta_31*Iv1 + beta_32*Iv2 + beta_33*Iv3 + beta_34*Iv4 + beta_35*Iv5)
      
      lam4 = (beta_41*I1 + beta_42*I2 + beta_43*I3 + beta_44*I4 + beta_45*I5 +
                beta_41*Iv1 + beta_42*Iv2 + beta_43*Iv3 + beta_44*Iv4 + beta_45*Iv5)
      
      lam5 = (beta_51*I1 + beta_52*I2 + beta_53*I3 + beta_54*I4 + beta_55*I5 +
                beta_51*Iv1 + beta_52*Iv2 + beta_53*Iv3 + beta_54*Iv4 + beta_55*Iv5)
      
      
      # daily force of infection among vaccinated
      lam1v = 1/facIvacc * lam1
      lam2v = 1/facIvacc * lam2
      lam3v = 1/facIvacc * lam3
      lam4v = 1/facIvacc * lam4
      lam5v = 1/facIvacc * lam5
      

      
      ####################### ODE ######################
      
      ##### Susceptible
      
      #susceptible population (S)
      dS1 = -lam1 * S1 - u1 + wr*Rmild1 + wr*Rhosp1 + wr*Ricu1 + wr*Rmild1v + wr*Rhosp1v + wr*Ricu1v + wv*V1  
      dS2 = -lam2 * S2 - u2 + wr*Rmild2 + wr*Rhosp2 + wr*Ricu2 + wr*Rmild2v + wr*Rhosp2v + wr*Ricu2v + wv*V2  
      dS3 = -lam3 * S3 - u3 + wr*Rmild3 + wr*Rhosp3 + wr*Ricu3 + wr*Rmild3v + wr*Rhosp3v + wr*Ricu3v + wv*V3  
      dS4 = -lam4 * S4 - u4 + wr*Rmild4 + wr*Rhosp4 + wr*Ricu4 + wr*Rmild4v + wr*Rhosp4v + wr*Ricu4v + wv*V4  
      dS5 = -lam5 * S5 - u5 + wr*Rmild5 + wr*Rhosp5 + wr*Ricu5 + wr*Rmild5v + wr*Rhosp5v + wr*Ricu5v + wv*V5  
      
      # Auxiliary for drawing (?)
      dV1 = u1 - wv*V1 
      dV2 = u2 - wv*V2
      dV3 = u3 - wv*V3
      dV4 = u4 - wv*V4
      dV5 = u5 - wv*V5
      
      # Auxiliary for drawing (?)
      dVp1 = u1  
      dVp2 = u2 
      dVp3 = u3 
      dVp4 = u4 
      dVp5 = u5
      
      #susceptible vaccinated population (Sv)
      dSv1 = u1 - wv*V1 -lam1v * Sv1
      dSv2 = u2 - wv*V2 -lam2v * Sv2
      dSv3 = u3 - wv*V3 -lam3v * Sv3
      dSv4 = u4 - wv*V4 -lam4v * Sv4
      dSv5 = u5 - wv*V5 -lam5v * Sv5
      
      
      ##### Exposed 
      
      #exposed 
      dE1 = lam1 * S1 - (a * E1)
      dE2 = lam2 * S2 - (a * E2)
      dE3 = lam3 * S3 - (a * E3)
      dE4 = lam4 * S4 - (a * E4)
      dE5 = lam5 * S5 - (a * E5)
      
      # cumulative number of infected
      dEin1 = lam1 * S1 
      dEin2 = lam2 * S2 
      dEin3 = lam3 * S3 
      dEin4 = lam4 * S4 
      dEin5 = lam5 * S5 
      
      # vaccinated exposed
      dEv1 = lam1v * Sv1 - (a * Ev1)
      dEv2 = lam2v * Sv2 - (a * Ev2)
      dEv3 = lam3v * Sv3 - (a * Ev3)
      dEv4 = lam4v * Sv4 - (a * Ev4)
      dEv5 = lam5v * Sv5 - (a * Ev5)
      
      # cumulative number of infected if vacciated
      dEinv1 = lam1v * Sv1 
      dEinv2 = lam2v * Sv2 
      dEinv3 = lam3v * Sv3 
      dEinv4 = lam4v * Sv4 
      dEinv5 = lam5v * Sv5 
      
      
      ##### Infectious 
      
      # Infectious 
      
      dI1 = (a * E1) - (g * I1)  #gamma (infectious period)
      dI2 = (a * E2) - (g * I2)
      dI3 = (a * E3) - (g * I3)
      dI4 = (a * E4) - (g * I4)
      dI5 = (a * E5) - (g * I5)
      
      # Infectious among vaccinated
      dIv1 = (a * Ev1) - (g * Iv1)
      dIv2 = (a * Ev2) - (g * Iv2)
      dIv3 = (a * Ev3) - (g * Iv3)
      dIv4 = (a * Ev4) - (g * Iv4)
      dIv5 = (a * Ev5) - (g * Iv5)
      
      # asymp infeciton
      dMild1       =   p_mild1 * g * I1   - gM * Mild1 #recovered rate gM
      dMild2       =   p_mild2 * g * I2   - gM * Mild2
      dMild3       =   p_mild3 * g * I3   - gM * Mild3
      dMild4       =   p_mild4 * g * I4   - gM * Mild4
      dMild5       =   p_mild5 * g * I5   - gM * Mild5
      
      # asymp infeciton among vaccinated
      dMild1v       =   p_mild1v * g * Iv1   - gM * Mild1v
      dMild2v       =   p_mild2v * g * Iv2   - gM * Mild2v
      dMild3v       =   p_mild3v * g * Iv3   - gM * Mild3v
      dMild4v       =   p_mild4v * g * Iv4   - gM * Mild4v
      dMild5v       =   p_mild5v * g * Iv5   - gM * Mild5v
      
      # among vaccinated
      dHosp1       =   p_hosp_t1 * g * I1   - gHOSP * Hosp1
      dHosp2       =   p_hosp_t2 * g * I2   - gHOSP * Hosp2
      dHosp3       =   p_hosp_t3 * g * I3   - gHOSP * Hosp3
      dHosp4       =   p_hosp_t4 * g * I4   - gHOSP * Hosp4
      dHosp5       =   p_hosp_t5 * g * I5   - gHOSP * Hosp5
      
      # FOR PLOTTING
      dHosp1in       =   p_hosp_t1 * g * I1   
      dHosp2in       =   p_hosp_t2 * g * I2   
      dHosp3in       =   p_hosp_t3 * g * I3   
      dHosp4in       =   p_hosp_t4 * g * I4   
      dHosp5in       =   p_hosp_t5 * g * I5   
      
      # Hospitalized among vaccinated
      dHosp1v       =   p_hosp_t1v * g * Iv1   - gHOSP * Hosp1v
      dHosp2v       =   p_hosp_t2v * g * Iv2   - gHOSP * Hosp2v
      dHosp3v       =   p_hosp_t3v * g * Iv3   - gHOSP * Hosp3v
      dHosp4v       =   p_hosp_t4v * g * Iv4   - gHOSP * Hosp4v
      dHosp5v       =   p_hosp_t5v * g * Iv5   - gHOSP * Hosp5v
      
      # FOR PLOTTING
      dHosp1inv       =   p_hosp_t1v * g * Iv1   
      dHosp2inv       =   p_hosp_t2v * g * Iv2   
      dHosp3inv       =   p_hosp_t3v * g * Iv3   
      dHosp4inv       =   p_hosp_t4v * g * Iv4   
      dHosp5inv       =   p_hosp_t5v * g * Iv5   
      
      # ICU admission 
      dICU1        =   p_hosp_t1 * p_icu_t1 * g * I1   - gICU * ICU1
      dICU2        =   p_hosp_t2 * p_icu_t2 * g * I2   - gICU * ICU2
      dICU3        =   p_hosp_t3 * p_icu_t3 * g * I3   - gICU * ICU3
      dICU4        =   p_hosp_t4 * p_icu_t4 * g * I4   - gICU * ICU4
      dICU5        =   p_hosp_t5 * p_icu_t5 * g * I5   - gICU * ICU5
      
      # FOR PLOTTING
      dICU1in        =   p_hosp_t1 * p_icu_t1 * g * I1   
      dICU2in        =   p_hosp_t2 * p_icu_t2 * g * I2   
      dICU3in        =   p_hosp_t3 * p_icu_t3 * g * I3   
      dICU4in        =   p_hosp_t4 * p_icu_t4 * g * I4   
      dICU5in        =   p_hosp_t5 * p_icu_t5 * g * I5   
      
      
      # ICU admission among vaccinated
      dICU1v        =   p_hosp_t1v * p_icu_t1v * g * Iv1   - gICU * ICU1v
      dICU2v        =   p_hosp_t2v * p_icu_t2v * g * Iv2   - gICU * ICU2v
      dICU3v        =   p_hosp_t3v * p_icu_t3v * g * Iv3   - gICU * ICU3v
      dICU4v        =   p_hosp_t4v * p_icu_t4v * g * Iv4   - gICU * ICU4v
      dICU5v        =   p_hosp_t5v * p_icu_t5v * g * Iv5   - gICU * ICU5v
      
      # FOR PLOTTING
      dICU1inv        =   p_hosp_t1v * p_icu_t1v * g * Iv1   
      dICU2inv        =   p_hosp_t2v * p_icu_t2v * g * Iv2   
      dICU3inv        =   p_hosp_t3v * p_icu_t3v * g * Iv3   
      dICU4inv        =   p_hosp_t4v * p_icu_t4v * g * Iv4   
      dICU5inv        =   p_hosp_t5v * p_icu_t5v * g * Iv5   
      
      
      # Death (?)
      dDead_s1     =   p_fatal_s_t1 * p_hosp_t1 * p_icu_t1 * g * I1   - gICU * Dead_s1
      dDead_s2     =   p_fatal_s_t2 * p_hosp_t2 * p_icu_t2 * g * I2   - gICU * Dead_s2
      dDead_s3     =   p_fatal_s_t3 * p_hosp_t3 * p_icu_t3 * g * I3   - gICU * Dead_s3
      dDead_s4     =   p_fatal_s_t4 * p_hosp_t4 * p_icu_t4 * g * I4   - gICU * Dead_s4
      dDead_s5     =   p_fatal_s_t5 * p_hosp_t5 * p_icu_t5 * g * I5   - gICU * Dead_s5
      
      
      # Death among vaccinated (?)
      dDead_s1v     =   p_fatal_s_t1v * p_hosp_t1v * p_icu_t1v * g * Iv1   - gICU * Dead_s1v
      dDead_s2v     =   p_fatal_s_t2v * p_hosp_t2v * p_icu_t2v * g * Iv2   - gICU * Dead_s2v
      dDead_s3v     =   p_fatal_s_t3v * p_hosp_t3v * p_icu_t3v * g * Iv3   - gICU * Dead_s3v
      dDead_s4v     =   p_fatal_s_t4v * p_hosp_t4v * p_icu_t4v * g * Iv4   - gICU * Dead_s4v
      dDead_s5v     =   p_fatal_s_t5v * p_hosp_t5v * p_icu_t5v * g * Iv5   - gICU * Dead_s5v
      
      
      
      # Death (?)
      dDead_a1     =   p_fatal_a_t1 * g * I1    - gF * Dead_a1
      dDead_a2     =   p_fatal_a_t2 * g * I2    - gF * Dead_a2
      dDead_a3     =   p_fatal_a_t3 * g * I3    - gF * Dead_a3
      dDead_a4     =   p_fatal_a_t4 * g * I4    - gF * Dead_a4
      dDead_a5     =   p_fatal_a_t5 * g * I5    - gF * Dead_a5
      
      
      # Death among vaccinated (?)
      dDead_a1v     =   p_fatal_a_t1v * g * Iv1    - gF * Dead_a1v
      dDead_a2v     =   p_fatal_a_t2v * g * Iv2    - gF * Dead_a2v
      dDead_a3v     =   p_fatal_a_t3v * g * Iv3    - gF * Dead_a3v
      dDead_a4v     =   p_fatal_a_t4v * g * Iv4    - gF * Dead_a4v
      dDead_a5v     =   p_fatal_a_t5v * g * Iv5    - gF * Dead_a5v
      
      
      ##### Recovered
      
      # recovered from mild illness
      dRmild1      =   gM * Mild1  - wr*Rmild1
      dRmild2      =   gM * Mild2  - wr*Rmild2
      dRmild3      =   gM * Mild3  - wr*Rmild3
      dRmild4      =   gM * Mild4  - wr*Rmild4
      dRmild5      =   gM * Mild5  - wr*Rmild5
      
      # recovered from mild illness among vaccinated
      dRmild1v      =   gM * Mild1v  - wr*Rmild1v
      dRmild2v      =   gM * Mild2v  - wr*Rmild2v
      dRmild3v      =   gM * Mild3v  - wr*Rmild3v
      dRmild4v      =   gM * Mild4v  - wr*Rmild4v
      dRmild5v      =   gM * Mild5v  - wr*Rmild5v
      
      
      # recovered from hosp
      dRhosp1      =   gHOSP * Hosp1  - wr*Rhosp1
      dRhosp2      =   gHOSP * Hosp2  - wr*Rhosp2
      dRhosp3      =   gHOSP * Hosp3  - wr*Rhosp3
      dRhosp4      =   gHOSP * Hosp4  - wr*Rhosp4
      dRhosp5      =   gHOSP * Hosp5  - wr*Rhosp5
      
      # recovered from hosp among vaccinated
      dRhosp1v      =   gHOSP * Hosp1v  - wr*Rhosp1v
      dRhosp2v      =   gHOSP * Hosp2v  - wr*Rhosp2v
      dRhosp3v      =   gHOSP * Hosp3v  - wr*Rhosp3v
      dRhosp4v      =   gHOSP * Hosp4v  - wr*Rhosp4v
      dRhosp5v      =   gHOSP * Hosp5v  - wr*Rhosp5v
      
      
      
      # recovered from ICU
      dRicu1       =   gICU  * ICU1  - wr*Ricu1
      dRicu2       =   gICU  * ICU2  - wr*Ricu2
      dRicu3       =   gICU  * ICU3  - wr*Ricu3
      dRicu4       =   gICU  * ICU4  - wr*Ricu4
      dRicu5       =   gICU  * ICU5  - wr*Ricu5
      
      # recovered from ICU among vaccinated
      dRicu1v       =   gICU  * ICU1v  - wr*Ricu1v
      dRicu2v       =   gICU  * ICU2v  - wr*Ricu2v
      dRicu3v       =   gICU  * ICU3v  - wr*Ricu3v
      dRicu4v       =   gICU  * ICU4v  - wr*Ricu4v
      dRicu5v       =   gICU  * ICU5v  - wr*Ricu5v
      
      # Death ???
      dRdead_s1    =   gICU * Dead_s1
      dRdead_s2    =   gICU * Dead_s2
      dRdead_s3    =   gICU * Dead_s3
      dRdead_s4    =   gICU * Dead_s4
      dRdead_s5    =   gICU * Dead_s5
      
      dRdead_s1v    =   gICU * Dead_s1v
      dRdead_s2v    =   gICU * Dead_s2v
      dRdead_s3v    =   gICU * Dead_s3v
      dRdead_s4v    =   gICU * Dead_s4v
      dRdead_s5v    =   gICU * Dead_s5v
      
      dRdead_a1    =   gF * Dead_a1
      dRdead_a2    =   gF * Dead_a2
      dRdead_a3    =   gF * Dead_a3
      dRdead_a4    =   gF * Dead_a4
      dRdead_a5    =   gF * Dead_a5
      
      dRdead_a1v    =   gF * Dead_a1v
      dRdead_a2v    =   gF * Dead_a2v
      dRdead_a3v    =   gF * Dead_a3v
      dRdead_a4v    =   gF * Dead_a4v
      dRdead_a5v    =   gF * Dead_a5v
      
      
      
      
      
      
      # combine all the results
      results = c (dS1, dS2, dS3, dS4, dS5,
                   dE1, dE2, dE3, dE4, dE5,
                   dI1, dI2, dI3, dI4, dI5,
                   dMild1, dMild2, dMild3, dMild4, dMild5,
                   dHosp1, dHosp2, dHosp3, dHosp4, dHosp5,
                   dICU1, dICU2, dICU3, dICU4, dICU5,
                   dDead_s1, dDead_s2, dDead_s3, dDead_s4, dDead_s5,
                   dDead_a1, dDead_a2, dDead_a3, dDead_a4, dDead_a5,
                   dRmild1, dRmild2, dRmild3, dRmild4, dRmild5,
                   dRhosp1, dRhosp2, dRhosp3, dRhosp4, dRhosp5,
                   dRicu1, dRicu2, dRicu3, dRicu4, dRicu5,
                   dRdead_s1, dRdead_s2, dRdead_s3, dRdead_s4, dRdead_s5,
                   dRdead_a1, dRdead_a2, dRdead_a3, dRdead_a4, dRdead_a5,
                   dV1, dV2, dV3, dV4, dV5,
                   dSv1, dSv2, dSv3, dSv4, dSv5,
                   dEv1, dEv2, dEv3, dEv4, dEv5,
                   dIv1, dIv2, dIv3, dIv4, dIv5,
                   dMild1v, dMild2v, dMild3v, dMild4v, dMild5v,
                   dHosp1v, dHosp2v, dHosp3v, dHosp4v, dHosp5v,
                   dICU1v, dICU2v, dICU3v, dICU4v, dICU5v,
                   dDead_s1v, dDead_s2v, dDead_s3v, dDead_s4v, dDead_s5v,
                   dDead_a1v, dDead_a2v, dDead_a3v, dDead_a4v, dDead_a5v,
                   dRmild1v, dRmild2v, dRmild3v, dRmild4v, dRmild5v,
                   dRhosp1v, dRhosp2v, dRhosp3v, dRhosp4v, dRhosp5v,
                   dRicu1v, dRicu2v, dRicu3v, dRicu4v, dRicu5v,
                   dRdead_s1v, dRdead_s2v, dRdead_s3v, dRdead_s4v, dRdead_s5v,
                   dRdead_a1v, dRdead_a2v, dRdead_a3v, dRdead_a4v, dRdead_a5v,
                   dVp1, dVp2, dVp3, dVp4, dVp5,
                   dEin1, dEin2, dEin3, dEin4, dEin5,
                   dEinv1, dEinv2, dEinv3, dEinv4, dEinv5,
                   dHosp1in, dHosp2in, dHosp3in, dHosp4in, dHosp5in,
                   dHosp1inv, dHosp2inv, dHosp3inv, dHosp4inv, dHosp5inv,
                   dICU1in, dICU2in, dICU3in, dICU4in, dICU5in,
                   dICU1inv, dICU2inv, dICU3inv, dICU4inv, dICU5inv
                   )
      
      list (results)
    }
  )
}





compute_model_base_V01 = function(Bfun, duration_time, w, param){
  
  
  parameter_list = param
  
  parameter_list$Bfun = Bfun
  
  # submodels weights 
  w1 = w[1]
  w2 = w[2]
  w3 = w[3]
  w4 = w[4]
  w5 = w[5]
  
  
  # initial values
  N = param$N                           # population
  S = (N-param$zacetno_stevilo) / N     # Susceptible population
  
  S1 = w1 * S
  S2 = w2 * S
  S3 = w3 * S
  S4 = w4 * S
  S5 = w5 * S
  
  zacetno_stevilo = param$zacetno_stevilo / 5   # Initial number of infection
  
  E1 = zacetno_stevilo/N
  E2 = zacetno_stevilo/N
  E3 = zacetno_stevilo/N
  E4 = zacetno_stevilo/N
  E5 = zacetno_stevilo/N
  
  initial_values = c (S1 = S1, S2 = S2, S3 = S3, S4 = S4, S5 = S5,
                      E1 = E1, E2 = E2, E3 = E3, E4 = E4, E5 = E5,
                      I1 = 0,  I2 = 0,  I3 = 0,  I4 = 0,  I5 = 0,
                      Mild1 = 0, Mild2 = 0, Mild3 = 0, Mild4 = 0, Mild5 = 0,
                      Hosp1 = 0, Hosp2 = 0, Hosp3 = 0, Hosp4 = 0, Hosp5 = 0,
                      ICU1 = 0, ICU2 = 0, ICU3 = 0, ICU4 = 0, ICU5 = 0,
                      Dead_s1 = 0, Dead_s2 = 0, Dead_s3 = 0, Dead_s4 = 0, Dead_s5 = 0,
                      Dead_a1 = 0, Dead_a2 = 0, Dead_a3 = 0, Dead_a4 = 0, Dead_a5 = 0,
                      Rmild1 = 0, Rmild2 = 0, Rmild3 = 0, Rmild4 = 0, Rmild5 = 0,
                      Rhosp1 = 0, Rhosp2 = 0, Rhosp3 = 0, Rhosp4 = 0, Rhosp5 = 0,
                      Ricu1 = 0, Ricu2 = 0, Ricu3 = 0, Ricu4 = 0, Ricu5 = 0,
                      Rdead_s1 = 0, Rdead_s2 = 0, Rdead_s3 = 0, Rdead_s4 = 0, Rdead_s5 = 0,
                      Rdead_a1 = 0, Rdead_a2 = 0, Rdead_a3 = 0, Rdead_a4 = 0, Rdead_a5 = 0,
                      V1 = 0, V2 = 0, V3 = 0, V4 = 0, V5 = 0,
                      Sv1 = 0, Sv2 = 0, Sv3 = 0, Sv4 = 0, Sv5 = 0,
                      Ev1 = 0, Ev2 = 0, Ev3 = 0, Ev4 = 0, Ev5 = 0,
                      Iv1 = 0, Iv2 = 0, Iv3 = 0, Iv4 = 0, Iv5 = 0,
                      Mild1v = 0, Mild2v = 0, Mild3v = 0, Mild4v = 0, Mild5v = 0,
                      Hosp1v = 0, Hosp2v = 0, Hosp3v = 0, Hosp4v = 0, Hosp5v = 0,
                      ICU1v = 0, ICU2v = 0, ICU3v = 0, ICU4v = 0, ICU5v = 0,
                      Dead_s1v = 0, Dead_s2v = 0, Dead_s3v = 0, Dead_s4v = 0, Dead_s5v = 0,
                      Dead_a1v = 0, Dead_a2v = 0, Dead_a3v = 0, Dead_a4v = 0, Dead_a5v = 0,
                      Rmild1v = 0, Rmild2v = 0, Rmild3v = 0, Rmild4v = 0, Rmild5v = 0,
                      Rhosp1v = 0, Rhosp2v = 0, Rhosp3v = 0, Rhosp4v = 0, Rhosp5v = 0,
                      Ricu1v = 0, Ricu2v = 0, Ricu3v = 0, Ricu4v = 0, Ricu5v = 0,
                      Rdead_s1v = 0, Rdead_s2v = 0, Rdead_s3v = 0, Rdead_s4v = 0, Rdead_s5v = 0,
                      Rdead_a1v = 0, Rdead_a2v = 0, Rdead_a3v = 0, Rdead_a4v = 0, Rdead_a5v = 0,
                      Vp1 = 0, Vp2 = 0, Vp3 = 0, Vp4 = 0, Vp5 = 0,
                      Ein1 = E1, Ein2 = E2, Ein3 = E3, Ein4 = E4, Ein5 = E5,
                      Einv1 = 0, Einv2 = 0, Einv3 = 0, Einv4 = 0, Einv5 = 0,
                      Hosp1in = 0, Hosp2in = 0, Hosp3in = 0, Hosp4in = 0, Hosp5in = 0,
                      Hosp1inv = 0, Hosp2inv = 0, Hosp3inv = 0, Hosp4inv = 0, Hosp5inv = 0,
                      ICU1in = 0, ICU2in = 0, ICU3in = 0, ICU4in = 0, ICU5in = 0,
                      ICU1inv = 0, ICU2inv = 0, ICU3inv = 0, ICU4inv = 0, ICU5inv = 0
                      
  )
  
  
  
  timepoints = seq (1, duration_time, by=1)
  
  
  # Simulate SEIR model
  
  out_seir = lsoda(initial_values, timepoints, seir_vacc_nonvacc_waning_5grps_V01, parameter_list)
  
  # proportion to number
  out_seir[,2:ncol(out_seir)] = round(out_seir[,2:ncol(out_seir)]*N, digits = 2)
  
  output = data.frame(out_seir)
  
  # combine vaccinated and unvaccinated by compartments
  output$S1 = output$S1 + output$Sv1
  output$S2 = output$S2 + output$Sv2
  output$S3 = output$S3 + output$Sv3
  output$S4 = output$S4 + output$Sv4
  output$S5 = output$S5 + output$Sv5
  
  output$E1 = output$E1 + output$Ev1
  output$E2 = output$E2 + output$Ev2
  output$E3 = output$E3 + output$Ev3
  output$E4 = output$E4 + output$Ev4
  output$E5 = output$E5 + output$Ev5
  
  output$I1 = output$I1 + output$Iv1
  output$I2 = output$I2 + output$Iv2
  output$I3 = output$I3 + output$Iv3
  output$I4 = output$I4 + output$Iv4
  output$I5 = output$I5 + output$Iv5  
  
  output$Mild1 = output$Mild1 + output$Mild1v 
  output$Mild2 = output$Mild2 + output$Mild2v
  output$Mild3 = output$Mild3 + output$Mild3v
  output$Mild4 = output$Mild4 + output$Mild4v
  output$Mild5 = output$Mild5 + output$Mild5v
  
  output$Hosp1 = output$Hosp1 + output$Hosp1v
  output$Hosp2 = output$Hosp2 + output$Hosp2v
  output$Hosp3 = output$Hosp3 + output$Hosp3v
  output$Hosp4 = output$Hosp4 + output$Hosp4v
  output$Hosp5 = output$Hosp5 + output$Hosp5v
  
  output$ICU1 = output$ICU1 + output$ICU1v
  output$ICU2 = output$ICU2 + output$ICU2v
  output$ICU3 = output$ICU3 + output$ICU3v
  output$ICU4 = output$ICU4 + output$ICU4v
  output$ICU5 = output$ICU5 + output$ICU5v
  
  output$Dead_s1 = output$Dead_s1 + output$Dead_s1v 
  output$Dead_s2 = output$Dead_s2 + output$Dead_s2v
  output$Dead_s3 = output$Dead_s3 + output$Dead_s3v
  output$Dead_s4 = output$Dead_s4 + output$Dead_s4v
  output$Dead_s5 = output$Dead_s5 + output$Dead_s5v
  
  
  output$Dead_a1 = output$Dead_a1 + output$Dead_a1v
  output$Dead_a2 = output$Dead_a2 + output$Dead_a2v
  output$Dead_a3 = output$Dead_a3 + output$Dead_a3v
  output$Dead_a4 = output$Dead_a4 + output$Dead_a4v
  output$Dead_a5 = output$Dead_a5 + output$Dead_a5v
  
  output$Rmild1 = output$Rmild1 + output$Rmild1v
  output$Rmild2 = output$Rmild2 + output$Rmild2v
  output$Rmild3 = output$Rmild3 + output$Rmild3v
  output$Rmild4 = output$Rmild4 + output$Rmild4v
  output$Rmild5 = output$Rmild5 + output$Rmild5v
  
  output$Rhosp1 = output$Rhosp1 + output$Rhosp1v
  output$Rhosp2 = output$Rhosp2 + output$Rhosp2v
  output$Rhosp3 = output$Rhosp3 + output$Rhosp3v
  output$Rhosp4 = output$Rhosp4 + output$Rhosp4v
  output$Rhosp5 = output$Rhosp5 + output$Rhosp5v
  
  
  output$Ricu1 = output$Ricu1 + output$Ricu1v
  output$Ricu2 = output$Ricu2 + output$Ricu2v
  output$Ricu3 = output$Ricu3 + output$Ricu3v
  output$Ricu4 = output$Ricu4 + output$Ricu4v
  output$Ricu5 = output$Ricu5 + output$Ricu5v
  
  
  output$Rdead_s1 = output$Rdead_s1 + output$Rdead_s1v
  output$Rdead_s2 = output$Rdead_s2 + output$Rdead_s2v
  output$Rdead_s3 = output$Rdead_s3 + output$Rdead_s3v
  output$Rdead_s4 = output$Rdead_s4 + output$Rdead_s4v
  output$Rdead_s5 = output$Rdead_s5 + output$Rdead_s5v
  
  output$Rdead_a1 = output$Rdead_a1 + output$Rdead_a1v
  output$Rdead_a2 = output$Rdead_a2 + output$Rdead_a2v
  output$Rdead_a3 = output$Rdead_a3 + output$Rdead_a3v
  output$Rdead_a4 = output$Rdead_a4 + output$Rdead_a4v
  output$Rdead_a5 = output$Rdead_a5 + output$Rdead_a5v
  
  
  
  
  
  ### join together: deaths
  
  output$Rdead_daily1 = c(0,diff(output$Rdead_s1+output$Rdead_a1))
  output$Rdead_daily2 = c(0,diff(output$Rdead_s2+output$Rdead_a2))
  output$Rdead_daily3 = c(0,diff(output$Rdead_s3+output$Rdead_a3))
  output$Rdead_daily4 = c(0,diff(output$Rdead_s4+output$Rdead_a4))
  output$Rdead_daily5 = c(0,diff(output$Rdead_s5+output$Rdead_a5))
  
  
  ### join together vacc+unvacc: infected
  
  output$Ein1 = c(0, diff(output$Ein1 + output$Einv1))
  output$Ein2 = c(0, diff(output$Ein2 + output$Einv2))
  output$Ein3 = c(0, diff(output$Ein3 + output$Einv3))
  output$Ein4 = c(0, diff(output$Ein4 + output$Einv4))
  output$Ein5 = c(0, diff(output$Ein5 + output$Einv5))
  
  
  # correct infected: seen/unseen 
  output$Ein1 = parameter_list[["w_inf_daily_t"]] * output$Ein1
  output$Ein2 = parameter_list[["w_inf_daily_t"]] * output$Ein2
  output$Ein3 = parameter_list[["w_inf_daily_t"]] * output$Ein3
  output$Ein4 = parameter_list[["w_inf_daily_t"]] * output$Ein4
  output$Ein5 = parameter_list[["w_inf_daily_t"]] * output$Ein5
  
  
  
  # join together vacc+unvacc: Hosp in
  output$Hosp1in = c(0, diff(output$Hosp1in + output$Hosp1inv))
  output$Hosp2in = c(0, diff(output$Hosp2in + output$Hosp2inv))
  output$Hosp3in = c(0, diff(output$Hosp3in + output$Hosp3inv))
  output$Hosp4in = c(0, diff(output$Hosp4in + output$Hosp4inv))
  output$Hosp5in = c(0, diff(output$Hosp5in + output$Hosp5inv))
  
  
  # join together vacc+unvacc: ICU in
  output$ICU1in = c(0, diff(output$ICU1in + output$ICU1inv))
  output$ICU2in = c(0, diff(output$ICU2in + output$ICU2inv))
  output$ICU3in = c(0, diff(output$ICU3in + output$ICU3inv))
  output$ICU4in = c(0, diff(output$ICU4in + output$ICU4inv))
  output$ICU5in = c(0, diff(output$ICU5in + output$ICU5inv))
  
  
  
  
  
  return(output)
  
}





##### Compute model #######

compute_model_V01 = function(Bfun, duration_time, w, param) {
  
  
  output = compute_model_base_V01(Bfun, duration_time, w, param)
  
  
  
  ##############################
  ### shift: hosp (shift left by 6 days)
  ##############################
  
  org_st = output$Hosp1
  premk_st = output$Hosp1
  for (j in 1:length(org_st)) {
    if (j>param$premik_hosp)  #shifting parameters
      premk_st[j-param$premik_hosp] = org_st[j]
  }
  output$Hosp1 = premk_st
  
  org_st = output$Hosp2
  premk_st = output$Hosp2
  for (j in 1:length(org_st)) {
    if (j>param$premik_hosp)
      premk_st[j-param$premik_hosp] = org_st[j]
  }
  output$Hosp2 = premk_st
  
  org_st = output$Hosp3
  premk_st = output$Hosp3
  for (j in 1:length(org_st)) {
    if (j>param$premik_hosp)
      premk_st[j-param$premik_hosp] = org_st[j]
  }
  output$Hosp3 = premk_st
  
  
  org_st = output$Hosp4
  premk_st = output$Hosp4
  for (j in 1:length(org_st)) {
    if (j>param$premik_hosp)
      premk_st[j-param$premik_hosp] = org_st[j]
  }
  output$Hosp4 = premk_st
  
  
  org_st = output$Hosp5
  premk_st = output$Hosp5
  for (j in 1:length(org_st)) {
    if (j>param$premik_hosp)
      premk_st[j-param$premik_hosp] = org_st[j]
  }
  output$Hosp5 = premk_st
  
  
  
  ##############################
  ### shift: hospIN (shift left by 6 days)
  ##############################
  
  org_st = output$Hosp1in
  premk_st = output$Hosp1in
  for (j in 1:length(org_st)) {
    if (j>param$premik_hosp)
      premk_st[j-param$premik_hosp] = org_st[j]
  }
  output$Hosp1in = premk_st
  
  
  org_st = output$Hosp2in
  premk_st = output$Hosp2in
  for (j in 1:length(org_st)) {
    if (j>param$premik_hosp)
      premk_st[j-param$premik_hosp] = org_st[j]
  }
  output$Hosp2in = premk_st
  
  org_st = output$Hosp3in
  premk_st = output$Hosp3in
  for (j in 1:length(org_st)) {
    if (j>param$premik_hosp)
      premk_st[j-param$premik_hosp] = org_st[j]
  }
  output$Hosp3in = premk_st
  
  
  org_st = output$Hosp4in
  premk_st = output$Hosp4in
  for (j in 1:length(org_st)) {
    if (j>param$premik_hosp)
      premk_st[j-param$premik_hosp] = org_st[j]
  }
  output$Hosp4in = premk_st
  
  
  org_st = output$Hosp5in
  premk_st = output$Hosp5in
  for (j in 1:length(org_st)) {
    if (j>param$premik_hosp)
      premk_st[j-param$premik_hosp] = org_st[j]
  }
  output$Hosp5in = premk_st
  
  
  
  ##############################
  ### shift: icu (shift left by 5 days)
  ##############################
  
  org_st = output$ICU1
  premk_st = output$ICU1
  for (j in 1:length(org_st)) {
    if (j>param$premik_icu)
      premk_st[j-param$premik_icu] = org_st[j]
  }
  output$ICU1 = premk_st
  
  org_st = output$ICU2
  premk_st = output$ICU2
  for (j in 1:length(org_st)) {
    if (j>param$premik_icu)
      premk_st[j-param$premik_icu] = org_st[j]
  }
  output$ICU2 = premk_st
  
  
  org_st = output$ICU3
  premk_st = output$ICU3
  for (j in 1:length(org_st)) {
    if (j>param$premik_icu)
      premk_st[j-param$premik_icu] = org_st[j]
  }
  output$ICU3 = premk_st
  
  org_st = output$ICU4
  premk_st = output$ICU4
  for (j in 1:length(org_st)) {
    if (j>param$premik_icu)
      premk_st[j-param$premik_icu] = org_st[j]
  }
  output$ICU4 = premk_st
  
  org_st = output$ICU5
  premk_st = output$ICU5
  for (j in 1:length(org_st)) {
    if (j>param$premik_icu)
      premk_st[j-param$premik_icu] = org_st[j]
  }
  output$ICU5 = premk_st
  
  
  ##############################
  ### shift: icu IN  (shift left by 5 days)
  ##############################
  
  org_st = output$ICU1in
  premk_st = output$ICU1in
  for (j in 1:length(org_st)) {
    if (j>param$premik_icu)
      premk_st[j-param$premik_icu] = org_st[j]
  }
  output$ICU1in = premk_st
  
  
  org_st = output$ICU2in
  premk_st = output$ICU2in
  for (j in 1:length(org_st)) {
    if (j>param$premik_icu)
      premk_st[j-param$premik_icu] = org_st[j]
  }
  output$ICU2in = premk_st
  
  
  org_st = output$ICU3in
  premk_st = output$ICU3in
  for (j in 1:length(org_st)) {
    if (j>param$premik_icu)
      premk_st[j-param$premik_icu] = org_st[j]
  }
  output$ICU3in = premk_st
  
  org_st = output$ICU4in
  premk_st = output$ICU4in
  for (j in 1:length(org_st)) {
    if (j>param$premik_icu)
      premk_st[j-param$premik_icu] = org_st[j]
  }
  output$ICU4in = premk_st
  
  org_st = output$ICU5in
  premk_st = output$ICU5in
  for (j in 1:length(org_st)) {
    if (j>param$premik_icu)
      premk_st[j-param$premik_icu] = org_st[j]
  }
  output$ICU5in = premk_st
  
  
  ###################################
  
  
  pdat = data.frame(groups=c(rep("infected: group 1", nrow(output)),
                              rep("hospitalization 1", nrow(output)),
                              rep("ICU 1", nrow(output)),
                              rep("death: cumulative 1", nrow(output)),
                              rep("death: daily 1", nrow(output)),
                              rep("vaccinated 1", nrow(output)),
                              rep("hospIN 1", nrow(output)),
                              rep("ICUIN 1", nrow(output)),
                              
                              rep("infected: group 2", nrow(output)),
                              rep("hospitalization 2", nrow(output)),
                              rep("ICU 2", nrow(output)),
                              rep("death: cumulative 2", nrow(output)),
                              rep("death: daily 2", nrow(output)),
                              rep("vaccinated 2", nrow(output)),
                              rep("hospIN 2", nrow(output)),
                              rep("ICUIN 2", nrow(output)),
                              
                              rep("infected: group 3", nrow(output)),
                              rep("hospitalization 3", nrow(output)),
                              rep("ICU 3", nrow(output)),
                              rep("death: cumulative 3", nrow(output)),
                              rep("death: daily 3", nrow(output)),
                              rep("vaccinated 3", nrow(output)),
                              rep("hospIN 3", nrow(output)),
                              rep("ICUIN 3", nrow(output)),
                              
                              rep("infected: group 4", nrow(output)),
                              rep("hospitalization 4", nrow(output)),
                              rep("ICU 4", nrow(output)),
                              rep("death: cumulative 4", nrow(output)),
                              rep("death: daily 4", nrow(output)),
                              rep("vaccinated 4", nrow(output)),
                              rep("hospIN 4", nrow(output)),
                              rep("ICUIN 4", nrow(output)),
                              
                              
                              rep("infected: group 5", nrow(output)),
                              rep("hospitalization 5", nrow(output)),
                              rep("ICU 5", nrow(output)),
                              rep("death: cumulative 5", nrow(output)),
                              rep("death: daily 5", nrow(output)),
                              rep("vaccinated 5", nrow(output)),
                              rep("hospIN 5", nrow(output)),
                              rep("ICUIN 5", nrow(output))
                              
                              
  ),
  "number"=c(output$Ein1,
              output$Hosp1,
              output$ICU1,
              output$Rdead_s1 + output$Rdead_a1,
              output$Rdead_daily1,
              output$Vp1,
              output$Hosp1in,
              output$ICU1in,
              
              output$Ein2,
              output$Hosp2,
              output$ICU2,
              output$Rdead_s2 + output$Rdead_a2,
              output$Rdead_daily2,
              output$Vp2,
              output$Hosp2in,
              output$ICU2in,
              
              output$Ein3,
              output$Hosp3,
              output$ICU3,
              output$Rdead_s3 + output$Rdead_a3,
              output$Rdead_daily3,
              output$Vp3,
              output$Hosp3in,
              output$ICU3in,
              
              output$Ein4,
              output$Hosp4,
              output$ICU4,
              output$Rdead_s4 + output$Rdead_a4,
              output$Rdead_daily4,
              output$Vp4,
              output$Hosp4in,
              output$ICU4in,
              
              output$Ein5,
              output$Hosp5,
              output$ICU5,
              output$Rdead_s5 + output$Rdead_a5,
              output$Rdead_daily5,
              output$Vp5,
              output$Hosp5in,
              output$ICU5in
              
              
  ),
  days = c(output$time,
            output$time,
            output$time,
            output$time,
            output$time,
            output$time,
            output$time,
            output$time,
            
            output$time,
            output$time,
            output$time,
            output$time,
            output$time,
            output$time,
            output$time,
            output$time,
            
            output$time,
            output$time,
            output$time,
            output$time,
            output$time,
            output$time,
            output$time,
            output$time,
            
            output$time,
            output$time,
            output$time,
            output$time,
            output$time,
            output$time,
            output$time,
            output$time,
            
            output$time,
            output$time,
            output$time,
            output$time,
            output$time,
            output$time,
            output$time,
            output$time
  )
  )
  
  
  
  return(pdat)
  
}









#########################################################
#### Joining model projections                      ##### 
#########################################################


joining_model_projections = function(pdat) 
{
    odat = pdat[pdat$groups == "infected: group 1",]
    odat$number = pdat[pdat$groups == "infected: group 1","number"] + 
      pdat[pdat$groups == "infected: group 2","number"] + 
      pdat[pdat$groups == "infected: group 3","number"] + 
      pdat[pdat$groups == "infected: group 4","number"] + 
      pdat[pdat$groups == "infected: group 5","number"]
    
    odat$groups = "infected: group"
    
    
    hdat = pdat[pdat$groups == "hospitalization 1",]
    hdat$number = pdat[pdat$groups == "hospitalization 1","number"] + 
      pdat[pdat$groups == "hospitalization 2","number"] + 
      pdat[pdat$groups == "hospitalization 3","number"] + 
      pdat[pdat$groups == "hospitalization 4","number"] + 
      pdat[pdat$groups == "hospitalization 5","number"]
    
    hdat$groups = "hospitalization"
    
    hindat = pdat[pdat$groups == "hospIN 1",]
    hindat$number = pdat[pdat$groups == "hospIN 1","number"] + 
      pdat[pdat$groups == "hospIN 2","number"] + 
      pdat[pdat$groups == "hospIN 3","number"] + 
      pdat[pdat$groups == "hospIN 4","number"] + 
      pdat[pdat$groups == "hospIN 5","number"]
    
    hindat$groups = "hosp in"
    
    
    
    idat = pdat[pdat$groups == "ICU 1",]
    idat$number = pdat[pdat$groups == "ICU 1","number"] + 
      pdat[pdat$groups == "ICU 2","number"] + 
      pdat[pdat$groups == "ICU 3","number"] + 
      pdat[pdat$groups == "ICU 4","number"] + 
      pdat[pdat$groups == "ICU 5","number"]
    
    idat$groups = "icu"
    
    
    iindat = pdat[pdat$groups == "ICUIN 1",]
    iindat$number = pdat[pdat$groups == "ICUIN 1","number"] + 
      pdat[pdat$groups == "ICUIN 2","number"] + 
      pdat[pdat$groups == "ICUIN 3","number"] + 
      pdat[pdat$groups == "ICUIN 4","number"] + 
      pdat[pdat$groups == "ICUIN 5","number"]
    
    iindat$groups = "icu in"
    
    
    
    
    
    dudat = pdat[pdat$groups == "death: daily 1",]
    dudat$number = pdat[pdat$groups == "death: daily 1","number"] + 
      pdat[pdat$groups == "death: daily 2","number"] + 
      pdat[pdat$groups == "death: daily 3","number"] + 
      pdat[pdat$groups == "death: daily 4","number"] + 
      pdat[pdat$groups == "death: daily 5","number"]
    
    dudat$groups = "death: daily"
    
    
    kudat = pdat[pdat$groups == "death: cumulative 1",]
    kudat$number = pdat[pdat$groups == "death: cumulative 1","number"] + 
      pdat[pdat$groups == "death: cumulative 2","number"] + 
      pdat[pdat$groups == "death: cumulative 3","number"] + 
      pdat[pdat$groups == "death: cumulative 4","number"] + 
      pdat[pdat$groups == "death: cumulative 5","number"]
    
    kudat$groups = "death: cumulative"


    ## Vaccination data
    cpdat = pdat[pdat$groups == "vaccinated 1",]
    cpdat$number = pdat[pdat$groups == "vaccinated 1","number"] + 
      pdat[pdat$groups == "vaccinated 2","number"] + 
      pdat[pdat$groups == "vaccinated 3","number"] + 
      pdat[pdat$groups == "vaccinated 4","number"] + 
      pdat[pdat$groups == "vaccinated 5","number"]
    
    cpdat$groups = "vaccinated"
    
    
    spdat = rbind(odat, hdat, idat, dudat, kudat, hindat, iindat, cpdat)
    
    
    

    return(spdat)
}


############################
#### aux functions      ####
############################


### Bt function #####
# daily vaccination rate

Bt_rect_time = function(end_time, Bw, win_len)
{
  if (length(Bw) != length(win_len))
    win_len = rep(10, length(Bw))  #every 10 day vax rate updated
  
  Bfun = rep(0, end_time)
  ct = 1
  for(i in 1:length(Bw)) {
    
    Bfun[ct:(ct + win_len[i]-1)] = rep(Bw[i], win_len[i])
    ct = ct + win_len[i]
  }
  
  if (ct < end_time)
    Bfun[ct:end_time] = rep(Bw[length(Bw)], end_time-ct+1)
  else
    Bfun = Bfun[1:end_time]
  
  return(Bfun)
  
}



### shifting  ####

shift <- function(x, lag) {
  n <- length(x)
  xnew <- rep(NA, n)
  if (lag < 0) {
    xnew[1:(n-abs(lag))] <- x[(abs(lag)+1):n]
  } else if (lag > 0) {
    xnew[(lag+1):n] <- x[1:(n-lag)]
  } else {
    xnew <- x
  }
  return(xnew)
}


### smoothing function: averaging  ####
ma <- function(x, n = 5, side = 2)
{
  ts = stats::filter(x, rep(1 / n, n), sides = side)
  as.numeric(ts)
}
