





statenames <- c("S", "E1", "E2", "E3", "E4", "E5", "I", "V", "D", "alpha_eff", "a_eff")
fp_names <- c("alpha", "Beta", "S_0", "D_0", "DataSet", "Model1", "Model2", "Model3", "Model4", "Model5")
rp_names <- c("sigma_alpha", "a", "sigma_a", "gamma", "delta", "c_alpha", "c_beta", "f_alpha", "f_beta", "M_mu", "M_rate")
ivp_names <- c("V_0")


##Transmission rate is 8.26e-10 per m^3.  Both houses are approximately 5000 cubic meters.
alpha=8.26e-10/HouseVolume
D_0= 2000 * HouseVolume
Beta=0.4

fixed_params <- c(alpha= alpha, Beta=Beta, S_0=S_0, D_0=D_0, DataSet=DataSet, Model1=Model1, Model2=Model2, Model3=Model3, Model4=Model4, Model5=Model5)



#params <- c(a=179182.9, gamma=0.04457654, delta=0.02746199, c_alpha=0.5204119, c_beta=0.3150371, f_alpha=4.542733, f_beta=2.537318, M_mu=1e9, M_rate=1, V_0= 4764373928, fixed_params)
params <- c(sigma_alpha= 0.9028420, a=230357.1, sigma_a=0.256687919, gamma=0.21767414, delta=0.04795614, c_alpha=0.98553161, c_beta=0.85587484, f_alpha=0.28826892, f_beta=0.11939665, M_mu=174469637, M_rate=0.10675372, V_0= 69411319490, fixed_params)          
         

MDV_sir <- pomp(dat, time="Day", t0=t0, rprocess=euler.sim(rproc, delta.t=delta.t), rmeasure=rmeas, dmeasure=dmeas,
	initializer=initlz, 
	paramnames=c(rp_names, ivp_names, fp_names), statenames=statenames,
	params=params,	
	toEstimationScale=toEst, fromEstimationScale=fromEst,
	covar=CovarTable,
  	tcovar="Time"
  ) 

