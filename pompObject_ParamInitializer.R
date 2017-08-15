





statenames <- c("S", "E1", "E2", "E3", "E4", "E5", "I", "V", "D", "alpha_eff", "a_eff")
fp_names <- c("alpha", "Beta", "S_0", "D_0", "DataSet", "Model1", "Model2", "Model3", "Model4", "Model5")
rp_names <- c("sigma_alpha", "a", "sigma_a", "gamma", "delta", "c_alpha", "c_beta", "f_alpha", "f_beta", "M_mu", "M_rate")
ivp_names <- c("V_0")


##Transmission rate is 8.26e-10 per m^3.  Both houses are approximately 5000 cubic meters.
alpha=8.26e-10/HouseVolume
D_0= 2000 * HouseVolume
Beta=0.4


fixed_params <- c(alpha= alpha, Beta=Beta, S_0=S_0, D_0=D_0, DataSet=DataSet, Model1=Model1, Model2=Model2, Model3=Model3, Model4=Model4, Model5=Model5)



source(paste(SourcePath, "ParamInitializer.R", sep=""))

         

MDV_sir <- pomp(dat, time="Day", t0=t0, rprocess=euler.sim(rproc, delta.t=delta.t), rmeasure=rmeas, dmeasure=dmeas,
	initializer=initlz, 
	paramnames=c(rp_names, ivp_names, fp_names), statenames=statenames,
	params=params,	
	toEstimationScale=toEst, fromEstimationScale=fromEst,
	covar=CovarTable,
  	tcovar="Time"
  ) 

