

if(Model3==0 && Model4==0)
{
	toEst <- Csnippet("
		Talpha = log(alpha);
		Tsigma_alpha=log(sigma_alpha);
		Ta = log(a);
		Tsigma_a=log(sigma_a);
		Tgamma = log(gamma);
		Tdelta = log(delta);
		Tc_alpha = logit(c_alpha);
		Tc_beta = log(c_beta);
		Tf_alpha = logit(f_alpha);
		Tf_beta = log(f_beta);
		TM_mu = log(M_mu);
		TM_rate = log(M_rate);
		TV_0 = log(V_0);
		TD_0 = log(D_0);

	")

	fromEst <- Csnippet("
		Talpha = exp(alpha);
		Tsigma_alpha=exp(sigma_alpha);
		Ta = exp(a);
		Tsigma_a=exp(sigma_a);
		Tgamma = exp(gamma);
		Tdelta = exp(delta);
		Tc_alpha = expit(c_alpha);
		Tc_beta = exp(c_beta);
		Tf_alpha = expit(f_alpha);
		Tf_beta = exp(f_beta);
		TM_mu = exp(M_mu);
		TM_rate = exp(M_rate);
		TV_0 = exp(V_0);
		TD_0 = exp(D_0);
	")
}



if(Model3==0 && Model4==1)
{
	toEst <- Csnippet("
		Talpha = log(alpha);
		Tsigma_alpha=log(sigma_alpha);
		Ta = log(a);
		Tsigma_a=log(sigma_a);
		Tgamma = log(gamma);
		Tdelta = log(delta);
		Tc_alpha = logit(c_alpha);
		Tc_beta = log(c_beta);
		Tf_alpha = log(f_alpha);
		Tf_beta = log(f_beta);
		TM_mu = log(M_mu);
		TM_rate = log(M_rate);
		TV_0 = log(V_0);
		TD_0 = log(D_0);

	")

	fromEst <- Csnippet("
		Talpha = exp(alpha);
		Tsigma_alpha=exp(sigma_alpha);
		Ta = exp(a);
		Tsigma_a=exp(sigma_a);
		Tgamma = exp(gamma);
		Tdelta = exp(delta);
		Tc_alpha = expit(c_alpha);
		Tc_beta = exp(c_beta);
		Tf_alpha = exp(f_alpha);
		Tf_beta = exp(f_beta);
		TM_mu = exp(M_mu);
		TM_rate = exp(M_rate);
		TV_0 = exp(V_0);
		TD_0 = exp(D_0);
	")
}

if(Model3==1 && Model4==0)
{
	toEst <- Csnippet("
		Talpha = log(alpha);
		Tsigma_alpha=log(sigma_alpha);
		Ta = log(a);
		Tsigma_a=log(sigma_a);
		Tgamma = log(gamma);
		Tdelta = log(delta);
		Tc_alpha = log(c_alpha);
		Tc_beta = log(c_beta);
		Tf_alpha = logit(f_alpha);
		Tf_beta = log(f_beta);
		TM_mu = log(M_mu);
		TM_rate = log(M_rate);
		TV_0 = log(V_0);
		TD_0 = log(D_0);

	")

	fromEst <- Csnippet("
		Talpha = exp(alpha);
		Tsigma_alpha=exp(sigma_alpha);
		Ta = exp(a);
		Tsigma_a=exp(sigma_a);
		Tgamma = exp(gamma);
		Tdelta = exp(delta);
		Tc_alpha = exp(c_alpha);
		Tc_beta = exp(c_beta);
		Tf_alpha = expit(f_alpha);
		Tf_beta = exp(f_beta);
		TM_mu = exp(M_mu);
		TM_rate = exp(M_rate);
		TV_0 = exp(V_0);
		TD_0 = exp(D_0);
	")
}

if(Model3==1 && Model4==1)
{
	toEst <- Csnippet("
		Talpha = log(alpha);
		Tsigma_alpha=log(sigma_alpha);
		Ta = log(a);
		Tsigma_a=log(sigma_a);
		Tgamma = log(gamma);
		Tdelta = log(delta);
		Tc_alpha = log(c_alpha);
		Tc_beta = log(c_beta);
		Tf_alpha = log(f_alpha);
		Tf_beta = log(f_beta);
		TM_mu = log(M_mu);
		TM_rate = log(M_rate);
		TV_0 = log(V_0);
		TD_0 = log(D_0);

	")

	fromEst <- Csnippet("
		Talpha = exp(alpha);
		Tsigma_alpha=exp(sigma_alpha);
		Ta = exp(a);
		Tsigma_a=exp(sigma_a);
		Tgamma = exp(gamma);
		Tdelta = exp(delta);
		Tc_alpha = exp(c_alpha);
		Tc_beta = exp(c_beta);
		Tf_alpha = exp(f_alpha);
		Tf_beta = exp(f_beta);
		TM_mu = exp(M_mu);
		TM_rate = exp(M_rate);
		TV_0 = exp(V_0);
		TD_0 = exp(D_0);
	")
}

