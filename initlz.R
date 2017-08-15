



initlz <- Csnippet("
	
	S=S_0;
	E1=0;
	E2=0;
	E3=0;
	E4=0;
	E5=0;
	I=0;
	
	V=V_0;
	D=D_0;
	
	if (nearbyint(Model1)==1)
	{
		alpha_eff=alpha*rlnorm(-sigma_alpha*sigma_alpha/2.0, sigma_alpha);
	}else
	{
		alpha_eff=alpha;
	}

	if (nearbyint(Model2)==1)
	{
		a_eff=a*rlnorm(-sigma_a*sigma_a/2.0, sigma_a);
	}else
	{
		a_eff=a;
	}
")

