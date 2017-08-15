


dmeas <- Csnippet("
	
	double tol=1.0e-17;
	double DetectionLimit=2;
	
	double probit_beta_0= -2.206;
	double probit_beta_1= 1.555;

	double vcn_beta_0= 1.127;
	double vcn_beta_1= -0.151;

	
	double lVirusConc = log10(V/D +1);
	
	double ProbitValue=probit_beta_0 + probit_beta_1*lVirusConc;
	double dNormValue=vcn_beta_0 + vcn_beta_1 *lVirusConc;

	
	if (log10(VCN1+1)<DetectionLimit)
	{
		lik = pnorm(ProbitValue,0,1,0,1);		//prob that virus is undetectable	//last argument is log=TRUE, second to last is lower.tail=FALSE
	}
	else
	{
		lik = pnorm(ProbitValue,0,1,1,1);			//prob that virus is detectable
		lik += dnorm(log10(VCN1+1), lVirusConc, dNormValue, 1);	//prob that virus is exactly equal to data
	}
	
	if (Ndata>1)
	{
		if (log10(VCN2+1)<DetectionLimit)
		{
			lik += pnorm(ProbitValue,0,1,0,1);
		}
		else
		{
			lik += pnorm(ProbitValue,0,1,1,1);
			lik += dnorm(log10(VCN2+1), lVirusConc, dNormValue, 1);
		}
		
		if (Ndata>2)
		{
			if (log10(VCN3+1)<DetectionLimit)
			{
				lik += pnorm(ProbitValue,0,1,0,1);
			}
			else
			{
				lik += pnorm(ProbitValue,0,1,1,1);
				lik += dnorm(log10(VCN3+1), lVirusConc, dNormValue, 1);
			}
			
			if (Ndata>3)
			{
				if (log10(VCN4+1)<DetectionLimit)
				{
					lik += pnorm(ProbitValue,0,1,0,1);
				}
				else
				{
					lik += pnorm(ProbitValue,0,1,1,1);
					lik += dnorm(log10(VCN4+1), lVirusConc, dNormValue, 1);
				}
				
				if (Ndata>4)
				{
					if (log10(VCN5+1)<DetectionLimit)
					{
						lik += pnorm(ProbitValue,0,1,0,1);
					}
					else
					{
						lik += pnorm(ProbitValue,0,1,1,1);
						lik += dnorm(log10(VCN5+1), lVirusConc, dNormValue, 1);
					}
					
					if (Ndata>5)
					{
						if (log10(VCN6+1)<DetectionLimit)
						{
							lik += pnorm(ProbitValue,0,1,0,1);
						}
						else
						{
							lik += pnorm(ProbitValue,0,1,1,1);
							lik += dnorm(log10(VCN6+1), lVirusConc, dNormValue, 1);
						}
					}
				}
			}
		}
	}
	
	if(isnan(lik))
	{
                //Rprintf(\"%lg\\n\",lik);	
		lik=-300;
     	}
	
	lik=exp(lik) + tol;
")
