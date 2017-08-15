
rmeas <- Csnippet("

	double DetectionLimit=2;
	double lVirusConc = log10(V/D +1);
	
	double probit_beta_0= -2.206;
	double probit_beta_1= 1.555;
	
	double vcn_beta_0=1.127;
	double vcn_beta_1= -0.151;
		
	double ProbitValue=probit_beta_0 + probit_beta_1*lVirusConc;
	double NormValue=vcn_beta_0 + vcn_beta_1 *lVirusConc;

	Ndata=3;
	int Continue;
	double TempVCN;
	
	if (rbinom(1,  pnorm(ProbitValue, 0,1, 0,0)))
	{
		VCN1=0;
	}
	else 
	{
		Continue=1;
		while (Continue)
		{
			TempVCN=rnorm(lVirusConc, NormValue);
			if (TempVCN>DetectionLimit)
			{
				VCN1=pow(10, TempVCN);
				Continue=0;
			}
		}
	}
		
	if(Ndata>1)
	{
		if (rbinom(1,  pnorm(ProbitValue, 0,1, 0,0)))
		{
			VCN2=0;
		}
		else 
		{
			Continue=1;
			while (Continue)
			{
				TempVCN=rnorm(lVirusConc, NormValue);
				if (TempVCN>DetectionLimit)
				{
					VCN2=pow(10, TempVCN);
					Continue=0;
				}
			}
		}

	
		if(Ndata>2)
		{
			if (rbinom(1,  pnorm(ProbitValue, 0,1, 0,0)))
			{
				VCN3=0;
			}
			else 
			{
				Continue=1;
				while (Continue)
				{
					TempVCN=rnorm(lVirusConc, NormValue);
					if (TempVCN>DetectionLimit)
					{
						VCN3=pow(10, TempVCN);
						Continue=0;
					}
				}
			}
			
			if(Ndata>3)
			{
				if (rbinom(1,  pnorm(ProbitValue, 0,1, 0,0)))
				{
					VCN4=0;
				}
				else 
				{
					Continue=1;
					while (Continue)
					{
						TempVCN=rnorm(lVirusConc, NormValue);
						if (TempVCN>DetectionLimit)
						{
							VCN4=pow(10, TempVCN);
							Continue=0;
						}
					}
				}
				
				if(Ndata>4)
				{
					if (rbinom(1,  pnorm(ProbitValue, 0,1, 0,0)))
					{
						VCN5=0;
					}
					else 
					{
						Continue=1;
						while (Continue)
						{
							TempVCN=rnorm(lVirusConc, NormValue);
							if (TempVCN>DetectionLimit)
							{
								VCN5=pow(10, TempVCN);
								Continue=0;
							}
						}
					}
				
					if(Ndata>5)
					{
						if (rbinom(1,  pnorm(ProbitValue, 0,1, 0,0)))
						{
							VCN6=0;
						}
						else 
						{
							Continue=1;
							while (Continue)
							{
								TempVCN=rnorm(lVirusConc, NormValue);
								if (TempVCN>DetectionLimit)
								{
									VCN6=pow(10, TempVCN);
									Continue=0;
								}
							}
						}
					}
				}
			}
		}
	}
")	



