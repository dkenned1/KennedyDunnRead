 



rproc <- Csnippet("
	//DEFINE VARIABLES
	double rate[6], trans[10];
	
	int VirusTransfer; 
	
	double c_eff;
	double f_eff;
		
	if(nearbyint(BirdNumber)==0)			//If birds are absent...
	{
		S = 0;
		E1 = 0;
		E2 = 0;
		E3 = 0;
		E4 = 0;
		E5 = 0;
		I = 0;
		V += 0;
		D += 0;
	}
	
	{
		if(nearbyint(Placement)==1)			//If this is a placement date...
		{
			S=BirdNumber;
			E1 = 0;
			E2 = 0;
			E3 = 0;
			E4 = 0;
			E5 = 0;

			I = 0;

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

			if (nearbyint(Model3)==1)
			{
				c_eff=rbeta(c_alpha, c_beta);
			}else
			{
				c_eff=c_alpha;
			}

			if (nearbyint(Model4)==1)
			{
				f_eff=rbeta(f_alpha, f_beta);
			}else
			{
				f_eff=f_alpha;
			}

			V = V*(1-c_eff)*(1-f_eff);
			D = D*(1-c_eff);
		}
		else if (nearbyint(BirdNumber)>1)
		{


			// POISSON NEW EXPOSURES
			rate[0] = alpha_eff*V;

			// BINOMIAL NEW INFECTIONS
			rate[1] = Beta;

			// NEW INFECTIOUS VIRUS
			rate[2] = a_eff*BirdShed;
			
			if(rate[2]>1e9)	//Code issues if rate[2]>=1e11
			{
				rate[2]=1e9;
			}			
			//Rprintf(\"%lg\\n\",rate[2]);

			//NEW DUST
			rate[3] = BirdShed;

			// VIRUS DEGRADATION AND VENTILATION
			rate[4] = gamma + delta;

			// DUST VENTILATION
			rate[5] = gamma;

			//VIRUS TRANSFER
			VirusTransfer = rbinom(1, 1-exp(-M_rate*dt));

			reulermultinom(1,  S, &rate[0], dt, &trans[0]);
			reulermultinom(1, E1, &rate[1], dt, &trans[1]);
			reulermultinom(1, E2, &rate[1], dt, &trans[2]);
			reulermultinom(1, E3, &rate[1], dt, &trans[3]);
			reulermultinom(1, E4, &rate[1], dt, &trans[4]);
			reulermultinom(1, E5, &rate[1], dt, &trans[5]);


			if (nearbyint(Model5)==1)
			{
				trans[6]=VirusTransfer*M_mu + rate[2]*I*dt;
			}else
			{
				trans[6]=rate[2]*I*dt;
			}

			trans[7]=rate[3]*(BirdNumber)*dt;
			trans[8]=rate[4]*V*dt;
			trans[9]=rate[5]*D*dt;


			S -= trans[0];
			E1 += trans[0] - trans[1];
			E2 += trans[1] - trans[2];
			E3 += trans[2] - trans[3];
			E4 += trans[3] - trans[4];
			E5 += trans[4] - trans[5];
			I += trans[5];

			V += trans[6] - trans[8];
			D += trans[7] - trans[9];				

			alpha_eff=alpha_eff;
			a_eff=a_eff;
		}
	}
")

