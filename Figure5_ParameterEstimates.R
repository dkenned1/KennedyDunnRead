

library(pomp)
library(plyr)
library(reshape2)
library(magrittr)
library(ggplot2)

library(foreach)
library(doParallel)
library(doRNG)


DataSets=c(0,5)
Y_offsets=c(.1,-.1)
PchShapes=c(1:6, 15:20)
PchShapes=c(1:20)

plot(1,1, col=0, axes=FALSE, ylim=c(1,12), xlim=c(-8,12), main="", xlab="", ylab="")

TempStor_a=numeric()
TempStor_delta=numeric()
TempStor_c_alpha=numeric()
TempStor_f_alpha=numeric()
TempStor_M_rate=numeric()

for (j in 1:length(DataSets))
{
	DataSet=DataSets[j]
	Y_offset=Y_offsets[j]
	
	if (DataSet==0)
	{
		Models=c("11111", "11101", "11100")
	}
	if (DataSet==5)
	{
		Models=c("11111", "11101", "11011", "11001", "10111", "10101", "10011", "10001", "01111", "01101", "01011", "01001")
	}

	for (i in 1:length(Models))
	{
		Model=Models[i]
		SepModel=unlist(strsplit(Model, ""))

		Output=read.table(paste("C:\\Users\\Dave\\Dropbox\\MDV_VirusDynamics\\R_code\\pompCode\\OutputFiles\\DataSet", DataSet, "_Model", Model, "_Output.txt", sep=""), header=TRUE, stringsAsFactors=FALSE)	#Read in data
		Output= Output[Output$loglik!="loglik",]				#Remove parameter names

		Output=data.frame(lapply(Output,as.numeric))				#Convert to dataframe of numerics

		Output=Output[order(Output$loglik, decreasing=TRUE),]					#sort by likelihood

		Output=Output[1,]							#Truncate to best likelihood




		if (SepModel[3]=="1")
		{
			Temp_mu=1/(1+(Output$c_beta/Output$c_alpha))
			Temp_nu=Output$c_beta + Output$c_alpha

			Temp_mu_guess=1/(1+(Output$c_beta.1/Output$c_alpha.1))
			Temp_nu_guess=Output$c_beta.1 + Output$c_alpha.1

			Output$c_alpha=Temp_mu
			Output$c_beta=Temp_nu

			Output$c_alpha.1=Temp_mu_guess
			Output$c_beta.1=Temp_nu_guess
		}
		if (SepModel[4]=="1")
		{
			Temp_mu=1/(1+(Output$f_beta/Output$f_alpha))
			Temp_nu=Output$f_beta + Output$f_alpha

			Temp_mu_guess=1/(1+(Output$f_beta.1/Output$f_alpha.1))
			Temp_nu_guess=Output$f_beta.1 + Output$f_alpha.1

			Output$f_alpha=Temp_mu
			Output$f_beta=Temp_nu

			Output$f_alpha.1=Temp_mu_guess
			Output$f_beta.1=Temp_nu_guess
		}

#		print(Output$c_alpha)

		Output=log10(Output)

		if(SepModel[1]=="1")
		{
			points(Output$sigma_alpha,12+Y_offset, pch=PchShapes[i], col=j)
		}
		
		
		points(Output$a,11+Y_offset, pch=PchShapes[i], col=j)
		
		if(SepModel[2]=="1")
		{
			points(Output$sigma_a,10+Y_offset, pch=PchShapes[i], col=j)
			TempStor_a[length(TempStor_a)+1]=(Output$a)
		}

		points(Output$gamma,9+Y_offset, pch=PchShapes[i], col=j)
		
		points(Output$delta,8+Y_offset, pch=PchShapes[i], col=j)
		
		TempStor_delta[length(TempStor_delta)+1]=(Output$delta)

		points(Output$c_alpha,7+Y_offset, pch=PchShapes[i], col=j)
		if (j==1)
		TempStor_c_alpha[length(TempStor_c_alpha)+1]=(Output$c_alpha)

		if(SepModel[3]=="1")
		{
			points(Output$c_beta,6+Y_offset, pch=PchShapes[i], col=j)
		}
		points(Output$f_alpha,5+Y_offset, pch=i, col=j)
		TempStor_f_alpha[length(TempStor_f_alpha)+1]=(Output$f_alpha)

		if(SepModel[4]=="1")
		{
			points(Output$f_beta,4+Y_offset, pch=PchShapes[i], col=j)
		}
		
		if(SepModel[5]=="1")
		{
			points(Output$M_rate,3+Y_offset, pch=PchShapes[i], col=j)

			if (j==2)
			TempStor_M_rate[length(TempStor_M_rate)+1]=(Output$M_rate)
			points(Output$M_mu,2+Y_offset, pch=PchShapes[i], col=j)
		}
		
		points(Output$V_0,1+Y_offset, pch=PchShapes[i], col=j)
	}
}

TextLocation=-3
TextSize=1

text(TextLocation, 12, expression(paste("Transmission rate, scale (", sigma[alpha], ")"), sep=""), adj=1, cex=TextSize)
#text(TextLocation-1, 12, expression(paste("Transmission rate, scale"), sep=""), adj=1, cex=TextSize)
#text(TextLocation, 12, expression(paste(sigma[alpha]), sep=""), adj=1, cex=TextSize+1)

text(TextLocation, 11, expression(paste("Virus shed rate, mean (", mu[a], ")"), sep=""), adj=1, cex=TextSize)
text(TextLocation, 10, expression(paste("Virus shed rate, scale (", sigma[a], ")"), sep=""), adj=1, cex=TextSize)

text(TextLocation, 9, expression(paste("Ventilation rate (", gamma, ")"), sep=""), adj=1, cex=TextSize)
text(TextLocation, 8, expression(paste("Within cohort virus decay rate (", delta, ")"), sep=""), adj=1, cex=TextSize)

text(TextLocation, 7, expression(paste("Between cohort cleanout, mean (", mu[C], ")"), sep=""), adj=1, cex=TextSize)
text(TextLocation, 6, expression(paste("Between cohort cleanout, sample size (", nu[C], ")"), sep=""), adj=1, cex=TextSize)

text(TextLocation, 5, expression(paste("Between cohort virus decay, mean (", mu[f], ")"), sep=""), adj=1, cex=TextSize)
text(TextLocation, 4, expression(paste("Between cohort virus decay, sample size (", nu[f], ")"), sep=""), adj=1, cex=TextSize)

text(TextLocation, 3, expression(paste("Virus reintroduction, rate parameter (", M[r], ")"), sep=""), adj=1, cex=TextSize)
text(TextLocation, 2, expression(paste("Virus reintroduction, virus copies (", M[mu], ")"), sep=""), adj=1, cex=TextSize)

text(TextLocation, 1, expression(paste("Initial virus copies (", V[0], ")"), sep=""), adj=1, cex=TextSize)

axis(1, c(-2:10), cex.axis=1.5)

#expression(paste("Sampled values, ", mu, "=5, ", sigma,"=1")))

mtext("log10 parameter value", 1, line=3, cex=2, at=4)