

library(pomp)
library(plyr)
library(reshape2)
library(magrittr)
library(ggplot2)

library(foreach)
library(doParallel)
library(doRNG)


DataSet=5
Model="11111"
SepModel=unlist(strsplit(Model, ""))


Output=read.table(paste("C:\\Users\\Dave\\Dropbox\\MDV_VirusDynamics\\R_code\\pompCode\\OutputFiles\\DataSet", DataSet, "_Model", Model, "_Output.txt", sep=""), header=TRUE, stringsAsFactors=FALSE)	#Read in data
Output= Output[Output$loglik!="loglik",]				#Remove parameter names

Output=data.frame(lapply(Output,as.numeric))				#Convert to dataframe of numerics

Output=Output[order(Output$loglik, decreasing=TRUE),]					#sort by likelihood

Output=Output[Output$loglik>max(Output$loglik-50),]			#Truncate to loglikelihoods within 50 points of best

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

m1=Output

t_m1=log10(m1)
t_m1$loglik=m1$loglik
t_m1$loglik.se=m1$loglik.se

MaxLHood=max(t_m1$loglik)
t_m1=t_m1[t_m1$loglik>MaxLHood-100,]

guesses=t_m1[,25:36]
results=t_m1[,c(3:14,37)]
colnames(guesses)=colnames(results)[1:12]

all <- ldply( list(guess=guesses, result=results), .id="type")
pairs(~loglik + sigma_alpha + a + sigma_a + gamma + delta + c_alpha + c_beta + f_alpha + f_beta + M_mu + M_rate + V_0, data=all, col=ifelse(all$type=="guess", grey(0.5), "red"), pch=16)




PasteCollapse=function(x)
{
	paste0(x, collapse="")
}	

Models=expand.grid(c("0","1"), c("0", "1"), c("0","1"), c("0","1"), c("0","1"))
Models=apply(Models, 1, PasteCollapse)


MaxLik=numeric(length(Models))
ModelStor=numeric(length(Models))
NumGoodSets=numeric(length(Models))
SearchesPerformed=numeric(length(Models))
AIC=numeric(length(Models))

for (i in 1:length(Models))
{
	Output=read.table(paste("C:\\Users\\Dave\\Dropbox\\MDV_VirusDynamics\\R_code\\pompCode\\OutputFiles\\DataSet", DataSet, "_Model", Models[i], "_Output.txt", sep=""), header=TRUE, stringsAsFactors=FALSE)	#Read in data
	Output= Output[Output$loglik!="loglik",]				#Remove parameter names
	Output=data.frame(lapply(Output,as.numeric))				#Convert to dataframe of numerics

	NumGoodSets[i]=sum(Output$loglik>max(Output$loglik-50))
	MaxLik[i]=max(Output$loglik)
	ModelStor[i]=Models[i]
	SearchesPerformed[i]=10*length(unique(Output$Seed))
	
	Parameters=as.numeric(unlist(strsplit(Models[i],"")))
	Parameters[5]=Parameters[5]*2
	
	ParameterPenalty=sum(Parameters)*2
	
	AIC[i]=-MaxLik[i]*2 + ParameterPenalty
}

DeltaAIC=round(AIC-min(AIC), digits=1)

AIC_Weights=exp(-DeltaAIC/2)/ sum(exp(-DeltaAIC/2))

PrintFile=cbind(ModelStor, NumGoodSets, 100*round(NumGoodSets/SearchesPerformed, digits=3), MaxLik, DeltaAIC, AIC_Weights)
PrintFile=data.frame(PrintFile, stringsAsFactors=FALSE)

PrintFile$DeltaAIC=as.numeric(PrintFile$DeltaAIC)
PrintFile$MaxLik=as.numeric(PrintFile$MaxLik)
PrintFile$AIC_Weights=as.numeric(PrintFile$AIC_Weights)

PrintFile$MaxLik=round(PrintFile$MaxLik, digits=1)
PrintFile$AIC_Weights=round(PrintFile$AIC_Weights, digits=2)


PrintFile=PrintFile[order(PrintFile$DeltaAIC, decreasing=FALSE),]


print(PrintFile)


PrintFile[order(PrintFile[,1], decreasing=TRUE),]