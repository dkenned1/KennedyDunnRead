

TempModel=Model
if (DataSet==12 | DataSet==13)
{
	TempModel="11111"
}

Output=read.table(paste("C:\\Users\\Dave\\Dropbox\\MDV_VirusDynamics\\R_code\\pompCode\\OutputFiles\\DataSet", DataSet, "_Model", TempModel, "_Output.txt", sep=""), header=TRUE, stringsAsFactors=FALSE)	#Read in data
Output= Output[Output$loglik!="loglik",]				#Remove parameter names

Output=data.frame(lapply(Output,as.numeric))				#Convert to dataframe of numerics

Output=Output[order(Output$loglik, decreasing=TRUE),]					#sort by likelihood


fixed_params <- c(alpha= alpha, Beta=Beta, S_0=S_0, D_0=D_0, DataSet=DataSet, Model1=Model1, Model2=Model2, Model3=Model3, Model4=Model4, Model5=Model5)


i=1
Temp=Output[i,]

sigma_alpha=Temp$sigma_alpha
a=Temp$a
sigma_a=Temp$sigma_a
gamma=Temp$gamma
delta=Temp$delta
c_alpha=Temp$c_alpha
c_beta=Temp$c_beta
f_alpha=Temp$f_alpha
f_beta=Temp$f_beta
M_mu=Temp$M_mu
M_rate=Temp$M_rate
V_0=Temp$V_0
         

params <- c(sigma_alpha=sigma_alpha, a=a, sigma_a=sigma_a, gamma=gamma, delta=delta, c_alpha=c_alpha, c_beta=c_beta, f_alpha=f_alpha, f_beta=f_beta, M_mu=M_mu, M_rate=M_rate, V_0=V_0, fixed_params)          
