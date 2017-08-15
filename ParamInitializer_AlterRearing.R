

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
        

sigma_alpha=sigma_alpha*AlterValue[1]
a=a*AlterValue[2]
sigma_a=sigma_a*AlterValue[3]
gamma=gamma*AlterValue[4]
delta=delta*AlterValue[5]

if (AlterValue[6]==2 && Model3==1)		#Double efficiency (only half of previous dust remains relative to other parameter set), mu_new = (mu_old +1)/2
{
	Temp_alpha=c_alpha + c_beta/2
	Temp_beta= c_beta/2
	c_alpha=Temp_alpha
	c_beta=Temp_beta
}
if (AlterValue[6]==2 && Model3==0)		#Double efficiency (only half of previous dust remains relative to other parameter set), mu_new = (mu_old +1)/2
{
	c_alpha=(c_alpha+1)/2
}

if (AlterValue[6]==0.5 && Model3==1)		#Half efficiency (twice the previous dust remains relative to other parameter set), mu_new = 2*mu_old-1 
{
	Temp_alpha = c_alpha - c_beta
	Temp_beta = 2*c_beta
	c_alpha=max(Temp_alpha,0)
	c_beta=Temp_beta
}
if (AlterValue[6]==0.5 && Model3==0)		#Half efficiency (twice the previous dust remains relative to other parameter set), mu_new = 2*mu_old-1 
{
	c_alpha=max(2*c_alpha -1, 0)
}

if (AlterValue[7]!=1 && Model3==1)
{
	c_alpha=AlterValue[7]*c_alpha
	c_beta=AlterValue[7]*c_beta
}

if (AlterValue[8]==2 && Model4==1)		#Double efficiency (only half of previous dust remains relative to other parameter set), mu_new = (mu_old +1)/2
{
	Temp_alpha=f_alpha + f_beta/2
	Temp_beta= f_beta/2
	f_alpha=Temp_alpha
	f_beta=Temp_beta
}
if (AlterValue[8]==2 && Model4==0)		#Double efficiency (only half of previous dust remains relative to other parameter set), mu_new = (mu_old +1)/2
{
	f_alpha=(f_alpha+1)/2
}

if (AlterValue[8]==0.5 && Model4==1)		#Half efficiency (twice the previous dust remains relative to other parameter set), mu_new = 2*mu_old-1 
{
	Temp_alpha = f_alpha - f_beta
	Temp_beta = 2*f_beta
	f_alpha=max(Temp_alpha,0)
	f_beta=Temp_beta
}
if (AlterValue[8]==0.5 && Model4==0)		#Half efficiency (twice the previous dust remains relative to other parameter set), mu_new = 2*mu_old-1 
{
	f_alpha=max(2*f_alpha -1, 0) 
}

if (AlterValue[9]!=1 && Model4==1)
{
	f_alpha=AlterValue[9]*f_alpha
	f_beta=AlterValue[9]*f_beta
}

M_mu=M_mu*AlterValue[10]
M_rate=M_rate*AlterValue[11]
V_0=V_0*AlterValue[12]

        

params <- c(sigma_alpha=sigma_alpha, a=a, sigma_a=sigma_a, gamma=gamma, delta=delta, c_alpha=c_alpha, c_beta=c_beta, f_alpha=f_alpha, f_beta=f_beta, M_mu=M_mu, M_rate=M_rate, V_0=V_0, fixed_params)          

