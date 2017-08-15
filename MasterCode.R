

#Run with command: qsub BatchFileR.sh -v arg1=0a00000a1
#This runs model set "0", model "00000", with seed "1"

library(pomp)
library(plyr)
library(reshape2)
library(magrittr)
library(ggplot2)

library(foreach)
library(doParallel)
library(doRNG)


theme_set(theme_bw())
stopifnot(packageVersion("pomp")>="1.6")

OnGrid=TRUE

GrowerNames=c("FarmAHouse1", "FarmAHouse2", "FarmEHouse1", "FarmEHouse2", "FarmEHouse3", "FarmEHouse4", "FarmDHouse1", "FarmDHouse2", "FarmDHouse3", "FarmDHouse4", "FarmBHouse1", "FarmBHouse2")

			#0 -> FarmAHouse1, 
			#1 -> FarmAHouse2, 
			#2-> FarmEHouse1
			#3-> FarmEHouse2
			#4-> FarmEHouse3
			#5-> FarmEHouse4
			#6-> FarmDHouse1
			#7-> FarmDHouse2
			#8-> FarmDHouse3
			#9-> FarmDHouse4
			#10-> FarmBHouse1
			#11-> FarmBHouse2
			#12-> Sim1
			#13-> Sim2


if (OnGrid==TRUE)
{

	args <- commandArgs(trailingOnly = F)
	myargument <- args[length(args)]
	
	myargument <- sub("-","",myargument)


	Temp=strsplit(myargument, "a")
	Temp=unlist(Temp)

	DataSet = Temp[1]		
	Model = Temp[2]
	SeedArg = Temp[3]
	
	Seed=as.numeric(SeedArg)
	set.seed(Seed)

	Path="/gpfs/home/dak30/work/pomp_MDV/DataInput/"
	SourcePath=paste("/gpfs/home/dak30/work/pomp_MDV/R_code/", sep="")
	WritePath="/gpfs/home/dak30/work/pomp_MDV/GlobalSearchOutput/"
	StewPath=paste("/gpfs/home/dak30/work/pomp_MDV/GlobalSearchOutput/MDV_GlobalSearch_DataSet", DataSet, "_Model", Model, "_Seed", Seed, ".rda", sep="")
	


}
if (OnGrid==FALSE)
{
	DataSet=0
	Model="01000"
	Seed=104
	
	set.seed(Seed)

	Path="C:\\Users\\Dave\\Dropbox\\MDV_VirusDynamics\\DataFiles\\pompData\\"
	SourcePath=paste("C:\\Users\\Dave\\Dropbox\\MDV_VirusDynamics\\R_code\\pompCode\\RunFiles\\", sep="")
	WritePath=paste("C:\\Users\\Dave\\Dropbox\\MDV_VirusDynamics\\R_code\\pompCode\\OutputFiles\\", sep="")
	StewPath=paste("MDV_GlobalSearch_DataSet", DataSet, "_Model", Model, "_Seed", Seed, ".rda", sep="")
}

Temp=unlist(strsplit(Model, ""))
Model1=as.numeric(Temp[1])
Model2=as.numeric(Temp[2])
Model3=as.numeric(Temp[3])
Model4=as.numeric(Temp[4])
Model5=as.numeric(Temp[5])

DataSet=as.numeric(DataSet)

##Load data and pomp object##

delta.t=1.0/1.0

source(paste(SourcePath, "LoadData.R", sep=""))
source(paste(SourcePath, "ToEstFromEst.R", sep=""))
source(paste(SourcePath, "rmeas.R", sep=""))
source(paste(SourcePath, "dmeas.R", sep=""))


source(paste(SourcePath, "initlz.R", sep=""))
source(paste(SourcePath, "rproc.R", sep=""))
source(paste(SourcePath, "CovarTable.R", sep=""))
source(paste(SourcePath, "pompObject.R", sep=""))

#source(paste(SourcePath, "initlz_old.R", sep=""))
#source(paste(SourcePath, GrowerNames[DataSet+1], "_rproc.R", sep=""))
#source(paste(SourcePath, "pompObject_old.R", sep=""))



if (0)
{
	PreCalcTime=proc.time(); logmeanexp(replicate(10, logLik(pfilter(MDV_sir, Np=1000))), se=TRUE); PostCalcTime=proc.time(); print((PostCalcTime-PreCalcTime)[[3]])
	nsim <- 9
	x <- simulate(MDV_sir, nsim=nsim, as.data.frame=TRUE, include.data=TRUE)
}
if (0)
{
	obj=MDV_sir
	x <- coef(obj, transform=TRUE)
	obj1 <- obj
	coef(obj1, transform=TRUE) <- x
	identical(coef(obj), coef(obj1))
	identical(coef(obj1, transform=TRUE), x)
}

if (Model3==0)
{
	c_alpha_range=c(0,1)
}else (c_alpha_range=c(0,10))
if (Model4==0)
{
	f_alpha_range=c(0,1)
}else (f_alpha_range=c(0,10))

MDV_box <- rbind(
 sigma_alpha = c(0,1),
 a = c(5.5, 7.5),
 sigma_a = c(0,1),
 gamma = c(0,1),
 delta = c(0,1),
 c_alpha = c_alpha_range,		#Upper limit of 1 makes this compatible with model lacking variation in c
 c_beta = c(0,10),		
 f_alpha = f_alpha_range,		#Upper limit of 1 makes this compatible with model lacking variation in f
 f_beta = c(0,10),
 M_mu = c(7,9),
 M_rate = c(0.01,1),
 V_0 = c(7, 11)
)

guesses <- as.data.frame(apply(MDV_box, 1, function(x)runif(10, x[1], x[2])))
guesses[,2]=10^guesses[,2]
guesses[,10]=10^guesses[,10]
guesses[,12]=10^guesses[,12]

library(foreach)
library(doParallel)
library(doRNG)
registerDoParallel()
registerDoRNG(Seed)



stew(file=StewPath,
{
	m1 <- foreach(guess=iter(guesses,"row"),
		.packages='pomp',.combine=rbind,
		.export=c("MDV_sir", "MDV_box", "fixed_params", "guesses"),
		.options.multicore=list(set.seed=TRUE)
	) %dopar% {

		mf <- mif2(MDV_sir,
			Np=1000,
			Nmif=500,
			start=c(unlist(guess), fixed_params),
			cooling.type="geometric",
			cooling.fraction.50=0.5,
			transform=TRUE,
			rw.sd=rw.sd(sigma_alpha=0.02, a=0.02, sigma_a=0.02, gamma=0.02, delta=0.02, c_alpha=0.02, c_beta=0.02, f_alpha=0.02, f_beta=0.02, M_mu=0.02, M_rate=0.02, V_0=ivp(0.2)) 
		)	
		ll <- logmeanexp(replicate(10, logLik(pfilter(mf, Np=10000))), se=TRUE)
		
		data.frame(as.list(coef(mf)), guess, loglik=ll[1], loglik.se=ll[2])
	}	
})
	
	

if (OnGrid==TRUE)
{
	write.table(cbind(Seed,DataSet,m1), file=paste(WritePath, "DataSet", DataSet, "_Model", Model, "_Output.txt", sep=""), append=TRUE, row.names=FALSE)
}
