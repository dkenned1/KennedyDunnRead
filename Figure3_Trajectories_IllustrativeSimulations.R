

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


par(mfcol=c(2,1), mar=c(1,1,0,0), oma=c(6,6,2,2))

AxisTextSize=2
AxisLabelSize=2
YLabOffset=4
XLabOffset=3
nsim <- 10

theme_set(theme_bw())
stopifnot(packageVersion("pomp")>="1.6")

OnGrid=FALSE

GrowerNames=c("FarmAHouse1", "FarmAHouse2", "FarmEHouse1", "FarmEHouse2", "FarmEHouse3", "FarmEHouse4", "FarmDHouse1", "FarmDHouse2", "FarmDHouse3", "FarmDHouse4", "FarmBHouse1", "FarmBHouse2", "SimulatedFarm1", "SimulatedFarm2")

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
			#12-> SimulatedFarm1
			#13-> SimulatedFarm2

if (OnGrid==FALSE)
{
	DataSet=12
	Model="11111"
	Seed=1
	
	set.seed(Seed)

	Path="C:\\Users\\Dave\\Dropbox\\MDV_VirusDynamics\\DataFiles\\pompData\\"
	SourcePath=paste("C:\\Users\\Dave\\Dropbox\\MDV_VirusDynamics\\R_code\\pompCode\\RunFiles\\", sep="")
	WritePath=paste("C:\\Users\\Dave\\Dropbox\\MDV_VirusDynamics\\R_code\\pompCode\\OutputFiles\\", sep="")
	StewPath=paste("MDV_GlobalSearch_DataSet", DataSet, "_Model", Model, "_Seed", Seed, ".rda", sep="")
	ParamReadPath=paste("C:\\Users\\Dave\\Dropbox\\MDV_VirusDynamics\\R_code\\pompCode\\OutputFiles\\DataSet", DataSet, "_Model", Model, "_Output.txt", sep="")
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

if(1)
{
Temp=which(dat[,3]<100)
dat[Temp,3]=0
Temp=which(dat[,4]<100)
dat[Temp,4]=0
Temp=which(dat[,5]<100)
dat[Temp,5]=0
Temp=which(dat[,6]<100)
dat[Temp,6]=0
Temp=which(dat[,7]<100)
dat[Temp,7]=0
Temp=which(dat[,8]<100)
dat[Temp,8]=0
}

source(paste(SourcePath, "initlz.R", sep=""))
source(paste(SourcePath, "rproc.R", sep=""))
source(paste(SourcePath, "CovarTable.R", sep=""))
source(paste(SourcePath, "pompObject_ParamInitializer.R", sep=""))


if(DataSet==12)			#Sim1
{
	rect_left <- c(0,50,100,150,200,250,300)
	rect_right <- c(-1e6, 40,90,140,190,240,290)
	rectangles <- data.frame(
	  xmin = rect_left,
	  xmax = rect_right,
	  ymin = -10,
	  ymax = 10
	)
}
if(DataSet==13)			#Sim2
{
	rect_left <- c(0,90,180,270)
	rect_right <- c(-1e6, 80,170,260)
	rectangles <- data.frame(
	  xmin = rect_left,
	  xmax = rect_right,
	  ymin = -10,
	  ymax = 10
	)
}


makeTransparent<-function(someColor, alpha)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
    blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}


library(reshape2)
library(ggplot2)

log_trans_mean=function(y)
{
	Temp2=log10(y+1)
	mean(Temp2, na.rm=TRUE)
}


{

	plot(1, 1, ylim=c(0,6.6), pch=20, col=0, xlim=c(10,325), axes=FALSE)
	rect(rectangles[,2],rectangles[,3],rectangles[,1],rectangles[,4],col = makeTransparent("red",50))	
	box()

	for (i in 1:nsim)
	{
		x <- simulate(MDV_sir, nsim=1, as.data.frame=TRUE, include.data=FALSE)
		SimulatedData=unlist(apply(x[,3:8], 1, log_trans_mean))
		
		Time=x$time[seq(1, nrow(x), 7)]
		SimulatedData=SimulatedData[seq(1, nrow(x), 7)]

		
		lines(Time, SimulatedData, col=i, lwd=1)
	}

	y <- 0:6
	aty <- axTicks(2)
	labels <- sapply(aty,function(i)
					 as.expression(bquote(10^ .(i)))
					 )
	axis(2,at=aty,labels=labels, las=1, cex.axis=AxisTextSize)

}




DataSet=13

DataSet=as.numeric(DataSet)

##Load data and pomp object##

delta.t=1.0/1.0

source(paste(SourcePath, "LoadData.R", sep=""))
source(paste(SourcePath, "ToEstFromEst.R", sep=""))
source(paste(SourcePath, "rmeas.R", sep=""))
source(paste(SourcePath, "dmeas.R", sep=""))

if(1)
{
Temp=which(dat[,3]<100)
dat[Temp,3]=0
Temp=which(dat[,4]<100)
dat[Temp,4]=0
Temp=which(dat[,5]<100)
dat[Temp,5]=0
Temp=which(dat[,6]<100)
dat[Temp,6]=0
Temp=which(dat[,7]<100)
dat[Temp,7]=0
Temp=which(dat[,8]<100)
dat[Temp,8]=0
}

source(paste(SourcePath, "initlz.R", sep=""))
source(paste(SourcePath, "rproc.R", sep=""))
source(paste(SourcePath, "CovarTable.R", sep=""))
source(paste(SourcePath, "pompObject_ParamInitializer.R", sep=""))


if(DataSet==12)			#Sim1
{
	rect_left <- c(0,50,100,150,200,250,300)
	rect_right <- c(-1e6, 40,90,140,190,240,290)
	rectangles <- data.frame(
	  xmin = rect_left,
	  xmax = rect_right,
	  ymin = -10,
	  ymax = 10
	)
}
if(DataSet==13)			#Sim2
{
	rect_left <- c(0,90,180,270)
	rect_right <- c(-1e6, 80,170,260)
	rectangles <- data.frame(
	  xmin = rect_left,
	  xmax = rect_right,
	  ymin = -10,
	  ymax = 10
	)
}




{

	plot(1, 1, ylim=c(0,6.6), pch=20, col=0, xlim=c(10,325), axes=FALSE)
	rect(rectangles[,2],rectangles[,3],rectangles[,1],rectangles[,4],col = makeTransparent("red",50))	
	box()

	for (i in 1:nsim)
	{
		x <- simulate(MDV_sir, nsim=1, as.data.frame=TRUE, include.data=FALSE)
		SimulatedData=unlist(apply(x[,3:8], 1, log_trans_mean))
		
		Time=x$time[seq(1, nrow(x), 7)]
		SimulatedData=SimulatedData[seq(1, nrow(x), 7)]

		
		lines(Time, SimulatedData, col=i, lwd=1)
	}

	y <- 0:6
	aty <- axTicks(2)
	labels <- sapply(aty,function(i)
					 as.expression(bquote(10^ .(i)))
					 )
	axis(2,at=aty,labels=labels, las=1, cex.axis=AxisTextSize)
	axis(1, pretty(c(10,325)), cex.axis=AxisTextSize)

	mtext("Time (days)", 1, cex=AxisLabelSize, line=XLabOffset)
	
	mtext("VCN per mg dust", 2, cex=AxisLabelSize, line=YLabOffset, las=0, at=7)

}


