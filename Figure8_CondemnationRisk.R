

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

library(reshape2)
library(ggplot2)

theme_set(theme_bw())
stopifnot(packageVersion("pomp")>="1.6")

OnGrid=FALSE

AxisTextSize=2
AxisLabelSize=2
YLabOffset=4
XLabOffset=3
nsim <- 5000

PointSize=1.5
PointColor="dodgerblue"

UnknownHistColor="dodgerblue"

x_start=320
x_end=1630

par(oma=c(6,6,2,2), mfcol=c(2,1), mar=c(1,1,1,1))

DaysBefore=30


StorAlterValues=c(2,2,2,2,2,2,2,2,2,2,2,2,2,5, 0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,-5,1)

Stor1=numeric(length(StorAlterValues))
Stor2=numeric(length(StorAlterValues))

for (zz in 1:length(StorAlterValues))
{

Params=14

AlterValue=numeric(Params)

AlterValue[1]=1		#sigma_alpha
AlterValue[2]=1		#a
AlterValue[3]=1		#sigma_a
AlterValue[4]=1		#gamma
AlterValue[5]=1		#delta
AlterValue[6]=1		#c_alpha
AlterValue[7]=1		#c_beta
AlterValue[8]=1		#f_alpha
AlterValue[9]=1		#f_beta
AlterValue[10]=1	#M_mu
AlterValue[11]=1	#M_rate
AlterValue[12]=1	#V_0
AlterValue[13]=1	#S_0
AlterValue[14]=0	#CohortDuration

AlterValue[((zz-1)%%Params)+1]=StorAlterValues[zz]		#sigma_alpha



#sigma_alpha=sigma_alpha*AlterValue[1]
#a=a*AlterValue[2]
#sigma_a=sigma_a*AlterValue[3]
#gamma=gamma*AlterValue[4]
#delta=delta*AlterValue[5]
#c_alpha=c_alpha*AlterValue[6]
#c_beta=c_beta*AlterValue[7]
#f_alpha=f_alpha*AlterValue[8]
#f_beta=f_beta*AlterValue[9]
#M_mu=M_mu*AlterValue[10]
#M_rate=M_rate*AlterValue[11]
#V_0=V_0*AlterValue[12]
#S_0=S_0*AlterValue[13]
#CohortDuration=CohortDuration + AlterValue[14]

makeTransparent<-function(someColor, alpha)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
    blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}


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



DataSet=5

Models=c("01101", "01001", "01011", "10101", "10011", "11001", "10001", "11101", "01111", "11011", "10111", "11111")
Weights=c(0.19, 0.16, 0.16, 0.08, 0.08, 0.08, 0.06, 0.05, 0.05, 0.04, 0.03, 0.03)

Seed=104
set.seed(Seed)

SimsPerModel=as.numeric(table(sample(length(Models), nsim, replace=TRUE, prob=Weights)))

for (ii in 1:length(Models))
{
	Model=Models[ii]
	TempSims=SimsPerModel[ii]
	
	Path="C:\\Users\\Dave\\Dropbox\\MDV_VirusDynamics\\DataFiles\\pompData\\"
	SourcePath=paste("C:\\Users\\Dave\\Dropbox\\MDV_VirusDynamics\\R_code\\pompCode\\RunFiles\\", sep="")
	WritePath=paste("C:\\Users\\Dave\\Dropbox\\MDV_VirusDynamics\\R_code\\pompCode\\OutputFiles\\", sep="")
	StewPath=paste("MDV_GlobalSearch_DataSet", DataSet, "_Model", Model, "_Seed", Seed, ".rda", sep="")
	ParamReadPath=paste("C:\\Users\\Dave\\Dropbox\\MDV_VirusDynamics\\R_code\\pompCode\\OutputFiles\\DataSet", DataSet, "_Model", Model, "_Output.txt", sep="")

	Temp=unlist(strsplit(Model, ""))
	Model1=as.numeric(Temp[1])
	Model2=as.numeric(Temp[2])
	Model3=as.numeric(Temp[3])
	Model4=as.numeric(Temp[4])
	Model5=as.numeric(Temp[5])

	DataSet=as.numeric(DataSet)

	##Load data and pomp object##

	delta.t=1.0/1.0

	source(paste(SourcePath, "LoadData_simulation.R", sep=""))
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
	source(paste(SourcePath, "CovarTable_AlterRearing.R", sep=""))
	source(paste(SourcePath, "pompObject_ParamInitializer_AlterRearing.R", sep=""))

	if(DataSet==5)			
	{
		rect_left <- c(  305, 389,473,586,655,719,785,855,919,977,1040,1100,1153,1213,1262,1318,1380,1443,1493,1549,1614,2000)
		rect_right <- c(-100, 376,465,541,626,694,760,821,897,962,1017,1084,1141,1201,1255,1311,1358,1428,1481,1541,1597,1655)
		rectangles <- data.frame(
		  xmin = rect_left,
		  xmax = rect_right,
		  ymin = -10,
		  ymax = 10
		)
	}

	PlottingThreshold=30

	if (ii==1)
	{
		x <- simulate(MDV_sir, nsim=TempSims, as.data.frame=TRUE, include.data=FALSE)
	}
	else
	{
		y <- simulate(MDV_sir, nsim=TempSims, as.data.frame=TRUE, include.data=FALSE)
		
		x=rbind(x,y)
	}
}

if (nsim>PlottingThreshold)
{
	
	Times=unique(x$time)	
	Temp3=rowSums(x[,10:15])/(rowSums(x[,9:15])+.001)
	
	IntervalWidths=c(.50, .75, .95, .99)
	
	Bounds=matrix(,length(Times),length(IntervalWidths)*2+1)
	for (i in 1:length(Times))
	{
		Bounds[i,] = as.numeric(quantile(Temp3[(1:nsim)*length(Times)+i], p=c((.5-IntervalWidths/2), (.5+IntervalWidths/2),.5), na.rm=TRUE))
	}
	
	plot(Times, Bounds[,2], ylim=c(0,1), pch=20, col=0, xlim=c(x_start,x_end), axes=FALSE, xlab="", ylab="", main="")

	polygon(c(Times, rev(Times)), c(Bounds[,4], rev(Bounds[,8])), col = "grey90", border = NA)
	polygon(c(Times, rev(Times)), c(Bounds[,3], rev(Bounds[,7])), col = "grey80", border = NA)
	polygon(c(Times, rev(Times)), c(Bounds[,2], rev(Bounds[,6])), col = "grey70", border = NA)
	polygon(c(Times, rev(Times)), c(Bounds[,1], rev(Bounds[,5])), col = "grey50", border = NA)
	
	lines(Times, Bounds[,9], col="black", lwd=3)
	
	PotentialCondemnation1=(Bounds[Times%in%(rect_right[-1]-DaysBefore),])
	
	rect(rectangles[,2],rectangles[,3],rectangles[,1],rectangles[,4],col = makeTransparent("red",50))
	rect(rectangles[1,2],rectangles[1,3],rectangles[1,1],rectangles[1,4],col = UnknownHistColor)
	rect(rectangles[nrow(rectangles),2],rectangles[nrow(rectangles),3],rectangles[nrow(rectangles),1],rectangles[nrow(rectangles),4],col = UnknownHistColor)

	box()

	y <- 0:1
	aty <- axTicks(2)
	axis(2,at=aty,labels=pretty(y), las=1, cex.axis=AxisTextSize)



	FocalTimes=rect_right[-1]-DaysBefore		#FocalTimesForCondemnationRisk
	
	SumPotentialCondemnation1=0
	SumBirds1=0
	for (m in 1:length(FocalTimes))
	{
		SumPotentialCondemnation1=SumPotentialCondemnation1 + sum(x[x$time==FocalTimes[m],9])
		SumBirds1 = SumBirds1 + sum(x[x$time==FocalTimes[m],9:15])
	}
}








DataSet=0

Models=c("11101", "11100", "11111")
Weights=c(0.47, 0.31, 0.22)


SimsPerModel=as.numeric(table(sample(length(Models), nsim, replace=TRUE, prob=Weights)))

for (ii in 1:length(Models))
{
	Model=Models[ii]
	TempSims=SimsPerModel[ii]
	
	Path="C:\\Users\\Dave\\Dropbox\\MDV_VirusDynamics\\DataFiles\\pompData\\"
	SourcePath=paste("C:\\Users\\Dave\\Dropbox\\MDV_VirusDynamics\\R_code\\pompCode\\RunFiles\\", sep="")
	WritePath=paste("C:\\Users\\Dave\\Dropbox\\MDV_VirusDynamics\\R_code\\pompCode\\OutputFiles\\", sep="")
	StewPath=paste("MDV_GlobalSearch_DataSet", DataSet, "_Model", Model, "_Seed", Seed, ".rda", sep="")
	ParamReadPath=paste("C:\\Users\\Dave\\Dropbox\\MDV_VirusDynamics\\R_code\\pompCode\\OutputFiles\\DataSet", DataSet, "_Model", Model, "_Output.txt", sep="")

	Temp=unlist(strsplit(Model, ""))
	Model1=as.numeric(Temp[1])
	Model2=as.numeric(Temp[2])
	Model3=as.numeric(Temp[3])
	Model4=as.numeric(Temp[4])
	Model5=as.numeric(Temp[5])

	DataSet=as.numeric(DataSet)

	##Load data and pomp object##

	delta.t=1.0/1.0

	source(paste(SourcePath, "LoadData_simulation.R", sep=""))
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
	source(paste(SourcePath, "CovarTable_AlterRearing.R", sep=""))
	source(paste(SourcePath, "pompObject_ParamInitializer_AlterRearing.R", sep=""))

	if(DataSet==0)			
	{
		rect_left <- c(466, 592, 690, 809, 908, 1012, 1124, 1236, 1363, 1454, 1558, 1e7)
		rect_right <- c(-100, 549, 674, 765, 891, 997, 1084, 1209, 1350, 1443, 1528, 1649)
		rectangles <- data.frame(
		  xmin = rect_left,
		  xmax = rect_right,
		  ymin = -10,
		  ymax = 10
		)
	}

	if (ii==1)
	{
		x <- simulate(MDV_sir, nsim=TempSims, as.data.frame=TRUE, include.data=FALSE)
	}
	else
	{
		y <- simulate(MDV_sir, nsim=TempSims, as.data.frame=TRUE, include.data=FALSE)
		
		x=rbind(x,y)
	}
}


if (nsim>PlottingThreshold)
{
	Times=unique(x$time)	
	Temp3=rowSums(x[,10:15])/(rowSums(x[,9:15])+.001)
	
	IntervalWidths=c(.50, .75, .95, .99)
	
	Bounds=matrix(,length(Times),length(IntervalWidths)*2+1)
	for (i in 1:length(Times))
	{
		Bounds[i,] = as.numeric(quantile(Temp3[(1:nsim)*length(Times)+i], p=c((.5-IntervalWidths/2), (.5+IntervalWidths/2),.5), na.rm=TRUE))
	}
	
	plot(Times, Bounds[,2], ylim=c(0,1), pch=20, col=0, xlim=c(x_start,x_end), axes=FALSE, xlab="", ylab="", main="")

	polygon(c(Times, rev(Times)), c(Bounds[,4], rev(Bounds[,8])), col = "grey90", border = NA)
	polygon(c(Times, rev(Times)), c(Bounds[,3], rev(Bounds[,7])), col = "grey80", border = NA)
	polygon(c(Times, rev(Times)), c(Bounds[,2], rev(Bounds[,6])), col = "grey70", border = NA)
	polygon(c(Times, rev(Times)), c(Bounds[,1], rev(Bounds[,5])), col = "grey50", border = NA)
	
	lines(Times, Bounds[,9], col="black", lwd=3)
	
	PotentialCondemnation2=(Bounds[Times%in%(rect_right[-1]-DaysBefore),])
	
	rect(rectangles[,2],rectangles[,3],rectangles[,1],rectangles[,4],col = makeTransparent("red",50))
	rect(rectangles[1,2],rectangles[1,3],rectangles[1,1],rectangles[1,4],col = UnknownHistColor)
	rect(rectangles[nrow(rectangles),2],rectangles[nrow(rectangles),3],rectangles[nrow(rectangles),1],rectangles[nrow(rectangles),4],col = UnknownHistColor)


	box()

	y <- 0:1
	axis(2,at=aty,labels=pretty(y), las=1, cex.axis=AxisTextSize)

	axis(1, pretty(range(Times)), cex.axis=AxisTextSize)

	mtext("Time (days)", 1, cex=AxisLabelSize, line=XLabOffset)
	
	mtext("Fraction exposed or infectious", 2, cex=AxisLabelSize, line=YLabOffset, las=0, at=1.1)


	FocalTimes=rect_right[-1]-DaysBefore		#FocalTimesForCondemnationRisk
	
	SumPotentialCondemnation2=0
	SumBirds2=0
	for (m in 1:length(FocalTimes))
	{
		SumPotentialCondemnation2=SumPotentialCondemnation2 + sum(x[x$time==FocalTimes[m],9])
		SumBirds2 = SumBirds2 + sum(x[x$time==FocalTimes[m],9:15])
	}

}

print(1- SumPotentialCondemnation1/SumBirds1)
print(1- SumPotentialCondemnation2/SumBirds2)

print(zz)

Stor1[zz]=1- SumPotentialCondemnation1/SumBirds1
Stor2[zz]=1- SumPotentialCondemnation2/SumBirds2

}


par(mfcol=c(1,1), mar=c(2,2,2,2), oma=c(4,4,1,1))

#Reorder1=Stor1[c(1,15, 2,16, 3,17, 4,18, 5,19, 6,20, 7,21, 8,22, 9,23, 10,24, 11,25, 12,26, 13,27, 14,28)]
#Reorder2=Stor2[c(1,15, 2,16, 3,17, 4,18, 5,19, 6,20, 7,21, 8,22, 9,23, 10,24, 11,25, 12,26, 13,27, 14,28)]

Reorder1=Stor1[c(15,1, 16,2, 17,3, 18,4, 19,5, 20,6, 21,7, 22,8, 23,9, 24,10, 25,11, 26,12, 27,13, 28,14)]
Reorder2=Stor2[c(15,1, 16,2, 17,3, 18,4, 19,5, 20,6, 21,7, 22,8, 23,9, 24,10, 25,11, 26,12, 27,13, 28,14)]


LeftAdj=.1
RightAdj=.1
PlottingLocations=c(seq(1+LeftAdj,28,2), seq(2-RightAdj, 28,2))
PlottingLocations=sort(PlottingLocations)

plot(PlottingLocations, Reorder1, col=2, pch=1, ylim=c(0,1), axes=FALSE, xlab="", ylab="", main="")
abline(h=Stor1[29], col=2, lwd=2)

points(PlottingLocations, Reorder2, col=1, pch=2)
abline(h=Stor2[29], col=1, lwd=2)

abline(v=seq(0.5, 29, 2), lty="dotted", lwd=1)

box()

axis(2, pretty(c(0,1)), las=2, cex.axis=2)
mtext("Condemnation risk", 2, cex=2, line=4)


XLine=3
XCex=2

XLocations=seq(1.5, 29.5, 2)
mtext(expression(sigma[alpha]), 1, cex=XCex, line=XLine, at=XLocations[1])
mtext(expression(mu[a]), 1, cex=XCex, line=XLine, at=XLocations[2])
mtext(expression(sigma[a]), 1, cex=XCex, line=XLine, at=XLocations[3])
mtext(expression(gamma), 1, cex=XCex, line=XLine, at=XLocations[4])
mtext(expression(delta), 1, cex=XCex, line=XLine, at=XLocations[5])
mtext(expression(mu[c]), 1, cex=XCex, line=XLine, at=XLocations[6])
mtext(expression(nu[c]), 1, cex=XCex, line=XLine, at=XLocations[7])
mtext(expression(mu[f]), 1, cex=XCex, line=XLine, at=XLocations[8])
mtext(expression(nu[f]), 1, cex=XCex, line=XLine, at=XLocations[9])
mtext(expression(M[mu]), 1, cex=XCex, line=XLine+.35, at=XLocations[10])
mtext(expression(M[r]), 1, cex=XCex, line=XLine+.1, at=XLocations[11])
mtext(expression(V[0]), 1, cex=XCex, line=XLine+.1, at=XLocations[12])
mtext(expression(S[0]), 1, cex=XCex, line=XLine+.1, at=XLocations[13])
mtext(expression(tau[max]), 1, cex=XCex, line=XLine+.1, at=XLocations[14])

axis(1, labels=c(rep(c("0.5x", "2x"), 13), "-5","+5"), at=PlottingLocations, cex.axis=.85) 

