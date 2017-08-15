



	Path="C:\\Users\\Dave\\Dropbox\\MDV_VirusDynamics\\DataFiles\\pompData\\"

	par(mfcol=c(1,2))
	
	dat=read.table(paste(Path, "FarmAHouse1_pompData_fulldataset.txt", sep=""), header=TRUE, fill=TRUE)
	Temp=dat
	
	dat=read.table(paste(Path, "FarmAHouse2_pompData_fulldataset.txt", sep=""), header=TRUE, fill=TRUE)
	Temp=rbind(Temp, dat)

	dat=read.table(paste(Path, "FarmEHouse1_pompData_fulldataset.txt", sep=""), header=TRUE, fill=TRUE)
	Temp=rbind(Temp, dat)

	dat=read.table(paste(Path, "FarmEHouse2_pompData_fulldataset.txt", sep=""), header=TRUE, fill=TRUE)
	Temp=rbind(Temp, dat)

	dat=read.table(paste(Path, "FarmEHouse3_pompData_fulldataset.txt", sep=""), header=TRUE, fill=TRUE)
	Temp=rbind(Temp, dat)

	dat=read.table(paste(Path, "FarmEHouse4_pompData_fulldataset.txt", sep=""), header=TRUE, fill=TRUE)
	Temp=rbind(Temp, dat)

	dat=read.table(paste(Path, "FarmDHouse1_pompData_fulldataset.txt", sep=""), header=TRUE, fill=TRUE)
	Temp=rbind(Temp, dat)

	dat=read.table(paste(Path, "FarmDHouse2_pompData_fulldataset.txt", sep=""), header=TRUE, fill=TRUE)
	Temp=rbind(Temp, dat)

	dat=read.table(paste(Path, "FarmDHouse3_pompData_fulldataset.txt", sep=""), header=TRUE, fill=TRUE)
	Temp=rbind(Temp, dat)

	dat=read.table(paste(Path, "FarmDHouse4_pompData_fulldataset.txt", sep=""), header=TRUE, fill=TRUE)
	Temp=rbind(Temp, dat)
	
	dat=read.table(paste(Path, "FarmBHouse1_pompData_fulldataset.txt", sep=""), header=TRUE, fill=TRUE)
	Temp=rbind(Temp, dat)

	dat=read.table(paste(Path, "FarmBHouse2_pompData_fulldataset.txt", sep=""), header=TRUE, fill=TRUE)
	Temp=rbind(Temp, dat)

	DataStor=Temp
	
	DataStor[which(DataStor[,3]<100),3]=0
	DataStor[which(DataStor[,4]<100),4]=0
	DataStor[which(DataStor[,5]<100),5]=0
	DataStor[which(DataStor[,6]<100),6]=0
	DataStor[which(DataStor[,7]<100),7]=0
	DataStor[which(DataStor[,8]<100),8]=0
	
	DataStor[is.na(DataStor[,3]),3]=0
	DataStor[DataStor[,3]==0,3]=NA
	DataStor[is.na(DataStor[,4]),4]=0
	DataStor[DataStor[,4]==0,4]=NA
	DataStor[is.na(DataStor[,5]),5]=0
	DataStor[DataStor[,5]==0,5]=NA
	DataStor[is.na(DataStor[,6]),6]=0
	DataStor[DataStor[,6]==0,6]=NA
	DataStor[is.na(DataStor[,7]),7]=0
	DataStor[DataStor[,7]==0,7]=NA
	DataStor[is.na(DataStor[,8]),8]=0
	DataStor[DataStor[,8]==0,8]=NA

	dat=DataStor
	

	
	sd_na=function(x){sd(log10(x+1), na.rm=TRUE)}
	mean_na=function(x){mean(log10(x+1), na.rm=TRUE)}

	SDs=apply((dat[,3:8]), 1, sd_na)
	Means=apply((dat[,3:8]), 1, mean_na)

	TempMeans=Means[Means>3]
	TempSDs=SDs[Means>3]

#	plot(Means, SDs)
#	Model1=lm(SDs~Means)
#	abline(Model1)
	
	
	DataStor=Temp

	DataStor[which(DataStor[,3]<100),3]=0
	DataStor[which(DataStor[,4]<100),4]=0
	DataStor[which(DataStor[,5]<100),5]=0
	DataStor[which(DataStor[,6]<100),6]=0
	DataStor[which(DataStor[,7]<100),7]=0
	DataStor[which(DataStor[,8]<100),8]=0

	
	dat=DataStor
	Means=apply((dat[,3:8]), 1, mean_na)

	count_zero=function(x){sum(x==0, na.rm=TRUE)}
	count_nonzero=function(x){sum(x!=0, na.rm=TRUE)}

	num_abs=apply((dat[,3:8]), 1, count_zero)
	num_pres=apply((dat[,3:8]), 1, count_nonzero)

	Model=glm(cbind(num_pres, num_abs)~Means, family=binomial(link="probit"))

#	plot(Means, logit(num_pres/(num_abs+num_pres)))	
#	abline(Model)

	plot(Means, num_pres/(num_abs+num_pres), ylab="Fraction positive", xlab="Mean VCN per mg dust (log10+1)", cex.lab=1.5)
	curve(pnorm(coef(Model)[1] + coef(Model)[2]*x), add=TRUE)
	
	
	
	plot(TempMeans, TempSDs, xlab="Mean VCN per mg dust (log10+1)", ylab="Standard deviation (log10 units)", cex.lab=1.5)
	Model2=lm(TempSDs~TempMeans)
	abline(Model2)


	print(Model2)
	print(Model)
