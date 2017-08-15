


if (DataSet==0)		#FarmAHouse1
{
	dat=read.table(paste(Path, "FarmAHouse1_pompData_fulldataset.txt", sep=""), header=TRUE, fill=TRUE)
	S_0=21900 	
	t0 = 466
	HouseVolume = 5000	#cubic meters
}
if (DataSet==5)		#FarmEHouse4
{
	dat=read.table(paste(Path, "FarmEHouse4_pompData_fulldataset.txt", sep=""), header=TRUE, fill=TRUE)
	S_0=29600	
	t0=305		
	HouseVolume = 5000	#cubic meters
}
