


if (DataSet==0)
{
 	PlacementDates= c(466, 592, 690, 809, 908, 1012, 1124, 1236, 1363, 1454, 1558)			#FarmAHouse1
	LoadOutDates=  c(549, 674, 765, 891, 997, 1084, 1209, 1350, 1443, 1528, 1649)			#FarmAHouse1
	FlockSizes= c(28000, 28500, 24000, 26600, 21900, 30100, 21700, 33800, 28000, 31000, 27500)	#FarmAHouse1
}
	

if (DataSet==5)
{
	PlacementDates= c(305,389,473,586,655,719,785,855,919,977, 1040,1100,1153,1213,1262,1318,1380,1443,1493,1549,1614)					#FarmEHouse4
	LoadOutDates=  c(376,465,541,626,694,760,821,897,962,1017,1084,1141,1201,1255,1311,1358,1428,1481,1541,1597,1655)					#FarmEHouse4
	FlockSizes= c(29600,26761,24904,29800,28500,28000,24500,25900,21600,24500,25400,26900,27400,28000,27000,27900,27500,26500,27900,27900,27500)		#FarmEHouse4
}



Start=min(c(PlacementDates, LoadOutDates))
End=max(c(PlacementDates, LoadOutDates))

Time=seq(Start, End, delta.t)

BirdShed=numeric(length(Time))
BirdNumber=numeric(length(Time))
CohortAge=numeric(length(Time))
Placement=numeric(length(Time))
LoadOut=numeric(length(Time))

j=1
for (i in 1:length(Time))
{
	if (PlacementDates[j]<=Time[i])
	{
		CohortAge[i]=Time[i]-PlacementDates[j]
		BirdShed[i]=368*exp(-326/((Time[i]-PlacementDates[j])^1.64))+10.8;
		BirdNumber[i]=FlockSizes[j]
		Placement[i]=0
		LoadOut[i]=0
	}
	else
	{
		CohortAge[i]=0
		BirdShed[i]=0
		BirdNumber[i]=0
		Placement[i]=0
		LoadOut[i]=0
	}

	if (PlacementDates[j]==Time[i])
	{
		Placement[i-1]=1
		BirdNumber[i-1]=FlockSizes[j]
	}
	if (LoadOutDates[j]==Time[i])
	{
		j=j+1
		LoadOut[i]=1
		BirdShed[i]=0
		BirdNumber[i]=0
	}	

}

#cbind(Time, BirdShed, BirdNumber)


CovarTable <- data.frame(
  Time=Time,
#  CohortAge=CohortAge,
  BirdShed=BirdShed,
  BirdNumber=BirdNumber,
  Placement=Placement,
  LoadOut=LoadOut
  )
  


