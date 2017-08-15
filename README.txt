

##Instructions for MDV virus dynamics paper...
##Files are present on computer and on grid...

##Fitting data using maximum likelihood in pomp
1) Log on to LionXG...
2) Go to directory: /gpfs/home/dak30/work/pomp_MDV/R_code
	2b)Make sure files are up to date with files from desktop... if loading desktop files, be sure to change OnGrid to TRUE.
3) Run batch file:
	##qsub BatchFileR.sh -v arg1=5a00000a39		#Here "5" is the DataSet, "00000" is the model, "39" is the seed
	3b) ./BatchSubmitter 5 00000 		#Here "5" is the DataSet, "00000" is the seed.  This file will run 10 realizations with seeds 0-9.  Change the seeds if necessary.
4) Run this for a set of datasets and models
5) Change to output directory: /gpfs/home/dak30/work/pomp_MDV/GlobalSearchOutput
6) Copy DataSet*_Output.txt to desktop computer directory: "C:\\Users\\Dave\\Dropbox\\MDV_VirusDynamics\\R_code\\pompCode\\OutputFiles\\"
7) View output by running file in R (make sure to edit DataSet and Model accordingly): 
	> source("C:\\Users\\Dave\\Dropbox\\MDV_VirusDynamics\\R_code\\pompCode\\RunFiles\\AnalyzingPompOutput.R")
8) View simulations of maximum likelihood by running file in R (again make sure to edit DataSet and Model accordingly:
	> source("C:\\Users\\Dave\\Dropbox\\MDV_VirusDynamics\\R_code\\pompCode\\RunFiles\\MasterSimulator.R")


##Determining likelihood function for model
1) Run file in R: "C:\\Users\\Dave\\Dropbox\\MDV_VirusDynamics\\R_code\\pompCode\\RunFiles\\DeterminingLHoodFunction.R"
