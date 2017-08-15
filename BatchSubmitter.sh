#!/bin/bash
#PBS -j oe
#PBS -l walltime=23:59:59
#PBS -l nodes=1
#PSB -l pmem=2gb

####To run type ./BatchSubmitter.sh 0 00000		###Here 0 is the focal DataSet and 00000 is the focal model

for (( i=0; i<10;i++)); do
	##echo hello $1a$2a$i
	echo `qsub BatchFileR.sh -v arg1=$1a$2a$i`
done

qsub BatchFileR.sh -v arg1=5a00100a8