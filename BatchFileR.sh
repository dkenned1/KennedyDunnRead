#!/bin/bash
#PBS -j oe
#PBS -l walltime=23:59:59
#PBS -l nodes=10:ppn=1
#PBS -l pmem=1gb

cd $PBS_O_WORKDIR
module load R/3.3.0

date +"%m/%d/%Y %H:%M:%S $HOSTNAME"

R CMD BATCH -${arg1} MasterCode.R

date +"%m/%d/%Y %H:%M:%S $HOSTNAME"

