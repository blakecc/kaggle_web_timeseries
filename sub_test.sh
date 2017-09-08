#PBS -q UCTlong
#PBS -l nodes=1:ppn=15:series600
#PBS -N kag_web_sub

#PBS -m abe
#PBS -M blake.rsa@gmail.com

cd /home/cnnbla001/kaggle_web_timeseries
module load software/R-3.3.2 mpi/openmpi-1.10.1

mpirun -np 15 -hostfile $PBS_NODEFILE R --slave CMD BATCH compile_submission_2_cluster.R
