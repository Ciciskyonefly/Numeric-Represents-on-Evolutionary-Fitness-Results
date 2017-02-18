#! /bin/bash
echo we are in the $PWD directory
jobname=run
cat << EOF | qsub
#PBS -N $jobname
#PBS -q batch
#PBS -l nodes=ubri03
#PBS -o \$PBS_O_WORKDIR\$jobname.txt
#PBS -e \$PBS_O_WORKDIR\$jobname.txt
cd \$PBS_O_WORKDIR
R CMD BATCH --vanilla --slave run.R hello.txt
EOF

