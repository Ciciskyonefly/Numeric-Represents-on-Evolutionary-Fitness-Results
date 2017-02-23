
#PBS -o serverrecoder/${PBS_JOBID}.out
#PBS -e serverrecoder/${PBS_JOBID}.err
#PBS -l nodes=ubri02
/usr/bin/R CMD BATCH --vanilla --slave '--args tsp pre c(50) c(100,1000,10000) each' run_bash.R shell-output/LM_50_each.txt
