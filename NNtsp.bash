#! /bin/bash
# efg, 10 Octoober 2016
#PBS -o serverrecoder/${PBS_JOBID}.out
#PBS -e serverrecoder/${PBS_JOBID}.err
#PBS -l nodes=ubri04
/usr/bin/R CMD BATCH --vanilila --slave '--args NN tsp' run.R NNtsp.txt



# cat arrayjob.bash
