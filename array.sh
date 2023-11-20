#!/bin/bash
#$ -cwd
#$ -o /exports/eddie/scratch/mghaderi/logso/$JOB_NAME.$JOB_ID.$TASK_ID
#$ -e /exports/eddie/scratch/mghaderi/logse/$JOB_NAME.$JOB_ID.$TASK_ID
set -e
fname=$1
t_n=$(sed "$SGE_TASK_ID q;d" $fname | awk '{print $1}')
s_n=$(sed "$SGE_TASK_ID q;d" $fname | awk '{print $2}')
e_n=$(sed "$SGE_TASK_ID q;d" $fname | awk '{print $3}')
r_n=$(sed "$SGE_TASK_ID q;d" $fname | awk '{print $4}')
./submit.sh $t_n $s_n $e_n $r_n 