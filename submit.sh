#!/bin/bash
set -e
source /etc/profile.d/modules.sh
module load roslin/R/4.2.1 
./Rscript.R $1 $2 $3 $4
