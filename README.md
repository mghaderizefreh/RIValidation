# running simulations and getting results
To simulate and analyse a scenario run `Rscript.R` with 4 inputs <function><study><fittingMethod><repNumber>
where function is 1:cte (known), 2:linear, 3:wood,
study is 1:pairwise, 2:amplitude change, 3:phase change, 4:change of onset, 5:frequency change and 6:change in observation period,
fittingMethod is 1: QR, 2:RR, 3:AVF but if function is 1 this input ignored (not needed if function is constant or known)
and repNumber is any positive integer

Alternatively, create a list of inputs with 4 columns
The columns in input file correspond to
<function><study><fittingMethod><repNumber>
and run in cluster

`qsub -t 1-<NL> -l h_vmem=5G -cwd ./array.sh <fileName>`

where `<NL>` is the number of lines in input file `<fileName>`

# plots
for getting the plots, concatenate similar files (remove redundant headers) and run the file plots.R. The data used for the paper are compressed and put in the folder results.

