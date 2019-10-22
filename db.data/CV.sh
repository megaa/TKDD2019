advertisers=(2259)
cvrnds=(CV1)
BDIV=(64 32 16 8 4 2)
#advertisers=(2259 2261 2821 2997 1458 3358 3386 3427 3476)
#cvrnds=(CV1 CV2 CV3 CV4)

for ((i=0; i<${#advertisers[@]}; i++)); do
    for ((j=0; j<${#cvrnds[@]}; j++)); do
        echo "AD: ${advertisers[$i]} ${cvrnds[$j]}"
        
        # initial data preparation
        cp ${cvrnds[$j]}/*${advertisers[$i]}* .
        #Rscript ../Winrate_reg.R ${advertisers[$i]}
        #Rscript ../wp_regression.R ${advertisers[$i]} 1

        # tune parameters for ORTB and PRUD, then do the benchmark
        for ((k=0; k<${#BDIV[@]}; k++)); do
            Rscript ../lambda_tune.R ${advertisers[$i]} ${BDIV[$k]} 
            Rscript ../bf_tune.R ${advertisers[$i]} ${BDIV[$k]}
            Rscript ../benchmark.R ${advertisers[$i]} ${BDIV[$k]}
        done

        # copy benchmark results
        mkdir -p ../cache/${cvrnds[$j]}
        mv ../cache/parm_${advertisers[$i]}* ../cache/${cvrnds[$j]}
        #rm *${advertisers[$i]}*
    done
done


