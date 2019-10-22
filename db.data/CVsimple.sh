advertisers=(2259)
cvrnds=(CV1)
BDIV=(64)
#advertisers=(2259 2261 2821 2997 1458 3358 3386 3427 3476)
#cvrnds=(CV1 CV2 CV3 CV4)
#BDIV=(64 32 16 8 4 2)

for ((i=0; i<${#advertisers[@]}; i++)); do
    for ((j=0; j<${#cvrnds[@]}; j++)); do
        echo "AD: ${advertisers[$i]} ${cvrnds[$j]}"
        
        # initial data preparation
        cp ${cvrnds[$j]}/*${advertisers[$i]}* .
        cp ../cache/${cvrnds[$j]}/*${advertisers[$i]}* ../cache

        # parameters for ORTB and PRUD are in the cache, do the benchmark directly
        for ((k=0; k<${#BDIV[@]}; k++)); do
            Rscript ../benchmark.R ${advertisers[$i]} ${BDIV[$k]}
        done

        # copy benchmark results
        mv ../cache/parm_${advertisers[$i]}* ../cache/${cvrnds[$j]}
        #rm *${advertisers[$i]}*
    done
done


