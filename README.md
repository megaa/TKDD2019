# Code for TKDD2019 work

## Introduction
This is a demonstration for the experiments in the paper "Budget-Constrained Real-Time Bidding Optimization: 
Multiple Predictors Make It Better".

## Prerequisites

* R version >= 3.4.4 with the following packages installed: 
```
magrittr
dplyr
data.table
Matrix
FeatureHashing
stringr
```
* Python 2.7

## Test Steps
1. Clone this repository into your home directory (`~/TKDD2019/`)
2. In `~/TKDD2019/db.data/CV1/`, `tar zxvf 2259-1.tgz; tar zxvf 2259-2.tgz`
3. In `~/TKDD2019/db.data/`, run `./CVsimple.sh`

## Expected Results
```
AD: 2259 CV1
Benchmark ADID = 2259, BDGT_DIV = 64
$Total.Cost
[1] 19451381

$Total.Clk
[1] 69

$Total.Clk.Price
[1] 8593

$Campaign.Budget
[1] 303927.8

$Predicted.Clk
[1] 0

PERFECT: Clk 69 Cost 8593 BLeft 295334.83
ORTB C = 50.383358
ORTB with lambda 6.500160E-06: Clk 12 Cost 303923 BLeft 4.83
ORTB with lambda 6.500162E-06: Clk 12 Cost 303923 BLeft 4.83
PRUD2 lift = 144
PRUD2 lift  144, bf 5.387279E-06: nImp 201440, Clk 18 Cost 303790 BLeft 137.83
PRUD2 lift  144, bf 5.387280E-06: nImp 201440, Clk 18 Cost 303790 BLeft 137.83
```

## Copyright Notices
This project reuses a part (namely, CTR predictor) from the work of Weinan Zhang (https://github.com/wnzhang/optimal-rtb) under the Apache 2.0 license.
