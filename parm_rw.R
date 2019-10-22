setwd("~/TKDD2019")

source("globalSetting.R")

if (interactive()) {
    stopifnot(FALSE)
} else {
    ori_parm_start = 0
    FPATH = ""
    args <- commandArgs(trailingOnly = TRUE)
    ARG1 <- as.character(args[1])
    if (nchar(ARG1) >= 2) {
        if (substr(ARG1, nchar(ARG1), nchar(ARG1)) == "/") {
            ori_parm_start = 1
            FPATH <- ARG1
        }
    }
    ADID <- AD.parm$ADID[AD.parm$ADID == args[ori_parm_start + 1]]
    BDGT_DIV <- as.integer(args[ori_parm_start + 2])
    name <- as.character(args[ori_parm_start + 3])
    parm1 <- if (!is.na(args[ori_parm_start + 4])) as.numeric(args[ori_parm_start + 4]) else NA
    parm2 <- if (!is.na(args[ori_parm_start + 5])) as.numeric(args[ori_parm_start + 5]) else NA
    parm3 <- if (!is.na(args[ori_parm_start + 6])) as.numeric(args[ori_parm_start + 6]) else NA
}

if (!is.na(parm1)) {
    switch(name,
	"BF" = parm <- list(BF = c(parm1, parm2)),
	"BF.C" = parm <- list(BF.C = as.integer(parm1)),
	"LIFT" = parm <- list(LIFT = parm1),
	"LAM" = parm <- list(LAM = c(parm1, parm2)),
	"LAM.C" = parm <- list(LAM.C = parm1),
	"C" = parm <- list(C = parm1),
	"RES.P" = parm <- list(RES.P = list(total.cost = parm2, achieved.num.click = as.integer(parm1), budget.left = parm3)),
	"RES.O" = parm <- list(RES.O = list(total.cost = parm2, achieved.num.click = as.integer(parm1), budget.left = parm3)))
	
    update_benchmark_parm(ADID, BDGT_DIV, parm, name, FPATH)
}

show_benchmark_parm(ADID, BDGT_DIV, FPATH)


