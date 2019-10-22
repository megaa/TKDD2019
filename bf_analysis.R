suppressPackageStartupMessages(library("methods"))
suppressPackageStartupMessages(library("magrittr"))
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("data.table"))
suppressPackageStartupMessages(library("Matrix"))
suppressPackageStartupMessages(library("FeatureHashing"))

setwd("~/TKDD2019")

source("~/TKDD2019/globalSetting.R")
source("~/TKDD2019/algorithms.R")    # ToDo: remove the hard-coded path

    args <- commandArgs(trailingOnly = TRUE)
    ADID <- if (!is.na(args[1])) AD.parm$ADID[AD.parm$ADID == args[1]] else G.ADID
    BDGT_DIV <- if (!is.na(args[2])) as.integer(args[2]) else G.BDGT_DIV
    START <- if (!is.na(args[3])) as.integer(args[3]) else 1
    END <- if (!is.na(args[4])) as.integer(args[4]) else 0
    scale <- if (!is.na(args[5])) as.integer(args[5]) else 1
    prud2.l.pct <- if (!is.na(args[6])) as.integer(args[6]) else G.PRUD2.L.PCT
    if (prud2.l.pct == 0) {
	prud2.l <- if (!is.na(args[7])) as.integer(args[7]) else AD.parm$LIFT[AD.parm$ADID == ADID] 
    } else if (prud2.l.pct == 1) {
	prud2.l <- get_benchmark_parm(ADID, BDGT_DIV, "LIFT")
    }

bicc <- readRDS(sprintf("~/TKDD2019/db.data/ictr_%s.Rds", ADID))
bicc <- bicc %>% mutate(R_CLK = as.integer(is_click) * 1)
g.wp <- readRDS(sprintf("~/TKDD2019/db.data/gwp.cen_%s.Rds", ADID))
m.am <- readRDS(sprintf("~/TKDD2019/db.data/%s_%s.Rds", if (TRUE) "amtr" else "amte", ADID))
m.wp <- hashed.model.matrix(~V1, data = data.frame(V1 = c(1,2,3)), hash.size = m.am@Dim[[2]], is.dgCMatrix = FALSE)
m.wp@i <- m.am@i
m.wp@p <- m.am@p
m.wp@x <- m.am@x
m.wp@Dim <- m.am@Dim
wp.hat <- g.wp$predict(m.wp)
#wp.err <- readRDS("~/TKDD2019/db.data/wp_err_train.Rds")
#wp.hat <- bicc$PayingPrice + wp.err
bicc <- bicc %>% mutate(pred_wp = wp.hat)

cat(sprintf("# of imp = %d\n", length(bicc$pred_wp)))

p.fun <- switch(G.PRED.TO.USE, "PERFECT" = click_predictor_PERFECT, "WN" = click_predictor_WN)
bicc <- p.fun(ADID, bicc, isTrain = TRUE)
bicc_bak <- bicc

overall.AD <- list(Total.Cost = 0, Total.Clk = 0, Total.Clk.Price = 0)
overall.AD$Total.Cost <- sum(bicc$PayingPrice)                              # Total cost if we get all imprssions
predict.AD <- list(
    Campaign.Budget = overall.AD$Total.Cost / BDGT_DIV)  # Budget available for the testing campaign lifetime
cat(sprintf("TOTAL_COST = %d, BDGT = %.2f\n", overall.AD$Total.Cost, predict.AD$Campaign.Budget))

bf1.minimum <- get_predictor_parm(ADID, "MIN", isTrain = TRUE) / 300
bf2.maximum <- get_predictor_parm(ADID, "MAX", isTrain = TRUE) / 1
cat(sprintf("bf_min = %10.6f, bf_max = %10.6f\n", bf1.minimum, bf2.maximum))

for (l in 0:299) {
    if (length(bicc$PayingPrice[bicc$pred_wp + l >= bicc$PayingPrice & bicc$R_CLK > 0]) >= (as.numeric(prud2.l.pct) / 100.0) * length(bicc$PayingPrice[bicc$R_CLK > 0])) break
}
LIFT <- l
cat(sprintf("PRUD2 lift %d%% = %10.6f\n\n", prud2.l.pct, LIFT))
bicc$pred_wp <- bicc$pred_wp + LIFT
bicc$pred_wp[bicc$pred_wp > 300] <- 300
bicc <- bicc %>% mutate(BidEff = pred_clk / pred_wp)

for (i in 1:10) {
    cat(sprintf("#%6d: PCLK = %.6f, PWP = %10.6f, BE = %E\n", i, bicc$pred_clk[i], bicc$pred_wp[i], bicc$BidEff[i]))
}
for (i in (length(bicc$pred_wp) - 10):length(bicc$pred_wp)) {
    cat(sprintf("#%6d: PCLK = %.6f, PWP = %10.6f, BE = %E\n", i, bicc$pred_clk[i], bicc$pred_wp[i], bicc$BidEff[i]))
}
BE_vec <- sort(bicc$BidEff)
RES_vec <- integer(as.integer((length(BE_vec) - 1) / scale) + 1)
if (END == 0) END <- length(BE_vec)
j <- 1 
for (i in seq.int(1, length(BE_vec), by = scale)) {
    if (i < START) {
        #cat(" skipped\n")
        RES_vec[j] <- -1 
    }
    else if (i > END) {
        RES_vec[j] <- -1
    }
    else {
        cat(sprintf("#%d: s%d -> ", j, i))
        rr <- algorithm_PRUD2(ADID, predict.AD$Campaign.Budget, LIFT, g.wp, BE_vec[i], bicc_bak, isWpRdy = TRUE, isTrain = TRUE)
        show_algo_res(rr)
        RES_vec[j] <- rr$achieved.num.click
    }
    j <- j + 1
}
#RES_vec
saveRDS(RES_vec, sprintf("BET_%sB%ds%d.Rds", ADID, BDGT_DIV, scale)

