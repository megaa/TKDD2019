suppressPackageStartupMessages(library("magrittr"))
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("data.table"))
#suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("Matrix"))
suppressPackageStartupMessages(library("FeatureHashing"))

setwd("~/TKDD2019")

source("~/TKDD2019/globalSetting.R")
source("~/TKDD2019/algorithms.R")  # ToDo: remove the hard-coded path


ORTB_optimal_reg <- function(lam1, lam2, N) {
    lam.m <- (lam2/lam1)^(1/N)
    lam <- lam1
    for (i in 0:N) {
	res.ORTB <- algorithm_ORTB(ADID, predict.AD$Campaign.Budget, p.lambda = lam, p.c = P.C, bicc.testing.AD)
	show_algo_res(res.ORTB)
	lam <- lam * lam.m
    }

    res.ORTB
}

PRUD2_optimal_reg <- function(bf1, bf2, N, lift) {
    bf.m <- (bf2/bf1)^(1/N)
    bf <- bf1
    for (i in 0:N) {
	res.PRUD2 <- algorithm_PRUD2(ADID, predict.AD$Campaign.Budget, lift, g.wp, bf, bicc.testing.AD, isWpRdy = FALSE, isTrain = FALSE, isLiftFirst = TRUE)
	show_algo_res(res.PRUD2)
	bf <- bf * bf.m
    }

    res.PRUD2
}

WPOL1_optimal_reg <- function(lift) {
    res.WPOL1 <- algorithm_WPOL1(ADID, predict.AD$Campaign.Budget, lift, g.wp, bicc.testing.AD, isWpRdy = FALSE, isTrain = FALSE, isLiftFirst = TRUE)
    show_algo_res(res.WPOL1)
    #for (bp in c(0.53, 0.5325, 0.535, 0.5375, 0.54, 0.5425, 0.545, 0.5475, 0.55, 0.61, 0.6125, 0.615, 0.6175, 0.62, 0.6225, 0.625, 0.6275, 0.63)) {
    #for (bp in c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.3, 0.5, 0.7, 0.9)) {
    for (bp in c(0.9, 0.7, 0.5, 0.3, 0.1, 0.08, 0.06, 0.04, 0.02, 0.01)) {
        res.WPOL1 <- algorithm_WPOL1(ADID, predict.AD$Campaign.Budget, lift, g.wp, bicc.testing.AD, isWpRdy = FALSE, isTrain = FALSE, isLiftFirst = TRUE, bidProb = bp * 64)
        show_algo_res(res.WPOL1)
    }

    res.WPOL1
}


if (interactive()) {
    ADID <- G.ADID 
    BDGT_DIV <- G.BDGT_DIV
    LIFT <- Inf
    prud.b1 <- 0
    prud.b2 <- 0
} else {
    args <- commandArgs(trailingOnly = TRUE)
    ADID <- AD.parm$ADID[AD.parm$ADID == args[1]]
    BDGT_DIV <- as.integer(args[2])
    if (!is.na(args[5])) LIFT <- as.integer(args[5]) else LIFT <- Inf
    if (!is.na(args[3])) prud.b1 <- as.numeric(args[3]) else prud.b1 <- 0
    if (!is.na(args[4])) prud.b2 <- as.numeric(args[4]) else prud.b2 <- 0
}

ortb.l <- get_benchmark_parm(ADID, BDGT_DIV, "LAM")
ortb.l1 <- ortb.l[1]
ortb.l2 <- ortb.l[2]
P.C  <- get_benchmark_parm(ADID, BDGT_DIV, "C")
if (LIFT == Inf | prud.b1 == 0 | prud.b2 == 0) {
    prud.b <- get_benchmark_parm(ADID, BDGT_DIV, "BF")
    prud.b1 <- prud.b[1]
    prud.b2 <- prud.b[2]
    LIFT <- get_benchmark_parm(ADID, BDGT_DIV, "LIFT")
}

#if (ADID == "1458" | ADID == "3358" | ADID == "3386" | ADID == "3427" | ADID == "3476") {
#    bicc.testing <- readRDS("~/TKDD2019/cache/bicc_testing2nd.Rds")
#} else if (ADID == "2259" | ADID == "2261" | ADID == "2821" | ADID == "2997") {
#    bicc.testing <- readRDS("~/TKDD2019/cache/bicc_testing3rd.Rds")
#}
#bicc.testing.AD <- bicc.testing %>% filter(adid == ADID) #%>% mutate(R_CLK = as.integer(as.logical(R_CLK))) # to limit R_CLK no more than 1
#rm(bicc.testing)
#gc()
# already changed to use icte_xxxx.Rds instead of bicc_testingxxx.Rds, may be conflicting with the old flow
if (file.exists("~/TKDD2019/cache/bicc_testing2nd.Rds") | file.exists("~/TKDD2019/cache/bicc_testing3rd.Rds")) {
    cat("Error! cache/bicc_testingxxx exists, may be conflicting!!!\n")
    stopifnot(FALSE)
}
bicc.testing.AD <- readRDS(sprintf("~/TKDD2019/db.data/icte_%s.Rds", ADID))
bicc.testing.AD <- bicc.testing.AD %>% select(BidID, BiddingPrice, IP, Region, City, AdExchange, Domain, URL, AdSlotId, AdSlotWidth, 
    AdSlotHeight, AdSlotVisibility, AdSlotFormat, CreativeID, PayingPrice, adid, usertag, weekday, hour, imp_t, is_click)
bicc.testing.AD <- bicc.testing.AD %>% mutate(R_CONV = 0)
setnames(bicc.testing.AD, "is_click", "R_CLK")

overall.AD <- list(Total.Cost = 0, Total.Clk = 0, Total.Clk.Price = 0)
overall.AD$Total.Cost <- sum(bicc.testing.AD$PayingPrice)                              # Total cost if we get all imprssions
overall.AD$Total.Clk <- sum(bicc.testing.AD$R_CLK)                                     # Total #click from all impressions
overall.AD$Total.Clk.Price <- sum((bicc.testing.AD %>% filter(R_CLK > 0))$PayingPrice) # Total cost if we get only all impressions w/ click

predict.AD <- list(
    Campaign.Budget = overall.AD$Total.Cost / BDGT_DIV,  # Budget available for the testing campaign lifetime
    Predicted.Clk = 0) #bb$num.click)                        # Predicted total #click   !!! take care with /2 !!!

cat(sprintf("Benchmark ADID = %s, BDGT_DIV = %d\n", ADID, BDGT_DIV))
print(overall.AD)
print(predict.AD)

res.PERFECT <- algorithm_PERFECT(predict.AD$Campaign.Budget, bicc.testing.AD)
show_algo_res(res.PERFECT)

cat(sprintf("ORTB C = %f\n", P.C))
RES <- ORTB_optimal_reg(ortb.l1, ortb.l2, 1)
update_benchmark_parm(ADID, BDGT_DIV, list(RES.O = RES), "RES.O")

cat(sprintf("PRUD2 lift = %d\n", LIFT))
g.wp <- readRDS(sprintf("~/TKDD2019/db.data/gwp.cen_%s.Rds", ADID))
for (lift in c(0) + LIFT) { #c(-1 * LIFT, 0, 30, 60, 90, 120) + LIFT) {
    RES <- PRUD2_optimal_reg(prud.b1, prud.b2, 1, lift)
    if (lift == LIFT) {
        update_benchmark_parm(ADID, BDGT_DIV, list(RES.P = RES), "RES.P")
    }
}
#RES <- WPOL1_optimal_reg(1)
#RES <- WPOL1_optimal_reg(LIFT)

