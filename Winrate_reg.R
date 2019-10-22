#' Loading required library
suppressPackageStartupMessages({
  library(magrittr)
  library(data.table)
  library(dplyr)
  #library(ggplot2)
})

setwd("~/TKDD2019")
source("globalSetting.R")

args <- commandArgs(trailingOnly = TRUE)
ADID <- as.character(args[1])

if (ADID == "1458" | ADID == "3358" | ADID == "3386" | ADID == "3427" | ADID == "3476") {
    season <- "2nd"
} else if (ADID == "2259" | ADID == "2261" | ADID == "2821" | ADID == "2997") {
    season <- "3rd"
}

loginfo("Execution started\n")
loginfo("Begin to do winrate regression by adid...\n")

bid <- 1:300
#for (cur_adid in if (season == "training2nd") c("1458", "3358", "3386", "3427", "3476") else c("2259", "2261", "2821", "2997")) {
    cur_adid <- ADID
    loginfo("adid: %s\n", cur_adid)
    bic <- readRDS(sprintf("db.data/ictr_%s.Rds", cur_adid))
    win <- rep(0.0, 300)
    for (i in 1:300) {
	    win[i] <- sum(bic$PayingPrice <= bid[i]) / length(bic$PayingPrice)
    }
    g.wr <- winrate_regression(bid, win)
    loginfo("winrate regression done! c = %f\n", g.wr$r$par)
    if (FALSE) {
        pdf(file = sprintf("wr_%s.pdf", cur_adid))
        print(ggplot(data = data.frame(x = bid, y = win), aes(x = x, y = y)) + geom_line())
        dev.off()
        pdf(file = sprintf("wr_fit_%s.pdf", cur_adid))
        print(ggplot(data = data.frame(x = bid, y = g.wr$predict(bid)), aes(x = x, y = y)) + geom_line())
        dev.off()
    }
    saveRDS(g.wr, sprintf("db.data/gwr_%s.Rds", cur_adid))
    #message("Press Return To Continue...\n")
    #invisible(readLines("stdin", n=1))
#}

loginfo("Execution finish!\n")

