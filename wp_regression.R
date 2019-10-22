#' Loading required library
suppressPackageStartupMessages({
  library(magrittr)
  library(data.table)
  library(dplyr)
  library(Matrix)
  library(FeatureHashing)
  library(stringr)
})

setwd("~/TKDD2019")

source("globalSetting.R")
source("colClass.R")

#season <- "training3rd" #"training2nd"
#ad.to.run <- c("2261") #c("2259", "2261", "2821", "2997")
#ad.to.run <- c("3358") #c("1458", "3358", "3386", "3427", "3476")


get_am <- function(adid, isTrain = FALSE) {
    yzx <- readLines(sprintf("db.data/%s.yzx_%s.txt", if (isTrain) "train" else "test", adid))
    yzx <- str_replace_all(yzx, ":1", "")
    yzx <- strsplit(yzx, " ")
    yzx <- lapply(yzx, as.integer)
    rm_first_2 <- function(vec) { tail(vec, length(vec) - 2) }
    yzx1 <- lapply(yzx, rm_first_2)
    yzx1 <- lapply(yzx1, sort)
    rm_dup <- function(vec) { vec[!duplicated(vec)] }
    yzx1 <- lapply(yzx1, rm_dup)
    ami <- unlist(yzx1)
    amx <- rep(as.numeric(1.0), length(ami))
    yzx1.cnt <- lapply(yzx1, length)
    amp <- append(as.integer(0), as.integer(cumsum(yzx1.cnt)))
    am <- Matrix(sparse = TRUE)
    am <- as(am, "dgCMatrix")
    am@i <- ami
    am@x <- amx
    am@p <- amp
    am@Dim[[2]] <- as.integer(length(yzx))
    if (isTrain) {
        am@Dim[[1]] <- as.integer(max(ami) + 1)
    } else {
	r <- system2("wc", sprintf("-l db.data/featindex_%s.txt", adid), stdout = TRUE)
	r <- strsplit(r, " ")
	suppressWarnings(s <- as.integer(r[[1]]))
 	am@Dim[[1]] <- s[!is.na(s)]
    }
    am
}

args <- commandArgs(trailingOnly = TRUE)
ADID <- as.character(args[1])
RE.RUN <- if (!is.na(args[2])) as.logical(as.integer(args[2])) else FALSE # to force re-run

if (ADID == "1458" | ADID == "3358" | ADID == "3386" | ADID == "3427" | ADID == "3476") {
    season <- "2nd"
} else if (ADID == "2259" | ADID == "2261" | ADID == "2821" | ADID == "2997") {
    season <- "3rd"
}
ad.to.run <- c(ADID)

loginfo("Execution started\n")

if (file.exists(sprintf("cache/bicc_testing%s.Rds", season))) {
    bidimpclk2 <- readRDS(sprintf("cache/bicc_testing%s.Rds", season))
    #bidimpclk2 <- bidimpclk2[bidimpclk2$R_CLK != 0,]
    bicc2 <- bidimpclk2 %>% filter(adid == ADID)
} else {
    bicc2 <- readRDS(sprintf("db.data/icte_%s.Rds", ADID))
    bicc2 <- bicc2 %>% select(BidID, BiddingPrice, IP, Region, City, AdExchange, Domain, URL, AdSlotId, AdSlotWidth, 
        AdSlotHeight, AdSlotVisibility, AdSlotFormat, CreativeID, PayingPrice, adid, usertag, weekday, hour, imp_t, is_click)
    bicc2 <- bicc2 %>% mutate(R_CONV = 0)
    setnames(bicc2, "is_click", "R_CLK")
}

loginfo("Begin to do winning price regression\n")

for (cur_adid in ad.to.run) {
    loginfo("adid: %s\n", cur_adid)
    if (!RE.RUN & file.exists(sprintf("db.data/gwp.cen_%s.Rds", cur_adid))) {
        g.wp <- readRDS(sprintf("db.data/gwp.cen_%s.Rds", cur_adid))
        m2.am <- readRDS(sprintf("db.data/amte_%s.Rds", cur_adid))
    } else {
        bic <- readRDS(sprintf("db.data/ictr_%s.Rds", cur_adid))
        m1.am <- get_am(cur_adid, TRUE)
        m1.wp <- hashed.model.matrix(~V1, data = data.frame(V1 = c(1,2,3)), hash.size = m1.am@Dim[[2]], is.dgCMatrix = FALSE)  # just to create a CSCMatrix
        m1.wp@i <- m1.am@i
        m1.wp@p <- m1.am@p
        m1.wp@x <- m1.am@x
        m1.wp@Dim <- m1.am@Dim
        g.wp <- censored_regression2(m1.wp, bic$PayingPrice, rep(TRUE, nrow(bic)), sd(bic$PayingPrice))
        #g.wp <- linear_regression(m1.wp, bic$PayingPrice, 1000)
        loginfo("wp regression done!\n")
        saveRDS(g.wp, sprintf("db.data/gwp.cen_%s.Rds", cur_adid))
        saveRDS(m1.am, sprintf("db.data/amtr_%s.Rds", cur_adid))
        
        m2.am <- get_am(cur_adid, FALSE)
        #stopifnot(m1.am@Dim[[2]] != m2.am@Dim[[2]])
        rm(m1.am)
        saveRDS(m2.am, sprintf("db.data/amte_%s.Rds", cur_adid))
    }

    # predict wp with the learned model for testing data
    m2.wp <- hashed.model.matrix(~V1, data = data.frame(V1 = c(1,2,3)), hash.size = m2.am@Dim[[2]], is.dgCMatrix = FALSE)  # just to create a CSCMatrix
    m2.wp@i <- m2.am@i
    m2.wp@p <- m2.am@p
    m2.wp@x <- m2.am@x
    m2.wp@Dim <- m2.am@Dim
    wp.hat2 <- g.wp$predict(m2.wp)
    bicc2 <- bicc2 %>% mutate(Price.hat = wp.hat2)
    bicc2 <- bicc2 %>% filter(R_CLK != 0)
    bicc2.mean <- mean(bicc2$PayingPrice)
    bicc2.sd   <- sd(bicc2$PayingPrice)
    wp.hat2.mean <- mean(bicc2$Price.hat)

    loginfo("mean of real wp (w/ Click):      %f\n", bicc2.mean)
    loginfo("sdev of real wp (w/ Click):      %f\n", bicc2.sd)
    loginfo("mean of predicted wp (w/ Click): %f\n", wp.hat2.mean)
    loginfo("MSE  of predicted wp (w/ Click): %f  RMSE: %f\n", mean((bicc2$PayingPrice - bicc2$Price.hat)^2), mean((bicc2$PayingPrice - bicc2$Price.hat)^2) ^ 0.5)
    lift <- bicc2.mean - wp.hat2.mean
    loginfo("MSE of predicted wpf (w/ Click): %f (lift %f)\n\n", mean((bicc2$PayingPrice - (bicc2$Price.hat + lift))^2), lift)
}

loginfo("Execution finish!\n")

