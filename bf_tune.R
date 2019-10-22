suppressPackageStartupMessages(library("methods"))
suppressPackageStartupMessages(library("magrittr"))
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("data.table"))
suppressPackageStartupMessages(library("Matrix"))
suppressPackageStartupMessages(library("FeatureHashing"))

setwd("~/TKDD2019")

source("~/TKDD2019/globalSetting.R")
source("~/TKDD2019/algorithms.R")    # ToDo: remove the hard-coded path

PRUD2_optimal_reg <- function(bf1, bf2, N, res1 = NULL, res2 = NULL, halt.a4.max = 0, show.title = TRUE) {
    if (show.title) cat(sprintf("Regression from %E ~ %E (dist %E) with %d values...\n", bf1, bf2, bf2 - bf1, N + 1))
    res <- data.frame(bf = numeric(), click = integer(), budget.left = numeric())
    bf.m <- if (N > 0) (bf2/bf1)^(1.0/as.numeric(N)) else 1
    bf <- bf1
    cur.max <- 0
    reach.max <- FALSE
    for (i in 0:N) {
	if (i == 0 & !is.null(res1)) {
	    res.PRUD2 <- res1
	    cat(sprintf("PRUD2 lift %4d, bf %E: ", LIFT, bf))
	} else if (i == N & !is.null(res2)) {
	    res.PRUD2 <- res2
	    cat(sprintf("PRUD2 lift %4d, bf %E: ", LIFT, bf))
	} else {
	    res.PRUD2 <- algorithm_PRUD2(ADID, predict.AD$Campaign.Budget, LIFT, g.wp, bf, bicc, isWpRdy = TRUE, isTrain = TRUE)
	}
	show_algo_res(res.PRUD2)
	g.run.cnt <<- g.run.cnt + 1
	if (res.PRUD2$achieved.num.click > cur.max) cur.max <- res.PRUD2$achieved.num.click
	res <- rbind(res, data.frame(bf = bf, click = res.PRUD2$achieved.num.click, budget.left = res.PRUD2$budget.left, cost = res.PRUD2$total.cost))

	if (halt.a4.max != 0 & cur.max == halt.a4.max) reach.max <- TRUE
	if (halt.a4.max != 0 & reach.max & res.PRUD2$achieved.num.click < cur.max) { cat(sprintf("Halt after reaching max: %d\n", cur.max)); break }
	bf <- bf * bf.m
    }
    as(res, "data.table")
}


if (interactive()) {
    ADID <- G.ADID 
    BDGT_DIV <- G.BDGT_DIV
    prud2.b1.tune <- G.PRUD2.B1.TUNE
    prud2.b2.tune <- G.PRUD2.B2.TUNE
    prud2.s.tune <- G.PRUD2.S.TUNE
    prud2.l.pct <- G.PRUD2.L.PCT
    prud2.l <- AD.parm$LIFT[AD.parm$ADID == ADID]
} else {
    args <- commandArgs(trailingOnly = TRUE)
    ADID <- if (!is.na(args[1])) AD.parm$ADID[AD.parm$ADID == args[1]] else G.ADID
    BDGT_DIV <- if (!is.na(args[2])) as.integer(args[2]) else G.BDGT_DIV
    prud2.b1.tune <- if (!is.na(args[3])) as.numeric(args[3]) else G.PRUD2.B1.TUNE
    prud2.b2.tune <- if (!is.na(args[4])) as.numeric(args[4]) else G.PRUD2.B2.TUNE
    prud2.s.tune <- if (!is.na(args[5])) as.integer(args[5]) else G.PRUD2.S.TUNE
    prud2.l.pct <- if (!is.na(args[6])) as.integer(args[6]) else G.PRUD2.L.PCT
    if (prud2.l.pct == 0) {
	prud2.l <- if (!is.na(args[7])) as.integer(args[7]) else AD.parm$LIFT[AD.parm$ADID == ADID] 
    } else if (prud2.l.pct == 1) {
	prud2.l <- get_benchmark_parm(ADID, BDGT_DIV, "LIFT")
    }
}
LIFT <- if (prud2.l.pct == 0 | prud2.l.pct == 1) prud2.l else Inf

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

bf1.minimum <- get_predictor_parm(ADID, "MIN", isTrain = TRUE) / 300
bf2.maximum <- get_predictor_parm(ADID, "MAX", isTrain = TRUE) / 1

if (LIFT == -Inf) {  # varying liftor such as L0203
    pc.file <- sprintf("~/TKDD2019/cache/L0203_%s.Rds", ADID)
    stopifnot(file.exists(pc.file))
    L0203 <- readRDS(pc.file)
    bicc$pred_wp[bicc$pred_wp < 1] <- 1
    bicc$pred_wp[bicc$pred_wp > 300] <- 300
    bicc$pred_wp <- bicc$pred_wp + L0203[as.integer(floor(bicc$pred_wp))]
    LIFT <- 0
}

overall.AD <- list(Total.Cost = 0, Total.Clk = 0, Total.Clk.Price = 0)
overall.AD$Total.Cost <- sum(bicc$PayingPrice)                              # Total cost if we get all imprssions
overall.AD$Total.Clk <- sum(bicc$R_CLK)                                     # Total #click from all impressions
overall.AD$Total.Clk.Price <- sum((bicc %>% filter(R_CLK > 0))$PayingPrice) # Total cost if we get only all impressions w/ click

predict.AD <- list(
    Campaign.Budget = overall.AD$Total.Cost / BDGT_DIV)  # Budget available for the testing campaign lifetime

cat(sprintf("Bid Factor tuning ADID = %s, BDGT_DIV = %d\n", ADID, BDGT_DIV))
print(overall.AD)
print(predict.AD)

if (LIFT == Inf) {
    for (l in 0:299) {
	if (length(bicc$PayingPrice[bicc$pred_wp + l >= bicc$PayingPrice & bicc$R_CLK > 0]) >= (as.numeric(prud2.l.pct) / 100.0) * length(bicc$PayingPrice[bicc$R_CLK > 0])) break
    }
    LIFT <- l
    cat(sprintf("PRUD2 lift %d%% = %10.6f\n\n", prud2.l.pct, LIFT))
}

g.run.cnt <- 0

# check and adjust initial range to be within bf1.minimum ~ bf2.maximum
if (prud2.b1.tune < bf1.minimum) {
    cat(sprintf("Reset bf1 from %E to minimal possible value: %E\n", prud2.b1.tune, bf1.minimum))
    prud2.b1.tune <- bf1.minimum
}
if (prud2.b2.tune < bf1.minimum) {
    cat(sprintf("Reset bf2 from %E to: %E\n", prud2.b2.tune, bf1.minimum * 16))
    prud2.b2.tune <- bf1.minimum * 16
}
if (prud2.b2.tune > bf2.maximum) {
    cat(sprintf("Reset bf2 from %E to maximal possible value: %E\n", prud2.b2.tune, bf2.maximum))
    prud2.b2.tune <- bf2.maximum
}
if (prud2.b1.tune > bf2.maximum) {
    cat(sprintf("Reset bf1 from %E to: %E\n", prud2.b1.tune, bf2.maximum / 16))
    prud2.b1.tune <- bf2.maximum / 16
}

bf.ratio <- (prud2.b2.tune / prud2.b1.tune) ^ (1.0 / prud2.s.tune)
res <- PRUD2_optimal_reg(prud2.b1.tune, prud2.b2.tune, prud2.s.tune)
last.range <- c(prud2.b1.tune, prud2.b2.tune)
last.range.array <- data.frame(bf1 = numeric(), bf2 = numeric())

res.sorted <- arrange(res, desc(click))
res.click.max <- res.sorted$click[1]
res.clkmax.so.far <- res.click.max
bf.clkmax.so.far <- prud2.b1.tune * (bf.ratio ^ (which(res$click == res.clkmax.so.far) - 1))
bf.opt1 <- 0
bf.opt2 <- 0
init.shape.OK <- FALSE
prud2.s.tune.bak <- prud2.s.tune
while (bf.opt1 == 0) {
    bf1 <- 0
    bf2 <- 0
    if (!init.shape.OK) {  # check if the initial small-large-small shape encountered
	last.range.array <- rbind(last.range.array, data.frame(bf1 = last.range[1], bf2 = last.range[2])) 
	init.shape.OK <- TRUE
	if (min(which(res$click == res.click.max)) == 1) {  
	    # it is not the shape, but large-small-smaller
	    init.shape.OK <- FALSE
	    if (res$bf[1] / 2 < bf1.minimum) {
                # deal with the extreme left-skewed
		bf1 <- bf1.minimum
		res1 <- NULL
		bf2 <- bf1 * 8  # this multiplicator should be well chosen (currently based on 2997B32 result)
		res2 <- NULL
		prud2.s.tune <- prud2.s.tune * 2 + 1
	    } else {
	        bf1 <- res$bf[1] / 2
	        res1 <- NULL
	        bf2.idx <- 1 
	        bf2 <- res$bf[bf2.idx]
	        res2 <- list(total.cost = res$cost[bf2.idx], achieved.num.click = res$click[bf2.idx], budget.left = res$budget.left[bf2.idx])
	    }
	}
	if (max(which(res$click == res.click.max)) == length(res$click)) {
	    # it is not the shape, but smaller-small-large
	    init.shape.OK <- FALSE
	    bf2 <- res$bf[nrow(res)] * 2
	    res2 <- NULL
	    if (bf1 == 0) {
		bf1.idx <- nrow(res) 
		bf1 <- res$bf[bf1.idx]
		res1 <- list(total.cost = res$cost[bf1.idx], achieved.num.click = res$click[bf1.idx], budget.left = res$budget.left[bf1.idx])
	    }
	}
	if (init.shape.OK) {
	    cat("\nInitial shape check passed!\n")
	    prud2.s.tune <- prud2.s.tune.bak
	} else {
	    res.click.max <- 0
	    #if (nrow(match_df(last.range.array, data.frame(bf1 = bf1, bf2 = bf2))) > 0) {
	    if (nrow(merge(data.frame(bf1 = bf1, bf2 = bf2), last.range.array)) > 0 & prud2.s.tune == prud2.s.tune.bak) {
		bf1 <- bf1 / 2
		bf2 <- bf2 * 2
		res1 <- NULL
		res2 <- NULL
	    }
	}
    }
    
    if (init.shape.OK & res.sorted$click[2] != res.click.max) {  # there are no consecutive max values
	bfm.idx <- which(res$click == res.click.max)
	bf1.idx <- bfm.idx - 1
	bf2.idx <- bfm.idx + 1
	bf1 <- if (bf1.idx >= 1) res$bf[bf1.idx] else res$bf[1] / bf.ratio
	res1 <- if (bf1.idx >= 1) list(total.cost = res$cost[bf1.idx], achieved.num.click = res$click[bf1.idx], budget.left = res$budget.left[bf1.idx]) else NULL
	bf2 <- if (bf2.idx <= nrow(res)) res$bf[bf2.idx] else res$bf[nrow(res)] * bf.ratio
	res2 <- if (bf2.idx <= nrow(res)) list(total.cost = res$cost[bf2.idx], achieved.num.click = res$click[bf2.idx], budget.left = res$budget.left[bf2.idx]) else NULL
	res.click.max <- 0
    } else if (init.shape.OK) {  # there are consecutive max values, check if they follow the converge rule
        bfm1.idx <- min(which(res$click == res.click.max))
        bfm2.idx <- max(which(res$click == res.click.max))
        for (i in bfm1.idx:(bfm2.idx - 1)) {
	    if (res$budget.left[i] < 300 & res$budget.left[i + 1] >= 300 & res$budget.left[i + 1] < 600) {  # it converges!
	        bf.opt1 <- res$bf[i]
	        bf.opt2 <- res$bf[i + 1]
	        break
	    }
	    if (res$bf[i + 1] / res$bf[i] < 1.0000005) {  # it already narrows down to a very small range
		bf.opt1 <- res$bf[i]
		bf.opt2 <- res$bf[i + 1]
		break
	    }
	}
	
	# not converge, find the new range
	if (bf.opt1 == 0) {
	    if (res$budget.left[bfm1.idx] >= 300) {
	        bf1.idx <- bfm1.idx - 1
	        bf2.idx <- bfm1.idx
	    } else if (res$budget.left[bfm2.idx] < 300) {
		bf1.idx <- bfm2.idx
		bf2.idx <- bfm2.idx + 1
	    } else {
		for (i in bfm1.idx:(bfm2.idx - 1)) {
		    if (res$budget.left[i] < 300 & res$budget.left[i + 1] >= 300) break
		}
		bf1.idx <- i
		bf2.idx <- i + 1
	        #bf1.idx <- bfm1.idx
	        #bf2.idx <- bfm2.idx
	    }
	    bf1 <- if (bf1.idx >= 1) res$bf[bf1.idx] else res$bf[1] / bf.ratio
	    res1 <- if (bf1.idx >= 1) list(total.cost = res$cost[bf1.idx], achieved.num.click = res$click[bf1.idx], budget.left = res$budget.left[bf1.idx]) else NULL
	    bf2 <- if (bf2.idx <= nrow(res)) res$bf[bf2.idx] else res$bf[nrow(res)] * bf.ratio
	    res2 <- if (bf2.idx <= nrow(res)) list(total.cost = res$cost[bf2.idx], achieved.num.click = res$click[bf2.idx], budget.left = res$budget.left[bf2.idx]) else NULL
	}
    }
    
    # check if the same range is to be run
    if (bf.opt1 == 0) {
	#if (isTRUE(all.equal(last.range, c(bf1, bf2)))) {   this evaluates to TRUE if last.range[1] != b1 AND last.range[2] != bf2 !!!!!!!!!!!!
	if (identical(last.range, c(bf1, bf2))) {
	    cat("\n!!! Equal range encountered!!!\n")
	    bf.opt1 <- res$bf[1]
	    bf.opt2 <- res$bf[2]
	}
    }

    # check if we got optimal bf, if not, re-run regression with narrowed down range
    if (bf.opt1 == 0) {
	cat("\n")
	if (bf2 / bf1 <= 1.05) {
	    last.s.tune <- max(7, prud2.s.tune)
	} else {
	    last.s.tune <- prud2.s.tune
	}
	bf.ratio <- (bf2 / bf1) ^ (1.0 / as.numeric(last.s.tune))
	res <- PRUD2_optimal_reg(bf1, bf2, last.s.tune, res1, res2, res.click.max)
	while (TRUE) {
	    res.sorted <- arrange(res, desc(click))
	    res.click.max <- res.sorted$click[1]
	    if (res.click.max >= res.clkmax.so.far) break
	
	    # the narrowed down range does not hit the previously max value so far, increase the number of sections and re-run
	    last.s.tune <- last.s.tune * 2 + 1

        # deal with the grow-too-large tune section problem
        if (last.s.tune >= 31) {
            bf.ratio <- (bf.clkmax.so.far / bf1) ^ (1.0 / as.numeric(floor(last.s.tune / 2)))
            bf2.srch <- bf.clkmax.so.far
            s.tune.adj <- 0
            while (bf2.srch < bf2) {
                bf2.srch <- bf2.srch * bf.ratio
                s.tune.adj <- s.tune.adj + 1
            }
            last.s.tune <- floor(last.s.tune / 2) + s.tune.adj
            if (bf2.srch != bf2) res2 <- NULL
            bf2 <- bf2.srch
        } else {
	    	bf.ratio <- (bf2 / bf1) ^ (1.0 / as.numeric(last.s.tune))
	}
	
	    res <- PRUD2_optimal_reg(bf1, bf2, last.s.tune, res1, res2, res.clkmax.so.far)
	}
	last.range <- c(bf1, bf2)
	res.clkmax.so.far <- max(res.click.max, res.clkmax.so.far)
    bf.clkmax.so.far <- bf1 * (bf.ratio ^ (which(res$click == res.clkmax.so.far) - 1))
    }
}

cat(sprintf("\nADID %s BDIV %d Optimal BF: %E ~ %E (%d), run count = %d\n", ADID, BDGT_DIV, bf.opt1, bf.opt2, res.click.max, g.run.cnt))

cat("\nBegin to try alternative lift values...\n")
LIFT.backup <- LIFT
LIFT.best <- -Inf
LIFT.best.click <- 0
LIFT.best.bdgtlft <- 0
for (l.adj in seq(-60, 180, 10)) {
#for (l.adj in seq(0, 100, 1)) {
    LIFT <- LIFT.backup + l.adj
    if (LIFT > 300) next
    res <- PRUD2_optimal_reg(bf.opt2, bf.opt2, 0, show.title = FALSE)
    show_algo_res(res)
    if (res$click[1] > LIFT.best.click | (res$click[1] == LIFT.best.click & res$budget.left[1] >= LIFT.best.bdgtlft)) {
        LIFT.best <- LIFT
        LIFT.best.click <- res$click[1]
        LIFT.best.bdgtlft <- res$budget.left[1]
    }
}
for (LIFT in seq(LIFT.best - 9, LIFT.best + 9, 1)) {
    if (LIFT > 300) next
    res <- PRUD2_optimal_reg(bf.opt2, bf.opt2, 0, show.title = FALSE)
    show_algo_res(res)
    if (res$click[1] > LIFT.best.click | (res$click[1] == LIFT.best.click & res$budget.left[1] >= LIFT.best.bdgtlft)) {
        LIFT.best <- LIFT
        LIFT.best.click <- res$click[1]
        LIFT.best.bdgtlft <- res$budget.left[1]
    }
}

LIFT <- LIFT.backup
cat(sprintf("ADID %s BDIV %d Best LIFT %d, click %d\n", ADID, BDGT_DIV, LIFT.best, LIFT.best.click))
#stopifnot(FALSE) # perfect WP experiment
update_benchmark_parm(ADID, BDGT_DIV, list(BF = c(bf.opt1, bf.opt2)), "BF")
#update_benchmark_parm(ADID, BDGT_DIV, list(BF.C = res.click.max), "BF.C")
#update_benchmark_parm(ADID, BDGT_DIV, list(LIFT = LIFT), "LIFT")
update_benchmark_parm(ADID, BDGT_DIV, list(BF.C = LIFT.best.click), "BF.C")
update_benchmark_parm(ADID, BDGT_DIV, list(LIFT = LIFT.best), "LIFT")
