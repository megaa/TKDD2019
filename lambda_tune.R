suppressPackageStartupMessages(library("methods"))
suppressPackageStartupMessages(library("magrittr"))
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("data.table"))

setwd("~/TKDD2019")

source("~/TKDD2019/globalSetting.R")
source("~/TKDD2019/algorithms.R")    # ToDo: remove the hard-coded path

ORTB_optimal_reg <- function(lam1, lam2, N, res1 = NULL, res2 = NULL, halt.a4.max = 0) {
    cat(sprintf("Regression from %E ~ %E (dist %E) with %d values...\n", lam1, lam2, lam2 - lam1, N + 1))
    res <- data.frame(lam = numeric(), click = integer(), budget.left = numeric())
    lam.m <- (lam2/lam1)^(1.0/as.numeric(N))
    lam <- lam1
    cur.max <- 0
    reach.max <- FALSE
    for (i in 0:N) {
	if (i == 0 & !is.null(res1)) {
	    res.ORTB <- res1
	    cat(sprintf("ORTB with lambda %E: ", lam))
	} else if (i == N & !is.null(res2)) {
	    res.ORTB <- res2
	    cat(sprintf("ORTB with lambda %E: ", lam))
	} else {
            res.ORTB <- algorithm_ORTB(ADID, predict.AD$Campaign.Budget, p.lambda = lam, p.c = P.C, bicc.training2nd.AD, isTrain = TRUE)
        }
	show_algo_res(res.ORTB)
	g.run.cnt <<- g.run.cnt + 1
	if (res.ORTB$achieved.num.click > cur.max) cur.max <- res.ORTB$achieved.num.click
	res <- rbind(res, data.frame(lam = lam, click = res.ORTB$achieved.num.click, budget.left = res.ORTB$budget.left, cost = res.ORTB$total.cost))

	if (halt.a4.max != 0 & cur.max == halt.a4.max) reach.max <- TRUE
	if (halt.a4.max != 0 & reach.max & res.ORTB$achieved.num.click < cur.max) { cat(sprintf("Halt after reaching max: %d\n", cur.max)); break }
	lam <- lam * lam.m
    }
    as(res, "data.table")
}


if (interactive()) {
    ADID <- G.ADID 
    BDGT_DIV <- G.BDGT_DIV
    ortb.l1.tune <- G.ORTB.L1.TUNE
    ortb.l2.tune <- G.ORTB.L2.TUNE
    ortb.s.tune <- G.ORTB.S.TUNE
} else {
    args <- commandArgs(trailingOnly = TRUE)
    ADID <- if (!is.na(args[1])) AD.parm$ADID[AD.parm$ADID == args[1]] else G.ADID
    BDGT_DIV <- if (!is.na(args[2])) as.integer(args[2]) else G.BDGT_DIV
    ortb.l1.tune <- if (!is.na(args[3])) as.numeric(args[3]) else G.ORTB.L1.TUNE
    ortb.l2.tune <- if (!is.na(args[4])) as.numeric(args[4]) else G.ORTB.L2.TUNE
    ortb.s.tune <- if (!is.na(args[5])) as.integer(args[5]) else G.ORTB.S.TUNE
}
#P.C  <- AD.parm$P.C[AD.parm$ADID == ADID]
g.wr <- readRDS(sprintf("~/TKDD2019/db.data/gwr_%s.Rds", ADID))
P.C <- as.numeric(g.wr$r$par)

bicc.training2nd.AD <- readRDS(sprintf("~/TKDD2019/db.data/ictr_%s.Rds", ADID))
bicc.training2nd.AD <- bicc.training2nd.AD %>% mutate(R_CLK = as.integer(is_click) * 1)

lam1.minimum <- get_predictor_parm(ADID, "MIN", isTrain = TRUE) * P.C / ((300 + P.C)^2 - P.C^2)
lam2.maximum <- get_predictor_parm(ADID, "MAX", isTrain = TRUE) * P.C / ((1 + P.C)^2 - P.C^2)

overall.AD <- list(Total.Cost = 0, Total.Clk = 0, Total.Clk.Price = 0)
overall.AD$Total.Cost <- sum(bicc.training2nd.AD$PayingPrice)                              # Total cost if we get all imprssions
overall.AD$Total.Clk <- sum(bicc.training2nd.AD$R_CLK)                                     # Total #click from all impressions
overall.AD$Total.Clk.Price <- sum((bicc.training2nd.AD %>% filter(R_CLK > 0))$PayingPrice) # Total cost if we get only all impressions w/ click

predict.AD <- list(
    Campaign.Budget = overall.AD$Total.Cost / BDGT_DIV)  # Budget available for the testing campaign lifetime

cat(sprintf("Lambda tuning ADID = %s, BDGT_DIV = %d\n", ADID, BDGT_DIV))
print(overall.AD)
print(predict.AD)

g.run.cnt <- 0

# check and adjust initial range to be within lam1.minimum ~ lam2.maximum
if (ortb.l1.tune < lam1.minimum) {
    cat(sprintf("Reset lam1 from %E to minimal possible value: %E\n", ortb.l1.tune, lam1.minimum))
    ortb.l1.tune <- lam1.minimum
}
if (ortb.l2.tune < lam1.minimum) {
    cat(sprintf("Reset lam2 from %E to: %E\n", ortb.l2.tune, lam1.minimum * 16))
    ortb.l2.tune <- lam1.minimum * 16
}
if (ortb.l2.tune > lam2.maximum) {
    cat(sprintf("Reset lam2 from %E to maximal possible value: %E\n", ortb.l2.tune, lam2.maximum))
    ortb.l2.tune <- lam2.maximum
}
if (ortb.l1.tune > lam2.maximum) {
    cat(sprintf("Reset lam1 from %E to: %E\n", ortb.l1.tune, lam2.maximum / 16))
    ortb.l1.tune <- lam2.maximum / 16
}

lam.ratio <- (ortb.l2.tune / ortb.l1.tune) ^ (1.0 / ortb.s.tune)
res <- ORTB_optimal_reg(ortb.l1.tune, ortb.l2.tune, ortb.s.tune)
last.range <- c(ortb.l1.tune, ortb.l2.tune)
last.range.array <- data.frame(lam1 = numeric(), lam2 = numeric())

res.sorted <- arrange(res, desc(click))
res.click.max <- res.sorted$click[1]
res.clkmax.so.far <- res.click.max
lam.clkmax.so.far <- ortb.l1.tune * (lam.ratio ^ (which(res$click == res.clkmax.so.far) - 1))
lam.opt1 <- 0
lam.opt2 <- 0
init.shape.OK <- FALSE
ortb.s.tune.bak <- ortb.s.tune
while (lam.opt1 == 0) {
    lam1 <- 0
    lam2 <- 0
    if (!init.shape.OK) {  # check if the initial small-large-small shape encountered
	last.range.array <- rbind(last.range.array, data.frame(lam1 = last.range[1], lam2 = last.range[2])) 
	init.shape.OK <- TRUE
	if (min(which(res$click == res.click.max)) == 1) {  
	    # it is not the shape, but large-small-smaller
	    init.shape.OK <- FALSE
	    if (res$lam[1] / 2 < lam1.minimum) {
                # deal with the extreme left-skewed
		lam1 <- lam1.minimum
	        res1 <- NULL
		lam2 <- lam1 * 16  # this multiplicator should be well chosen (currently based on 2997B2)
		res2 <- NULL
		ortb.s.tune <- ortb.s.tune * 2 + 1
	    } else {
	        lam1 <- res$lam[1] / 2
	        res1 <- NULL
	        lam2.idx <- 1 
	        lam2 <- res$lam[lam2.idx]
	        res2 <- list(total.cost = res$cost[lam2.idx], achieved.num.click = res$click[lam2.idx], budget.left = res$budget.left[lam2.idx])
	    }
	}
	if (max(which(res$click == res.click.max)) == length(res$click)) {
	    # it is not the shape, but smaller-small-large
	    init.shape.OK <- FALSE
	    lam2 <- res$lam[nrow(res)] * 2
	    res2 <- NULL
	    if (lam1 == 0) {
		lam1.idx <- nrow(res) 
		lam1 <- res$lam[lam1.idx]
		res1 <- list(total.cost = res$cost[lam1.idx], achieved.num.click = res$click[lam1.idx], budget.left = res$budget.left[lam1.idx])
	    }
	}
	if (init.shape.OK) {
	    cat("\nInitial shape check passed!\n")
	    ortb.s.tune <- ortb.s.tune.bak
	} else {
	    res.click.max <- 0
	    #if (nrow(match_df(last.range.array, data.frame(lam1 = lam1, lam2 = lam2))) > 0) {
	    if (nrow(merge(data.frame(lam1 = lam1, lam2 = lam2), last.range.array)) > 0 & ortb.s.tune == ortb.s.tune.bak) {
		lam1 <- lam1 / 2
		lam2 <- lam2 * 2
		res1 <- NULL
		res2 <- NULL
	    }
	}
    }
    
    if (init.shape.OK & res.sorted$click[2] != res.click.max) {  # there are no consecutive max values
	lamm.idx <- which(res$click == res.click.max)
	lam1.idx <- lamm.idx - 1
	lam2.idx <- lamm.idx + 1
	lam1 <- if (lam1.idx >= 1) res$lam[lam1.idx] else res$lam[1] / lam.ratio
	res1 <- if (lam1.idx >= 1) list(total.cost = res$cost[lam1.idx], achieved.num.click = res$click[lam1.idx], budget.left = res$budget.left[lam1.idx]) else NULL
	lam2 <- if (lam2.idx <= nrow(res)) res$lam[lam2.idx] else res$lam[nrow(res)] * lam.ratio
	res2 <- if (lam2.idx <= nrow(res)) list(total.cost = res$cost[lam2.idx], achieved.num.click = res$click[lam2.idx], budget.left = res$budget.left[lam2.idx]) else NULL
	res.click.max <- 0
    } else if (init.shape.OK) {  # there are consecutive max values, check if they follow the converge rule
        lamm1.idx <- min(which(res$click == res.click.max))
        lamm2.idx <- max(which(res$click == res.click.max))
        for (i in lamm1.idx:(lamm2.idx - 1)) {
	    if (res$budget.left[i] < 300 & res$budget.left[i + 1] >= 300 & res$budget.left[i + 1] < 600) {  # it converges!
	        lam.opt1 <- res$lam[i]
	        lam.opt2 <- res$lam[i + 1]
	        break
	    }
	    if (res$lam[i + 1] / res$lam[i] < 1.0000005) {  # it already narrows down to a very small range
		lam.opt1 <- res$lam[i]
		lam.opt2 <- res$lam[i + 1]
		break
	    }
	}
	
	# not converge, find the new range
	if (lam.opt1 == 0) {
	    if (res$budget.left[lamm1.idx] >= 300) {
	        lam1.idx <- lamm1.idx - 1
	        lam2.idx <- lamm1.idx
	    } else if (res$budget.left[lamm2.idx] < 300) {
		lam1.idx <- lamm2.idx
		lam2.idx <- lamm2.idx + 1
	    } else {
		for (i in lamm1.idx:(lamm2.idx - 1)) {
		    if (res$budget.left[i] < 300 & res$budget.left[i + 1] >= 300) break
		}
		lam1.idx <- i
		lam2.idx <- i + 1
	        #lam1.idx <- lamm1.idx
	        #lam2.idx <- lamm2.idx
	    }
	    lam1 <- if (lam1.idx >= 1) res$lam[lam1.idx] else res$lam[1] / lam.ratio
	    res1 <- if (lam1.idx >= 1) list(total.cost = res$cost[lam1.idx], achieved.num.click = res$click[lam1.idx], budget.left = res$budget.left[lam1.idx]) else NULL
	    lam2 <- if (lam2.idx <= nrow(res)) res$lam[lam2.idx] else res$lam[nrow(res)] * lam.ratio
	    res2 <- if (lam2.idx <= nrow(res)) list(total.cost = res$cost[lam2.idx], achieved.num.click = res$click[lam2.idx], budget.left = res$budget.left[lam2.idx]) else NULL
	}
    }
    
    # check if the same range is to be run
    if (lam.opt1 == 0) {
	#if (isTRUE(all.equal(last.range, c(lam1, lam2)))) {   this evaluates to TRUE if last.range[1] != lam1 AND last.range[2] != lam2 !!!!!!!!!!!!
	if (identical(last.range, c(lam1, lam2))) {
	    cat("\n!!! Equal range encountered!!!\n")
	    lam.opt1 <- res$lam[1]
	    lam.opt2 <- res$lam[2]
	}
    }

    # check if we got optimal lambda, if not, re-run regression with narrowed down range
    if (lam.opt1 == 0) {
	cat("\n")
	if (lam2 / lam1 <= 1.05) {
	    last.s.tune <- max(7, ortb.s.tune)
	} else {
	    last.s.tune <- ortb.s.tune
	}
	lam.ratio <- (lam2 / lam1) ^ (1.0 / as.numeric(last.s.tune))
	res <- ORTB_optimal_reg(lam1, lam2, last.s.tune, res1, res2, res.click.max)
	while (TRUE) {
	    res.sorted <- arrange(res, desc(click))
	    res.click.max <- res.sorted$click[1]
	    if (res.click.max >= res.clkmax.so.far) break
	
	    # the narrowed down range does not hit the previously max value so far, increase the number of sections and re-run
	    last.s.tune <- last.s.tune * 2 + 1

        # deal with the grow-too-large tune section problem
        if (last.s.tune >= 31) {
            lam.ratio <- (lam.clkmax.so.far / lam1) ^ (1.0 / as.numeric(floor(last.s.tune / 2)))
            lam2.srch <- lam.clkmax.so.far
            s.tune.adj <- 0
            while (lam2.srch < lam2) {
                lam2.srch <- lam2.srch * lam.ratio
                s.tune.adj <- s.tune.adj + 1
            }
            last.s.tune <- floor(last.s.tune / 2) + s.tune.adj
            if (lam2.srch != lam2) res2 <- NULL
            lam2 <- lam2.srch
        } else {
	        lam.ratio <- (lam2 / lam1) ^ (1.0 / as.numeric(last.s.tune))
        }

	    res <- ORTB_optimal_reg(lam1, lam2, last.s.tune, res1, res2, res.clkmax.so.far)
	}
	last.range <- c(lam1, lam2)
	res.clkmax.so.far <- max(res.click.max, res.clkmax.so.far)
    lam.clkmax.so.far <- lam1 * (lam.ratio ^ (which(res$click == res.clkmax.so.far) - 1))
    }
}

cat(sprintf("\nADID %s BDIV %d Optimal Lambda: %E ~ %E (%d), run count = %d\n", ADID, BDGT_DIV, lam.opt1, lam.opt2, res.click.max, g.run.cnt))

update_benchmark_parm(ADID, BDGT_DIV, list(LAM = c(lam.opt1, lam.opt2)), "LAM")
update_benchmark_parm(ADID, BDGT_DIV, list(LAM.C = res.click.max), "LAM.C")
update_benchmark_parm(ADID, BDGT_DIV, list(C = P.C), "C")

