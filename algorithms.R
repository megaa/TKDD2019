algorithm_PERFECT <- function(budget, bicc) {
    cat("PERFECT: ")
    total.cost <- 0
    num.click <- 0

    bicc <- bicc %>% filter(R_CLK > 0) %>% mutate(C.Cost = PayingPrice / R_CLK) %>% arrange(C.Cost)
    bicc <- data.table(bicc)  # !!! convert it back to data.table !!!

    for (i in 1:nrow(bicc)) {
	if (bicc[i]$PayingPrice <= budget) {
	    budget <- budget - bicc[i]$PayingPrice
	    total.cost <- total.cost + bicc[i]$PayingPrice
            num.click <- num.click + bicc[i]$R_CLK
	}
    }

    list(total.cost = total.cost, achieved.num.click = num.click, budget.left = budget) 
}

GET_C_PARM <- function(name, parm) {
    switch(parm,
        "THRES" = G.PRED.DATA$thres[G.PRED.DATA$name == name],
	"MEAN" =  G.PRED.DATA$mean[G.PRED.DATA$name == name],
	"SD" =  G.PRED.DATA$sd[G.PRED.DATA$name == name])
}

click_predictor_PERFECT <- function(ADID, bicc, isTrain = FALSE, p.clk.cutoff = GET_C_PARM("PERFECT", "THRES"), p.clk.mean = GET_C_PARM("PERFECT", "MEAN"), p.clk.sd = GET_C_PARM("PERFECT", "SD")) {
    pc.file <- sprintf("~/TKDD2019/cache/pred.clk%s_%s.Rds", if (isTrain) ".train" else "", ADID)
    if (file.exists(pc.file)) {
	pred.clk <- readRDS(pc.file)
    } else {
        pred.clk <- sort(rnorm(nrow(bicc), p.clk.mean, p.clk.sd), TRUE)
	saveRDS(pred.clk, pc.file)
    }
    bicc <- bicc %>% mutate(KEY = 1:nrow(bicc))  # add a column "KEY" for restoring the original order later
    bicc <- bicc %>% arrange(desc(R_CLK), desc(PayingPrice))
    for (i in 1:length(bicc$R_CLK)) {
	if (bicc$R_CLK[i] == 0) break
    }
    num.clk <- i - 1
    pred.clk.adj <- p.clk.cutoff - pred.clk[num.clk]
    pred.clk <- pred.clk + pred.clk.adj  # shift values so that they match the designated cutoff value
    pred.clk[pred.clk < 0] = 0  # adjust negative values to 0
    bicc <- bicc %>% mutate(pred_clk = pred.clk)
    bicc %>% arrange(KEY)  # restore the original order
}

click_predictor_parm_PERFECT <- function(ADID, name, isTrain = FALSE) {
    stopifnot(FALSE)  # not ready yet
    0
}

click_predictor_WN <- function(ADID, bicc, isTrain = FALSE) {
    pc.file <- sprintf("~/TKDD2019/db.data/%s.yzx_%s.txt.lr.pred", if (isTrain) "train" else "test", ADID)
    stopifnot(file.exists(pc.file))
    pred.clk <- read.table(pc.file)
    bicc <- bicc %>% mutate(pred_clk = pred.clk$V1)
}

click_predictor_parm_WN <- function(ADID, name, isTrain = FALSE) {
    pc.file <- sprintf("~/TKDD2019/db.data/%s.yzx_%s.txt.lr.pred", if (isTrain) "train" else "test", ADID)
    stopifnot(file.exists(pc.file))
    pred.clk <- read.table(pc.file)
    if (name == "MIN") {
	ret <- min(pred.clk$V1)
    } else if (name == "MAX") {
	ret <- max(pred.clk$V1)
    } else {
	stopifnot(FALSE)
    }
    ret
}

get_predictor_parm <- function(ADID, name, isTrain = FALSE) {
    p.fun <- switch(G.PRED.TO.USE,
	"WN" = click_predictor_parm_WN,
	"PERFECT" = click_predictor_parm_PERFECT
    )

    p.fun(ADID, name, isTrain)
}

bid_ORTB <- function(theta, p.lambda, p.c) {
    ret <- sqrt(p.c * theta / p.lambda + p.c * p.c) - p.c
    ret <- ifelse(ret < 0, 0, ret)
    ret
}

algorithm_ORTB <- function(ADID, budget, p.lambda, p.c, bicc, isTrain = FALSE) {
    cat(sprintf("ORTB with lambda %E: ", p.lambda))
    p.fun <- switch(G.PRED.TO.USE, "PERFECT" = click_predictor_PERFECT, "WN" = click_predictor_WN)
    biccx <- p.fun(ADID, bicc, isTrain)
    total.cost <- 0
    num.click <- 0

#    for (i in 1:nrow(biccx)) {
#	theta <- biccx[i]$pred_clk
#	bid <- bid_ORTB(theta, p.lambda, p.c)
#	if (bid >= biccx[i]$PayingPrice & biccx[i]$PayingPrice <= budget) {
#	    budget <- budget - biccx[i]$PayingPrice
#	    total.cost <- total.cost + biccx[i]$PayingPrice
#	    num.click <- num.click + biccx[i]$R_CLK
#	}
#    }

    biccx <- biccx %>% mutate(bid.value = bid_ORTB(pred_clk, p.lambda, p.c))
    biccx <- biccx %>% mutate(accu.cost = 0, budget.left = budget)
    biccx <- biccx %>% mutate(item.cost = as.integer(bid.value >= PayingPrice) * PayingPrice)
    biccx$accu.cost <- cumsum(biccx$item.cost)
    biccx$budget.left <- biccx$budget.left - biccx$accu.cost
    biccx <- biccx %>% mutate(ach.click = as.integer(item.cost > 0) * R_CLK)
    num.click <- sum(biccx$ach.click[biccx$budget.left >= 0])
    total.cost <- max(biccx$accu.cost[biccx$budget.left >= 0])
    budget <- budget - total.cost
    
    list(total.cost = total.cost, achieved.num.click = num.click, budget.left = budget)
}

get_m <- function(df, model, transpose) {
  hashed.model.matrix(model, df, hash.size = 2^20, transpose = transpose, is.dgCMatrix = FALSE)
}

cutoff_PRUDENT <- function(ldscp, budget) {
    p <- 1
    cost <- 0

    while (p <= 300) {
	cost <- cost + p * ldscp$price.count[p]
	if (cost > budget) break
	p <- p + 1
    }

    p - 1
}

bid_PRUDENT <- function(imp, gwp) {
    bid <- 0 
}

algorithm_WPOL1 <- function(ADID, budget, lift, g.wp, bicc, isWpRdy = FALSE, isTrain = FALSE, isLiftFirst = TRUE, bidProb = 1) {
#    bid.cutoff <- if (cutoff != 0) cutoff else cutoff_PRUDENT(ldscp, budget)
#    pred.to.use <- if (G.PRED.TO.USE == "WN") sprintf("WN%s", ADID) else G.PRED.TO.USE
    if (!isWpRdy) {
	m.am <- readRDS(sprintf("~/TKDD2019/db.data/%s_%s.Rds", if (isTrain) "amtr" else "amte", ADID))
	m.wp <- hashed.model.matrix(~V1, data = data.frame(V1 = c(1,2,3)), hash.size = m.am@Dim[[2]], is.dgCMatrix = FALSE)
	m.wp@i <- m.am@i
	m.wp@p <- m.am@p
	m.wp@x <- m.am@x
	m.wp@Dim <- m.am@Dim
	wp.hat <- g.wp$predict(m.wp)
#    if (isTrain) {
#        wp.err <- readRDS("~/TKDD2019/db.data/wp_err_train.Rds")
#    } else {
#        wp.err <- readRDS("~/TKDD2019/db.data/wp_err_test.Rds")
#    }
#    wp.hat <- bicc$PayingPrice + wp.err	
	bicc <- bicc %>% mutate(pred_wp = wp.hat)

	### L0203 liftor test ### ToDo: make it more modular
	if (FALSE) { #(LIFT == -Inf) {  # varying liftor such as L0203
	    pc.file <- sprintf("~/TKDD2019/cache/L0203_%s.Rds", ADID)
	    stopifnot(file.exists(pc.file))
	    L0203 <- readRDS(pc.file)
	    bicc$pred_wp[bicc$pred_wp < 1] <- 1
	    bicc$pred_wp[bicc$pred_wp > 300] <- 300
	    bicc$pred_wp <- bicc$pred_wp + L0203[as.integer(floor(bicc$pred_wp))]
	    #LIFT <- 0
	}
    }

    if (bidProb != 0) {
        cat(sprintf("WPOL1 lift %4d, bP %7.4f: ", lift, bidProb))
    } else {
        cat(sprintf("WPOL1 lift %4d: ", lift))
    }

    p.fun <- switch(G.PRED.TO.USE, "PERFECT" = click_predictor_PERFECT, "WN" = click_predictor_WN)
    biccx <- p.fun(ADID, bicc, isTrain)
    total.cost <- 0
    num.click <- 0

    if (isLiftFirst) {
        biccx$pred_wp <- biccx$pred_wp + lift
        biccx$pred_wp[biccx$pred_wp > 300] <- 300
        biccx$pred_wp[biccx$pred_wp < 0] <- 0
        lift.on.fly <- 0
    } else {
        lift.on.fly <- lift
    }
    biccx <- biccx %>% mutate(accu.cost = 0, budget.left = budget)
#    biccx <- biccx %>% mutate(item.cost = as.integer(pred_clk / pred_wp >= bf) * PayingPrice)
    if (bidProb != 0) {
        set.seed(1)
        biccx <- biccx %>% mutate(toBid = runif(nrow(biccx), 0, 1))
        biccx <- biccx %>% mutate(item.cost = as.integer(toBid < (1.0/bidProb) & (pred_wp + lift.on.fly) >= PayingPrice) * PayingPrice)
    } else {
        biccx <- biccx %>% mutate(item.cost = as.integer((pred_wp + lift.on.fly) >= PayingPrice) * PayingPrice)
    }
    biccx$accu.cost <- cumsum(biccx$item.cost)
    biccx$budget.left <- biccx$budget.left - biccx$accu.cost
    #if (isTrain) {   # alt-criterion
    if (FALSE) {      # original-criterion
        biccx <- biccx %>% mutate(ach.click = as.integer(item.cost > 0) * pred_clk)
    } else {
        biccx <- biccx %>% mutate(ach.click = as.integer(item.cost > 0) * R_CLK)
    }
    num.click <- as.integer(sum(biccx$ach.click[biccx$budget.left >= 0]))
    total.cost <- max(biccx$accu.cost[biccx$budget.left >= 0])
    budget <- budget - total.cost

    list(total.cost = total.cost, achieved.num.click = num.click, budget.left = budget)
}

prepare_bicc <- function(ADID, lift, g.wp, bicc, isWpRdy, isTrain, isLiftFirst) {
    #bid.cutoff <- if (cutoff != 0) cutoff else cutoff_PRUDENT(ldscp, budget)
    #pred.to.use <- if (G.PRED.TO.USE == "WN") sprintf("WN%s", ADID) else G.PRED.TO.USE
    if (!isWpRdy) {
        m.am <- readRDS(sprintf("~/TKDD2019/db.data/%s_%s.Rds", if (isTrain) "amtr" else "amte", ADID))
        m.wp <- hashed.model.matrix(~V1, data = data.frame(V1 = c(1,2,3)), hash.size = m.am@Dim[[2]], is.dgCMatrix = FALSE)
        m.wp@i <- m.am@i
        m.wp@p <- m.am@p
        m.wp@x <- m.am@x
        m.wp@Dim <- m.am@Dim
        wp.hat <- g.wp$predict(m.wp)
        #if (isTrain) {
        #    wp.err <- readRDS("~/TKDD2019/db.data/wp_err_train.Rds")
        #} else {
        #    wp.err <- readRDS("~/TKDD2019/db.data/wp_err_test.Rds")
        #}
        #wp.hat <- bicc$PayingPrice + wp.err	
        bicc <- bicc %>% mutate(pred_wp = wp.hat)

        ### L0203 liftor test ### ToDo: make it more modular
        if (FALSE) { #(LIFT == -Inf) {  # varying liftor such as L0203
            pc.file <- sprintf("~/TKDD2019/cache/L0203_%s.Rds", ADID)
            stopifnot(file.exists(pc.file))
            L0203 <- readRDS(pc.file)
            bicc$pred_wp[bicc$pred_wp < 1] <- 1
            bicc$pred_wp[bicc$pred_wp > 300] <- 300
            bicc$pred_wp <- bicc$pred_wp + L0203[as.integer(floor(bicc$pred_wp))]
            #LIFT <- 0
        }
    }

    if (isLiftFirst) {
        bicc$pred_wp <- bicc$pred_wp + lift
        bicc$pred_wp[bicc$pred_wp > 300] <- 300
        bicc$pred_wp[bicc$pred_wp < 0] <- 0
    }

    p.fun <- switch(G.PRED.TO.USE, "PERFECT" = click_predictor_PERFECT, "WN" = click_predictor_WN)
    bicc <- p.fun(ADID, bicc, isTrain)

    bicc
}

algorithm_PRUD2 <- function(ADID, budget, lift, g.wp, bf, bicc, isWpRdy = FALSE, isTrain = FALSE, isLiftFirst = TRUE) {
    cat(sprintf("PRUD2 lift %4d, bf %E: ", lift, bf))

    biccx <- prepare_bicc(ADID, lift, g.wp, bicc, isWpRdy, isTrain, isLiftFirst)
    total.cost <- 0
    num.click <- 0
    if (isLiftFirst) lift.on.fly <- 0 else lift.on.fly <- lift

    biccx <- biccx %>% mutate(accu.cost = 0, budget.left = budget)
#    biccx <- biccx %>% mutate(item.cost = as.integer(pred_clk / pred_wp >= bf) * PayingPrice)
    biccx <- biccx %>% mutate(item.cost = as.integer(pred_clk / pred_wp >= bf & (pred_wp + lift.on.fly) >= PayingPrice) * PayingPrice)
    biccx$accu.cost <- cumsum(biccx$item.cost)
    biccx$budget.left <- biccx$budget.left - biccx$accu.cost
    #if (isTrain) {   # alt-criterion
    if (FALSE) {      # original-criterion
        biccx <- biccx %>% mutate(ach.click = as.integer(item.cost > 0) * pred_clk)
    } else {
        biccx <- biccx %>% mutate(ach.click = as.integer(item.cost > 0) * R_CLK)
    }
    num.click <- as.integer(sum(biccx$ach.click[biccx$budget.left >= 0]))
    total.cost <- max(biccx$accu.cost[biccx$budget.left >= 0])
    
    list(total.cost = total.cost, achieved.num.click = num.click, budget.left = budget - total.cost)
}

