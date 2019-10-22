loginfo <- function(fmt, ...) {
  cat(sprintf("(%s) ", Sys.time()))
  cat(sprintf(fmt, ...))
}

parse_timestamp <- function(str) {
  stopifnot(nchar(str) == 17)
  base <- strptime(substring(str, 1, 14), "%Y%m%d%H%M%S")
  ms <- as.numeric(substring(str, 15, 17)) * 1e-3
  base + ms
}

linear_regression <- function(m, y, lambda2 = 1000, start = rep(0.0, nrow(m))) {
  f <- function(w) {
    sum((w %*% m - y)^2) + lambda2 * sum(tail(w, -1)^2) / 2
  }
  g <- function(w) {
    2 * (m %*% (w %*% m - y)) + lambda2 * c(0, tail(w, -1))
  }
  r <- optim(start, f, g, method = "L-BFGS-B", control = list(
  maxit = ifelse(interactive(), 100, 20000)
  , trace = 3#ifelse(interactive(), 1, 0)
  ))
  list(predict = function(m) r$par %*% m, r = r)
}

censored_regression2 <- function(m, y, is_win, sigma, lambda2 = 1, start = rep(0.0, nrow(m))) {
  f.w <- function(w) {
    z <- (w %*% m - y) / sigma
    - (sum(dnorm(z[is_win], log = TRUE)) + sum(pnorm(z[!is_win], lower.tail = TRUE, log.p = TRUE))) + lambda2 * sum(w^2) / 2
  }
  g.w <- function(w) {
    z <- (w %*% m - y) / sigma
    z.observed <- dzdl.observed <- z[is_win]
    z.censored <- z[!is_win]
    dzdl.censored <- -exp(dnorm(z.censored, log = TRUE) - pnorm(z.censored, log.p = TRUE))
    dzdl <- z
    dzdl[!is_win] <- dzdl.censored
    (m %*% dzdl) / sigma + w
  }
  r.w <- optim(start, f.w, g.w, method = "L-BFGS-B", control = list(
  maxit = ifelse(interactive(), 100, 20000)
  , trace = 3#ifelse(interactive(), 1, 0)
  ))
  list(predict = function(m) r.w$par %*% m, r = r.w)
}

winrate_regression <- function(bid, win, start = 0.0, useNULL = FALSE) {
    f <- function(c) {
	sum((bid / (c[1] + bid) - win)^2)
    }
    g <- function(c) {
        sum(2.0 * (bid / (c + bid) - win) * (-1 * bid / ((c + bid)^2)))
    }
    r <- optim(start, f, if (useNULL) NULL else g, method = "L-BFGS-B", control = list(
    	maxit = 20000
	, trace = 3
    ))
    list(predict = function(bid) (bid / (r$par + bid)), r = r)
}

show_algo_res <- function(res) {
    cat(sprintf("Clk %d Cost %d BLeft %.2f\n", res$achieved.num.click, as.integer(res$total.cost), res$budget.left))
}

update_benchmark_parm <- function(ADID, BDIV, parm, name, fpath = "") {
    parm.obj.file <- if (fpath != "") sprintf("%sparm_%sB%d.Rds", fpath, ADID, BDIV) else sprintf("~/TKDD2019/cache/parm_%sB%d.Rds", ADID, BDIV)
    if (file.exists(parm.obj.file)) {
	parm.obj <- readRDS(parm.obj.file)
    } else {
	parm.obj <- list(
	    ADID = ADID,
	    BDIV = BDIV,
	    BF = as.numeric(c(0,0)),
	    BF.C = as.integer(0),
	    LIFT = as.numeric(0),
	    LAM = as.numeric(c(0,0)),
	    LAM.C = as.integer(0),
	    C = as.numeric(0),
	    RES.P = list(total.cost = as.numeric(0), achieved.num.click = as.integer(0), budget.left = as.numeric(0)),
	    RES.O = list(total.cost = as.numeric(0), achieved.num.click = as.integer(0), budget.left = as.numeric(0)))
    }

    switch(name, 
	"BF" = parm.obj$BF <- parm$BF,
	"BF.C" = parm.obj$BF.C <- parm$BF.C,
	"LIFT" = parm.obj$LIFT <- parm$LIFT,
	"LAM" = parm.obj$LAM <- parm$LAM,
	"LAM.C" = parm.obj$LAM.C <- parm$LAM.C,
	"C" = parm.obj$C <- parm$C,
	"RES.P" = parm.obj$RES.P <- parm$RES.P,
	"RES.O" = parm.obj$RES.O <- parm$RES.O)

    switch(name,
	"BF" = cat(sprintf("BF updated to %E %E\n", parm$BF[1], parm$BF[2])),
	"BF.C" = cat(sprintf("BF.C updated to %d\n", parm$BF.C)),
	"LIFT" = cat(sprintf("LIFT updated to %d\n", parm$LIFT)),
	"LAM" = cat(sprintf("LAM updated to %E %E\n", parm$LAM[1], parm$LAM[2])),
	"LAM.C" = cat(sprintf("LAM.C updated to %d\n", parm$LAM.C)),
	"C" = cat(sprintf("C updated to %f\n", parm$C)) #,
	#"RES.P" = cat(sprintf("RES.P updated to Click %d, Total.Cost %d, Budget.Left %.2f\n", parm$RES.P$achieved.num.click, parm$RES.P$total.cost, parm$RES.P$budget.left)),
	#"RES.O" = cat(sprintf("RES.O updated to Click %d, Total.Cost %d, Budget.Left %.2f\n", parm$RES.O$achieved.num.click, parm$RES.O$total.cost, parm$RES.O$budget.left))
    )

    saveRDS(parm.obj, parm.obj.file)
}

get_benchmark_parm <- function(ADID, BDIV, name, fpath = "") {
    parm.obj.file <- if (fpath != "") sprintf("%sparm_%sB%d.Rds", fpath, ADID, BDIV) else sprintf("~/TKDD2019/cache/parm_%sB%d.Rds", ADID, BDIV)
    if (file.exists(parm.obj.file)) {
	parm.obj <- readRDS(parm.obj.file)

	res <- switch(name, 
	    "BF" = parm.obj$BF,
	    "BF.C" = parm.obj$BF.C,
	    "LIFT" = parm.obj$LIFT,
	    "LAM" = parm.obj$LAM,
	    "LAM.C" = parm.obj$LAM.C,
	    "C" = parm.obj$C,
	    "RES.P" = parm.obj$RES.P,
	    "RES.O" = parm.obj$RES.O)
    } else {
	cat(sprintf("Error!!! Parameter file: %s not exists!!!\n", parm.obj.file))
        res <- NULL
    }

    res
}

show_benchmark_parm <- function(ADID, BDIV, fpath = "") {
    parm.obj.file <- if (fpath != "") sprintf("%sparm_%sB%d.Rds", fpath, ADID, BDIV) else sprintf("~/TKDD2019/cache/parm_%sB%d.Rds", ADID, BDIV)
    if (file.exists(parm.obj.file)) {
        parm.obj <- readRDS(parm.obj.file)
        cat(sprintf("ADID %s BDGT_DIV %d", parm.obj$ADID, parm.obj$BDIV))
        if (fpath != "") cat(sprintf(" (in %s)\n", fpath)) else cat("\n")
        cat(sprintf("Bid Factor: %E %E (%d)\n", parm.obj$BF[1], parm.obj$BF[2], parm.obj$BF.C))
        cat(sprintf("Lift:       %f\n", parm.obj$LIFT))
        cat(sprintf("Click %4d, Total.Cost %8d, Budget.Left %.2f\n", parm.obj$RES.P$achieved.num.click, as.integer(parm.obj$RES.P$total.cost), parm.obj$RES.P$budget.left))
        cat(sprintf("Lambda:     %E %E (%d)\n", parm.obj$LAM[1], parm.obj$LAM[2], parm.obj$LAM.C))
        #cat("sdlfjlkjsd\n")
        cat(sprintf("C:          %f\n", parm.obj$C))
        cat(sprintf("Click %4d, Total.Cost %8d, Budget.Left %.2f\n", parm.obj$RES.O$achieved.num.click, as.integer(parm.obj$RES.O$total.cost), parm.obj$RES.O$budget.left))
    } else {
        cat(sprintf("Error!!! Parameter file: %s not exists!!!\n", parm.obj.file))
    }
}

show_benchmark_parm_CV <- function(ADID, BDIV, fpath = "") {
    cat(sprintf("ADID %s BDGT_DIV %d CV results", ADID, BDIV))
    if (fpath != "") cat(sprintf(" (in %s)\n", fpath)) else cat("\n")
    cat("    Otr     Ptr     Ote     Pte                  Oc          Pc       Oe       Pe\n")
    
    LAM.C <- 0
    BF.C <- 0
    RO.click <- 0
    RP.click <- 0
    RO.cost <- 0
    RP.cost <- 0
    for (i in 1:4) {
        cvs <- sprintf("CV%d", i)
        parm.obj.file <- if (fpath != "") sprintf("%s%s/parm_%sB%d.Rds", fpath, cvs, ADID, BDIV) else sprintf("~/TKDD2019/cache/%s/parm_%sB%d.Rds", cvs, ADID, BDIV)
        if (file.exists(parm.obj.file)) {
	        parm.obj <- readRDS(parm.obj.file)
            cat(sprintf("%4d    %4d    %4d    %4d            %8d    %8d    %8.2f %8.2f\n", 
                        parm.obj$LAM.C,
                        parm.obj$BF.C,
                        parm.obj$RES.O$achieved.num.click,
                        parm.obj$RES.P$achieved.num.click,
                        parm.obj$RES.O$total.cost,
                        parm.obj$RES.P$total.cost,
                        parm.obj$RES.O$total.cost / parm.obj$RES.O$achieved.num.click,
                        parm.obj$RES.P$total.cost / parm.obj$RES.P$achieved.num.click))
            LAM.C <- LAM.C + parm.obj$LAM.C
            BF.C <- BF.C + parm.obj$BF.C
            RO.click <- RO.click + parm.obj$RES.O$achieved.num.click
            RP.click <- RP.click + parm.obj$RES.P$achieved.num.click
            RO.cost <- RO.cost + parm.obj$RES.O$total.cost
            RP.cost <- RP.cost + parm.obj$RES.P$total.cost
        } else {
            cat(sprintf("Error!!! Parameter file: %s not exists!!!\n", parm.obj.file))
        }
    }

    cat(sprintf("%7.2f %7.2f %7.2f %7.2f %6.2f%% %11.2f %11.2f %8.2f %8.2f\n", 
                LAM.C/4.0,
                BF.C/4.0,
                RO.click/4.0,
                RP.click/4.0,
                (RP.click - RO.click) / RO.click * 100.0,
                RO.cost/4.0,
                RP.cost/4.0,
                RO.cost/RO.click,
                RP.cost/RP.click))
}

show_benchmark_parm_CD <- function(ADID, BDIV, fpath = "") {
    cat(sprintf("ADID %s BDGT_DIV %d CD results", ADID, BDIV))
    if (fpath != "") cat(sprintf(" (in %s)\n", fpath)) else cat("\n")
    cat("    Otr     Ptr     Ote     Pte                  Oc          Pc       Oe       Pe\n")
    
    LAM.C <- 0
    BF.C <- 0
    RO.click <- 0
    RP.click <- 0
    RO.cost <- 0
    RP.cost <- 0
    for (i in 1:4) {
        cvs <- sprintf("CD%d", i)
        parm.obj.file <- if (fpath != "") sprintf("%s%s/parm_%sB%d.Rds", fpath, cvs, ADID, BDIV) else sprintf("~/TKDD2019/cache/%s/parm_%sB%d.Rds", cvs, ADID, BDIV)
        if (file.exists(parm.obj.file)) {
	        parm.obj <- readRDS(parm.obj.file)
            cat(sprintf("%4d    %4d    %4d    %4d            %8d    %8d    %8.2f %8.2f\n", 
                        parm.obj$LAM.C,
                        parm.obj$BF.C,
                        parm.obj$RES.O$achieved.num.click,
                        parm.obj$RES.P$achieved.num.click,
                        parm.obj$RES.O$total.cost,
                        parm.obj$RES.P$total.cost,
                        parm.obj$RES.O$total.cost / parm.obj$RES.O$achieved.num.click,
                        parm.obj$RES.P$total.cost / parm.obj$RES.P$achieved.num.click))
            LAM.C <- LAM.C + parm.obj$LAM.C
            BF.C <- BF.C + parm.obj$BF.C
            RO.click <- RO.click + parm.obj$RES.O$achieved.num.click
            RP.click <- RP.click + parm.obj$RES.P$achieved.num.click
            RO.cost <- RO.cost + parm.obj$RES.O$total.cost
            RP.cost <- RP.cost + parm.obj$RES.P$total.cost
        } else {
            cat(sprintf("Error!!! Parameter file: %s not exists!!!\n", parm.obj.file))
        }
    }

    cat(sprintf("%7.2f %7.2f %7.2f %7.2f %6.2f%% %11.2f %11.2f %8.2f %8.2f\n", 
                LAM.C/4.0,
                BF.C/4.0,
                RO.click/4.0,
                RP.click/4.0,
                (RP.click - RO.click) / RO.click * 100.0,
                RO.cost/4.0,
                RP.cost/4.0,
                RO.cost/RO.click,
                RP.cost/RP.click))
}


G.PRED.DATA <- data.frame(
    name =  c("PERFECT", "WN1458",  "WN3358",  "WN3386",   "WN3427", "WN3476"),
    thres = c(0.004,     0.3450875, 0.2196508, 0.05818844, 0.185394, 0.03249527), 
    mean =  c(0.005,     0,         0,         0,          0,        0),
    sd =    c(0.001,     0,         0,         0,          0,        0))

AD.parm <- data.frame(
    ADID = c("1458",    "3358",    "3386",    "3427",    "3476",    "2259",    "2261",    "2821",    "2997"),
    #P.C  = c(33.76,     51.13,     38.66,     42.44,     40.90,     50.40,     47.12,     46.96,     28.45), # <- for original dataset
    #P.C  = c(00.00,     51.14,     00.00,     00.00,     00.00,     50.36,     00.00,     00.00,     00.00),  # <- for 2-1 fixed splitted dataset
    P.C  = c(00.00,     00.00,     00.00,     00.00,     00.00,     50.47,     00.00,     00.00,     00.00),  # <- for 2-1 random splitted dataset
    #LIFT = c(188.0,     161.0,     176.0,     157.0,     165.0))  # <- 99%
    LIFT = c(82.0,      73.0,      92.0,      77.0,      78.0,      118.0,     137.0,     104.0,     107.0))  # <- 90%
    #LIFT = c(-Inf,      -Inf,      -Inf,      -Inf,      -Inf,      -Inf,      -Inf,      -Inf,      -Inf))  # <- varying liftor such as L0203  
    #LIFT = c(18.038121, 34.989813, 18.860934, 19.255725, 10.363717))

#ADID <- #"3358"                 #"1458"                #"3386"
#P.C <-  #51.13                  #33.76                 #38.66
#LIFT <- #163.038462-101.480254  #102.875728-81.721716  #124.004494-94.854700

G.PRED.TO.USE <- "WN" #"PERFECT"

G.ADID     <- "2997"
G.BDGT_DIV <- 64 

G.ORTB.L1.TUNE <- 1E-20 #9.338823E-04 #2E-06
G.ORTB.L2.TUNE <- 1     #9.469768E-04 #3E-03
G.ORTB.S.TUNE  <- 7

G.ORTB.L1  <- 1.948803E-04 
G.ORTB.L2  <- 1.950567E-04
G.ORTB.S   <- 1

G.PRUD2.B1.TUNE <- 1E-20 #2.820559E-05  #1E-06 
G.PRUD2.B2.TUNE <- 1     #2.835114E-05  #1E-03
G.PRUD2.S.TUNE  <- 7
G.PRUD2.L.PCT <- 95  # 0: pick from AD.parm$LIFT[]; 1: pick from the last LIFT value stored in parm file

G.PRUD2.B1 <- 5.236547E-04 #4.367818E-03 
G.PRUD2.B2 <- 5.334523E-04 #4.375334E-03
G.PRUD2.S  <- 1

