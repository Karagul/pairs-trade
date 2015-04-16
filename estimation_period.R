
library(quantmod)
## Estimation period
from <- "2013-01-01"
to <- "2013-12-31"

env <- new.env()
Symbols <- c("HD", "LOW", "F", "GM", "XOM", "BP", "ECA", "DVN", "CVX", "BAC",
             "MS", "JPM", "GS", "WFM", "SBUX")
getSymbols(Symbols = Symbols, env = env, from = from, to = to)
args <- eapply(env = env, FUN = function(x) {
                              ClCl(x)
                          })[Symbols]
returns <- na.omit(do.call(what = merge, args = args))
colnames(returns) <- Symbols
## convert reutrns to normalized prices
norm.prices <- cumsum(returns)

norm.prices.df <- as.data.frame(norm.prices)
## function to find the average squared deviation of the normalized prices
## for two stocks
stats <- function(TIC1, TIC2) {
    attach(norm.prices.df)
    cat("The average squared deviation is", mean((TIC1 - TIC2)^2), "\n")
    cat("The standard deviation of the squared deviations is:", sd((TIC1 - TIC2)^2),
        "\n")
    cat("Have a nice day. \n")
    detach(norm.prices.df)
}

## run this function by typing g() into the interpreter and it will create a
## .csv file in your working directory with all the pairs and their avg
## squared dev and stdev of squared deviation sorted from lowest avg sq dev
## to highest

g <- function() {
    list <- matrix(0, nrow = dim(norm.prices.df)[2], ncol = dim(norm.prices.df)[2])
    asds <- matrix(0, nrow = dim(norm.prices.df)[2], ncol = dim(norm.prices.df)[2])
    asstdevs <- matrix(0, nrow = dim(norm.prices.df)[2], ncol = dim(norm.prices.df)[2])
    for (i in 1:dim(norm.prices.df)[2]) {
        for (j in 1:dim(norm.prices.df)[2]) {
            list[i, j] <- as.character(paste(names(norm.prices.df)[i], names(norm.prices.df)[j]),
                                       sep = "")
            asds[i, j] <- mean((norm.prices.df[, i] - norm.prices.df[, j])^2)
            asstdevs[i, j] <- sd((norm.prices.df[, i] - norm.prices.df[, j])^2)
        }
    }

    ll <- dim(norm.prices.df)[2]
    tmp1 <- rep(99, times = ll^2)
    tmp2 <- rep(99, times = ll^2)
    tmp3 <- rep(99, times = ll^2)
    for (z in 0:(ll - 1)) {
        tmp1[(z * ll + 1):(z * ll + ll)] <- list[(z + 1), ]
        tmp2[(z * ll + 1):(z * ll + ll)] <- asds[(z + 1), ]
        tmp3[(z * ll + 1):(z * ll + ll)] <- asstdevs[(z + 1), ]
    }
    est.stats <- cbind(tmp1, tmp2, tmp3)
    est.stats <- as.data.frame(est.stats)
    names(est.stats) <- c("Pair", "Avg. Squared. Dev", "StdDev Squared Dev")
    est.stats <- est.stats[est.stats$"Avg. Squared. Dev" != 0, ]
    est.stats <- est.stats[with(est.stats, order(est.stats[, 2])), ]
    est.stats <- est.stats[seq(2, nrow(est.stats), by = 2), ]
    write.csv(est.stats, file = "stats_forall_pairs.csv")
}
