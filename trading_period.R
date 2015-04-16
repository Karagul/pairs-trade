library(quantmod)
## Trading period
from <- "2014-01-01"
to <- Sys.Date()

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

## run this function by typing g() into the interpreter and it will create a
## .csv file in your working directory with all the pairs and their daily
## squared deviations for each day in the trading period.

g <- function() {
    ll <- dim(norm.prices.df)[2]
    list <- matrix(0, nrow = ll, ncol = ll)
                                        # create a 3-way array (num stock x num stock x num days)
    sd <- array(0, dim = c(nrow = ll, ncol = ll, dim(norm.prices.df)[1]))
    for (i in 1:ll) {
        for (j in 1:ll) {
            list[i, j] <- as.character(paste(names(norm.prices.df)[i], names(norm.prices.df)[j]),
                                       sep = "")
            sd[i, j, ] <- (norm.prices.df[, i] - norm.prices.df[, j])^2
        }
    }

    tmp1 <- rep(99, times = ll^2)
    tmp2 <- matrix(99, ncol = ll^2, nrow = dim(norm.prices)[1])
    for (z in 0:(ll - 1)) {
        tmp1[(z * ll + 1):(z * ll + ll)] <- list[(z + 1), ]
        tmp2[, (z * ll + 1):(z * ll + ll)] <- t(sd[(z + 1), , ])
    }

    result <- tmp2
    result <- as.data.frame(result)
    names(result) <- tmp1
    write.csv(result, file = "squared_devs.csv")
}
