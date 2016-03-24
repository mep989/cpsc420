
# Meadows ch. 1 -- reinvestment in capital (Fig. 14)

# Given an initial amount of capital, and a fraction of economic output to
# reinvest, return a data frame containing the capital and profit over time.
#
# init.capital ($)
# fraction.of.output.invested (frac/year)
# sim.length (years)
# prev.results -- a data frame of previous results, if this new call should
#   add to a previously running sim (set to NULL for a new sim). In this case,
#   init.capital is irrelevant. The sim.length is how much *additional* 
#   time to run the sim.
#
reinvestment.sim <- function(init.capital=10e3, 
    fraction.of.output.invested=.20, sim.length=10, prev.results=NULL) {

    if (is.null(prev.results)) {
        init.profit <- 0
        init.time <- 0
    } else {
        num.rows <- nrow(prev.results)
        init.time <- prev.results[num.rows,"time"]
        init.capital <- prev.results[num.rows,"capital"]
        init.profit <- prev.results[num.rows,"profit"]
    }

    delta.t <- .1    # years
    time <- seq(init.time,sim.length+init.time,delta.t)   # years

    baseline.return.on.investment <- .1      # 1/yr
    capital <- vector(length=length(time))   # $
    capital[1] <- init.capital
    profit <- vector(length=length(time))    # $
    profit[1] <- init.profit

    for (step in 2:length(time)) {

        capital.prime <- capital[step-1] * baseline.return.on.investment *
            fraction.of.output.invested

        profit[step] <- capital[step-1] * (1 - fraction.of.output.invested)
        capital[step] <- capital[step-1] + capital.prime * delta.t
    }

    return(rbind(prev.results,
        data.frame(time,capital,profit)))
}


# Given the results of interest.sim(), plot the balance over time.
plot.reinvestment <- function(sim.results) {

    all.values <- c(sim.results$profit, sim.results$capital)
    plot(sim.results$time, sim.results$capital, xlab="time (yrs)",
        ylab="$", type="l", col="brown", lwd=2, lty="dashed",
        main=paste("Economic production with reinvestment"),
        ylim=range(all.values))
    lines(sim.results$time, sim.results$profit, col="green", lwd=2)
    delta.t <- sim.results$time[2] - sim.results$time[1]
    total.profit <- sum(sim.results$profit) * delta.t
    text(par("usr")[2]-.5,0,
        labels=paste0("Avg profit/yr: $",
            round(total.profit/max(sim.results$time))),
        col="darkgreen", adj=c(1,NA))
    legend("topleft",legend=c("capital","profit"),fill=c("brown","green"))
}
