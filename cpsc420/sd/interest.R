
# Meadows ch. 1 -- interest-bearing bank account (Fig. 12)

# Given an interest rate and an initial balance, return a data frame
# containing its balance over time.
#
# init.balance ($)
# annual.interest.rate (frac/year)
# amortize.period (months)
# sim.length (months)
#
interest.sim <- function(init.balance=10e3, annual.interest.rate=.01, 
    amortize.period=1, sim.length=5*12) {

    delta.t <- amortize.period
    init.time <- 0

    time <- seq(init.time,init.time+sim.length,delta.t)   # month

    balance <- vector(length=length(time))   # $
    balance[1] <- init.balance

    # For the annual interest rate specified, compute the equivalent "per
    # amortization period" interest rate.
    # First, since we're doing this all in months, just convert to months.
    # (1 + ar) = (1 + mr)^12,  so  mr = (1 + ar)^(1/12) - 1
    monthly.rate <- (1 + annual.interest.rate)^(1/12) - 1

    # Now, convert the monthly rate to the proper period rate.
    # (1 + mr) = (1 + pr)^ap,  so  pr = (1 + mr)^(1/ap) - 1
    amortization.period.rate <- (1 + monthly.rate)^amortize.period - 1

    for (step in 2:length(time)) {

        balance.prime <- balance[step-1] * amortization.period.rate / delta.t

        balance[step] <- balance[step-1] + balance.prime * delta.t
    }

    return(data.frame(time=time,balance=balance))
}


# Given the results of interest.sim(), plot the balance over time.
plot.interest <- function(sim.results, annual.interest.rate="1%",
    amortize.period="monthly") {

    plot(sim.results$time, sim.results$balance, xlab="time (mo.)",
        ylab="$", type="l", col="darkgreen", lwd=2,
        main=paste("Balance for ", annual.interest.rate, "APR compounded", 
            amortize.period))
    text(max(sim.results$time),max(sim.results$balance),
        labels=paste0("$",round(max(sim.results$balance),2)),
        col="darkgreen")
}
