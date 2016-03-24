
# Meadows ch. 1 -- bathtub model (Fig. 5)

# Given an initial level of water in a bathtub, and parameters indicating how
# it is used, return a data frame of water levels over time. 
#
# inflow.rate from the faucet, when on (gallons/sec)
# outflow.rate down the drain, when unplugged (gallons/sec)
# turn.on.faucet.time -- faucet begins off, then is turned on at this point
#   (sec)
# turn.off.faucet.time -- faucet turns back off at this point (sec)
# pull.plug.time -- drain begins closed, then is unplugged at this point (sec)
# sim.length (min)
#
bathtub.sim <- function(init.water.level.gal=50, turn.on.faucet.time=10,
    turn.off.faucet.time=120, pull.plug.time=40, inflow.rate=15/60,
    outflow.rate=20/60, sim.length=5) {

    delta.t <- 1   # sec
    time <- seq(0,sim.length*60,delta.t)   # sec

    water.level <- vector(length=length(time))   # gallons

    water.level[1] <- init.water.level.gal

    for (step in 2:length(time)) {

        time.now <- time[step]

        water.level.prime <- 0    # gallons/sec
        if (time.now > turn.on.faucet.time &&
            time.now < turn.off.faucet.time) {
            water.level.prime <- water.level.prime + inflow.rate
        }
        if (time.now > pull.plug.time) {
            water.level.prime <- water.level.prime - outflow.rate
        }

        water.level[step] <- 
            max(water.level[step-1] + water.level.prime * delta.t, 0)
    }

    return(list(
        results=data.frame(time=time,water.level=water.level),
        params=list(init.water.level.gal=init.water.level.gal,
                    turn.on.faucet.time=turn.on.faucet.time,
                    turn.off.faucet.time=turn.off.faucet.time,
                    pull.plug.time=pull.plug.time,
                    inflow.rate=inflow.rate,
                    outflow.rate=outflow.rate,
                    sim.length=sim.length)))
}


# Given the results of bathtub.sim(), plot the water level over time.
plot.bathtub.water.level <- function(sim.results) {
    results <- sim.results$results
    max.y <- max(80,results$water.level)
    plot(results$time, results$water.level, xlab="time (sec)",
        ylab="gallons", type="n", ylim=c(0,max.y))
    rect(sim.results$params$turn.on.faucet.time,par("usr")[3],
         sim.results$params$turn.off.faucet.time,par("usr")[4],
         col="lightblue", border=NA)
    lines(results$time, results$water.level, lwd=2, col="blue")
    abline(h=seq(0,max.y,10),col="darkgrey",lty="dashed")
    abline(v=sim.results$params$pull.plug.time,lty="dashed",col="red")
}
