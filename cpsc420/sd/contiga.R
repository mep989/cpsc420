
# Making the coffee mug (from coffee.R) an insulated Contiga mug with an air
# layer.
#
contiga.sim <- function(init.coffee.temp=192, room.temp=70, sim.length=2*60, 
    prev.results=NULL) {

    delta.t <- .1   # min
    if (is.null(prev.results)) {
        init.time <- 0
        init.insul.temp <- init.coffee.temp
    } else {
        num.rows <- nrow(prev.results)
        init.time <- prev.results[num.rows,"time"]
        init.coffee.temp <- prev.results[num.rows,"coffee.temp"]
        init.insul.temp <- prev.results[num.rows,"insul.temp"]
    }

    time <- seq(init.time,init.time+sim.length,delta.t)   # min

    coffee.temp <- vector(length=length(time))   # degF
    coffee.temp[1] <- init.coffee.temp
    insul.temp <- vector(length=length(time))   # degF
    insul.temp[1] <- init.insul.temp

    coffee.cooling.factor <- .01   # (degF/min)/degF
    insul.cooling.factor <- .01    # (degF/min)/degF

    for (step in 2:length(time)) {

        coffee.discrepancy <- coffee.temp[step-1] - insul.temp[step-1]
        insul.discrepancy <- insul.temp[step-1] - room.temp

        coffee.temp.prime <- -(coffee.cooling.factor * coffee.discrepancy)
        insul.temp.prime <- -(insul.cooling.factor * insul.discrepancy)

        coffee.temp[step] <- coffee.temp[step-1] + coffee.temp.prime * delta.t
        insul.temp[step] <- insul.temp[step-1] + insul.temp.prime * delta.t
    }

    return(rbind(prev.results[-1,],
        data.frame(time=time,coffee.temp=coffee.temp,insul.temp=insul.temp,
            room.temp=room.temp)))
}

