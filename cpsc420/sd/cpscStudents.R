
require(ggplot2)
require(tidyr)

# Stephen's model of the CPSC student flows.

# Given an initial number of freshmen and parameters concerning the curric
# and economy, return a data frame giving the number of students in various
# academic categories over time.
#
# init.freshmen (students)
# economy (GDP in trillions of dollars)
# CPSC.curric.rigor (unitless; 0 = child's play, 1 = brutality)
# admissions.policy (unitless; 0 = only Einsteins admitted, 1 = pulse optional)
# sim.length (years)
# prev.results -- a data frame of previous results, if this new call should
#   add to a previously running sim (set to NULL for a new sim). In this case,
#   init.coffee.temp is irrelevant. The sim.length is how much *additional* 
#   time to run the sim.
#
cpsc.students.sim <- function(init.freshmen=1e3, economy=16,
    CPSC.curric.rigor=.5, admissions.policy=.9, sim.length=20, 
    prev.results=NULL) {

    delta.t <- 1    # year

    # (Setting this before other variables, since it should in fact be based
    # explicitly on init.freshmen, rather than "the number of freshmen in last
    # year's results.")
    baseline.matriculation.pop <- init.freshmen    # st/yr

    if (is.null(prev.results)) {
        init.time <- 2016
        init.undeclared <- 0
        init.declared.other <- 0
        init.CPSC.majors <- 0
        init.CPSC.minors <- 0
    } else {
        num.rows <- nrow(prev.results)
        init.time <- prev.results[num.rows,"time"]
        init.freshmen <- prev.results[num.rows,"freshmen"]
        init.undeclared <- prev.results[num.rows,"undeclared"]
        init.declared.other <- prev.results[num.rows,"declared.other"]
        init.CPSC.majors <- prev.results[num.rows,"CPSC.majors"]
        init.CPSC.minors <- prev.results[num.rows,"CPSC.minors"]
    }

    time <- seq(init.time,init.time+sim.length,delta.t)   # years

    freshmen <- vector(length=length(time))               # st
    freshmen[1] <- init.freshmen
    undeclared <- vector(length=length(time))             # st
    undeclared[1] <- init.undeclared
    declared.other <- vector(length=length(time))         # st
    declared.other[1] <- init.declared.other
    CPSC.majors <- vector(length=length(time))            # st
    CPSC.majors[1] <- init.CPSC.majors
    CPSC.minors <- vector(length=length(time))            # st
    CPSC.minors[1] <- init.CPSC.minors

    freshman.dropout.rate <- .18         # 1/yr
    undeclared.dropout.rate <- .15       # 1/yr
    declared.other.dropout.rate <- .05   # 1/yr
    declare.other.rate <- .05            # 1/yr
    baseline.career.driven.convert.rate <- .05     # (st/yr)/(trill$)
    graduation.rate <- .25               # 1/yr. Assume same for all majors.
    baseline.scaleback.rate <- .20       # 1/yr

    # The "location" in the logistic function that maps to .5; i.e., the
    # number of CPSC majors that would call for half of undeclared students to
    # declare CPSC.
    baseline.interest.driven.convert.thresh <- 200   # students

    for (step in 2:length(time)) {

        # 1. Compute derivatives for this time step.
        #    a. Preliminary derivatives.
        freshman.matriculation.prime <- 
            baseline.matriculation.pop * admissions.policy
        freshman.dropout.prime <- freshman.dropout.rate * freshmen[step-1]
        undeclared.dropout.prime <- undeclared.dropout.rate *
            undeclared[step-1]
        declared.other.dropout.prime <- declared.other.dropout.rate *
            declared.other[step-1]
        interest.driven.convert.prime <- 
            plogis(CPSC.majors[step-1],
                   location=baseline.interest.driven.convert.thresh) *
            (1 - CPSC.curric.rigor) * undeclared[step-1]
        if (economy > 20) {
            # The economy's so great no one majors in CPSC for that reason.
            career.driven.convert.prime <- 0
        } else {
            career.driven.convert.prime <- 
                baseline.career.driven.convert.rate * freshmen[step-1] *
                (20 - economy) 
        }
        scaleback.prime <- baseline.scaleback.rate * CPSC.curric.rigor *
            CPSC.majors[step-1]
        declared.other.graduate.prime <- graduation.rate * 
            declared.other[step-1]
        CPSC.majors.graduate.prime <- graduation.rate * 
            CPSC.majors[step-1]
        CPSC.minors.graduate.prime <- graduation.rate * 
            CPSC.minors[step-1]
        declare.other.prime <- declare.other.rate * undeclared[step-1]

        # Since freshmen always move on, we make everyone advance who doesn't
        # do something else.
        advance.prime <- freshmen[step-1]/delta.t +
            -freshman.dropout.prime +
            -career.driven.convert.prime

            
        #    b. Final derivatives for each stock.
        freshmen.prime <- freshman.matriculation.prime +
            -career.driven.convert.prime +
            -advance.prime +
            -freshman.dropout.prime
        undeclared.prime <- advance.prime +
            -undeclared.dropout.prime +
            -declare.other.prime +
            -interest.driven.convert.prime
        declared.other.prime <- declare.other.prime +
            -declared.other.dropout.prime +
            -declared.other.graduate.prime
        CPSC.majors.prime <- career.driven.convert.prime +
            interest.driven.convert.prime +
            -scaleback.prime +
            -CPSC.majors.graduate.prime
        CPSC.minors.prime <- scaleback.prime +
            -CPSC.minors.graduate.prime

        # 2. Compute new stock values.
        freshmen[step] <- freshmen[step-1] + freshmen.prime * delta.t
        undeclared[step] <- undeclared[step-1] + undeclared.prime * delta.t
        declared.other[step] <- declared.other[step-1] + 
            declared.other.prime * delta.t
        CPSC.majors[step] <- CPSC.majors[step-1] + CPSC.majors.prime * delta.t
        CPSC.minors[step] <- CPSC.minors[step-1] + CPSC.minors.prime * delta.t

        # 3. Round to make these values discrete.
        freshmen[step] <- round(freshmen[step])
        undeclared[step] <- round(undeclared[step])
        declared.other[step] <- round(declared.other[step])
        CPSC.majors[step] <- round(CPSC.majors[step])
        CPSC.minors[step] <- round(CPSC.minors[step])
    }

    return(rbind(prev.results,
        data.frame(time,freshmen,undeclared,declared.other,CPSC.majors,
        CPSC.minors))
    )
}


# Given the results of interest.sim(), plot the balance over time.
plot.cpsc <- function(sim.results) {

    tidy.results <- gather(sim.results, student.type, num,
        freshmen:CPSC.minors)
    p <- ggplot(tidy.results, aes(x=time, y=num, col=student.type)) + 
        geom_line(size=1) + 
        xlab("year") + ylab("# students") + labs(color="Student type") +
        scale_color_manual(limits=c("freshmen",
                                      "undeclared",
                                      "declared.other",
                                      "CPSC.majors",
                                      "CPSC.minors"),
                             labels=c("Freshmen",
                                      "Undeclared",
                                      "Other major",
                                      "CPSC majors",
                                      "CPSC minors"),
                            values=c("green","gray","orange","red","pink"))
    print(p)
}
