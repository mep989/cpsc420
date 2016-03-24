
# Aliens v. Vampires -- version 2.1
# (logistic growth with human reproduction)
# CPSC 420 -- spring 2016


# Set up time.
delta.t <- 1   # years
time <- seq(1940,2050,delta.t)

# Utility functions to convert between i and t.
itot <- function(i) (i-1)*delta.t + 1940
ttoi <- function(t) (t-1940)/delta.t + 1

# Simulation parameters.
init.human.pop <- 7e9
alien.abduction.rate <- 3      # (people/year)/year
bite.rate <- .1                # (people/year)/vampire
birth.rate <- .01              # (people/year)/person

A <- vector()
V <- vector()
H <- vector()
earth.population <- vector()

# Initial conditions. (No aliens until 1940, and only one lonely vampire.)
A[1] <- 0
V[1] <- 1
H[1] <- init.human.pop
earth.population[1] <- V[1] + H[1]


# Simulate.
for (i in 2:length(time)) {

    logistic.factor <- H[i-1]/earth.population[i-1]

    # Compute flows.
    A.prime <- alien.abduction.rate * (time[i] - 1940) * 
        logistic.factor   # people/year
    V.prime <- bite.rate * V[i-1] * logistic.factor       # people/year
    H.prime <- H[i-1] * birth.rate -(V.prime + A.prime)    # people/year
    earth.population.prime <- -A.prime    # people/year

    # Compute stocks.
    A[i] <- A[i-1] + A.prime * delta.t                   # people
    V[i] <- V[i-1] + V.prime * delta.t                   # people
    H[i] <- H[i-1] + H.prime * delta.t                   # people
    earth.population[i] <- V[i] + H[i]                   # people
}


# Plot results.
all.values <- c(A,V,H,earth.population)
plot(time,A,type="l",col="green",lwd=2,
    ylim=c(min(all.values),max(all.values)),
    main="Aliens v. Vampires apocalypse -- oh my!!",
    xlab="year",
    ylab="# of victims")
lines(time,V,col="red",lwd=2)
lines(time,H,col="black",lwd=1)
lines(time,earth.population,col="brown",lty="dotted",lwd=3)
legend("topleft",legend=c("Alien abductions","Vampire bites","Humans","Earthlings"),
    fill=c("green","red","black","brown"))

