
myfunc <- function(avec) {
    avec <- c(avec[1],avec[3:length(avec)])
}

fred <- c(5,7,9,3)
cat("fred has",length(fred),"elements.\n")
myfunc(fred)
cat("fred has",length(fred),"elements.\n")

