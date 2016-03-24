
# Survey of distinctive R features
# CPSC 420 Spring 2016

# You can always use ls() and rm() to see/remove objects in your environment

# Can use = or <- for assignment
x=4
x<-5

# It's a vector already
x[2]=6
x[5]=4

# Use is.na() to check for NA values
is.na(x)

# Use "which()" to get index numbers that evaluate to TRUE
which(is.na(x))
which(x == 4)

# any() and all() are nice shortcuts
any(is.na(x))
all(is.na(x))

# You can check...and *set*, a vector's length
length(x)
length(x)=3

# c(), rep(), seq(), and ":" are four common ways to make quick vectors 
y = c(4,5,6)
y = seq(3,9,2)
y = rep(15,6)
y = 15:20
y = c(4,5,seq(9,3,-2),rep(4,9),7)

# Vectorized functions: mean,median,max,min,sd,sum,var,prod
mean(y)
max(x)
max(x,na.rm=TRUE)    # To tell R to just ignore the NAs in the calculation

# Vectorized operations
a = 5
b = a^2 - 2
a < 8
a = c(5,7,3)
b = a^2 - 2
a < 8
a < b

# logical vectors: &, |, !
(a < b  &  a > 4)  |  a %% 2 == 0

# Can specify parameters by name instead of by position
seq(from=3, to=1)

# character data (strings)
x = "Joe"
y = "Blow"
paste(x,y)
paste(x,y,sep="")
paste(x,y,sep="KAWABUNGA")

paste("Harry Potter Episode", c(1:6,paste("7",c("a","b"))))

# index vectors
x = c(1,4,6,3,7)
x[c(2,3,4)]
x[x<5]

# stitching stuff together (e.g., to insert or delete from a vector)
x <- c(x[1:2],17,x[3:5])    # Insert a 17 at position 3

# Matrices. Can use the matrix() function, or just flat set the dim() to a
# 2-dimensional value
bob = matrix(1:12,nrow=3)
dim(bob) = c(6,2)
is.matrix(bob)

# Arrays. Can use the array() function, or just flat set the dim() to an
# n-dimensional value
sue = array(1:12,dim=c(2,2,3))
is.matrix(sue)
is.array(sue)

# "Parallel arrays/vectors" (usually better to use a data frame)
names = c("Bob","Sam","Jane","Pete","Betty")
ages = c(32,21,18,35,45)
genders = c("male","male","female","male","female")

# Sort, by diff criteria
sort(names)
sort(ages)
sort(ages,decreasing=TRUE)
order(ages)     # Stare at this output. It's the indices of the vector you
                # would retrieve in order to sort it.
names[order(ages)]
paste(names[order(ages)],"(who is ",ages[order(ages)], " years old)",sep="")

# lists, data.frames
some.list = list(5,T,c(5,2),"dude")   # lists can be heterogeneous

# [] vs [[]]
some.list[2:3]    # Get a sublist with just elements 2 and 3
some.list[2]      # Get a sublist with just element 2
some.list[[2]]    # Extract element 2 itself (not a list with element 2)

# A data frame is just a list of equal-length vectors.
peeps = data.frame(names,ages,genders,stringsAsFactors=FALSE)
peeps[2:3]                # Access like a list
peeps[c(1,3,4),c(1,3)]    # Access like a matrix
peeps$ages                # Extract one column

# str() is a *great* function for showing you the structure of any object
str(x)
str(sue)
str(peeps)



####################### R vs. Java/C++ idioms ##############################

# Form list of people who'll get discounts -- the Java way:
discounts = vector()
numD = 0
for (i in 1:length(names)) {
    if (ages[i] > 30 || genders[i] == "female") {
        numD = numD + 1
        discounts[numD] = names[i]
    }
}

# Form list of people who'll get discounts -- the R way:
discounts = names[ages > 30 | genders == "female"]


# Similarly: get total ticket price -- the Java way:
totalPrice = 0
for (i in 1:length(names)) {
    if (ages[i] > 30 || genders[i] == "female") {
        totalPrice = totalPrice + 10
    } else {
        totalPrice = totalPrice + 20
    }
}
totalPrice

# Similarly: get total ticket price -- the R way:
total.price = sum(ifelse(ages > 30 | genders == "female",10,20))


salutations = ifelse(genders == "female",paste("Ms.",names),paste("Mr.",names))

#I/O:
name = readline(prompt="What be ye called? ")
age = readline(prompt="How old are you? ")


# plotting & stuff
x = seq(0,100,.1)
func = function(x) x^2 - 20*x + 6
y = func(x)
noise = rnorm(length(x),mean=0,sd=10)   # "normally distributed noise"
y = y + noise
plot(x,y,type="l")  # xlim/ylim/xlab/ylab/main. col/lwd/lty

# use points() and lines() to add to the existing graph

bestline = lm(y~x)
abline(bestline)
legend(legend=c("thing1","thing2","thing3"),fill=c("red","blue","green"),x="topright")

par(mfrow=c(2,3))
