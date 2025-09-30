
# Xa and Xb are ind. exponential distribuions with mean of 10 years
# PDF is:
# domain is 0 to INF
# f(t) = 0.2exp(-0.1 * t) - 0.2exp(-0.2 * t)

probFunc <- function(t = 0) {
    return((0.2 * exp(-0.1 * t)) - (0.2 * exp(-0.2 * t)))
}

satelliteLifespan <- function(iterationNum = 1000) {

    x1 <- 1.0 / ( dexp(iterationNum, rate = -0.1))
    estimate <- 0.0
    return(estimate)
}


# --- Find PI using monte-carlo --- #
# PI can be approximated by comparing the ratio of darts thrown in a unit square [0-1] that are within 1 L2 distance of the origin
# The distance can be squared, right? Since the cutoff is r=1 we shouldn't need to square.
#
# Ratio between square [(0, 0), (1, 1)] and 1/4th circle r=1 centered at (0,0) is A = 1 to A = 1/4 PI
# So the ratio of darts thrown in the circle vs the total area is  (1/4 * PI) / 1
# So 

piRoutine <- function(iterationNum = 10000) {
    # seems we need to seed RNG
    set.seed(42)

    # "random uniform" abbreviated to "runif"? "r_unif", "rUnif", or even "randunif" would be so much clearer
    x <- runif(iterationNum, min = 0.0, max = 1.0)
    y <- runif(iterationNum, min = 0.0, max = 1.0)

    # combines x and ys into a new list
    dist2 <- (x * x) + (y * y)

    # filter with "which"
    PositiveDarts <- which(dist2 <= 1.0)

    # count number of positive darts
    numPositive <- length(PositiveDarts)

    # approx PI from ratio
    # needs to be multiplied by 4 because area ratio is r * 2 to 1/4 * PI * r * r; substituting r = 1 ratio becomes -> 1 : 1/4 * PI
    # (so the expected ratio is about 1/4 * PI)
    PI <- 4.0 * (numPositive / iterationNum)
    return(PI)
}

PI_Est <- piRoutine(10000)
print("PI estimation with 10000 iterations: ")
print(PI_Est)