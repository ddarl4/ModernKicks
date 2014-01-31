#If you need the population variance 
#(that is, denominator of 1/n instead of 1/(n-1))
n <- length(delay)
(n-1)/n*var(delay)


# how to check the type of an object
data.class(fd)