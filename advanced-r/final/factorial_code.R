library(purrr)
library(microbenchmark)


#' Compute factorial in a loop
factorial_loop <- function(n) {
    #' Throw an error if n < 0
    stopifnot(n >= 0)

    #' Define factorial of 0
    if (n == 0) {
        return(1)
    }
    res <- 1
    for (i in 1:n) {
        res <- res*i
    }
    res
}

#' Compute factorial with reduce
factorial_reduce <- function(n) {
    #' Throw an error if n < 0
    stopifnot(n >= 0)

    #' Define factorial of 0
    if (n == 0) {
        1
    }
    else {
        reduce(1:n, `*`)
    }
}

factorial_func <- function(n) {
    #' Throw an error if n < 0
    stopifnot(n >= 0)

    #' Define factorial of 0
    if (n == 0) {
        1
    }
    else {
        n*factorial_func(n - 1)
    }
}


#' Initialize a table to store factorial values
fac_tbl <- c(1, rep(NA, 200))
factorial_mem <- function(n) {
    #' Throw an error if n < 0
    stopifnot(n >= 0)

    #' Define factorial of 0
    if (n == 0) {
        return(1)
    }

    #' If not already computed
    if (is.na(fac_tbl[n])) {
        #' Update the table
        fac_tbl[n - 1] <<- factorial_mem(n - 1)
        fac_tbl[n] <<- n*fac_tbl[n - 1]
    }

    #' At this point the table is already filled up at least to n. Return the stored value
    fac_tbl[n]
}

print(microbenchmark(factorial_loop(1),
                     factorial_reduce(1),
                     factorial_func(1),
                     factorial_mem(1)))

print(microbenchmark(factorial_loop(10),
                     factorial_reduce(10),
                     factorial_func(10),
                     factorial_mem(10)))

print(microbenchmark(factorial_loop(20),
                     factorial_reduce(20),
                     factorial_func(20),
                     factorial_mem(20)))

print(microbenchmark(factorial_loop(50),
                     factorial_reduce(50),
                     factorial_func(50),
                     factorial_mem(50)))

print(microbenchmark(factorial_loop(100),
                     factorial_reduce(100),
                     factorial_func(100),
                     factorial_mem(100)))
