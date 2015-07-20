## ProgrammingAssignment2
## LuFo 2015-07-20

## Two functions to demonstrate caching mechanisms via environments
## 1st function combines a list of helper functions to set/get a matrix 
##    and to set/get its inversed (solved) values
## 2nd function is to check existence of cached values and 
##    either to re-use it or to calculate and store it

## Test data see f.e. https://de.wikipedia.org/wiki/Inverse_Matrix
## > m<-matrix(c(2,1,5,3),2,2,T)
## [,1] [,2]
## [1,]    2    1
## [2,]    5    3
## > s<-solve(m)
## [,1] [,2]
## [1,]    3   -1
## [2,]   -5    2

## function makeCacheMatrix stores four functions:
## set: to set x<<-y and clean up m<<-NULL
## get: to get x
## setsolve: to calculate solve s to matrix m
## getsolve: to get s
makeCacheMatrix <- function(m = matrix()) {
    s <- NULL
    set <- function(y) {
        m <<- y
        s <<- NULL
    }
    get <- function() m
    setsolve <- function(solve) s <<- solve ## this <<- writes into 'cache'
    getsolve <- function() s
    ## following stores these four functions in a list
    list(set = set, 
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## function cacheSolve 
## checks with makeCacheMatrix$getsolve if calculated solve is available in cache
## to be called in 2 step approach like:
## 1)  functionvector <- makeCacheMatrix(c(1, 11, 20))
## 2)  cachesolve(functionvector)
## if found: return cached solve
## if not found: calculate solve, write to cache, return
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## take x as argument of vector of functions including $getsolve
    s <- x$getsolve()
    ## try to read cached value with function $getsolve
    if(!is.null(s)) {
        ## if cached value found
        message("read cached data")
    }
    else {
        ## this is executed only if cached value hasn't been found
        data <- x$get()         ## get origin matrix
        s <- solve(data, ...)   ## calculate solve of matrix
        x$setsolve(s)           ## write to cache
        message("calculated solve and written to cache for next usage")
    }
    s                       ## return calculated solve
}

## To execute programm assignment 2 do the following
## 0: > rm(list=ls())
##    > source("ProgrammingAssignment2/cachematrix.R") 
## 1: functionvector <- makeCacheMatrix(matrix(c(2,1,5,3),2,2,T))
##    this sets the matrix but not its solve, check with:
## 2: functionvector$getsolve() returning NULL
## 3: cacheSolve(functionvector)
##    in this first run cache isn't filled and mssage is returned
## 4: cacheSolve(functionvector)
##    in this second (and all following runs) cache is read
## 
##-- Additional test setting cache value manually:
## > functionvector$setsolve(2)
## > cacheSolve(functionvector)
## read cached data
## [1] 2
## > 