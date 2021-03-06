
## In this assignment I try to write a pair of functions that cache the inverse of a matrix.
## For this assignment, assume that the matrix supplied is always invertible.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    ## xi is my inverse matrix that I want to cache
    xi <- NULL

    ## Function to set the matrix (to solve) after I've instantiate a makeCacheMatrix object
    ## This is a closure, a function defined in another function which encloses the
    ## environment of the parent function and can access all its variables
    set <- function(y) {
        ## This syntax never create a variable in the current environment (the 'set' function
        ## defined here) but instead modifies an existing variable found by walking up
        ## the parent environments, in such case the x formal argument given while
        ## calling makeCachematrix
        x <<- y

        ## as above, with "<<-" I modify the xi local variable defined in makeCacheMatrix environment.
        ## By erasing the cached matrix when I assign a new value on an already defined makeCacheMatrix
        ## object, I ensure that the inverse matrix has to be calculated by solve using cacheSolve
        ## So I will call solve on a new matrix object
        xi <<- NULL
    }

    ## a function to get the matrix
    get <- function() x

    ## a function to set the inverse of my matrix
    setsolve <- function(solved) xi <<- solved

    ## a function to get the inverse of my matrix
    getsolve <- function() xi

    ## now return a list with all this functions defined as key(name) = value (function)
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
    ## get inverse of my matrix (invertible as stated by assignment)
    xi <- x$getsolve()

    ## is solve(x) already calculated?
    if (! is.null(xi)) {
        ## yes; returning cached data
        message("getting cached data")
        return(xi)
    }

    ## If I arrive here, xi is NULL and I have to solve my matrix.
    message("Solving matrix and caching data")

    ## Getting matrix to solve:
    data <- x$get()

    ## calculate the inverse of matrix
    xi <- solve(data)

    ## caching data
    x$setsolve(xi)

    ## Return a matrix that is the inverse of 'x'
    return(xi)

}

##
## Testing code with an invertible matrix
##

## Instantiate and invertible matrix
m <- matrix(c(4,3,3,2), nrow=2,ncol=2)

## Instantiate the cached matrix
cached.m <- makeCacheMatrix()

## Set m matrix in cached object
cached.m$set(m)

## But I could also type
cached.m <- makeCacheMatrix(m)

## getting the matrix in cached object
cached.m$get()

## solving matrix for the first time and caching object
mi <- cacheSolve(cached.m)

## get cached object (the inverted matrix)
mi_cached <- cacheSolve(cached.m)

## by multipling a matrix for its inverse I get the identity matrix
i <- m %*% mi_cached

