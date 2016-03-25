## Functions provide a cached version of the solve() function. 
## The user can store a matrix and it's inverse solution in a cache
## Whenever the user calls the cacheSolve function, a check will be made
## to see if the inverse has already been calculated. 
## usage is as follows:
## 1. User creates a new makeCacheMatrix object with a matrix as parameter
## 2. User calls cacheSolve function with object from step 1 as parameter and 
## any other possible parameters which will be passed on to the original solve()
## function. 



## makeCacheMatrix() Creates a list containing the functions for getting and
## setting a matrix and the related inversion. It functions as
## the equivalent of a class in object oriented programming. 

makeCacheMatrix <- function(x = matrix()) 
{
    # x = the matrix that needs to be inverted
    # i = Variable containing the inverse matrix
    i <- NULL
    #When resetting de matrix, the inverse needs to be set to NULL
    setMatrix <- function(y)
    {
        x <<- y
        i <<- NULL
    }
    #Returns the matrix
    getMatrix <- function() x
    #Sets the inverse of the matrix
    setInverse <- function(inverse) i <<- inverse
    #Returns the inverse of the matrix
    getInverse <- function() i 
    #Result is a list with functions
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse, getInverse = getInverse)
}


## Will return the inverse of X, if possible a cached value will
## be supplied. Stores the inverse in the cache if none is present. 

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
    #get the current inverse value
    i <- x$getInverse()
    #If the inverse is already filled, return this value and
    #exit the function
    if(!is.null(i))
    {
        message("Getting cached inverse data")
        return (i)
    }
    message("inverse not cached, calculating....")
    #otherwise get the matrix of x
    data <- x$getMatrix()
    #calculate the inverse of data
    i <- solve(data, ...)
    message("inverse calculated!")
    #store the calculated inverse in x
    x$setInverse(i)
    #return the inverse
    i
}


## Example of a matrix which can be inversed: rbind(c(1, -1/4), c(-1/4, 1))

## URL of an online inverse calculator for verification that the supplied values
## really are the inverse (I'm not a mathematician and needed proof I did the right
## thing) : 
## http://matrix.reshish.com/inverse.php
