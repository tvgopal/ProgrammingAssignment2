## Put comments here that give an overall description of what your
## functions do
## This function calculates the inverse of matrix if not already done earlier. If done earlier, it fetches from 
## cache instead of calculating again.

## Write a short comment describing this function
# First set the value of matrix
# get the value of matrix values
# set the value of the inverse of matrix
# get the value of matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) 
        {
                x <<- y
                inverse <<- NULL
        }
        
get <- function() x
setInverse <- function(inverse1) inverse <<- inverse1
getInverse <- function() inverse
list( set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
# This function calculates inverse of matrix. If it finds a value in 
# inv it returns from cache. Else it computes the inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) 
        {
           message("get cache data if exists")
           return(inv)
        }
        invData <- x$get()
        inv <- solve(invData)
        x$setInverse(inv)
        inv
}
## Testing ##
# set values for matrix
# > x = rbind(c(1,2), c(3,4))
# > x
#      [,1] [,2]
# [1,]    1    2
# [2,]    3    4
# > m = makeCacheMatrix(x)
# > m$get()
#      [,1] [,2]
# [1,]    1    2
# [2,]    3    4
# First run does not fetch from cache
# > cacheSolve(m)
#      [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
# Second run fetches from cache
# > cacheSolve(m)
# get cache data if exists
#      [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
