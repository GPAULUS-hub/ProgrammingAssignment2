## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function has been created to be used later in other computations. It caches the matrix
## caching the matrix is interesting to call later the inverse of that matrix in the case we get a NULL result.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv}
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
        }


## Write a short comment describing this function

## the function cacheSolve calls the inverse of a matrix directly from the cache and corrects a potentiel NULL result.

cacheSolve <- function(x, ...){
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
        
        ## Return a matrix that is the inverse of 'x'
## an example of this function can be observed with the following code.

source("makeCacheMatrix.R")
Mymatrix <- makeCacheMatrix(matrix(1:4), nrow = 2, ncol = 2)
Mymatrix$get()
Mymatrix$getInverse() ## You get a NULL result
cacheSolve(Mymatrix) ## You solve the issue
cacheSolve(Mymatrix) ## Writing this line 2 times ensures you that the data is getting cached'
Mymatrix$getInverse() ## Finally you can get the final result without returning a NULL result


