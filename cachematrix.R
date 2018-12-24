## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function: 
# set & get matrix; set value of the inverse; get value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Write a short comment describing this function:
# cacheSolve computes inverse of the special “matrix” returned by makeCacheMatrix.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}

# Testing for a matrix  M <- matrix(1:4, 2, 2)
M <- matrix(1:4, 2, 2)
        Answer1 <- makeCacheMatrix(M)
        cacheSolve(Answer1)
        
# Check the answer using solve function in R, solve(M)
solve(M)