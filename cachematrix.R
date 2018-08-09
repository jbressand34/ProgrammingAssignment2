## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The function makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function()
        x
    setinverse <- function(inv)
        i <<- inv
    getinverse <- function()
        i
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getminverse = getinverse
    )
}


## Write a short comment describing this function

## The function cacheSolve calculates the inverse of
## the special "matrix" created with the above
## function. However, it first checks to see if the
## inverse has already been calculated. If so, it
## gets the inverse from the cache and skips the
## computation. Otherwise, it calculates the inverse
## of the data and sets the value of the inverse in
## the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
