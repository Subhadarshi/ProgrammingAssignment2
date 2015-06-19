## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" which is a list
## to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. get the value of the inverse
## 4. set the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the the special
## 'matrix' created with the above function.
## However, it first checks if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise it calculates the inverse of the data and sets the value
## of the inverse in the cache via setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
