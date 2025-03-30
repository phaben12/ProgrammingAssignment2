## Put comments here that give an overall description of what your functions do
## A pair of functions that cache the inverse of a matrix


## Write a short comment describing this function
## Creates a special matrix object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse<- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        }




## Write a short comment describing this function

## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
        }




phil <- makeCacheMatrix(matrix(1:4, 2, 2))
phil$get()
cacheSolve(phil)
phil$getInverse()
phil$set(matrix(c(2, 2, 1, 4), 2, 2))
phil$get()
phil$getInverse()
cacheSolve(phil)
phil$getInverse()










