## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## `<<-` is a global assignment operator, meaning that the variable can be assigned in a function yet has a scope in the parent environment. 
## This function returns a list of other functions and inverts a matrix.

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
## This function looks to see if an inverse matrix has already been completed as a result of the `MakeCacheMatrix()`.
## If so, the inverts matrix is returned; if not, this function computes the matrix and stores it in the cache.

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

