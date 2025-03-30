## Put comments here that give an overall description of what your functions do
## A pair of functions that cache the inverse of a matrix


## Write a short comment describing this function
## Creates a special matrix object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<-solve
  getinv <- function() m
  list(set =set, get = get, setinv = setinv, getinv=getinv)
}




## Write a short comment describing this function

## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinv()
        if(!is.null(m)){
              message("getting cached data")
              return(m)
        }
        data <-x$get()
        m <-solve(data, ...)
        x$setinv(m)
        m
}





