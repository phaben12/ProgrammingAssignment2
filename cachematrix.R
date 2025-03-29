## Put comments here that give an overall description of what your
## functions do

## to use (if working directory is not set 
## source("C:\\Users\\Philip\\Documents\\coursera\\R-Programming\\ProgrammingAssignment2\\cachematrix.r")
## to use (if working directory is set) 
## source("cachematrix.r")

## create a 2x2 matrix with set values
## qq <- matrix((1:4),nrow=2, ncol=2)  ## unsolvable matrix
## qq <- matrix(rnorm(4),nrow=2, ncol=2) ## solvable matrix creation


## Write a short comment describing this function
## `<<-` is a global assignment operator, meaning that the variable can be assigned in a function yet has a scope in the parent environment. 
## This function returns a list of other functions and inverts a matrix.

makeCacheMatrix <- function(x = matrix()) {
  ## x is a square invertible matrix
  ## Return set/get the special matrix obj with the input matrix 
  ## and its inverse
  
  ## check if x is invertible 
  ##result <- try(determinant(x)$modulus)
  inv = NULL
  if (class(try(solve(x), silent = T)) != "try-error"){
    #print("i'm here!")
    set = function(y){
      x <<-y
      inv <<- NULL
    }
    get = function() x
    setinv = function(inverse) inv <<- inverse 
    getinv = function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
  }else{
    print("x not an invertible matrix")
  }
}


## Write a short comment describing this function
## This function looks to see if an inverse matrix has already been completed as a result of the `MakeCacheMatrix()`.
## If so, the inverts matrix is returned; if not, this function computes the matrix and stores it in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  #get x's invert from cache
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }else{# calculate x's invert
    mat = x$get()
    inv = solve(mat, ...)
    x$setinv(inv)
    return(inv)
  }
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

