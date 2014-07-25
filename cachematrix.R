## Put comments here that give an overall description of what your
## functions do

## this function creates a special matrix to set and retrieve the value of the matrix, and set and retrieve the value of its inverse (assuming it is invertible) 

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setinverse <- function(solve) m <<- solve
   getinverse <- function() m
   list(set = set, get = get, setinverse = setinverse,
        getinverse = getinverse)
}


## this function calculates the mean of the special "matrix" created with the above function, but first checks to see if the value is stored in the cache. If so, it uses this, otherwise it calculates it normally

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   m <- x$getinverse()
   if(!is.null(m)) {
      message("getting cached data")
      return(m) }
   data <- x$get()
   m <- solve(data, ...)
   x$setinverse(m)
   m
}

a<-matrix(rnorm(1:10000),100,100)  #initialize a large matrix
cacheSolve(makeCacheMatrix(a))    #run the two functions to calculate and retrieve the inverse of the matrix

