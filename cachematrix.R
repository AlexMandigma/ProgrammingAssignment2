##Inverse of a Matrix
##The functions will look for the inverse matrix of an invertible matrix

##We will be using the solve function since 
##Solve-This generic function solves the equation a %*% x = b for x, where b can be either a vector or a matrix.
##https://stat.ethz.ch/R-manual/R-devel/library/base/html/solve.html 
##From the assignment https://github.com/rdpeng/ProgrammingAssignment2 , the mean was edited to Solve to get the answer

##This function creates a special "matrix" object that can cache its inverse

#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse using the Solve function
#4. get the value of the inverse using the solve function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the mean of the special "vector" created with the above function. However, 
##it first checks to see if the mean has already been calculated. 
##If so, it gets the mean from the cache and skips the computation. 
##Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

##Example
##> a <- diag(2,10)
##> invertibleMatrix  <- makeCacheMatrix(a)
##> cacheSolve(invertibleMatrix)
##[,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
##[1,]  0.5  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0   0.0
##[2,]  0.0  0.5  0.0  0.0  0.0  0.0  0.0  0.0  0.0   0.0
##[3,]  0.0  0.0  0.5  0.0  0.0  0.0  0.0  0.0  0.0   0.0
##[4,]  0.0  0.0  0.0  0.5  0.0  0.0  0.0  0.0  0.0   0.0
##[5,]  0.0  0.0  0.0  0.0  0.5  0.0  0.0  0.0  0.0   0.0
##[6,]  0.0  0.0  0.0  0.0  0.0  0.5  0.0  0.0  0.0   0.0
##[7,]  0.0  0.0  0.0  0.0  0.0  0.0  0.5  0.0  0.0   0.0
##[8,]  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.5  0.0   0.0
##[9,]  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.5   0.0
##[10,]  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0   0.5
