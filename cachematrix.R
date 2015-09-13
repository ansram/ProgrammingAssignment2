#Coursera - R Programming
#Programming assignment 2

# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly.

# makeCacheMatrix creates a list containing a function to 
# Set the value of the matrix 
# Get the value of the matrix 
# Set the value of inverse of the matrix 
# Get the value of inverse of the matrix 


makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { 
     inv <- x$getinverse() 
     if(!is.null(inv)) { 
         message("getting cached data.") 
           return(inv) 
     }
     data <- x$get() 
     inv <- solve(data) 
     x$setinverse(inv) 
     inv 
} 

#try a sample run here

#create a matrix
mat = matrix(c(1,1,2,3,1,3,1,2,4),nrow=3,ncol=3)
mat1 = makeCacheMatrix(mat)
mat1$get()

#      [,1] [,2] [,3]
#[1,]    1    3    1
#[2,]    1    1    2
#[3,]    2    3    4

#first time access, cache is not used
cacheSolve(mat1)

#      [,1] [,2] [,3]
#[1,]    2    9   -5
#[2,]    0   -2    1
#[3,]   -1   -3    2

#now read inverse from cache.
cacheSolve(mat1)

#getting cached data.
#      [,1] [,2] [,3]
#[1,]    2    9   -5
#[2,]    0   -2    1
#[3,]   -1   -3    2

