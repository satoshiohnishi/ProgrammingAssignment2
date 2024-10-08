# the inverse matrix is defined as a square matrix X that, 
# when multiplied with the original matrix A, 
# results in the identity matrix E. It is expressed as:
# AX=XA=E
# where X is the inverse of A. The inverse matrix is denoted as A^(-1).


# make a new function called makeCacheMatrix that creates a special "matrix",
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# Cache the inverse of a matrix when it is calculated
cacheSolve <- function(x, ...) {
  start_time <- Sys.time()  # Record the start time
  
  inv <- x$getInverse()
  
  # If the inverse is already calculated, return it
  if(!is.null(inv)) {
    end_time <- Sys.time()  # Record the end time
    time_taken <- difftime(end_time, start_time, units = "secs")
    message("Getting cached data...")
    message(sprintf("Run time : %.6f seconds", as.numeric(time_taken)))
    return(inv)
  }
  
  # If the inverse is not already calculated, calculate it
  message("Calculating inverse matrix...")
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  
  end_time <- Sys.time()  # Record the end time
  time_taken <- difftime(end_time, start_time, units = "secs")
  message(sprintf("Run time : %.6f seconds", as.numeric(time_taken)))
  
  inv
}

# Generate 2000 x 2000 matrix
n <- 2000
m <- matrix(runif(n*n), nrow=n, ncol=n)

# Create a new cache matrix
cached_m <- makeCacheMatrix(m)

# Calculate the inverse matrix for the first time
result1 <- cacheSolve(cached_m)

# Call the cached inverse matrix
result2 <- cacheSolve(cached_m)


# Result1 run time: 6.282528 seconds
# Result2 run time: 0.000012 seconds
