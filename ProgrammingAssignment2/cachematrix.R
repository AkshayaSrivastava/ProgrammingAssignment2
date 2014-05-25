## Function 1 creates, stores and caches a matrix. Cacheing the matrix allows other functions 
## access to the cached information
## Function 2 solves, returns, and caches the inverse of the matrix. This allows Function 1 to
## access the inverse of the original matrix without repeatedly solving for the inverse.

## Create, store and cache a matrix

makeCacheMatrix <- function(x = matrix()) {
      cache <- NULL # Setting the cache to null
      set <- function(mat) { # A setter, allows the function to get a new matrix
        x <<- mat
        cache <<- NULL
      }
      get <- function() x # A getter, allows the function to return a stored matrix
      cachesave <- function(mat) cache <<- mat # save the matrix to the cache
      cacheload <- function() cache # get the matrix from the cache
      list(set = set, get = get, 
           cachesave = cachesave,
           cacheload = cacheload) # define your subfunctions/methods
}


## Solves, returns, and caches the inverse of the matrix

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      mat <- x$cacheload() # Try loading cached matrix
      if(!is.null(mat)) {
        message("getting cached data")
        return(mat)
      }
      data <- x$get()
      mat <- solve(data, ...) # get the inverse
      x$cachesave(mat) # save the inverse back into the cache
      mat # output the inverse
}
