## Caching the Inverse of a Matrix:

## This function creates a special "matrix" object that can cache its inverse.
## Declaring x as a matrix
makeCacheMatrix <- function(x = matrix()) {
  ## setting inv as NULL
  inv <- NULL
  ## Defining the set function
  ## Assign the input argument to the x object in the parent environment, 
  ## Assign the value of NULL to the m object in the parent environment.(clears
  ## any value of inv previously cached)
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## this function takes advantage of the lexical scoping features in R. Since 
  ## the symbol x is not defined within get(), R retrieves it from the parent 
  ## environment of makeCacheMatrix
  get <- function() x
  ## defining the setter for the inverse of inv
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting JP cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}


##Testing The Functions

jp_matrix <- makeCacheMatrix(matrix(4:1, 2, 2))
jp_matrix$get()
## [,1] [,2]
## [1,]    4    2
## [2,]    3    1
jp_matrix$getInverse()
## NULL
cacheSolve(jp_matrix)
## [,1] [,2]
## [1,] -0.5    1
## [2,]  1.5   -2
cacheSolve(jp_matrix)
## getting JP cached data
## [,1] [,2]
## [1,] -0.5    1
## [2,]  1.5   -2
jp_matrix$getInverse()
## [,1] [,2]
## [1,]   -0.5  1
## [2,]    1.5 -2
jp_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
jp_matrix$get()
## [,1] [,2]
## [1,]    2    1
## [2,]    2    4
jp_matrix$getInverse()
##NULL
cacheSolve(jp_matrix)
## [,1]       [,2]
## [1,]  0.6666667 -0.1666667
## [2,] -0.3333333  0.3333333
cacheSolve(jp_matrix)
## getting JP cached data
## [,1]       [,2]
## [1,]  0.6666667 -0.1666667
## [2,] -0.3333333  0.3333333
jp_matrix$getInverse()
## [,1]       [,2]
## [1,]  0.6666667 -0.1666667
## [2,] -0.3333333  0.3333333