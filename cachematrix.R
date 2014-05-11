## Put comments here that give an overall description of what your
## functions do

## Function which transcribes plain matrix into list that contains matrix and cache of inversion


makeMatrix <- function(x = matrix()) { 
  i <- NULL                           #sets variable for inversion
  set <- function(y) {                #records given matrix
    x <<- y
    i <<- NULL
  }
  get <- function() x                 # function to get the matrix
  setinverse <- function(inverse) i <<- inverse # function to cache inversion
  getinverse <- function() i          # function to get the caches inversion
  list(set = set, get = get           # gives a list with acces to original matrix, caches inversion and functions to write and read them
       
       setinverse = setinverse,
       getinverse = getinverse)

}


##  Function which computes inversion of a given matrix or reads from cache if already computed

cacheInverse <- function(x, ...) {
  
  i <- x$getinverse()             # checks the cache
  if(!is.null(i)) {               # if inversion is already there
    message("getting cached inversion")   
    return(i)                     #then it is just read and returned
  }
  temp <- x$get()                 # if inversion is not cached, original matrix is recalled
  i <- solve(temp,...)            # inversion is computed
  x$setinverse(i)                 # and put into cache
  i                               # finally, either way, inversion is returned
  
}


