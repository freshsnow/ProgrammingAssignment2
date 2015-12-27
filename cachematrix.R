## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix returns an object with the following variables and functions 
#   Internal variable "x": used to store the matrix of interest x
#   Internal variable "inv": used to cache the inverse of matrix x
#   Function makeCacheMatrix$set(y): Stores matrix x in object and sets the 
#                                    internal variable "inv" to null 
#   Function makeCacheMatrix$get(): Returns matrix x
#   Function makeCacheMatrix$setinverse(inverse): Sets the cache variable "inv" to inverse 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # creates variable "inv" and sets it to NULL 
  set <- function(y) {
    x <<- y   # take matrix y used in makeCacheMatrix$set(y) and stores it in variable "x"
    inv <<- NULL # set the cached inverse value to null since a new matrix is now stored. 
  }
  get <- function () x # returns the stored matrix at variable x 
  setinverse <- function (inverse) inv <<- inverse # sets the cache variable as inverse 
  getinverse <- function () inv # returns the cache inverse 
  
  #function returns with a list containing four functions 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse() #obtain the variable that caches the inverse of x
        if( !is.null(inv) ) {#if the variable is not null, it means the cache has already been calculated 
          message("returning cached inverse")
          return(inv) #return the cached inverse 
        }
        
        #The inverse of x has not been cached. Hence, calculate the inverse, store it and then return the inverse value
        
        m <- x$get()
        inv <- solve(m)
        x$setinverse(inv)
        inv
        
}
