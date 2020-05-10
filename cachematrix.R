## Firstly, compute the inverse of the special matrix, the function is called "makeCacheMatrix"
##The function has only one argument, a matrix
makeCacheMatrix <- function(x = matrix()) {
  
##NULL in R represents values undefined 
  m <- NULL
##With this code we set the matrix
 set <- function(y) {
   x <<- y
   m <<- NULL
 }
##With this code we get the matrix and return the matrix in our case "x" 
 get <- function (){
   x
 } 
 ## Method to set the inverse of the matrix with the function solve, after that, we assign 
 ##that our previous undefined variable "m" is the inverse matrix.
 setsolve <- function(solve) {
   m <<- solve  
 }

##To get the inverse matrix, we have to assign it, to use it in the future, this is where the inverse of our matrix is save.
 getsolve <- function() {
   m
 }
##Return a list of all our methods
 list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}
##The following function calculates the inverse of the special "matrix" created with the above function. 
##However, this function has a particularity, if the inverse of the matrix has already been calculated, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setmean function.


cacheSolve <- function( x, ...) {
##Return a matrix that is the inverse of "x"
  m <- x$getsolve()
##Just return the inverse if its already set
  if(!is.null(m)) {
    message ("getting cached data")
    return(m)
  }
##Get the matrix from our object
  data <- x$get()
##Calculare the inverse
  m <- solve(data, ...)
##Set the inverse to the object
  x$setsolve(m)
##Retrun the matrix
  m
}
 
