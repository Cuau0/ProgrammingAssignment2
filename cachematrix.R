#Functions were created to store an matrix and then calculate its inverse

#Save and create a special matrix based on the matrix assigned by the user 
#which is stored in the cache to be used later 
#with their respective functions in a list of 3 elements

makeCacheMatrix <- function(x = matrix()){
  n <- NULL
  get <- function() x
  setinv <- function(inv) n <- inv
  getinv <- function() n
  list(get = get,
       setinv = setinv,
       getinv = getinv)
  }
  
#Solve the saved matrix using the functions of the defined list to 
#be able to solve the inverse of the matrix using 
#the "solver" function and solve the inverse directly.

cacheSolve <- function(x,...){
  n <- x$getinv()
  if(!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setinv(n)
  n
}
 
##Example
t <- matrix(c(1,3,3,5,5,7,7,9,9), ncol = 3, nrow = 3)  
my_matrix <- makeCacheMatrix(t)
cacheSolve(my_matrix)
