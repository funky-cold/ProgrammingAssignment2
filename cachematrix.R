#makeCacheMatrix creates a list containing
#set
#get
#set inverse
#get inverse


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL  #default is set to NULL
  set <- function(y) {  #set function
    x <<- y   #set matrix
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse  #Set the inverse
  getinv <- function() i  # get the inverse
  list(set = set, get = get,  #create list
       setinv = setinv,
       getinv = getinv)
}


cacheSolve <- function(x, ...) {
  i <- x$getinv()  #Retrieve to check for existence
  if(!is.null(i)) {  #If this is already stored
    message("getting cached inverse matrix") #Returns stored matrix if it's been computed
    return(i)
  }
  data <- x$get()  #Get the matrix if it has not previously been computed
  i <- solve(data, ...)  #Apply solve function
  x$setinv(i)  #Cache
  i  #Return
}