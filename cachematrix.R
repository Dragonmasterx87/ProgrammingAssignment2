# Coursera R Programming ->>> Programming Assignment 2 ####
# Function creates a vector which in turn cache's the calculation of the vector's inverse calculation
# This unique object is created using lists, using the 'set' & 'get' functions 
# This Function has these objects:
# >> get() - Gets the value of the matrix
# >> set() - Sets the value of the matrix
# >> getinverse() - Gets the inverse of the matrix
# >> setinverse() - Sets the inverse of the matrix

StupidCacheMatrix <- function(x = matrix()) {
  inVer <- NULL   # because everything in this world starts out as nothing so does our vector
  
  set <- function(y) { # sets the actual matrix 'x' and NULL's the cache'd inverse   
    x <<- y
    inVer <<- NULL # re-setting inverse calculations
  }
  
  get <- function() x # re-initialize my initial/internal object, returns the actual matrix 'x'
  
  setInverse <- function(inverse) inVer <<- inverse #
  getInverse <- function() inVer
  
  list(set = set, get = get,  # now to put all of this crap together
       setInverse = setInverse,
       getInverse = getInverse)
}


# This ridiculous function will calculate the inverse of the 'test' square matrix, caching
# the result in the StupidCacheMatrix object.
# If in case a cache already exists, it will output/return a pre-computed cached result, without bothering to calculate it.
# why? because its a lazy little shit. LoL. 

cacheSolve <- function(x, ...) {
  
  inVer <- x$getInverse() # Now to geta  matrix that is an inverse of the object 'x'
  if(!is.null(inVer)) {
    message("for the love of R im getting the cached data")
    return(inVer)
  }
  data <- x$get()
  
  inVer <- solve(data)  # This just calculates the inverse
  x$setInverse(inVer)
  inVer
}

# To test the functions do this:
# The command below makes a matrix of 2 rows and 2 columns containting the values 1, 2, 3, 4, 
# and begins calculating the inverse, storing in a list file containing all the information
# the function outlines (set, get, setinverse and getinverse)
x <- StupidCacheMatrix(matrix(1:4, 2, 2)) 

# if you are curious what the matrix itself looks like
matrix(1:4, 2, 2)

# The command below outputs/returns and now check out the inverse of the matrix above
cacheSolve(x)

# The comand below basically returns the same result as step 2, but without calculations (for the love of R im getting the cached data)
cacheSolve(x)
