## this function will create a square matrix object
## this code is written following the logic of an analogous code
##provided in makeVector and cachemean functions described in
##https://class.coursera.org/rprog-010/human_grading/view/courses/972581/assessments/3/submissions

makeCacheMatrix <- function(x = matrix()) {

m <- NULL # provides default value for m (the matrix inverse) prior to use of cacheSolve function

y <- NULL # provides default value for y prior to use of cacheSolve function

mat_set <- function(y) { #this sets the value of the matrix
  
x <<- y ## storing the matrix in cache

m <<- NULL # # sets the value of m (the matrix inverse) to NULL

  }  

mat_get <- function() x #gets the matrix 

inv_set <- function (solve) m <<- solve # solving for the inverse of the matrix

inv_get <- function () m #gets the inverse value

# creates a special list vector to set the value of the matrix, get the value of the matrix
## set the value of inverse and get the value of inverse 

list(mat_set = mat_set, mat_get = mat_get,inv_set = inv_set, inv_get = inv_get)  

}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## using the solve() function
  ## this code is written following the logic of an analogous code
  ##provided in makeVector and cachemean functions described in
  ##https://class.coursera.org/rprog-010/human_grading/view/courses/972581/assessments/3/submissions
  
  
  m <- x$inv_get() # if an inverse has already been calculated this gets it
  
  if(!is.null(m)){ # check to see if cacheSolve has been run before
    
    message("getting cached data")
      
    return(m)
  }
  
  # in case we have not yet run the cacheSolve, so can not just retrieve data, we need to calculate 
  
  y <- x$mat_get() # retrive value of input matrix
  
  x$mat_set(y) # caching value of input matrix
  
  m <- solve(y, ...) # compute the value of the inverse matrix
  
  x$inv_set(m) # cache the value of inverse matrix
  
  m # return the inverse
   
}
