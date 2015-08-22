## Assigment: Caching the Inverse of a Matrix @michaluscinski

## This function creates a special "matrix" object that can cache its invers in inv_matrix object
## Argument x is a matrix
## Result is a list with functions:
##  set - you can change a matrix
##  comp_inv - you can compute inverse of a matrix
##  get - you can get a matrix
##  get_inv - yoy can get an inverse of a matrix
##  get_status - you can check whether you should recompute inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- matrix()
  status <- FALSE  

  set_matrix <- function(y = matrix()){
    x <<- y
    status <<- FALSE
  }
  
  comp_inv_matrix <- function() {
    inv_matrix <<- solve(x)
    status <<- TRUE
  }
  
  comp_inv_matrix()
  
  get_matrix <- function() x
  get_inv_matrix <- function() inv_matrix
  get_status <- function() status
  
  list(set = set_matrix,
       comp_inv = comp_inv_matrix,
       get = get_matrix, 
       get_inv = get_inv_matrix, 
       get_status = get_status)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated and the matrix has not changed,
## then the cachesolveretrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  if(x$get_status() == TRUE){
    message("YEAH, We have cached data!")
    return(x$get_inv()) ## Return an inverse of matrix
  }
  
  else{
    message("We don't have cached data! We need to make computation!")
    x$comp_inv() ## Recompute an inverse of matrix
    message("After computation we can show result:")
    x$get_inv() ## Return an inverse of matrix
  }
    
}


## This is how I testes it.
# rm(list = ls()) # clear working space

# matrix_1 <- matrix(1:4, nrow=2, ncol=2)
# matrix_2 <- matrix(4:1, nrow=2, ncol=2)
# matrix_1
# matrix_2

# a <- makeCacheMatrix(matrix_1)
# class(a)
# a$get()
# a$get_inv()
# a$get_status()
# a$set(matrix_2)
# a$get_status()
# cacheSolve(a)
