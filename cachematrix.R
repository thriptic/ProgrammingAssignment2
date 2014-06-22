## makeCacheMatrix() and cacheSolve() are employed to efficiently calculate the 
## inverse of a defined matrix. This inverse is 'cached' so that it can be 
## rapidly returned upon further calls of cacheSolve() without employing
## significant CPU resources.

# makeCacheMatrix: 
# matrix -> list

# Outputs 4 functions used for calculating inverse of matrix via the
# cacheSolve function


makeCacheMatrix <- function(mat = numeric(0)) {

	manual.inverse <- numeric(0)
	
	set.matrix <- function(y) {  # sets matrix
			
		mat <<- y
		manual.inverse <<- numeric(0) # used to clear existing cached inverted vals
	}
	
	set.inverse <-function(manual) {
	
		manual.inverse <<- manual
	}
	
	get.matrix <- function() {
		mat
	}
	
	get.inverse <- function() {
		manual.inverse
	}
	
	
	list(setmatrix = set.matrix,
		 setinverse = set.inverse,
		 getmatrix = get.matrix,
		 getinverse = get.inverse
		)

		
}



z <- makeCacheMatrix(mat) # required to avoid '$ operator on atomic vector' error


cacheSolve <- function(r = z, ...) {

## list -> matrix
		
## Returns the inverse value of a matrix defined in makeCacheMatrix
## If inverse has already been calculated, returns previously calculated value.		
		
	inv <- r$getinverse()
	
	# Checks to see if inverse has already been computed.
	if (is.null(inv)) {
		data <- r$getmatrix() #imports matrix values from makeCacheMatrix for inversion
		inv <- solve(data) # inverts matrix
		r$set.inverse(inv) # sets cached value for future function calls
	}else {  
        message('cached value')
	}
	inv
	
}