# This function creates or takes a given matrix and provides set/get methods 
# on the "mat" object. It also defines set/get methods on the inverse of a 
# matrix viz., set_matrix_inverse and get_matrix_inverse. Finally it returns 
# a list of all these methods that can be called on the object


makeCacheMatrix <- function(mat = matrix()) {
	# Assigning the variable to NULL
	matrixInv <- NULL						
	
	#Closure method set
    set <- function(m) {
        mat <<- m 		
        matrixInv <<- NULL
    }
	
	#Closure method get.
    get <- function() {	mat	}
	
	#Closure methods set/get for matrix inverse variable
    set_matrix_inverse <- function(inverse) {	matrixInv <<- inverse		}
    get_matrix_inverse <- function() 		{	matrixInv					}
    list(set=set, get=get, set_matrix_inverse=set_matrix_inverse, 
		get_matrix_inverse=get_matrix_inverse) # Declaring a list of methods
}

# This function is a helper function which takes a matrix object and creates 
# an inverse of it if it is not already created. It calls the solve method 
# to create inverse of a matrix only if it is not already created. If this 
# function is called more than once, then inverse of the matrix that is cached
# during the first call is returned without recomputing it.

cacheSolve <- function(mat, ...) {

	# Trying to get the matrixInv object on a given matrix (mat)
    matrixInv <- mat$get_matrix_inverse()
	
	# Check if the inverse of the matrix is NULL and only then create the 
	# inverse of the matrix
    if(is.null(matrixInv)) {
		data <- mat$get()
		matrixInv <- solve(data)
		mat$set_matrix_inverse(matrixInv)
    } else {
		# matrix inverse is already created, print the message
        message("getting cached data.")
	}
    return(matrixInv)
}

x = rbind(c(1, 2), c(3, 4))
m = makeCacheMatrix(x)
m$get()
cacheSolve(m)
cacheSolve(m)
