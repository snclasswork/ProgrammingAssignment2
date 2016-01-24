# OBJECTIVE
# cache computations (e.g. mean of vector, inverse of vector)
# so you can lookup in cache rather than recompute

# DO:
# Check to see if calculation has already been performed
# Yes - return result
# No - calculate and return result

# Inverse
# error check using det(the_matrix)
# assume that the matrix supplied is always invertible



# function to create a numeric vector
makeCacheMatrix <- function(x = numeric()) {
        
        # local variable
        m <- NULL
        
        set <- function(y) {
                
                # global variables
                x <<- y
                m <<- NULL
        }
        
        get <- function() {x}
        
        setinv <- function(solve) {m <<- solve}
        
        getinv <- function() {m}
        
        # pointer list to function calls
        list(
                set = set, 
                get = get,
                setinv = setinv,
                getinv = getinv)
        
}

# function to check for existing calc and return or calc
cacheSolve <- function(x, ...) {
        
        m <- x$getinv()
        
        # if calc already performed, return value
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # if calc has not been performed, calc
        #message("hold please, calculating...")
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}


# run a test of functions
#run_test = function(the_matrix){

#        temp = makeCacheMatrix(the_matrix)

#        start.time = Sys.time()
#        the_inv <- cacheSolve(temp)
#        run_time = Sys.time() - start.time
#        print(the_inv)
#        print(run_time)

#        start.time = Sys.time()
#        the_inv <- cacheSolve(temp)
#        run_time = Sys.time() - start.time
#        print(the_inv)
#        print(run_time)
#}



#my_list <- c(1,1,4,0,3,1,4,4,0)
#my_vector <- matrix(my_list,3,3)
#my_vector
#run_test(my_vector)

#check calc
#det(my_vector) #-48
#solve(my_vector)


