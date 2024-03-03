pq_class <- function(sign, p, q, nums){
  # check if sign is 1 or -1
  if (!(sign %in% c(1, -1))){
    stop("sign must be either 1 or -1")
  }
  
  # check if the nums vector is of the correct size
  if(length(nums) != p + q + 1){
    stop("incorrect size for p, q, and nums")
  }
  
  # check if p or q is negative.
  if (p < 0 | q < 0 | p %% 1 != 0 | q %% 1 != 0){
    stop("p and q must be positive values.")
  }
  
  # check if any digits in the nums vector are non-integer or non 0-9
  if (any(nums < 0 | nums > 9 | nums %% 1 != 0)) {
    stop("nums must contain integers between 0 and 9.")
  }
  
  # if all tests are passed, create a pqnumber object
  structure(list(sign = sign, p = p, q = q, nums = nums), class = 'pqnumber')
}


is_pqnumber <- function(x) {
  # Check if the object has the "pqnumber" class
  if (!inherits(x, "pqnumber")) {
    return(FALSE)
  }
  
  # Check if the object has all the required arguments to be a pqnumber
  if (!(all(c("sign", "p", "q", "nums") %in% names(x)))) {
    return(FALSE)
  }
  # Check if sign is either 1 or -1
  if (!(x$sign %in% c(1, -1))) {
    return(FALSE)
  }
  
  # Check if p and q are non-negative integers
  if (!(is.numeric(x$p)) | !(is.numeric(x$q)) | x$p %% 1 != 0 | x$q %% 1 != 0 | x$p < 0 | x$q < 0) {
    return(FALSE)
  }
  
  # Check if nums is a vector containing only integers and is of length p + q + 1
  if (!(is.numeric(x$nums)) | length(x$nums) != x$p + x$q + 1) {
    return(FALSE)
  }
  
  # Check if nums contains only contains integers between 0 and 9
  if (any(x$nums < 0 | x$nums > 9 | x$nums %% 1 != 0)) {
    return(FALSE)
  }
  
  # input is a pqnumber if all tests pass.
  return(TRUE)
}



print.pqnumber <- function(x, dec = FALSE){
  # if dec is true then run the algorithm to print out the value of pqnumber.
  if (dec == TRUE){
    p = x$p
    nums = x$nums
    
    #start a running sum of the number
    dec_rep = 0
    
    # multiply the number by the corresponding power of 10 and add to dec_rep
    for (i in 1:length(nums)){
      dec_rep = dec_rep + (nums[i] * 10^(i - p - 1)) 
    }
    # return dec_rep with the correct number of decimal points.
    return (format(dec_rep, nsmall = x$p))
  } else {
    # just a bunch of concatenations to print in the desired format.
    cat(cat(paste0("sign = ",x$sign),
            paste0("p = ", x$p),
            paste0("q = ", x$q),
            sep = "\n"),
        cat("nums =", x$nums),
        sep='\n')
  }
}