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



print.pqnumber <- function(x, DEC = FALSE){
  # if dec is true then run the algorithm to print out the value of pqnumber.
  if (DEC == TRUE){
    p = x$p
    nums = x$nums
    
    #start a running sum of the number
    dec_rep = 0
    
    # multiply the number by the corresponding power of 10 and add to dec_rep
    for (i in 1:length(nums)){
      dec_rep = dec_rep + (nums[i] * 10^(i - p - 1)) 
    }
    # return dec_rep with the correct number of decimal points.
    return (format((x$sign * dec_rep), nsmall = x$p))
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

as_pqnumber <- function(x, p, q){
  
  UseMethod("as_pqnumber")
}

as_pqnumber.numeric <- function(x, p, q){
  int_portion = floor(x)
  dec_portion = round(x - floor(x), p)
  
  int_vec = rev(unlist(strsplit(as.character(int_portion), '')))
  
  dec_vec = rev(unlist(strsplit(as.character(dec_portion), '')))
  dec_vec = dec_vec[1:p]
  dec_vec = dec_vec[dec_vec != '.']
  dec_vec
  if (length(int_vec) < q + 1){
    length(int_vec) <- q + 1
    int_vec[is.na(int_vec)] <- 0
  }
  
  if (length(dec_vec) < p){
    dec_rev = rev(dec_vec)
    length(dec_rev) <- p
    dec_rev[is.na(dec_rev)] <- 0
    dec_vec = rev(dec_rev)
  }
  
  nums = c(dec_vec, int_vec)
  num_rep = as.numeric(nums)
  
  if (x < 0){
    sign = -1
  } else {
    sign = 1
  }
  
  
  return(pq_class(sign = sign, p = p, q = q, nums = num_rep))
  
}

as_numeric <- function(x){
  
  UseMethod("as_numeric")
}

as_numeric.pqnumber <- function(x){
  if (!inherits(x, 'pqnumber')){
    stop('x must be a pqnumber')
  }
  return (c(x$sign, x$p, x$q, x$nums))
}


carry_over <- function(pq_1, pq_2){
  p = max(c(pq_1$p, pq_2$p))
  q = max(c(pq_1$q, pq_2$q))
  
  num1 <- pq_1$nums
  q_p1 <- pq_1$q
  p_p1 <- pq_1$p
  
  num2 <- pq_2$nums
  q_p2 <- pq_2$q
  p_p2 <- pq_2$p
  
  while (q_p1 < q){
    num1 = c(num1, 0)
    q_p1 = q_p1 + 1
  }
  
  while (q_p2 < q){
    num2 = c(num2, 0)
    q_p2 = q_p2 + 1
  }
  
  while (p_p2 < p){
    num2 <- c(0, num2)
    p_p2 <- p_p2 + 1
  }
  
  while (p_p1 < p){
    num1 <- c(0, num1)
    p_p1 <- p_p1 + 1
  }
  
  sum_vec <- num1 + num2
  
  for (i in 1:(length(sum_vec) - 1)){
    while(sum_vec[i] > 9){
      sum_vec[i] <- sum_vec[i] - 10
      sum_vec[i + 1] = sum_vec[i + 1] + 1
    }
  }
  
  while(sum_vec[length(sum_vec)] > 9){
    sum_vec[length(sum_vec)] <- sum_vec[length(sum_vec)] - 10
    sum_vec = c(sum_vec, 1)
  }
  return(list(sum = sum_vec, p = p, q = q))
}

add <- function(x, y){
  if (x$sign == y$sign){
    results <- carry_over(x, y)
    return(pq_class(sign = x$sign, results$p, results$q, results$sum))
  } else {
    subtract(x, y)
  }
}



subtract <- function(pq_1, pq_2){
  
}