pqnumber <- function(sign, p, q, nums){
  # Constructor function that creates pqnumber objects
  #Args:
  #   sign:  sign of pqnumber either 1 or -1
  #   p:     digits before decimal place 
  #   q:     digits after decimal place
  #   nums:  the numbers that will be used for p and q 
  #Returns:
  #   pqnumber object.
  
  
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
  # Function that checks if the requirements of a pqnumber are met.
  #Args:
  #   x:  a pqnumber
  #Returns:
  #   boolean: either True or False if the input is a pqnumber.
  
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
  # Adds a method to print that allows us to print in our own way,
  #Args:
  #   x:    pqnumber
  #   DEC:  Boolean for decimal representation.
  #Returns:
  #   num or pqnumber object.
  
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
    return (x$sign * dec_rep)
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
  # defines a general function.
  # Args:
  #   x:  pqnumber
  #   p:  digits before decimal of x.
  #   q:  digits after decimal of x.
  # Returns:
  #   pqnumber object.
  
  # if dec is true then run the algorithm to print out the value of pqnumber.
  
  UseMethod("as_pqnumber")
}

as_pqnumber.numeric <- function(x, p, q){
  # defines a method on numeric to to return a pqnumber object of x,p,q
  # Args:
  #   x:  float
  #   p:  digits before decimal of x.
  #   q:  digits after decimal of x.
  # Returns:
  #   pqnumber object
  
  # convert x into its integer and decimal components
  int_portion = floor(x)
  dec_portion = round(x - floor(x), p)
  
  # convert the integer into a string and split it and store it as a vector.
  int_vec = rev(unlist(strsplit(as.character(int_portion), '')))
  
  # do the same for the decimal portion and remove the leading 0 and the '.'
  dec_vec = rev(unlist(strsplit(as.character(dec_portion), '')))
  dec_vec = dec_vec[1:p]
  dec_vec = dec_vec[dec_vec != '.']

  # if the length of the integer vector is less than the specified q, pad it with 0
  if (length(int_vec) < q + 1){
    length(int_vec) <- q + 1
    int_vec[is.na(int_vec)] <- 0
  }
  
  # if the length of the decimal vector is less than the specified p, pad it with 0 on the back.
  if (length(dec_vec) < p){
    dec_rev = rev(dec_vec)
    length(dec_rev) <- p
    dec_rev[is.na(dec_rev)] <- 0
    dec_vec = rev(dec_rev)
  }
  
  # concatenate both decimal and integer vectors and return as a numeric vector.
  nums = c(dec_vec, int_vec)
  num_rep = as.numeric(nums)
  
  # determine the sign
  if (x < 0){
    sign = -1
  } else {
    sign = 1
  }

  # create a new pqnumber object.
  return(pqnumber(sign = sign, p = p, q = q, nums = num_rep))
  
}

as_numeric <- function(x){
  # defines a generic function that takes a pqnumber and outputs a numeric vector.
  # Args:
  #   x:  pqnumber
  # Returns:
  #   numeric vector containing p, q, nums
  UseMethod("as_numeric")
}


as_numeric.pqnumber <- function(x){
  # defines a method on pqnumber to to return a numeric vector of x,p,q
  # Args:
  #   x:  pqnumber
  # Returns:
  #   numeric vector containing p, q, nums
  
  # check if x is a pqnumber.
  if (!inherits(x, 'pqnumber')){
    stop('x must be a pqnumber')
  }
  
  #simply return its sign, p, q, and nums vector.
  return (c(x$sign, x$p, x$q, x$nums))
}


carry_over <- function(pq_1, pq_2){
  # Mimics the carrying over that is done in addition.
  # Args:
  #   pq_1:  pqnumber
  #   pq_2:  pqnumber
  # Returns:
  #   list:   containing the p, q, and summed-nums vector.
  
  # pick the p and the q.
  p = max(c(pq_1$p, pq_2$p))
  q = max(c(pq_1$q, pq_2$q))
  
  #store each p,q, and nums of both pqnumbers
  num1 <- pq_1$nums
  q_p1 <- pq_1$q
  p_p1 <- pq_1$p
  
  num2 <- pq_2$nums
  q_p2 <- pq_2$q
  p_p2 <- pq_2$p
  
  # pad the vectors appropriately to make sure the required digit is padded.
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
  
  #sum the two vectors together.
  sum_vec <- num1 + num2
  
  # if there is a value greater than 9, subtract 10 and add 1 to the next element.
  for (i in 1:(length(sum_vec) - 1)){
    while(sum_vec[i] > 9){
      sum_vec[i] <- sum_vec[i] - 10
      sum_vec[i + 1] = sum_vec[i + 1] + 1
    }
  }
  
  # if the last value is greater than 9, subtract 10 and add 1 to the end of the vector.
  while(sum_vec[length(sum_vec)] > 9){
    sum_vec[length(sum_vec)] <- sum_vec[length(sum_vec)] - 10
    sum_vec = c(sum_vec, 1)
    q <- q + 1
  }
  
  # return all elements in list format
  return(list(sum = sum_vec, p = p, q = q))
}

add <- function(x, y){
  # adds two pqnumber objects together.
  # Args:
  #   x:  pqnumber
  #   y:  pqnumber
  # Returns:
  #   pqnumber object
  
  # if the signs are the same then we are performing addition, other wise its a subtraction.
  if (x$sign == y$sign){
    fin_sign <- x$sign
    x$sign <- 1
    y$sign <- 1
    results <- carry_over(x, y)
    return(pqnumber(sign = fin_sign, results$p, results$q, results$sum))
  } else {
    x$sign <- 1
    y$sign <- 1
    subtract(x, y)
  }
}

borrow <- function(pq_1, pq_2){
  # mimics the borrowing of subtraction.
  # Args:
  #   pq_1:  pqnumber
  #   pq_2:  pqnumber
  # Returns:
  #   list containing appropriate p,q, and subtraction vector values.
  
  #store all our necessary values.
  p = max(c(pq_1$p, pq_2$p))
  q = max(c(pq_1$q, pq_2$q))
  
  num1 <- pq_1$nums
  q_p1 <- pq_1$q
  p_p1 <- pq_1$p
  
  num2 <- pq_2$nums
  q_p2 <- pq_2$q
  p_p2 <- pq_2$p
  
  
  # pad the vectors with 0s appropriately
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
  
  # subtract the two vectors.
  sub_vec <- num1 - num2
  
  # if there's a negative value then add 10 and subtract one from the next element.
  for (i in 1:(length(sub_vec) - 1)){
    while (sub_vec[i] < 0){
      sub_vec[i] <- sub_vec[i] + 10
      sub_vec[i+1] <- sub_vec[i + 1] - 1
    }
  }
  
  #return a list containing the new computed subtraction vector, p and q.
  return(list(sub = sub_vec, p = p, q = q))
}

subtract <- function(pq_1, pq_2){
  # subtracts two pqnumber values.
  # Args:
  #   pq_1:  pqnumber
  #   pq_2:  pqnumber
  # Returns:
  #   pqnumber object
  
  # if the signs are different than we are performing addition.
  if (pq_1$sign != pq_2$sign){
    fin_sign <- pq_1$sign
    pq_1$sign <- 1
    pq_2$sign <- 1
    res <- add(pq_1, pq_2)
    res$sign <- fin_sign
    return(res)
  } else { # pick the larger pq number and pass it into the borrow function and create the pqnumber object.
    if(abs(print(pq_1, DEC=TRUE)) < abs(print(pq_2, DEC=TRUE))){
      res <- borrow(pq_2, pq_1)
      return(pqnumber(sign = (-1*pq_1$sign), p = res$p, q = res$q, nums = res$sub))
    } else if (abs(print(pq_1, DEC=TRUE)) > abs(print(pq_2, DEC=TRUE))){
      res <- borrow(pq_1, pq_2)
      return(pqnumber(sign = (pq_1$sign), p = res$p, q = res$q, nums = res$sub))
    } else {
      return(pqnumber(1, 0, 0, 0))
    }
  }
}


multiply <- function(x, y){
  # multiplies two pqnumber values.
  # Args:
  #   pq_1:  pqnumber
  #   pq_2:  pqnumber
  # Returns:
  #   pqnumber object
  stop("\nMultiply function not defined -- \nI couldn't figure out how to perform multiplication as specified.")
}




