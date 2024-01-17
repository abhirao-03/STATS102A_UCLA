gcd <- function(x, y, ...){
  # Outputs the GCD of two integers.
  #Args:
  #x: integer
  #y: integer
  #Return:
  #GCD: integer
  
  nums = c(abs(x), abs(y)) #stores pos x, y in case of later transformation.
  a = max(nums)            #makes a the higher value and b the lower value
  b = min(nums)
  
  if (a %% b == 0){        #if a mod b = 0 then the min of {a,b} is the GCD.
    return (b)
  }
  else{                    #follow the algorithm described in HW1.pdf
    mod_xy <- a %% b
    while (mod_xy != 0){
      mod_xy <-  a %% b
      a <- b
      b <- mod_xy
    }
    return (a)
  }
}

lcm <- function(x, ...){
  #Outputs the LCM of the integers.
  #Args:
  #x: vector    n x 1 dimensional vector where 1 < n < 101
  #Return:
  #LCM: integer  1-dimensional vector

  #We can use the relationship that LCM(a, b) = (a * b)/GCD(a, b)
  #run the code block below till we have a vector with 1 integer value.
  while (length(x) != 1){
    a <- abs(x[1])
    b <- abs(x[2])
    x <- x[2:length(x)]
    x[1] <- a * b / gcd(a,b)
  }
  return (x)
}

is_prime <- function(x, ...){
  # from https://www.geeksforgeeks.org/prime-numbers/
  final_list <- as.logical(1:length(x))
  for (i in 1:length(x)){
    n <- x[i]
    final_list[i] = all((1:(n-1))^(n-1) %% n == 1)
  }
  return (final_list)
}