messy_impute <- function(input_tibble, center="mean", margin, ...){
  #error handling for center values not equal to expected inputs.
  if (center != "mean" & center != "median"){
    stop("center must be mean or median.")
  }
  #error handling for margin values not equal to 1 or 2
  if (!(margin %in% c(1, 2))){
    stop("margin must be either 1 or 2")
  }
  
  #rows and columns of NA values.
  na_matrix <- which(is.na(input_tibble), arr.ind=TRUE)
  
  #Inputs mean/median value through students/rows 
  if (center == "mean"){
    if (margin == 1){
      for (i in 1:length(na_matrix[, 1])){
        if (na_matrix[i, 2] %in% c(1, 2, 3, 4, 5, 6)){
          placeholder_row <- input_tibble[na_matrix[i, 1],]
          placeholder_row <- unlist(placeholder_row, use.names = FALSE)
          
          input_tibble[na_matrix[i,1], na_matrix[i,2]] <- mean(placeholder_row[2:6], na.rm = TRUE, ...)
        } else {
            input_tibble[na_matrix[i,1], na_matrix[i,2]] <- mean(placeholder_row[7:13], na.rm = TRUE, ...)
        }
      }
    } else { #Inputs mean/median values through columns/assignment
        for (i in 1:length(na_matrix[,2])){
          input_tibble[na_matrix[i,1], na_matrix[i,2]] <- mean(input_tibble[[na_matrix[i,2]]], na.rm = TRUE)
        }
      }
  } else {
    if (margin == 1){
      for (i in 1:length(na_matrix[, 1])){
        if (na_matrix[i, 2] %in% c(1, 2, 3, 4, 5, 6)){
          placeholder_row <- input_tibble[na_matrix[i, 1],]
          placeholder_row <- unlist(placeholder_row, use.names = FALSE)
          
          input_tibble[na_matrix[i,1], na_matrix[i,2]] <- median(placeholder_row[2:6], na.rm = TRUE)
        } else {
          input_tibble[na_matrix[i,1], na_matrix[i,2]] <- median(placeholder_row[7:13], na.rm = TRUE)
        }
      }
    } else { #Inputs mean/median values through columns/assignment
      for (i in 1:length(na_matrix[,2])){
        input_tibble[na_matrix[i,1], na_matrix[i,2]] <- median(input_tibble[[na_matrix[i,2]]], na.rm = TRUE)
      }
    }
  }
  return (input_tibble)
}