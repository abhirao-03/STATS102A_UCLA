messy_impute <- function(input_tibble, center = "mean", margin, ...) {
  # Takes in messy data and imputes NA values using either mean or median and either row/column imputation.
  # Args:
  # input_tibble: tibble - dataset with 13 columns of homeworks and quizzes.
  # center: string - specified to use either mean or median function.
  # Return:
  # imputed_tibble - tibble with NA values imputed.

  # error handling for if the input_tibble is in fact a tibble
  if (!is_tibble(input_tibble)) {
    stop("the input data must be in the format of a tibble!")
  }

  # error handling for center values not equal to expected inputs.
  if (center != "mean" & center != "median") {
    stop("center must be 'mean' or 'median'!")
  }

  # error handling for margin values not equal to 1 or 2
  if (!(margin %in% c(1, 2))) {
    stop("margin must be either 1 or 2!")
  }

  # rows and columns of NA values.
  na_matrix <- which(is.na(input_tibble), arr.ind = TRUE)


  if (center == "mean") {
    if (margin == 1) { # Inputs mean value through students/rows
      for (i in 1:length(na_matrix[, 1])) {
        if (na_matrix[i, 2] %in% c(2, 3, 4, 5, 6)) { # if the assignment is a HW, average ONLY the HW assignments.
          placeholder_row <- input_tibble[na_matrix[i, 1], ]
          placeholder_row <- unlist(placeholder_row, use.names = FALSE)

          input_tibble[na_matrix[i, 1], na_matrix[i, 2]] <- mean(placeholder_row[2:6], na.rm = TRUE, ...)
        } else { # if the assignment is a Quiz, average ONLY the Quiz assignments.
          placeholder_row <- input_tibble[na_matrix[i, 1], ]
          placeholder_row <- unlist(placeholder_row, use.names = FALSE)
          input_tibble[na_matrix[i, 1], na_matrix[i, 2]] <- mean(placeholder_row[7:13], na.rm = TRUE, ...)
        }
      }
    } else { # Inputs mean values through columns/assignment
      for (i in 1:length(na_matrix[, 2])) {
        input_tibble[na_matrix[i, 1], na_matrix[i, 2]] <- mean(input_tibble[[na_matrix[i, 2]]], na.rm = TRUE, ...)
      }
    }
  } else {
    if (margin == 1) { # Inputs median value through student/rows
      for (i in 1:length(na_matrix[, 1])) {
        if (na_matrix[i, 2] %in% c(1, 2, 3, 4, 5, 6)) { # if the assignment is a HW, average ONLY the HW assignments.
          placeholder_row <- input_tibble[na_matrix[i, 1], ]
          placeholder_row <- unlist(placeholder_row, use.names = FALSE)
          input_tibble[na_matrix[i, 1], na_matrix[i, 2]] <- median(placeholder_row[2:6], na.rm = TRUE)
        } else { # if the assignment is a Quiz, average ONLY the Quiz assignments.
          placeholder_row <- input_tibble[na_matrix[i, 1], ]
          placeholder_row <- unlist(placeholder_row, use.names = FALSE)
          input_tibble[na_matrix[i, 1], na_matrix[i, 2]] <- median(placeholder_row[7:13], na.rm = TRUE)
        }
      }
    } else { # Inputs median values through columns/assignment
      for (i in 1:length(na_matrix[, 2])) {
        input_tibble[na_matrix[i, 1], na_matrix[i, 2]] <- median(input_tibble[[na_matrix[i, 2]]], na.rm = TRUE)
      }
    }
  }
  return(input_tibble)
}



tidy_impute <- function(input_tibble, center = "mean", margin, ...) {
  # Takes in tidy data and imputes NA values using either mean or median and either row/column imputation.
  # Args:
  # input_tibble: tibble - dataset with 3 columns, UID, Assignments, and Marks.
  # center: string - specified to use either mean or median function.
  # Return:
  # imputed_tibble - tibble with NA values imputed.


  # Error correcting steps are all the same from messy_impute so we just bring them in.
  # error handling for if the input_tibble is in fact a tibble
  if (!is_tibble(input_tibble)) {
    stop("the input data must be in the format of a tibble!")
  }

  if (ncol(input_tibble) != 3) {
    stop("the input_tibble is likely of the wrong size, keep tibble in the format: UID, Assignments, Marks")
  }

  # error handling for center values not equal to expected inputs.
  if (center != "mean" & center != "median") {
    stop("center must be 'mean' or 'median'!")
  }

  # error handling for margin values not equal to 1 or 2
  if (!(margin %in% c(1, 2))) {
    stop("margin must be either 1 or 2!")
  }

  original_colnames <- colnames(input_tibble)
  colnames(input_tibble) <- c("UID", "Assignments", "Marks")

  tidy_na <- which(is.na(input_tibble), arr.ind = TRUE)

  if (center == "mean") { # Inputs mean into NA values.
    if (margin == 1) { # Calculates mean using student-wise marks.
      for (ind in 1:length(tidy_na[, 1])) {
        na_ind <- tidy_na[, 1][ind]
        na_uid <- unlist(input_tibble[na_ind, ][1], use.names = FALSE)
        na_student <- filter(input_tibble, input_tibble$UID == na_uid)

        for (i in 1:length(na_student$Marks)) {
          if (i <= 5) { # if the assignment is a HW then ONLY compute HW mean
            if (is.na(na_student$Marks[i])) {
              input_tibble[na_ind, 3] <- mean(na_student$Marks[1:5], na.rm = TRUE, ...)
            }
          } else { # if the assignment is a Quiz then ONLY compute Quiz mean
            if (is.na(na_student$Marks[i])) {
              input_tibble[na_ind, 3] <- mean(na_student$Marks[6:12], na.rm = TRUE, ...)
            }
          }
        }
      }
    } else { # Calculates mean using assignment-wise marks.
      for (i in 1:length(tidy_na[, 1])) {
        na_ind <- tidy_na[, 1][i]
        na_assignment_id <- unlist(input_tibble[na_ind, ][2], use.names = FALSE)
        na_assignment <- filter(input_tibble, input_tibble$Assignments == na_assignment_id)
        input_tibble[na_ind, 3] <- mean(na_assignment$Marks, na.rm = TRUE, ...)
      }
    }
  } else { # Inputs median into NA values.
    if (margin == 1) { # Calculates mean using student-wise marks.
      for (ind in 1:length(tidy_na[, 1])) {
        na_ind <- tidy_na[, 1][ind]
        na_uid <- unlist(input_tibble[na_ind, ][1], use.names = FALSE)
        na_student <- filter(input_tibble, input_tibble$UID == na_uid)

        for (i in 1:length(na_student$Marks)) {
          if (i <= 5) { # if the assignment is a HW then ONLY compute HW mean
            if (is.na(na_student$Marks[i])) {
              input_tibble[na_ind, 3] <- median(na_student$Marks[1:5], na.rm = TRUE, ...)
            }
          } else { # if the assignment is a Quiz then ONLY compute Quiz mean
            if (is.na(na_student$Marks[i])) {
              input_tibble[na_ind, 3] <- median(na_student$Marks[6:12], na.rm = TRUE, ...)
            }
          }
        }
      }
    } else { # Calculates median using assignment-wise marks.
      for (i in 1:length(tidy_na[, 1])) {
        na_ind <- tidy_na[, 1][i]
        na_assignment_id <- unlist(input_tibble[na_ind, ][2], use.names = FALSE)
        na_assignment <- filter(input_tibble, input_tibble$Assignments == na_assignment_id)
        na_assignment
        input_tibble[na_ind, 3] <- median(na_assignment$Marks, na.rm = TRUE)
      }
    }
  }
  colnames(input_tibble) <- original_colnames
  return(input_tibble)
}
