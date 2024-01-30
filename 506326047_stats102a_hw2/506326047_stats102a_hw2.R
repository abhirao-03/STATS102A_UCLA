set.seed(506326047) ## set the seed to UID

## Create a character vector containing the names of homeworks and quizzes
names <- paste0("Homework_", 1:5)
names <- append(names, paste0("Quiz_", 1:7))

## Create the gradebook_matrix with 1200 observations and reshape it to a matrix.
gradebook_matrix <- matrix(runif(n=1200, min = 0, max = 100), ncol=length(names), byrow=TRUE)

## Rename the columns
colnames(gradebook_matrix) <- names

## Create the gradebook dataframe with the UID column and the gradebook_matrix.
gradebook <- data.frame(UID=123456789+(1:100), gradebook_matrix)