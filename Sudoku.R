

## 1st solution ##

#### building random matrix

s <- matrix(data = NA,9,9)

for (i in 1:9) {
  for (j in 1:9) {
    s[i,j] <- sample(1:9,1)
  }
}

print(s)

#### generate valid Sudoku matrix

s <- matrix(data = c(5,6,1,8,4,7,9,2,3,
                     3,7,9,5,2,1,6,8,4,
                     4,2,8,9,6,3,1,7,5,
                     6,1,3,7,8,9,5,4,2,
                     7,9,4,6,5,2,3,1,8,
                     8,NA,2,1,3,4,NA,9,6,
                     9,3,5,4,7,8,2,6,1,
                     1,4,6,2,9,5,8,3,7,
                     2,8,7,3,1,6,4,5,9),9,9)
print(s)
SolverOne(s)

#### generate verifier function

OnetoNineChecking <- function(x) {
  for (i in 1:9) {
    if (!(i %in% x)) {
      return(FALSE)
    }
  }
  return(TRUE)
}

#### generate verifier

verifier <- function(s) {
  for (n in 1:9) {
    if (!OnetoNineChecking(s[n,]) | !OnetoNineChecking(s[,n])) {
      return(FALSE)
    } 
  }
  
  for (i in 0:2) {
    for (j in 0:2) {
      if (!OnetoNineChecking(s[1:3+3*i,1:3+3*j])) {
       return(FALSE)
      } 
    }
  }
  return(TRUE)
}

verifier(s)

## Building solver for one empty space

BlankIndex <- function(s) {
  for (i in 1:9) {
    for (j in 1:9) {
      while (is.na(s[i,j])) {
        return(c(i,j))
      }
    }
  }
  return(FALSE)
}


SolverOne <- function(s) {

## base case
  index <- BlankIndex(s)
  if (index == FALSE) {
    print("base case")
    return(verifier(s))
  }

## recursive case  
  for (n in 1:9) {
    print(index)
    print(n)
    print("-----------")
    s[index[1],index[2]] <- n
    SolverOne(s)
    if (SolverOne(s)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

SolverOne(s)

