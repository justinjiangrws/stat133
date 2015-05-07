# Midterm 3

# Write a function called numStarElements. Your function should take the following
# arguments
#   <chvec>: A character vector containing strings of letters, possibly 
#     with the "*" symbol
#
# and return the following
#   <num.star>: an integer indicating how many elements of <chvec> contain the "*"
#     symbol. For example: numStarElements(c('star', 'st*r', '***')) should return 2
numStarElements <- function(chvec){
  ats <- grep("\\*", chvec)
  return(length(ats))
}

# Write a function called numDigits that count the number of (single) digits in
# a string.  The function should return 0 if there is no digit in the
# string. Your function should take the following arguments:
#   <chvec>: A character vector of length 1 (chvec contains only 1 string)
#
# and return the following
#   <total>: A single number (the number of digits in chvec)
numDigits <- function(chvec){
  
  temp <- unlist(strsplit(chvec, ""))
  length(as.numeric(grep("[0-9]", temp, value = T)))
}

# Some test cases:
# all.equal(numDigits("1z3p ! 21"), 4)
# all.equal(numDigits("abcdefg"), 0)


# Write a function called hisToTheirs that converts every instance of him in a string to them; 
# every instance of he to they and every instance of his to theirs You can assume everything is
# lower case. Be careful not to replace words that contain him/he/his (eg you don't want to
# replace the with ther). Your function should take the argument
#   <chvec>: A character vector
#
# and return
#   <theirchvec>: The same character vector with the required substitutions.


hisToHers <- function(chvec) {
  herchvec <- gsub("\\<his\\>", "their", chvec)
  herchvec <- gsub("\\<he\\>", "they", herchvec)
  herchvec <- gsub("\\<him\\>", "them", herchvec)
  return(herchvec)
}

test7 <- hisToHers("he said yes")
test8 <- hisToHers("his hat was red")
test9 <- hisToHers("I saw him he")

# Write a function called mostCommonLetter that finds the most common letter in a 
# string. If there is a tie for most common letter return all of the letters that 
# were most common. Your function should convert all letters in the string to 
# *upper case* and you should remove everything other than letters. 
# Your function has the argument
#  <chvec>: A character vector of length 1 (chvec contains only 1 string)
#
# and it should return
#  <letter> The most common letter or letters in the string.
# For example mostCommonLetter("aabbccccdddd") should return 
# [1] "c" "d"

mostCommonLetter <- function(chvec) {
  chvec <- toupper(chvec)
  chvec <- gsub("[^[:alpha:]]", "", chvec)
  letts <- strsplit(chvec, "")
  tb <- table(letts)
  names(tb)[tb == max(tb)]
}

test4 <- mostCommonLetter("aaabbcc")
test5 <- mostCommonLetter("aaabbbcc")
