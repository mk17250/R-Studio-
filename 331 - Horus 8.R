require(tidyr)
require(dplyr)  
     
require(dslabs)
data("reported_heights")
reported_heights

class(reported_heights$height)

new_ht <- as.numeric(reported_heights$height)

sum(is.na(new_ht)) # count how many NAs in the x vector

require(dplyr)
mutate(reported_heights, new_ht = suppressWarnings(as.numeric(height))) %>% # supress the NAs warning
  filter(is.na(new_ht)) %>%
  head(n = 10)

not_inches <- function(x, smallest = 50, tallest = 84){
  inches = suppressWarnings(as.numeric(x))
  ind = is.na(inches) | inches < smallest | inches > tallest  # logical vector: TRUE refers to a problematic entry
  return(ind)
}

problems <- filter(reported_heights, not_inches(height)) %>%
  pull(height)
head(problems, n = 20)

length(problems)

pattern <- "^[4-7]'\\d{1,2}\"$"
require(stringr)
require(dplyr)
str_detect(problems, pattern) %>% sum

head(problems, n=20) %>% str_view(pattern)

str_subset(problems, "''|inches")                                

pattern <- "^[4-7]'\\d{1,2}$"
## replace different ways of representing feet and inches, then detect the pattern
problems %>%
  str_replace("feet|ft|foot", "'") %>%     # replace feet, ft, foot with '
  str_replace("inches|in|''|\"", "") %>%   # remove all inches symbols
  str_detect(pattern) %>%
  sum