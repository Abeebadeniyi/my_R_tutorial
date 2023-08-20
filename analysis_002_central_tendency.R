# =========================MEASURE OF CENTRAL TENDENCY==========================

# packages section-------------------------------------------------------------

library(tidyverse)


# mean and median: mostly used on numeric data. Example and proof:--------------

numeric_vector <-  c(1,3,2,2,4,5,6) 
char_vector <-  c("a","b","d", "c", "a") 

mean(numeric_vector)    # gives the desired result

mean(char_vector)      # now R is unsure by giving us NA as the result

median(numeric_vector)  # gives the desired result

median(char_vector)   # now R is unsure by giving us NA as the result. see the
                          # error message yourself.

# mode is another measure of the centre. works well with numeric and categorical
# data. Example and proof:-----------------------------------------------------

mode(numeric_vector)  # oops! R gives us the unintended result. this is because
                     # the mode() does something else in R unlike the mean()
                     # and median() functions. let us fix this by writing our
                     # own function that does the desired thing

mode(char_vector)   # same reason as above

# let us build a function that calculate the desired mode and let us name it
 # modal_func

modal_func <- function(x){
  
# specification and assumption: x is a vector of at least length 1
  
  
# 1. let us use the table() function to calculate the freq.count of vector x
  
freq_count <- table(x)

# 2. remember mode is the value with highest number of observation. Now let us 
# use the max function to determine the highest count:

max_value <-  max(freq_count)


# 3. Now, that we have dtermined the max count. let us determine the values(s)
#  with the highest number of count. this will be our desired result

result <-  names(freq_count)[freq_count == max_value] # this means give me the 
 # name(s) in my table(i.e freq_count table) for which the value is the maximum


# we could stop here and return the value. But what if there is no mode( all the
# values in our vectors all have the same occurence or count. what do we want
# our function to return? thus, we need to proceed further and fix this:


if (length(result) == length(freq_count)){
  
  result <- NA
}else{
  
  return(result) 
     }
result
}


# now let use this newly minted function to calculate the two vectors we have
# created ------------------------

modal_func(numeric_vector) # voila! we got our desired result.
modal_func(char_vector)    # voila! we got our intended result.
# thus, we can use mode on both numeric and categorical data


# now let us modified the char_vector to see what the modal_fuc we just created
# will return if the values all have equal occurence. the expected result should 
# be NA just as we have specified within our function

char_vector_2 <- c("a", "b", "c", "d")

modal_func(char_vector_2) # BAM! got the expected result



# let us simulate some data now-----------------------------------------------


# a dataframe of the height and weight of 20 individuals
height_and_weight_20 <- tribble(
  ~id,   ~sex,     ~ht_in, ~wt_lbs,
  "001", "Male",   71,     190,
  "002", "Male",   69,     177,
  "003", "Female", 64,     130,
  "004", "Female", 65,     153,
  "005", NA,       73,     173,
  "006", "Male",   69,     182,
  "007", "Female", 68,     186,
  "008", NA,       73,     185,
  "009", "Female", 71,     157,
  "010", "Male",   66,     155,
  "011", "Male",   71,     213,
  "012", "Female", 69,     151,
  "013", "Female", 66,     147,
  "014", "Female", 68,     196,
  "015", "Male",   75,     212,
  "016", "Female", 69,     19000,
  "017", "Female", 66,     194,
  "018", "Female", 65,     176,
  "019", "Female", 65,     176,
  "020", "Female", 65,     102
)

# let us calculate the mean,median and mode of the heigh(in inches) for these

mean(height_and_weight_20$ht_in)

median(height_and_weight_20$ht_in)

modal_func(height_and_weight_20$ht_in)

# the result of the modal_func() is a character(look at the quatation mark. 
# we can fix this by coercing it to a numeric using as.numeric() finction

modal_func(height_and_weight_20$ht_in) %>% as.numeric() # fixed!!!


# Now, let us have the mean, mode and median of ht_in display to us all-together

# method 1: use reframe() when dealing with ungrouped data
height_and_weight_20 %>% 
  reframe(mean_ht_in = mean(ht_in),
            median_ht_in = median(ht_in),
            mode_ht_in = modal_func(ht_in)
)


# method 2: using summarise(), which is always used togther with grouped data
# but can still be used with ungrouped data. Notice the message given after 
# running this code:

height_and_weight_20 %>% 
  summarise(mean_ht_in = mean(ht_in),
          median_ht_in = median(ht_in),
          mode_ht_in = modal_func(ht_in)
)             # gave the intended result but see the message on your console



# Dealing with missing values---------------------------------------------------


# let us simulate a data:

vec <- c(1,2,3,NA,4)

mean(vec) # R returns NA because NA does not mean the absence of a value in R.
# Rather, it means the value exist but it is not known or it is unavailable at
# the moment. So, R is unsure what to return and simply return NA, which is 
# reasonably logical. 



# we can tell R to ignore the NA by using na.rm argument:
mean(vec, na.rm = TRUE) # Now R returns a value. this is called complete case
                      # analysis. which means dropping values or rows with NAs

# In the above example, NA appear in a vector. let us see how will can deal 
# with NAs when it appears in a dataframe:



# Let us introduce NAs into row 1 and 2 of the ht_in variable

height_and_weight_20 <- height_and_weight_20 %>% 
                       mutate(ht_in = replace(ht_in, c(1,2), NA))

print(height_and_weight_20)       # view it worked.



# let us display the mean, mode, max, min, median of ht_in using tidtyverse way

height_and_weight_20 %>% 
                        reframe(min_ht_in = min(ht_in),
                                mean_ht_in = mean(ht_in),
                                median_ht_in = median(ht_in),
                                mode_ht_in = modal_func(ht_in),
                                max_ht_in = max(ht_in)
                                )

# did you observed lots of NAs except for the mode_ht_in. let us fix this


# fixing the NAs in our result:------------------------------------------

# Method 1. using the na.rm = TRUE :

height_and_weight_20 %>% 
  reframe(min_ht_in = min(ht_in, na.rm = TRUE),
          mean_ht_in = mean(ht_in, na.rm = TRUE),
          median_ht_in = median(ht_in, na.rm = TRUE),
          mode_ht_in = modal_func(ht_in, na.rm = TRUE)%>% as.numeric(),
          max_ht_in = max(ht_in,na.rm = TRUE)
  ) # read the error message. the error originated from our modal_func becuz
  # we did not specify na.rm in when writing our function. Let us fix and
  # optimise our fuction defining the na.rm = TRUE as default


# optimising the modal_fuc():


modal_func_optimised <- function(x, na.rm = TRUE){
  
  # specification and assumption: x is a vector of at least length 1
  
  
  # 1. let us use the table() function to calculate the freq.count of vector x
  
  freq_count <- table(x)
  
  # 2. remember mode is the value with highest number of observation. Now let us 
  # use the max function to determine the highest count:
  
  max_value <-  max(freq_count)
  
  
  # 3. Now, that we have dtermined the max count. let us determine the values(s)
  #  with the highest number of count. this will be our desired result
  
  result <-  names(freq_count)[freq_count == max_value] # this means give me the 
  # name(s) in my table(i.e freq_count table) for which the value is the maximum
  
  
  # we could stop here and return the value. But what if there is no mode( all the
  # values in our vectors all have the same occurence or count. what do we want
  # our function to return? thus, we need to proceed further and fix this:
  
  
  if (length(result) == length(freq_count)){
    
    result <- NA
  }else{
    
    return(result) 
  }
  result
}

# let us re-run our codes----------------------------------------------------- 


height_and_weight_20 %>% 
  reframe(min_ht_in = min(ht_in, na.rm = TRUE),
          mean_ht_in = mean(ht_in, na.rm = TRUE),
          median_ht_in = median(ht_in, na.rm = TRUE),
          mode_ht_in = modal_func_optimised(ht_in, na.rm = TRUE)%>% as.numeric(),
          max_ht_in = max(ht_in,na.rm = TRUE) 
  ) # Awesome!!!. we got no error message this time. Our optimisation worked




# let us use mean, mode, and median to determine an error within our the------- 
# height_and_weight_20---------------------------------------------------------




# we are going to us the wt_lbs column-------


height_and_weight_20 %>% 
  reframe(min_wt_lbs = min(wt_lbs, na.rm = TRUE),
          mean_wt_lbs = mean(wt_lbs, na.rm = TRUE),
          median_wt_lbs = median(wt_lbs, na.rm = TRUE),
          mode_wt_lbs= modal_func_optimised(wt_lbs, na.rm = T)%>% as.numeric(),
          max_wt_lbs= max(wt_lbs,na.rm = TRUE) 
  ) # notice the maximum how the maximum weight is 19000, a value which is not
    # probable to occur in reality, Also notice how the mean is sensitive to 
    # this extreme value(1900) and how the mode and the median are not.



# using the meantables package to get the mean result.:

install.packages("meantables")
library("meantables")

# use the mean_table fuction from the meantables package to get the result:

# methdod 1: nested way
mean_table(height_and_weight_20, wt_lbs)

#method 2: tidyverse way:

height_and_weight_20 %>% mean_table(wt_lbs)




# Hurray!!! We are done



#=============================== THE END=======================================


























