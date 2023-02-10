survey = readRDS(url("https://github.com/Adv-R-Programming/Adv-R-Reader/raw/main/class_survey.rds"))

example_vector = c(1, 2, 6, 8, 4, 2, 8, 2, 7, 10, 33)

example_function = function(num_vec) {
  
  # get the mean
  vec_mean = mean(num_vec)
  
  # get the median
  vec_median = median(num_vec)
  
  # get the mode !!!! except this function does not get the mode, we need to do that manually
  vec_mode = mode(num_vec)
  
  # create named vector for output
  output = c("mean" = vec_mean, "median" = vec_median, "mode" = vec_mode)
  
  # make sure all results are numeric
  if(!all(is.numeric(output))){stop("Not all values are numeric!")}
  
  # return results
  return(output)
}

example_function(example_vector)


# ************** Question 1 ***************
# get our class favorite characters
char_vec = survey$fav_char

puzzle_1 = function(characters) {
  
  # sort chars by alphabetical order
  sorted_char = sort(char_vec)
  
  # get the first letter of each name
  char_letters = substr(x = sorted_char, start = 1, stop = 1)
  
  # create a dataframe of character and their initial
  char_df = data.frame("char_name" = sorted_char, "char_initial" = char_letters)
  
  # randomly flip a count for each char
  char_df$toss_1 = sample(x = c("heads", "tails"),
                          size = nrow(char_df),
                          replace = TRUE)
  
  # for each that got heads, flip again, those with tails are out
  char_df$toss_2 = ifelse(char_df$toss_1 == "heads",
                          sample(x = c("heads", "tails"),
                                 size = nrow(char_df),
                                 replace = TRUE),
                          NA)
  
  # do it again
  char_df$toss_3 = ifelse(char_df$toss_2 == "heads",
                          sample(x = c("heads", "tails"),
                                 size = nrow(char_df),
                                 replace = TRUE),
                          NA)
  
  # add TRUE / FALSE for those with 3 heads
  ## set to TRUE if the 3rd toss is heads
  ## (as the other two had to be heads to toss a third time)
  char_df$lucky = ifelse(char_df$toss_3 == "heads", TRUE, FALSE)
  
  ## fill NAs with FALSE
  char_df[is.na(char_df$lucky), "lucky"] = FALSE
  
  # return results
  return(char_df)
}

puzzle_1(char_vec)


# ************** Question 2 **************

puzzle_2 = function(survey_dataframe) {
  
  # pivot the survey data from wide to long
  survey_long = tidyr::pivot_longer(survey_dataframe, cols = -fav_char, values_transform = as.character)
  
  # get all the questions people answered TRUE
  # REMOVE NAS
  all_true = survey_long[survey_long$value == TRUE, ]
  
  # count the number of rows (the number of questions with answers of TRUE)
  num_true = nrow(na.omit(all_true))
  
  # return that number
  return(num_true)
}

puzzle_2(survey)


