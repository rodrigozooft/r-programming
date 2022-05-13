# Print out the dataset
print(multiple_choice_responses)

# Check if CurrentJobTitleSelect is a factor
is.factor(multiple_choice_responses$CurrentJobTitleSelect)

# Change all the character columns to factors
responses_as_factors <- multiple_choice_responses %>%
    mutate_if(is.character, as.factor)

number_of_levels <- responses_as_factors %>%
	# apply the function nlevels to each column
    summarise_all(nlevels) %>%
    # change the dataset from wide to long
    gather(variable, num_levels)

# Select the 3 rows with the highest number of levels
number_of_levels %>%
    top_n(3, num_levels)
    
number_of_levels %>%
	# filter for where the column called variable equals CurrentJobTitleSelect
    filter(variable == "CurrentJobTitleSelect") %>%
	# pull num_levels
    pull()

responses_as_factors %>%
    # pull CurrentJobTitleSelect
    pull(CurrentJobTitleSelect) %>%
    # get the values of the levels
    levels()
# Make a bar plot
ggplot(multiple_choice_responses, aes(fct_infreq(EmployerIndustry))) + 
    geom_bar() + 
    # flip the coordinates
    coord_flip()

# Make a bar plot
ggplot(multiple_choice_responses, aes(x = fct_rev(fct_infreq(EmployerIndustry)))) + 
    geom_bar() + 
    # flip the coordinates
    coord_flip()

multiple_choice_responses %>%
  # remove NAs
  filter(!is.na(EmployerIndustry) & !is.na(Age)) %>%
  # get mean_age by EmployerIndustry
  group_by(EmployerIndustry) %>%
  summarise(mean_age = mean(Age)) %>%
  # reorder EmployerIndustry by mean_age 
  mutate(EmployerIndustry = fct_reorder(EmployerIndustry, mean_age)) %>%
  # make a scatterplot of EmployerIndustry by mean_age
  ggplot(aes(x = EmployerIndustry, y = mean_age)) + 
    geom_point() + 
    coord_flip()    

# Make a bar plot of the responses
ggplot(mc_responses_reordered, aes(WorkInternalVsExternalTools)) + 
    geom_bar() + 
    coord_flip()

multiple_choice_responses %>%
    # Move "I did not complete any formal education past high school" and "Some college/university study without earning a bachelor's degree" to the front
    mutate(FormalEducation = fct_relevel(FormalEducation, "I did not complete any formal education past high school", "Some college/university study without earning a bachelor's degree")) %>%
    # Move "I prefer not to answer" to be the last level.
    mutate(FormalEducation = fct_relevel(FormalEducation, "I prefer not to answer", after = Inf)) %>%
    # Move "Doctoral degree" to be after the 5th level
    mutate(FormalEducation = fct_relevel(FormalEducation, "Doctoral degree", after = 5)) %>%
    # Examine the new level order
    pull(FormalEducation) %>%
    levels()
