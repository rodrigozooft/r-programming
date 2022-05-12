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