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