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

multiple_choice_responses %>%
    # rename the appropriate levels to "High school" and "Some college"
    mutate(FormalEducation = fct_recode(FormalEducation,
    "High school" = "I did not complete any formal education past high school", 
    "Some college" = "Some college/university study without earning a bachelor's degree")) %>%
    # make a bar plot of FormalEducation
    ggplot(aes(x = FormalEducation)) + 
    geom_bar()

multiple_choice_responses %>%
    # Create new variable, grouped_titles, by collapsing levels in CurrentJobTitleSelect
    mutate(grouped_titles = fct_collapse(CurrentJobTitleSelect, 
        "Computer Scientist" = c("Programmer", "Software Developer/Software Engineer"), 
        "Researcher" = "Scientist/Researcher", 
        "Data Analyst/Scientist/Engineer" = c("DBA/Database Engineer", "Data Scientist", 
                                              "Business Analyst", "Data Analyst", 
                                              "Data Miner", "Predictive Modeler"))) %>%
    # Keep all the new titles and turn every other title into "Other"
    mutate(grouped_titles = fct_other(grouped_titles, 
                             keep = c("Computer Scientist", 
                                     "Researcher", 
                                     "Data Analyst/Scientist/Engineer"))) %>% 
    # Get a count of the grouped titles
    count(grouped_titles)

multiple_choice_responses %>%
  # remove NAs of MLMethodNextYearSelect
  filter(!is.na(MLMethodNextYearSelect)) %>%
  # create ml_method, which lumps all those with less than 5% of people into "Other"
  mutate(ml_method = fct_lump(MLMethodNextYearSelect, prop = 0.05)) %>%
  # count the frequency of your new variable, sorted in descending order
  count(ml_method, sort = TRUE)

multiple_choice_responses %>%
  # remove NAs 
  filter(!is.na(MLMethodNextYearSelect)) %>%
  # create ml_method, retaining the 5 most common methods and renaming others "other method" 
  mutate(ml_method = fct_lump(MLMethodNextYearSelect, n = 5, other_level = "other method")) %>%
  # count the frequency of your new variable, sorted in descending order
  count(ml_method, sort = TRUE)

learning_platform_usefulness <- multiple_choice_responses %>%
  # select columns with LearningPlatformUsefulness in title
  select(contains("LearningPlatformUsefulness")) %>%
  # change data from wide to long
  gather(learning_platform, usefulness) %>%
  # remove rows where usefulness is NA
  filter(!is.na(usefulness)) %>%
  # remove "LearningPlatformUsefulness" from each string in learning_platform 
  mutate(learning_platform = str_remove(learning_platform, "LearningPlatformUsefulness"))

