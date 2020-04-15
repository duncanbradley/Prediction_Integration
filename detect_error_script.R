library(tidyverse)

# read in the data
# separate the columns into file and count
detect_script_error <- read_csv('detect_script_error.csv', col_names = FALSE) %>%
  separate(col = X1, sep = ":", into = c("file", "count")) 


# replace asc with DA1 - this is was .lst needs
detect_script_error$file <- str_replace(string = detect_script_error$file, 
                                        pattern = "asc", 
                                        replacement = "DA1")

# non-problematic trials
detect_script_error %>% 
  filter(count == 0) %>%
  select(file) %>%
  write_csv(path = 'filenames_corr.csv', col_names = FALSE)

# problematic trials
detect_script_error %>% 
  filter(count > 0) %>%
  select(file) %>%
  write_csv(path = 'filenames_error.csv', col_names = FALSE)

# where count = 2, likely due to recalibration

