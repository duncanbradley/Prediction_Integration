library(tidyverse)


# FA Processing
  # read in the data
  # separate the columns into file and count
FA_detect_script_error <- read_csv('Batches/FA_detect_script_error.csv', col_names = FALSE) %>%
  separate(col = X1, sep = ":", into = c("file", "count")) 


# replace asc with DA1 - this is what .lst needs
FA_detect_script_error$file <- str_replace(string = FA_detect_script_error$file, 
                                        pattern = "asc", 
                                        replacement = "DA1")

# non-problematic trials
FA_detect_script_error %>% 
  filter(count == 0) %>%
  select(file) %>%
  write_csv(path = 'FA_filenames_corr.csv', col_names = FALSE)

# problematic trials
FA_detect_script_error %>% 
  filter(count > 0) %>%
  select(file) %>%
  write_csv(path = 'FA_filenames_error.csv', col_names = FALSE)

# where count = 2, likely due to recalibration


# ED Processing
# read in the data
# separate the columns into file and count
ED_detect_script_error <- read_csv('Batches/ED_detect_script_error.csv', col_names = FALSE) %>%
  separate(col = X1, sep = ":", into = c("file", "count")) 


# replace asc with DA1 - this is what .lst needs
ED_detect_script_error$file <- str_replace(string = ED_detect_script_error$file, 
                                           pattern = "asc", 
                                           replacement = "DA1")

# non-problematic trials
ED_detect_script_error %>% 
  filter(count == 0) %>%
  select(file) %>%
  write_csv(path = 'ED_filenames_corr.csv', col_names = FALSE)

# problematic trials
ED_detect_script_error %>% 
  filter(count > 0) %>%
  select(file) %>%
  write_csv(path = 'ED_filenames_error.csv', col_names = FALSE)

# where count = 2, likely due to recalibration


#### CREATING BATCHES - FixAlign

# files with no error in script (correct)
# read in data
FA_filenames_corr <- read_csv('Batches/FA_filenames_corr.csv', col_names = FALSE) #57 obs. 20 20 17

# assign batch numbers
batch_numbers_FA_corr <- c(rep("batch1", 20),
  rep("batch2", 20),
  rep("batch3", 17))

# add batch numbers as a column
FA_filenames_corr <- FA_filenames_corr %>%
  cbind(batch_numbers_FA_corr)

# write separate .csv files
FA_filenames_corr %>%
  filter(batch_numbers_FA_corr == "batch1") %>%
  select(X1) %>%
  write_csv(path = 'FA_batch1_corr.csv', col_names = FALSE)

FA_filenames_corr %>%
  filter(batch_numbers_FA_corr == "batch2") %>%
  select(X1) %>%
  write_csv(path = 'FA_batch2_corr.csv', col_names = FALSE)

FA_filenames_corr %>%
  filter(batch_numbers_FA_corr == "batch3") %>%
  select(X1) %>%
  write_csv(path = 'FA_batch3_corr.csv', col_names = FALSE)

# files with error in script 
# read in data
FA_filenames_error <- read_csv('FA_filenames_error.csv', col_names = FALSE) #36 obs. 20 16

# assign batch numbers
batch_numbers_FA_error <- c(rep("batch4", 20),
                           rep("batch5", 16))

# add batch numbers as a column
FA_filenames_error <- FA_filenames_error %>%
  cbind(batch_numbers_FA_error)

# write separate csv files
FA_filenames_error %>%
  filter(batch_numbers_FA_error == "batch4") %>%
  select(X1) %>%
  write_csv(path = 'FA_batch4_error.csv', col_names = FALSE)

FA_filenames_error %>%
  filter(batch_numbers_FA_error == "batch5") %>%
  select(X1) %>%
  write_csv(path = 'FA_batch5_error.csv', col_names = FALSE)

#### CREATING BATCHES - EyeDoctor

# files with no error in script (correct)
# read in data
ED_filenames_corr <- read_csv('Batches/ED_filenames_corr.csv', col_names = FALSE) # 61 obs. 20 20 21

# assign batch numbers
batch_numbers_ED_corr <- c(rep("batch1", 20),
                           rep("batch2", 20),
                           rep("batch3", 21))

# add batch numbers as a column
ED_filenames_corr <- ED_filenames_corr %>%
  cbind(batch_numbers_ED_corr)

# write separate .csv files
ED_filenames_corr %>%
  filter(batch_numbers_ED_corr == "batch1") %>%
  select(X1) %>%
  write_csv(path = 'ED_batch1_corr.csv', col_names = FALSE)

ED_filenames_corr %>%
  filter(batch_numbers_ED_corr == "batch2") %>%
  select(X1) %>%
  write_csv(path = 'ED_batch2_corr.csv', col_names = FALSE)

ED_filenames_corr %>%
  filter(batch_numbers_ED_corr == "batch3") %>%
  select(X1) %>%
  write_csv(path = 'ED_batch3_corr.csv', col_names = FALSE)

# files with error in script
# read in data
ED_filenames_error <- read_csv('ED_filenames_error.csv', col_names = FALSE) #39 obs. 20 19

# assign batch numbers
batch_numbers_ED_error <- c(rep("batch4", 20),
                            rep("batch5", 19))

# add batch numbers as a column
ED_filenames_error <- ED_filenames_error %>%
  cbind(batch_numbers_ED_error)

# write separate .csv files
ED_filenames_error %>%
  filter(batch_numbers_ED_error == "batch4") %>%
  select(X1) %>%
  write_csv(path = 'ED_batch4_error.csv', col_names = FALSE)

ED_filenames_error %>%
  filter(batch_numbers_ED_error == "batch5") %>%
  select(X1) %>%
  write_csv(path = 'ED_batch5_error.csv', col_names = FALSE)


