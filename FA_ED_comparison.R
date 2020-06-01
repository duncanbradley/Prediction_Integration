library(tidyverse)

# reading in total time fix_align files
TT_FA_batch1_corr <- read_csv('FA_EXCEL_FILES/TT_FA_batch1_corr')
TT_FA_batch2_corr <- read_csv('FA_EXCEL_FILES/TT_FA_batch2_corr')
TT_FA_batch3_corr <- read_csv('FA_EXCEL_FILES/TT_FA_batch3_corr')
TT_FA_batch4_error <- read_csv('FA_EXCEL_FILES/TT_FA_batch4_error')
TT_FA_batch5_error <- read_csv('FA_EXCEL_FILES/TT_FA_batch5_error')

# removing rows with no data
# removing R8 column, which has no data
TT_FA_b1_c <- TT_FA_batch1_corr %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  select(- R8)

TT_FA_b2_c <- TT_FA_batch2_corr %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  select(- R8)

TT_FA_b3_c <- TT_FA_batch3_corr %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  select(- R8)

TT_FA_b4_e <- TT_FA_batch4_error %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  select(- R8)

TT_FA_b5_e <- TT_FA_batch5_error %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  select(- R8)

# reading in the file lists
FA_b1_lst <- read_csv('Batches/FA_batch1_corr.lst', col_names = FALSE)
FA_b2_lst <- read_csv('Batches/FA_batch2_corr.lst', col_names = FALSE)
FA_b3_lst <- read_csv('Batches/FA_batch3_corr.lst', col_names = FALSE)
FA_b4_lst <- read_csv('Batches/FA_batch4_error.lst', col_names = FALSE)
FA_b5_lst <- read_csv('Batches/FA_batch5_error.lst', col_names = FALSE)

# extract just participant numbers from file names
FA_b1_lst$X1 <- FA_b1_lst$X1 %>%
  str_sub(6, 7)

FA_b2_lst$X1 <- FA_b2_lst$X1 %>%
  str_sub(6, 7)

FA_b3_lst$X1 <- FA_b3_lst$X1 %>%
  str_sub(6, 7)

FA_b4_lst$X1 <- FA_b4_lst$X1 %>%
  str_sub(6, 7)

FA_b5_lst$X1 <- FA_b5_lst$X1 %>%
  str_sub(6, 7)

# replacing participant numbers in the data files with participant numbers
# from the batch files

# for some reason this didn't work properly
#for (i in unique(TT_FA_b1_c$subj)) {
#  print(i)
#  print(FA_b1_lst$X1[i])
#  TT_FA_b1_c <- TT_FA_b1_c %>% 
#    mutate(subj = replace(subj, subj == i, FA_b1_lst$X1[i]))
#}

TT_FA_b1_c$subj <- mapvalues(TT_FA_b1_c$subj, unique(TT_FA_b1_c$subj), FA_b1_lst$X1)
TT_FA_b2_c$subj <- mapvalues(TT_FA_b2_c$subj, unique(TT_FA_b2_c$subj), FA_b2_lst$X1)
TT_FA_b3_c$subj <- mapvalues(TT_FA_b3_c$subj, unique(TT_FA_b3_c$subj), FA_b3_lst$X1)
TT_FA_b4_e$subj <- mapvalues(TT_FA_b4_e$subj, unique(TT_FA_b4_e$subj), FA_b4_lst$X1)
TT_FA_b5_e$subj <- mapvalues(TT_FA_b5_e$subj, unique(TT_FA_b5_e$subj), FA_b5_lst$X1)


TT_FA_b1_c %>% ggplot(aes(subj)) +
  geom_bar()

TT_FA_b2_c %>% ggplot(aes(subj)) +
  geom_bar()

TT_FA_b3_c %>% ggplot(aes(subj)) +
  geom_bar()

TT_FA_b4_e %>% ggplot(aes(subj)) +
  geom_bar()

TT_FA_b5_e %>% ggplot(aes(subj)) +
  geom_bar()


# binding all together
TT_FA <- rbind(TT_FA_b1_c,
               TT_FA_b2_c,
               TT_FA_b3_c,
               TT_FA_b4_e,
               TT_FA_b5_e) %>%
  mutate(method = "FA")

#----------------------------------

# reading in total time Eye Doctor files 
TT_ED_b1_c <- read_csv('ED_EXCEL_FILES/TT_ED_batch1_corr.csv')
TT_ED_b2_c <- read_csv('ED_EXCEL_FILES/TT_ED_batch2B_corr.csv')
TT_ED_b3_c <- read_csv('ED_EXCEL_FILES/TT_ED_batch3_corr.csv')
TT_ED_b4_e <- read_csv('ED_EXCEL_FILES/TT_ED_batch4_error.csv')
TT_ED_b5_e <- read_csv('ED_EXCEL_FILES/TT_ED_batch5_error.csv')

# reading in the file lists
ED_b1_lst <- read_csv('Batches/ED_batch1_corr.lst', col_names = FALSE)
ED_b2_lst <- read_csv('Batches/ED_batch2_corr.lst', col_names = FALSE)
ED_b3_lst <- read_csv('Batches/ED_batch3_corr.lst', col_names = FALSE)
ED_b4_lst <- read_csv('Batches/ED_batch4_error.lst', col_names = FALSE)
ED_b5_lst <- read_csv('Batches/ED_batch5_error.lst', col_names = FALSE)

# extract just participant numbers from file names
ED_b1_lst$X1 <- ED_b1_lst$X1 %>%
  str_sub(4, 5)

ED_b2_lst$X1 <- ED_b2_lst$X1 %>%
  str_sub(4, 5)

ED_b3_lst$X1 <- ED_b3_lst$X1 %>%
  str_sub(4, 5)

ED_b4_lst$X1 <- ED_b4_lst$X1 %>%
  str_sub(4, 5)

ED_b5_lst$X1 <- ED_b5_lst$X1 %>%
  str_sub(4, 5)

# replacing participant numbers in the data files with participant numbers
# from the batch files
TT_ED_b1_c$subj <- mapvalues(TT_ED_b1_c$subj, unique(TT_ED_b1_c$subj), ED_b1_lst$X1)
TT_ED_b2_c$subj <- mapvalues(TT_ED_b2_c$subj, unique(TT_ED_b2_c$subj), ED_b2_lst$X1)
TT_ED_b3_c$subj <- mapvalues(TT_ED_b3_c$subj, unique(TT_ED_b3_c$subj), ED_b3_lst$X1)
TT_ED_b4_e$subj <- mapvalues(TT_ED_b4_e$subj, unique(TT_ED_b4_e$subj), ED_b4_lst$X1)
TT_ED_b5_e$subj <- mapvalues(TT_ED_b5_e$subj, unique(TT_ED_b5_e$subj), ED_b5_lst$X1)

# Batch2 has an error message because the data has 19 rows but there are
# 20 replacement participant numbers
# Participant 52 was not included, so this is removed from the replacement list:
ED_b2_lst <- ED_b2_lst %>%
  filter(X1 != 52)

TT_ED_b2_c$subj <- mapvalues(TT_ED_b2_c$subj, unique(TT_ED_b2_c$subj), ED_b2_lst$X1)



# binding all together
TT_ED <- rbind(TT_ED_b1_c,
               TT_ED_b2_c,
               TT_ED_b3_c,
               TT_ED_b4_e,
               TT_ED_b5_e) %>%
  mutate(R7 = NA,
         method = "ED") 
  

#-----------------------------------------

n <- c(ED_b1_lst$X1, ED_b2_lst$X1, ED_b3_lst$X1, ED_b4_lst$X1, ED_b5_lst$X1)
EDnums <- data.frame(n)
duplicated(EDnums$n)
#35 is duplicated - this shouldn't matter because 35 is not in the FA data

n <- c(FA_b1_lst$X1, FA_b2_lst$X1, FA_b3_lst$X1, FA_b4_lst$X1, FA_b5_lst$X1)
FAnums <- data.frame(n)
duplicated(FAnums$n)
#none duplicated

# Participant 52 is not in the FA data
52 %in% FAnums$n

# entire total time dataset with both methods
# filtering to include only 

TT <- rbind(TT_ED, TT_FA) 

TT$subj <- as.character(TT$subj)
str(TT$subj)

TT <- TT %>% filter(TT$subj %in% FAnums$n)

TT %>% ggplot(aes(subj)) +
  geom_bar()

TT %>% 
  pivot_longer(cols = R1:R7, names_to = "regions", values_to = "values") %>%
  ggplot(aes(x = regions, 
             y = values,
             colour = method)) +
  geom_point(position = position_jitterdodge(),
             alpha = 0.1) +
  theme_minimal()

#find NAs
TT %>% 
  group_by(method) %>%
  summarise_all(~ sum(is.na(.)))

TT %>% 
  group_by(method) %>%
  summarise_all(~ sum(!is.na(.)))

TT %>% 
  group_by(method) %>%
  summarise(R7 = sum(!is.na(R7)))

# mean and sd for both groups
TT %>%
  group_by(method) %>%
  summarise_at(vars(R1:R7), mean, na.rm = TRUE)

TT %>%
  group_by(method) %>%
  summarise_at(vars(R1:R7), sd, na.rm = TRUE)

TT %>%
  group_by(method) %>%
  summarise_at(vars(R1:R7), n, na.rm = TRUE)

# grand total of fixations
totals <- aggregate(R1 ~ method, data = TT, FUN = sum) %>%
  cbind("R2" = 
          aggregate(R2 ~ method, data = TT, FUN = sum)$R2,
        "R3" =
          aggregate(R3 ~ method, data = TT, FUN = sum)$R3,
        "R4" = 
          aggregate(R4 ~ method, data = TT, FUN = sum)$R4,
        "R5" = 
          aggregate(R5 ~ method, data = TT, FUN = sum)$R5,
        "R6" = 
          aggregate(R6 ~ method, data = TT, FUN = sum)$R6,
        "R7" = 
          c("0", aggregate(R7 ~ method, data = TT, FUN = sum)$R7))

totals <- melt(totals, id = "method")

totals$value <- as.numeric(totals$value)

totals %>% ggplot(aes(x = variable,
                       y = value,
                       colour = method)) +
  geom_point()

totals <- totals %>%  pivot_wider(names_from = method, values_from = value)

sum(totals$ED)
sum(totals$FA)

#calculate and visualise difference (subtraction) between the same observations

diffs <- TT %>% 
  select(subj, item, cond, R1, R2, R3, R4, R5, R6, R7, method) %>%
  pivot_wider(names_from = method, 
              values_from = c(R1, R2, R3,R4, R5, R6, R7)) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(
    R1diff = R1_ED - R1_FA,
    R2diff = R2_ED - R2_FA,
    R3diff = R3_ED - R3_FA,
    R4diff = R4_ED - R4_FA,
    R5diff = R5_ED - R5_FA,
    R6diff = R6_ED - R6_FA,
    R7diff = R7_ED - R7_FA) %>%
  select(subj, item, cond, R1diff, R2diff, R3diff, R4diff, R5diff, R6diff, R7diff)


diffs %>%
  summarise_at(vars(R1diff:R7diff), mean, na.rm = TRUE)

diffs %>%
  summarise_at(vars(R1diff:R7diff), sd, na.rm = TRUE)

diffs_longer <- diffs %>%
  pivot_longer(cols = R1diff:R7diff, names_to = "regions", values_to = "values")

diffs_longer %>% ggplot(aes(x = regions,
                     y = values)) +
  geom_jitter(width = 0.1,
              alpha = 0.05) +
  theme_minimal()


# visualisations - do not really give that much info
TT %>% 
  ggplot(aes(x = R1,
             y = method,
             colour = method)) +
  geom_violin()

TT %>% 
  ggplot(aes(x = R2,
             y = method,
             colour = method)) +
  geom_violin()

TT %>% 
  ggplot(aes(x = R3,
             y = method,
             colour = method)) +
  geom_violin()

TT %>% 
  ggplot(aes(x = R4,
             y = method,
             colour = method)) +
  geom_violin()

TT %>% 
  ggplot(aes(x = R5,
             y = method,
             colour = method)) +
  geom_violin()

TT %>% 
  ggplot(aes(x = R6,
             y = method,
             colour = method)) +
  geom_violin()
