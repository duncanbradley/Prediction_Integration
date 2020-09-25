library(tidyverse)
library(plyr)
library(dplyr)
library(naniar)
library(fitdistrplus)
library(lme4)
library(lmerTest)
library(broom)

# reading in total time fix_align files
TT_FA_batch1_corr <- read_csv('FA_files/TT_FA_batch1_corr')
TT_FA_batch2_corr <- read_csv('FA_files/TT_FA_batch2_corr')
TT_FA_batch3_corr <- read_csv('FA_files/TT_FA_batch3_corr')
TT_FA_batch4_error <- read_csv('FA_files/TT_FA_batch4_error')
TT_FA_batch5_error <- read_csv('FA_files/TT_FA_batch5_error')

# removing rows with no data
# removing R8 column, which has no data
TT_FA_b1_c <- TT_FA_batch1_corr %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  dplyr::select(- R8)

TT_FA_b2_c <- TT_FA_batch2_corr %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  dplyr::select(- R8)

TT_FA_b3_c <- TT_FA_batch3_corr %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  dplyr::select(- R8)

TT_FA_b4_e <- TT_FA_batch4_error %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  dplyr::select(- R8)

TT_FA_b5_e <- TT_FA_batch5_error %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  dplyr::select(- R8)

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
# this didn't work properly
#for (i in unique(TT_FA_b1_c$subj)) {
#  print(i)
#  print(FA_b1_lst$X1[i])
#  TT_FA_b1_c <- TT_FA_b1_c %>% 
#    mutate(subj = replace(subj, subj == i, FA_b1_lst$X1[i]))
#}

# replacing participant numbers in the data files with participant numbers
# from the batch files
TT_FA_b1_c$subj <- mapvalues(TT_FA_b1_c$subj, unique(TT_FA_b1_c$subj), FA_b1_lst$X1)
TT_FA_b2_c$subj <- mapvalues(TT_FA_b2_c$subj, unique(TT_FA_b2_c$subj), FA_b2_lst$X1)
TT_FA_b3_c$subj <- mapvalues(TT_FA_b3_c$subj, unique(TT_FA_b3_c$subj), FA_b3_lst$X1)
TT_FA_b4_e$subj <- mapvalues(TT_FA_b4_e$subj, unique(TT_FA_b4_e$subj), FA_b4_lst$X1)
TT_FA_b5_e$subj <- mapvalues(TT_FA_b5_e$subj, unique(TT_FA_b5_e$subj), FA_b5_lst$X1)


# binding all together
TT_FA <- rbind(TT_FA_b1_c,
               TT_FA_b2_c,
               TT_FA_b3_c,
               TT_FA_b4_e,
               TT_FA_b5_e) %>%
  mutate(method = "FA")

# visualising missing data
vis_miss(TT_FA)

#----------------------------------

# reading in total time Eye Doctor files 
TT_ED_b1_c <- read_csv('ED_files/TT_ED_batch1_corr.csv')
TT_ED_b2_c <- read_csv('ED_files/TT_ED_batch2B_corr.csv')
TT_ED_b3_c <- read_csv('ED_files/TT_ED_batch3_corr.csv')
TT_ED_b4_e <- read_csv('ED_files/TT_ED_batch4_error.csv')
TT_ED_b5_e <- read_csv('ED_files/TT_ED_batch5_error.csv')

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

# the participant numbers are replaced using the same code as before
# but now with P52 removed from the 'to' dataframe
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

n <- c(FA_b1_lst$X1, FA_b2_lst$X1, FA_b3_lst$X1, FA_b4_lst$X1, FA_b5_lst$X1)
FAnums <- data.frame(n)
duplicated(FAnums$n)
#none duplicated

# Participant 52 is not in the FA data
52 %in% FAnums$n

# entire total time dataset with both methods
TT <- rbind(TT_ED, TT_FA) 

TT$subj <- as.character(TT$subj)
str(TT$subj)

# filtering to include only participants in the fixalign dataset
TT <- TT %>% filter(TT$subj %in% FAnums$n)

TT %>% ggplot(aes(subj)) +
  geom_bar()

plot <- TT %>% 
  pivot_longer(cols = R1:R7, names_to = "regions", values_to = "values") %>%
  ggplot(aes(x = regions, 
             y = values,
             colour = method)) +
  geom_point(position = position_jitterdodge(),
             alpha = 0.1) +
  labs(y = "Total Fixation Time (ms)",
       x = NULL,
       colour = "Processing Method:") +
  scale_color_manual(labels = c("Manual", "Automatic"), 
                     values = c("darkblue", "orange")) +
  guides(colour = guide_legend(override.aes = list(alpha = 1)),
         colour = guide_legend(override.aes = list(size=20))) +
  theme_minimal(base_size = 20) +
  theme(legend.position="bottom")
plot
ggsave("plot1", plot, device = "png", path = "Batches",
       height = 4.5,
       width = 7)
  
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

plot <- diffs_longer %>% ggplot(aes(x = regions,
                     y = values)) +
  geom_jitter(width = 0.1,
              alpha = 0.05) +
  labs(y = "Difference in \nTotal Fixation Time (ms)",
       x = NULL) +
  scale_x_discrete(labels= c("R1", "R2", "R3", "R4", "R5", "R6", "R7")) +
  theme_minimal(base_size = 20)
plot
ggsave("plot1", plot, device = "png", path = "Batches",
       height = 4.5,
       width = 7)


# ANALYSIS ####

# download the FP and RPfrom the folders with these names in https://github.com/E-LeLuan/Duncans_Grant


# in the analysis I will use all available participants from the Eye Doctor process
# so rather than using the combined dataframe, I use the separate dataframes
length(unique(TT_ED$subj))
length(unique(TT_FA$subj))

# in the script 'Questions 01_10_18' 
# condition 1 is where prediction is facilitated
# condition 2 is where prediction is unfacilitated
# (for further verification: the error in item 16 occurred on the unfacilitated condition,
# which was condition 2)
TT_ED$cond <- recode(TT_ED$cond, `1` = "Facilitated", `2` = "Unfacilitated")
TT_ED$cond <- as.factor(TT_ED$cond)

# EYE DOCTOR ANALYSIS ####
# TOTAL TIME #

# using na.omit also introduces additional info (location of omitted values)
# (see https://statisticsglobe.com/na-omit-r-example/ )
# this is removed using as.numeric()
descdist(as.numeric(na.omit(TT_ED$R4)))
# looks closest to a gamma distribution
descdist(as.numeric(na.omit(TT_ED$R5)))
# this is closest to lognormal but will use gamma

TT_ED_R4 <- glmer(R4 ~ cond + (1 + cond | subj) + (1 + cond | item), 
                 family = Gamma(link = "identity"), 
                 data = TT_ED)
summary(TT_ED_R4)

TT_ED_R5 <- glmer(R5 ~ cond + (1 + cond | subj) + (1 + cond | item), 
                  family = Gamma(link = "identity"), 
                  data = TT_ED)
summary(TT_ED_R5)

TT_R4_means <- TT_ED %>% 
  group_by(cond) %>% 
  dplyr::summarise(mean = mean(R4, na.rm = TRUE))

TT_ED %>%
  ggplot(aes(x = cond,
             y = R4)) +
  geom_jitter() +
  geom_point(data = TT_R4_means, stat = "identity",
             aes(y = mean,
                 x = cond,
                 color = cond),
             size = 3)

TT_R5_means <- TT_ED %>% 
  group_by(cond) %>% 
  dplyr::summarise(mean = mean(R5, na.rm = TRUE))

TT_ED %>%
  ggplot(aes(x = cond,
             y = R5)) +
  geom_jitter() +
  geom_point(data = TT_R5_means, stat = "identity",
             aes(y = mean,
                 x = cond,
                 color = cond),
             size = 3)

table <- tidy(TT_ED_R4) %>%
  filter(group == "fixed",
         term != "(Intercept)") %>%
  cbind(Model = rep("TT_ED_R4")) %>%
  arrange(Model) %>%
  dplyr::select(- group)

table <- tidy(TT_ED_R5) %>%
  filter(group == "fixed",
         term != "(Intercept)") %>%
  cbind(Model = rep("TT_ED_R5")) %>%
  dplyr::select(- group) %>%
  rbind(table) %>%
  arrange(Model)

# FIRST PASS #
 
# reading in the files
# reading in total time fix_align files
FP_ED_b1_c <- read_csv('ED_files/FP_ED_batch1_corr.csv')
FP_ED_b2_c <- read_csv('ED_files/FP_ED_batch2B_corr.csv')
FP_ED_b3_c <- read_csv('ED_files/FP_ED_batch3_corr.csv')
FP_ED_b4_e <- read_csv('ED_files/FP_ED_batch4_error.csv')
FP_ED_b5_e <- read_csv('ED_files/FP_ED_batch5_error.csv')

# replacing participant numbers with numbers from participant lists
FP_ED_b1_c$subj <- mapvalues(FP_ED_b1_c$subj, unique(FP_ED_b1_c$subj), ED_b1_lst$X1)
FP_ED_b2_c$subj <- mapvalues(FP_ED_b2_c$subj, unique(FP_ED_b2_c$subj), ED_b2_lst$X1)

# Batch2 has an error message because the data has 19 rows but there are
# 20 replacement participant numbers
# Participant 52 was not included, so this is removed from the replacement list:
ED_b2_lst <- ED_b2_lst %>%
  filter(X1 != 52)

# the participant numbers are replaced using the same code as before
# but now with P52 removed from the 'to' dataframe
FP_ED_b2_c$subj <- mapvalues(FP_ED_b2_c$subj, unique(FP_ED_b2_c$subj), ED_b2_lst$X1)

FP_ED_b3_c$subj <- mapvalues(FP_ED_b3_c$subj, unique(FP_ED_b3_c$subj), ED_b3_lst$X1)
FP_ED_b4_e$subj <- mapvalues(FP_ED_b4_e$subj, unique(FP_ED_b4_e$subj), ED_b4_lst$X1)
FP_ED_b5_e$subj <- mapvalues(FP_ED_b5_e$subj, unique(FP_ED_b5_e$subj), ED_b5_lst$X1)

# binding all together
FP_ED <- rbind(FP_ED_b1_c,
               FP_ED_b2_c,
               FP_ED_b3_c,
               FP_ED_b4_e,
               FP_ED_b5_e) 

FP_ED$cond <- recode(FP_ED$cond, `1` = "Facilitated", `2` = "Unfacilitated")
FP_ED$cond <- as.factor(FP_ED$cond)

descdist(as.numeric(na.omit(FP_ED$R4)))
# looks closest to a gamma distribution
descdist(as.numeric(na.omit(FP_ED$R5)))
# this is closest to lognormal but will use gamma

FP_ED_R4 <- glmer(R4 ~ cond + (1 + cond | subj) + (1 + cond | item), 
                  family = Gamma(link = "identity"), 
                  data = FP_ED)
summary(TT_ED_R4)

FP_ED_R5 <- glmer(R5 ~ cond + (1 + cond | subj) + (1 + cond | item), 
                  family = Gamma(link = "identity"), 
                  data = FP_ED)
summary(FP_ED_R5)

FP_R4_means <- FP_ED %>% 
  group_by(cond) %>% 
  dplyr::summarise(mean = mean(R4, na.rm = TRUE))

FP_ED %>%
  ggplot(aes(x = cond,
             y = R4)) +
  geom_jitter() +
  geom_point(data = FP_R4_means, stat = "identity",
             aes(y = mean,
                 x = cond,
                 color = cond),
             size = 3)

FP_R5_means <- FP_ED %>% 
  group_by(cond) %>% 
  dplyr::summarise(mean = mean(R5, na.rm = TRUE))

FP_ED %>%
  ggplot(aes(x = cond,
             y = R5)) +
  geom_jitter() +
  geom_point(data = FP_R5_means, stat = "identity",
             aes(y = mean,
                 x = cond,
                 color = cond),
             size = 3)

table <- tidy(FP_ED_R4) %>%
  filter(group == "fixed",
         term != "(Intercept)") %>%
  cbind(Model = rep("FP_ED_R4")) %>%
  dplyr::select(- group) %>%
  rbind(table) %>%
  arrange(Model)

table <- tidy(FP_ED_R5) %>%
  filter(group == "fixed",
         term != "(Intercept)") %>%
  cbind(Model = rep("FP_ED_R5")) %>%
  dplyr::select(- group) %>%
  rbind(table) %>%
  arrange(Model)

# REGRESSION PATH #
  
# reading in the files
# reading in total time fix_align files
RP_ED_b1_c <- read_csv('ED_files/RP_ED_batch1_corr.csv')
RP_ED_b2_c <- read_csv('ED_files/RP_ED_batch2B_corr.csv')
RP_ED_b3_c <- read_csv('ED_files/RP_ED_batch3_corr.csv')
RP_ED_b4_e <- read_csv('ED_files/RP_ED_batch4_error.csv')
RP_ED_b5_e <- read_csv('ED_files/RP_ED_batch5_error.csv')

# replacing participant numbers with numbers from participant lists
RP_ED_b1_c$subj <- mapvalues(RP_ED_b1_c$subj, unique(RP_ED_b1_c$subj), ED_b1_lst$X1)
RP_ED_b2_c$subj <- mapvalues(RP_ED_b2_c$subj, unique(RP_ED_b2_c$subj), ED_b2_lst$X1)

# Batch2 has an error message because the data has 19 rows but there are
# 20 replacement participant numbers
# Participant 52 was not included, so this is removed from the replacement list:
ED_b2_lst <- ED_b2_lst %>%
  filter(X1 != 52)

# the participant numbers are replaced using the same code as before
# but now with P52 removed from the 'to' dataframe
RP_ED_b2_c$subj <- mapvalues(RP_ED_b2_c$subj, unique(RP_ED_b2_c$subj), ED_b2_lst$X1)

RP_ED_b3_c$subj <- mapvalues(RP_ED_b3_c$subj, unique(RP_ED_b3_c$subj), ED_b3_lst$X1)
RP_ED_b4_e$subj <- mapvalues(RP_ED_b4_e$subj, unique(RP_ED_b4_e$subj), ED_b4_lst$X1)
RP_ED_b5_e$subj <- mapvalues(RP_ED_b5_e$subj, unique(RP_ED_b5_e$subj), ED_b5_lst$X1)

# binding all together
RP_ED <- rbind(RP_ED_b1_c,
               RP_ED_b2_c,
               RP_ED_b3_c,
               RP_ED_b4_e,
               RP_ED_b5_e) 

RP_ED$cond <- recode(FP_ED$cond, `1` = "Facilitated", `2` = "Unfacilitated")
RP_ED$cond <- as.factor(FP_ED$cond)

descdist(as.numeric(na.omit(RP_ED$R4)))
# looks closest to a gamma distribution
descdist(as.numeric(na.omit(RP_ED$R5)))
# this is closest to lognormal but will use gamma

RP_ED_R4 <- glmer(R4 ~ cond + (1 + cond | subj) + (1 + cond | item), 
                  family = Gamma(link = "identity"), 
                  data = RP_ED)
summary(RP_ED_R4)

RP_ED_R5 <- glmer(R5 ~ cond + (1 + cond | subj) + (1 + cond | item), 
                  family = Gamma(link = "identity"), 
                  data = RP_ED)
summary(RP_ED_R5)

RP_R4_means <- RP_ED %>% 
  group_by(cond) %>% 
  dplyr::summarise(mean = mean(R4, na.rm = TRUE))

RP_ED %>%
  ggplot(aes(x = cond,
             y = R4)) +
  geom_jitter() +
  geom_point(data = RP_R4_means, stat = "identity",
             aes(y = mean,
                 x = cond,
                 color = cond),
             size = 3)

RP_R5_means <- RP_ED %>% 
  group_by(cond) %>% 
  dplyr::summarise(mean = mean(R5, na.rm = TRUE))

RP_ED %>%
  ggplot(aes(x = cond,
             y = R5)) +
  geom_jitter() +
  geom_point(data = RP_R5_means, stat = "identity",
             aes(y = mean,
                 x = cond,
                 color = cond),
             size = 3)

table <- tidy(RP_ED_R4) %>%
  filter(group == "fixed",
         term != "(Intercept)") %>%
  cbind(Model = rep("RP_ED_R4")) %>%
  dplyr::select(- group) %>%
  rbind(table) %>%
  arrange(Model)

table <- tidy(RP_ED_R5) %>%
  filter(group == "fixed",
         term != "(Intercept)") %>%
  cbind(Model = rep("RP_ED_R5")) %>%
  dplyr::select(- group) %>%
  rbind(table) %>%
  arrange(Model)

# FIXALIGN ANALYSIS ####
# TOTAL TIME #
descdist(as.numeric(na.omit(TT_FA$R4)))
# looks closest to a gamma distribution
descdist(as.numeric(na.omit(TT_FA$R5)))
# this is closest to lognormal but will use gamma

TT_FA_R4 <- glmer(R4 ~ cond + (1 + cond | subj) + (1 + cond | item), 
                  family = Gamma(link = "identity"), 
                  data = TT_FA)
summary(TT_FA_R4)

TT_FA_R5 <- glmer(R5 ~ cond + (1 + cond | subj) + (1 + cond | item), 
                  family = Gamma(link = "identity"), 
                  data = TT_FA)
summary(TT_FA_R5)

TT_R4_means <- TT_FA %>% 
  group_by(cond) %>% 
  dplyr::summarise(mean = mean(R4, na.rm = TRUE))

TT_FA %>%
  ggplot(aes(x = cond,
             y = R4)) +
  geom_jitter() +
  geom_point(data = TT_R4_means, stat = "identity",
             aes(y = mean,
                 x = cond,
                 color = cond),
             size = 3)

TT_R5_means <- TT_FA %>% 
  group_by(cond) %>% 
  dplyr::summarise(mean = mean(R5, na.rm = TRUE))

TT_FA %>%
  ggplot(aes(x = cond,
             y = R5)) +
  geom_jitter() +
  geom_point(data = TT_R5_means, stat = "identity",
             aes(y = mean,
                 x = cond,
                 color = cond),
             size = 3)

table <- tidy(TT_FA_R4) %>%
  filter(group == "fixed",
         term != "(Intercept)") %>%
  cbind(Model = rep("TT_FA_R4")) %>%
  dplyr::select(- group) %>%
  rbind(table) %>%
  arrange(Model)

table <- tidy(TT_FA_R5) %>%
  filter(group == "fixed",
         term != "(Intercept)") %>%
  cbind(Model = rep("TT_FA_R5")) %>%
  dplyr::select(- group) %>%
  rbind(table) %>%
  arrange(Model)

# FIRST PASS #

# reading in total time fix_align files
FP_FA_batch1_corr <- read_csv('FA_files/FP_FA_batch1_corr')
FP_FA_batch2_corr <- read_csv('FA_files/FP_FA_batch2_corr')
FP_FA_batch3_corr <- read_csv('FA_files/FP_FA_batch3_corr')
FP_FA_batch4_error <- read_csv('FA_files/FP_FA_batch4_error')
FP_FA_batch5_error <- read_csv('FA_files/FP_FA_batch5_error')

# removing rows with no data
# removing R8 column, which has no data
FP_FA_b1_c <- FP_FA_batch1_corr %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  dplyr::select(- R8)

FP_FA_b2_c <- FP_FA_batch2_corr %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  dplyr::select(- R8)

FP_FA_b3_c <- FP_FA_batch3_corr %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  dplyr::select(- R8)

FP_FA_b4_e <- FP_FA_batch4_error %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  dplyr::select(- R8)

FP_FA_b5_e <- FP_FA_batch5_error %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  dplyr::select(- R8)

# replacing participant numbers in the data files with participant numbers
# from the batch files
FP_FA_b1_c$subj <- mapvalues(FP_FA_b1_c$subj, unique(FP_FA_b1_c$subj), FA_b1_lst$X1)
FP_FA_b2_c$subj <- mapvalues(FP_FA_b2_c$subj, unique(FP_FA_b2_c$subj), FA_b2_lst$X1)
FP_FA_b3_c$subj <- mapvalues(FP_FA_b3_c$subj, unique(FP_FA_b3_c$subj), FA_b3_lst$X1)
FP_FA_b4_e$subj <- mapvalues(FP_FA_b4_e$subj, unique(FP_FA_b4_e$subj), FA_b4_lst$X1)
FP_FA_b5_e$subj <- mapvalues(FP_FA_b5_e$subj, unique(FP_FA_b5_e$subj), FA_b5_lst$X1)


# binding all together
FP_FA <- rbind(FP_FA_b1_c,
               FP_FA_b2_c,
               FP_FA_b3_c,
               FP_FA_b4_e,
               FP_FA_b5_e)

FP_FA$cond <- recode(FP_FA$cond, `1` = "Facilitated", `2` = "Unfacilitated")
FP_FA$cond <- as.factor(FP_FA$cond)

descdist(as.numeric(na.omit(FP_FA$R4)))
# looks closest to a gamma distribution
descdist(as.numeric(na.omit(FP_FA$R5)))
# this is closest to lognormal but will use gamma

FP_FA_R4 <- glmer(R4 ~ cond + (1 + cond | subj) + (1 + cond | item), 
                  family = Gamma(link = "identity"), 
                  data = FP_FA)
summary(FP_FA_R4)

FP_FA_R5 <- glmer(R5 ~ cond + (1 + cond | subj) + (1 + cond | item), 
                  family = Gamma(link = "identity"), 
                  data = FP_FA)
summary(FP_FA_R5)

FP_R4_means <- FP_FA %>% 
  group_by(cond) %>% 
  dplyr::summarise(mean = mean(R4, na.rm = TRUE))

FP_FA %>%
  ggplot(aes(x = cond,
             y = R4)) +
  geom_jitter() +
  geom_point(data = FP_R4_means, stat = "identity",
             aes(y = mean,
                 x = cond,
                 color = cond),
             size = 3)

FP_R5_means <- FP_FA %>% 
  group_by(cond) %>% 
  dplyr::summarise(mean = mean(R5, na.rm = TRUE))

FP_FA %>%
  ggplot(aes(x = cond,
             y = R5)) +
  geom_jitter() +
  geom_point(data = FP_R5_means, stat = "identity",
             aes(y = mean,
                 x = cond,
                 color = cond),
             size = 3)

table <- tidy(FP_FA_R4) %>%
  filter(group == "fixed",
         term != "(Intercept)") %>%
  cbind(Model = rep("FP_FA_R4")) %>%
  dplyr::select(- group) %>%
  rbind(table) %>%
  arrange(Model)

table <- tidy(FP_FA_R5) %>%
  filter(group == "fixed",
         term != "(Intercept)") %>%
  cbind(Model = rep("FP_FA_R5")) %>%
  dplyr::select(- group) %>%
  rbind(table) %>%
  arrange(Model)

# REGRESSION PATH #

# reading in total time fix_align files
RP_FA_batch1_corr <- read_csv('FA_files/RP_FA_batch1_corr')
RP_FA_batch2_corr <- read_csv('FA_files/RP_FA_batch2_corr')
RP_FA_batch3_corr <- read_csv('FA_files/RP_FA_batch3_corr')
RP_FA_batch4_error <- read_csv('FA_files/RP_FA_batch4_error')
RP_FA_batch5_error <- read_csv('FA_files/RP_FA_batch5_error')

# removing rows with no data
# removing R8 column, which has no data
RP_FA_b1_c <- RP_FA_batch1_corr %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  dplyr::select(- R8)

RP_FA_b2_c <- RP_FA_batch2_corr %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  dplyr::select(- R8)

RP_FA_b3_c <- RP_FA_batch3_corr %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  dplyr::select(- R8)

RP_FA_b4_e <- RP_FA_batch4_error %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  dplyr::select(- R8)

RP_FA_b5_e <- RP_FA_batch5_error %>%
  filter(seq != 0) %>%
  filter(cond != 0) %>%
  dplyr::select(- R8)

# replacing participant numbers in the data files with participant numbers
# from the batch files
RP_FA_b1_c$subj <- mapvalues(RP_FA_b1_c$subj, unique(RP_FA_b1_c$subj), FA_b1_lst$X1)
RP_FA_b2_c$subj <- mapvalues(RP_FA_b2_c$subj, unique(RP_FA_b2_c$subj), FA_b2_lst$X1)
RP_FA_b3_c$subj <- mapvalues(RP_FA_b3_c$subj, unique(RP_FA_b3_c$subj), FA_b3_lst$X1)
RP_FA_b4_e$subj <- mapvalues(RP_FA_b4_e$subj, unique(RP_FA_b4_e$subj), FA_b4_lst$X1)
RP_FA_b5_e$subj <- mapvalues(RP_FA_b5_e$subj, unique(RP_FA_b5_e$subj), FA_b5_lst$X1)


# binding all together
RP_FA <- rbind(RP_FA_b1_c,
               RP_FA_b2_c,
               RP_FA_b3_c,
               RP_FA_b4_e,
               RP_FA_b5_e)

RP_FA$cond <- recode(RP_FA$cond, `1` = "Facilitated", `2` = "Unfacilitated")
RP_FA$cond <- as.factor(RP_FA$cond)

descdist(as.numeric(na.omit(RP_FA$R4)))
# looks closest to a gamma distribution
descdist(as.numeric(na.omit(RP_FA$R5)))
# this is closest to lognormal but will use gamma

RP_FA_R4 <- glmer(R4 ~ cond + (1 + cond | subj) + (1 + cond | item), 
                  family = Gamma(link = "identity"), 
                  data = RP_FA)
summary(RP_FA_R4)

RP_FA_R5 <- glmer(R5 ~ cond + (1 + cond | subj) + (1 + cond | item), 
                  family = Gamma(link = "identity"), 
                  data = RP_FA)
summary(RP_FA_R5)

RP_R4_means <- RP_FA %>% 
  group_by(cond) %>% 
  dplyr::summarise(mean = mean(R4, na.rm = TRUE))

RP_FA %>%
  ggplot(aes(x = cond,
             y = R4)) +
  geom_jitter() +
  geom_point(data = RP_R4_means, stat = "identity",
             aes(y = mean,
                 x = cond,
                 color = cond),
             size = 3)

RP_R5_means <- RP_FA %>% 
  group_by(cond) %>% 
  dplyr::summarise(mean = mean(R5, na.rm = TRUE))

RP_FA %>%
  ggplot(aes(x = cond,
             y = R5)) +
  geom_jitter() +
  geom_point(data = RP_R5_means, stat = "identity",
             aes(y = mean,
                 x = cond,
                 color = cond),
             size = 3)

table <- tidy(RP_FA_R4) %>%
  filter(group == "fixed",
         term != "(Intercept)") %>%
  cbind(Model = rep("RP_FA_R4")) %>%
  dplyr::select(- group) %>%
  rbind(table) %>%
  arrange(Model)

table <- tidy(RP_FA_R5) %>%
  filter(group == "fixed",
         term != "(Intercept)") %>%
  cbind(Model = rep("RP_FA_R5")) %>%
  dplyr::select(- group) %>%
  rbind(table) %>%
  arrange(Model)




