#### Library####
source("DataTools.R")
#
#### Data ####
data_1 <- Create_Dataset("Treatment", replicate(5, paste(sample(LETTERS, 3, replace=TRUE), collapse="")), 
                       "Yield", c(8, 11, 70, 17, 12),
                       "Time", c("Summer"),
                       "Species", c("MutA"),
                       Normality =T ,  Replicates = 5, 
                       Skewness = c(2), Kurtosis = c(20), 
                       Seed =1456, Sigma = NULL)$Data

data_2 <- Create_Dataset("Treatment", replicate(5, paste(sample(LETTERS, 3, replace=TRUE), collapse="")), 
                         "Yield", c(10, 14, 90, 20, 12),
                         "Time", c("Summer"),
                         "Species", c("MutB"),
                         Normality =T ,  Replicates = 5, 
                         Skewness = c(2), Kurtosis = c(20), 
                         Seed =1456, Sigma = NULL)$Data

data_3 <- Create_Dataset("Treatment", replicate(5, paste(sample(LETTERS, 3, replace=TRUE), collapse="")), 
                         "Yield", c(1, 4, 9, 2, 1),
                         "Time", c("Summer"),
                         "Species", c("WT"),
                         Normality =T ,  Replicates = 5, 
                         Skewness = c(2), Kurtosis = c(20), 
                         Seed =1456, Sigma = NULL)$Data


data_4 <- Create_Dataset("Treatment", replicate(5, paste(sample(LETTERS, 3, replace=TRUE), collapse="")), 
                         "Yield", c(3, 5, 22, 7, 5),
                         "Time", c("Winter"),
                         "Species", c("MutA"),
                         Normality =T ,  Replicates = 5, 
                         Skewness = c(2), Kurtosis = c(20), 
                         Seed =1456, Sigma = NULL)$Data

data_5 <- Create_Dataset("Treatment", replicate(5, paste(sample(LETTERS, 3, replace=TRUE), collapse="")), 
                         "Yield", c(7, 11, 80, 20, 10),
                         "Time", c("Winter"),
                         "Species", c("MutB"),
                         Normality =T ,  Replicates = 5, 
                         Skewness = c(2), Kurtosis = c(20), 
                         Seed =1456, Sigma = NULL)$Data

data_6 <- Create_Dataset("Treatment", replicate(5, paste(sample(LETTERS, 3, replace=TRUE), collapse="")), 
                         "Yield", c(0.3, 1, 3, 0.9, 0.8),
                         "Time", c("Winter"),
                         "Species", c("WT"),
                         Normality =T ,  Replicates = 5, 
                         Skewness = c(2), Kurtosis = c(20), 
                         Seed =1456, Sigma = NULL)$Data


data_Yield <- rbind(data_1, data_2, data_3, data_4, data_5,data_6)


data_7 <- Create_Dataset("Treatment", replicate(5, paste(sample(LETTERS, 3, replace=TRUE), collapse="")), 
                         "Weight", c(20, 18, 28, 21, 19),
                         "Time", c("Summer"),
                         "Species", c("MutA"),
                         Normality =T ,  Replicates = 5, 
                         Skewness = c(2), Kurtosis = c(20), 
                         Seed =1456, Sigma = NULL)$Data

data_8 <- Create_Dataset("Treatment", replicate(5, paste(sample(LETTERS, 3, replace=TRUE), collapse="")), 
                         "Weight", c(21, 19, 34, 20, 23),
                         "Time", c("Summer"),
                         "Species", c("MutB"),
                         Normality =T ,  Replicates = 5, 
                         Skewness = c(2), Kurtosis = c(20), 
                         Seed =1456, Sigma = NULL)$Data

data_9 <- Create_Dataset("Treatment", replicate(5, paste(sample(LETTERS, 3, replace=TRUE), collapse="")), 
                         "Weight", c(10, 7, 9, 11, 9),
                         "Time", c("Summer"),
                         "Species", c("WT"),
                         Normality =T ,  Replicates = 5, 
                         Skewness = c(2), Kurtosis = c(20), 
                         Seed =1456, Sigma = NULL)$Data


data_10 <- Create_Dataset("Treatment", replicate(5, paste(sample(LETTERS, 3, replace=TRUE), collapse="")), 
                         "Weight", c(3, 5, 22, 7, 5),
                         "Time", c("Winter"),
                         "Species", c("MutA"),
                         Normality =T ,  Replicates = 5, 
                         Skewness = c(2), Kurtosis = c(20), 
                         Seed =1456, Sigma = NULL)$Data

data_11 <- Create_Dataset("Treatment", replicate(5, paste(sample(LETTERS, 3, replace=TRUE), collapse="")), 
                         "Weight", c(17, 11, 27, 16, 15),
                         "Time", c("Winter"),
                         "Species", c("MutB"),
                         Normality =T ,  Replicates = 5, 
                         Skewness = c(2), Kurtosis = c(20), 
                         Seed =1456, Sigma = NULL)$Data

data_12 <- Create_Dataset("Treatment", replicate(5, paste(sample(LETTERS, 3, replace=TRUE), collapse="")), 
                         "Weight", c(3, 2, 1, 5, 2),
                         "Time", c("Winter"),
                         "Species", c("WT"),
                         Normality =T ,  Replicates = 5, 
                         Skewness = c(2), Kurtosis = c(20), 
                         Seed =1456, Sigma = NULL, Report = F)$Data


data_Weight <- rbind(data_7, data_8, data_9, data_10, data_11,data_12)
data <- merge(data_Weight, data_Yield)
rm(data_1, data_2,data_3, data_4,data_5,data_6,data_7,data_8,data_9,data_10,data_11, data_12, data_Weight, data_Yield)

#
#### ####
data_info <- Dataset_Info(Yield~Treatment*Species, data,
                          Colour = "Default",  Style= "Stata")

aov_info(aov(Yield~Treatment, data))
