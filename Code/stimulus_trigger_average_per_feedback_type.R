library(PTAk)
# Drawing stimulus trigger for subject S02 session 01, 
# distinguishing between positive and negaitve feedbacks
library(data.table)
library(ggplot2)
library(abind)
library(reshape)

S02_sess <- fread('../Data/train/Data_S02_Sess01.csv', sep = ',')
S02_sess <- rbind(S02_sess,  fread('../Data/train/Data_S02_Sess02.csv', sep = ','), 
                  fread('../Data/train/Data_S02_Sess03.csv', sep = ','),
                  fread('../Data/train/Data_S02_Sess04.csv', sep = ','),
                  fread('../Data/train/Data_S02_Sess05.csv', sep = ','))
head(S02_sess) 
# Labels from TrainLabels file for this session
feedback_labels <- fread('../Data/TrainLabels.csv', sep = ',')[1:340,]

# Indexes of feedback event
feedback_strt_indx <- which(S02_sess$FeedBackEvent == 1)

positive_feedback_indexes <- feedback_strt_indx[which(feedback_labels$Prediction == 1)]
negative_feedback_indexes <- feedback_strt_indx[which(feedback_labels$Prediction == 0)]

# List of stimulus triggering data.frames for each feedback event in the session
positive_window_list <- lapply(positive_feedback_indexes, function(x) as.data.frame(S02_sess)[(x - 1000):(x + 1000),2:(length(S02_sess[1,])-2)])
negative_window_list <- lapply(negative_feedback_indexes, function(x) as.data.frame(S02_sess)[(x - 1000):(x + 1000),2:(length(S02_sess[1,])-2)])

# Merge list of data.frame into one data.frame
positive_window_matrix <- abind(positive_window_list, along=3)
negative_window_matrix <- abind(negative_window_list, along=3)

# Take row mean across all data.frames
positive_window_average <- apply(positive_window_matrix, c(1,2), mean)
negative_window_average <- apply(negative_window_matrix, c(1,2), mean)

# Melting positive window average
melt_positive_average <- melt(positive_window_average)
head(melt_positive_average)

# Melting negative window average
melt_negative_average <- melt(negative_window_average)
head(melt_negative_average)

# Adding time for melted positive data set 
melt_positive_average$Time <- rep(as.data.frame(S02_sess)[1:2001, 1], (length(S02_sess[1,])-3))
melt_positive_average$Feedback_type <- as.factor(rep(1, nrow(melt_positive_average)))
head(melt_positive_average)

# Adding time for melted negative data set 
melt_negative_average$Time <- rep(as.data.frame(S02_sess)[1:2001, 1], (length(S02_sess[1,])-3))
melt_negative_average$Feedback_type <- as.factor(rep(0, nrow(melt_positive_average)))
head(melt_negative_average)

melt_average <- rbind(melt_positive_average, melt_negative_average)
str(melt_average)

pdf(file="../Figures/stimululs_trigger_average_S02_per_feedback_type.pdf", height = 12, width = 16)
ggplot(melt_average, aes(x = Time, y = value, group = Feedback_type, colour = Feedback_type)) + 
  geom_line() + 
  facet_wrap(~X2)
dev.off()
