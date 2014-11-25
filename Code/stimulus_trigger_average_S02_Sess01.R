library(PTAk)
library(data.table)
library(ggplot2)
library(abind)
setwd("../Data/train/")

S02_sess01 <- fread('Data_S02_Sess01.csv', sep = ',')
feedback_strt_indx <- which(S02_sess01$FeedBackEvent == 1)

window_list <- lapply(feedback_strt_indx, function(x) as.data.frame(S02_sess01)[(x - 1000):(x + 1000),2:(length(S01_sess01[1,])-2)])

all_window_matrix <- abind(window_list, along=3)
all_window_average <- apply(all_window_matrix, c(1,2), mean)
head(all_window_average)
str(all_window_average)

melt_S02_sess01 <- melt(all_window_average)
head(melt_S02_sess01)

melt_S02_sess01$Time <- rep(as.data.frame(S02_sess01)[1:2001, 1], (length(S02_sess01[1,])-3))
head(melt_S01_sess01)
pdf(file="../Figures/stimululs_trigger_average_S02_Sess01.pdf", height = 12, width = 16)
ggplot(melt_S01_sess01, aes(x = Time, y = value, group = X2, colour = X2)) + 
  geom_line() + 
  facet_wrap(~X2)
dev.off()
