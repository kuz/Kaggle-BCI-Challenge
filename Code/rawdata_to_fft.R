#libraries
library(zoo)

path="/Users/annaleontjeva/Desktop/My_files/Kaggle-BCI-Challenge/Data/train/"
setwd(path)

#---Functions---#
fourier_transform_on_window <-  function(x){
  window <- 1:length(x)
  trend <- lm(x~window)
  detrended_data <- trend$residuals
  fourier_components_norm <- abs(fft(detrended_data))/(length(detrended_data)/2)
  fourier_components_half <-fourier_components_norm[1:(length(fourier_components_norm)/2)]
  return(fourier_components_half)
}
pieces <- function(dt, window_size=200, step=200){
  rollapply(dt, window_size, by=step, function(x) fourier_transform_on_window(x))
}
fft_per_ch_per_slice <- function(dt, window_size=200, step=200){
  fft_per_ch_per_slice <- c()
  for(electrode in 2:(ncol(dt)-2)){
    print(electrode)
    fft_per_slice <- pieces(dt[,electrode],window_size,step)
    fft_per_ch_per_slice <- cbind(fft_per_ch_per_slice,fft_per_slice)
  }
  return(fft_per_ch_per_slice)
}

#---Data---#
Data_S02_Sess01 <- fread("Data_S02_Sess01.csv")
Data_S02_Sess01 <- as.data.frame(Data_S02_Sess01)

TrainLabels <- fread("TrainLabels.csv")
TrainLabels <- as.data.frame(TrainLabels)
TrainLabels$FeedBackNr <- as.numeric(str_sub(TrainLabels$IdFeedBack,start=-3))


fft_per_ch_per_slice <- fft_per_ch_per_slice(Data_S02_Sess01, window_size=200, step=200)

file_out <- "/Users/annaleontjeva/Desktop/My_files/Kaggle-BCI-Challenge/Data/fft_per_ch_per_slice.txt" 
write.table(fft_per_ch_per_slice,file_out,
            col.names=FALSE, row.names=FALSE, quote=F, sep=",")


#------------#
idx_feedback_true <- which(Data_S02_Sess01$FeedBackEvent==1)

#dt_feedback_true <- Data_S02_Sess01[idx_feedback_true[1]:(idx_feedback_true[1]+799),]
#fft_per_ch_per_slice_ev <- fft_per_ch_per_slice(dt_feedback_true, window_size=200, step=200)
#fft_per_ch_per_slice_ev <- as.data.frame(fft_per_ch_per_slice_ev)
#fft_per_ch_per_slice_ev$label <- TrainLabels$Prediction[1]


fft_positive_feedback_800 <- c()
for(feedback in 1:length(idx_feedback_true)){
#for(feedback in 1:4){
  print(paste("feedback: ",feedback, sep=''))
  dt_feedback_true <- Data_S02_Sess01[idx_feedback_true[feedback]:(idx_feedback_true[feedback]+799),]
  fft_per_ch_per_slice_ev <- fft_per_ch_per_slice(dt_feedback_true, window_size=200, step=200)
  fft_positive_feedback_800 <- rbind(fft_positive_feedback_800,fft_per_ch_per_slice_ev)
}

fft_positive_feedback_800_lbled <- as.data.frame(fft_positive_feedback_800)
colnames(fft_positive_feedback_800_lbled) <- paste("A_",1:length(colnames(fft_positive_feedback_800_lbled)),sep="")
#labels <- rep(TrainLabels$Prediction[1:60],each=4)
#fft_positive_feedback_800_lbled$label <- labels

melted_fft_pos_feedback <- melt(data=fft_positive_feedback_800_lbled)
#melted_fft_pos_feedback$channels <- rep

#ggplot(fft_positive_feedback_800_lbled, 
#       aes(x=fft_positive_feedback_800_lbled$A_1,y=fft_positive_feedback_800_lbled$A_2))+geom_line()+theme_bw()

list.files()