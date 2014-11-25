#library("PTAk")
# Sandbox
# You can try things here
#
library(zoo)
setwd("/Users/annaleontjeva/Desktop/My_files/Kaggle-BCI-Challenge/Data/train/")

#---Fast fourier transform----#
Data_S02_Sess01 <- fread("Data_S02_Sess01.csv")
tmp_fourier <- fft(Data_S02_Sess01$AF3)
amplitude <- abs(tmp_fourier)
normalized_amp <- amplitude/(length(tmp_fourier)/2)

#----FFT with window-----#
data <- Data_S02_Sess01$AF3
window_size <- 200
window <- 1:window_size
data_in_window <- data[window]
trend <- lm(data_in_window~window)
detrended_data <- trend$residuals
fourier_components_norm <- abs(fft(detrended_data))/(length(detrended_data)/2)
fourier_components_half <-fourier_components_norm[1:(length(fourier_components_norm)/2)]


#----More generic function with sliding window----#
Data_S02_Sess01 <- as.data.frame(Data_S02_Sess01)
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

#rollapply(Data_S02_Sess01$AF3, 200, function(x) fourier_transform_on_window(x))#overlapping
nonoverlap_slices <- rollapply(Data_S02_Sess01$AF3,200, by=200,function(x) fourier_transform_on_window(x))#non=overlapping
plot(nonoverlap_slices[4,2:50],type="l")

fft_per_ch_per_slice <- c()
for(electrode in 2:(ncol(Data_S02_Sess01)-2)){
  print(electrode)
  fft_per_slice <- pieces(Data_S02_Sess01[,electrode],window_size=200,step=200)
  fft_per_ch_per_slice <- cbind(fft_per_ch_per_slice,fft_per_slice)
}
write.table(fft_per_ch_per_slice,"/Users/annaleontjeva/Desktop/My_files/Kaggle-BCI-Challenge/Data/fft_per_ch_per_slice.txt",
            col.names=FALSE,row.names=FALSE, quote=F,sep=",")


#ft_S02_Sess01 <- apply(Data_S02_Sess01[,c(2:4),with=FALSE],2,function(x) pieces(x, window_size=200,step=200))
