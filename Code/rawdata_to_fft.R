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

fft_per_ch_per_slice <- fft_per_ch_per_slice(Data_S02_Sess01, window_size=200, step=200)

file_out <- "/Users/annaleontjeva/Desktop/My_files/Kaggle-BCI-Challenge/Data/fft_per_ch_per_slice.txt" 
write.table(fft_per_ch_per_slice,file_out,
            col.names=FALSE,row.names=FALSE, quote=F,sep=",")
