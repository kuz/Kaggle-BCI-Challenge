Kaggle BCI Challenge
====================
Here are listed the major steps we should take and the results of those steps as we took them.

1. Figure out good features
---------------------------
[Different datasets on GBM](Results/Summary/one_run_gbm.txt) using same model on all dataset: GBM(500, 0.05, 1)
[Different datasets on Multinom](Results/Summary/one_run_multinom.txt) using same model on all dataset: Multinom(100, 10)


2. Find the best classifier
---------------------------

### Cross-validation results ###

Each dataset has a separate file where all CV results are preserved  
[Cz; 1300ms; +meta](Results/Summary/cz2secmeta.txt)  
[Cz; 1300ms; PCA; +meta](Results/Summary/cz2sec_pca_meta.txt)  
[Cz; 1300ms](Results/Summary/cz2sec.txt)  
[8ch; 700ms](Results/Summary/8ch700ms.txt)  
[8ch; 1300ms](Results/Summary/8ch1300ms.txt)  
[8ch; 1300ms; PCA](Results/Summary/8ch1300ms_pca.txt)  
[Cz; 1300ms; FFT](Results/Summary/fft_cz1300ms.txt)  


3. Add META data on top just in case
------------------------------------
Kaggle allows to present two model for final judgment. We plan to present two model: one without meta data (subject id, session id, feedback time) and another one with this information. The theory is that if the dataset would be big enough the metadata should not have given any useful info. But in this case test set is only 10 subject, so it is better to be on the safe side.
