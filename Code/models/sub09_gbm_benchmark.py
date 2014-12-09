## author: phalaris
## kaggle bci challenge gbm benchmark

from __future__ import division
import numpy as np
import pandas as pd
import sklearn.ensemble as ens

train_subs = ['02','06','07','11','12','13','14','16','17','18','20','21','22','23','24','26']
test_subs = ['01','03','04','05','08','09','10','15','19','25']
train_labels = pd.read_csv('../../Data/TrainLabels.csv')
submission = pd.read_csv('../../Results/SampleSubmission.csv')

#train = pd.DataFrame(columns=['subject','session','feedback_num','start_pos'] + ['Cz_' + s for s in map(str,range(261))],index=range(5440))
train = pd.DataFrame(columns=['Cz_' + s for s in map(str,range(261))],index=range(5440))
counter = 0
print 'loading train data'
data = {}
for i in train_subs:
    for j in range(1,6):
        temp = pd.read_csv('../../Data/train/Data_S' + i + '_Sess0'  + str(j) + '.csv')
        fb = temp.query('FeedBackEvent == 1',engine='python')['FeedBackEvent']        
        counter2 = 0
        for k in fb.index:
                temp2 = temp.loc[int(k):int(k)+260,'Cz']
                temp2.index = ['Cz_' + s for s in map(str,range(261))]
                train.loc[counter,['Cz_' + s for s in map(str,range(261))]] = temp2
                #train.loc[counter,'session'] = j
                #train.loc[counter, 'subject'] = i
                #train.loc[counter, 'feedback_num'] = counter2
                #train.loc[counter, 'start_pos'] = k
                counter +=1  
                counter2 +=1
    print 'subject ', i

train.to_csv('train_cz.csv',ignore_index=True)

#test = pd.DataFrame(columns=['subject','session','feedback_num','start_pos'] + ['Cz_' + s for s in map(str,range(261))],index=range(3400))
test = pd.DataFrame(columns=['Cz_' + s for s in map(str,range(261))],index=range(3400))
print 'loading test data'
counter = 0
data = {}
for i in test_subs:
    for j in range(1,6):
        temp = pd.read_csv('../../Data/test/Data_S' + i + '_Sess0'  + str(j) + '.csv')
        fb = temp.query('FeedBackEvent == 1',engine='python')['FeedBackEvent']        
        counter2 = 0
        for k in fb.index:
                temp2 = temp.loc[int(k):int(k)+260,'Cz']
                temp2.index = ['Cz_' + s for s in map(str,range(261))]
                test.loc[counter,['Cz_' + s for s in map(str,range(261))]] = temp2
                #test.loc[counter,'session'] = j
                #test.loc[counter, 'subject'] = i
                #test.loc[counter, 'feedback_num'] = counter2
                #test.loc[counter, 'start_pos'] = k
                counter +=1  
                counter2 +=1
    print 'subject ', i  

test.to_csv('test_cz.csv',ignore_index=True)

print 'training GBM'

gbm = ens.GradientBoostingClassifier(n_estimators=500,learning_rate=0.05, max_features=0.25)
gbm.fit(train, train_labels.values[:,1].ravel())
preds = gbm.predict_proba(test)
preds = preds[:,1]
submission['Prediction'] = preds
submission.to_csv('../../Results/sub09_gbm_benchmark_noids.csv',index=False)
print 'Done'
