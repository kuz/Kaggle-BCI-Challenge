This folder contains configurations for The Overfitter.
It will pick up all configurations from "run" folder with "conf_" in their name.

Legend for the Status columns
    G -- Good: configuration is ready to use
    R -- Running: test of this configuration is running
    W -- Writing: somebody is writing this configuration
    X -- eXcluded: unusable method, there will be a file "x_conf_method.r" if somebody is interested
    E -- Error: some error occurred, needs to be dealt with, there will be a file "e_conf_method.r" which can be used in "conftest.r" to reproduce the error
    L -- Long: algorithm works, but is too slow, use those if you have time

Machine Learning Method                       caret Name     Status    One CV (m)    All CV (m)    File                   Comment
-----------------------                       ----------     ------    ----------    ----------    ----                   -------
Bayesian Generalized Linear Model             bayesglm       GOOD        0:24         331:00       conf_bayesglm.r
Stochastic Gradient Boosting                  gbm            GOOD        0:20         743:00       conf_gbm.r
Penalized Multinomial Regression              multinom       GOOD        0:16         114:00       conf_multinom.r
Random Forest                                 rf             GOOD        0:23        2750:00       conf_rf.r
Regularized Random Forest                     rrf            GOOD        0:53                      conf_rrf.r
Bagged CART                                   treebag        GOOD        5:48                      conf_treebag.r
Bagged Flexible Discriminant Analysis         bagFDA         L         198:00                      l_conf_bagFDA.r
Bagged Logic Regression                       logicBag       X              -                      ?                      "Some of the values of the predictors are not 0 or 1"
Bagged MARS                                   bagEarth       L         216:00                      l_conf_bagEarth.r
Bagged Model                                  bag            E              -                                             TRY WITHOUT CARET "Please specify 'bagControl' with the appropriate functions" e_conf_bag.r
Boosted Classification Trees                  ada            GOOD        0:54                      conf_ada.r
Boosted Generalized Additive Model            gamboost       L         727:12                      l_conf_gamboost.r     
Boosted Generalized Linear Model              glmboost       L         595:30                      l_conf_glmboost.r
Boosted Linear Model                          bstLs          X              -                      x_conf_bstLs.r         TRY WITHOUT CARET "only classification models that produce probabilities are allowed" x_conf_bstLs.r
Boosted Logistic Regression                   LogitBoost     GOOD        0:17         144:00       conf_LogitBoost.r
Boosted Smoothing Spline                      bstSm          X              -                      x_conf_bstSm.r         TRY WITHOUT CARET "only classification models that produce probabilities are allowed" x_conf_bstSm.r
Boosted Tree                                  blackboost     GOOD        1:58                      conf_blackboost.r
Boosted Tree                                  bstTree        X              -                      x_conf_bstTree.r       TRY WITHOUT CARET "only classification models that produce probabilities are allowed" x_conf_bstTree.r
C4.5-like Trees                               J48            GOOD        0:30         175:00       conf_J48.r
C5.0                                          C5.0           GOOD        0:45                      conf_C50.r
CART                                          rpart          GOOD        0:18          43:30       conf_rpart.r
CART                                          rpart2         GOOD        0:18          28:45       conf_rpart2.r
Conditional Inference Random Forest           cforest        GOOD       36:00                      conf_cforest.r
Conditional Inference Tree                    ctree          GOOD        1:50                      conf_ctree.r
Conditional Inference Tree                    ctree2         GOOD        1:25                      conf_ctree2.r


