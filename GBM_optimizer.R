#------------------------------------------------------
#Code Name: GBM Generalized
#Objective: Integrate EASI variables for POC with EASI
#Date: 18/07/2013
#Date modified:19/07/2013
#------------------------------------------------------

                          # NOTE: XX refers to group nuumber
                    # Replace XX with group number to run automated code

# Specify the group number in 'XX'
setwd("S:/Opportunity Analysis/Offshore folder/POC_EASI_Models/EASI/Group_XX")
#=====================================================================================
# Data Preparation: Schedule 1 (Import Data)
#=====================================================================================
# Import data
  modbse.gXX <- read.csv("Data/POC_EASI_GRP_XX.csv")

# Identify product names: to check product
  unique(modbse.gXX$PRODUCT_SHORT_NAME_FF)

# Check Distribution: Quantiles
  quantile(modbse.gXX$GWP_ANNUALIZED, probs = seq(0,1,0.01))

# Remove High Outlier: standard truncation at 5% and 95%
  modbse.gXX.t <- modbse.gXX[(modbse.gXX$GWP_ANNUALIZED <= 111
                            & modbse.gXX$GWP_ANNUALIZED >= 111),]
# NOTE: TO RUN FOR NON TRUNCATED USE 'modbse.gXX' instead of g5.t in next step

#=====================================================================================
# Data Preparation: Schedule 2 (Subset Data)
#=====================================================================================
# Remove Dependent
#---------------------------------------------------------------------------------
rm.dep <- names(modbse.gXX.t) %in% c("GWP_ANNUALIZED",
                                       "WRITTEN_PREM_AMT")
  
  mb.gXX.1 <- modbse.gXX.t[!rm.dep]

# Remove Date abd Code/Indicators
#---------------------------------------------------------------------------------
rm.ind <- names(mb.gXX.1) %in% c( "DUNS_NO",
                                    "GROUP_NO","TREE_POS",
                                    "PRODUCT_SEGMENT","PRODUCT_GROUP_NAME",
                                    "MIN_EFF_MONTH","EFF_DATE" ,
                                    "UPD_DT","EFF_YEAR", "UPD_YR","UPD_MO",
                                    "EMP_HERE_CD","EMP_TOTAL_CD",
                                    "DIVISION_IND", 
                                    "CHARTIS_SECTOR_NAME",
                                    "CHARTIS_INDUSTRY_NAME", 
                                    "DOM_ULT_INDUSTRY",                
                                    "DOM_ULT_INDUSTRY_SECTOR",                         
                                    "GLBL_ULT_FIPS_CTRY",              
                                    "GLB_ULT_INDUSTRY",                
                                    "GLB_ULT_INDUSTRY_SECTOR",
                                    "POPULATION_CD",                   
                                    "FIPS_MSA_CD",                     
                                    "FIPS_COUNTY_CD", "DB_UPD_DT","POSTAL_CODE",
                                    "postal_code_1","ZIP_CODE",
                                    "STNAME", "FIPS")
#"remove new dependent" + eff month wala)
  mb.gXX.2 <- mb.gXX.1[!rm.ind]

# Remove with no deviation idnetified: group specific (thrown by gbm procedure)
#---------------------------------------------------------------------------------
#   rm.std <- names(mb.g6.2) %in% c("PRODUCT_GROUP_NAME",
#                                 "Int_Metals_Minerals_S3YA",
#                                 "Int_Metals_Minerals_PGS3Y",
#                                 "Int_Misc_Other_PGS3Y",
#                                 )
#   mb.g6.2 <- mb.g6.2[!rm.std]
  
# Remove Misc Variables: Specific to Groups
#---------------------------------------------------------------------------------
mb.gXX <- mb.gXX.2

#NOTE: check data dimension 41 variables have been dropped till now
#---------------------------------------------------------------------------------
# Remove unrequired data sets  
  rm(mb.gXX.1)
  rm(mb.gXX.2)
  #rm(mb.gXX.3)
  rm(modbse.gXX)
                                                                    
#=====================================================================================
# Data Preparation: Schedule 3 (Partition in to train and test)
#=====================================================================================
  library("caret")
  library("e1071")
  set.seed(786)
  key <- createDataPartition(mb.gXX$LN_GWP, p =0.70, list =F)
  mb.train <- mb.gXX[key,]
  mb.test <- mb.gXX[-key,]
                                  

#=====================================================================================
# GBM: Run Model
#=====================================================================================
  library("gbm")
  tree = 100
  mb.gbm <- gbm(LN_GWP ~ .,  
                distribution = 'gaussian',
                data = mb.train,
                n.trees = tree,  
                interaction.depth = 2,
                n.minobsinnode = 60,
                shrinkage = 0.12,
                bag.fraction = 0.8,
                train.fraction = 0.93,
                cv.folds=2,
                keep.data = TRUE,
                verbose = TRUE)
# RMSE
#---------------------------------------------------------------------------------
  library('Metrics')
# VAL
#---------------------------------------------------------------------------------
  mb.test$mb.gbm.pred <- predict(mb.gbm, mb.test, n.trees = tree, type = "response")
  rmse(mb.test$mb.gbm.pred, mb.test$LN_GWP)
# DEV
#---------------------------------------------------------------------------------
  mb.train$mb.gbm.pred <- predict(mb.gbm, mb.train, n.trees = tree, type = "response")
  rmse(mb.train$mb.gbm.pred, mb.train$LN_GWP)

#=====================================================================================
# GBM: MODEL performance
#=====================================================================================
# check via OOB
  best.iter <-  gbm.perf(mb.gbm, method ="OOB")
  print(best.iter)
  # plot performance
  #summary(mb.gbm, n.trees = 100, 20)
  #plot.gbm(mb.gbm, 1, return.grid =F) 

#=====================================================================================
# MAPE: Scaled preditcion
#=====================================================================================
 taking exponential of prediction
  mb.test$act.exp <- exp(mb.test$LN_GWP)
  mb.train$act.exp <- exp(mb.train$LN_GWP)

# the error 'epsilon' is taken on the development data
# with the hypothesis that implementation will follow same error trend
  mb.train$epsilon <- mb.train$LN_GWP - mb.train$mb.gbm.pred
  mean(mb.train$epsilon)

# Scaled Predictors(VAL)
#--------------------------------------------------------------------------------
  mb.test$pred.sc.GWP <- exp(mean(mb.train$epsilon))*exp(mb.test$mb.gbm.pred)
  mb.test$pred.sc.GWP1 <- exp(0.5*var(mb.train$epsilon))*exp(mb.test$mb.gbm.pred)
# DEV
#--------------------------------------------------------------------------------
  mb.train$pred.sc.GWP <- exp(mean(mb.train$epsilon))*exp(mb.train$mb.gbm.pred)
  mb.train$pred.sc.GWP1 <- exp(0.5*var(mb.train$epsilon))*exp(mb.train$mb.gbm.pred)

#=====================================================================================
# ERROR MATRICES: at Overall and Short Name level
#=====================================================================================
# OVERALL ERROR: VAL
#---------------------------------------------------------------------------------
  errsum.val <- sapply(mb.test[,c("pred.sc.GWP1", "act.exp")], sum)
  err.v <- (abs(errsum.val[2] - errsum.val[1])/errsum.val[2])
  print(err.v)

# DEV
#---------------------------------------------------------------------------------
  errsum.dev <- sapply(mb.train[,c("pred.sc.GWP1", "act.exp")], sum)
  err.d <- (abs(errsum.dev[2] - errsum.dev[1])/errsum.dev[2])
  print(err.d)

# SHORT NAMES ERROR: create performance matrix 'sn'(VAL)
#---------------------------------------------------------------------------------
  sn.val <- ddply(mb.test, .(PRODUCT_SHORT_NAME_FF), summarize,
                  freq =length(act.exp), 
                  sumpred = sum(pred.sc.GWP1), sumact = sum(act.exp),
                  proderr = abs(sumpred-sumact)/sumact)  
  sn.val
# DEV
#---------------------------------------------------------------------------------
  sn.dev <- ddply(mb.train, .(PRODUCT_SHORT_NAME_FF), summarize,
                  freq =length(act.exp), 
                  sumpred = sum(pred.sc.GWP1), sumact = sum(act.exp),
                  proderr = abs(sumpred-sumact)/sumact)
  sn.dev
 
  list.PA <- c(sn.val,sn.dev)
# Export
  write.csv(list.PA, "Results/july DT/SN.csv")
#=====================================================================================
# MODEL performance : Score Plots
#=====================================================================================
# Quantiles for 5 (VAL)
#---------------------------------------------------------------------------------
  mb.test$dev.gbm.buck5 <- cut(mb.test$pred.sc.GWP1, 
                               unique(quantile(mb.test$pred.sc.GWP1, probs= seq(0, 1,0.1))), 
                               include.lowest = TRUE)  
  val <- ddply(mb.test, .(dev.gbm.buck5), summarize, 
               freq =length(act.exp), 
               sumpred = sum(pred.sc.GWP1), sumact = sum(act.exp),
               decerr = abs(sumpred-sumact)/sumact)  
  val
# Score PLot
  plot(val$sumact, type = "l")
  lines(val$sumpred, col = "blue")
# Export
  write.csv(val, file="Results/july DT/score.val.csv")

# Score for dev
#---------------------------------------------------------------------------------
  mb.train$dev.gbm.buck5 <- cut(mb.train$pred.sc.GWP1, 
                               unique(quantile(mb.train$pred.sc.GWP1, probs= seq(0, 1,0.1))), 
                               include.lowest = TRUE) 
  val.d <- ddply(mb.train, .(dev.gbm.buck5), summarize, 
                 freq =length(act.exp), 
                 sumpred = sum(pred.sc.GWP1), sumact = sum(act.exp),
                 decerr = abs(sumpred-sumact)/sumact)  
  val.d
# Score PLot
  plot(val.d$sumact, type = "l")
  lines(val.d$sumpred, col = "red")
# Export
  write.csv(val.d, file="Results/july DT/score.dev.csv")

# Save Results
#---------------------------------------------------------------------------------
save.image("~/1207103_gbm_groupXX.RData")
#=====================================================================================
#FIN