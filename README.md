# GBM-optimizor

## Tuning GBM parameters

With almost as many as 7-8 parameters to be defined during GBM run, it is an arduous process to find the optimal number of trees, shrinkage, train.fraction, interaction depth, n.minobsinnode. The only identifable causality that exists is the negative corrleation number of trees with shrinkage or learning rate. More so, **since there is no range boundation for these parameters, it's translates into a non-convex hull optimsation problem**. 
The following function takes in a broad range parameter values and in through an automation tabulates error statistics betwene train and test data. Through a little manual deliberation one can understand the zones of localised optima and identify the optimal vlaues taking heed of *overfitting bias*.
