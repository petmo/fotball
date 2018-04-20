# fotball
# Logg over best xgboost performance:
k = 8,


tune.grid <- expand.grid(eta = c(0.01),
                         gamma = c(0.1),
                         max_depth = c(4),
                         min_child_weight = c(1.0),
                         subsample = c(0.8),
                         colsample_bytree = c(0.6),
                         lambda = c(1.2),
                         nrounds = c(4000),
                         alpha = c(0.0001))

[361]	test-rmse:1.710669	train-rmse:1.368868 


k = 12, alpha = 0.6
[409]	test-rmse:1.707866	train-rmse:1.296126 
^ganske ræv på test-eksempel