



# Example of opera's results for testing modules --------------------------



library("opera")
library("mgcv")
library("caret")





# data --------------------------------------------------------------------

data(electric_load)
# attach(electric_load)
idx_data_test <- 620:nrow(electric_load)
data_train <- electric_load[-idx_data_test, ] 
data_test <- electric_load[idx_data_test, ]  



# Models ------------------------------------------------------------------

# A generalized additive model using the mgcv package
gam.fit <- gam(Load ~ s(IPI) + s(Temp) + s(Time, k=3) + 
                 s(Load1) + as.factor(NumWeek), data = data_train)
gam.forecast <- predict(gam.fit, newdata = data_test)



# A medium term generalized additive model followed by an autoregressive short-term correction.
# medium term model
medium.fit <- gam(Load ~ s(Time,k=3) + s(NumWeek) + s(Temp) + s(IPI), data = data_train)
electric_load$Medium <- c(predict(medium.fit), predict(medium.fit, newdata = data_test))
electric_load$Residuals <- electric_load$Load - electric_load$Medium

# autoregressive correction
ar.forecast <- numeric(length(idx_data_test))
for (i in seq(idx_data_test)) {
  ar.fit <- ar(electric_load$Residuals[1:(idx_data_test[i] - 1)])
  ar.forecast[i] <- as.numeric(predict(ar.fit)$pred) + electric_load$Medium[idx_data_test[i]]
}


# A gradient boosting model using caret package
gbm.fit <- train(Load ~ IPI + IPI_CVS + Temp + Temp1 + Time + Load1 + NumWeek, 
                 data = data_train, method = "gbm")
gbm.forecast <- predict(gbm.fit, newdata = data_test)




Y <- data_test$Load
X <- cbind(gam.forecast, ar.forecast, gbm.forecast)





# MLpol -------------------------------------------------------------------

MLpol0 <- mixture(model = "MLpol", loss.type = "square")

MLpol <- MLpol0
for (i in 1:length(Y)) {
  MLpol <- predict(MLpol, newexperts = X[i, ], newY = Y[i])
}



# Ridge -------------------------------------------------------------------

Ridge0 <- mixture(model = "Ridge", loss.type = "square")

Ridge <- Ridge0
for (i in 1:length(Y)) {
  Ridge <- predict(Ridge, newexperts = X[i, ], newY = Y[i])
}




# BOA ---------------------------------------------------------------------

BOA0 <- mixture(model = "BOA", loss.type = "square")

BOA <- BOA0
for (i in 1:length(Y)) {
  BOA <- predict(BOA, newexperts = X[i, ], newY = Y[i])
}




