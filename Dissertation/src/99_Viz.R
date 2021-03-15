###############################################################################
############## SCRIPT FOR GENERATING DATA VISUALIZATIONS ######################
###############################################################################


# ESTIMATED POINTS VS ACTUAL POINTS FOR ELASTIC NET REGRESSION=================
df <- as.data.frame(cbind(y_train,predictions_train))

ggplot(df)+
  geom_point(aes(x=y_train,y=predictions_train))
