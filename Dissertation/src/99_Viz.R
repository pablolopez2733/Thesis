###############################################################################
############## SCRIPT FOR GENERATING DATA VISUALIZATIONS ######################
###############################################################################


# ESTIMATED POINTS VS ACTUAL POINTS FOR ELASTIC NET REGRESSION for CLE=================
df <- as.data.frame(cbind(y_train,predictions_train))

cle_en <- ggplot(df, aes(x = y_train, y = predictions_train))+
  geom_point(aes(x=y_train,y=predictions_train)) +
  stat_smooth(geom='line', alpha=0.8, se=FALSE, method='lm',colour="#fc56d0")+
  dark_theme_gray() +
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, family = "Calibri",color = "white",hjust = 0),
    plot.subtitle = element_text(size = 11, family = "Calibri",color = "white",hjust = 0),
    axis.title = element_text(size = 9, family = "Calibri",color = "#adadad"),
    plot.caption = element_text(size=8, family = "Serif",color = "#adadad",hjust = 0),
    axis.text = element_text(size = 9, family = "Trebuchet MS",color = "#adadad"),
    panel.grid = element_line(size=.11))+
  labs(x='Predicted point differential', y='Actual point differential',
       title = "Predicted Score Differential vs Actual Score Differential",
       subtitle = 'R^2 = 0.6077 | Team: Cleveland Cavaliers | 2014-2015 Season',
       caption = "Model: elastic net regression | Data: Training Set" )

cle_en

# 
