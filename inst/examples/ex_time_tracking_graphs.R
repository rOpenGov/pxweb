require(ggthemes)

# Time series graph
tsgraph <- ggplot(timeSeries,aes(x=obs,y=time,color=as.factor(level))) + 
	geom_line() +
	geom_smooth() + 
	ggtitle("Data query time series") +
	scale_color_discrete(name="Node level") +
	scale_y_continuous(minor_breaks = seq(0,4,0.25))
tsgraph