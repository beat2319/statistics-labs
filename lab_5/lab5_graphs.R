library(ggplot2)
library(patchwork)
library(ggpubr)
library(ggrain)
#install.packages("patchwork")
#install.packages("ggrain")
# rain plot info https://github.com/njudd/ggrain/blob/main/R/geom_rain.R
# - rain plot tips https://www.youtube.com/watch?v=9w6hAp8VlhE 

data <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/School/3rd_Year/Semester_1/GEOG_3023/Labs/Lab_5/lab5.csv")
data$fftype <- factor(data$fftype, levels = c("low", "med", "high")) #reorganizing the data for the graph to be more concise

# rainplot graph of Effect of Fire Frequency on Average Elevation already normalized
ggplot(data, aes(x = fftype, y = avgelev, fill = fftype)) +
  geom_rain(
    rain.side = 'r',
    alpha = 0.7,
    boxplot.args = list(color="gray20"),
    point.args = list(
      shape = 21,        
      colour = "gray20",  
      stroke = 0.3,
      size = 2
    )
  )+
  labs(
    title = "Effect of Fire Frequency on Average Elevation",
    x = "Fire Frequency",
    y = "Average Elevation (m)",
    fill = "Fire Frequency"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    legend.position = "none"  
  )

# rainplot graph of fftype on mean_dist_grass pre and post normalized
common_grid_layers <- list(  
  geom_rain(
    rain.side = 'r',
    alpha = 0.7,
    boxplot.args = list(color = "gray20"),
    point.args = list(
      shape = 21,        
      colour = "gray20",  
      stroke = 0.3,
      size = 1.5
    )
  ),
  labs(x = "Fire Frequency", fill = "Fire Frequency"),
  theme(
    plot.subtitle = element_text(face = "bold", size = 9, hjust = 0.98),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 9),
    legend.position = "none"
  )
)

a1 <- ggplot(data, aes(x = fftype, y = mean_dist_grass, fill = fftype)) +
  common_grid_layers +
  labs(
    y = "Mean Distance to Grassland",
    subtitle = "raw"
  )

data$log_mean_dist_grass <- log(data$mean_dist_grass)

a2 <- ggplot(data, aes(x = fftype, y = log_mean_dist_grass, fill = fftype)) +
  common_grid_layers +
  labs(
    y = NULL,
    subtitle = "log-transformed"
  )

a_plot <- a1 + a2 + 
  plot_annotation(
    title = "Mean Distance to Grassland by Fire Frequency",
    theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))
  )

print(a_plot)

# rainplot graph of fftype on dis_rav pre and post normalized
common_grid_layers <- list(  
  geom_rain(
    rain.side = 'r',
    alpha = 0.7,
    boxplot.args = list(color = "gray20"),
    point.args = list(
      shape = 21,        
      colour = "gray20",  
      stroke = 0.3,
      size = 1.5
    )
  ),
  labs(x = "Fire Frequency", fill = "Fire Frequency"),
  theme(
    plot.subtitle = element_text(face = "bold", size = 9, hjust = 0.98),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 9),
    legend.position = "none"
  )
)

b1 <- ggplot(data, aes(x = fftype, y = dist_rav, fill = fftype)) +
  common_grid_layers +
  labs(
    y = "Distance to Ravine Drainage",
    subtitle = "raw"
  )

data$log_dist_rav <- log(data$dist_rav)

b2 <- ggplot(data, aes(x = fftype, y = log_dist_rav, fill = fftype)) +
  common_grid_layers +
  labs(
    y = NULL,
    subtitle = "log-transformed"
  )

b_plot <- b1 + b2 + 
  plot_annotation(
    title = "Distance to Ravine Drainage by Fire Frequency",
    theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))
  )

print(b_plot)

# rainplot graph of fftype on avgstep raw data only 
ggplot(data, aes(x = fftype, y = avgstep, fill = fftype)) +
  geom_rain(
    rain.side = 'r',
    alpha = 0.7,
    boxplot.args = list(color="gray20"),
    point.args = list(
      shape = 21,        
      colour = "gray20",  
      stroke = 0.3,
      size = 2
    )
  )+
  labs(
    title = "Average Slope by Fire Frequency",
    x = "Fire Frequency",
    y = "Average Slope",
    fill = "Fire Frequency"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    legend.position = "none"  
  )

# grouped bar chart of tree cover by fire frequency
ggplot(data, aes(x = fftype, fill = factor(covcode))) +
  geom_bar(position = "dodge") +
  labs(
    x = "Fire Frequency",
    y = "Count",
    fill = "Tree Cover Type",
    title = "Tree Cover Type by Fire Frequency"
  ) +
  scale_fill_discrete(
    labels = c("Ponderosa Pine", "Ponderosa/Douglas-fir", "Mixed Conifer")
  )+
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11)
  )

# grouped bar chart of steepness by fire frequency
ggplot(data, aes(x = fftype, fill = factor(step1))) +
  geom_bar(position = "dodge") +
  labs(
    x = "Fire Frequency",
    y = "Count",
    fill = "Slope Steepness",
    title = "Slope Steepness by Fire Frequency"
  ) +
  scale_fill_discrete(
    labels = c("<15 degrees", ">15 degrees")
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11)
  )


