#PACKAGES
library(tidyverse)

#READ IN THE DATA
training.data <- read.csv("training_set.csv", header=TRUE)
test.data <- read.csv("test_set.csv", header=TRUE)
head(training.data)
head(test.data)

#PREDICTION FORMAT
n.test <- 100
g.hat <- sample(c("A", "B", "C", "D", "E", "F", "G", "H", "J", "K", "L", "M", 
                  "N", "X"), n.test, replace = TRUE) # random predictions
y.hat <- rnorm(n.test,6,1)
write.csv(cbind(g.hat,y.hat), file = "chemical_predictions_group_F_week_1.csv", 
          row.names=FALSE)

#SCATTER PLOTS
training.data = training.data %>%
  mutate(Impurity.Type.Num = recode(Impurity.Type, "A"=1, "B"=2, "C"=3, "D"=4, 
                                    "E"=5, "F"=6, "G"=7, "H"=8, "J"=9, "K"=10, 
                                    "L"=11, "M"=12, "N"=13))
pairs(training.data[,c("Impurity.Percent", "Impurity.Type.Num", "I", "II", 
                       "III", "IV", "V", "Temp")])

#I-V correlate with the impurity type
labels = c("A", "B", "C", "D", "E", "F", "G", "H", "J", "K", 
           "L", "M", "N")
reversed_labels <- rev(labels)

#Values of I above 103 lead to impurity type A
ggplot(training.data, aes(x = I, y = Impurity.Type.Num, color = factor(Impurity.Type.Num))) +
  geom_point(size=2.5) + 
  scale_y_continuous(breaks = 1:13, labels = labels) +  # Custom y-axis labels
  labs(x = "I value", y = "Impurity Type") +  # Axis labels
  scale_color_manual(values = rainbow(13)[13:1]) +  # Map colors to labels
  theme(legend.position = "none", text = element_text(size=20)) +  # Remove the legend and set text size
  geom_vline(xintercept = 103, linetype = "dashed", color = "black", size = 0.5) + # Line at 103
  annotate("text", x = 104, y = 13, label = "I = 103", angle = 0, size=7)  # Add label next to line

#Values of II above 17 lead to D, G, J or M
ggplot(training.data, aes(x = II, y = Impurity.Type.Num, color = factor(Impurity.Type.Num))) +
  geom_point(size=2.5) + 
  scale_y_continuous(breaks = 1:13, labels = labels) +  # Custom y-axis labels
  labs(x = "II value", y = "Impurity Type") +  # Axis labels
  scale_color_manual(values = rainbow(13)[13:1]) +  # Map colors to labels
  theme(legend.position = "none", text = element_text(size=20)) +  # Remove the legend and set text size
  geom_vline(xintercept = 16.7, linetype = "dashed", color = "black", size = 0.5) + # Line at 103
  annotate("text", x = 18.5, y = 13, label = "II = 16.7", angle = 0, size=7)  # Add label next to line

#Values of III above 27 lead to impurity type E
ggplot(training.data, aes(x = III, y = Impurity.Type.Num, color = factor(Impurity.Type.Num))) +
  geom_point(size=2.5) + 
  scale_y_continuous(breaks = 1:13, labels = labels) +  # Custom y-axis labels
  labs(x = "III value", y = "Impurity Type") +  # Axis labels
  scale_color_manual(values = rainbow(13)[13:1]) +  # Map colors to labels
  theme(legend.position = "none", text = element_text(size=20)) +  # Remove the legend and set text size
  geom_vline(xintercept = 15, linetype = "dashed", color = "black", size = 0.5) + # Line at 103
  annotate("text", x = 20, y = 13, label = "III = 15", angle = 0, size=7)  # Add label next to line


#Values of IV above 17 lead to impurity type B, G or H
g_data <- training.data[training.data$Impurity.Type.Num == 7,]  # "G" corresponds to 7
g_max_iv <- g_data[which.max(g_data$IV), ]  # Find the row with the largest IV
ggplot(training.data, aes(x = IV, y = Impurity.Type.Num, color = factor(Impurity.Type.Num))) +
  geom_point(size = 2.5) +  # Plot the points
  scale_y_continuous(breaks = 1:13, labels = labels) +  # Custom y-axis labels
  labs(x = "IV value", y = "Impurity Type") +  # Axis labels
  scale_color_manual(values = rainbow(13)[13:1]) +  # Map colors to labels
  theme(legend.position = "none", text = element_text(size = 20)) +  # Remove the legend and set text size
  geom_vline(xintercept = 18, linetype = "dashed", color = "black", size = 0.5) + # Line at 103
  annotate("text", x = 21, y = 13, label = "IV = 18", angle = 0, size = 7) +  # Add label next to line
  # Add a red circle around the "G" value with the largest IV
  geom_point(data = g_max_iv, aes(x = IV, y = Impurity.Type.Num), color = "red", size = 6, shape = 1)  # Red circle

#Values of V
ggplot(training.data, aes(x = V, y = Impurity.Type.Num, color = factor(Impurity.Type.Num))) +
  geom_point(size=2.5) + 
  scale_y_continuous(breaks = 1:13, labels = labels) +  # Custom y-axis labels
  labs(x = "V value", y = "Impurity Type") +  # Axis labels
  scale_color_manual(values = rainbow(13)[13:1]) +  # Map colors to labels
  theme(legend.position = "none", text = element_text(size=20)) +  # Remove the legend and set text size
  geom_vline(xintercept = 51, linetype = "dashed", color = "black", size = 0.5) + # Line at 103
  annotate("text", x = 49, y = 13, label = "V = 51", angle = 0, size = 7) # Add label next to line
  


#EXPLORATORY PLOTS

#Bar plot
ggplot(data = training.data) +
  geom_bar(mapping = aes(Impurity.Type))

#Histogram
ggplot(data = training.data) +
  geom_histogram(mapping = aes(I), binwidth=0.5)
ggplot(data = training.data) +
  geom_histogram(mapping = aes(II), binwidth=0.5)
ggplot(data = training.data) +
  geom_histogram(mapping = aes(III), binwidth=0.5)
ggplot(data = training.data) +
  geom_histogram(mapping = aes(IV), binwidth=0.8)
ggplot(data = training.data) +
  geom_histogram(mapping = aes(V), binwidth=0.5)

#Frequency polygons
  #I
ggplot(data = training.data, mapping = aes(x = I, colour = Impurity.Type)) +
  geom_freqpoly(binwidth = 0.1)
smaller.I <- training.data %>% 
  filter(I < 102)
ggplot(data = smaller.I, mapping = aes(x = I, colour = Impurity.Type)) +
  geom_freqpoly(binwidth = 0.1)

  #II
ggplot(data = training.data, mapping = aes(x = II, colour = Impurity.Type)) +
  geom_freqpoly(binwidth = 0.1)
left.II = training.data %>%
  filter(II<16)
ggplot(data = left.II, mapping = aes(x = II, colour = Impurity.Type)) +
  geom_freqpoly(binwidth = 0.1)
right.II = training.data %>%
  filter(II>=16)
ggplot(data = right.II, mapping = aes(x = II, colour = Impurity.Type)) +
  geom_freqpoly(binwidth = 0.1)
right.II = training.data %>%
  filter(II>20)
ggplot(data = right.II, mapping = aes(x = II, colour = Impurity.Type)) +
  geom_freqpoly(binwidth = 1)
  #Values of II above 20 tend to lead to impurity type M (in general)

  #III
ggplot(data = training.data, mapping = aes(x = III, colour = Impurity.Type)) +
  geom_freqpoly(binwidth = 0.1)
smaller.III <- training.data %>% 
  filter(III < 20)
ggplot(data = smaller.III, mapping = aes(x = III, colour = Impurity.Type)) +
  geom_freqpoly(binwidth = 0.1)

  #IV
ggplot(data = training.data, mapping = aes(x = IV, colour = Impurity.Type)) +
  geom_freqpoly(binwidth = 0.1)
left.IV = training.data %>%
  filter(IV < 15)
ggplot(data = left.IV, mapping = aes(x = IV, colour = Impurity.Type)) +
  geom_freqpoly(binwidth = 1)

  #V
ggplot(data = training.data, mapping = aes(x = V, colour = Impurity.Type)) +
  geom_freqpoly(binwidth = 0.1)
left.V = training.data %>%
  filter(V<=51)
ggplot(data = left.V, mapping = aes(x = V, colour = Impurity.Type)) +
  geom_freqpoly(binwidth = 1)
right.V = training.data %>%
  filter(V>51)
ggplot(data = right.V, mapping = aes(x = V, colour = Impurity.Type)) +
  geom_freqpoly(binwidth = 1)

#Boxplots
ggplot(data = training.data, mapping = aes(x = Impurity.Type, y = Impurity.Percent)) +
  geom_boxplot()
ggplot(data = training.data, mapping = aes(x = Impurity.Type, y = Temp)) +
  geom_boxplot()

ggplot(data = training.data, mapping = aes(x = Impurity.Type, y = I)) +
  geom_boxplot()
Type.C = training.data %>%
  filter(Impurity.Type == "C")
ggplot(data = training.data, mapping = aes(x = Impurity.Type, y = I)) +
  geom_point()

ggplot(data = training.data, mapping = aes(x = Impurity.Type, y = II)) +
  geom_boxplot()
