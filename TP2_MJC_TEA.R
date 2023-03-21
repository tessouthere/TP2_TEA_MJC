## Drug Performance Evaluation - Exploratory Data Analysis ##
  # Tess Anderson and Matthew Carrico #

library(tidyverse)
# Loading in the Dataset #
data <- read_csv('Drug_clean.csv')

# Exploring the Dataset #
head(data)
glimpse(data)
summary(data)
view(data)
names(data)

# Checking for Missing Data #
colSums(is.na(data)) # no missing data in any of the columns

# Statistical Summary of Quantitative Variables in Dataset #
data_QuantitativeOnly<-data[c(3,4,7,8,9)]
summary(data_QuantitativeOnly)

# Summary of Qualitative Variables in Dataset #
# Frequency Tables for Each Categorical Variable #
tableCondition <- table(data$Condition)
tableForm <- table(data$Form)
tableIndication <- table(data$Indication)
tableType <- table(data$Type)
# Proportion Tables for Each Categorical Variable #
prop.table(tableCondition)
prop.table(tableForm)
prop.table(tableIndication)
prop.table(tableType)

# Correlation Calculations to Check for Relationships between Variables #
# Correlation Table - Examining relationships between all numerical variables. #
library('psych')
library(ggplot2)
library(reshape2)
dataCorrelation <- cor(data_QuantitativeOnly)
melted <- melt(dataCorrelation)
library(ggplot2)
heatmap <- ggplot(melted, aes(Var1, Var2)) +                           # Create heatmap with ggplot2
  geom_tile(aes(fill = value))
heatmap <- heatmap + 
  geom_text(aes(Var2, Var1, label = round(value,digits=2)), color = "white", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = 'bottom',
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
heatmap

# Histograms and Distributions #
ggplot(data=data,
       aes(x=Satisfaction))+
  geom_histogram()+
  labs(title='Histogram of Satisfaction Ratings')

ggplot(data=data,
       aes(x=Effective))+
  geom_histogram()+
  labs(title='Histogram of Effectiveness Ratings')

ggplot(data=data,
       aes(x=EaseOfUse))+
  geom_histogram()+
  labs(title='Histogram of Ease of Use Ratings')

ggplot(data=data,
       aes(x=Price))+
  geom_histogram()+
  labs(title='Histogram of Price')
  
ggplot(data=data,
       aes(Price))+
  geom_boxplot(fill='red')+
  labs(title="Boxplot of Price",
       x='Drug Price')

# Scatterplots and Pairwise #
data %>% 
  ggplot(aes(Satisfaction, Effective))+
  geom_point(size=2)+
  labs(title='Drug Satisfaction vs. Effectiveness Rating')
data %>% 
  ggplot(aes(EaseOfUse,Effective))+
  geom_point(size=2)+
  labs(title='Ease of Use vs. Effectivness Rating')
data %>% 
  ggplot(aes(EaseOfUse,Satisfaction))+
  geom_point()+
  labs(title='Ease of Use vs. Drug Satisfaction')
pairs(data_QuantitativeOnly)

# Barplots #
data %>% 
  ggplot(aes(Type))+
  geom_bar()
data %>% 
  ggplot(aes(Indication))+
  geom_bar()
data %>% 
  ggplot(aes(Form))+
  geom_bar()
