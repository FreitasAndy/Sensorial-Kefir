###Testing

# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(forcats)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/zonination/perceptions/master/probly.csv", header=TRUE, sep=",")
data <- data %>%
  gather(key="text", value="value") %>%
  mutate(text = gsub("\\.", " ",text)) %>%
  mutate(value = round(as.numeric(value),0))


library(Hmisc)
data2 = df[3:9]
hist.data.frame(df_y)

y1<-rnorm(5000,5,1.09)
y2<-rnorm(5000,5,0.80)
y3<-rnorm(5000,5,0.50)
y4<-rnorm(5000,5,0.02)
df_y<-data.frame(y1,y2,y3,y4)

class(df_y,20)

hist(df$ImpGlobal, 400)

library(ggplot2)
library(tidyr)

DevType <- c('Designer', 'Developer, Back', 'Developer, front', 'Engineer')
Salary <- c(120, 340, 72, 400)
Master <- c('1', '2', '3', '4')
Bachelor <- c('6', '1', '3', '1')
University <- c('6', '2', '0', '2')
data1 <- data.frame(DevType, Salary, Master, Bachelor, University)

data1 <- gather(data1, key, value, -DevType, -Salary)
