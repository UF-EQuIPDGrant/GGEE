install.packages("HH")
install.packages("lattice")

library(tidytext)
library(dplyr)
library(tidyverse)
library(dplyr)
library(readxl)
library(psych)
library(likert)
library(HH)

end_of_day_surveys <- read_excel("Data/end of day surveys.xlsx", sheet=1)

graph1<-likert(Item~., end_of_day_surveys, ReferenceZero=3, ylab = "Statement", xlab = "Percentage", main = list("Student End of Day Survey", x=unit(.62, "npc")), auto.key = list(columns = 2, reverse.rows = T))

graph1

png("/Users/kristadulany/Documents/GitHub/GGEE/Data/Graph1.png",
    height=720, width=1080)
graph1
dev.off()

###MAKE DATA FRAME BY HAND
Item <- c("I felt confident when completeing today's camp activites", "I enjoyed completing today's camp activities", "I find today's camp activties difficult")

Strongly_Disagree <-c(1.42, 1.42, 23.49)
Somewhat_Disagree <- c(0.71, 1.42, 22.42)
Neither <- c(4.96, 3.90, 22.06)
Somewhat_Agree <- c(27.66, 23.05, 25.98)
Strongly_Agree <- c(65.25, 70.21, 6.05)

df <- data.frame(Item, Strongly_Disagree, Somewhat_Disagree, Neither, Somewhat_Agree, Strongly_Agree)
view(df)

plot(likert(summary = df), plot.percent.neutral=FALSE, legend.position="right")
