library(ggplot2)
library(dplyr)


data <- read.csv('data/inc5000_data.csv')

#Question 1

state_summary <- count(data, State)

state_summary <- arrange(state_summary, desc(n))

state_plot <- ggplot(data=state_summary, aes(x = reorder(State,n), y=n)) + 
  geom_bar(stat="identity", width=.5, position = position_dodge(width=1)) + 
  coord_flip() +
  labs(title="Fastest Growing American Companies by State") + xlab("State") + ylab("Count") +
  theme(axis.ticks.y=element_blank()) 


#Question 2

ny <- subset(data,State == "NY")
ny_complete <- complete.cases(ny[,])
table(ny_complete)

ny_indus <- group_by(ny, Industry)

ny_avg_ees <- summarize(ny_indus, avg_ee = mean(Employees))

ny_bp <- ggplot(ny_indus, aes(factor(Industry), Employees))

ny_bp + geom_boxplot() + ylim(0,1000)

ny_bp + geom_boxplot() + ylim(0,1000) + scale_x_discrete(breaks=NULL)

ny_bp + geom_boxplot() + ylim(0,250) + theme(axis.text.x = element_text(angle=90)) +coord_flip()

#Question 3

data_complete <- subset(data, is.na(data$Employees) == FALSE)

data_complete$revperee <- data_complete$Revenue / data_complete$Employees

industry <- group_by(data_complete, Industry)

industry_rev_per_ee <- summarize(industry, revperee = mean(revperee))

industry_plot <- ggplot(data=industry_rev_per_ee, aes(x=Industry, y=revperee)) +
                  geom_bar(stat="identity")