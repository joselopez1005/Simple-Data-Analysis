
library(ggplot2)
library(sampling)
library(scatterplot3d)
library(Matrix)
library(arules)

AdultUCI <- read.csv("C:/Users/User/Downloads/AdultUCI.dat")


x <- data.frame(AdultUCI)

#Based on the result, there are missing values in the data set.
any(is.na(x))

#Will only return a dataframe that has no duplicated rows as well as remove any rows with missing values
clean_x <- unique(x[complete.cases(x),])


#Looking at the is.na() function, there are no missing values found within the clean_x data frame
any(is.na(clean_x))

#Displaying distributions of attributes
hist(clean_x$age)
boxplot(clean_x$education.num)
hist(clean_x$hours.per.week)
barplot(table(clean_x$relationship), xlab="Relationship", ylab="count")
barplot(table(clean_x$income), xlab="Income", ylab="count");

#Displaying relationships between continous and discrete attributes
ggplot(clean_x, aes(y = clean_x$age, x = clean_x$income)) +geom_point()
ggplot(clean_x, aes(x = clean_x$hours.per.week, y = clean_x$occupation)) +geom_point()
boxplot(clean_x$age ~ clean_x$education, data = clean_x, horizontal = TRUE)
boxplot(clean_x$education.num ~ clean_x$income, data = clean_x, horizontal = TRUE)


continous_df <- data.frame(clean_x$age, clean_x$fnlwgt, clean_x$education.num, clean_x$capital.gain,
                        clean_x$capital.loss, clean_x$hours.per.week)
#Will group them according to their education level
aggregate(.~clean_x$education, data=continous_df, FUN = mean)


continous_df <- data.frame(clean_x$age, clean_x$fnlwgt, clean_x$education.num, clean_x$capital.gain,
                           clean_x$capital.loss, clean_x$hours.per.week)
#Will find the euclidean distance from the first 8 objects.
dist(continous_df[1:8,], method="euclidean")
#WIll scale the objects
continous_df <- scale(continous_df)
dist(continous_df[1:8,], method="euclidean")


random_df <- clean_x[sample(1:nrow(clean_x), 500, replace = TRUE),]
#Since TRUE = 1 and False = 0, we can sum to find the total duplicates
sum(duplicated(random_df))


scatter_df <- data.frame(clean_x$age, clean_x$education.num, clean_x$hours.per.week)
scatterplot3d(scatter_df)
pc <- prcomp(as.matrix(scatter_df))
#Will tell us more information about the PC
summary(pc)
#WIll graph the most important PC
plot(pc)
plot(pc$x)

summary(discretize(clean_x$age, method = "interval", breaks = 3))
summary(discretize(clean_x$age, method = "frequency", breaks = 3))


continous_df <- data.frame(clean_x$age, clean_x$fnlwgt, clean_x$education.num, clean_x$capital.gain,
                           clean_x$capital.loss, clean_x$hours.per.week)
cor(continous_df)
