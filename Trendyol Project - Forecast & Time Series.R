
age.gender <- select(personality, Age, Gender) # Just those two variables. dplyr approach

age.gender.base <- data.frame(personality$Age, personality$Gender) # base package approach using the "$" sign.
# base package approach using indexing using squared brackets instead of the "$" sign.
age.gender.base.indexing <- data.frame(personality[,"Age"], personality[,"Gender"]) 

extra1.to.item21_open <- select(personality, item1_extra:item21_open) #dplyr approach
extra.to.item12_aggr.base <- personality[,c(4:24)] #base approach

only.items <- select(personality, starts_with("item"), ends_with("_2")) # Selects all vars starting with "item" and all vars end with _2

males_older_20 <- filter(personality, Age > 20 & Gender == 2)
males.20.base <- personality[personality$Age > 20 & personality$Gender == 2, ]


extra <- select(personality, ends_with("extra"))
# check the reliability of our personality dimensions
psych::alpha(extra)
psych::describe(open)


personality$Agre <- rowMeans(select(personality, ends_with("aggr")))
personality$sum.extra <- rowSums(select(personality, ends_with("extra"))
                                 
###### Data Management ######

names(df.test.data)
colnames(df.test.data) <- c("item_1", "item_2", "item_3")

df.test.data[df.test.data==-99] <- NA
df.test.data.missig.na <- dplyr::na_if(df.test.data,-99) #dplyr

df.test.data <- mutate(df.test.data, NA_Col = ifelse(item_1 == -99 & item_1<0, NA, df.test.data$item_1))
                                 
table(is.na(df.test.data))      

df.test <- df.test.data[!is.na(df.test.data$item_1),] #check only first item
df.test.data.complete.cases <- df.test.data[complete.cases(df.test.data),] # return logical vector
df.test.data.complete.cases.omitted.na <- na.omit(df.test.data)


personality$item2_aggr <-  dplyr::recode(personality$item2_aggr, '1' = '5', '5'='1', '2'='4', '4'='2', '3'='3')
personality$item2_aggr <- 6 - personality$item2_aggr

####################
# MERGE

stacked.df <- rbind(first, second) # rowbind - need same number of columns
bidcols <- cbind(first, third) # colbind - need same number of rows


############
# DESCRIPTIVE

sapply(personality[, 26:30], describe)
tapply(personality$Extra, personality$Gender.f, describe) # different for female and male
aggregate(personality$Extra ~ personality$Gender.f, FUN = 'mean')


##########################
# gives the position of Age in columns
grep('Age', colnames(personality))

cor(openness)
description <- describe(openness) # 
summary(openness)

############################
sink('dnafkjbnasdi.txt')
print("this is the correlation matrix")
round(cor(extra.to.item12_aggr.base),2) # Rounded to two decimals
describe(extra.to.item12_aggr.base)
sink()

write.csv(df.bfik, "df.bfik.csv")
write.csv2(df.bfik, "df.bfik.comma.decimal.csv")

# write.csv uses "." for the decimal point and a comma for the separator.
# write.csv2 uses a comma for the decimal point and a semicolon for the separator,
# the Excel convention for CSV files in some Western European locales.



df.summarise <- personality %>% group_by(Gender,Age) %>% summarise(Mean = mean(item1_extra)
iris                                                              
iris  %>% group_by(Species)  %>% summarise(Mean = mean(Sepal.Length))


boxplot(Sepal.Width ~ Species, data = iris, frame = FALSE,
        col = c("#999999", "#E69F00", "#56B4E9"), ylab = "Sepal Length")
boxplot(Sepal.Length ~ Species, data = iris, frame = FALSE,
        col = c("#999999", "#E69F00", "#56B4E9"), ylab = "Sepal Length")
boxplot(Sepal.Width ~ Species, data = iris, frame = FALSE,
        col = c("#999999", "#E69F00", "#56B4E9"), ylab = "Sepal Length")
boxplot(Sepal.Length ~ Species, data = iris, frame = TRUE,
        col = c("#999999", "#E69F00", "#56B4E9"), ylab = "Sepal Length")

boxplot(Sepal.Length ~ Species, data = iris, frame = FALSE,
        col = c("#999999", "#E69F00", "#56B4E9"), ylab = "Sepal Length")

iris_new <- iris[,1:4]
iris_new
iris_new$var <- rowMeans(iris_new)
iris_new
round(mean(iris_new$var), digits = 4)

person<- read_sav("BFI_K_English.sav")
aggreable <- select(person, ends_with("aggr"))
psych::alpha(aggreable)

aggreable$item2_aggr <-  6 - aggreable$item2_aggr
psych::alpha(aggreable)


plot(iris$Sepal.Length, iris$Petal.Length)
iris
iris$new <-  factor(iris$Species, levels = c(1,2,3))
iris

x <- data.frame(-1,0,1,2,3)
x
y <- x^2
y


boxplot(Sepal.Width ~ Species, data = iris, frame = FALSE,
        col = c("#999999", "#E69F00", "#56B4E9"), ylab = "Sepal Length")
boxplot(Sepal.Length ~ Species, data = iris, frame = FALSE,
        col = c("#999999", "#E69F00", "#56B4E9"), ylab = "Sepal Length")
