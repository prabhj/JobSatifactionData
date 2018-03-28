
library(rvest)
wiki_url <- read_html("http://wiki.socr.umich.edu/index.php/SOCR_Data_2011_US_JobsRanking#2011_Ranking_of_the_200_most_common_Jobs_in_the_US")
html_nodes(wiki_url, "#content")
pd_data <- html_table(html_nodes(wiki_url, "table")[[1]])
head(pd_data)
summary(pd_data)
pd_data$Description
str(pd_data)
pd_data$Job_Title=as.factor(pd_data$Job_Title)
install.packages("tm", dependencies = TRUE)
require(tm)

# Cleaning the description

#Remove underscores
pd_data$Description <- gsub("[^a-zA-Z]+", " ", pd_data$Description)
pd_data$Description[1:4]

#Convert JD data to corpus
JD_data = Corpus(VectorSource(pd_data$Description))
JD_data
inspect(JD_data[1:2])

#lowercase
JD_clean_data= tm_map(JD_data,tolower)
inspect(JD_clean_data[1:2])

#Handle punctuations
JD_clean_data= tm_map(JD_clean_data,removePunctuation)
inspect(JD_clean_data[1:2])

#Handle whitespaces
JD_clean_data= tm_map(JD_clean_data,stripWhitespace)
inspect(JD_clean_data[1:2])

#Create a document term matrix
dt_mat=DocumentTermMatrix(JD_clean_data)
inspect(dt_mat)

set.seed(10000)
idx <- sample(nrow(pd_data),floor(nrow(pd_data)*0.9))    # 80% training + 20% testing
train_data<-pd_data[idx, ]
test_data<-pd_data[-idx, ]
dt_mat_train<-dt_mat[idx, ]
dt_mat_test<-dt_mat[-idx, ]
corpus_train<-JD_clean_data[idx]
corpus_test<-JD_clean_data[-idx]

#Distributing Stress_Category and Hiring_Potential
prop.table(table(train_data$Stress_Category))
prop.table(table(test_data$Stress_Category))
prop.table(table(train_data$Hiring_Potential))
prop.table(table(test_data$Hiring_Potential))

train_data$stage<-train_data$Stress_Category %in% c(3:5)
train_data$stage<-factor(train_data$stage, levels=c(F, T), labels = c("low", "high"))
test_data$stage<-test_data$Stress_Category %in% c(3:5)
test_data$stage<-factor(test_data$stage, levels=c(F, T), labels = c("low", "high"))

prop.table(table(train_data$stage))
prop.table(table(test_data$stage))

library(wordcloud)


wordcloud(corpus_train, min.freq = 4, random.order = FALSE)


# graphically visualize
low_st<-subset(train_data, stage=="low")
high_st<-subset(test_data, stage=="high")
wordcloud(low_st$Description, max.words = 15)

summary(findFreqTerms(dt_mat_train, 5))

dict<-as.character(findFreqTerms(dt_mat_train, 3))
train<-DocumentTermMatrix(corpus_train, list(dictionary=dict))
test<-DocumentTermMatrix(corpus_test, list(dictionary=dict))

count_f <- function(wordFreq) {
  wordFreq <- ifelse(wordFreq > 0, 1, 0)
  wordFreq <- factor(wordFreq, levels = c(0, 1), labels = c("No", "Yes"))
  return(wordFreq)
}
train <- apply(train, MARGIN = 2, count_f)
test <- apply(test, MARGIN = 2, count_f)
head(train)
dim((train))
dim((test))

#Naive Bayes implementation
library(e1071)
classifier <- naiveBayes(train, train_data$stage)

test_pred<-predict(classifier, test)

library(caret)
library(gmodels)
CrossTable(test_pred, test_data$stage)
caret::confusionMatrix(test_pred, test_data$stage)

test_data$Stress_Category

#confusionMatrix(test_pred, test_data$stage)
CrossTable(test_pred,test_data$stage)

# improving
classifier_nb <- naiveBayes(train, train_data$stage,laplace = 15)

test_pred<-predict(classifier_nb, test)

library(caret)
library(gmodels)
CrossTable(test_pred, test_data$stage)
caret::confusionMatrix(test_pred, test_data$stage)

test_data$Stress_Category

library(MASS)
train_DF = data.frame(lapply(as.data.frame(train),as.numeric), stage = train_data$stage)
test_DF = data.frame(lapply(as.data.frame(test),as.numeric), stage = test_data$stage)

lda <- lda(data=train_DF, stage~.)
pred2 = predict(lda, test_DF)

CrossTable(pred2$class, test_DF$stage)

# LDA Confusion Matrix
print("LDA Confusion Matrix")
confusionMatrix(pred2$class, test_DF$stage)

# Naive Bayes with Laplace Smoothening Confusion Matrix
print("Naive Bayes with Laplace Smoothening Confusion Matrix")
caret::confusionMatrix(test_pred, test_data$stage)

head(test_DF$stage)

library(party)
data_ctree <- ctree(pd_data_train$stage ~ pd_data_train$Job_Title  + pd_data_train$`Average_Income(USD)`+pd_data_train$Overall_Score , data=pd_data_train)
plot(data_ctree, cex=2)

library(rpart)
library(rattle)

iris_rpart = rpart(train_data$stage~  train_data$`Average_Income(USD)`+train_data$Overall_Score + train_data$Work_Environment  + train_data$Physical_Demand + train_data$Hiring_Potential, data=train_data)
print(iris_rpart)
fancyRpartPlot(iris_rpart, cex = 1.5, caption = "rattle::Predictions for stress")

# taking into account the stress level
iris_rpart = rpart(train_data$stage~ train_data$`Average_Income(USD)`+train_data$Overall_Score + train_data$Work_Environment + train_data$Stress_Level + train_data$Physical_Demand + train_data$Hiring_Potential, data=train_data)
print(iris_rpart)
fancyRpartPlot(iris_rpart, cex = 1.5, caption = "rattle::Predictions for stress")

library(C50)
set.seed(1234)
qol_model<-C5.0(train_data[,-c(10,11)], train_data$stage)
qol_model

summary(qol_model)
qol_pred<-predict(qol_model, test_data[ ,-c(10,11)])  # removing the last 2 columns CHRONICDISEASESCORE and cd, whjich represent the clinical outcomes we are predicting!
# install.packages("caret")
library(caret)
confusionMatrix(table(qol_pred, test_data$stage))

lm_data=pd_data[,-c(10)]
summary(lm_data$Overall_Score)
hist(lm_data$Overall_Score, main="histogram for overall score")
library(GGally)

bin = lm_data

bin$bi_score = as.factor(ifelse(bin$Overall_Score>median(bin$Overall_Score),1,0))

g_score <- ggpairs(data=bin[,-c(2)], title="High Low Overall score",
                   mapping=ggplot2::aes(colour = bi_score),
                   lower=list(combo=wrap("facethist",binwidth=1)),
                   # upper = list(continuous = wrap("cor", size = 4.75, alignPercent = 1))
)
g_score

cor(lm_data[c(3,4,5,6,8)])

car::vif(lm(Overall_Score ~ Work_Environment+Stress_Level+Physical_Demand+lm_data$`Average_Income(USD)` + Hiring_Potential, data=lm_data))

library(psych)
pairs.panels(lm_data[c(3,4,5,6,8,9)])

# lm without Job Title
fit_li=lm(lm_data$Overall_Score~ Work_Environment+Stress_Level+Physical_Demand+lm_data$`Average_Income(USD)` + Hiring_Potential, data=lm_data)
fit_li

plot(fit_li, which = 1:2)

# Improving

new_step= step(fit_li,k=2,direction = "backward")
summary(new_step)
