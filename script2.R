#install the package

install.packages("jsonlite")
library(jsonlite)
install.packages("readxl")
library(readxl)

#Reading the data

result <- fromJSON("C:/Users/N-GeetaChabra/Desktop/DARP/data/no_pii_grievance_v2/no_pii_grievance_v2.json")
result <- as.data.frame(result)


result1 <- fromJSON("C:/Users/N-GeetaChabra/Desktop/DARP/data/no_pii_action_history_v2/no_pii_action_history_v2.json")
result1<- as.data.frame(result1)

CategoryCode_Mapping<-read_excel("C:/Users/N-GeetaChabra/Desktop/DARP/data/CategoryCode_Mapping_v2.xlsx", sheet = 2)

#######
Cm=subset(CategoryCode_Mapping, select = c(1,2))
df=print(subset(result, select = c(1,2,11, 15)))

typeof(df$CategoryV7)

df$CategoryV7 <- unlist(df$CategoryV7)

#######

colnames(df)[2]="Code"

total<-merge(df, Cm, by="Code", all.x = "True")


######################

sum(is.na(total$Description))

########
set.seed(475)

######Count complaint by issue
total$Description<-replace(total$Description, is.na(total$Description), 'Not Available') 
total$Code<-replace(total$Code, is.na(total$Code), '000')
#Convert letters to lower case
total$Description<-tolower(total$Description)
nrow(total[total$Description=="other", ])

total$Description[total$Description == "other"] <- "others" 


countbyissue<-aggregate(total$Description, by=list(total$Description), length)


#Give data frame meaniful names
colnames(countbyissue)<-c("issue","count")
sum(countbyissue$count)

#Order data in descending order
countbyissue<-countbyissue[order(-countbyissue$count),]



#Create a bar plot
barplot(countbyissue$count, main = "Count of Each Issue", col="blue")



#Convert complaints into a Corpus using tm package
complaintCorpus<-Corpus(VectorSource(total$Description))


#Create DTM using TM
dtm<-DocumentTermMatrix(complaintCorpus)

#View the DTM
inspect(dtm)

#Sum each term across all documents (i.e column sum)
cumulativeAllTerms<-colSums(as.matrix(dtm))

#Sort in descending order
cumulativeAllTerms<-cumulativeAllTerms[order(-cumulativeAllTerms)]

#Show top 100 terms
head(cumulativeAllTerms, 100)

#Find terms that appear 10 times or more
freqterms<-findFreqTerms(dtm, lowfreq = 10)

#Limit DTM to contain terms that appear >= 10
dtm<-DocumentTermMatrix(complaintCorpus, list(dictionary=freqterms))

#Sum count of each term across all documents
cumulativeAllTerms<-colSums(as.matrix(dtm))

#Sort in descending order and take top 30 terms
Top30<-head(cumulativeAllTerms[order(-cumulativeAllTerms)], 30)

#Convert to data frame
Top30<-data.frame(term=names(Top30), count=Top30)
Top30<-Top30[order(-Top30$count),]



#Plot
barplot(rev(Top30$count), horiz = T, names.arg = Top30$term, las=2, col="blue", main="Most Frequent 30 Terms")


#####

#Convert issue/target variable to factor, in order to conserve levels in case some categories don't appear in one of the set (highly unlikely since data is balanced)
total$Description<-as.factor(total$Description)



#Create an index with 75% split based on issue value in raw data
inTrain<-createDataPartition(total$Description,p=0.75,list=FALSE)

#Subset raw data with index
train<-total[inTrain,]

#Subset raw data with NOT index
test<-total[-inTrain,]

#Subset cleaned corpus for training & test sets
corpustrain<-total[inTrain]
corpustest<-total[-inTrain]

#Create DTM based on subsetted cleaned corpus
dtmtrain<-DocumentTermMatrix(corpustrain, list(dictionary=freqterms))
dtmtest<-DocumentTermMatrix(corpustest, list(dictionary=freqterms))

#Function to convert non-zero values to 1
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
}

#Convert non-zero values to 1 in train and test DRM
dtmtrain<- dtmtrain %>% apply(MARGIN=2,convert_counts)
dtmtest<- dtmtest %>% apply(MARGIN=2,convert_counts)

#Convert DTM to data frames
dtmtrain<-as.data.frame(dtmtrain)
dtmtest<-as.data.frame(dtmtest)

#Bind target variable to test and train DTMs
dtmtrain<-cbind(issue_tv=train$Description,dtmtrain)
dtmtest<-cbind(issue_tv=test$Description,dtmtest)

#Train a model based on Naive Bayes using e1017 package
fit_NB<-naiveBayes(dtmtrain,dtmtrain$issue_tv)

#Predict using Naive Bayes model using the test set
pred_NB<-predict(fit_NB, newdata= dtmtest)

#Create a confusion matrix for that model/prediction
conf_NB<-confusionMatrix(pred_NB,dtmtest$issue_tv)
conf_NB$overall["Accuracy"]





