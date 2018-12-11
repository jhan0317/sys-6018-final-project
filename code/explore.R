# SYS 6018 Final Project

#####################
# Working Directory #
#####################

set.seed(4.669)

# Set working directory
wd = "F:/2018 Fall/SYS 6018, Data Mining/final"
setwd(wd)

library(data.table)
reviews = fread("allreviews_v2.csv")

#############
# ClassName #
#############

# Get the Department names and Course Number of the classes
dept = gsub("[[:digit:]]","",reviews$className)
nums = gsub("[^[:digit:]]","",reviews$className)
reviews$dept = dept
reviews$nums = nums

# Check to see how long the course numbers are. They should be exactly 4
len = unlist(lapply(as.numeric(nums), nchar))
hist(len)

# Not all of them are 4. Some are missing and some have more. These are errors

# Set NA as 0000
reviews[is.na(len)]$nums = "0000"

# Set all 3 characters to 4 by pasting 0 at the end
# These are errors due to people not familiar with UVA's system
# (e.g. ECON 201 vs ECON 2010)
reviews[len==3]$nums = unlist(lapply(reviews[len==3]$nums, function(x) paste(x,"0",sep="")))

# 6 Characters and 1 Characters have too little info about what they should have
# been. Set to 0000. These do not make up much of the data
reviews[nchar(reviews$nums)!=4]$nums = "0000"

# Histogram of the values
hist(as.numeric(reviews$nums))

# For the department get only the first 4 characters of the department name
reviews$dept = unlist(lapply(reviews$dept, function(x) substr(x, 1, 4)))

# Drop class name now that we are done with it
reviews$className = NULL

###################
# Professor Names #
###################

# Read in professor info
prof_info = fread("prof_info.csv")

# Get first and last names
first = sapply(strsplit(prof_info$name," "), head, 1)
last = sapply(strsplit(prof_info$name," "), tail, 1)
prof_info$first = first
prof_info$last = last

# Drop name because we are done with it
prof_info$name = NULL

#########################
# Categorical Variables #
#########################

# For each of the "color" ratings, turn them into numeric variables (ordinal)
cols = c("clarityColor","easyColor","helpColor")
dict = setNames(c(1,2,3),c("poor","average","good"))

# Iterate through these and use the dictionary to set their values
for (col in cols) {
  temp = reviews[,col,with=FALSE]
  temp[] = dict[unlist(temp)]
  reviews[[col]] = temp[[1]]
}

# Repeat for "quality" and "interest"
dict = setNames(c(1,2,3,4,5),c("awful","poor","average","good","awesome"))
temp = reviews[,"quality",with=FALSE]
temp[] = dict[unlist(temp)]
reviews[["quality"]] = temp[[1]]

dict = setNames(c(1,2,3,4,5),c("Low","Meh","Sorta interested","Really into it","It's my life"))
temp = reviews[,"interest",with=FALSE]
temp[] = dict[unlist(temp)]
reviews$interest_val = reviews$interest
reviews$interest = NULL
reviews[["interest_val"]] = temp[[1]]

# If there are any missing values in "interest" set it to the middle at 3
reviews$interest_val[is.na(reviews$interest_val)]=3

################
# Textbook Use #
################

# Err on the side of yes for textbook usage
table(reviews$textBookUse)

reviews$textBookUse = reviews$textBookUse != "No"
table(reviews$textBookUse)

##########
# Grades #
##########

# Merge the ground truth values from allcourses.csv
tempcourse = fread("allcourses.csv")
tempcourse = tempcourse[tempcourse$Total>0]

# A mode function
# https://www.tutorialspoint.com/r/r_mean_median_mode.htm
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Get the grades a particular professor has given out in the past years and find
# the proportion of them. This could be useful for predictions.
agg = tempcourse[, .(
  First=getmode(`Instructor First Name`),
  Last=getmode(`Instructor Last Name`),
  `Course GPA`=mean(`Course GPA`),
  A=(sum(A)+sum(`A+`))/sum(Total),
  `A-`=sum(`A-`)/sum(Total),
  `B+`=sum(`B+`)/sum(Total),
  B=sum(B)/sum(Total),
  `B-`=sum(`B-`)/sum(Total),
  `C+`=sum(`C+`)/sum(Total),
  C=sum(C)/sum(Total),
  `C-`=sum(`C-`)/sum(Total),
  `D+`=sum(`D+`)/sum(Total),
  D=sum(D)/sum(Total),
  `D-`=sum(`D-`)/sum(Total),
  `F`=sum(`F`)/sum(Total),
  Total=sum(Total)
  ), 
  by=c("Instructor Email","Subject","Course Number")]

##################
# Merge together #
##################

# Get ready to merge all of the information we have extracted so far together

names(reviews)
names(prof_info)

# Merge the first dataframe, reviews, with prof_info using teacher ids
reviews = merge(reviews, prof_info, by.x="teacher_id", by.y="tid", all.x=TRUE)

reviews$nums = as.numeric(reviews$nums)
temp = merge(reviews, agg, by.x=c("first", "last", "dept", "nums"),
             by.y=c("First", "Last", "Subject", "Course Number"),
             all.x=TRUE)

# We only care about cases where we have the ground truth values
temp_complete = complete.cases(temp)
final = temp[temp_complete]

# Remove these as they do not provide too much information/are redundant
final[,c("teacher_id","reviewDate","sId","unUsefulGrouping","usefulGrouping","Instructor Email")]=NULL

#############
# Get Dummy #
#############

# Here get all of the teacher tags and turn them into dummy varaibles

library(qdapTools)

final$teacherTags = unlist(lapply(final$teacherTags, function(x) gsub("\\[|\\]|'","",x)))
final$teacherTags = lapply(final$teacherTags, function(x) as.list(strsplit(x,", ")[[1]]))
dummy = mtabulate(final$teacherTags)
final$teacherTags = NULL
final = cbind(final, dummy)

###########
# Do text #
###########

# Do some preliminary text analysis and cleaning

library(tm)
library(openNLP)
library(stringr)
library(plyr)
library(text2vec)

train.clean = final$comments

# Trim whitespace
train.clean = trimws(train.clean)
# Convert ascii characters (will remove most)
train.clean = iconv(train.clean, to='ASCII', sub='')
# Count the number of total characters
char.1 = nchar(train.clean)
# Remove stopwords
train.clean = removeWords(train.clean, stopwords("english"))
# Remove extra spaces
train.clean = str_squish(train.clean)
# Count the number of 'relevant' characters, removed stopwords and whitespace
char.2 = nchar(train.clean)
# Count the number of digits
num = nchar(gsub("[^0-9]+", "", train.clean))
# Remove numbers
train.clean = gsub("[0-9]+","",train.clean)
# Count the number of upper case words
upper = ldply(str_match_all(train.clean,"[A-Z]"),length)[,1]
# Change all to lower case
train.clean = tolower(train.clean)
# Count the number of punctuation marks
punct = str_count(train.clean,"[:punct:]")
# Remove punctuation marks
train.clean = removePunctuation(train.clean)
# Stem words
train.clean = stemDocument(train.clean)

# Here are some more variables extracted from the text, bind them to the final
# dataframe
text_info = data.frame(char_1 = char.1,
                       char_2 = char.2,
                       char_prop = char.2/(char.1+1),
                       num = num,
                       num_prop = num/(char.1+1),
                       upper = upper,
                       upper_prop = upper/(char.1+1),
                       punct = punct,
                       punct_prop = punct/(char.1+1))

final = cbind(final, text_info)

############

# Here are just some interesting functions for seeing which words are popular
# between different GPA groups

# # Better. Bind this to final
# final = cbind(final, tfidf.95)
# 
# # Let's look at some keywords
# tail(sort(colSums(tfidf.95)),10)
# 
# # Groups
# tail(sort(colSums(tfidf.95[final$`Course GPA`>3.5,])),10)
# tail(sort(colSums(tfidf.95[final$`Course GPA`<2.5,])),10)

######################
# Sentiment Analysis #
######################

# Now we see the sentiment of the text

comments = final$comments
final$comments = NULL

library(syuzhet)
senti_train = get_nrc_sentiment(as.character(comments))
senti_train_prop = senti_train/rowSums(senti_train)

# https://stackoverflow.com/questions/18142117/how-to-replace-nan-value-with-zero-in-a-huge-data-frame/18143097
is.nan.data.frame <- function(x)
do.call(cbind, lapply(x, is.nan))

senti_train_prop[is.nan(senti_train_prop)] = 0

# Bind to final
final = cbind(final, senti_train_prop)

# Let's see the results
median_grade = median(final$`Course GPA`)

ind = final$`Course GPA` >= median_grade
high = sort(colMeans(final[ind, 63:72]))

ind = final$`Course GPA` < median_grade
low = sort(colMeans(final[ind, 63:72]))

# Last index is the most different
plot(abs(high-low))
names(low)[10]

#################
# Final Cleanup #
#################

# Do some final cleanup before writing the data

# Decide to use first and last name together, after all

final$name = paste(final$first, final$last)
final$first = NULL
final$last = NULL

final$name = as.factor(final$name)
final$dept = as.factor(final$dept)

# We have to make Professor names and Courses dummies as well for glmnet
library(dummies)
dum_name = dummy(final$name)
dum_dept = dummy(final$dept)
final_dummy = cbind(final, dum_name, dum_dept)
final_dummy$name = NULL
final_dummy$dept = NULL

# Since we probably won't have grade distributions drop those
final_dummy[,18:30] = NULL

##########################
# Training and Test Sets #
##########################

# Split the data into training and test sets

sample = floor(0.75*nrow(final_dummy))
set.seed(4.669)
index = sample(seq_len(nrow(final_dummy)), size = sample)
train = final_dummy[index,]
test = final_dummy[-index,]

#########
# TFIDF #
#########

# Get the training text
train_text = train.clean[index]

# Create a word tokenizer
it_train=itoken(train_text, tolower, tokenizer=word_tokenizer,ids=index)

# Create the vocabulary - up to 2 ngrams
vocab = create_vocabulary(it_train, stopwords=, ngram = c(1L, 2L))

# Prune vocabulary
pruned_vocab = prune_vocabulary(vocab, term_count_min=50, doc_proportion_max=0.40, doc_proportion_min=0.005)

# Vectorize vocabulary
vectorizer = vocab_vectorizer(pruned_vocab)

# Apply to the training data
dtm_train = create_dtm(it_train, vectorizer)

# Create a framework for TF-IDF
tfidf = TfIdf$new()

# Fit training data to this TF-IDF framework
dtm_train_tfidf = fit_transform(dtm_train, tfidf)

# Make to matrix
dim(dtm_train_tfidf) = c(length(train_text),length(dtm_train_tfidf)/length(train_text))

# Set names of column
colnames(dtm_train_tfidf) = pruned_vocab$term

# Repeat to test

test_text = train.clean[(1:nrow(final_dummy))[-index]]

# Create a word tokenizer
it_test = itoken(test_text, tolower, tokenizer=word_tokenizer,ids=(1:nrow(final_dummy))[-index])
# Apply to the testing data
dtm_test = create_dtm(it_test, vectorizer)
# Create a framework for TF-IDF
tfidf = TfIdf$new()
# Fit testing data to this TF-IDF framework
dtm_test_tfidf = fit_transform(dtm_test, tfidf)
# Make to matrix
dim(dtm_test_tfidf) = c(length(test_text),length(dtm_test_tfidf)/length(test_text))
# Set names of column
colnames(dtm_test_tfidf) = pruned_vocab$term

# Bind
train = cbind(train, as.matrix(dtm_train_tfidf))
test = cbind(test, as.matrix(dtm_test_tfidf))
dim(train)
dim(test)

##############
# Write Data #
##############

write.csv(train, "train.csv", row.names=FALSE)
write.csv(test, "test.csv", row.names=FALSE)

#############
# Read Data #
#############

train = fread("train.csv")
test = fread("test.csv")

###############################
# OLS model - Lasso and Ridge #
###############################

library(glmnet)
library(doParallel)
library(parallel)

registerDoParallel(detectCores())

X = train
X$`Course GPA` = NULL
X = data.matrix(X)
Y = data.matrix(train$`Course GPA`)
dim(X)
dim(Y)

library(e1071)
skewness(Y)

# Alpha == 1 indicates LASSO.
par(mfrow=c(1,2))
glm_model = cv.glmnet(x=as(X, "dgCMatrix"),y=Y,alpha=0, parallel=TRUE)
# Non parallel
# glm_model = cv.glmnet(x=as(X, "dgCMatrix"),y=Y,alpha=0)
plot(glm_model)
min(glm_model$cvm)
lam = glm_model$lambda.min

glm_model_2 = cv.glmnet(x=as(X, "dgCMatrix"),y=Y,alpha=1, parallel=TRUE)
# Non parallel
# glm_model_2 = cv.glmnet(x=as(X, "dgCMatrix"),y=Y,alpha=1)
plot(glm_model_2)
min(glm_model_2$cvm)
lam2 = glm_model_2$lambda.min

# Test set
X.test = test
X.test$`Course GPA` = NULL
X.test = data.matrix(X.test, rownames.force=FALSE)
Y.test = data.matrix(test$`Course GPA`)

# Predict
pred = predict(glm_model, newx=X.test, s=lam)
pred2 = predict(glm_model_2, newx=X.test, s=lam2)

mse = sum((Y.test-pred)^2)/nrow(pred)
mse2 = sum((Y.test-pred2)^2)/nrow(pred2)

par(mfrow=c(1,2))
plot(Y.test, pred, main="Ridge", xlab="Actual Grades", ylab="Predicted Grades")
text(x=1,y=3.4,paste("MSE =",mse))
abline(0,1)
plot(Y.test, pred2, main="Lasso", xlab="Actual Grades", ylab="Predicted Grades")
text(x=1,y=3.4,paste("MSE =",mse2))
abline(0,1)

# Get importance
sd = apply(X, 2, sd)
coeff = as.matrix(coef(glm_model, s=lam))
coeff_2 = as.matrix(coef(glm_model_2, s=lam2))
imps = coeff[-1,1]*sd
imps_2 = coeff_2[-1,1]*sd

# Get the most important features
head(sort(abs(imps), decreasing=TRUE), 20)
head(sort(abs(imps_2), decreasing=TRUE), 20)

##############
# Sentiments #
##############

# Let's take another look at sentiments, in particular the positive

# Plot
par(mfrow=c(1,1))
pos_median = median(train$positive)
hist(train$`Course GPA`[train$positive>=pos_median], breaks=20,main="Positive", col=rgb(1,0,0,.3), xlim=c(2,4), xlab="GPA")
hist(train$`Course GPA`[train$positive<pos_median], breaks=10,main="Negative", col=rgb(0,0,1,.3), add=TRUE)

neg_median = median(train$negative)
hist(train$`Course GPA`[train$negative>=neg_median], breaks=10,main="Negative", col=rgb(0,0,1,.3), xlim=c(2,4), xlab="GPA")
hist(train$`Course GPA`[train$negative<neg_median], breaks=20,main="Negative", col=rgb(1,0,0,.3), add=TRUE)

sd_positive = sd(train$positive)
mean_positive = mean(train$positive)

###########
# Outlier #
###########

# There was one outlier course(s) of interest.

final[final$`Course GPA`<1,]

# Chem 3910 and Chem 3920 by James Demas.
reviews$comments[(reviews$nums==3910 | reviews$nums==3920) & reviews$last=="Demas"]

##############################
# Most (and least) Satisfied #
##############################

temp = final[final$dept=="STAT" | final$dept =="CS" | final$dept == "SYS"]
temp = final
agg2 = aggregate(temp[,c("positive","negative",'Course GPA',"overallRate")], list(temp$name), mean)
counts = aggregate(temp[,c("positive")], list(temp$name), length)$positive
names(counts) = "counts"
agg2 = cbind(agg2, counts)
agg2 = agg2[agg2$counts>=10,]
agg2 = agg2[order(-agg2$positive),]

# Order by positive - negative
agg2=agg2[order(-(agg2$positive)),]

# Plot positive sentiments with overall rating
plot(agg2$positive, agg2$overallRate)

                           
####################################################
# Predict GPA based on survey of course evaluation #
####################################################
                           
# Read in data from collected surveys                           
survey = fread("survey.csv")
survey = survey[,-c(1, 10)]
names(survey) = c('comments','clarityRate', 'easyRate', 'helpfulRate', 'overallRate', 'quality', 'interest', 'teacherTags')

# Check if there is any missing value
sum(is.na(survey))   # No missing value
# 0

# Turn clarityRate, easyRate, helpfulRate and quality into numeric variables
cols = c("clarityRate", "easyRate","helpfulRate", "quality")
dict = setNames(c(1,2,3,4,5),c("Awful","Poor","Average","Good","Awesome"))
# Iterate through these and use the dictionary to set their values
for (col in cols) {
  temp = survey[,col,with=FALSE]
  temp[] = dict[unlist(temp)]
  survey[[col]] = temp[[1]]
}

# Calculate clarityColor, easyColor, helpColor
# If "Rate" equals to 4 or 5, then "Color" = 3
# If "Rate" equals to 3, then "Color" = 2
#"Rate" equals to 1, then "Color" = 1
survey$clarityColor = ifelse(survey$clarityRate>3, 3, 2)
survey$clarityColor[survey$clarityRate == 1] = 1
survey$easyColor = ifelse(survey$easyRate>3, 3, 2)
survey$easyColor[survey$easyRate == 1] = 1
survey$helpColor = ifelse(survey$helpfulRate>3, 3, 2)
survey$helpColor[survey$helpfulRate == 1] = 1

# Not sure what is helpCount, nothelpCount and status, just set them to the median of training data
survey$helpCount = median(final_dummy$helpCount)
survey$nothelpCount = median(final_dummy$nothelpCount)
survey$status = median(final_dummy$status)

survey$textBookUse = TRUE

dict = setNames(c(1,2,3,4,5),c("Low","Meh","Sorta Interested","Really into it","It's my life"))
temp = survey[,"interest",with=FALSE]
temp[] = dict[unlist(temp)]
survey$interest_val = survey$interest
survey$interest = NULL
survey[["interest_val"]] = temp[[1]]

survey$averageScore = mean(survey$overallRate)
survey$numOfRating = 17    #  17 17

# get all the tags and turn them into dummy varaibles
survey$teacherTags = unlist(lapply(survey$teacherTags, function(x) gsub("\\[|\\]|'","",x)))
survey$teacherTags = lapply(survey$teacherTags, function(x) as.list(strsplit(x,", ")[[1]]))
dummy = mtabulate(survey$teacherTags)  # 17, 16
survey$teacherTags = NULL
survey_final = cbind(survey, dummy)  # 17 32
colnames(survey_final)[30] ="\"\"Skip class? You wont pass.\"\""

# Text
# Do some preliminary text analysis and cleaning
train.clean = survey$comments
# Trim whitespace
train.clean = trimws(train.clean)
# Convert ascii characters (will remove most)
train.clean = iconv(train.clean, to='ASCII', sub='')
# Count the number of total characters
char.1 = nchar(train.clean)
# Remove stopwords
train.clean = removeWords(train.clean, stopwords("english"))
# Remove extra spaces
train.clean = str_squish(train.clean)
# Count the number of 'relevant' characters, removed stopwords and whitespace
char.2 = nchar(train.clean)
# Count the number of digits
num = nchar(gsub("[^0-9]+", "", train.clean))
# Remove numbers
train.clean = gsub("[0-9]+","",train.clean)
# Count the number of upper case words
upper = ldply(str_match_all(train.clean,"[A-Z]"),length)[,1]
# Change all to lower case
train.clean = tolower(train.clean)
# Count the number of punctuation marks
punct = str_count(train.clean,"[:punct:]")
# Remove punctuation marks
train.clean = removePunctuation(train.clean)
# Stem words
train.clean = stemDocument(train.clean)

# Here are some more variables extracted from the text, bind them to the final
# dataframe
text_info = data.frame(char_1 = char.1,
                       char_2 = char.2,
                       char_prop = char.2/(char.1+1),
                       num = num,
                       num_prop = num/(char.1+1),
                       upper = upper,
                       upper_prop = upper/(char.1+1),
                       punct = punct,
                       punct_prop = punct/(char.1+1))

survey_final = cbind(survey_final, text_info)  # 17 41

# Sentiment analysis
# Now we see the sentiment of the text
survey_comments = survey_final$comments
survey_final$comments = NULL

senti_train = get_nrc_sentiment(as.character(survey_comments))
senti_train_prop = senti_train/rowSums(senti_train)

# https://stackoverflow.com/questions/18142117/how-to-replace-nan-value-with-zero-in-a-huge-data-frame/18143097
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

senti_train_prop[is.nan(senti_train_prop)] = 0

# Bind to final
survey_final = cbind(survey_final, senti_train_prop)

# tfidf
survey_text = train.clean

# Create a word tokenizer
it_survey = itoken(survey_text, tolower, tokenizer=word_tokenizer,ids=sequence(nrow(survey)))
# Apply to the survey data
dtm_survey = create_dtm(it_survey, vectorizer)
# Create a framework for TF-IDF
tfidf = TfIdf$new()
# Fit survey data to this TF-IDF framework
dtm_survey_tfidf = fit_transform(dtm_survey, tfidf)
# Make to matrix
dim(dtm_survey_tfidf) = c(length(survey_text),length(dtm_survey_tfidf)/length(survey_text))
# Set names of column
colnames(dtm_survey_tfidf) = pruned_vocab$term

# Keywords
# tail(sort(colSums(dtm_survey_tfidf)),10)

# Output
# write.csv(as.matrix(dtm_survey_tfidf), "survey_tfidf.csv", row.names = FALSE)

# Bind
survey_final = cbind(survey_final, as.matrix(dtm_survey_tfidf))
dim(survey_final)  #  17 450

survey_final$deptSYS = 1  
survey_final$nameMatthewGerber = 1  
survey_final$nums = 6018 # 17 453

# Add the new variables from survey to the training set
trainNew = train
testNew = test
trainNew$nameMatthewGerber = 0 
testNew$nameMatthewGerber = 0
dim(trainNew) # 4095 1405
dim(testNew)  # 1365 1405
write.csv(trainNew, "train_new.csv", row.names=FALSE)
write.csv(testNew, "test_new.csv", row.names=FALSE)

# Combine with the original data
# If there is missing value in survey data (Ex:dummy variables of professor names), fill in with 0
all = rbind(trainNew, survey_final, fill=TRUE)  # 4112 1405
all[is.na(all)] = 0
survey_cleaned = all[4096:4112,] #  17 1405

write.csv(survey_cleaned, "survey_cleaned.csv", row.names=FALSE)

##############
# Prediction #
##############
# Read in data
survey_cleaned = fread("survey_cleaned.csv")  # 17 1405
trainNew = fread("train_new.csv")
testNew = fread("test_new.csv")

allNew = rbind(trainNew, testNew)
X = allNew
X$`Course GPA` = NULL
X = data.matrix(X)
Y = data.matrix(allNew$`Course GPA`)
dim(X) # 5460 1404
dim(Y) # 5460    1

# Alpha == 1 indicates LASSO.
par(mfrow=c(1,2))
glm_model = cv.glmnet(x=as(X, "dgCMatrix"),y=Y,alpha=0)
plot(glm_model)
min(glm_model$cvm)     # 0.01579014
lam = glm_model$lambda.min

glm_model_2 = cv.glmnet(x=as(X, "dgCMatrix"),y=Y,alpha=1)
plot(glm_model_2)
min(glm_model_2$cvm)   # 0.01470104
lam2 = glm_model_2$lambda.min

# Test on survey data
X.test = survey_cleaned
X.test$`Course GPA` = NULL
X.test = data.matrix(X.test, rownames.force=FALSE)

# Predict
pred = predict(glm_model, newx=X.test, s=lam)
pred2 = predict(glm_model_2, newx=X.test, s=lam2)
mean(pred)  # 3.51096
mean(pred2) # 3.569815

hist(pred, main="GPA predicted from Ridge regression", xlab="GPA")
hist(pred2, main="GPA predicted from Lasso regression", xlab="GPA")

##########
# Output #
##########
survey = fread("survey.csv")
survey$ridge = pred
survey$lasso = pred2
write.csv(survey, "survey_predictions.csv", row.names = FALSE)
