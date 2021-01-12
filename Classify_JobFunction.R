#Load required packages
library(tm) #tm or the text-mining package helps reformat the dataframe to clean up job titles 
library(SnowballC) #This package breaks down job titles to just stem words
library(class) #class contains the nearest neighbor algorithm
library(dplyr) #Data manipulation
library(reshape2)#Convert data from long to wide format to create confusion matrix

#Load data
data = read_csv('JF_JL_Automation/data/data.csv')

#Clean data
data <- data.frame(data)

#Create a corpus (series of docs) with the column of job titles. In this instance every title is a document
docs <- Corpus(VectorSource(data$job_title))

docs <- tm_map(docs, content_transformer(tolower))#Makes them all lowercase
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)#If there are multiple spaces between characters, they are collapsed to a single space
docs <- tm_map(docs, removeWords, stopwords("english"))#Removes words that aren't in the english language
docs <- tm_map(docs, stemDocument, language = "english")#Stems the words (e.g., "Manager" turns into "manag")

#Create a document term matrix. 
#This is a table where there is a column for each word that exists among all job titles
#Each row corresponds to a title and a 1 will appear in every column of words that are found in the title
dtm <- DocumentTermMatrix(docs)

#Turn the matrix into a dataframe
#mat.df has 427 columns (427 unique words contained in all the titles)
mat_df <- as.data.frame(data.matrix(dtm), stringsAsfactors = FALSE)

#Rebind the function
mat_df <- cbind(mat_df, data$job_function)
colnames(mat_df)[ncol(mat_df)] <- "job_function"

#Isolate the target class (job function)
y <- mat_df[,"job_function"]

#Isolate the features (words form titles)
X <- mat_df[,!colnames(mat_df) %in% "job_function"]

#Create a test and train group for the model (make 70% of data for training)
X_train = X[1:700,]
X_test = X[701:1000,]

y_train = y[1:700]
y_test = y[701:1000]

#Loop through several k values to determine which one offers the greatest accuracy
accuracy = c()

for(i in 1:20){
  #Run the model
  pred <- knn(X_train, X_test, y_train, k=i)
  
  #Create confusion matrix
  cm = as.matrix(table(Actual = y_test, Predicted = pred))
  
  #Calculate accuracy and append to list
  accuracy <- append(accuracy, sum(diag(cm))/length(y_test))
}

#Plot accuracy against k values 1 through 20
plot(1:20, accuracy, type = "l", lty = 1)

#Accuracy is highest (77%) when k=1 and really drops off when k>14
#If the contacts in this dataset had very similar titles, increasing k would likely imporve accuracy
#However, there were 427 unique words across the 1000 job titles (lots of variation)
#Here, the best way to improve accuracy, would be by increasing the size of the training data

#Run the model with k=1 and return probability
pred <- knn(X_train, X_test, y_train, k=1, prob = TRUE)

#Create data  frame with title, actual job function, predicted job function, and probability
ind_pred <- cbind(data[701:1000,'job_title'],y_test,pred,data.frame(attr(pred,"prob")))
colnames(ind_pred) <- c("job_title","actual","predicted","probability")

# Probability represents the proportion of the votes for the winning job function
# If probability is not 1, that means there were ties

