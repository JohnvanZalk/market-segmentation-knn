#!/usr/bin/Rscript

##Using crontab
#To edit or create new schedule, type crontab -e in Terminal
#Press i to enter insert mode
#MIN HR DAY MON DOY CMD
#Time is in UTC! (UTC is 4 hours later than EDT)
#Insert /usr/bin/Rscript at top of script or in cron job line
#Press CTRL+C to exit insert mode
#Save and quit by holding :wq
#Make R script executable with chmod +x /home/user/Location/Of/Script 

#Load required packages
library(tm) # tm or the text-mining package helps reformat the dataframe to clean up job titles 
library(SnowballC) #This package breaks down job titles to just stem words
library(class) # class contains the nearest neighbor algorithm
library(dplyr) #data manipulation
library(RForcecom) # Like simple_salesforce in Python, this is a data integration with Salesforce

#Authenticate to Salesforce
username <- "jvanzalk@sepapower.org"
password <- "SEPA2019WIfWAglPLQZYyyrAcQ6klSgu"
loginURL <- "https://login.salesforce.com/"
apiVersion <- "46.0"
session <- rforcecom.login(username, password, loginURL, apiVersion)

##Pull all leads and contacts with titles (job function prediction based on title)

#Pull all contacts with job titles
ContactQuery <- "SELECT Contact_18_Digit_ID__c,Title,	Account_Organization_Type__c, Job_Function_2__c FROM Contact WHERE Title!=''"
AllContacts <-rforcecom.query(session, ContactQuery)
#Create new column with lead ids to rbind leads and contacts
AllContacts$Lead_18_Digit_ID__c <-NA
AllContacts<-AllContacts[,c(1,5,2,3,4)]
names(AllContacts)<-c("Contact_ID","Lead_ID", "Title", "Org_Type","Function")

#Pull all leads with job titles that have not been converted to contacts
LeadQuery <- "SELECT Lead_18_Digit_ID__c,Title,Org_Type__c, Job_Function_2__c FROM Lead WHERE Title!=''AND IsConverted=FALSE"
AllLeads <-rforcecom.query(session, LeadQuery)
#Reformat columns to match AllContacts dataframe
AllLeads$Contact_18_Digit_ID__c <-NA
AllLeads<-AllLeads[,c(5,1,2,3,4)]
names(AllLeads)<-c("Contact_ID","Lead_ID", "Title", "Org_Type","Function")

#Combine leads and contacts
df<-rbind(AllContacts,AllLeads)

#Subset dataframe to return leads/contacts in utility or corporate organizations
#Individuals from government, non-profit & educational organizations are not included in this analysis 
selected<-c("Utility","Corporate")
df2<-df[df$Org_Type %in% selected,]

## Create a test and train group for the model

#Create another dataframe with contacts/leads that do not have job functions (test group)
test<-subset(df2,is.na(Function))
#Mark blanks job functions as unassigned (must have some text in there)
test$Function  <- "Unassigned"

#Create a datafram without existing functions to use for training
train<-df2[df2$Function !="",]

##Initial assignment where titles contain keywords
#Many job functions can be identifed based on keywords in titles before relying on computer prediction


#Utility
#if there are indivduals from utilities wihtout job functions, proceed with functions below
if(sum(test$Org_Type == "Utility")>0){
  
  Utility <- subset(test, test$Org_Type == "Utility")
  
  #Transmission Planning & Operations job function keywords
  #^=start, $=end
  transmission <- c("transmission","wholesale markets","power marketing", "power utilization",
                    "grid integration", "siting","synchrophasor"," ISO ","market operations",
                    "congestion")
  #The computer is searching the column of job titles for the above keywords
  u1<-grepl(paste(transmission, collapse = "|"), Utility$Title, ignore.case = TRUE)
  #If any of the keywords are found in the job titles, assign that contact/lead to the Transmission job function
  Utility$Function[u1] <- 'Transmission Planning & Operations'
  #Create a dataframe (table1) with newly assigned job functions and remove them from the unassigned dataframe
  table1<-Utility[Utility$Function=='Transmission Planning & Operations',]
  Utility<-Utility[Utility$Function=='Unassigned',]
  
  #Generation keywords
  generation <- c("supply","generation","production", "plant", "site operations",
                  "asset","fuel","generating", "station","system maintenance", "turbine")
  u2<-grepl(paste(generation, collapse = "|"), Utility$Title, ignore.case = TRUE)
  Utility$Function[u2] <- 'Generation'
  table2<-Utility[Utility$Function=='Generation',]
  Utility<-Utility[Utility$Function=='Unassigned',]
  
  #Resource planning keywords
  resource <- c("resource planner","resource analyst", "integrated resource","integrated planning", 
                "resource planning", "power resources","forecasting","resource acquisition",
                "district planning", "portfolio","IRP", "planning analyst","supply side planner",
                "system planner","procurment","contract","resource manager")
  u3<-grepl(paste(resource, collapse = "|"), Utility$Title, ignore.case = TRUE)
  Utility$Function[u3] <- 'Resource Planning'
  table3<-Utility[Utility$Function=='Resource Planning',]
  Utility<-Utility[Utility$Function=='Unassigned',]
  
  #Distribution Planning & Operations keywords
  distribution <- c("distribution","delivery","system", "^DER$"," DER ","DER ","meter", "storage", "battery", "interconnection",
                    "distributed", "project engineer","line engineer", "integration engineer",
                    "system modeling", "power qualtiy analysis","system analytics",
                    "energy delivery", "system protection","substation design",
                    "substation engineer", "power systems engineer","outage manager", 
                    "protection control","smart grid","^engineer$","R&D")
  u4<-grepl(paste(distribution, collapse = "|"), Utility$Title, ignore.case = TRUE)
  Utility$Function[u4] <- 'Distribution Planning & Operations'
  table4<-Utility[Utility$Function=='Distribution Planning & Operations',]
  Utility<-Utility[Utility$Function=='Unassigned',]
  
  #Customer service keywords
  customer <- c("key account","business account","commercial services", 
                "development services", "commercial","industrial","account manager",
                "energy auditor", "energy specialist","energy management", "customer innovation",
                "customer connections","electrical services","corporate support","major accounts",
                "account executive","business services","energy advisor",
                "energy services manager","utility services","member services",
                "customer services","customer relations","account manager",
                "customer representative", "customer support","customer care",
                "customer operations","customer solutions","billing",
                "customer expirience","member relations","customer service", "account rep")
  u5<-grepl(paste(customer, collapse = "|"), Utility$Title, ignore.case = TRUE)
  Utility$Function[u5] <- 'Customer Service'
  table5<-Utility[Utility$Function=='Customer Service',]
  Utility<-Utility[Utility$Function=='Unassigned',]
  
  #Marketing keywords
  marketing <- c("communications","corporate relations","marketing", "corporate development",
                 "community affairs", "public relations","engagement","economic development")
  u6<-grepl(paste(marketing, collapse = "|"), Utility$Title, ignore.case = TRUE)
  Utility$Function[u6] <- 'Marketing'
  table6<-Utility[Utility$Function=='Marketing',]
  Utility<-Utility[Utility$Function=='Unassigned',]
  
  #Program Management keywords
  programs <- c("vehicle","transportation","electrification", "renewable", "energy manager",
                "innovation","grid modernization","demand response","efficiency",
                "conservation","sustain","markets and innovation","customer programs","solar",
                "product", "program","dsm","altnerative energy","advanced technology",
                "emerging tech", "tech","project manage")
  u7<-grepl(paste(programs, collapse = "|"), Utility$Title, ignore.case = TRUE)
  Utility$Function[u7] <- 'Program Management'
  table7<-Utility[Utility$Function=='Program Management',]
  Utility<-Utility[Utility$Function=='Unassigned',]
  
  #Government & Regulatory Affairs keywords
  regulatory <- c("regulation","regulatory","government", "public affairs", "environment",
                  "FERC","NERC","compliance","policy","rate","pricing","general counsel",
                  "attorney","commissioner","compliance","govt"," reg ","reg.",
                  "revenue requirements","federal")
  u8<-grepl(paste(regulatory, collapse = "|"), Utility$Title, ignore.case = TRUE)
  Utility$Function[u8] <- 'Government & Regulatory Affairs'
  table8<-Utility[Utility$Function=='Government & Regulatory Affairs',]
  Utility<-Utility[Utility$Function=='Unassigned',]
  
  #Leadership and Organizational Planning keywords
  leadership <- c("^CEO$","^CIO$","^COO$", "^CTO$", "^CFO$","^CSO$","President","Chief","General Manager",
                  "Managing Director","SVP","EVP","Lead Innovator","board","borough manager",
                  "chairman","city administrator","council member","GM","Mayor","Town Manager",
                  "village administrator","strategic planning","strategic initiatives",
                  "strategy &","strategy analyst","corporate strategy","business strategy",
                  "economist","risk","business development","superintendent","electric director",
                  "utility director","utilities director","investor","city manager","village manager")
  u9<-grepl(paste(leadership, collapse = "|"), Utility$Title, ignore.case = TRUE)
  Utility$Function[u9] <- 'Leadership and Organizational Planning'
  table9<-Utility[Utility$Function=='Leadership and Organizational Planning',]
  Utility<-Utility[Utility$Function=='Unassigned',]
  
  #General Management
  management <- c("accountant","accounting","admin$", "administrative", "controller","auditor",
                  "secretary","clerk","consultant","executive assistant","financ","human resources",
                  "information technology"," IT "," IT$","^IT ", "Information security","intern",
                  "of operations","office manager","operations manager","business")
  u10<-grepl(paste(management, collapse = "|"), Utility$Title, ignore.case = TRUE)
  Utility$Function[u10] <- 'General Management'
  table10<-Utility[Utility$Function=='General Management',]
  Utility<-Utility[Utility$Function=='Unassigned',]
  
  #Unidentified keywords
  unidentified <- c("Mr.","^Mr$","Mrs$", "Mrs.","^Ms$","^Miss","^na$","^member$","^self&",
                    "Unknown","Test","N/A","employee","guest","none","visitor","person$",
                    "^associate$","^assistant$","^manager$","^director$","^specialist$",
                    "^professional$","^principal$","^analyst$","senior manager$", "sr. manager$")
  u11<-grepl(paste(unidentified, collapse = "|"), Utility$Title, ignore.case = TRUE)
  Utility$Function[u11] <- 'Unidentified'
  table11<-Utility[Utility$Function=='Unidentified',]
  Utility<-Utility[Utility$Function=='Unassigned',]
  
  #Combine all individuals with newly assigned job functions into a single dataframe
  assigned_u<-rbind(table1,table2,table3,table4,table5,table6,table7,table8,table9,table10,table11)
  
  #Create a dataframe with unassigned indivduals
  test_u<-Utility
  
  #If contacts/leads remain unassigned, use knn to predict job functions
  if(nrow(test_u) > 0){
    
    #Take random sample of train (train size must be greater than or equal to test)
    #No class should be greater than 10% in size of another class
    #If number of test rows is less than 2000, make train 2000 (take 200 from each job function)
    samplesize_u <-nrow(test_u)
    samplesize_u[samplesize_u<2000] <- 2000
    
    #Divide sample by 10 (10 job functions) to get the same number of samples per function
    samplesize_u = samplesize_u / 10
    samplesize_u <-round(samplesize_u)
    samplesize_u <-samplesize_u+1
    
    #Create a training dataset with only utility contacts/leads
    train_u<- train[train$Org_Type =='Utility',]
    
    corporate_only_functions<-c("Operations & Maintenance" ,"Product Development and Management",
                                "Engineering","Business Development")
    
    #If utility contacts/leads are assigned corporate job functions, change the functions to blank and add them to test
    if(nrow(train_u[train_u$Function %in% corporate_only_functions, ])>0){
      error_u<-train_u[train_u$Function %in% corporate_only_functions, ]
      error_u$Function <- NA
      test_u<-rbind(test_u,error_u)
    }
    
    #Remove "Unidentified" job titles (individuals that went through process already, but could not be assigned)
    utility_functions<-c("Transmission Planning & Operations" ,"Program Management","Generation",
                         "Leadership and Organizational Planning","Customer Service",
                         "Resource Planning","Marketing","Government & Regulatory Affairs",
                         "General Management","Distribution Planning & Operations")
    train_u<-train_u[train_u$Function %in% utility_functions,]
    
    #Calculate the number of contacts/leads per job function
    norows_u<-  train_u %>%
      group_by(Function) %>%
      summarise(no_rows = length(Function))
    
    #Take the minimum group size 
    mingroupsize_u<-min(norows_u[,2])
    
    #If the minumum group size is less than the required sample size, generate additional samples
    if(mingroupsize_u < samplesize_u){
      
      #Take at least that minimum number of observations from each group
      train_a_u<-train_u %>% group_by(Function) %>% sample_n(mingroupsize_u)
      
      #Calculate the remainder of observations needed to meet the sample size 
      remainder_u <- (samplesize_u * 10) - nrow(train_a_u)
      
      #Take a random sample from the leftover train pile to meet the sample size
      train_b_u <- sample_n(train_u, remainder_u)
      
      #Bind train 1 and 2 to meet the sample size
      train_u <-rbind.data.frame(train_a_u,train_b_u)
      
    }  
    
    if(mingroupsize_u > samplesize_u){
      train_u<-train_u %>% group_by(Function) %>% sample_n(samplesize_u)
    }
    
    #Convert to dataframe
    train_u<-data.frame(train_u)
    test_u<-data.frame(test_u)
    
    #Test and train dataframes are combined for cleaning so a count column will allow them to be seperated later on
    train_u$count<- rep(1,nrow(train_u))
    test_u$count<- rep(2,nrow(test_u))
    
    #Bind dfs together for cleaning
    df_u <- rbind(train_u,test_u)
    
    #Create a Corpus (collection of text from titles)
    docs_u <- Corpus(VectorSource(df_u$Title))
    
    docs_u <- tm_map(docs_u, content_transformer(tolower))#make all titles lowercase
    docs_u <- tm_map(docs_u, removeNumbers) 
    docs_u <- tm_map(docs_u, removePunctuation)
    docs_u <- tm_map(docs_u, stripWhitespace)#If there are multiple spaces between characters, collapse them to a single space
    docs_u <- tm_map(docs_u, removeWords, stopwords("english"))#removes words that aren't in the english language
    docs_u <- tm_map(docs_u, stemDocument, language = "english") #stems the words e.g. "Manager" turns into "manag"
    
    #Create a dataframe where column headers are unique words from titles and rows are counts of those words in each individuals title
    dtm_u <- DocumentTermMatrix(docs_u)
    mat.df_u <- as.data.frame(data.matrix(dtm_u), stringsAsfactors = FALSE)
    
    #Bind job functions to new dataframe
    mat.df_u <- cbind(mat.df_u, df_u$Function)
    colnames(mat.df_u)[ncol(mat.df_u)] <- "Function"
    
    #Bind the count
    mat.df_u <- cbind(mat.df_u, df_u$count)
    colnames(mat.df_u)[ncol(mat.df_u)] <- "count"
    
    #Split data by whether they have a job function or not
    train_u1 <- which(mat.df_u$count == 1)
    test_u1 <- which(mat.df_u$count == 2)
    
    #Isolate the classifier (in this case job function)
    cl_u <- mat.df_u[, "Function"]
    
    #Convert list to vector
    test_u1<-unlist(test_u1, use.names=FALSE)
    train_u1<-unlist(train_u1, use.names=FALSE)
    
    #Create model data and remove job function and count
    #Model data is combination of both test and train data
    modeldata_u <- mat.df_u[,!colnames(mat.df_u) %in% "Function"]
    modeldata_u <- modeldata_u[,!colnames(modeldata_u) %in% "count"]
    
    #Run the model
    #k=3 is chosen after testing
    #prob=TURE will return the % of nearest neighbors that were from the winning job Function class
    knn.pred_u <- knn(modeldata_u[train_u1, ], modeldata_u[test_u1, ], cl_u[train_u1], k=3,prob = TRUE)
    
    #Confusion matrix displays how many contacts/leads were assigned to each job function 
    conf.mat_u <- table("Predictions" = knn.pred_u, Actual = cl_u[test_u1])
    conf.mat_u
    
    #Create data frame with test data and predicted category
    df.pred_u <- cbind(knn.pred_u, modeldata_u[test_u1, ])
    
    #Return just the new job functions and attach them to the origional dataframe with IDs
    df.pred_u <- df.pred_u[ c(1) ]
    final_u <- cbind(test_u,df.pred_u)
    #Attach the probabilities
    prob_u<-data.frame(attr(knn.pred_u,"prob"))
    final_u <- cbind(final_u, prob_u)
    
    #Remove unecessary columns
    final_u <- final_u[ -c(4,5,6) ]
    
    #Rename probability column
    colnames(final_u)[5] <- "knn.prob_u"
    
    #Mark contacts/leads with less than 50% majority as "Unidentified" (all three neighbors had different fuctions)
    final_u$knn.pred_u <- as.character(final_u$knn.pred_u)
    final_u$knn.pred_u[final_u$knn.prob_u < .50] <- "Unidentified"
    
    #Remove probability
    final_u <- final_u[ -c(5) ]
    
    #Rename columns to match the dataframe assigned_u
    names(final_u)<-c("Contact_ID","Lead_ID","Title", "Function")
    assigned_u <- assigned_u[-c(4)]
    
    #Combine functions identified in intial cleanse with those identified from knn
    final_u <-rbind(final_u,assigned_u)
    
  }
  
  #If no contacts leads requried knn, label assigned as final
  if(nrow(test_u) == 0){final_u <- assigned_u}
  
  #Log assignment results in the following format: Utility Job Function Log YYYY-MM-DD.csv
  Date<-Sys.Date()
  path_out = '/home/rstudio/JF_JL_Automation/Job Function Log/'
  write.csv(final_u,paste(path_out,'Utility Job Level Log ',Date,'.csv',sep = ''))
  
  final_u<- final_u[-c(3)]
}

##Repeat steps for individuals at corporate organzations

if(sum(test$Org_Type == "Corporate")>0){
  
  Corporate <- subset(test, test$Org_Type == "Corporate")
  
  #Business Development keywords
  bizdev <- c("marketing","business development","sales","regional","lead","outreach","development",
              "relationship","account executive", "business analyst", "account manager","accounts", 
              "BD", "client", "customer", "economist", "key account", "origination",
              "market research","communication","advisor","growth")
  c1<-grepl(paste(bizdev, collapse = "|"), Corporate$Title, ignore.case = TRUE)
  Corporate$Function[c1] <- 'Business Development'
  table12<-Corporate[Corporate$Function=='Business Development',]
  Corporate<-Corporate[Corporate$Function=='Unassigned',]
  
  #Engineering keywords
  engineering <- c("engineer", "construction", "Eng.", "Engr$",
                   "interconnection", "PE", "project")
  c2<-grepl(paste(engineering, collapse = "|"), Corporate$Title, ignore.case = TRUE)
  Corporate$Function[c2] <- 'Engineering'
  table13<-Corporate[Corporate$Function=='Engineering',]
  Corporate<-Corporate[Corporate$Function=='Unassigned',]
  
  #Operations & maintenance keywords
  operations <- c("field supervisor", "operations manager", "technical manager",
                  "applications engineer", "field engineer","field operator", "asset", "technician",
                  "field services", "generation", "plant",  "transmission", "O&M", "portfolio",
                  "procurement", "asset management")
  c3<-grepl(paste(operations, collapse = "|"), Corporate$Title, ignore.case = TRUE)
  Corporate$Function[c3] <- 'Operations & Maintenance'
  table14<-Corporate[Corporate$Function=='Operations & Maintenance',]
  Corporate<-Corporate[Corporate$Function=='Unassigned',]
  
  #Product Development and Management keywords
  product <- c("product","innovation", "channel manager","line manager", "design", 
               "market", "feedback", "R&D", "PM", "program", "software engineer",
               "software developer", "software architect","renewable","solar","battery",
               "storage","microgrid","smart grid","alternative","emerging","new","research",
               "utility","utilities","technology","solutions architect","vehicle",
               "technical manager","technical director")
  c4<-grepl(paste(product, collapse = "|"), Corporate$Title, ignore.case = TRUE)
  Corporate$Function[c4] <- 'Product Development and Management'
  table15<-Corporate[Corporate$Function=='Product Development and Management',]
  Corporate<-Corporate[Corporate$Function=='Unassigned',]
  
  #Government & Regulatory Affairs keywords
  govt <- c("govt"," reg ","reg.","policy", "government", "state", "affairs",
            "regulation", "regulatory", "counsel","attorney", "lawyer", "legal" ,
            "paralegal", "compliance","legislative")
  c5<-grepl(paste(govt, collapse = "|"), Corporate$Title, ignore.case = TRUE)
  Corporate$Function[c5] <- 'Government & Regulatory Affairs'
  table16<-Corporate[Corporate$Function=='Government & Regulatory Affairs',]
  Corporate<-Corporate[Corporate$Function=='Unassigned',]
  
  #Leadership and Organizational Planning keywords
  leader <- c("^CEO$","^CIO$","^COO$", "^CTO$", "^CFO$","^CSO$","President","Chief",
              "Managing Director","SVP","EVP","board","founder", "owner", "partner",
              "chairman","strategic","strategy","economist","risk",
              "executive director","^cmo$","^cco$","america")
  c6<-grepl(paste(leader, collapse = "|"), Corporate$Title, ignore.case = TRUE)
  Corporate$Function[c6] <- 'Leadership and Organizational Planning'
  table17<-Corporate[Corporate$Function=='Leadership and Organizational Planning',]
  Corporate<-Corporate[Corporate$Function=='Unassigned',]
  
  #General Management
  gm <- c("accountant","accounting","admin$", "administrative", "controller","auditor",
          "secretary","clerk","consultant","executive assistant","financ","human resources",
          "information technology"," IT "," IT$","^IT ", "Information security",
          "intern","of operations","office manager","operations manager","business",
          "assistant to", "communictions", "general manager", "GM", "information technology",
          "public relations", "publisher", "reporter", "tax")
  c7<-grepl(paste(gm, collapse = "|"), Corporate$Title, ignore.case = TRUE)
  Corporate$Function[c7] <- 'General Management'
  table18<-Corporate[Corporate$Function=='General Management',]
  Corporate<-Corporate[Corporate$Function=='Unassigned',]
  
  #Unidentified keywords
  unidentified <- c("Mr.","^Mr$","Mrs$", "Mrs.","^Ms$","^Miss","^na$","^member$","^self&",
                    "Unknown","Test","N/A","employee","guest","none","visitor","person$",
                    "^associate$","^assistant$","^manager$","^director$","^specialist$",
                    "^professional$","^principal$","^analyst$","senior manager$", 
                    "sr. manager$","editor","Fellow","MBA","^senior analyst$","^VP$",
                    "student","professor","installer")
  c8<-grepl(paste(unidentified, collapse = "|"), Corporate$Title, ignore.case = TRUE)
  Corporate$Function[c8] <- 'Unidentified'
  table19<-Corporate[Corporate$Function=='Unidentified',]
  Corporate<-Corporate[Corporate$Function=='Unassigned',]
  
  assigned_c<-rbind(table12,table13,table14,table15,table16,table17,table18,table19)
  
  #If contacts/leads remain unassigned, use knn to predict job functions
  test_c<-Corporate
  if(nrow(test_c) > 0){
    
    #take random sample of train (train must have more  or equal to than test)
    #max 10% difference in size of each class
    #If number of test rows is less than 2000, make train 2000 (take 200 form each job function)
    samplesize_c <-nrow(test_c)
    samplesize_c[samplesize_c<2000] <- 2000
    samplesize_c = samplesize_c / 7
    samplesize_c <-round(samplesize_c)
    samplesize_c <-samplesize_c+1
    
    #eventually going to bring in from the beginning
    train_c<- train[train$Org_Type =='Corporate',]
    
    utility_only_functions<-c("Transmission Planning & Operations" ,"Program Management",
                              "Generation","Customer Service","Resource Planning",
                              "Marketing","Distribution Planning & Operations")
    
    #If corporate contacts/leads are assigned utility job functions, change the functions to blank and add them to test 
    if(nrow(train_c[train_c$Function %in% utility_only_functions,])>0){
      error_c<-train_c[train_c$Function %in% utility_only_functions,]
      error_c$Function <- NA
      test_c<-rbind(test_c,error_c)
    }
    
    #Remove "Unidentified" job titles
    corporate_functions<-c("Operations & Maintenance" ,"Product Development and Management",
                           "Engineering","Business Development",
                           "Leadership and Organizational Planning",
                           "Government & Regulatory Affairs","General Management")
    train_c<-train_c[train_c$Function %in% corporate_functions,]
    
    #calculate the number of contacts/leads per job function
    norows_c<-  train_c %>%
      group_by(Function) %>%
      summarise(no_rows = length(Function))
    
    #take the minimum group size 
    mingroupsize_c<-min(norows_c[,2])
    
    if(mingroupsize_c < samplesize_c){
      
      #take at least that minimum number of observations from each group
      train_a_c<-train_c %>% group_by(Function) %>% sample_n(mingroupsize_c)
      
      #calculate the remainder of observations needed to meet the sample size
      remainder_c <- (samplesize_c * 10) - nrow(train_a_c)
      
      #take a random sample from the leftover train pile to meet the sample size
      train_b_c <- sample_n(train_c, remainder_c)
      
      #bind train 1 and 2 to meet the sample size
      train_c <-rbind.data.frame(train_a_c,train_b_c)
      
    }  
    
    if(mingroupsize_c > samplesize_c){
      train_c<-train_c %>% group_by(Function) %>% sample_n(samplesize_c)
    }
    
    train_c<-data.frame(train_c)
    test_c<-data.frame(test_c)
    
    #We will combine the dataframes for cleaning so this count will allow us to seperate them later on
    train_c$count<- rep(1,nrow(train_c))
    test_c$count<- rep(2,nrow(test_c))
    
    #merge dfs together for cleaning
    df_c <- rbind(train_c,test_c)
    
    docs_c <- Corpus(VectorSource(df_c$Title))
    
    docs_c <- tm_map(docs_c, content_transformer(tolower))#makes them all lowercase
    docs_c <- tm_map(docs_c, removeNumbers)
    docs_c <- tm_map(docs_c, removePunctuation)
    docs_c <- tm_map(docs_c, stripWhitespace)#If there are multiple spaces between characters, they are collapsed to a single space
    docs_c <- tm_map(docs_c, removeWords, stopwords("english"))#removes words that aren't in the english language
    docs_c <- tm_map(docs_c, stemDocument, language = "english") #stems the words e.g. "Manager" turns into "manag"
    
    dtm_c <- DocumentTermMatrix(docs_c)
    
    mat.df_c <- as.data.frame(data.matrix(dtm_c), stringsAsfactors = FALSE)
    
    #Bind the Function
    mat.df_c <- cbind(mat.df_c, df_c$Function)
    colnames(mat.df_c)[ncol(mat.df_c)] <- "Function"
    
    #Bind the count
    mat.df_c <- cbind(mat.df_c, df_c$count)
    colnames(mat.df_c)[ncol(mat.df_c)] <- "count"
    
    # Split data by whether they have a real job function or not
    train_c1 <- which(mat.df_c$count == 1)
    test_c1 <- which(mat.df_c$count == 2)
    
    #isolate the classifier
    cl_c <- mat.df_c[, "Function"]
    
    test_c1<-unlist(test_c1, use.names=FALSE)
    train_c1<-unlist(train_c1, use.names=FALSE)
    
    # Create model data and remove Function and count
    modeldata_c <- mat.df_c[,!colnames(mat.df_c) %in% "Function"]
    modeldata_c <- modeldata_c[,!colnames(modeldata_c) %in% "count"]
    
    #Run the model
    #prob=TURE will return the % of nearest neighbors that were from the winning job Function class
    knn.pred_c <- knn(modeldata_c[train_c1, ], modeldata_c[test_c1, ], cl_c[train_c1], k=3,prob = TRUE)
    
    # Confusion matrix displays how many contacts/leads were assigned to each class 
    conf.mat_c <- table("Predictions" = knn.pred_c, Actual = cl_c[test_c1])
    conf.mat_c
    
    # Create data frame with test data and predicted category
    df.pred_c <- cbind(knn.pred_c, modeldata_c[test_c1, ])
    
    #return just the new job functions and attach them to the origional dataframe with IDs
    df.pred_c <- df.pred_c[ c(1) ]
    final_c <- cbind(test_c,df.pred_c)
    #Attach the probabilities
    prob_c<-data.frame(attr(knn.pred_c,"prob"))
    final_c <- cbind(final_c, prob_c)
    
    #Remove columns
    final_c <- final_c[ -c(4,5,6) ]
    
    #Rename probability column
    colnames(final_c)[5] <- "knn.prob_c"
    
    #Mark contacts/leads with less than 50% majority as "Unidentified"
    final_c$knn.pred_c <- as.character(final_c$knn.pred_c)
    final_c$knn.pred_c[final_c$knn.prob_c < .50] <- "Unidentified"
    
    #Remove probability
    final_c <- final_c[ -c(5) ]
    
    #Rename columns to match assigned
    names(final_c)<-c("Contact_ID","Lead_ID","Title", "Function")
    assigned_c <- assigned_c[-c(4)]
    
    #Combine functions identified in intial cleanse with those identified from the machine learning
    final_c <-rbind(final_c,assigned_c)
    
    }
    
  #If no contacts leads requried knn, label assigned as final
  if(nrow(test_c) == 0){final_c <- assigned_c}
    
  #Log assignment results in the following format: Corporate Job Function Log YYYY-MM-DD.csv
  Date<-Sys.Date()
  path_out = '/home/rstudio/JF_JL_Automation/Job Function Log/'
  write.csv(final_c,paste(path_out,'Corporate Job Function Log ',Date,'.csv',sep = ''))
    
  final_c<- final_c[-c(3)]
}

# Merge individuals from corporate and utility organizations
if(sum(test$Org_Type == "Corporate")==0 & sum(test$Org_Type == "Utility")>0){final <- final_u}#if there are no corp individuals, final dataframe consists only of ind. from utilties
if(sum(test$Org_Type == "Corporate")>0 & sum(test$Org_Type == "Utility")==0){final <- final_c}
if(sum(test$Org_Type == "Corporate")>0 & sum(test$Org_Type == "Utility")>0){final <- rbind(final_u,final_c)}

#If contacts/leads at Government, Non-Profit & Education orgs have functions, remove them and bind to final to be updated in Salesforce
if(nrow(df[df$Org_Type=="Government, Non-Profit & Education" && df$Function!="", ])>0){
  GNE<-df[df$Org_Type=="Government, Non-Profit & Education" && df$Function!="", ]
  GNE<-GNE[-c(3,4)]
  GNE$Function  <- ""
  final <- rbind(final,GNE)
}

#Seperate final into leads and contacts
Contacts<- final[!is.na(final$Contact_ID), ]
Contacts <- Contacts[ -c(2) ]
names(Contacts)<-c("Id", "Job_Function_2__c")

Leads<- final[!is.na(final$Lead_ID), ]
Leads <- Leads[ -c(1) ]
names(Leads)<-c("Id", "Job_Function_2__c")

##Update records in Salesforce
#Contacts
contact_job_info <- rforcecom.createBulkJob(session,  operation='update', object='Contact')

batches_info <- rforcecom.createBulkBatch(session,
                                          jobId=contact_job_info$id,
                                          data=Contacts,
                                          batchSize=25)

close_job_info <- rforcecom.closeBulkJob(session, jobId=contact_job_info$id)

#Leads
lead_job_info <- rforcecom.createBulkJob(session,  operation='update', object='Lead')

batches_info <- rforcecom.createBulkBatch(session,
                                          jobId=lead_job_info$id,
                                          data=Leads,
                                          batchSize=25)

close_job_info <- rforcecom.closeBulkJob(session, jobId=lead_job_info$id)
