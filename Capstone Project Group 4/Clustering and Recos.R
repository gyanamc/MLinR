library(tm)

setwd("D:/New folder/rcodesandfile/Final")

#Generate a dataframe pixstory with variables - Title, Narration(articles) and interests (user generated)

pixstory <- read.csv("1000.csv")
 pixstory

#Concert into a corpus
narration <- VCorpus(VectorSource(pixstory$Narration))

#replace comma with spaces
clean_narration <- tm_map(narration, 
                         content_transformer(
                            function(x) gsub(","," ",x)
                            )
                         )


#Data cleaning....
inspect(clean_narration[[5]])

clean_narration2 <- tm_map(clean_narration, content_transformer(tolower))


clean_narration3 <- tm_map(clean_narration2, removePunctuation)

stopwords <- read.csv("stoplong.csv", header = FALSE)
stopwords <- as.character(stopwords)
stopwords <- c(stopwords, stopwords())

clean_narration4 <- tm_map(clean_narration3,removeWords,stopwords)

clean_narration5 = tm_map(clean_narration4, removeNumbers)

clean_narration6 = tm_map(clean_narration5, stripWhitespace)


inspect(clean_narration6[[5]])



#Generate the Document Term matrix
narration_dtm <- DocumentTermMatrix(clean_narration6)
narration_dtm
narration_dtm <- removeSparseTerms(narration_dtm, 0.999)
narration_dtm
#Inspect to Document Term matrix
inspect(narration_dtm)

#Convert the Document term matrix into a data frame

mat.df = as.data.frame(data.matrix(narration_dtm), stringsAsFactors = FALSE)
inspect(clean_narration6[[3]])

#Add the keywords(machine generated) from DTM to the dataframe - pixstory

for(j in 1:narration_dtm$nrow){
  s=""
  print(j)
for( k in 1:narration_dtm$ncol){
  if(mat.df[ j,k]==1)
  {
    s=paste(s,narration_dtm$dimnames$Terms[k],",")
    
  }
}
  pixstory$Keywords[j] = s
  
  }

#                  K-Means Clustering

#Setting the seed ensures repeatable results
set.seed(100)

#Create 3 clusters
narration_clusters <-  kmeans(narration_dtm, 3)

#Inspect the results
narration_clusters$cluster
narration_clusters
saveRDS(narration_clusters$cluster,"narration_cluster.rds")


#Add cluster information to the dataframe - pixstory 
for ( story in 1:nrow(pixstory)) {
  pixstory$Cluster[story] = narration_clusters$cluster[story]
}





#                  K-Means Optimization

#Function to find the optimum no. of clusters
optimal_cluster_plot <- function(data, iterations=10, seed=1000){
  
  #Set within-sum-of-squares for a single cluster
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  
  #Iterate upto 10 clusters and measure wss.
  for (i in 2:iterations){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
  }
  
  #Plot wss for each value of k and find the elbow
  plot(1:iterations, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares", col="red")}

#Execute the function
optimal_cluster_plot(narration_dtm)
saveRDS(optimal_cluster_plot(narration_dtm),"optimal_cluster.rds")

#-----------------------------------------------------------------------
#Content based recommendation engine


# import raw data file and trim leading and trailing whitespaces
setwd("D:/New folder/rcodesandfile")


Newsarticle <- read.csv("matdf.csv", header = TRUE)

Newsarticle = as.matrix(Newsarticle)

#Set up List of Algorithms

library(geometry)
cosine_sim <- function(a, b) dot(a,b)/sqrt(dot(a,a)*dot(b,b))

#Apply the Algorithm to the Matrix dataset

cos_sims <- apply(Newsarticle, 1,
                  FUN = function(y) 
                    cosine_sim(Newsarticle[5,], y))

write.csv(pixstory, "pixstory_Final")

#Add the the cosine distances from the selected article against other articles 
pixstory$Cossims = cos_sims

library(dplyr)
str(pixstory)

#To display - selected the title, cluster and the cosine distances (Run only once)

pixstory = pixstory %>%
  select( ,1,5,6)

#To run second time onwards for recommentdation 

cos_sims <- apply(Newsarticle, 1,
                  FUN = function(y) 
                    cosine_sim(Newsarticle[4,], y))


pixstory$Cossims =  cos_sims

pixstoryF = pixstory %>% arrange(desc(Cossims))

#Get top n result(1st one will be the same article)

print(top_n(pixstoryF,4,Cossims))
write.csv (pixstory, "pixstory.csv")

-------------------------------------------------------------------------------
