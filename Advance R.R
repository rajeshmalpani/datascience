### Advanced R Programming Part I and II  ####

url <- "http://www.jaredlander.com/2012/02/another-kind-of-super-bowl-pool/"
require(XML)
# read an HTML table 
bowl <- readHTMLTable("http://www.jaredlander.com/2012/02/another-kind-of-super-bowl-pool/", header = FALSE, stringsAsFactors=FALSE, which=1)
bowl


### Chapter 1.2 - Use XPath for complex searches in HTML ####
address <-"http://www.menupages.com/restaurants/fiores-pizza/menu"
thePage <- readLines(address)
head(thePage)

require(XML)
pageRender <- htmlParse(thePage)

# exract street address from html page
address <- xpathApply(pageRender, "//li[@class='address adr']/span[@class = 'addr street-address']", fun=xmlValue)[[1]]
address
# exract citt from html page
city <- xpathApply(pageRender, "//li[@class='address adr']/span/span[@class = 'locality']", fun=xmlValue)[[1]]
city

headers <- xpathSApply(pageRender, "//*[@id='restaurant-menu']/h3", xmlValue)
headers

items <- xpathSApply(pageRender, "//table[starts-with(@class, 'prices-')]")
items

items <- lapply(items, readHTMLTable, stringsAsFactors=FALSE)

require(plyr)
menu <- "http://www.menupages.com/restaurants/all-areas/all-neighborhoods/pizza/"

doc <- htmlParse(menu)
doc

# parse Name and Link of the list of restaurants returned from menu url
placeNameLink <- xpathApply(doc, "//table/tr/td[@class='name-address']/a[@class='link']",
                            fun=function(x){ c(Name=xmlValue(x, recursive = FALSE), Link=xmlAttrs(x)[2]) })
placeNameLink <- ldply(placeNameLink) # convert the list object to a dataframe
head(placeNameLink)

### Chapter 1.3 - Use xmlToList for easier parsing ####
teaFile <- "http://www.jaredlander.com/data/SocialComments.xml"
require(XML)
teaParsed <- xmlToList(teaFile)
length(teaParsed)  # length of lst
str(teaParsed)      # structure of list

teaParsed[[1]][[1]]$id                                  # to extract id tag data of first child within first child
teaParsed[[1]][[1]]$author$name                         # to extract name tag within author tag of first child within first child
teaParsed[[1]][[1]]$published                           # to extract published tag data of first child within first child
teaParsed[[1]][[1]]$content$.attrs                      # to extract all attributes within a tag
teaParsed[[1]][[1]]$content$.attrs[["sentimentScore"]]  # to extract a given attribute within a tag



### Chap 4.1 - Build a recommendation engine with RecommenderLab ####
setwd("/Users/rajeshmalpani/workspace/Data Science/JaredLander/data")
require(utils)
download.file("http://files.grouplens.org/datasets/movielens/ml-100k.zip", destfile="/Users/rajeshmalpani/workspace/Data Science/JaredLander/data/ml-100k.zip")
unzip("ml-100k.zip", exdir="movies")
dir("ml-100k")
ratings <- read.table("ml-100k/u.data", header = FALSE, sep = "\t", col.names = c("UserID", "MovieID", "Rating", "Timetamp"))
head(ratings)
ratings$Timetamp <- as.POSIXct(ratings$Timetamp, origin = "1970-01-01")   # Convert data to Posix Format

require(reshape2)

ratingsMat <- dcast(UserID ~ MovieID, data=ratings, value.var = "Rating")
head(ratingsMat)
names(ratingsMat)
require(useful)
corner(ratingsMat)
rownames(ratingsMat) <- sprintf("User%s", ratingsMat$UserID)  # assign the rownname as UserId#
ratingsMat$UserID <- NULL   # remove UserID column from RatingsMat 

colnames(ratingsMat) <- sprintf("Movie%s", colnames(ratingsMat))   # assign column name as Movie#
corner(ratingsMat)
ratingsMat <- as.matrix(ratingsMat)   # convert data to a matrix

install.packages("recommenderlab"); 
require(recommenderlab)
rateMat <- as(ratingsMat, "realRatingMatrix")
head(as(rateMat, "data.frame"))
as(rateMat, "list")[[1]]
image(rateMat)
hist(getRatings(normalize(rateMat)), breaks = 100)

itemRec <- Recommender(rateMat, method="POPULAR")   # Get recommendation based on POPULAR
itemRec

getModel(itemRec)     # get the model built by recommender 

### Chap 4.2 - Mine text with RTextTools ####

install.packages("RTextTools")
require(RTextTools)

data("NYTimes", package="RTextTools")
head(NYTimes); dim(NYTimes)

# create a sparse matrix of title with option to removeNumbers, and stem words and removeSparseTerms
timesMat <- create_matrix(NYTimes$Title, removeNumbers = TRUE, stemWords = TRUE, removeSparseTerms = .998)  
timesMat

# create a training and test set
container <- create_container(timesMat, labels=NYTimes$Topic.Code, trainSize = 1:2500, testSize = 2501:NROW(NYTimes), virgin = FALSE)

# build a SVM & Elastic Net
SVM <- train_model(container = container, "SVM")        # SVM Model
GLMNET <- train_model(container = container, "GLMNET")  # Elastic Net model
 
SVM_Classify <- classify_model(container, SVM)          # Create a classification from the model
GLMNET_Classify <- classify_model(container, GLMNET)    # Create a classification from the model

analytis <- create_analytics(container, cbind(SVM_Classify, GLMNET_Classify))   # create analytics from the model classification
summary(analytis)

### Chap 5.1 Network Analysis / Get started with igraph  ####
update.packages("igraph")
require(igraph); 
require(tcltk)



g <- graph(c(1,2, 1,3,  2,3, 3,5), n=5 )   # demo of an igraph
plot(g)

g <- graph.tree(40, 4)

# different ways to layout a graph
plot(g)
plot(g, layout=layout.circle)
plot(g, layout=layout.fruchterman.reingold)
plot(g, layout=layout.graphopt)
plot(g, layout=layout.kamada.kawai)

# tcltk, Rcmdr & rgl hangs RStudio, troubleshoot later
tkplot(g, layout=layout.kamada.kawai)
l <- layout.kamada.kawai(g)
rglplot(g, layout=l)

g <- graph(c(1,1, 1,2,  1,3, 2,3, 4,5), n=5 )   # demo of an igraph
plot(g)

### Chap 5.2 - Read edgelists ####
jets <- read.table("http://www.jaredlander.com/data/routes.csv", sep=",", header = TRUE, stringsAsFactors = FALSE)
head(jets)

flights <- graph.data.frame(jets, directed = TRUE)  # build an directed edgelist 
print.igraph(flights, full = TRUE)    # prints the edgelists by location
plot(flights)
E(flights)      # to get a list of edges of this edgelist
V(flights)      # to get a list of vertices for the edgelist

vcount(flights) # count of # of vertices in edgelist
ecount(flights) # count of # of edges in edgelist 

flights2 <- as.undirected(flights)    # create an undirected edgelist 
print.igraph(flights2, full = TRUE)   # print the full edgelist of undirected edgelist
plot(flights2)
E(flights2)
vcount(flights2) # count of # of vertices in edgelist
ecount(flights2) # count of # of edges in edgelist 

plot(flights, layout=layout.fruchterman.reingold, edge.width=E(flights)$Time/100 )   # weighted graph by flight time
plot(flights, layout=layout.kamada.kawai)


### Chap 5.3 - Common graph metrics ####

average.path.length(flights)
diameter(flights)

farthest.nodes(flights)                     # gives a vertex list of farthest nodes
V(flights)[farthest.nodes(flights)]         # get the name of the farthest nodes 

largest.cliques(flights)                    # get the flight path with largest clique i.e. distance
V(flights)[largest.cliques(flights)[[1]]]   # get the first named vertices of largest clique route 
V(flights)[largest.cliques(flights)[[2]]]   # get the second named vertices of largest clique route 

transitivity(flights)
degree(flights)     # degree of each of the 

hist(degree(flights))  # frequency of degrees of flights

shortest.paths(flights)  # shortest segment in terms of hops for a vertex to another 
heatmap(shortest.paths(flights))  # a heatmap showing connectivity between airports

V(flights)[degree(flights) >= 30]$color <- "green"    # to color code flights with degree Greater than Equal to 30 to green
V(flights)[degree(flights) <= 14]$color <- "red"      # to color code flights with degree Less than Equal to 14 to red
plot(flights)  # when plotted the flights are now color coded per above assignment

plot(flights, edge.width=E(flights)$Time)  # this messes graph as width is taken literally
plot(flights, edge.width=E(flights)$Time/100)  # scaled version gives a better visua

flights3 <- flights
E(flights3)$weight <- E(flights)$Time
heatmap(shortest.paths(flights3))  # a heatmap showing connectivity between airports that is weighted by time
shortest.paths(flights3)           # as it is weighted now, it gives a measure of time, instead of hops

