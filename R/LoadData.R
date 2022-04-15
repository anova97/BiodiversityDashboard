## to avoid long file loading, I have created separate files for polish, austria and Colombia dataset

# occurence <- read_csv("data/biodiversity-data 2/occurence.csv")
# 
# poland <- occurence[which(occurence$country=="Poland"),]
# 
# write.csv(poland,  "data/Poland.csv", row.names = FALSE)

poland <- read.csv("data/Poland.csv")

centerLatPl <- 52.068166394
centerLonPl <- 19.4749981

austria <- read.csv("data/Austria.csv")

centerLatAt <- 48
centerLonAt <- 16

kenya <- read.csv("data/Kenya.csv")

centerLatKe <- -1
centerLonKe <-  36

nepal <- read.csv("data/Nepal.csv")

centerLatNp <- 26
centerLonNp <-  87


logging::loginfo(msg = "data successfully loaded")
