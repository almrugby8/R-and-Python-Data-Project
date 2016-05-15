### R and Python Final Data Project ###
### Aaron Mauner ###
library(plyr)
library(rworldmap)
library(ggplot2)
library(data.table)
library(tableone)

### Data set pulled form twitter using twitterpull.py

### Data import
tweets <- read.csv("/Users/AaronMauner/Google Drive/UCD Spring '16/R and Python/Data Project/tweetsparsed.csv", na.strings="NA")

#removing indicator column
tweets <- tweets[,-1]

#Creating variable for each day of tweets
tweets$day <- substr(tweets$creation.time, 0, 10)

#deleting anything that is abnormal from creation_time column
tweets_clean<-tweets[grepl(" ", tweets$day),] #removes only 116 dirt obs

#formatting into an R date#
tweets_clean$day_R <- as.Date(tweets_clean$day,
                                   format = "%a %b %d")

#Removing multiple tweets by the same person
tweets_sorted <- tweets_clean[order(tweets_clean$timestamp),]
tweets_unique <- tweets_sorted[!duplicated(tweets_sorted$user.id),]

#length of time of data pull in days
(max(tweets_sorted$timestamp) - min(tweets_sorted$timestamp))/8.64e+7

#creating counts data set to plot count frequency against day#
ggplot(tweets_unique, aes(factor(day_R))) + geom_bar(fill='blue', colour="black") + labs(title="Number of Tweets by Date",x="Date",y="Number of Tweets")

#Pulling tweets with spatial information
tweets_geo <- tweets[which(!is.na(tweets$country)),]
countries <- as.data.frame(summary(tweets_geo$country))
setDT(countries, keep.rownames = TRUE)[]
colnames(countries) <- c("rn","count")
#fixing data for non-latin alphabet nations and renaming and removing null values
countries_arabic <- countries[69:73,]
countries_asian <- countries[74:78,]
countries <- countries[2:68]
#fixing Arabic, Urdu, and Persian language character countries
arabic_countries <- as.data.frame(c("United Arab Emirates","Saudi Arabia","Pakistan","Lebanon","Bahrain"))
countries_arabic <- as.data.frame(countries_arabic[,countries_arabic$count])
countries_arabic <- as.data.frame(cbind(arabic_countries,countries_arabic))
#fixing Asian language character countries
asian_countries <- as.data.frame(c("India","Thailand","South Korea","China","Japan"))
countries_asian <- as.data.frame(countries_asian[,countries_asian$count])
countries_asian <- as.data.frame(cbind(asian_countries,countries_asian))
#Rejoin corrected data set
countries <- rbind(countries,countries_arabic,countries_asian,use.names=F)
#Changing MISC countires
countries
countries[6,1] <- data.frame(rn="Belgium")
countries[8,1] <- data.frame(rn="Bosnia and Herzegovina")
countries[9,1] <- data.frame(rn="Brazil")
countries[12,1] <- data.frame(rn="Czech Republic")
countries[17,1] <- data.frame(rn="Denmark")
countries[18,1] <- data.frame(rn="Germany")
countries[22,1] <- data.frame(rn="Spain")
countries[29,1] <- data.frame(rn="Iceland")
countries[30,1] <- data.frame(rn="Italy")
countries[33,1] <- data.frame(rn="Latvia")
countries[37,1] <- data.frame(rn="Mexico")
countries[40,1] <- data.frame(rn="Netherlands")
countries[45,1] <- data.frame(rn="Norway")
countries[49,1] <- data.frame(rn="Poland")
countries[51,1] <- data.frame(rn="Philippines")
countries[53,1] <- data.frame(rn="Switzerland")
countries[54,1] <- data.frame(rn="Slovakia")
countries[56,1] <- data.frame(rn="Serbia")
countries[57,1] <- data.frame(rn="Finland")
countries[57,1] <- data.frame(rn="Sweden")
countries[59,1] <- data.frame(rn="Turkey")
countries[66,1] <- data.frame(rn="Vietnam")
countries[67,1] <- data.frame(rn="Russia")

#Plotting data points on a map
countrymap <- mapCountryData(pdf, nameColumnToPlot="count",catMethod="pretty",
               colourPalette=c("bisque","aquamarine","cyan",
               "deepskyblue","dodgerblue","blue","darkorchid",
               "darkslateblue","purple"),borderCol = "black",
               addLegend=F, mapTitle="Number of Tweets by Country",
               oceanCol="cadetblue1",missingCountryCol="white")
do.call(addMapLegend, c(countrymap,legendMar=0.85, legendIntervals="data"))

#Number of Tweets per User
user_count <- count(tweets, "user.id")
#Gives average number of tweets, min, max, etc
summary(CreateTableOne("freq",data=user_count))

#Finding the frequency of tweets in each language for unique users
lang_freq <- CreateTableOne("lang",data=tweets_unique)
languages <- as.data.frame(lang_freq[[2]][1]) #language of und means undefined
languages <- languages[order(languages$Overall.lang.freq),]
languages

#Plotting unque language frequency
ggplot(tweets_unique, aes(factor(lang))) + geom_bar(fill="aquamarine", colour="black") + labs(title="Number of Tweets by Language",x="ISO-2 Letter Language",y="Number of Tweets")

#gathering data on how many users reported a country#
summary(CreateTableOne("count",data=countries))
countries[order(countries$count),]

