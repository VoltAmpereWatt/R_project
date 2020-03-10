
#I graduate in May. My parents are coming in from India for my graduation, 
# and they'll be in the city for a few days. Since I live with roommates, I can't 
# really have them stay at my place, so I need to book an Airbnb for us. 
# My motivation to undertake this project is to find out the 
# most value-for-money deal on Airbnbs in New York City.
# The project uses the NYC Airbnb data obtained from Kaggle. 
# (https://www.kaggle.com/dgomonov/new-york-city-airbnb-open-data#AB_NYC_2019.csv) 
nyc_ab = read.csv('nyc-airbnb.csv')
head(nyc_ab)


# Looking at a summary of the data using the summary function, or using the 
# describe function from the Hmisc package for a more comprehensive overview.
install.packages('Hmisc')
library(Hmisc)
summary(nyc_ab)
describe(nyc_ab)


# Loading the library to import dplyr, ggplot, etc.
library(tidyverse)


# What borough do I live in?
# If I'm looking for a place to stay, I am looking at multiple options so that 
# one of them would fit my needs. Also, my commencement ceremony will happen in Brooklyn, 
# so I already have a preference for where my Airbnb should be. So, a count of the 
# number of listings in each borough is obtained.
borough_counts = as.data.frame(table(nyc_ab$neighbourhood_group))
min_count = min(borough_counts$Freq)
max_count = max(borough_counts$Freq)


# This data is now plotted using a barplot.
dev.new()
ggplot(data = borough_counts,aes(Var1,Freq)) %+% geom_bar(stat='identity',fill='darkred') %+%
  ylim(0,as.numeric(max_count)+1000) %+% ggtitle('Distribution of Airbnbs per Borough') %+% 
  xlab('Borough') %+% ylab('Count')


# ... and which neighbourhood?
# I'm in luck! Brooklyn has the second highest number of Airbnb listings. 
# Which means I have options to find a place that works for mom and dad. 
# Now, I need a place that is easily accessible by public transport, close to university, 
# or close to the commencement venue, or in a neighbourhood I know.
# So here's a look at the neigbourhoods with most Airbnb listings.
# Selecting all Brooklyn listings
bklyn = subset(nyc_ab, subset = neighbourhood_group == 'Brooklyn')
# Getting a count of number of listings per neighbourhood
bklyn_neighbourhoods = as.data.frame(table(bklyn$neighbourhood))
row.names(bklyn_neighbourhoods) = bklyn_neighbourhoods$Var1
bklyn_neighbourhoods$Var1 = NULL
# Sorting the neighbourhoods by decreasing number of listings
bklyn_neighbourhoods = bklyn_neighbourhoods[order(bklyn_neighbourhoods$Freq,decreasing=TRUE),]
# Finding the top 15 neighbourhoods with most listings
dev.new()
ggplot(top15,aes(reorder(Var1,Freq),Freq)) %+% geom_bar(stat='identity',fill='darkgreen') %+%
  coord_flip() %+% xlab('Count') %+% ylab('Neighbourhood') %+% 
  ggtitle('Brooklyn Neighbourhoods with the most Airbnbs')


# With three, probably more, people involved, it is imperative that I have at least a private room
# or apartment. We shall examine the kinds of room types these listings actually are.
install.packages('RColorBrewer')
library(RColorBrewer)
# Create a two-way table with neighbourhood and room type.
room_types = xtabs(~neighbourhood+room_type,data=bklyn)
# Now, when you look at room_types, it lists the data from all the neighbourhoods in the entire city.
# We really just need the room types of the top 15 neighbourhoods in Brooklyn.
room_types_bklyn = as.data.frame(room_types[c(top15$Var1),])
# Visualizing this information
dev.new()
ggplot() %+% geom_bar(aes(y=Freq,x=reorder(neighbourhood,Freq),fill=room_type),
                    data=room_types_bklyn,stat='identity',
                    position='fill') %+% coord_flip() %+% xlab('Percentage of Room Types') %+% 
  scale_fill_brewer(palette = 'Set2') %+% ylab('Percentage') %+% xlab('Neighbourhood') %+%
  ggtitle('Percentages of Room Types in each Neighbourhood')

nyc_ab.trimmed = subset(nyc_ab,select = c(neighbourhood_group,neighbourhood,room_type,price,
                                                minimum_nights,number_of_reviews,availability_365))
# train = sample(nrow(nyc_ab), 0.7*nrow(nyc_ab))
# nyc_ab.train = nyc_ab.trimmed[train,]
# nyc_ab.validate = nyc_ab.trimmed[-train,]
# head(nyc_ab.train)
# fit.nyc_ab  = lm(price ~ neighbourhood_group + neighbourhood + room_type + price + minimum_nights + number_of_reviews +availability_365,data = nyc_ab.train)
# summary(fit.nyc_ab)


names(nyc_ab)
library(car)
scatterplotMatrix(nyc_ab.trimmed)

aggregate(nyc_ab.trimmed,by=list(nyc_ab.trimmed$neighbourhood_group),FUN=mean)
mean_prices = aggregate(nyc_ab.trimmed,by=list(nyc_ab.trimmed$neighbourhood_group,nyc_ab.trimmed$neighbourhood),FUN = mean)
mpbk = mean_prices[mean_prices$Group.1 == 'Brooklyn',]


bclat = 40.6826
bclong = -73.9754
class(c(bclat,bclong))
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

dist <- NULL
for(i in 1:nrow(bklyn)){
  dist[i] = euc.dist(c(bklyn[i,]$latitude,bklyn[i,]$longitude),c(bclat,bclong))
}

bklyn$dist = dist
bklyn.trimmed = subset(bklyn,select = c(neighbourhood_group,neighbourhood,room_type,latitude,longitude,
                                              dist,price,minimum_nights,number_of_reviews,availability_365))


ggplot(mpbk,aes(reorder(Group.2,price),price)) %+% 
  geom_bar(stat='identity',aes(fill=price)) %+% coord_flip() %+%
  xlab('Neighbourhood') %+% ylab('Mean Price') %+% ggtitle('Mean Price per Neighbourhood')


