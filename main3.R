
#I graduate in May. My parents are coming in from India for my graduation, 
# and they'll be in the city for a few days. Since I live with roommates, I can't 
# really have them stay at my place, so I need to book an Airbnb for us. 
# My motivation to undertake this project is to find out the 
# most value-for-money deal on Airbnbs in New York City.
# The project uses the NYC Airbnb data obtained from Kaggle. 
# (https://www.kaggle.com/dgomonov/new-york-city-airbnb-open-data#AB_NYC_2019.csv) 
setwd('./Wherebnb')
getwd()
nyc_ab = read.csv('data/nyc-airbnb.csv')
head(nyc_ab)


# Looking at a summary of the data using the summary function, or using the 
# describe function from the Hmisc package for a more comprehensive overview.
# install.packages('Hmisc')
# library(Hmisc)
# describe(nyc_ab)
summary(nyc_ab)


# Loading the library to import dplyr, ggplot, etc.
install.packages('tidyverse')
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
# dev.new()
png('reports/room_count_borough.png',width = 900,height = 720)
ggplot(data = borough_counts,aes(Var1,Freq)) %+% geom_bar(stat='identity',fill='darkred') %+%
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),axis.text=element_text(size=20),
        plot.title = element_text(size = 30),
        legend.text = element_text(size = 20),legend.title = element_text(size = 30),
        axis.title=element_text(size=30,face="bold")) %+%
  ylim(0,as.numeric(max_count)+1000) %+% ggtitle('Distribution of Airbnbs per Borough') %+% 
  xlab('Borough') %+% ylab('Count')
dev.off()

# ... and which neighbourhood?
# I'm in luck! Brooklyn has the second highest number of Airbnb listings. 
# Which means I have options to find a place that works for mom and dad. 
# Now, I need a place that is easily accessible by public transport, close to university, 
# or close to the commencement venue, or in a neighbourhood I know.
# So here's a look at the neigbourhoods with most Airbnb listings.
# Selecting all Brooklyn listings
bklyn = subset(nyc_ab, subset = neighbourhood_group == 'Brooklyn')
head(bklyn)
# Getting a count of number of listings per neighbourhood
bklyn_neighbourhoods = as.data.frame(table(bklyn$neighbourhood))
row.names(bklyn_neighbourhoods) = bklyn_neighbourhoods$Var1
# bklyn_neighbourhoods$Var1 = NULL
# Sorting the neighbourhoods by decreasing number of listings
bklyn_neighbourhoods = bklyn_neighbourhoods[order(bklyn_neighbourhoods$Freq,decreasing=TRUE),]
bklyn_neighbourhoods = bklyn_neighbourhoods[bklyn_neighbourhoods$Freq > 0,]
# Finding the top 15 neighbourhoods with most listings
# dev.new()
png('reports/listing_count_bk.png',width = 1600,height = 1600)
ggplot(bklyn_neighbourhoods,aes(Var1,Freq)) %+% geom_bar(stat='identity',fill='darkblue') %+%
  theme(axis.text=element_text(size=20),
        plot.title = element_text(size = 30),
        legend.text = element_text(size = 20),legend.title = element_text(size = 30),
        axis.title=element_text(size=30,face="bold")) %+% ylim(0,5000) %+%
  coord_polar() %+% xlab('Count') %+% ylab('Neighbourhood') %+% scale_y_log10() %+%
  ggtitle('Brooklyn Neighbourhoods with the most Airbnbs (Log10 scale)')
dev.off()

# With three, probably more, people involved, it is imperative that I have at least a private room
# or apartment. We shall examine the kinds of room types these listings actually are.
install.packages('RColorBrewer')
library(RColorBrewer)
# Create a two-way table with neighbourhood and room type.
room_types = xtabs(~neighbourhood+room_type,data=bklyn)
# Now, when you look at room_types, it lists the data from all the neighbourhoods in the entire city.
# We really just need the room types of the top 15 neighbourhoods in Brooklyn.
top15 =  head(bklyn_neighbourhoods,15)
room_types_bklyn = as.data.frame(room_types[c(top15$Var1),])
# Visualizing this information
# dev.new()
png('reports/room_type_percentage_bk.png',width = 1080,height = 1920)
ggplot() %+% geom_bar(aes(y=Freq,x=reorder(neighbourhood,Freq),fill=room_type),
                    data=room_types_bklyn,stat='identity',
                    position='fill') %+% coord_flip() %+% xlab('Percentage of Room Types') %+%
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),axis.text=element_text(size=20),
        plot.title = element_text(size = 30),
        legend.text = element_text(size = 20),legend.title = element_text(size = 30),
        axis.title=element_text(size=30,face="bold")) %+%
  scale_fill_brewer(palette = 'Set2') %+% ylab('Percentage') %+% xlab('Neighbourhood') %+%
  ggtitle('Percentages of Room Types in each Neighbourhood')
dev.off()


nyc_ab.trimmed = subset(nyc_ab,select = c(neighbourhood_group,neighbourhood,room_type,price,
                                                minimum_nights,number_of_reviews,availability_365))


# I need to find the neighbourhoods in Brooklyn that is an apartment and has the lowest price on average
aggregate(nyc_ab.trimmed,by=list(nyc_ab.trimmed$neighbourhood_group),FUN=mean)
# aggregate nyc_ab.trimmed by borough and neighbourhood, and calculate the average price for each combination
mean_prices = aggregate(nyc_ab.trimmed,by=list(nyc_ab.trimmed$neighbourhood_group,nyc_ab.trimmed$neighbourhood),FUN = mean)
# Extract the combinations where borough is Brooklyn
mpbk = mean_prices[mean_prices$Group.1 == 'Brooklyn',]


# Calculating euclidean distance of each listing from commencement venue
bclat = 40.6826
bclong = -73.9754
class(c(bclat,bclong))
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

dist <- NULL
for(i in 1:nrow(bklyn)){
  dist[i] = euc.dist(c(bklyn[i,]$latitude,bklyn[i,]$longitude),c(bclat,bclong))
}


# Creating a distance column and removing unused columns
bklyn$dist = dist
bklyn.trimmed = subset(bklyn,select = c(neighbourhood_group,neighbourhood,room_type,latitude,longitude,
                                              dist,price,minimum_nights,number_of_reviews,availability_365))

# Creating a data frame with max listing price of 600, since a lot of the prices 
# skew the data due to being outliers
test = bklyn.trimmed[bklyn.trimmed$price<=600,]
# The smooth scatter plot plots the density of observations in a particular region.
# The darker the color, the more number of points in the region.
png('reports/listing_price_density.png',width = 800, height = 640)
smoothScatter(x =test$dist,y = test$price,xlab = 'Distance from Commencement Venue',
              ylab = 'Price', main = 'Density of Listings ')
dev.off()
# From the image, we observe that most of the listings under a hundred dollars are
# available in the regions that are around an euclidean distance of 0.04 from the 
# commencement venue.

# For each neighbourhood in Brooklyn, the average prices are plotted.
# It is seen that as you move closer to downtown Brooklyn, the prices rise.
png('reports/mean_price_neighbourhood_bk.png',width = 1080,height = 1080)
ggplot(mpbk,aes(reorder(Group.2,price),price)) %+%
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),axis.text=element_text(size=20),
        plot.title = element_text(size = 30),
        legend.text = element_text(size = 20),legend.title = element_text(size = 30),
        axis.title=element_text(size=30,face="bold")) %+% ylim(0,600) %+%
  geom_bar(stat='identity',aes(fill=price)) %+% coord_flip() %+%
  xlab('Neighbourhood') %+% ylab('Mean Price') %+% ggtitle('Mean Price per Neighbourhood')
dev.off()

# We create a dataframe with the median prices of each kind of listing for different neighbourhoods.
# This is because some of the prices are practically outliers, and as a result, shift the average a whole lot.
# All neighbourhoods without a specific kind of listing show up as NA, which is handled by setting the NAs to zero.
med_room_type_prices = data.frame()
for (j in bklyn_neighbourhoods$Var1){
  med_room_type_prices = rbind(med_room_type_prices,
                               j = c(median(bklyn[bklyn$neighbourhood == j & bklyn$room_type == 'Entire home/apt',"price"], na.rm = TRUE),
                                     median(bklyn[bklyn$neighbourhood == j & bklyn$room_type == 'Private room',"price"], na.rm = TRUE),
                                     median(bklyn[bklyn$neighbourhood == j & bklyn$room_type == 'Shared room',"price"], na.rm = TRUE)))
  med_room_type_prices[is.na(med_room_type_prices)] = 0
}


# Another column is added which is the neighbourhood name for plotting purposes.
med_room_type_prices$neighbourhood = bklyn_neighbourhoods$Var1
# The column names are changed 
names(med_room_type_prices) = c('Apartment','Private','Shared','neighbourhood')


# What I need now is a long-form table for all combinations of neighbourhoods and room types
# This is used for plotting.
library(reshape2)
melted_med_prices = melt(med_room_type_prices)


# The melted data frame is now plotted. The median prices for each category of rooms for each neighbourhood is plotted.
# dev.new()
png('reports/room_type_median_price.png',width = 1920,height = 1080)
ggplot(melted_med_prices,aes(x = neighbourhood,y = value,group = interaction(neighbourhood,value),fill = variable)) %+%
  geom_bar(position = 'dodge',stat='identity') %+% scale_fill_brewer(palette = 'Dark2') %+%
  theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.text=element_text(size=20),
        plot.title = element_text(size = 30),
        legend.text = element_text(size = 20),legend.title = element_text(size = 30),
        axis.title=element_text(size=30,face="bold")) %+% xlab('Neighbourhood') %+% ylab("Average Price") %+%
  ggtitle("Average prices per room type for each neighbourhood") %+% ylim(0,300)
graphics.off()

