library(reshape2)
library(ggplot2)
library(digest)
library(ggmap)

setwd("C:\\Users\\HS\\Desktop\\school\\classes\\VIS\\HW4\\git\\Edav-P4\\hs2865")

### Showing numbers of claims
# agency = read.csv("agency_2015.csv", as.is = TRUE)
# agency_f = data.frame(agency)
# save(agency_f, file="agency_f.Rda")
# load("agency_f.Rda")

# agency_list = unique(agency_f$Agency)

# agency_count <- matrix(0, ncol=length(agency_list), nrow=1)
# colnames(agency_count) <- agency_list

# for (i in 1:length(agency_list)) {
#   agency_count[1, agency_list[i]] = dim(agency_f[agency_f$Agency == agency_list[i], ])[1]
# }

# extract top 20
# agency_count_top10 = agency_count[1, order(x=agency_count, decreasing=TRUE)[1:10]]

# agency_graph <- data.frame(ag = names(agency_count_top10), val = agency_count_top10)
# agency_graph$ag <- factor(agency_graph$ag, levels = agency_graph$ag)

# save(agency_graph, file="agency_graph.Rda")
load("agency_graph.Rda")

options(scipen=999)

g <- ggplot(agency_graph, aes(x=ag, y=val))
g <- g + geom_bar(stat="identity")
g <- g + xlab("Agency")
g <- g + ylab("Claims")
g <- g + ggtitle("Top 10 agencies that had claims during 2015")
plot(g)

### Showing data on maps
load("agency_f.Rda")
map_nyc <- get_map(location="New York City", maptype = "satellite", zoom = 11)


# extract information in october
agency_NYPD_oct = agency_f[agency_f$Agency == "NYPD" & substr(agency_f$Created.Date, 1, 2) == "10", ]
ggmap(map_nyc, extent = "device") + 
  geom_density2d(data=agency_NYPD_oct, 
                 aes(x = agency_NYPD_oct$Longitude, 
                     y = agency_NYPD_oct$Latitude), 
                 size = 0.3) + 
  stat_density2d(data=agency_NYPD_oct, 
                 aes(x = agency_NYPD_oct$Longitude, 
                     y = agency_NYPD_oct$Latitude, 
                     fill = ..level.., 
                     alpha = ..level..), 
                 size = 0.01, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 1.0), guide = FALSE) +
  ggtitle("Claims reported to NYPD in Oct 2015")


# extract information in november
agency_NYPD_nov = agency_f[agency_f$Agency == "NYPD" & substr(agency_f$Created.Date, 1, 2) == "11", ]
ggmap(map_nyc, extent = "device") + 
  geom_density2d(data=agency_NYPD_nov, 
                 aes(x = agency_NYPD_nov$Longitude, 
                     y = agency_NYPD_nov$Latitude), 
                 size = 0.3) + 
  stat_density2d(data=agency_NYPD_nov, 
                 aes(x = agency_NYPD_nov$Longitude, 
                     y = agency_NYPD_nov$Latitude, 
                     fill = ..level.., 
                     alpha = ..level..), 
                 size = 0.01, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 1.0), guide = FALSE) +
  ggtitle("Claims reported to NYPD in Nov 2015")


# extract information in december
agency_NYPD_dec = agency_f[agency_f$Agency == "NYPD" & substr(agency_f$Created.Date, 1, 2) == "12", ]
ggmap(map_nyc, extent = "device") + 
  geom_density2d(data=agency_NYPD_dec, 
                 aes(x = agency_NYPD_dec$Longitude, 
                     y = agency_NYPD_dec$Latitude), 
                 size = 0.3) + 
  stat_density2d(data=agency_NYPD_dec, 
                 aes(x = agency_NYPD_dec$Longitude, 
                     y = agency_NYPD_dec$Latitude, 
                     fill = ..level.., 
                     alpha = ..level..), 
                 size = 0.01, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 1.0), guide = FALSE) +
  ggtitle("Claims reported to NYPD in Dec 2015")


