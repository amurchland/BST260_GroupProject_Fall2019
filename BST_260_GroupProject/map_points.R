require(sf)
library(ggplot2)
library(viridis)
library(dplyr)

setwd("C:/Users/Keya Joshi/Desktop/PHS_Fall1/BST-260/final_project/CENSUS2010_BLK_BG_TRCT_SHP/")
tmp <- read_sf("CENSUS2010TRACTS_POLY.shp")

setwd("C:/Users/Keya Joshi/Desktop/PHS_Fall1/BST-260/final_project/")
data <- read.csv("cleaned.csv")
DT_sf = st_as_sf(data, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=NAD83 +no_defs" )

## brookline census tracts: https://www.brooklinema.gov/DocumentCenter/View/1402/Census-Tract-11x17-PDF

## boston census tracts: http://www.bostonplans.org/getattachment/d09af00c-2268-437b-9e40-fd06d0cd20a2

## cambridge: https://www.cambridgema.gov/~/media/Files/CDD/Maps/Census/cddmap_census_2010_ct_bounds.pdf?la=en

brighton <- c(5.02, 5.03, 5.04, 4.01, 4.02, 3.02, 7.01, 6.02, 6.01, 2.02, 2.01, 3.01, 1)

allston <- c(7.04, 7.03, 8.02, 8.03, 9815.01)

fenway <- c(101.03, 101.04, 102.04, 102.03, 104.08, 104.03, 104.04, 104.05)

lma <- c(103, 808.01, 809, 810.01, 811)

jp <- c(1207, 812, 1205, 1206, 9818, 1204, 1203.01, 1201.03, 1201.05, 1201.04, 1202.01, 9810, 1101.03, 9811, 1103.01)

westroxbury <- c(1106.01, 1302, 1301, 1303, 1304.02, 1304.04, 1304.06, 9807)

brookline <- c(4012, 4011, 4010, 4009, 4008, 4007, 4006, 4005, 4004, 4003, 4002, 4001)

hyde_park <- c(1401.02, 1402.01, 1402.02, 1403, 1401.07, 1401.05, 1404)

dedham <- c(4021.01, 4025, 4024, 4021.02, 4022, 4023)

roslindale <- c(1106.07, 1104.01, 1103.01, 9811, 1102.01, 1104.03, 1105.02, 1105.01, 1401.06)

southend <- c(708, 707, 706, 703, 709, 705, 704.02, 711.01, 712.01)

beacon_hill <- c(9817, 201.01, 202, 203.02)

westend <- c(203.01)

downtown <- c(203.03, 303, 701.01, 702)

northend <- c(301, 302, 304, 305)

southboston <- c(612, 607, 606, 608, 611.01, 610, 604, 603.01, 605.01, 602, 601.01)

dorchester <- c(907, 913, 912, 911, 910.01, 909.01, 914, 915, 903, 918, 917, 916, 902, 901, 919, 920, 924, 923, 922, 921.01, 1001, 1005, 1006.01, 1002, 1003, 1004, 1008, 1007, 1006.03)

roxbury <- c(806.01, 805, 804.01, 801, 803, 906, 814, 817, 818, 904, 813, 815, 819, 820, 821, 9803)

cambridge <- c(3541, 3539, 3537, 3542)

names <- c(brighton, allston, fenway, lma, jp, roxbury, brookline, hyde_park, dedham, roslindale, southend, beacon_hill, westend, downtown, northend, southboston, dorchester, westroxbury, cambridge)

tmp$NAME10 <- as.numeric(tmp$NAME10)

test <- tmp %>% dplyr::filter(NAME10 %in% names)


test$ids <- ifelse(test$NAME10 %in% brighton, "brighton", 
                   ifelse(test$NAME10 %in% allston, "allston", 
                          ifelse(test$NAME10 %in% fenway, "fenway",
                                 ifelse(test$NAME10 %in% lma, "lma", 
                                        ifelse(test$NAME10 %in% jp, "jp", 
                                               ifelse(test$NAME10 %in% westroxbury, "westroxbury", 
                                                      ifelse(test$NAME10 %in% brookline, "brookline", 
                                                             ifelse(test$NAME10 %in% hyde_park, "hydepark", 
                                                                    ifelse(test$NAME10 %in% dedham, "dedham", 
                                                                           ifelse(test$NAME10 %in% roslindale, "roslindale", 
                                                                                  ifelse(test$NAME10 %in% southend, "southend", 
                                                                                         ifelse(test$NAME10 %in% beacon_hill, "beaconhill", 
                                                                                                ifelse(test$NAME10 %in% westend, "westend", 
                                                                                                       ifelse(test$NAME10 %in% downtown, "downtown", 
                                                                                                              ifelse(test$NAME10 %in% northend, "northend", 
                                                                                                                     ifelse(test$NAME10 %in% southboston, "southboston", 
                                                                                                                            ifelse(test$NAME10 %in% dorchester, "dorchester", 
                                                                                                                                   ifelse(test$NAME10 %in% roxbury, "roxbury", 
                                                                                                                                          ifelse(test$NAME10 %in% cambridge, "cambridge", NA)))))))))))))))))))


#tmp2$Latitude <- as.numeric(gsub("\\+", "", tmp2$INTPTLAT10))
#tmp2$Longitude <-as.numeric(sub("0", "", tmp2$INTPTLON10))
#tmp2$Longitube2 <- gsub("\\-", "", tmp2$Longitude)
#tmp2 <- tmp %>% filter(TRACTCE10 %in% tracts)
#tmp3 <- left_join(x = tmp2, y = data, by=c("Latitude", "Longitude")) 


ggplot() +
  geom_sf(data = test, aes(fill = ids)) +
  geom_sf(data = DT_sf, col = "white", size = 3) + 
  scale_fill_manual(values = c(viridis(19, 
                                       option = "A", 
                                       direction = -1))) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        line = element_blank(),
        rect = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 16, face = "bold") , 
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) + 
  labs(fill = "Neighborhoods")
  


## genius: https://gist.github.com/andrewheiss/0580d6ffec37b6bc4d0ae8e77bf30956
lat <- data.frame()
long <- data.frame()
for(i in 1:nrow(DT_sf)){
  lat[i,1] <- DT_sf$geometry[[i]][1]
  long[i,1] <- DT_sf$geometry[[i]][2]
}

data2 <- data.frame(data, lat, long)


tmp <- data2 %>% sf::st_as_sf(coords = c("V1", "V1.1"), crs = 4326)

tmp_transformed <- tmp %>% st_transform(crs = 102003)

tmp_transformed_with_lat_lon <- cbind(tmp_transformed, st_coordinates(tmp_transformed))

ggplot() +
  geom_sf(data = test, aes(fill = ids)) +
  geom_point(data = tmp_transformed_with_lat_lon, aes(x = X, y = Y), col = "red", size = 3) + 
  coord_sf(crs = 102003) + 
  scale_fill_manual(values = c(viridis(19, 
                                       option = "A", 
                                       direction = -1))) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        line = element_blank(),
        rect = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 16, face = "bold") , 
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) + 
  labs(fill = "Neighborhoods")


## Test with clustering

km.out=kmeans(dat[,c(2, 6, 15, 16, 17)], 6, nstart = 2)
plot(dat[,c(2, 6, 15, 16, 17)], col=(km.out$cluster+1), main="K-Means Clustering Results", pch=20, cex=2)
tmp_transformed_with_lat_lon$cluster <- km.out$cluster

ggplot() +
  geom_sf(data = test) +
  geom_point(data = tmp_transformed_with_lat_lon, aes(x = X, y = Y, color = as.factor(cluster)), size = 3) + 
  coord_sf(crs = 102003) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        line = element_blank(),
        rect = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 16, face = "bold") , 
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) + 
  labs(col = "Cluster")

