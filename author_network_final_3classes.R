#https://github.com/mihai-craita/countries_center_box/blob/master/countries.csv
library(readxl)
library(dplyr)
library(purrr)
library(igraph)
library(ggplot2)
library(ggraph)
library(ggmap)
library(ggpubr)
library(graphics)
library(grid)
library(rgeos)
library(rworldmap)


#extra ggplot function

# copied from ggplot2 `geom_curve`
geom_curve2 <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        curvature = 0.5,
                        angle = 90,
                        ncp = 5,
                        arrow = NULL,
                        lineend = "butt",
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCurve2, # call `GeomCurve2` instead of `GeomCurve`
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,
      curvature = curvature,
      angle = angle,
      ncp = ncp,
      lineend = lineend,
      na.rm = na.rm,
      ...
    )
  )
}

# copied from ggplot2 `GeomCurve`
GeomCurve2 <- ggproto("GeomCurve2", GeomSegment,
                      # the following `default_aes =` statement is missing in ggplot2 `GeomCurve`
                      default_aes = aes(colour = "black", fill = "black", size = 0.5, linetype = 1, alpha = NA),
                      draw_panel = function(data, panel_params, coord, curvature = 0.5, angle = 90,
                                            ncp = 5, arrow = NULL, lineend = "butt", na.rm = FALSE) {
                        if (!coord$is_linear()) {
                          warning("geom_curve is not implemented for non-linear coordinates",
                                  call. = FALSE)
                        }
                        trans <- coord$transform(data, panel_params)
                        
                        curveGrob(
                          trans$x, trans$y, trans$xend, trans$yend,
                          default.units = "native",
                          curvature = curvature, angle = angle, ncp = ncp,
                          square = FALSE, squareShape = 1, inflect = FALSE, open = TRUE,
                          gp = gpar(
                            col = alpha(trans$colour, trans$alpha),
                            # the following `fill = ` statement is missing in ggplot2 `GeomCurve`
                            fill = alpha(trans$fill, trans$alpha),
                            lwd = trans$size * .pt,
                            lty = trans$linetype,
                            lineend = lineend),
                          arrow = arrow
                        )
                      }
)
####################


# get world map
wmap <- getMap(resolution="high")

# get centroids
centroids <- gCentroid(wmap, byid=TRUE)

# get a data.frame with centroids
df <- as.data.frame(centroids)
head(df)
df$name <- rownames(df)
rownames(df) <- NULL
countries <- df

#Shortning some country names and correcting some other ones to make them similar to those in our table
countries <- countries %>%
  mutate(name = recode(name, "United States of America" = "USA",
                       "United Kingdom" = "UK",
                       "Democratic Republic of the Congo" = "DRC",
                       "United Republic of Tanzania" = "Tanzania",
                       "Ivory Coast" = "Côte d’Ivoire",
                       "ROC" = "Republic of Congo"))
##################
#################
#The following reads from a relative path. If you open the R project, the working directory is automatically set correctly and this code will work
authors <- read_xlsx("data/AFILKScenarios_Literature_Database.xlsx", sheet="Authors_from1Region")
authors_2 <- read_xlsx("data/AFILKScenarios_Literature_Database.xlsx", sheet="Authors_from2Regions")
#countries <- read.csv("C:/BRAVE/personal/Bwalya/network_mapping/country-capitals.csv", sep=",", header = TRUE)

#For authors (Those with first author either from Africa, or Overseas)
#Rectifying some country names
authors <- as.data.frame(sapply(authors,gsub,pattern="The Netherlands",replacement="Netherlands"))
authors <- as.data.frame(sapply(authors,gsub,pattern="indonesia",replacement="Indonesia"))
authors <- as.data.frame(sapply(authors,gsub,pattern="Phillipines",replacement="Philippines"))
authors <- as.data.frame(sapply(authors,gsub,pattern="Ivory Coast",replacement="Côte d’Ivoire"))
authors <- as.data.frame(sapply(authors,gsub,pattern="ROC",replacement="Republic of Congo"))

#First column is first author's country, and the other columns are the coauthors
first_authors <- as.data.frame(authors[,1]) ; names(first_authors)="1st"
co_authors <- authors[,2:ncol(authors)] ; co_authors <- filter(co_authors, rowSums(is.na(co_authors)) != ncol(co_authors))


#For authors (Those with first author affiliated both in Africa and Overseas at the same time)
#Rectifying some country names
authors_2 <- as.data.frame(sapply(authors_2,gsub,pattern="The Netherlands",replacement="Netherlands"))
authors_2 <- as.data.frame(sapply(authors_2,gsub,pattern="indonesia",replacement="Indonesia"))
authors_2 <- as.data.frame(sapply(authors_2,gsub,pattern="Phillipines",replacement="Philippines"))
authors_2 <- as.data.frame(sapply(authors_2,gsub,pattern="Ivory Coast",replacement="Côte d’Ivoire"))
authors_2 <- as.data.frame(sapply(authors_2,gsub,pattern="ROC",replacement="Republic of Congo"))

##Counting unique first authors countries and unique coauthors countries
###########################
unique_countries_first <- data.frame(country=character(), count=integer())
unique_countries_co <- data.frame(country=character(), count=integer())

for(i in 1:nrow(first_authors)){
  # i=24
  row_i <- as.character(first_authors[i,][!is.na(first_authors[i,])]) #gets for a given row only existing author countries, and ignores empty cells
  row_i <- unlist(strsplit(row_i, "[|]"))
  
  unique_country_i <- tabulate(as.factor(row_i))
  names(unique_country_i) <- levels(as.factor(row_i))
  for(j in names(unique_country_i)){
    if(j %in% unique_countries_first$country) unique_countries_first[which(unique_countries_first$country==j),"count"] <- unique_countries_first[which(unique_countries_first$country==j),"count"] + as.numeric(unique_country_i[j])
    else unique_countries_first <- rbind(unique_countries_first, data.frame(country=j,count=as.numeric(unique_country_i[j])))
  }
}

for(i in 1:nrow(co_authors)){
  # i=24
  row_i <- as.character(co_authors[i,][!is.na(co_authors[i,])]) #gets for a given row only existing author countries, and ignores empty cells
  row_i <- unlist(strsplit(row_i, "[|]"))
  
  unique_country_i <- tabulate(as.factor(row_i))
  names(unique_country_i) <- levels(as.factor(row_i))
  for(j in names(unique_country_i)){
    if(j %in% unique_countries_co$country) unique_countries_co[which(unique_countries_co$country==j),"count"] <- unique_countries_co[which(unique_countries_co$country==j),"count"] + as.numeric(unique_country_i[j])
    else unique_countries_co <- rbind(unique_countries_co, data.frame(country=j,count=as.numeric(unique_country_i[j])))
  }
}

#counting global unique countries and all connections and counts
############################################################

country_pairs_old <- data.frame(country1=character(),country2=character(),count=integer())
unique_countries <- data.frame(country=character(), count=integer())

for(i in 1:nrow(authors)){
  # i=24
  row_i <- as.character(authors[i,][!is.na(authors[i,])]) #gets for a given row only existing author countries, and ignores empty cells
  row_i <- unlist(strsplit(row_i, "[|]"))

  unique_country_i <- tabulate(as.factor(row_i))
  names(unique_country_i) <- levels(as.factor(row_i))
  for(j in names(unique_country_i)){
    if(j %in% unique_countries$country) unique_countries[which(unique_countries$country==j),"count"] <- unique_countries[which(unique_countries$country==j),"count"] + as.numeric(unique_country_i[j])
    else unique_countries <- rbind(unique_countries, data.frame(country=j,count=as.numeric(unique_country_i[j])))
  }
  if(length(row_i)>=2){
    all_comb <- as.data.frame(t(combn(sort(row_i), 2)))
    names(all_comb) <- c("country1","country2")
    keep <- apply(all_comb, 1, function(x) length(unique(x[!is.na(x)])) != 1)
    all_comb <- all_comb[keep, ]
    all_comb <- all_comb %>% group_by_all %>% count
    names(all_comb)<-c("country1","country2","count")
  }

  # existing_row <- as.numeric(rownames(plyr::match_df(country_pairs[,c(1,2)],all_comb[,c(1,2)])))
  country_pairs_old <- rbind(country_pairs_old,all_comb)

}

##########Counting country pairs a bit differently to get only first authors to co-authors connections

###!!!!For authors affiliated either in Africa or overseas
country_pairs <- data.frame(country1=character(),country2=character(),count=integer())

'%ni%' <- Negate('%in%')
for(i in 1:nrow(authors)){
  row_i_first <- as.character(authors[i,1][!is.na(authors[i,1])])
  row_i_first <- unlist(strsplit(row_i_first, "[|]"))
  
  row_i_co <- as.character(authors[i,2:ncol(authors)][!is.na(authors[i,2:ncol(authors)])])
  row_i_co <- unlist(strsplit(row_i_co, "[|]"))
  
  # country1 <- NA
  # countr2 <- NA
  for(j in 1:length(row_i_first)){
    #if(row_i_first[j])
    country1 <- NA
    country1 <- row_i_first[j]
    if(length(row_i_co) > 0){
      for(k in 1:length(row_i_co)){
        countr2 <- NA
        country2 <- row_i_co[k]
        pair <- data.frame(country1,country2,1)
        names(pair) <- c("country1","country2","count")
        country_pairs <- rbind(country_pairs,pair)
      }
    }
  }
  
  
}



country_pairs <- country_pairs %>%
  group_by(country1, country2) %>%
  summarize(count = sum(count))

country_pairs <- country_pairs[which(country_pairs$country1 != country_pairs$country2),]
###!!!!


###!!!!For authors affiliated simultaneaously in Africa or overseas
country_pairs_2 <- data.frame(country1=character(),country2=character(),count=integer())

'%ni%' <- Negate('%in%')
for(i in 1:nrow(authors_2)){
  row_i_first <- as.character(authors_2[i,1][!is.na(authors_2[i,1])])
  row_i_first <- unlist(strsplit(row_i_first, "[|]"))
  
  row_i_co <- as.character(authors_2[i,2:ncol(authors_2)][!is.na(authors_2[i,2:ncol(authors_2)])])
  row_i_co <- unlist(strsplit(row_i_co, "[|]"))
  
  # country1 <- NA
  # countr2 <- NA
  for(j in 1:length(row_i_first)){
    #if(row_i_first[j])
    country1 <- NA
    country1 <- row_i_first[j]
    if(length(row_i_co) > 0){
      for(k in 1:length(row_i_co)){
        countr2 <- NA
        country2 <- row_i_co[k]
        pair <- data.frame(country1,country2,1)
        names(pair) <- c("country1","country2","count")
        country_pairs_2 <- rbind(country_pairs_2,pair)
      }
    }
  }
  
  
}



country_pairs_2 <- country_pairs_2 %>%
  group_by(country1, country2) %>%
  summarize(count = sum(count))

country_pairs_2 <- country_pairs_2[which(country_pairs_2$country1 != country_pairs_2$country2),]
###!!!


###!!!Combining country_pairs with country_pairs_2
country_pairs$Class <- "1Region"
country_pairs_2$Class <- "2Regions"


country_pairs <- rbind(country_pairs, country_pairs_2)

#####################################################

country_pairs$x <- as.numeric(NA)
country_pairs$y <- as.numeric(NA)
country_pairs$xend <- as.numeric(NA)
country_pairs$yend <- as.numeric(NA)

unique_countries$lon <- as.numeric(NA)
unique_countries$lat <- as.numeric(NA)


coord_finder_simpl <- function(r,unique_countries,lon_lat){#long_lat will take either CapitalLongitude or CapitalLatitude
  country_name <- as.character(unique_countries[r,"country"])
  corr_coord <- countries[which(countries$name==country_name), lon_lat]
  return(corr_coord)
}


coord_finder <- function(r,country1_2,lon_lat){#long_lat will take either CapitalLongitude or CapitalLatitude
  country_name <- as.character(country_pairs[r,country1_2])
  corr_coord <- countries[which(countries$name==country_name), lon_lat]
  return(corr_coord)
}


country_pairs$x <- unlist(lapply(1:nrow(country_pairs),FUN=coord_finder, country1_2="country1",lon_lat="x"))
country_pairs$xend <- unlist(lapply(1:nrow(country_pairs),FUN=coord_finder, country1_2="country2",lon_lat="x"))
country_pairs$y <- unlist(lapply(1:nrow(country_pairs),FUN=coord_finder, country1_2="country1",lon_lat="y"))
country_pairs$yend <- unlist(lapply(1:nrow(country_pairs),FUN=coord_finder, country1_2="country2",lon_lat="y"))

#adding weights
range01 <- function(x){(x/max(x))}

country_pairs$weight <- range01(country_pairs$count)

#Creating to and from Africa

dput(unique(country_pairs$country1))#lists in the console all unique countries of first authors

country_pairs$origin <- NA

#this is a bit hard-coded, but I select myself from the previous dput the african countries.
country_pairs[country_pairs$Class=="1Region" & (country_pairs$country1 %in% c("Cameroon", "Côte d’Ivoire", "Ethiopia", "Ghana", 
                                            "Kenya", "Senegal", "South Africa", "Tanzania")),"origin"] <- "From Africa"

'%ni%' <- Negate('%in%')
country_pairs[country_pairs$Class=="1Region" & (country_pairs$country1 %ni% c("Cameroon", "Côte d’Ivoire", "Ethiopia", "Ghana", 
                                            "Kenya", "Senegal", "South Africa", "Tanzania")),"origin"] <- "From Overseas"

country_pairs[country_pairs$Class=="2Regions","origin"] <- "From Africa & Overseas"




unique_countries$lon <- unlist(lapply(1:nrow(unique_countries),FUN=coord_finder_simpl, unique_countries=unique_countries, lon_lat="x"))
unique_countries$lat <- unlist(lapply(1:nrow(unique_countries),FUN=coord_finder_simpl, unique_countries=unique_countries, lon_lat="y"))

unique_countries$weight <- range01(unique_countries$count)

#doing the previous for main authors and co-authors separately
unique_countries_first$lon <- unlist(lapply(1:nrow(unique_countries_first),FUN=coord_finder_simpl, unique_countries=unique_countries_first, lon_lat="x"))
unique_countries_first$lat <- unlist(lapply(1:nrow(unique_countries_first),FUN=coord_finder_simpl, unique_countries=unique_countries_first, lon_lat="y"))


unique_countries_co$lon <- unlist(lapply(1:nrow(unique_countries_co),FUN=coord_finder_simpl, unique_countries=unique_countries_co, lon_lat="x"))
unique_countries_co$lat <- unlist(lapply(1:nrow(unique_countries_co),FUN=coord_finder_simpl, unique_countries=unique_countries_co, lon_lat="y"))

#The weights for first authors and co-authors should be normalized by the same maximum
unique_countries_first$weight <- unique_countries_first$count/max(c(unique_countries_first$count,unique_countries_co$count))

unique_countries_co$weight <- unique_countries_co$count/max(c(unique_countries_first$count,unique_countries_co$count))

unique_countries_co$Authorship <- "Co-author"
unique_countries_first$Authorship <- "First author"

unique_countries_merged <- rbind(unique_countries_co, unique_countries_first)
unique_countries_merged$Authorship <- factor(unique_countries_merged$Authorship, levels=c("Co-author", "First author"))

#writing out unique_countries...
write.table(unique_countries,"data/unique_countries.csv",
            sep=",", row.names = F)
write.table(unique_countries_first,"data/unique_countries_first.csv",
            sep=",", row.names = F)
write.table(unique_countries_co,"data/unique_countries_co.csv",
            sep=",", row.names = F)
write.table(unique_countries_merged,"data/unique_countries_merged.csv",
            sep=",", row.names = F)


#writing out also country_pairs
write.table(country_pairs,"data/country_pairs_3Class.csv",
            sep=",", row.names = F)
######################################################
###################PLOTING#########################
###################################################

# common plot theme
maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "#596673")) + #596673
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))

# common polygon geom for plotting the country shapes
country_shapes <- geom_polygon(data = map_data('world'), aes(x = long, y = lat, group = group),
                               fill = "#f2f6fa", color = "#515151", size = 0.15)
# common coordinate system for all the following plots
mapcoords <- coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))


##################
theme_transp_overlay <- theme(
  panel.background = element_rect(fill = "transparent", color = NA),
  plot.background = element_rect(fill = "transparent", color = NA)
)


p_base <- ggplot() + country_shapes + mapcoords + maptheme


#Edges with 2 different colors

p_edges_ori_afr <- ggplot(country_pairs[country_pairs$origin =="From Africa",]) +
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend,     # draw edges as arcs
                 size = weight),arrow=arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0.33, alpha = 0.25,color = "#af01ff") +#ff860d
  scale_size_continuous(guide = "none", range = c(0.7, 1.8)) +  # scale for edge widths
  mapcoords + maptheme + theme_transp_overlay +
  theme(legend.position = c(0.5, 0.05), legend.direction = "horizontal")

p_edges_ori_afr

p_edges_dest_afr <- ggplot(country_pairs[country_pairs$origin =="From Overseas",]) +
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend,     # draw edges as arcs
                 size = weight),arrow=arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0.33, alpha = 0.25,color = "#0dffff") +
  scale_size_continuous(guide = "none", range = c(0.7, 1.8)) +  # scale for edge widths
  mapcoords + maptheme + theme_transp_overlay +
  theme(legend.position = c(0.5, 0.05), legend.direction = "horizontal")

p_edges_dest_afr

p_edges_both <- ggplot(country_pairs[country_pairs$origin =="From Africa & Overseas",]) +
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend,     # draw edges as arcs
                 size = weight),arrow=arrow(length = unit(0.03, "npc"), type = "closed"),
             curvature = 0.33, alpha = 0.25,color = "#ff9901") + #08ff08
  scale_size_continuous(guide = "none", range = c(0.7, 1.8)) +  # scale for edge widths #c(0.7, 2.5)
  mapcoords + maptheme + theme_transp_overlay +
  theme(legend.position = c(0.5, 0.05), legend.direction = "horizontal")

p_edges_both



p_edges <- ggplot(country_pairs) +
  geom_curve2(aes(x = x, y = y, xend = xend, yend = yend,     # draw edges as arcs
                  size = weight),arrow=arrow(length = unit(0.03, "npc"), type = "closed"),
              curvature = 0.33, alpha = 0.15,color = "#0dffff") +
  geom_curve2(aes(x = x, y = y, xend = xend, yend = yend,     # draw edges as arcs
                  size = weight),arrow=arrow(length = unit(0.03, "npc"), type = "closed"),fill="red",
              curvature = 0.33, alpha = 0.5,color = NA)+
  scale_size_continuous(guide = "none", range = c(0.7, 2.5)) +  # scale for edge widths
  mapcoords + maptheme + theme_transp_overlay +
  theme(legend.position = c(0.5, 0.05), legend.direction = "horizontal")

p_edges


p_nodes_Legend2 <- ggplot(unique_countries) +
  geom_point(data=unique_countries_co, aes(x = lon, y = lat, size = weight),
             shape = 21, fill = "#172ce6", color = "#172ce6",    # draw nodes
             stroke = 0.5)+
  scale_size_continuous(guide = "none", range = c(0.4, 4)) +    # scale for node size
  geom_point(data=unique_countries_first,aes(x = lon, y = lat, fill = count),
             shape = 23, alpha=1, color = "grey",size=0.75,#, fill = "#0dffff", color = "#0dffff"    # draw nodes##8917e6
             stroke = 0.3) +
  #scale_color_gradient(low = "white", high = "red")+
  scale_fill_gradient(low = "white", high = "red")+
  mapcoords + maptheme + theme_transp_overlay+
  theme(legend.position = c(0.5, 0.05), legend.direction = "horizontal")


p_nodes_Legend2


p_nodes_noLegend2 <- ggplot(unique_countries) +
  geom_point(data=unique_countries_co, aes(x = lon, y = lat, size = weight),
             shape = 21, fill = "#172ce6", color = "#172ce6",    # draw nodes
             stroke = 0.5)+
  scale_size_continuous(guide = "none", range = c(1, 5)) +    # scale for node size
  geom_point(data=unique_countries_first,aes(x = lon, y = lat, fill = count),
             shape = 23, alpha=1, color = "grey",size=1.1,
             stroke = 0.3) +
  scale_fill_gradient(low = "white", high = "red")+
  mapcoords + maptheme + theme_transp_overlay+
  theme(legend.position = "none")


p_nodes_noLegend2



p_nodes <- ggplot(unique_countries) +
  geom_point(data=unique_countries_co, aes(x = lon, y = lat, size = weight,shape='fill_co', fill='fill_co'),
             stroke = 0.5)+
  geom_point(data=unique_countries_first,aes(x = lon, y = lat, size = weight, shape='fill_first', fill='fill_first'),
             stroke = 0.3) +
  scale_size_continuous(guide = "none", range = c(0.4, 4)) +    # scale for node size
  scale_shape_manual(breaks = c('fill_co','fill_first'), values=c(21,23))+
  scale_fill_manual(breaks = c('fill_co','fill_first'), values=c("#172ce6","#0dffff"))+
  mapcoords + maptheme + theme_transp_overlay+
  theme(legend.position = c(0.5, 0.05), legend.direction = "horizontal")

p_nodes



p_nodes_for_legend <- ggplot(unique_countries) +
  geom_point(data=unique_countries_merged, aes(x = lon, y = lat, group=Authorship, size = weight, shape=Authorship, fill=Authorship, color=Authorship),
             stroke=0.5)+
  geom_point(data=unique_countries_first,aes(x = lon, y = lat, size = weight))+
  scale_size_continuous(guide = "none", range = c(0.4, 4)) +    # scale for node size
  scale_fill_manual(values=c("#172ce6", "#0dffff"))+
  scale_color_manual(values=c("#172ce6", "#0dffff"))+
  scale_shape_manual(values=c(21,23))+
  mapcoords + maptheme + theme_transp_overlay+
  theme(legend.position = c(0.5, 0.05), legend.direction = "horizontal", 
        legend.text = element_text(color="white"), 
        legend.title = element_text(color="white"), 
        legend.background = element_blank())


p_nodes_for_legend


#################################COLOR GRADIENT FOR FIRST AUTHORS

#This is just for copying the legend to arrange it better in powerpoint
p <- p_base +
  annotation_custom(ggplotGrob(p_edges), ymin = -74) +
  annotation_custom(ggplotGrob(p_nodes_Legend2), ymin= -74)
#annotation_custom(ggplotGrob(p_nodes), ymin = -74)

print(p)


######################################################
#####################FINAL FOR PAPER#################

#Without country circles
p <- p_base +
  annotation_custom(ggplotGrob(p_edges_dest_afr), ymin = -74) +
  annotation_custom(ggplotGrob(p_edges_both), ymin = -74) +
  annotation_custom(ggplotGrob(p_edges_ori_afr), ymin = -74)
  #annotation_custom(ggplotGrob(p_nodes_noLegend2), ymin= -74)


print(p)

# Export
png("plots/network_map_noLegend_2_colors_3CLASS_withoutCircles.jpg",width = 20, height = 10, units = "cm", pointsize = 3, res=500) 
print(p)
dev.off()


#3 separate
p1 <- p_base +
  annotation_custom(ggplotGrob(p_edges_dest_afr), ymin = -74)

p2 <- p_base +
  annotation_custom(ggplotGrob(p_edges_both), ymin = -74) 

p3 <- p_base +
  annotation_custom(ggplotGrob(p_edges_ori_afr), ymin = -74)
#annotation_custom(ggplotGrob(p_nodes_noLegend2), ymin= -74)


# print(p1)

# Export
png("plots/network_map_noLegend_2_colors_3CLASS_withoutCircles_OVERSEAS.jpg",width = 20, height = 10, units = "cm", pointsize = 3, res=500) 
print(p1)
dev.off()

png("plots/network_map_noLegend_2_colors_3CLASS_withoutCircles_BOTH.jpg",width = 20, height = 10, units = "cm", pointsize = 3, res=500) 
print(p2)
dev.off()

png("plots/network_map_noLegend_2_colors_3CLASS_withoutCircles_AFRICA.jpg",width = 20, height = 10, units = "cm", pointsize = 3, res=500) 
print(p3)
dev.off()





#With country circles
p <- p_base +
  annotation_custom(ggplotGrob(p_edges_dest_afr), ymin = -74) +
  annotation_custom(ggplotGrob(p_edges_both), ymin = -74) +
  annotation_custom(ggplotGrob(p_edges_ori_afr), ymin = -74) +
  annotation_custom(ggplotGrob(p_nodes_noLegend2), ymin= -74)


print(p)

# Export
png("plots/network_map_noLegend_2_colors_3CLASS_2.jpg",width = 20, height = 10, units = "cm", pointsize = 3, res=500) 
print(p)
dev.off()

#After export, this graph is cropped and pasted with on the legend in powerpoint found under the folder powerpoint