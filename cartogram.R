#https://r-charts.com/spatial/cartogram-ggplot2/
#https://r-graph-gallery.com/a-smooth-transition-between-chloropleth-and-cartogram.html


#getting specific countries
#https://stackoverflow.com/questions/27671710/how-to-color-selected-countries-in-wrld-simpl

# Please Ignore, specific to a bug in the gallery
# library(pacman)
# pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

# Load libraries
library(dplyr)        # data wrangling
library(cartogram)    # for the cartogram
library(ggplot2)      # to realize the plots
library(broom)        # from geospatial format to data frame
library(tweenr)       # to create transition dataframe between 2 states
library(gganimate)    # To realize the animation
library(maptools)     # world boundaries coordinates
library(viridis)      # for a nice color palette
library(sf)
library(ggpubr)
library(cowplot)
library(glue)
library(magick)
library(gtools)

# Get the shape file of Africa
data(wrld_simpl)
afr=wrld_simpl[wrld_simpl$REGION==2,]

# A basic representation
plot(afr)
st_crs(afr)
afr <- st_transform(st_as_sf(afr), crs = "+proj=chamb +lat_1=22 +lon_1=0 +lat_2=22 +lon_2=45 +lat_3=-22 +lon_3=22.5 +datum=WGS84 +type=crs")
st_crs(afr)


# construct a cartogram using the population in 2005
afr_cartogram <- cartogram_cont(afr, "POP2005", itermax=7)

# A basic representation
plot(afr_cartogram)

#better plots with ggplot
ggplot(afr_cartogram)+geom_sf(aes(fill=POP2005))


#######################
authors <- read_xlsx("data/AFILKScenarios_Literature_Database.xlsx", sheet="study_country")
#countries <- read.csv("C:/BRAVE/personal/Bwalya/network_mapping/country-capitals.csv", sep=",", header = TRUE)

authors <- as.data.frame(sapply(authors,gsub,pattern="The Netherlands",replacement="Netherlands"))
authors <- as.data.frame(sapply(authors,gsub,pattern="indonesia",replacement="Indonesia"))
authors <- as.data.frame(sapply(authors,gsub,pattern="Phillipines",replacement="Philippines"))
authors <- as.data.frame(sapply(authors,gsub,pattern="Ivory Coast",replacement="Côte d’Ivoire"))
authors <- as.data.frame(sapply(authors,gsub,pattern="ROC",replacement="Republic of Congo"))


#country_pairs <- data.frame(country1=character(),country2=character(),count=integer())
unique_countries <- data.frame(country=character(), count=integer())

for(i in 1:nrow(authors)){
  # i=2
  row_i <- as.character(authors[i,][!is.na(authors[i,])]) #gets for a given row only existing author countries, and ignores empty cells
  row_i <- unlist(strsplit(row_i, "[|]"))

  unique_country_i <- tabulate(as.factor(row_i))
  names(unique_country_i) <- levels(as.factor(row_i))
  for(j in names(unique_country_i)){
    if(j %in% unique_countries$country) unique_countries[which(unique_countries$country==j),"count"] <- unique_countries[which(unique_countries$country==j),"count"] + as.numeric(unique_country_i[j])
    else unique_countries <- rbind(unique_countries, data.frame(country=j,count=as.numeric(unique_country_i[j])))
  }
  # if(length(row_i)>=2){
  #   all_comb <- as.data.frame(t(combn(sort(row_i), 2)))
  #   names(all_comb) <- c("country1","country2")
  #   keep <- apply(all_comb, 1, function(x) length(unique(x[!is.na(x)])) != 1)
  #   all_comb <- all_comb[keep, ]
  #   all_comb <- all_comb %>% group_by_all %>% count
  #   names(all_comb)<-c("country1","country2","count")
  #   
  # }

  # existing_row <- as.numeric(rownames(plyr::match_df(country_pairs[,c(1,2)],all_comb[,c(1,2)])))

  #country_pairs <- rbind(country_pairs,all_comb)

}

range01 <- function(x){(x/max(x))}
unique_countries$weight <- range01(unique_countries$count)
##########################
names(unique_countries)[which(names(unique_countries)=="country")] <- "NAME"


# afr_selected <- afr[afr$NAME %in% unique_countries$NAME,]
#changing the name of Tanzania and other countries
unique_countries[unique_countries$NAME == "Tanzania",]$NAME <- "United Republic of Tanzania"
unique_countries[unique_countries$NAME == "Republic of Congo",]$NAME <- "Congo"
unique_countries[unique_countries$NAME == "DRC",]$NAME <- "Democratic Republic of the Congo"

unique_countries[grep("&", unique_countries$NAME),]$NAME<-"Sao Tome and Principe"

unique_countries[unique_countries$NAME == "Côte d’Ivoire",]$NAME <- "Cote d'Ivoire"

afr_authors <- merge(afr,unique_countries[,c("NAME","count","weight")],by="NAME",all.x=TRUE)

# afr_authors <- merge(afr,unique_countries[,c("NAME","count","weight")],by="NAME",all.x=TRUE)

afr_authors_final <- afr_authors%>%
  mutate_all(~replace(., is.na(.), 0))

afr_authors_final$weight_scaled <- afr_authors_final$weight*100+1

afr_authors_final$all_same <- 1
# afr_authors_final$`Number of authors` <- cut(afr_authors_final$count,breaks = c(5,10,20,))

# afr_cartogram_authors <- cartogram_cont(afr_authors_final, "POP2005", itermax=7)
afr_cartogram_authors <- cartogram_cont(afr_authors_final, "weight_scaled", itermax=10)
afr_cartogram_no_deformed <- cartogram_cont(afr_authors_final, "all_same", itermax=0)


##########!
#no internal borders

g_deformed <- ggplot()+
  #geom_sf(data=st_union(afr_cartogram_authors),color="black",cex=1, fill=NA)+
  geom_sf(data=afr_cartogram_authors, aes(fill=count), cex=1, color=NA)+
  geom_sf(data=st_buffer(st_union(st_buffer(afr_cartogram_authors,100000)),-100000),color="black",cex=0.5, fill=NA)+
  # scale_fill_gradientn(colours = c("#FFFFFF00",heat.colors(n = 10,alpha = 1,rev = TRUE))[-c(2:5)], name = "Authorship")+
  #scale_fill_gradient(low = "white", high = "#CB454A", name = "Authorship")+
  # scale_fill_gradient2(low = "white", mid="yellow",
  #                      high = "red", breaks = c(-10, 0, 1, 20, 50), name = "Authorship")+ 
  scale_fill_gradientn(colors=c("white","yellow","red"), values=scales::rescale(c(-1,-0.9,1)), limits=c(0,13), name = "Authorship")+
  #scale_colour_identity()+
  theme_classic()+
  annotate("text",-Inf,Inf,label=" B", hjust=-0.5, vjust=1.5)+
  theme(legend.position = c(0.18,0.3),
        legend.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
  # guides(fill=guide_legend(title="Author count"))
 # scale_fill_continuous(name = "Author count")

g_deformed

g_not_deformed <- ggplot()+
  geom_sf(data=afr_cartogram_no_deformed, aes(fill=count), cex=1, color=NA)+
  geom_sf(data=st_buffer(st_union(st_buffer(afr_cartogram_no_deformed ,100000)),-100000),color="black",cex=0.5, fill=NA)+
  scale_fill_gradientn(colors=c("white","yellow","red"), values=scales::rescale(c(-1,-0.9,1)), limits=c(0,13), name = "Authorship")+
  theme_classic()+
  annotate("text",-Inf,Inf,label=" A", hjust=-0.5, vjust=1.5)+
  theme(panel.border = element_rect(colour = "black", fill=NA),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggarrange(g_not_deformed,g_deformed,align = "h", widths = c(1,1),heights=c(0.5,0.5))

#arranging
ggsave("plots/cartogram.png",ggarrange(g_not_deformed,g_deformed,align = "h", widths = c(1,1),heights=c(0.5,0.5)))
# plot_grid(g1,g3, align="h")

##########!
#with internal borders

afr_cartogram_authors <- afr_cartogram_authors %>% 
  mutate(borders = ifelse(count>0,"#000000", NA))

afr_cartogram_no_deformed <- afr_cartogram_no_deformed %>% 
  mutate(borders = ifelse(count>0,"#000000", NA))
g_deformed <- ggplot()+
  #geom_sf(data=st_union(afr_cartogram_authors),color="black",cex=1, fill=NA)+
  geom_sf(data=afr_cartogram_authors, aes(fill=count, color=borders), cex=0.8)+
  geom_sf(data=st_buffer(st_union(st_buffer(afr_cartogram_authors,100000)),-100000),color="black",cex=0.8, fill=NA)+
  # scale_fill_gradientn(colours = c("#FFFFFF00",heat.colors(n = 10,alpha = 1,rev = TRUE))[-c(2:5)], name = "Authorship")+
  #scale_fill_gradient(low = "white", high = "#CB454A", name = "Authorship")+
  # scale_fill_gradient2(low = "white", mid="yellow",
  #                      high = "red", breaks = c(-10, 0, 1, 20, 50), name = "Authorship")+ 
  scale_fill_gradientn(colors=c("white","yellow","red"), values=scales::rescale(c(-1,-0.9,1)), limits=c(0,13), name = "Authorship")+
  scale_colour_identity()+
  theme_classic()+
  annotate("text",-Inf,Inf,label=" B", hjust=-0.5, vjust=1.5)+
  theme(legend.position = c(0.18,0.3),
        legend.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
# guides(fill=guide_legend(title="Author count"))
# scale_fill_continuous(name = "Author count")

g_deformed

g_not_deformed <- ggplot()+
  geom_sf(data=afr_cartogram_no_deformed, aes(fill=count, color=borders), cex=0.8)+
  geom_sf(data=st_buffer(st_union(st_buffer(afr_cartogram_no_deformed ,100000)),-100000),color="black",cex=0.8, fill=NA)+
  scale_fill_gradientn(colors=c("white","yellow","red"), values=scales::rescale(c(-1,-0.9,1)), limits=c(0,13), name = "Authorship")+
  scale_colour_identity()+
  theme_classic()+
  annotate("text",-Inf,Inf,label=" A", hjust=-0.5, vjust=1.5)+
  theme(panel.border = element_rect(colour = "black", fill=NA),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggarrange(g_not_deformed,g_deformed,align = "h", widths = c(1,1),heights=c(0.5,0.5))

#arranging
ggsave("plots/cartogram_with_borders.png",ggarrange(g_not_deformed,g_deformed,align = "h", widths = c(1,1),heights=c(0.5,0.5)))
# plot_grid(g1,g3, align="h")

#####################!
#with internal borders less tick border lines

afr_cartogram_authors <- afr_cartogram_authors %>% 
  mutate(borders = ifelse(count>0,"#000000", NA))

afr_cartogram_no_deformed <- afr_cartogram_no_deformed %>% 
  mutate(borders = ifelse(count>0,"#000000", NA))
g_deformed <- ggplot()+
  #geom_sf(data=st_union(afr_cartogram_authors),color="black",cex=1, fill=NA)+
  geom_sf(data=afr_cartogram_authors, aes(fill=count, color=borders), cex=0.3)+
  geom_sf(data=st_buffer(st_union(st_buffer(afr_cartogram_authors,100000)),-100000),color="black",cex=0.5, fill=NA)+
  # scale_fill_gradientn(colours = c("#FFFFFF00",heat.colors(n = 10,alpha = 1,rev = TRUE))[-c(2:5)], name = "Authorship")+
  #scale_fill_gradient(low = "white", high = "#CB454A", name = "Authorship")+
  # scale_fill_gradient2(low = "white", mid="yellow",
  #                      high = "red", breaks = c(-10, 0, 1, 20, 50), name = "Authorship")+ 
  scale_fill_gradientn(colors=c("white","yellow","red"), values=scales::rescale(c(-1,-0.9,1)), limits=c(0,13), name = "Authorship")+
  scale_colour_identity()+
  theme_classic()+
  annotate("text",-Inf,Inf,label=" B", hjust=-0.5, vjust=1.5)+
  theme(legend.position = c(0.18,0.3),
        legend.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
# guides(fill=guide_legend(title="Author count"))
# scale_fill_continuous(name = "Author count")

g_deformed

g_not_deformed <- ggplot()+
  geom_sf(data=afr_cartogram_no_deformed, aes(fill=count, color=borders), cex=0.3)+
  geom_sf(data=st_buffer(st_union(st_buffer(afr_cartogram_no_deformed ,100000)),-100000),color="black",cex=0.5, fill=NA)+
  scale_fill_gradientn(colors=c("white","yellow","red"), values=scales::rescale(c(-1,-0.9,1)), limits=c(0,13), name = "Authorship")+
  scale_colour_identity()+
  theme_classic()+
  annotate("text",-Inf,Inf,label=" A", hjust=-0.5, vjust=1.5)+
  theme(panel.border = element_rect(colour = "black", fill=NA),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggarrange(g_not_deformed,g_deformed,align = "h", widths = c(1,1),heights=c(0.5,0.5))

#arranging
ggsave("plots/cartogram_with_borders_thin.png",ggarrange(g_not_deformed,g_deformed,align = "h", widths = c(1,1),heights=c(0.5,0.5)))
# plot_grid(g1,g3, align="h")


####Let's create the animation
#Customized function
cartogram_cust <- function(iteration){
  afr_cartogram_authors <- cartogram_cont(afr_authors_final, "weight_scaled", itermax=iteration)

  afr_cartogram_authors <- afr_cartogram_authors %>%
    mutate(borders = ifelse(count>0,"#000000", NA))

  g_deformed <- ggplot()+
    geom_sf(data=afr_cartogram_authors, aes(fill=count, color=borders), cex=0.3)+
    geom_sf(data=st_buffer(st_union(st_buffer(afr_cartogram_authors,100000)),-100000),color="black",cex=0.5, fill=NA)+
    scale_fill_gradientn(colors=c("white","yellow","red"), values=scales::rescale(c(-1,-0.9,1)), limits=c(0,13), name = "Authorship")+
    scale_colour_identity()+
    theme_classic()+
    theme(legend.position = c(0.18,0.3),
          legend.background = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())

  ggsave(glue("plots/for_magick2/image_{iteration}.png"),g_deformed, width = 1000, height = 1000, units = "px")
}


cartogram_cust_rev <- function(iteration){
  afr_cartogram_authors <- cartogram_cont(afr_authors_final, "weight_scaled", itermax=21-iteration)
  
  afr_cartogram_authors <- afr_cartogram_authors %>% 
    mutate(borders = ifelse(count>0,"#000000", NA))
  
  g_deformed <- ggplot()+
    geom_sf(data=afr_cartogram_authors, aes(fill=count, color=borders), cex=0.3)+
    geom_sf(data=st_buffer(st_union(st_buffer(afr_cartogram_authors,100000)),-100000),color="black",cex=0.5, fill=NA)+
    scale_fill_gradientn(colors=c("white","yellow","red"), values=scales::rescale(c(-1,-0.9,1)), limits=c(0,13), name = "Authorship")+
    scale_colour_identity()+
    theme_classic()+
    theme(legend.position = c(0.18,0.3),
          legend.background = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  # 
  # g_deformed <- ggplot(afr_cartogram_authors)+
  #   geom_sf(aes(fill=count))+
  #   scale_fill_gradientn(colours = heat.colors(n = 10,alpha = 0.5,rev = TRUE), name = "Authorship")+
  #   theme_classic()+
  #   theme(legend.position = c(0.18,0.3),
  #         legend.background = element_blank(),
  #         panel.border = element_rect(colour = "black", fill=NA),
  #         axis.title.x = element_blank(),
  #         axis.title.y = element_blank())
  
  ggsave(glue("plots/for_magick2/image_{iteration}.png"),g_deformed, width = 1000, height = 1000, units = "px")
}
for(i in 1:10){
  cartogram_cust(i)
}

for(i in 11:20){
  cartogram_cust_rev(i)
}

#setting working directory
#setwd("plots/for_magick2")
frames = c()
images = mixedsort(list.files(path="plots/for_magick2",pattern = "png"))

for (i in length(images):1) {
  x = image_read(file.path("plots/for_magick2",images[i]))
  x = image_scale(x, "1000")
  c(x, frames) -> frames
}
animation = image_animate(frames, fps = 10)
image_write(animation, "plots/cartogram_animation.gif")

