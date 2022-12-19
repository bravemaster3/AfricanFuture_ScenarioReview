library(readxl)
library(dplyr)
library(dplyr)
library(purrr)
library(igraph)
library(ggplot2)
library(ggraph)
library(ggmap)
library(ggpubr)
library(graphics)
library(tidyverse)


#hack for adding a duplicate axis #https://github.com/tidyverse/ggplot2/issues/3171
guide_axis_label_trans <- function(label_trans = identity, ...) {
  axis_guide <- guide_axis(...)
  axis_guide$label_trans <- rlang::as_function(label_trans)
  class(axis_guide) <- c("guide_axis_trans", class(axis_guide))
  axis_guide
}

guide_train.guide_axis_trans <- function(x, ...) {
  trained <- NextMethod()
  trained$key$.label <- x$label_trans(trained$key$.label)
  trained
}

#reading the table
parameters <- read_xlsx("data/AFILKScenarios_Literature_Database.xlsx", sheet="parameters")

parameters[is.na(parameters)] <- 0
ncol(parameters)

new_par <- as.data.frame(matrix(nrow=ncol(parameters),ncol=ncol(parameters)))
colnames(new_par) <- colnames(parameters)
rownames(new_par) <- colnames(parameters)

for(i in 1:nrow(new_par)){
  rowname_i <- rownames(new_par)[i]
  for(j in 1:ncol(new_par)){
    colname_j <- colnames(new_par)[j]
    par_sub <- parameters[,unique(c(rowname_i,colname_j))]
    par_sub$sub<- rowSums(par_sub[,unique(c(rowname_i,colname_j))])
    
    new_par[i,j] <- sum(par_sub$sub==2)
    
  }
}




heatmap(as.matrix(new_par))


dat2 <- new_par %>%
  rownames_to_column('Var1') %>%
  gather(Var2, Overlap, -Var1)

dat3 <- new_par %>%
  rownames_to_column('Var1') %>%
  gather(Var2, Overlap, -Var1) %>% 
  group_by(Var2) %>%
  mutate(total_overlap = sum(Overlap))


dat3_sort <- dat3[order(dat3$total_overlap),]

dat3$Var1 <- factor(dat3$Var1, levels = unique(dat3_sort$Var2))
dat3$Var2 <- factor(dat3$Var2, levels = unique(dat3_sort$Var2))

correct_order <- unique(dat3_sort$Var2)


write.table(new_par, "data/parameters_parameters.csv", row.names = TRUE, col.names = TRUE, sep=",")

## plot data
ggplot(dat2, aes(Var1, Var2)) +
  geom_tile(aes(fill = Overlap)) +
  #geom_text(aes(label = round(value, 1))) +
  scale_fill_gradient(low = "white", high = "red")+
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  guides(x.sec = guide_axis_label_trans(~""))

ggsave("plots/parameters.png", width=20, height=15, unit="cm", dpi=300)




ggplot(dat2, aes(Var1, Var2)) +
  geom_tile(aes(fill = Overlap)) +
  #geom_text(aes(label = round(value, 1))) +
  scale_fill_gradient(low = "white", high = "red")+
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))+
  guides(x.sec = guide_axis_label_trans(~""))

ggsave("plots/parameters2.png", width=20, height=15, unit="cm", dpi=300)


######ORDERED
## plot data
ggplot(dat3, aes(Var1, Var2)) +
  geom_tile(aes(fill = Overlap)) +
  #geom_text(aes(label = round(value, 1))) +
  scale_fill_gradient(low = "white", high = "red")+
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        panel.border = element_rect(fill=NA, color = "black"))+
  guides(x.sec = guide_axis_label_trans(~""))

ggsave("plots/parameters_ordered_bentX.png", width=20, height=15, unit="cm", dpi=300)


ggplot(dat3, aes(Var1, Var2)) +
  geom_tile(aes(fill = Overlap)) +
  #geom_text(aes(label = round(value, 1))) +
  scale_fill_gradient(low = "white", high = "red")+
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust=1),
        panel.border = element_rect(fill=NA, color = "black"))+
  guides(x.sec = guide_axis_label_trans(~""))

ggsave("plots/parameters_ordered.png", width=20, height=15, unit="cm", dpi=300)

#Removing one half of the graph...
##Only one side of the diagonal...

#UPPER DIAGONAL

new_par2 <- as.data.frame(matrix(nrow=length(correct_order),ncol=length(correct_order)))
colnames(new_par2) <- correct_order
rownames(new_par2) <- correct_order

for(i in 1:nrow(new_par2)){
  rowname_i <- rownames(new_par2)[i]
  for(j in 1:ncol(new_par2)){
    colname_j <- colnames(new_par2)[j]
    par_sub <- parameters[,unique(c(rowname_i,colname_j))]
    par_sub$sub<- rowSums(par_sub[,unique(c(rowname_i,colname_j))])

    if(i<j) new_par2[i,j] <- sum(par_sub$sub==2)
    if(i==j) new_par2[i,j] <- 0
    if(i>j) new_par2[i,j] <- NA

  }
}

dat2 <- new_par2 %>%
  rownames_to_column('Var1') %>%
  gather(Var2, Overlap, -Var1)

dat2 <- dat2[-which(is.na(dat2$Overlap)),]

dat3 <- new_par2 %>%
  rownames_to_column('Var1') %>%
  gather(Var2, Overlap, -Var1) %>% 
  group_by(Var2) %>%
  mutate(total_overlap = sum(Overlap))

dat3 <- dat3[-which(is.na(dat3$Overlap)),]

# dat3_sort <- dat3[order(dat3$total_overlap),]

dat3$Var1 <- factor(dat3$Var1, levels = unique(dat3_sort$Var2))
dat3$Var2 <- factor(dat3$Var2, levels = unique(dat3_sort$Var2))


#dat3_half <- dat3

# for(i in dat3_half$Var1){
#   for(j in dat3_half$Var2){
#     dat3_half[j,i] <- NA
#   }
# }
## plot data
ggplot(dat3, aes(Var1, Var2)) +
  geom_tile(aes(fill = Overlap)) +
  #geom_text(aes(label = round(value, 1))) +
  scale_fill_gradient(low = "white", high = "red")+
  theme_classic()+
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        panel.border = element_rect(fill=NA, color = "black"))+
  guides(x.sec = guide_axis_label_trans(~""))

ggsave("plots/parameters_ordered_bentX_one_side_upper.png", width=20, height=15, unit="cm", dpi=300)




#LOWER DIAGONAL

new_par2 <- as.data.frame(matrix(nrow=length(correct_order),ncol=length(correct_order)))
colnames(new_par2) <- correct_order
rownames(new_par2) <- correct_order

for(i in 1:nrow(new_par2)){
  rowname_i <- rownames(new_par2)[i]
  for(j in 1:ncol(new_par2)){
    colname_j <- colnames(new_par2)[j]
    par_sub <- parameters[,unique(c(rowname_i,colname_j))]
    par_sub$sub<- rowSums(par_sub[,unique(c(rowname_i,colname_j))])
    
    if(i>j) new_par2[i,j] <- sum(par_sub$sub==2)
    if(i==j) new_par2[i,j] <- 0
    if(i<j) new_par2[i,j] <- NA
    
  }
}

dat2 <- new_par2 %>%
  rownames_to_column('Var1') %>%
  gather(Var2, Overlap, -Var1)

dat2 <- dat2[-which(is.na(dat2$Overlap)),]

dat3 <- new_par2 %>%
  rownames_to_column('Var1') %>%
  gather(Var2, Overlap, -Var1) %>% 
  group_by(Var2) %>%
  mutate(total_overlap = sum(Overlap))

dat3 <- dat3[-which(is.na(dat3$Overlap)),]

# dat3_sort <- dat3[order(dat3$total_overlap),]

dat3$Var1 <- factor(dat3$Var1, levels = unique(dat3_sort$Var2))
dat3$Var2 <- factor(dat3$Var2, levels = unique(dat3_sort$Var2))


#dat3_half <- dat3

# for(i in dat3_half$Var1){
#   for(j in dat3_half$Var2){
#     dat3_half[j,i] <- NA
#   }
# }
## plot data
ggplot(dat3, aes(Var1, Var2)) +
  geom_tile(aes(fill = Overlap)) +
  #geom_text(aes(label = round(value, 1))) +
  scale_fill_gradient(low = "white", high = "red")+
  theme_classic()+
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        panel.border = element_rect(fill=NA, color = "black"))+
  guides(x.sec = guide_axis_label_trans(~""))

ggsave("plots/parameters_ordered_bentX_one_side_lower.png", width=20, height=15, unit="cm", dpi=300)
