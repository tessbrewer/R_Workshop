#PCA example
library(vegan)
library(plyr)

results <- read.delim('~/Documents/munich/R_Workshop/Expression_data/results.txt')
submap <- read.delim('~/Documents/munich/R_Workshop/Expression_data/submap.txt')

#verify order
results <- results[order(row.names(results)), ]
submap <- submap[order(row.names(submap)), ]
table(rownames(submap)==rownames(results))

micro.dist <- vegdist(decostand(results, method="hellinger"), method="bray")
#This is a nested command - hellinger transform dataframe, then do bray-curtis dissimilarity  
micro.pcoa <- cmdscale(micro.dist, eig=TRUE)
#Creating pcoa
A <-(micro.pcoa$eig[1])/sum(micro.pcoa$eig) #1st axis
B <-(micro.pcoa$eig[2])/sum(micro.pcoa$eig) #2nd axis

adonis2(micro.dist~submap$type)
adonis2(micro.dist~submap$FACS_pool_color)

submap$Axis01PCOA<-micro.pcoa$points[,1]
submap$Axis02PCOA<-micro.pcoa$points[,2]
submap$sample <- row.names(submap)

find_hull <- function(submap_file) submap_file[chull(submap_file$Axis01PCOA, submap_file$Axis02PCOA),]
micro.hulls <- ddply(submap, "library", find_hull)

ggplot(submap, aes(x=Axis01PCOA, y=Axis02PCOA, label=library)) +
  geom_polygon(data = micro.hulls, aes(colour=library), alpha = 0)+
  ggtitle('XPPX library') +
  geom_point(aes(fill=FACS_pool_color), size=5, pch=21) +
  ggrepel::geom_label_repel(arrow = NULL) +
  xlab(paste("PCoA1", round(A, digits = 2))) +
  ylab(paste("PCoA2", round(B, digits = 2))) +
  scale_fill_manual(values = c('#cc0000', 'white'), name = 'FACS pool color') +
  theme_bw() +
  theme(legend.justification = c(0.025, 0.985), legend.position = c(0.025, 0.985)) + 
  theme(text = element_text(color="#666666",size=20,family="Avenir")) +
  theme(title= element_text(color="#666666", size=26, family="Avenir")) +
  theme(legend.text = element_text(family="Avenir",size=20))
