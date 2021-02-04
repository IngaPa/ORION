# stat for ORION poster presentation

# stat behind ORION Podcast



# per country N of downloads
    t <- read.delim("C:/Users/ipatarc/Desktop/ORION_countries_stat.txt", 
                    he=T,stringsAsFactors = F)
    
    
    stat_per_region <- tapply(t$Total.Downloads,t$Region,sum)
    100*stat_per_region/sum(t$Total.Downloads)
    
    png("C:/Users/ipatarc/Desktop/ORION_regions_stat.png",height =825 ,width = 900)
    pie(stat_per_region, cex=3)
    dev.off()
    
    t$country[which(t$country=="United States")] <- "USA"
    t$country[t$country=="United Kingdom"] <- "UK"
    
    stat_per_country <- tapply(t$Total.Downloads,t$country,sum)
    TOP10 <- sort(stat_per_country,decreasing = T)[1:10]
    100*TOP10/sum(t$Total.Downloads)

# per region N of downloads
    png("C:/Users/ipatarc/Desktop/ORION_countries_stat.png",height =825 ,width = 900)
    pie(TOP10, cex=3)
    dev.off()
    
    pie(TOP10, cex=2)





#########################
# per podcast stat
library(ggplot2)
library("reshape2")
library(stringr)


download.stat <- read.delim("C:/Users/ipatarc/Desktop/downloads_stats.txt", 
                he=T,stringsAsFactors = F)

# formating date of release
download.stat$Release.Date <- as.Date(download.stat$Release.Date,
                                      format='%m/%d/%Y')


download.stat <- download.stat[order(download.stat$Release.Date),]
# eding N of episode
download.stat$EpisodeN <- 1:nrow(download.stat)


download.stat <- download.stat[order(download.stat$Downloads,
                                     decreasing = T),]



plot.df <- (head(download.stat,10))


# per episode stat
png("C:/Users/ipatarc/Desktop/ORION_perepisode_stat.png",
    height =825 ,
    width = 1200)

ggplot(download.stat,aes(x=EpisodeN,
                         y=Downloads,fill=Downloads,
                         order=Downloads),col="orange") +  
  stat_summary(fun.y=mean,position="dodge",geom="bar") +  
  ylab("Number of downloads\n") +
  xlab("\n Episode") + 
  scale_fill_gradient("Downloads",
                      low = "lightyellow", high = "red")+  
  
  theme_bw()+
  
  geom_text(data = plot.df, 
            size=7,
            aes( y = Downloads - 85,
                 label = paste(str_extract(plot.df$Episode,".{33}"),"..."),
            srt=90))+
  theme(text = element_text(size=30,face="bold"),
        axis.text =element_text(size=30,face="bold"),
        legend.text = element_text(size=20,face="bold"))

dev.off()




#########################
# per podcast stat - per open science category

install.packages("pheatmap")
library(pheatmap)


ORION.all.stat <- read.delim("C:/Users/ipatarc/Desktop/ORION_stat_Podcast.txt", 
                            he=T,stringsAsFactors = F)



ORION.all.stat$X.2[str_detect(ORION.all.stat$X.2,"Evaluation")] <- "Open Science Evaluation"
colnames(ORION.all.stat) <- c("Episode","Country","Region","Category","Category2","Release.Date", "Downloads")


cols = colorRampPalette(c("lightyellow","red" ))(6)

  pheatmap(table(ORION.all.stat$Category,
                 ORION.all.stat$Country),cluster_rows =  F,
           cluster_cols = F,
           color=cols,
           fontsize_row = 16,
           fontsize_col = 15)

