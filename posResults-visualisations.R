setwd("~/Desktop/parser-comparions-pycon-dublin-2016")

library(data.table)
library(dplyr)
library(dtplyr)
library(ggplot2)
library(tidyr)
library(forcats)
library(ggthemes)
library(scales)
library(gridExtra)

#Read in the data
posData <- fread("posTimes.csv")

#text.blob doesn't return puncs
posData <- posData[,textblob.puncs := pattern.puncs]


#Make long & join back
posTimes <- data.table(gather(posData[,c(1:8),with=F], parser, time, nlkt.time:stan.time, factor_key=TRUE))
posPuncs <- data.table(gather(posData[,c(1,2,3,9:13),with=F], parser, puncs, nlkt.puncs:stan.puncs, factor_key = TRUE))
posData2 <- cbind(posTimes[,c(1:5),with=F],posPuncs[,c(5:5),with=F])

#Rename the parsers
posData2 <- posData2 %>%
  mutate(parser = fct_recode(parser,
                              "nltk"    = "nlkt.time",
                              "spacy"      = "spacy.time",
                              "pattern" = "pattern.time",
                              "texblob" = "textblob.time",
                              "stanford"        = "stan.time"))

setnames(posData2,"sampleSize","sampleRows")

#At 1000 - row 74 contains an outlier for textblob, At 10000 row 96 contains outlier for pattern, 98 contains lot of words in 
#comparison to other samples.
#Remove outliers

posData2 <- posData2[!index %in% c(74,96),]

#Create a theme
#Create theme for ggplots
theme_present <- function(){
  theme(
    axis.text.x=element_text(size=16),
    axis.text.y=element_text(size=16),
    axis.title.x=element_text(size=20),
    axis.title.y=element_text(size=20),
    panel.background = element_blank(),
    title=element_text(size=20),
    legend.position="bottom",
    legend.text = element_text(size=14))
  
}

theme_present2 <- function(){
  theme(
    axis.text.x=element_text(size=14),
    axis.text.y=element_text(size=14),
    axis.title.x=element_text(size=14),
    axis.title.y=element_text(size=14),
    panel.background = element_blank(),
    title=element_text(size=14),
    legend.position="bottom",
    legend.text = element_text(size=10))
  
}





#Plot 1 with them all 
plot1 <- ggplot(posData2[sampleRows==10,], aes(x=totalWords,y=time)) + 
  geom_jitter(aes(colour=parser),size=3,height=3) + 
  scale_x_continuous(labels = comma) +
  ggtitle("Total time (secs) increasing with total words parsed") +
  theme_present() 
plot1

#Plot 2 without standford
plot2 <- ggplot(posData2[sampleRows==10 & parser != "stanford",], aes(x=totalWords,y=time)) + 
  geom_point(aes(colour=parser),size=3) +
  geom_smooth(method="lm",se=FALSE,aes(color=parser)) +
  scale_x_continuous(labels = comma) +
  ggtitle("Total time (secs) increasing with total words parsed") +
  theme_present() 
plot2

#Plot3 across all sample sizes
sampleSizes<-c(10,100,1000,10000)
p=list()

for(i in 1:length(sampleSizes)){
  p[[i]] <- ggplot(posData2[sampleRows == sampleSizes[i] & parser != "stanford",], aes(x=totalWords,y=time)) + 
    geom_point(aes(colour=parser),size=3) +
    geom_smooth(method="lm",se=FALSE,aes(color=parser)) +
    scale_x_continuous(labels = comma) + 
    theme_present2() 
}

plot3 <- do.call(grid.arrange,c(p,top="Total time (secs) incresaing broken by sample sizes"))

averagePOSTime <- posData2 %>% group_by(parser,sampleRows) %>% 
  summarise(mean.time = weighted.mean(time,totalWords/sum(totalWords),na.rm=T,)) %>%
  spread(sampleRows,mean.time)

speedGain <- data.frame(lapply(averagePOSTime[,(2:5),with=F], function(X) X/X[1]))
speedGain$average = round(rowMeans(speedGain,na.rm=T),2)
speedGain <- cbind(averagePOSTime$parse,speedGain)



