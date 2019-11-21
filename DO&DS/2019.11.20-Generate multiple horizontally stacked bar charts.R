library(export)
library(stringr)
library(ggplot2)
DO <- read.table(file = "/Users/lyz/Desktop/DO.txt",header = TRUE)
DS <- read.delim("/Users/lyz/Desktop/DS.txt")


str(DO)
unique(DO$Value)
unique(DO$Civ1)
unique(DO$Civ2)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  return(datac)
}

DO$Turn <- as.numeric(DO$Turn)

names <- unique(DO$Civ1)
for (i in 1:length(names)) {
  name.i <- names[i]
  DO.i <- DO[which(DO$Civ1==name.i),]
  tx.i <- str_c("Diplomatic Opinions of ",name.i," Toward other Countries")
  ab1.i <- summarySE(DO.i,measurevar="Turn",groupvars=c("Civ2","Value"))
  ab2.i <- ddply(ab1.i,"Civ2",transform,percent_weight=Turn/sum(Turn)*100)
  a <- ggplot(ab2.i,aes(x=Civ2,y=percent_weight,fill=Value))+geom_bar(stat = "identity",width = 0.5)+coord_flip()+
    scale_fill_brewer(palette="RdYlGn")+ylab("Percent")+xlab("Country")+ggtitle(tx.i)
  b <- a+theme_bw()+theme(panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank())
  filename <- str_c(tx.i,"_percent.tif")
  graph2tif(b,filename)
}

names <- unique(DO$Civ1)
for (i in 1:length(names)) {
  name.i <- names[i]
  DO.i <- DO[which(DO$Civ1==name.i),]
  tx.i <- str_c("Diplomatic Opinions of ",name.i," Toward other Countries")
  ab1.i <- summarySE(DO.i,measurevar="Turn",groupvars=c("Civ2","Value"))

  a <- ggplot(ab1.i,aes(x=Civ2,y=Turn,fill=Value))+geom_bar(stat = "identity",width = 0.5)+coord_flip()+
    scale_fill_brewer(palette="RdYlGn")+ylab("Turn")+xlab("Country")+ggtitle(tx.i)
  b <- a+theme_bw()+theme(panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank())
  filename <- str_c(tx.i,"_identity.tif")
  graph2tif(b,filename)
}

names <- unique(DS$Civ1)
for (i in 1:length(names)) {
  name.i <- names[i]
  DS.i <- DS[which(DS$Civ1==name.i),]
  tx.i <- str_c("Diplomatic Stance of ",name.i," Toward other Countries")
  ab1.i <- summarySE(DS.i,measurevar="Turn",groupvars=c("Civ2","Value"))
  
  a <- ggplot(ab1.i,aes(x=Civ2,y=Turn,fill=Value))+geom_bar(stat = "identity",width = 0.5)+coord_flip()+
    scale_fill_brewer(palette="RdYlGn")+ylab("Turn")+xlab("Country")+ggtitle(tx.i)
  b <- a+theme_bw()+theme(panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank())
  filename <- str_c(tx.i,"_identity.tif")
  graph2tif(b,filename)
}


DS$Turn <- as.numeric(DS$Turn)
names <- unique(DS$Civ1)
for (i in 1:length(names)) {
  name.i <- names[i]
  DS.i <- DS[which(DS$Civ1==name.i),]
  tx.i <- str_c("Diplomatic Stance of ",name.i," Toward other Countries")
  ab1.i <- summarySE(DS.i,measurevar="Turn",groupvars=c("Civ2","Value"))
  ab2.i <- ddply(ab1.i,"Civ2",transform,percent_weight=Turn/sum(Turn)*100)
  a <- ggplot(ab2.i,aes(x=Civ2,y=percent_weight,fill=Value))+geom_bar(stat = "identity",width = 0.5)+coord_flip()+
    scale_fill_brewer(palette="RdYlGn")+ylab("Precent")+xlab("Country")+ggtitle(tx.i)
  b <- a+theme_bw()+theme(panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank())
  filename <- str_c(tx.i,"_percent.tif")
  graph2tif(b,filename)
}
