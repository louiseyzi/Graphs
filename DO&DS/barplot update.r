library(RColorBrewer)
library(export)

d1 <- read.csv("Diplomatic-Stance.csv")

n <- ncol(d1)

col_names <- colnames(d1)

col_names <- gsub(".Diplomatic.Stance","",  col_names)

a <- strsplit(col_names[-1], split = "\\.")

a <- do.call("rbind",a)

ds <- NULL
for(i in 1:nrow(a)) {
  
  ds <- rbind(ds,   cbind(d1$Turn, a[i,1] ,a[i,2], as.character(d1[,i+1]) ) )
  
}

ds <- data.frame(ds)
ds[,1] <- as.integer(as.character(ds[,1]))

colnames(ds)  <- c("Turn",  "Civ1"  ,"Civ2"  ,"Value")

DS <- ds

d1 <- read.csv("Diplomatic-Opinion.csv")

n <- ncol(d1)

col_names <- colnames(d1)

col_names <- gsub(".Diplomatic.Opinion","",  col_names)

a <- strsplit(col_names[-1], split = "\\.")

a <- do.call("rbind",a)

do <- NULL
for(i in 1:nrow(a)) {
  
  do <- rbind(do,   cbind(d1$Turn, a[i,1] ,a[i,2], as.character(d1[,i+1]) ) )
  
}

do <- data.frame(do)
do[,1] <- as.integer(as.character(do[,1]))

colnames(do)  <- c("Turn",  "Civ1"  ,"Civ2"  ,"Value")

DO <- do


plot_it <- function(country, dataname) {
  
  if(dataname == "Diplomatic Stance") DO <- DS
  if(dataname == "Diplomatic Opinion") DO <- DO
  
  uniquevalues <- c(as.character(unique(DO$Value)))
  
  length_uniquevalues <- length(uniquevalues)
  
  DO2 <- subset(DO,  Civ1 == country)
  
  uns <- as.character(unique(DO2$Civ2))
  
  res <- list()
  
  mat <- NULL
  
  for(u in uns) {
  
  DO3 <- subset(DO2, Civ2 == u)
  
  
  turns <- rep("none",500)
  turns[DO3$Turn] <- as.character(DO3$Value)
  
  mat <- rbind(mat, turns)
  }
 
   a <- rep(uniquevalues, 500)
  
   mat2 <- matrix(0, nrow(mat), length(a))
   
  for(i in 1:nrow(mat)) {
    
    for(j in 1:ncol(mat)) {
      
       pos <-   (length_uniquevalues*(j-1)+1):(length_uniquevalues*j)
       p <- pos[match(mat[i,j], uniquevalues)]
       mat2[i,p] <- 1
      
    }
  }
  
   colnames(mat2) <- a
   rownames(mat2) <- uns
   

  cols <- brewer.pal(name="RdYlGn", length_uniquevalues)
  
  if(dataname == "Diplomatic Stance") {
    cols2 <- cols[c(5,1,3,6,4,7,2)]  }

  if(dataname == "Diplomatic Opinion")  {
    cols2 <- cols[c(5,4,3,6,7,8,2,1)]

  }

   par(xpd = T, mar = par()$mar + c(0,0,0,7))
  
   barplot(t(mat2), col = cols2, border = NA, horiz = TRUE,  space = 1.2,
           xlab = "Turn",
           main = paste0(dataname, " of ",country," Toward other Countries"))
   
   if(dataname == "Diplomatic Opinion") nm <- c("ally","friend","favorable","neutral","None","competitor","enemy", "archnemesis")
   else nm <- c("friendly","afraid","neutral","guarded","None","deceptive","hostile")
   legend(500,8, nm, pch = 15, col = as.character(cols), bty = "n")
   
   par(mar=c(5, 4, 4, 2) + 0.1)
   graph2tif(file = paste0(dataname, " of ",country," Toward other Countries"),
             width = 8, height = 6)
   
} 


for(dataname in c("Diplomatic Stance", "Diplomatic Opinion")) {
  for (country in as.character(unique(DS$Civ1))) {
    
    plot_it(country, dataname)
    
  }
}




