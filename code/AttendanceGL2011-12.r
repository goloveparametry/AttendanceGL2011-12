# Required packages
require("reshape")
require("ggplot2")
require("RColorBrewer")

# Load csv with Gambrinus Liga 2011/12 results
gl.result <- read.csv("data/GL2012_results.csv")

# Rename columns
colnames(gl.result) <- c("link","id","home","vis","scoreFt","scoreHt","att")

# Agregate mean attendance by team
aggregate(gl.result,list(gl.result$home), mean)

# Agregate sum attendance by team
aggregate(gl.result,list(gl.result$home), sum)

# Get League round from match ID
gl.result$rnd <- substr(gl.result$id,4,5)

# Load csv with Gambrinus Liga 2011/12 ranking (position after each round)
gl.ranking <- read.csv("data/GL2012_ranking.txt")

# Table transformation
gl.ranking <- melt(gl.ranking)

# Rename columns
colnames(gl.ranking) <- c("team","Xrnd","rank")

# Join key attributes as teamXrnd
gl.result$k1 <- paste(gl.result$home,"X",as.integer(gl.result$rnd)-1,sep="")
gl.ranking$k2 <- paste(gl.ranking$team,gl.ranking$Xrnd,sep="")

# Join both tables 
gl.att <- merge(gl.result,gl.ranking, 
	by.x="k1",
	by.y="k2",
	all.x=T)

# Subset gl.att 
gl.att <- subset(gl.att, select=c("id","home","vis","rnd","rank","att"))

# Table with stadiums capacity (hline)
hline.data <- data.frame(a = 1:2, b = c(18872,11700))

# Visualize Sparta Praha
gid = "Sparta Praha"
g <- ggplot( subset(gl.att , home == gid), aes(rnd,att,label=vis) ) 
g + geom_bar(aes(fill=as.factor(rank))) + scale_fill_manual(
	values = c(rev(brewer.pal(9,"Greens"))[as.integer(levels(as.factor(gl.att [gl.att $home == gid, 5])))],"gray"), 
	name="League Pos") + geom_hline(aes(yintercept = b), hline.data[1,]) + geom_text(hjust=1.1, size=4, angle = 90) + scale_x_discrete("League Round") + scale_y_continuous("Attendance") + opts(title=paste("Attendance of ",gid," home matches GL 2011/12",sep=""))

# Visualize Viktoria Plzen
gid = "Viktoria Plzeň"
h <- ggplot( subset(gl.att , home == gid), aes(rnd,att,label=vis) ) 
h + geom_bar(aes(fill=as.factor(rank))) + scale_fill_manual(
  values = c(rev(brewer.pal(9,"Greens"))[as.integer(levels(as.factor(gl.att [gl.att $home == gid, 5])))],"gray"), 
  name="League Pos") + geom_hline(aes(yintercept = b), hline.data[2,]) + geom_text(hjust=1.1, size=4, angle = 90) + scale_x_discrete("League Round") + scale_y_continuous("Attendance") + opts(title=paste("Attendance of ",gid," home matches GL 2011/12",sep=""))

# Visualize Banik Ostrava
gid = "Baník Ostrava"
i <- ggplot( subset(gl.att , home == gid), aes(rnd,att,label=vis) ) 
i + geom_bar(aes(fill=as.factor(rank))) + scale_fill_manual(
  values = c(rev(brewer.pal(9,"Greens"))[as.integer(levels(as.factor(gl.att [gl.att $home == gid, 5])))-(max(as.integer(levels(as.factor(gl.att [gl.att $home == gid, 5]))))-9)],"gray"), 
  name="League Pos") + geom_text(hjust=1.1, size=4, angle = 90) + scale_x_discrete("League Round") + scale_y_continuous("Attendance") + opts(title=paste("Attendance of ",gid," home matches GL 2011/12",sep=""))