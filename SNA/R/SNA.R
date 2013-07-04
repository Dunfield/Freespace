### Data from "Freespace"
### by David Hackett Fisher

library(igraph)

##data <- as.matrix(read.csv("~/Documents/Freespace/SNA/data/NFS_reduced_noFS.csv",row.names=1))
##data <- as.matrix(read.csv("~/Documents/Freespace/SNA/data/NFS_reduced_noFS_noUNI.csv",row.names=1))
##data <- as.matrix(read.csv("~/Documents/Freespace/SNA/data/NFS.csv",row.names=1))
##data <- as.matrix(read.csv("~/Documents/Freespace/SNA/data/NFS_noFS.csv",row.names=1))
data <- as.matrix(read.csv("~/Documents/Freespace/SNA/data/NFS_reduced.csv",row.names=1))
##data <- as.matrix(read.csv("~/Documents/Freespace/SNA/data/NFS_reduced_noUNI.csv",row.names=1))

group.net <- data %*% t(data)
person.net <- t(data) %*% data

diag(group.net) <- NA
diag(person.net) <- NA

write.csv(group.net, file = "~/Documents/Freespace/SNA/data/group.csv",row.names=FALSE, na="")


person2.g <- graph.adjacency(person.net,mode="undirected",
                            weighted=NULL, diag=FALSE)

group2.g <- graph.adjacency(group.net,mode="undirected",
                             weighted=NULL, diag=FALSE)

group.g <- graph.adjacency(group.net, weighted=TRUE,
                           mode="undirected", diag=FALSE)

person.g <- graph.adjacency(person.net, mode="undirected", weighted=TRUE, diag=FALSE)

png(file="~/Documents/Freespace/SNA/group-view.png", width=1000, height=1000, res=150)
la <- layout.fruchterman.reingold(group.g)
e.wt <- get.edge.attribute(group.g, "weight")
plot(group.g, layout=la, vertex.size=15,edge.width=e.wt,
     vertex.label=V(group.g)$name)
dev.off()

png(file="~/Documents/Freespace/SNA/group-simple.png", width=1000, height=1000, res=150)
la <- layout.fruchterman.reingold(group2.g)
plot(group2.g, layout=la, vertex.size=15,edge.width=0.1,
     vertex.label=V(group2.g)$name)
dev.off()

png(file="~/Documents/Freespace/SNA/person-view.png", width=2200, height=1700, res=150)
la <- layout.fruchterman.reingold(person.g)
e.wt <- get.edge.attribute(person.g, "weight")
plot(person.g, layout=la, vertex.size=3,edge.width=e.wt,
     vertex.label=V(person.g)$name)
dev.off()

png(file="~/Documents/Freespace/SNA/person-simple.png", width=2200, height=1700, res=150)
la <- layout.fruchterman.reingold(person2.g)
plot(person2.g, layout=la, vertex.size=3,edge.width=0.1,
     vertex.label=V(person2.g)$name)
dev.off()



## Degree
centrality.norm(group.g,type="degree",centralization=FALSE)
centrality.norm(group.g,type="degree",centralization=TRUE)
## Closeness
centrality.norm(group.g,type="closeness",centralization=FALSE)
centrality.norm(group.g,type="closeness",centralization=TRUE)
## Betweenness
centrality.norm(group.g,type="betweenness",centralization=FALSE)
centrality.norm(group.g,type="betweenness",centralization=TRUE)

## Eigenvector
cent.eig <- evcent(person.g)
names(cent.eig$vector) <- V(person.g)$name
ind <- order(-cent.eig$vector)
cent.eig$vector[ind][1:length(cent.eig$vector)]

cent.eig <- evcent(group.g)
names(cent.eig$vector) <- V(group.g)$name
ind <- order(-cent.eig$vector)
cent.eig$vector[ind][1:length(cent.eig$vector)]

## Kleinberg authority
cent.klein <- authority.score(person.g)
names(cent.klein$vector) <- V(person.g)$name
ind <- order(-cent.klein$vector)
cent.klein$vector[ind][1:length(cent.klein$vector)]

cent.klein <- authority.score(group.g)
names(cent.klein$vector) <- V(group.g)$name
ind <- order(-cent.klein$vector)
cent.klein$vector[ind][1:length(cent.klein$vector)]

cent.klein2 <- hub.score(group.g)
names(cent.klein2$vector) <- V(group.g)$name
ind <- order(-cent.klein2$vector)
cent.klein2$vector[ind][1:length(cent.klein2$vector)]

## Bonacich Power
cent.bonpow <- bonpow(person.g, exponent=1)
names(cent.bonpow) <-  V(person.g)$name
ind <- order(cent.bonpow)
cent.bonpow[ind][1:length(cent.bonpow)]

cent.bonpow <- bonpow(group.g, exponent=1)
names(cent.bonpow) <-  V(group.g)$name
ind <- order(cent.bonpow)
cent.bonpow[ind][1:length(cent.bonpow)]


ind <- cent.bonpow < -1.35
col.vec <- rep("")


#spinglass groups
png(file="~/Documents/Freespace/SNA/spinglass-test.png", width=2200, height=1700)
la <- layout.fruchterman.reingold(group.g)
e.wt <- get.edge.attribute(group.g, "weight")
com <- spinglass.community(group.g, weights=e.wt, spins=10, parupdate=TRUE, start.temp=1, stop.temp=0.01, 
                           cool.fact=0.99, update.rule="simple", gamma=1, implementation="orig", gamma.minus=1)
V(group.g)$color <- com$membership+1
group.g <- set.graph.attribute(group.g, "layout", layout.fruchterman.reingold(group.g))
group.g <- set.edge.attribute(group.g, "width", value=e.wt)
group.g <- set.vertex.attribute(group.g, "label.cex", value=3)
group.g <- set.vertex.attribute(group.g, "size", value=12)
plot(group.g)
dev.off()


#spinglass people
png(file="~/Documents/Freespace/SNA/spinglass-people.png", width=2200, height=1700)
la <- layout.fruchterman.reingold(person.g)
e.wt <- get.edge.attribute(person.g, "weight")
com <- spinglass.community(person.g, weights=e.wt, spins=20)
V(person.g)$color <- com$membership+1
person.g <- set.graph.attribute(person.g, "layout", layout.fruchterman.reingold(person.g))
person.g <- set.edge.attribute(person.g, "width", value=e.wt)
plot(person.g)
dev.off()



# Plot the eigevector and betweenness centrality
png(file="~/Documents/Freespace/SNA/plot-test.png", width=2000, height=1000)
lay <- layout.fruchterman.reingold(group.g)
pr.id <- 200

par(mfrow=c(1,2))
plot(bonpow(group.g, exponent=1), betweenness(group.g))

e.rank <- rank(-evcent(group.g)$vector)
b.rank <- rank(-betweenness(group.g))
c.rank <- rank(-bonpow(group.g, exponent=1))
s.top <- c.rank < 10 | b.rank < 10
text(bonpow(group.g)[s.top], betweenness(group.g)[s.top], cex=0.6, pos=4, labels=V(group.g)$name[s.top])
V(group.g)[pr.id]$color <- "yellow"
E(group.g)$color="grey95"
plot(group.g, layout=lay, vertex.size=15,
     vertex.label.cex=0.6, vertex.label=V(group.g)$name)
dev.off()

### Centrality
centrality.norm<-function(graph,type=c("degree","closeness","betweenness"),centralization=FALSE)
{    
  result<-NA
  g<-graph
  cent<-centralization
  if (!is.igraph(g)) {stop("Not a graph object")}
  if (type[[1]] == "degree") {
    if (!cent) { temp <- degree(g)/(vcount(g)-1)
                 names(temp) <- V(g)$name
                 ind <- order(-temp)
                 result <- temp[ind][1:length(temp)]}
    else result <- (sum(max(degree(g))-degree(g)))/((vcount(g)-1)*(vcount(g)-2))}
  else if (type[[1]] == "betweenness") {
    temp <- 2*betweenness(g)/((vcount(g)-1)*(vcount(g)-2))
    if (!cent) { names(temp) <- V(g)$name
                 ind <- order(-temp)
                 result <- temp[ind][1:length(temp)]  }
    else  result <- sum(max(temp)-temp)/(vcount(g)-1)}
  else if (type[[1]] == "closeness") {
    if (!cent) { temp <- closeness(g)
                 names(temp) <- V(g)$name
                 ind <- order(-temp)
                 result <- temp[ind][1:length(temp)] }
    else result <- (2*vcount(g)-3)*(sum(max(closeness(g))-closeness(g)))/((vcount(g)-1)*(vcount(g)-2))}
  else {stop("this type is unavailable or mispelled")}
  return(result)
}
