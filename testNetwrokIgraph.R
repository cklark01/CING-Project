library(RCy3)
library(igraph)
library(influential)
cytoscapePing()

ig2<-createIgraphFromNetwork()

plot.igraph(ig2, edge.arrow.size=0.2)

exportNetwork('/Users/constantinosclark/Desktop/CING/CING-Project/NetworkFiles/network1','SIF',ig2)

MyGraph <- sif2igraph(Path = "/Users/constantinosclark/Desktop/CING/CING-Project/NetworkFiles/network1.sif", directed=TRUE)
plot.igraph(MyGraph, edge.arrow.size=0.2)

plot(degree(MyGraph))
betweenness(MyGraph)
closeness(MyGraph)
plot(evcent(MyGraph)$vector)
vcount(MyGraph)
ecount(MyGraph)

graph.density(MyGraph)
average.path.length(MyGraph)

centralization.betweenness(MyGraph)$centralization
centralization.degree(MyGraph)$centralization

summary(MyGraph)
library(cancerGI)
# compute smallworldness
#https://search.r-project.org/CRAN/refmans/cancerGI/html/computeSmallWorldness.html
computeSmallWorldness (MyGraph, n=45, m=696)

# Global clustering coefficient
transitivity(MyGraph)
# Average clustering coefficient
transitivity(MyGraph, type = "average")
# The same as above
mean(transitivity(MyGraph, type = "local"), na.rm = TRUE)



#Randomness
n=20
p=0.2
m=(n*(n-1)/2)*p
er21 <- erdos.renyi.game(n, p, type="gnp")
plot(er21, layout=layout.circle(er21), main="gnp")
der21 <- degree(MyGraph)
der22 <- degree(er21)
summary(der21)
summary(der22)
hist(der21, col=rgb(0,0,1,.4), xlim=c(0,90), xlab="degree",ylab="freq", main="Graph Degree Distribution")
hist(der22, col=rgb(0,0,1,.4), xlim=c(0,20), xlab="degree",ylab="freq", main="p=2")



plot(ecdf(degree_distribution(MyGraph)))




#https://jcasasr.wordpress.com/2015/02/03/plotting-the-coreness-of-a-network-with-r-and-igraph/
g <- MyGraph

CorenessLayout <- function(g) {
  coreness <- graph.coreness(g);
  xy <- array(NA, dim=c(length(coreness), 2));
  
  shells <- sort(unique(coreness));
  for(shell in shells) {
    v <- 1 - ((shell-1) / max(shells));
    nodes_in_shell <- sum(coreness==shell);
    angles <- seq(0,360,(360/nodes_in_shell));
    angles <- angles[-length(angles)]; # remove last element
    xy[coreness==shell, 1] <- sin(angles) * v;
    xy[coreness==shell, 2] <- cos(angles) * v;
  }
  return(xy);
}

# g is the network
# compute coreness
coreness <- graph.coreness(g)
# assign colors
colbar <- rainbow(max(coreness))
# create layout
ll <- CorenessLayout(g)
# plot
plot(g, layout=ll, vertex.size=15, vertex.color=colbar[coreness], vertex.frame.color=colbar[coreness], main='Coreness')
