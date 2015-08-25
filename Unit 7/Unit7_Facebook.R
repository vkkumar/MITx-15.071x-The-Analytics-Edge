library(igraph)
require(tcltk)

setwd("~/Dropbox/EdX/The Analytics Edge/Unit 7/")
edges = read.csv("edges.csv")
users = read.csv("users.csv")
str(users)
str(edges)

attach(users)
table(users$school, users$gender)
table(users$gender)

g = graph.data.frame(edges, FALSE, users)
plot(g, vertex.size=2, vertex.label=NA)

summary(g)
mean(degree(g))

sum(degree(g) >= 10)

V(g)$size = degree(g)/2 + 2
max(V(g)$size)
min(V(g)$size)
plot(g, vertex.label = NA)
tkplot(g, canvas.width = 450, canvas.height = 450)

V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "blue"
tkplot(g, canvas.width = 450, canvas.height = 450)

V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "blue"
plot(g, vertex.label = NA)
tkplot(g, canvas.width = 450, canvas.height = 450)

V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "blue"
plot(g, vertex.label = NA)

V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "blue"
plot(g, vertex.label = NA)

help("igraph")
rglplot(g, vertex.label=NA)
plot(g, vertex.label=NA, edge.width=3)
