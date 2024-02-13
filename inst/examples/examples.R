# forceNetwork 
data(MisLinks)
data(MisNodes)

links <- MisLinks[1:5,]
nodes <- MisNodes[1:5,]
nodes$x <- c(0,30,40,10,200)
nodes$y <- c(1000,30,70,1,20)
nodes$id <- c(0,1,2,3,4)
links$id <- c(0,1,2,3,4)
nodes$rot <- 78
nodes$dx <- -100
nodes$dy <- ".35em"

# Create graph
networkD3::forceNetwork(Links = links, Nodes = nodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", X = "x", Y = "y", dx = "dx", dy = "dy", rotate_angle = "rot", opacity = 1, zoom = T, bounded = T)

