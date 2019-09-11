wiki_graph <-data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
dijkstra(wiki_graph, 1)
dijkstra(wiki_graph, 3)

dijkstra <- function(graph, init_node){
  nodes_set <- 1:max(graph[,1])
  stopifnot(is.numeric(init_node), init_node %in% nodes_set, is.data.frame(graph),names(graph) == c("v1","v2","w" ))
  
  path <- vector()
  destination <- 0
 
  df <- data.frame()
  for (i in 1:nrow(graph)){
    for(j in 1:nrow(graph)){
      df[i,j] <- 
    }
  }
   for(i in nodes_set)){
    if(graph$w[i] < min && graph$v1[i]== init_node){
      min <- graph$w[i]
      destination <- graph$v2[i]
    }
    
    path <- c(path, destination)
    cost <- c(cost, min)
  }
  
  
  
  return(path)
}
