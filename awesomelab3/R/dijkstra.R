dijkstra <- function(graph, init_node){
  
  # Set of all nodes
  nodes_set <- 1:max(graph[,1])
  
  # Ensure that the structure and class of the arguments are correct:
  stopifnot(is.numeric(init_node), init_node %in% nodes_set, is.data.frame(graph),names(graph) == c("v1","v2","w" ))

  # Conversion of the data frame to a matrix with number of rows and columns equal to the number of total nodes
  df <- matrix(0, ncol=max(graph[,1]), nrow=max(graph[,1]))
  k <- 1
  for(i in graph$v1){
    df[i,graph$v2[k]] <-graph$w[k] # each element of the matrix is the distance between node i (row) and node j (column).
    k <- k+1
  }
  
  # Replacement of 0 distances between two diferent nodes for the minimum distance path achieved going through other nodes.
  for(i in 1:(nrow(df)-1)){
    for(j in 2:ncol(df)){
      if(i!=j && df[i,j] ==0 ){ 
        min= 999
        for(s in (i+1):(j-1)){
          if((df[i,s]+ df[s,j])<=min){
            min <- df[i,s]+ df[s,j]
          }
        }
        df[i,j] <- min
      } 
    }
  }
  
  path <- df[init_node,]
  return(path) # return the distances vector from the initial node to every other nodes.
}

wiki_graph <-data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
                        v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
                        w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
dijkstra(wiki_graph, 1)
dijkstra(wiki_graph, 3)
