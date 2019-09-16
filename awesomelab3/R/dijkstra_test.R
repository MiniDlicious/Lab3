#' Dijkstra's Algorithm.
#'
#' \code{dijkstra} Finds the shortest path from the indicated source node ("init_node") to every adjacent node.
#' Dijkstra's algorithm begins by creating a vector of distances. The source node is marked with a distance of 0 and the rest with infinity (\code{Inf}).  
#' For each neighboring node, the distance will be calculated and compared with the value in the vector of distances. If the current distance is smaller than the distance in the vector will be changed. 
#' When each neighbor node distance has been evaluated, the source node will be marked as visited. This procedure will be repeated until all the nodes in the graph are visited. 
#' 
#' 
#' @param graph an object of class "\code{data.frame}" containing three columns: "v1", "v2" (the nodes of the graph) and "w" (the distance from "v1" to "v2").
#' @param init_node numeric scalar object.
#'
#' @return \code{dijkstra} returns a vector with the shortest path distances from the starting node to every other node.
#'
#' @examples
#' data(wiki_graph)
#' dijkstra(wiki_graph, 1)
#' dijkstra(wiki_graph, 3)
#' 
#' @references \url{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}
#'
#' @export

dijkstra_test <- function(graph, init_node){
  visited <- vector()
  distance_values <- rep(Inf, length(unique(graph[["v1"]])))
  distance_values[init_node] <- 0
  
  
  distance_cost <- distance_values
  
  while(any(distance_values == Inf)){
    # Create list with nodes not in visited and their distance values
    choosable_nodes <- distance_values
    choosable_nodes[visited] <- Inf
    
    # Choose minimal distance index as current node and add to visited
    current_node <- which.min(choosable_nodes)
    visited <- append(visited, current_node)
    
    # Create dataframe slice with adjecent nodes and weights, for nodes not in visited
    adjecent_nodes <- graph[which(graph$v1 == current_node),]
    adjecent_nodes <- adjecent_nodes[!adjecent_nodes$v2 %in% visited,]
    
    # Calculate the weights from current node. Current node weight + weight to adjecent node
    calculated_distance <- rep(0, length(distance_values))
    calculated_distance[adjecent_nodes$v2] <- distance_values[current_node] + adjecent_nodes$w
    
    # Save minimum node costs
    for(k in 1:length(adjecent_nodes$v2)){
      distance_cost[adjecent_nodes$v2[k]] <- min(distance_cost[adjecent_nodes$v2[k]], calculated_distance[adjecent_nodes$v2[k]])
    }
    
    # Create vector to be able to choose minimum distance vector
    tmp <- distance_cost
    tmp[tmp == 0] <- NA
    #tmp[visited[1:(length(visited)-1)]] <- NA
    tmp[visited] <- NA
    minimum_distance_node <- which.min(tmp)
    
    
    
    # Set distance value for the minimum distance node not in visited.
    distance_values[minimum_distance_node] <- distance_cost[minimum_distance_node]
    
    
    print(distance_values)
  }
  
  return(distance_values)
}
wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
dijkstra_test(wiki_graph, 3)

