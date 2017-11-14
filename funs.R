# Script to work with SCOTUS network data

vol <- environment()

load_data <- function() {

  if (!require("igraph", quietly = TRUE, warn.conflicts = FALSE)) {
    stop("Please download the igraph package")
  }
  if (!require("dplyr", quietly = TRUE, warn.conflicts = FALSE)) {
    stop("Please download the dplyr package")
  }
  if (!require("readr", quietly = TRUE, warn.conflicts = FALSE)) {
    stop("Please download the readr package")
  }
  if (!require("plotly", quietly = TRUE, warn.conflicts = FALSE)) {
    stop("Please download the plotly package")
  }
  if (!require("viridis", quietly = TRUE, warn.conflicts = FALSE)) {
    stop("Please download the viridis package")
  }
  if (!require("stringi", quietly = TRUE, warn.conflicts = FALSE)) {
    stop("Please download the viridis package")
  }

  vol$scotus_edges <- read_csv("https://statsmaths.github.io/stat_data/scotus_edges.csv",
                              col_types = cols(
                                                from = col_character(),
                                                to = col_character()
                                              ))
  vol$scotus_edges_co <- read_csv("https://statsmaths.github.io/stat_data/scotus_edges_cocitation.csv",
                                  col_types = cols(
                                                from = col_character(),
                                                to = col_character(),
                                                count = col_integer()
                                              ))
  vol$scotus_nodes <- read_csv("https://statsmaths.github.io/stat_data/scotus_nodes.csv",
                              col_type = cols(
                                        usid = col_character(),
                                        term = col_integer(),
                                        chief = col_character(),
                                        name = col_character(),
                                        issue = col_integer(),
                                        issue_description = col_character(),
                                        votes_against = col_integer(),
                                        liberal_flag = col_integer()
                                      ))
  vol$G <- graph.edgelist(as.matrix(scotus_edges), directed=FALSE)
  vol$G_co <- graph.edgelist(as.matrix(scotus_edges_co[,-3]), directed=FALSE)
  
}

select_topic_cites <- function(...) {

  issue_codes <- as.character(unlist(list(...)))

  usids <- filter(vol$scotus_nodes, issue %in% issue_codes)$usid
  H <- induced.subgraph(vol$G, match(usids, names(V(vol$G))))
  cmp <- components(H)

  these_groups <- which.max(cmp$csize >= 3)
  usids <- names(cmp$membership[which(!is.na(match(cmp$membership, these_groups)))])
  H <- induced.subgraph(H, match(usids, names(V(H))))

  set.seed(1)
  L <- layout.fruchterman.reingold(H)
  vs <- V(H)
  es <- get.edgelist(H)
  ids <- cbind(match(es[,1], names(vs)),
               match(es[,2], names(vs)))

  wc <- cluster_walktrap(H)
  nodes <- data_frame(x = L[,1], y = L[,2], usid = vs$name,
                      eigen = eigen_centrality(H)$vector,
                      between = betweenness(H),
                      cluster = membership(wc))
  nodes$gatekeeper <- as.numeric(nodes$between >= sort(nodes$between, decreasing = TRUE)[10] &
                                 nodes$eigen < sort(nodes$eigen, decreasing = TRUE)[10])
  nodes <- left_join(nodes, vol$scotus_nodes, by = "usid")
  nodes$YEAR <- sprintf("%d\nCASE: %s\nUSID: %s", nodes$term,
                        stri_trans_totitle(nodes$name), nodes$usid)
  nodes$issue_description <- stri_sub(nodes$issue_description, 1, 25)

  edges <- data_frame(x0 = L[ids[,1],1],
                      x1 = L[ids[,2],1],
                      y0 = L[ids[,1],2],
                      y1 = L[ids[,2],2])

  vol$nodes <- nodes
  vol$edges <- edges

}

select_topic_cocites <- function(...) {

  issue_codes <- as.character(unlist(list(...)))

  usids <- filter(vol$scotus_nodes, issue %in% issue_codes)$usid
  usids <- usids[!is.na(match(usids, names(V(vol$G_co))))]

  H <- induced.subgraph(vol$G_co, match(usids, names(V(vol$G_co))))
  cmp <- components(H)

  these_groups <- which.max(cmp$csize >= 3)
  usids <- names(cmp$membership[which(!is.na(match(cmp$membership, these_groups)))])
  H <- induced.subgraph(H, match(usids, names(V(H))))

  set.seed(1)
  L <- layout.fruchterman.reingold(H)
  vs <- V(H)
  es <- get.edgelist(H)
  ids <- cbind(match(es[,1], names(vs)),
               match(es[,2], names(vs)))

  wc <- cluster_walktrap(H)
  nodes <- data_frame(x = L[,1], y = L[,2], usid = vs$name,
                      eigen = eigen_centrality(H)$vector,
                      between = betweenness(H),
                      cluster = membership(wc))
  nodes$gatekeeper <- as.numeric(nodes$between >= sort(nodes$between, decreasing = TRUE)[10] &
                                 nodes$eigen < sort(nodes$eigen, decreasing = TRUE)[10])
  nodes <- left_join(nodes, vol$scotus_nodes, by = "usid")
  nodes$YEAR <- sprintf("%d\nCASE: %s\nUSID: %s", nodes$term,
                        stri_trans_totitle(nodes$name), nodes$usid)
  nodes$issue_description <- stri_sub(nodes$issue_description, 1, 25)

  edges <- data_frame(x0 = L[ids[,1],1],
                      x1 = L[ids[,2],1],
                      y0 = L[ids[,1],2],
                      y1 = L[ids[,2],2])

  vol$nodes <- nodes
  vol$edges <- edges

}




plot_centrality <- function() {

  p <- ggplot(vol$nodes, aes(x, y)) +
        geom_segment(aes(x = x0, xend = x1, y = y0, yend = y1),
                     data = vol$edges, size = 0.25, color = grey(0.8)) +
        geom_point(aes(color = eigen), size = 4) +
        scale_color_viridis() +
        geom_text(aes(label = YEAR), alpha = 0, size = 4) +
        theme_void()

  plot(p)
  
  suppressMessages(ggplotly(p = p, tooltip = c("label"), layerData = 3))
}

plot_issue <- function() {

  p <- ggplot(vol$nodes, aes(x, y)) +
        geom_segment(aes(x = x0, xend = x1, y = y0, yend = y1),
                     data = vol$edges, size = 0.25, color = grey(0.8)) +
        geom_point(aes(color = issue_description), size = 4) +
        geom_text(aes(label = YEAR), alpha = 0, size = 4) +
        theme_void()

  plot(p)
  
  suppressMessages(ggplotly(p = p, tooltip = c("label"), layerData = 3))
}

plot_cluster <- function() {

  p <- ggplot(vol$nodes, aes(x, y)) +
        geom_segment(aes(x = x0, xend = x1, y = y0, yend = y1),
                     data = vol$edges, size = 0.25, color = grey(0.8)) +
        geom_point(aes(color = factor(cluster)), size = 4, show.legend = FALSE) +
        geom_text(aes(label = YEAR), alpha = 0, size = 4) +
        theme_void()

  plot(p)
  
  suppressMessages(ggplotly(p = p, tooltip = c("label"), layerData = 3))
}

plot_year <- function() {

  p <- ggplot(vol$nodes, aes(x, y)) +
        geom_segment(aes(x = x0, xend = x1, y = y0, yend = y1),
                     data = vol$edges, size = 0.25, color = grey(0.8)) +
        geom_point(aes(color = term), size = 4) +
        scale_color_viridis() +
        geom_text(aes(label = YEAR), alpha = 0, size = 4) +
        theme_void()

  plot(p)
  
  suppressMessages(ggplotly(p = p, tooltip = c("label"), layerData = 3))
}

plot_vote <- function() {

  p <- ggplot(vol$nodes, aes(x, y)) +
        geom_segment(aes(x = x0, xend = x1, y = y0, yend = y1),
                     data = vol$edges, size = 0.25, color = grey(0.8)) +
        geom_point(aes(color = factor(votes_against)), size = 4) +
        scale_color_viridis(discrete = TRUE) +
        geom_text(aes(label = YEAR), alpha = 0, size = 4) +
        theme_void() +
        labs(color = "Number of Dissenting Votes")

  plot(p)
  
  suppressMessages(ggplotly(p = p, tooltip = c("label"), layerData = 3))
}


plot_gatekeeper <- function() {

  p <- ggplot(vol$nodes, aes(x, y)) +
        geom_segment(aes(x = x0, xend = x1, y = y0, yend = y1),
                     data = vol$edges, size = 0.25, color = grey(0.8)) +
        geom_point(aes(color = factor(gatekeeper)), size = 4) +
        geom_text(aes(label = YEAR), alpha = 0, size = 4) +
        theme_void()

  plot(p)
  
  suppressMessages(ggplotly(p = p, tooltip = c("label"), layerData = 3))
}

graph_data <- function(edges, nodes = NULL, nodes2 = NULL,
                       directed = FALSE, layout = NULL) {

  # if no nodes data frame, create it
  if (is.null(nodes))
    nodes <- dplyr::data_frame(id = unique(c(edges[[1]], edges[[2]])))

  # is this a bipartite graph?
  bipartite <- !is.null(nodes2)

  # make sure edges have associated nodes
  names(nodes)[1] <- "id"
  names(edges)[1:2] <- c("id", "id_out")
  nodes[[1]] <- as.character(nodes[[1]])
  edges[[1]] <- as.character(edges[[1]])
  edges[[2]] <- as.character(edges[[2]])
  node_names <- nodes$id
  bad_ids <- c(which(is.na(match(edges[[1]], node_names))),
               which(is.na(match(edges[[2]], node_names))))
  if (length(bad_ids) > 0)
    edges <- edges[-bad_ids,]

  # create the graph object
  H <- igraph::graph.edgelist(as.matrix(edges[,1:2]), directed = directed)

  # layout is stochastic, so make it here with a fixed seed
  set.seed(1)
  if (is.null(layout)) {
    if (bipartite) {
      L <- igraph::layout_as_bipartite(H)
    } else {
      L <- igraph::layout_nicely(H)
    }
  } else {
    L <- as.matrix(layout)
  }

  # get id's from the graph back into the edge list
  vs <- igraph::V(H)
  es <- igraph::get.edgelist(H)
  noms <- names(vs)
  if (is.null(noms)) noms <- seq_along(vs)
  ids <- cbind(match(es[,1], noms),
               match(es[,2], noms))

  # create node output
  cmp <- igraph::components(H)
  node_out <- dplyr::data_frame(id = as.character(noms),
                          x = L[,1], y = L[,2],
                          degree = igraph::degree(H, mode = "all"),
                          degree_in = igraph::degree(H, mode = "in"),
                          degree_out = igraph::degree(H, mode = "out"),
                          eigen = igraph::eigen_centrality(H, directed = FALSE)$vector,
                          close = igraph::closeness(H),
                          between = igraph::betweenness(H),
                          cluster = as.character(as.integer(igraph::membership(igraph::cluster_walktrap(H)))),
                          component = as.integer(cmp$membership),
                          component_size = cmp$csize[as.integer(cmp$membership)])

  these <- which(!is.na(match(names(nodes)[-1], names(node_out)[-1]))) + 1
  if (length(these))
    nodes <- nodes[,-these]
  node_out <- dplyr::inner_join(node_out, nodes, by = "id")

  # create output edge data
  edge_out <- dplyr::data_frame(x = L[ids[,1],1],
                                xend = L[ids[,2],1],
                                y = L[ids[,1],2],
                                yend = L[ids[,2],2])

  these <- which(!is.na(match(names(edges), names(edge_out))))
  if (length(these))
    edges <- edges[,-these]
  edge_out <- dplyr::bind_cols(edges, edge_out)

  list(nodes = node_out, edges = edge_out)
}

load_csv <- function() {
  if (!file.exists("nodes.csv"))
    stop("Cannot find file 'nodes.csv'; did you name it correctly and set the working directory?")
  if (!file.exists("edges.csv"))
    stop("Cannot find file 'edges.csv'; did you name it correctly and set the working directory?")
  
  nodes <- readr::read_csv("nodes.csv")
  edges <- readr::read_csv("edges.csv")
  
  vol$gr <- graph_data(edges, nodes, directed = FALSE)
}

list_plots <- function() {
  if (is.null(vol$gr))
     stop("You need to load data with 'load_csv' before running this function")
  
  noms <- names(vol$gr$nodes[-c(1,2,3)])
  cat(" ")
  cat(sprintf("%d: %s\n", seq_along(noms), noms))
  
}

plot_network_category <- function(num = 1) {
  noms <- names(vol$gr$nodes[-c(1,2,3)])

  cat(sprintf("Plotting graph using '%s'\n", noms[num])) 
  
  nodes <- vol$gr$nodes[,c(1,2,3,num + 3)]
  names(nodes)[4] <- "value"
  nodes$label <- sprintf("%s\n%s: %s", nodes$id, noms[num], nodes$value)
  p <- ggplot(nodes, aes(x, y)) +
        geom_segment(aes(x = x, xend = xend, y = y, yend = yend),
                     data = vol$gr$edges, size = 0.25, color = grey(0.8)) +
        geom_point(aes(color = factor(value)), size = 4) +
        scale_color_viridis() +
        geom_text(aes(label = label), alpha = 0, size = 4) +
        theme_void()

  plot(p)
  
  suppressMessages(ggplotly(p = p, tooltip = c("label"), layerData = 3))
}

plot_network_numeric <- function(num = 1) {
  noms <- names(vol$gr$nodes[-c(1,2,3)])

  cat(sprintf("Plotting graph using '%s'\n", noms[num])) 
  
  nodes <- vol$gr$nodes[,c(1,2,3,num + 3)]
  names(nodes)[4] <- "value"
  nodes$label <- sprintf("%s\n%s: %s", nodes$id, noms[num], nodes$value)
  p <- ggplot(nodes, aes(x, y)) +
        geom_segment(aes(x = x, xend = xend, y = y, yend = yend),
                     data = vol$gr$edges, size = 0.25, color = grey(0.8)) +
        geom_point(aes(color = as.numeric(value)), size = 4) +
        scale_color_viridis() +
        geom_text(aes(label = label), alpha = 0, size = 4) +
        theme_void()

  plot(p)
  
  suppressMessages(ggplotly(p = p, tooltip = c("label"), layerData = 3))
}


