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
  
  qplot(0,0)
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

  suppressMessages(ggplotly(p = p, tooltip = c("label"), layerData = 3))
}

plot_issue <- function() {

  p <- ggplot(vol$nodes, aes(x, y)) +
        geom_segment(aes(x = x0, xend = x1, y = y0, yend = y1),
                     data = vol$edges, size = 0.25, color = grey(0.8)) +
        geom_point(aes(color = issue_description), size = 4) +
        geom_text(aes(label = YEAR), alpha = 0, size = 4) +
        theme_void()

  suppressMessages(ggplotly(p = p, tooltip = c("label"), layerData = 3))
}

plot_cluster <- function() {

  p <- ggplot(vol$nodes, aes(x, y)) +
        geom_segment(aes(x = x0, xend = x1, y = y0, yend = y1),
                     data = vol$edges, size = 0.25, color = grey(0.8)) +
        geom_point(aes(color = factor(cluster)), size = 4, show.legend = FALSE) +
        geom_text(aes(label = YEAR), alpha = 0, size = 4) +
        theme_void()

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

  suppressMessages(ggplotly(p = p, tooltip = c("label"), layerData = 3))
}


plot_gatekeeper <- function() {

  p <- ggplot(vol$nodes, aes(x, y)) +
        geom_segment(aes(x = x0, xend = x1, y = y0, yend = y1),
                     data = vol$edges, size = 0.25, color = grey(0.8)) +
        geom_point(aes(color = factor(gatekeeper)), size = 4) +
        geom_text(aes(label = YEAR), alpha = 0, size = 4) +
        theme_void()

  suppressMessages(ggplotly(p = p, tooltip = c("label"), layerData = 3))
}
