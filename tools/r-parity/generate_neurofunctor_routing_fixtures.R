#!/usr/bin/env Rscript

# Routing-parity oracle for scalafim.spatial.SpatialGraph (STP P7.02).
#
# Builds a small neurofunctor BrainGraph with forward affine edges and
# warp edges whose inverses carry declared qualities, then records what
# neurofunctor::find_path_with_inverses chooses for a set of queries:
# forward-first routing, inverse fallback, and the inverse edge cost
# cost + penalty * (1 - quality). Output rows are pasted into
# modules/spatial/shared/src/test/scala/scalafim/spatial/RoutingParitySuite.scala.
#
# Row format: source,target,penalty,edges,used_inverses,total_cost
# where edges are the fixture edge names in path order, suffixed ":inverse"
# when neurofunctor routed through the inverted edge.
#
# A second "penalty flip" graph follows the "# flip" marker: X reaches Y only
# through inverses, either by one low-quality inverse (f1, cost 1, quality 0)
# or by two exact-quality inverses (f2, f3, cost 0.8 each). The inverse
# penalty decides which route wins, so changing it flips the chosen route.

suppressMessages(library(neurofunctor))

cat(sprintf("# neurofunctor %s\n", as.character(packageVersion("neurofunctor"))))

make_domain <- function(name) {
  Domain(TemplateSpace(name, name), VolumeGeometry(c(2L, 2L, 2L), diag(4)))
}

domain_names <- c("A", "B", "C", "D", "E", "X", "Y", "M")
domains <- lapply(domain_names, make_domain)
hashes <- setNames(vapply(domains, domain_hash, character(1)), domain_names)

graph <- BrainGraph()
for (domain in domains[1:5]) graph <- add_domain(graph, domain)

affine_edge <- function(source, target, cost) {
  Affine3DMorphism(hashes[[source]], hashes[[target]], diag(4), cost = cost)
}

warp_edge <- function(source, target, cost, quality) {
  edge <- Warp3DMorphism(hashes[[source]], hashes[[target]], "/w.nii", inverse_path = "/wi.nii", cost = cost)
  edge@inverse_quality <- quality
  edge
}

edges <- list(
  e1 = affine_edge("A", "B", 1.0),
  e2 = warp_edge("B", "C", 1.0, 0.2),
  e3 = warp_edge("D", "C", 0.5, 0.9),
  e4 = affine_edge("A", "D", 2.0),
  e5 = warp_edge("E", "A", 0.3, 0.5)
)
for (edge in edges) graph <- add_morphism(graph, edge)

queries <- list(
  c("A", "C", "1.0"),
  c("C", "A", "1.0"),
  c("C", "A", "10.0"),
  c("A", "E", "1.0"),
  c("C", "B", "1.0"),
  c("C", "B", "0.0"),
  c("D", "B", "1.0"),
  c("B", "E", "1.0")
)

run_queries <- function(graph, edges, queries) {
  edge_name <- function(morphism) {
    for (name in names(edges)) {
      edge <- edges[[name]]
      if (edge@source == morphism@source && edge@target == morphism@target) return(name)
      if (edge@source == morphism@target && edge@target == morphism@source) return(paste0(name, ":inverse"))
    }
    "identity"
  }
  for (query in queries) {
    path <- tryCatch(
      find_path_with_inverses(graph, hashes[[query[[1]]]], hashes[[query[[2]]]], penalty_factor = as.numeric(query[[3]])),
      error = function(e) NULL
    )
    if (is.null(path)) {
      cat(sprintf("%s,%s,%s,NOPATH,,\n", query[[1]], query[[2]], query[[3]]))
    } else {
      names_in_order <- vapply(path, edge_name, character(1))
      total <- sum(vapply(path, function(m) m@cost, numeric(1)))
      cat(sprintf(
        "%s,%s,%s,%s,%s,%.6f\n",
        query[[1]], query[[2]], query[[3]],
        paste(names_in_order, collapse = "|"),
        attr(path, "used_inverses"),
        total
      ))
    }
  }
}

run_queries(graph, edges, queries)

cat("# flip\n")
flip <- BrainGraph()
for (domain in domains[6:8]) flip <- add_domain(flip, domain)
flip_edges <- list(
  f1 = warp_edge("Y", "X", 1.0, 0.0),
  f2 = warp_edge("M", "X", 0.8, 1.0),
  f3 = warp_edge("Y", "M", 0.8, 1.0)
)
for (edge in flip_edges) flip <- add_morphism(flip, edge)
run_queries(flip, flip_edges, list(
  c("X", "Y", "0.0"),
  c("X", "Y", "0.5"),
  c("X", "Y", "1.0"),
  c("X", "Y", "10.0")
))
