#!/usr/bin/env Rscript

# neurofunctor law fixtures for scalafim.spatial (STP P7.06).
#
# Replays the laws neurofunctor's own tests check (tests/testthat/
# test-functor-laws.R, test-qc.R, test-hybrid-projector.R, test-field.R) on
# small hand-built graphs and records every case as an
# (input, operation, expected output) triplet. ScalaFIM rebuilds the same
# graphs from domains.tsv / edges.tsv and replays each triplet through its
# spatial API in NeurofunctorLawParitySuite.
#
# Usage (from the repository root):
#   Rscript tools/r-parity/generate_neurofunctor_law_fixtures.R [neurofunctor-source-dir]
#
# Outputs, all zero-based and in ScalaFIM element order (see order_of below):
#   modules/spatial/jvm/src/test/resources/scalafim/spatial/neurofunctor-laws/
#     manifest.json  R, package versions, neurofunctor source commit, encoding
#     domains.tsv    graph, domain, dims, voxel-to-world affine (row-major)
#     edges.tsv      graph, edge, source, target, kind, cost, inverse, quality,
#                    pullback affine (target world -> source world, row-major)
#     triplets.tsv   id, law, operation, graph, args, input, expected,
#                    tolerance, status, reason
#   modules/spatial/shared/src/test/scala/scalafim/spatial/fixtures/
#     NeurofunctorLawFixtures.scala  the same four files, line by line, so the
#                    shared suite loads them on the JVM and on Scala.js.
#
# Value encoding inside the input/expected/args cells: space-separated
# name=kind:payload fields.
#   dense:RxC:v,...        row-major matrix
#   sparse:RxC:i,j,x;...   zero-based triplets, R's stored entries
#   scalar:v | int:n | bool:TRUE | text:word
#   ids:e1|e2:inverse      morphism path, ":inverse" marks an inverted step
#   routes:e1|e2/e3        several paths, "/"-separated
#   points:x,y,z;...       world points (mm)
#   rows:i,...             zero-based target rows
#   error:nopath           neurofunctor raised an error
# Doubles use %.17g so they round-trip exactly.

suppressMessages({
  library(neurofunctor)
  library(Matrix)
})

args <- commandArgs(trailingOnly = TRUE)
source_dir <- if (length(args) >= 1) args[[1]] else path.expand("~/code/neurofunctor")

repo_root <- normalizePath(".")
resource_dir <- file.path(repo_root, "modules/spatial/jvm/src/test/resources/scalafim/spatial/neurofunctor-laws")
scala_file <- file.path(
  repo_root,
  "modules/spatial/shared/src/test/scala/scalafim/spatial/fixtures/NeurofunctorLawFixtures.scala"
)
if (!dir.exists(dirname(resource_dir))) stop("run from the scalafim repository root")
dir.create(resource_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(dirname(scala_file), showWarnings = FALSE, recursive = TRUE)

transform_path <- getExportedValue("neurotransform", "transform_path")

# ---------------------------------------------------------------------------
# Encoding helpers
# ---------------------------------------------------------------------------

num <- function(x) {
  out <- sprintf("%.17g", as.numeric(x))
  out[out == "-0"] <- "0"
  out
}

# ScalaFIM lays volume elements out with z fastest (row-major over x, y, z);
# neurofunctor uses x fastest. order_of() gives, for each element in
# neurofunctor order, its zero-based ScalaFIM index; several domains
# concatenate as hybrid parts. Every element-indexed value is permuted with it
# on export, as indices are shifted to zero-based.
order_of <- function(domains) {
  out <- integer(0)
  offset <- 0L
  for (d in domains) {
    dims <- d@geometry@dim
    f <- seq_len(prod(dims)) - 1L
    i <- f %% dims[1]
    j <- (f %/% dims[1]) %% dims[2]
    k <- f %/% (dims[1] * dims[2])
    out <- c(out, offset + as.integer((i * dims[2] + j) * dims[3] + k))
    offset <- offset + as.integer(prod(dims))
  }
  out
}

# Rows of `m` are elements in neurofunctor order when `rows` is given.
enc_dense <- function(m, rows = NULL) {
  m <- as.matrix(m)
  if (!is.null(rows)) {
    stopifnot(length(rows) == nrow(m))
    out <- m
    out[rows + 1L, ] <- m
    m <- out
  }
  sprintf("dense:%dx%d:%s", nrow(m), ncol(m), paste(num(t(m)), collapse = ","))
}

enc_sparse <- function(m, rows = NULL, cols = NULL) {
  m <- as(m, "CsparseMatrix")
  s <- summary(m)
  i <- if (is.null(rows)) s$i - 1L else rows[s$i]
  j <- if (is.null(cols)) s$j - 1L else cols[s$j]
  entries <- if (nrow(s) == 0) "" else paste(sprintf("%d,%d,%s", i, j, num(s$x)), collapse = ";")
  sprintf("sparse:%dx%d:%s", nrow(m), ncol(m), entries)
}

enc_scalar <- function(x) paste0("scalar:", num(x))
enc_int <- function(x) paste0("int:", as.integer(x))
enc_bool <- function(x) paste0("bool:", if (isTRUE(x)) "TRUE" else "FALSE")
enc_points <- function(p) {
  paste0("points:", paste(apply(p, 1, function(r) paste(num(r), collapse = ",")), collapse = ";"))
}
enc_rows <- function(rows) paste0("rows:", paste(as.integer(rows), collapse = ","))

fields <- function(...) {
  values <- list(...)
  paste(sprintf("%s=%s", names(values), unlist(values)), collapse = " ")
}

# Deterministic, exactly representable probe data.
probe <- function(n, k = 1L, salt = 0L) {
  i <- seq_len(n * k) + salt
  matrix(((i * 7L) %% 11L) - 5 + 0.25 * (i %% 3L), nrow = n, ncol = k)
}

world_probes <- rbind(
  c(0, 0, 0),
  c(1.5, -2, 3),
  c(10, 4, -1),
  c(-3.25, 0.5, 2.75)
)

# ---------------------------------------------------------------------------
# Graph fixtures
# ---------------------------------------------------------------------------

graphs <- list()
domain_rows <- character(0)
edge_rows <- character(0)

affine_of <- function(rows) matrix(c(rows, 0, 0, 0, 1), nrow = 4, byrow = TRUE)
translation <- function(x, y = 0, z = 0) affine_of(c(1, 0, 0, x, 0, 1, 0, y, 0, 0, 1, z))

new_graph <- function(name) {
  graphs[[name]] <<- list(graph = BrainGraph(), domains = list(), edges = list())
}

add_dom <- function(graph_name, name, dims, affine = diag(4)) {
  d <- Domain(
    VolumeSpace(name = name, subject_id = "toy", modality = "law"),
    VolumeGeometry(as.integer(dims), affine)
  )
  gr <- graphs[[graph_name]]
  gr$graph <- add_domain(gr$graph, d)
  gr$domains[[name]] <- d
  graphs[[graph_name]] <<- gr
  domain_rows <<- c(
    domain_rows,
    paste(graph_name, name, paste(dims, collapse = "x"), paste(num(t(affine)), collapse = ","), sep = "\t")
  )
}

add_edge <- function(graph_name, name, source, target, pullback, cost = 1) {
  gr <- graphs[[graph_name]]
  m <- Affine3DMorphism(
    domain_hash(gr$domains[[source]]),
    domain_hash(gr$domains[[target]]),
    pullback,
    cost = cost
  )
  gr$graph <- add_morphism(gr$graph, m)
  gr$edges[[name]] <- m
  graphs[[graph_name]] <<- gr
  quality <- if (is.na(m@inverse_quality)) "NA" else num(m@inverse_quality)
  edge_rows <<- c(
    edge_rows,
    paste(graph_name, name, source, target, "affine", num(cost), m@inverse_type, quality,
          paste(num(t(pullback)), collapse = ","), sep = "\t")
  )
}

g <- function(name) graphs[[name]]$graph
dom <- function(graph_name, name) graphs[[graph_name]]$domains[[name]]
hash <- function(graph_name, name) domain_hash(dom(graph_name, name))
ord <- function(graph_name, ...) order_of(lapply(c(...), function(name) dom(graph_name, name)))

# Edge names of a neurofunctor path; inverted steps get ":inverse".
path_ids <- function(graph_name, path) {
  edges <- graphs[[graph_name]]$edges
  ids <- vapply(path, function(m) {
    # a forward edge wins over the inverse of an opposite edge (loop2 has both)
    for (name in names(edges)) {
      e <- edges[[name]]
      if (e@source == m@source && e@target == m@target && identical(e@matrix, m@matrix)) return(name)
    }
    for (name in names(edges)) {
      e <- edges[[name]]
      if (e@source == m@target && e@target == m@source) return(paste0(name, ":inverse"))
    }
    stop("unknown morphism in path")
  }, character(1))
  paste0("ids:", paste(ids, collapse = "|"))
}

# "laws": the operator fixture. A, B, C, S, Q, T have identity voxel-to-world
# affines; R has 0.75 mm voxels and an offset origin. Every fractional mapping
# lands inside its source grid. The integer shifts ab and bc pull some rows to
# exactly one voxel outside the grid (x = 4 or y = 3), which both libraries
# leave empty; that agreement relies on the shift being exact in floating
# point, since a point a rounding error inside the edge would be renormalised
# by ScalaFIM and dropped by neurofunctor (see the "boundary" graph). sq has a
# scaling term, so it does not commute with as and the composed routes test
# the order in which path steps apply.
new_graph("laws")
add_dom("laws", "A", c(4, 3, 2))
add_dom("laws", "B", c(4, 3, 2))
add_dom("laws", "C", c(4, 3, 2))
add_dom("laws", "S", c(3, 2, 1))
add_dom("laws", "Q", c(2, 1, 1))
add_dom("laws", "T", c(3, 2, 2))
r_affine <- affine_of(c(0.75, 0, 0, 0.1, 0, 0.75, 0, 0.2, 0, 0, 0.75, 0.3))
add_dom("laws", "R", c(3, 2, 2), r_affine)
general <- affine_of(c(0.9, 0.1, 0, 0.2, 0, 0.8, 0.1, 0.3, 0, 0, 0.5, 0.1))
add_edge("laws", "ab", "A", "B", translation(1))
add_edge("laws", "bc", "B", "C", translation(0, 1))
add_edge("laws", "as", "A", "S", translation(0.3, 0.6, 0.25))
add_edge("laws", "sq", "S", "Q", affine_of(c(1.2, 0, 0, 0.5, 0, 1, 0, 0.5, 0, 0, 1, 0)))
add_edge("laws", "ar", "A", "R", general)
add_edge("laws", "bs", "B", "S", translation(0.1, 0.2, 0.5))
add_edge("laws", "br", "B", "R", translation(0.5, 0.25, -0.3))
add_edge("laws", "at", "A", "T", translation(0.4, 0.3, 0))

# "boundary": a half-voxel shift between equal grids, so the last x column of
# the target falls between the source's last voxel centre and its edge.
new_graph("boundary")
add_dom("boundary", "A", c(4, 3, 2))
add_dom("boundary", "B", c(4, 3, 2))
add_edge("boundary", "ab", "A", "B", translation(0.5, 0, 0))

# "inverse": only S -> A exists; A -> S needs the exact inverse.
new_graph("inverse")
add_dom("inverse", "A", c(4, 3, 2))
add_dom("inverse", "S", c(3, 2, 1))
add_edge("inverse", "sa", "S", "A", translation(-0.3, -0.6, -0.25))

# "loop": a general affine with an exact inverse, for coordinate round trips.
new_graph("loop")
add_dom("loop", "A", c(4, 3, 2))
add_dom("loop", "B", c(4, 3, 2))
add_edge("loop", "ab", "A", "B", affine_of(c(0.8, -0.6, 0, 1.5, 0.6, 0.8, 0, -0.5, 0, 0, 1.2, 0.25)))

# "loop2": an explicit reverse edge that does not invert the forward one.
new_graph("loop2")
add_dom("loop2", "A", c(4, 3, 2))
add_dom("loop2", "B", c(4, 3, 2))
add_edge("loop2", "ab", "A", "B", translation(1, 0, 0))
# ba scales x, so it does not commute with ab's x shift: the loop's result
# depends on the order its two steps are applied in.
add_edge("loop2", "ba", "B", "A", affine_of(c(1.2, 0, 0, -0.5, 0, 1.1, 0, 0, 0, 0, 1, 0.2)))

# "diamond": two commuting routes A->B->D and A->C->D, inserted so that
# neurofunctor's depth-first enumeration lists the dearer one first, and a
# skewed direct edge that does not commute.
new_graph("diamond")
for (d in c("A", "B", "C", "D")) add_dom("diamond", d, c(4, 3, 2))
add_edge("diamond", "ab", "A", "B", translation(1), cost = 1)
add_edge("diamond", "bd", "B", "D", translation(0, 1), cost = 1)
add_edge("diamond", "ac", "A", "C", translation(0, 1), cost = 0.5)
add_edge("diamond", "cd", "C", "D", translation(1), cost = 0.5)
add_edge("diamond", "ad", "A", "D", translation(1, 1.25), cost = 5)

# ---------------------------------------------------------------------------
# Triplets
# ---------------------------------------------------------------------------

triplets <- character(0)

emit <- function(id, law, operation, graph_name, args, input, expected, tolerance,
                 status = "exact", reason = "") {
  row <- paste(id, law, operation, graph_name, args, input, expected, num(tolerance), status, reason, sep = "\t")
  if (grepl("\n", row, fixed = TRUE)) stop("triplet rows must be single lines")
  triplets <<- c(triplets, row)
}

compile <- function(graph_name, source, target, ...) {
  suppressWarnings(compile_projector(g(graph_name), hash(graph_name, source), hash(graph_name, target), ...))
}

apply_p <- function(p, x) {
  out <- apply_projector(p, x)
  if (is.null(dim(out))) matrix(out, ncol = 1) else out
}

# -- identity law: F(id) = I ------------------------------------------------
p <- compile("laws", "A", "A")
x <- probe(24, 2)
emit("id-01", "identity", "compile", "laws", "source=A target=A",
     fields(x = enc_dense(x, ord("laws", "A"))),
     fields(matrix = enc_sparse(p@matrix, ord("laws", "A"), ord("laws", "A")),
            forward = enc_dense(apply_p(p, x), ord("laws", "A")), coverage = enc_scalar(p@coverage)),
     1e-12)

# -- compiled operators (single interpolation through the route) -------------
for (case in list(c("op-01", "A", "S"), c("op-02", "A", "R"), c("op-03", "B", "R"), c("op-04", "A", "C"), c("op-05", "A", "Q"))) {
  p <- compile("laws", case[[2]], case[[3]])
  x <- probe(n_elements(dom("laws", case[[2]])), 2, salt = 3L)
  path <- find_path(g("laws"), hash("laws", case[[2]]), hash("laws", case[[3]]))
  emit(case[[1]], "operator", "compile", "laws", sprintf("source=%s target=%s", case[[2]], case[[3]]),
       fields(x = enc_dense(x, ord("laws", case[[2]]))),
       fields(path = path_ids("laws", path), matrix = enc_sparse(p@matrix, ord("laws", case[[3]]), ord("laws", case[[2]])),
              forward = enc_dense(apply_p(p, x), ord("laws", case[[3]])),
              coverage = enc_scalar(p@coverage)),
       1e-12)
}

# -- composition law: F(g . f) = F(g) . F(f) ---------------------------------
# Integer shifts: resampling twice equals resampling once, so the law holds.
p_ab <- compile("laws", "A", "B")
p_bc <- compile("laws", "B", "C")
p_ac <- compile("laws", "A", "C")
x <- probe(24, 1, salt = 5L)
emit("comp-01", "composition", "compose", "laws", "source=A mid=B target=C",
     fields(x = enc_dense(x, ord("laws", "A"))),
     fields(direct = enc_dense(apply_p(p_ac, x), ord("laws", "C")),
            composed = enc_dense(apply_p(compose_projectors(p_ab, p_bc), x), ord("laws", "C")),
            composed_matrix = enc_sparse(compose_projectors(p_ab, p_bc)@matrix, ord("laws", "C"), ord("laws", "A")),
            law_holds = enc_bool(isTRUE(all.equal(apply_p(p_ac, x), apply_p(compose_projectors(p_ab, p_bc), x), tolerance = 0)))),
     1e-12)
# Fractional shifts: one interpolation through the fused route differs from two
# chained interpolations; both values are recorded, and the gap is the law's
# known failure for resampling operators.
p_as <- compile("laws", "A", "S")
p_sq <- compile("laws", "S", "Q")
p_aq <- compile("laws", "A", "Q")
x <- probe(24, 1, salt = 7L)
direct <- apply_p(p_aq, x)
composed <- apply_p(compose_projectors(p_as, p_sq), x)
emit("comp-02", "composition", "compose", "laws", "source=A mid=S target=Q",
     fields(x = enc_dense(x, ord("laws", "A"))),
     fields(direct = enc_dense(direct, ord("laws", "Q")), composed = enc_dense(composed, ord("laws", "Q")),
            composed_matrix = enc_sparse(compose_projectors(p_as, p_sq)@matrix, ord("laws", "Q"), ord("laws", "A")),
            law_holds = enc_bool(max(abs(direct - composed)) < 1e-12)),
     1e-12)

# -- adjoint law: <Px, y> = <x, P^T y> ---------------------------------------
for (case in list(c("adj-01", "A", "R"), c("adj-02", "A", "S"))) {
  p <- compile("laws", case[[2]], case[[3]])
  x <- probe(ncol(p@matrix), 2, salt = 11L)
  y <- probe(nrow(p@matrix), 2, salt = 13L)
  px <- apply_p(p, x)
  pty <- apply_p(projector_transpose(p), y)
  emit(case[[1]], "adjoint", "adjoint", "laws", sprintf("source=%s target=%s", case[[2]], case[[3]]),
       fields(x = enc_dense(x, ord("laws", case[[2]])), y = enc_dense(y, ord("laws", case[[3]]))),
       fields(forward = enc_dense(px, ord("laws", case[[3]])), adjoint = enc_dense(pty, ord("laws", case[[2]])),
              lhs = enc_scalar(sum(px * y)), rhs = enc_scalar(sum(x * pty))),
       1e-10)
}

# -- ROI restriction: restricted rows equal the full operator's rows ---------
roi <- c(5L, 2L, 11L)
p <- compile("laws", "A", "R", roi = roi)
full <- compile("laws", "A", "R")
x <- probe(24, 1, salt = 17L)
emit("roi-01", "roi-restriction", "compile-roi", "laws", paste0("source=A target=R roi=", enc_rows(ord("laws", "R")[roi])),
     fields(x = enc_dense(x, ord("laws", "A"))),
     fields(forward = enc_dense(apply_p(p, x)[roi, , drop = FALSE]),
            full_forward = enc_dense(apply_p(full, x)[roi, , drop = FALSE]),
            coverage = enc_scalar(p@coverage), n_rows = enc_int(nrow(p@matrix))),
     1e-12,
     "deviation:roi-shape",
     "neurofunctor keeps every target row and leaves rows outside the ROI empty; ScalaFIM returns only the ROI rows, in ROI order. Expected values are neurofunctor's ROI rows; nothing else is compared.")

# -- round trip, operator level: A -> T -> A through the exact inverse -------
# T sits inside A, so A -> T is interior everywhere. The way back is restricted
# to the A rows whose centres pull back strictly inside T's grid, so neither
# leg touches a grid edge and the round trip is compared exactly.
p_at <- compile("laws", "A", "T")
t_domain <- dom("laws", "T")
a_world <- world_coords(dom("laws", "A"))
t_voxels <- a_world - matrix(c(0.4, 0.3, 0), nrow(a_world), 3, byrow = TRUE)
interior <- which(apply(t_voxels, 1, function(v) all(v >= 0 & v <= (t_domain@geometry@dim - 1))))
p_ta <- compile("laws", "T", "A", use_inverses = TRUE, roi = interior)
x <- probe(24, 1, salt = 19L)
back_path <- find_path_with_inverses(g("laws"), hash("laws", "T"), hash("laws", "A"))
emit("rt-01", "round-trip", "round-trip-operator", "laws", paste0("source=A via=T roi=", enc_rows(ord("laws", "A")[interior])),
     fields(x = enc_dense(x, ord("laws", "A"))),
     fields(there = path_ids("laws", find_path(g("laws"), hash("laws", "A"), hash("laws", "T"))),
            back = path_ids("laws", back_path),
            back_matrix = enc_sparse(p_ta@matrix[interior, , drop = FALSE], cols = ord("laws", "T")),
            round_trip = enc_dense(apply_p(p_ta, apply_p(p_at, x))[interior, , drop = FALSE])),
     1e-12)

# -- round trip, Field level: views re-express the root, not the view --------
f <- Field(data = probe(24, 2, salt = 23L), domain = dom("laws", "A"), graph = g("laws"))
there <- to(f, dom("laws", "S"))
back <- to(there, dom("laws", "A"))
emit("rt-02", "round-trip", "field-reexpress", "laws", "root=A via=S",
     fields(x = enc_dense(probe(24, 2, salt = 23L), ord("laws", "A"))),
     fields(view = enc_dense(suppressWarnings(get_data(there)), ord("laws", "S")),
            round_trip = enc_dense(suppressWarnings(get_data(back)), ord("laws", "A"))),
     1e-12)

# -- round trip, coordinate level (SpatialQc.roundTrip) ----------------------
coordinate_round_trip <- function(id, graph_name, reason_status = "exact", reason = "") {
  there <- find_path_with_inverses(g(graph_name), hash(graph_name, "A"), hash(graph_name, "B"))
  back <- find_path_with_inverses(g(graph_name), hash(graph_name, "B"), hash(graph_name, "A"))
  loop <- c(there, back)
  pulled <- transform_path(loop, world_probes)
  d <- sqrt(rowSums((pulled - world_probes)^2))
  emit(id, "round-trip", "round-trip-coordinates", graph_name, "a=A b=B",
       fields(probes = enc_points(world_probes)),
       fields(path = paste0(path_ids(graph_name, there), "|", sub("^ids:", "", path_ids(graph_name, back))),
              used_inverses = enc_bool(isTRUE(attr(there, "used_inverses")) || isTRUE(attr(back, "used_inverses"))),
              pulled = enc_points(pulled), max = enc_scalar(max(d)), rms = enc_scalar(sqrt(mean(d^2)))),
       1e-12, reason_status, reason)
}
coordinate_round_trip("rt-03", "loop")
coordinate_round_trip("rt-04", "loop2")

# -- commutativity (SpatialQc.commutes / commutativity) ----------------------
routes <- all_paths(g("diamond"), hash("diamond", "A"), hash("diamond", "D"), max_paths = 10)
costs <- vapply(routes, function(r) sum(vapply(r, function(m) m@cost, numeric(1))), numeric(1))
pulled <- lapply(routes, function(r) transform_path(r, world_probes))
cheapest <- which.min(costs)
route_fields <- character(0)
for (i in seq_along(routes)) {
  d <- sqrt(rowSums((pulled[[i]] - pulled[[cheapest]])^2))
  route_fields <- c(route_fields, sprintf(
    "r%d_path=%s r%d_cost=%s r%d_pulled=%s r%d_max_vs_cheapest=%s r%d_rms_vs_cheapest=%s",
    i, path_ids("diamond", routes[[i]]), i, enc_scalar(costs[[i]]), i, enc_points(pulled[[i]]),
    i, enc_scalar(max(d)), i, enc_scalar(sqrt(mean(d^2)))
  ))
}
emit("comm-01", "commutativity", "commutativity", "diamond", "source=A target=D max_paths=10",
     fields(probes = enc_points(world_probes)),
     paste(c(fields(routes = enc_int(length(routes)), cheapest = enc_int(cheapest - 1L)), route_fields), collapse = " "),
     1e-12)

# -- all_paths ordering and cap ------------------------------------------------
capped <- all_paths(g("diamond"), hash("diamond", "A"), hash("diamond", "D"), max_paths = 1)
emit("paths-01", "commutativity", "all-paths", "diamond", "source=A target=D max_paths=1",
     "none=text:none",
     fields(capped = path_ids("diamond", capped[[1]]),
            uncapped = paste0("routes:", paste(vapply(routes, function(r) sub("^ids:", "", path_ids("diamond", r)), character(1)), collapse = "/"))),
     0,
     "deviation:all-paths-order",
     "neurofunctor lists routes in igraph depth-first order and truncates to max_paths before any ranking; ScalaFIM sorts by cost (then length, then ids) before capping. The uncapped route sets must be equal; the capped route is compared as ScalaFIM's cheapest, and neurofunctor's first-found route is kept for the record.")

# -- projection metrics --------------------------------------------------------
metric_fields <- function(m) {
  fields(
    n_target = enc_int(m$n_target), n_source = enc_int(m$n_source), nnz = enc_int(m$nnz),
    nnz_per_row = enc_scalar(m$nnz_per_row), coverage = enc_scalar(m$coverage),
    sample_n = enc_int(m$row_sums$sample_n), mean = enc_scalar(m$row_sums$mean),
    min = enc_scalar(m$row_sums$min), max = enc_scalar(m$row_sums$max),
    frac_gt_half = enc_scalar(m$row_sums$frac_gt_0.5), frac_near_one = enc_scalar(m$row_sums$frac_close_to_1),
    any_na = enc_bool(m$weights$any_na), any_negative = enc_bool(m$weights$any_negative),
    w_min = enc_scalar(m$weights$min), w_max = enc_scalar(m$weights$max)
  )
}
all_rows_reason <- "neurofunctor samples min(sample_rows, n_target) random rows (default 1000); ScalaFIM always uses every row. The generator passes sample_rows = n_target, so neurofunctor covers every row and the summaries are compared exactly."
p <- compile("laws", "A", "R")
emit("qc-01", "projection-metrics", "projection-metrics", "laws", "source=A target=R",
     "none=text:none", metric_fields(projection_metrics(p, sample_rows = nrow(p@matrix), seed = 1L)), 1e-12,
     "deviation:projection-metrics-rows", all_rows_reason)
p <- compile("boundary", "A", "B")
emit("qc-02", "projection-metrics", "projection-metrics", "boundary", "source=A target=B",
     "none=text:none",
     paste(fields(matrix = enc_sparse(p@matrix, ord("boundary", "B"), ord("boundary", "A"))),
           metric_fields(projection_metrics(p, sample_rows = nrow(p@matrix), seed = 1L))),
     1e-12,
     "deviation:trilinear-boundary",
     "The last x column of B pulls back to x = 3.5 on a 4-voxel source axis. neurofunctor drops those rows (row sum 0, coverage 0.75); ScalaFIM renormalises the in-grid corner and reports the rows as partially covered (row sum 1, covered-row fraction 1). Rows neurofunctor covers are compared exactly; the metrics that depend on the dropped rows are asserted as the declared difference.")

# -- hybrid assembly -----------------------------------------------------------
hy <- suppressWarnings(compile_to_hybrid(g("laws"), hash("laws", "A"),
                                         list(left = hash("laws", "S"), right = hash("laws", "R"), shifted = hash("laws", "C"))))
x <- probe(24, 2, salt = 29L)
y <- probe(nrow(hy$projector@matrix), 2, salt = 31L)
emit("hy-01", "hybrid", "to-hybrid", "laws", "source=A parts=left:S,right:R,shifted:C",
     fields(x = enc_dense(x, ord("laws", "A")), y = enc_dense(y, ord("laws", "S", "R", "C"))),
     fields(matrix = enc_sparse(hy$projector@matrix, ord("laws", "S", "R", "C"), ord("laws", "A")),
            forward = enc_dense(apply_p(hy$projector, x), ord("laws", "S", "R", "C")),
            adjoint = enc_dense(apply_p(projector_transpose(hy$projector), y), ord("laws", "A")),
            coverage = enc_scalar(hy$projector@coverage)),
     1e-12)
hy <- suppressWarnings(compile_from_hybrid(g("laws"), list(a = hash("laws", "A"), b = hash("laws", "B")), hash("laws", "S")))
x <- probe(48, 1, salt = 37L)
y <- probe(nrow(hy$projector@matrix), 1, salt = 41L)
emit("hy-02", "hybrid", "from-hybrid", "laws", "parts=a:A,b:B target=S",
     fields(x = enc_dense(x, ord("laws", "A", "B")), y = enc_dense(y, ord("laws", "S"))),
     fields(matrix = enc_sparse(hy$projector@matrix, ord("laws", "S"), ord("laws", "A", "B")),
            forward = enc_dense(apply_p(hy$projector, x), ord("laws", "S")),
            adjoint = enc_dense(apply_p(projector_transpose(hy$projector), y), ord("laws", "A", "B")),
            coverage = enc_scalar(hy$projector@coverage)),
     1e-12)
# neurofunctor has no block-diagonal compile; the oracle is Matrix::bdiag of
# neurofunctor's own per-part projectors.
p_left <- compile("laws", "A", "S")
p_right <- compile("laws", "B", "R")
bd <- bdiag(p_left@matrix, p_right@matrix)
x <- probe(48, 1, salt = 43L)
y <- probe(nrow(bd), 1, salt = 47L)
emit("hy-03", "hybrid", "block-diagonal", "laws", "parts=left:A>S,right:B>R",
     fields(x = enc_dense(x, ord("laws", "A", "B")), y = enc_dense(y, ord("laws", "S", "R"))),
     fields(matrix = enc_sparse(bd, ord("laws", "S", "R"), ord("laws", "A", "B")),
            forward = enc_dense(bd %*% x, ord("laws", "S", "R")),
            adjoint = enc_dense(t(bd) %*% y, ord("laws", "A", "B")),
            coverage = enc_scalar(mean(c(p_left@coverage, p_right@coverage)))),
     1e-12)

# -- backprojection ------------------------------------------------------------
root_data <- probe(24, 2, salt = 53L)
f <- Field(data = root_data, domain = dom("laws", "A"), graph = g("laws"))
view <- to(f, dom("laws", "S"))
view_data <- suppressWarnings(get_data(view))
back <- suppressWarnings(get_data(backproject(view)))
emit("bp-01", "backprojection", "backproject", "laws", "root=A view=S method=shortest",
     fields(root = enc_dense(root_data, ord("laws", "A"))),
     fields(view = enc_dense(view_data, ord("laws", "S")), backprojected = enc_dense(back, ord("laws", "A"))),
     1e-12,
     "deviation:backproject-inverse-setting",
     "neurofunctor compiles the backprojection with use_inverses = TRUE whatever the view's method; ScalaFIM reuses the view's own inverse setting (FALSE here). Routing is forward-first, so both choose the forward route and the values are compared exactly; the test also asserts ScalaFIM compiled without inverses.")

root_data <- probe(24, 2, salt = 59L)
f <- Field(data = root_data, domain = dom("inverse", "A"), graph = g("inverse"))
view <- to(f, dom("inverse", "S"), method = "inverse_ok")
view_data <- suppressWarnings(get_data(view))
back <- suppressWarnings(get_data(backproject(view)))
emit("bp-02", "backprojection", "backproject", "inverse", "root=A view=S method=inverse_ok",
     fields(root = enc_dense(root_data, ord("inverse", "A"))),
     fields(path = path_ids("inverse", find_path_with_inverses(g("inverse"), hash("inverse", "A"), hash("inverse", "S"))),
            view = enc_dense(view_data, ord("inverse", "S")), backprojected = enc_dense(back, ord("inverse", "A"))),
     1e-12)

forward_only <- tryCatch(suppressWarnings(get_data(to(f, dom("inverse", "S"), method = "shortest"))), error = function(e) NULL)
emit("bp-03", "backprojection", "view", "inverse", "root=A view=S method=shortest",
     fields(root = enc_dense(root_data, ord("inverse", "A"))),
     if (is.null(forward_only)) "view=error:nopath" else fields(view = enc_dense(forward_only, ord("inverse", "S"))),
     0)

# ---------------------------------------------------------------------------
# Write
# ---------------------------------------------------------------------------

write_lines <- function(name, header, rows) {
  lines <- c(header, rows)
  writeLines(lines, file.path(resource_dir, name), useBytes = TRUE)
  lines
}

domains_lines <- write_lines("domains.tsv", "graph\tdomain\tdims\taffine", domain_rows)
edges_lines <- write_lines(
  "edges.tsv",
  "graph\tedge\tsource\ttarget\tkind\tcost\tinverse\tquality\tpullback",
  edge_rows
)
triplet_lines <- write_lines(
  "triplets.tsv",
  "id\tlaw\toperation\tgraph\targs\tinput\texpected\ttolerance\tstatus\treason",
  triplets
)

git_value <- function(...) {
  out <- tryCatch(system2("git", c("-C", source_dir, ...), stdout = TRUE, stderr = FALSE), error = function(e) character(0))
  if (length(out) == 0) "unknown" else out[[1]]
}
source_commit <- git_value("rev-parse", "HEAD")
source_dirty <- tryCatch(
  length(system2("git", c("-C", source_dir, "status", "--porcelain", "--", "R", "src", "DESCRIPTION"), stdout = TRUE)) > 0,
  error = function(e) NA
)

# The triplets come from the installed package; confirm its plain R functions
# are the ones in the recorded source checkout, so the commit is meaningful.
installed_matches_source <- local({
  ns <- asNamespace("neurofunctor")
  checked <- 0L
  for (file in list.files(file.path(source_dir, "R"), pattern = "[.]R$", full.names = TRUE)) {
    for (expr in parse(file, keep.source = FALSE)) {
      if (is.call(expr) && identical(expr[[1]], as.name("<-")) && is.name(expr[[2]]) &&
          is.call(expr[[3]]) && identical(expr[[3]][[1]], as.name("function"))) {
        name <- as.character(expr[[2]])
        if (!exists(name, envir = ns, inherits = FALSE)) stop("installed neurofunctor lacks ", name)
        installed <- get(name, envir = ns, inherits = FALSE)
        if (!is.function(installed)) next
        from_source <- eval(expr[[3]], envir = baseenv())
        if (!identical(deparse(body(installed)), deparse(body(from_source))) ||
            !identical(deparse(formals(installed)), deparse(formals(from_source)))) {
          stop("installed neurofunctor function ", name, " differs from ", source_dir)
        }
        checked <- checked + 1L
      }
    }
  }
  checked
})

json_string <- function(x) paste0("\"", gsub("\"", "\\\\\"", x), "\"")
manifest <- c(
  "{",
  sprintf("  \"generator\": %s,", json_string("tools/r-parity/generate_neurofunctor_law_fixtures.R")),
  sprintf("  \"packet\": %s,", json_string("STP P7.06")),
  sprintf("  \"r_version\": %s,", json_string(R.version.string)),
  sprintf("  \"neurofunctor_version\": %s,", json_string(as.character(packageVersion("neurofunctor")))),
  sprintf("  \"neurotransform_version\": %s,", json_string(as.character(packageVersion("neurotransform")))),
  sprintf("  \"matrix_version\": %s,", json_string(as.character(packageVersion("Matrix")))),
  sprintf("  \"igraph_version\": %s,", json_string(as.character(packageVersion("igraph")))),
  sprintf("  \"neurofunctor_source_commit\": %s,", json_string(source_commit)),
  sprintf("  \"neurofunctor_source_dirty\": %s,", if (isTRUE(source_dirty)) "true" else "false"),
  sprintf("  \"installed_functions_match_source\": %d,", installed_matches_source),
  "  \"index_origin\": 0,",
  "  \"element_order\": \"scalafim: z fastest (row-major x, y, z); converted from neurofunctor's x-fastest order\",",
  "  \"affine_convention\": \"pullback: target world -> source world, row-major 4x4\",",
  "  \"files\": [\"domains.tsv\", \"edges.tsv\", \"triplets.tsv\"],",
  sprintf("  \"triplets\": %d", length(triplets)),
  "}"
)
writeLines(manifest, file.path(resource_dir, "manifest.json"), useBytes = TRUE)

scala_lines <- function(name, lines) {
  if (any(grepl("\"\"\"", lines, fixed = TRUE))) stop("fixture lines may not contain triple quotes")
  c(
    sprintf("  val %s: Vector[String] =", name),
    "    Vector(",
    paste0("      \"\"\"", lines, "\"\"\"", c(rep(",", length(lines) - 1), "")),
    "    )",
    ""
  )
}

writeLines(
  c(
    "package scalafim.spatial.fixtures",
    "",
    "/** neurofunctor law triplets (STP P7.06), line for line as checked in under",
    "  * `modules/spatial/jvm/src/test/resources/scalafim/spatial/neurofunctor-laws/`.",
    "  *",
    "  * Generated by `tools/r-parity/generate_neurofunctor_law_fixtures.R`; do not edit by hand. Embedded so the shared",
    "  * suite loads them on the JVM and on Scala.js; `NeurofunctorLawFixtureFilesSuite` (JVM) checks the copies agree.",
    "  */",
    "object NeurofunctorLawFixtures:",
    scala_lines("manifest", manifest),
    scala_lines("domains", domains_lines),
    scala_lines("edges", edges_lines),
    head(scala_lines("triplets", triplet_lines), -1)
  ),
  scala_file,
  useBytes = TRUE
)

cat(sprintf("wrote %d triplets, %d domains, %d edges (neurofunctor %s, source %s)\n",
            length(triplets), length(domain_rows), length(edge_rows),
            as.character(packageVersion("neurofunctor")), source_commit))
