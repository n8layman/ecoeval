test_that("alignment is one-to-one and leftovers become orphans", {
  fx <- fixture_run()
  p <- fx$pairs
  expect_equal(anyDuplicated(stats::na.omit(p$ai_rid)), 0L)
  expect_equal(anyDuplicated(stats::na.omit(p$gold_rid)), 0L)
  expect_setequal(unique(p$kind), c("pair", "ai_only", "gold_only"))
})

test_that("every in-scope record from both sides appears exactly once", {
  fx <- fixture_run()
  in_ai <- fx$ai$.rid[fx$ai$.paper %in% fx$scope$papers]
  in_gold <- fx$gold$.rid[fx$gold$.paper %in% fx$scope$papers]
  expect_setequal(stats::na.omit(fx$pairs$ai_rid), in_ai)
  expect_setequal(stats::na.omit(fx$pairs$gold_rid), in_gold)
})

test_that("a pooled model pairs correctly inside a two-record block", {
  # Fitting the EM per paper would give it two records to learn from, and on
  # two records it will confidently pair the wrong ones.
  species <- c("Myotis lucifugus", "Eptesicus fuscus", "Tadarida brasiliensis",
               "Nycticeius humeralis", "Perimyotis subflavus", "Lasiurus borealis")
  ai <- tibble::tibble(.rid = paste0("a", 1:6),
                       .paper = rep(c("P1", "P2", "P3"), each = 2),
                       sp = species,
                       loc = rep(c("Ohio", "Texas", "Florida"), each = 2))
  gold <- ai
  gold$.rid <- paste0("g", 1:6)
  gold$sp[1] <- "Myotis lucifigus"
  gold <- gold[c(2, 1, 3:6), ]

  p <- align_records(ai, gold, c("sp", "loc"))
  matched <- p[p$kind == "pair", ]
  expect_equal(nrow(matched), 6L)
  expect_equal(matched$gold_rid[match("a1", matched$ai_rid)], "g1")
  expect_equal(matched$gold_rid[match("a2", matched$ai_rid)], "g2")
})

test_that("the matcher survives blocks fastLink cannot model", {
  ai <- tibble::tibble(.rid = "a1", .paper = "P", sp = "Myotis lucifugus")
  gold <- tibble::tibble(.rid = "g1", .paper = "P", sp = "Myotis lucifigus")
  p <- align_records(ai, gold, "sp")
  expect_equal(nrow(p), 1L)
  expect_equal(p$kind, "pair")
  expect_equal(p$matcher, "similarity")
})

test_that("the matcher is permissive -- it proposes even when identifiers disagree", {
  ai <- tibble::tibble(.rid = c("a1", "a2"), .paper = "P",
                       sp = c("Myotis lucifugus", "Eptesicus fuscus"))
  gold <- tibble::tibble(.rid = c("g1", "g2"), .paper = "P",
                         sp = c("little brown bat", "big brown bat"))
  p <- align_records(ai, gold, "sp")
  expect_equal(sum(p$kind == "pair"), 2L)
})

test_that("records outside the paper map never pair", {
  ai <- tibble::tibble(.rid = "a1", .paper = "P1", sp = "x")
  gold <- tibble::tibble(.rid = "g1", .paper = "P2", sp = "x")
  p <- align_records(ai, gold, "sp",
                     paper_map = tibble::tibble(ai_paper = "P1", gold_paper = "P1"))
  expect_equal(p$kind, "ai_only")
})

test_that("a rejected link splits the pair into two orphans", {
  fx <- fixture_run()
  first <- fx$pairs[fx$pairs$kind == "pair", ][1, ]
  out <- align_records(fx$ai, fx$gold, fx$linkage, fx$paper_map,
                       rejected = first[, c("ai_rid", "gold_rid")])
  expect_false(paste(first$ai_rid, first$gold_rid) %in%
                 paste(out$ai_rid, out$gold_rid))
  expect_true(first$ai_rid %in% out$ai_rid[out$kind == "ai_only"])
  expect_true(first$gold_rid %in% out$gold_rid[out$kind == "gold_only"])
})

test_that("a manual link wins over any automatic link it conflicts with", {
  fx <- fixture_run()
  orphan_ai <- fx$pairs$ai_rid[fx$pairs$kind == "ai_only"][1]
  taken_gold <- fx$pairs$gold_rid[fx$pairs$kind == "pair"][1]
  added <- tibble::tibble(ai_rid = orphan_ai, gold_rid = taken_gold)

  out <- align_records(fx$ai, fx$gold, fx$linkage, fx$paper_map, added = added)
  expect_equal(out$ai_rid[match(taken_gold, out$gold_rid)], orphan_ai)
  expect_equal(anyDuplicated(stats::na.omit(out$gold_rid)), 0L)
})

test_that("the granularity check catches a key that cannot separate records", {
  fx <- fixture_run()
  g <- granularity_check(fx$gold, fx$linkage, "gold")
  # The two P09 gold rows differ only by year, which is not in the linkage key.
  expect_true(any(g$paper == "10.1000/p09"))
  expect_equal(g$n[g$paper == "10.1000/p09"], 2L)
})

test_that("the granularity check runs on normalised values", {
  recs <- tibble::tibble(.rid = c("r1", "r2"), .paper = "P",
                         sp = c("Myotis lucifugus", "myotis  LUCIFUGUS"))
  expect_equal(nrow(granularity_check(recs, "sp")), 1L)
})

test_that("identifiability reports records indistinguishable on the key", {
  fx <- fixture_run()
  ident <- identifiability_check(fx$gold, fx$linkage, "gold")
  expect_true(all(ident$n_indistinguishable >= 2L))
})

test_that("paper alignment falls back to identifiers when nothing links", {
  a <- tibble::tibble(.paper = c("x", "y"))
  b <- tibble::tibble(.paper = c("y", "z"))
  pm <- align_papers(a, b, fields = character(0))
  expect_equal(pm$ai_paper, "y")
  expect_true(all(pm$accepted))
})

test_that("the matcher is reproducible run to run", {
  # fastLink clusters string distances internally and that clustering is
  # randomly initialised, so this only holds because ecoeval pins the seed.
  fx <- fixture_run()
  a <- align_records(fx$ai, fx$gold, fx$linkage, fx$paper_map)
  b <- align_records(fx$ai, fx$gold, fx$linkage, fx$paper_map)
  expect_equal(paste(a$ai_rid, a$gold_rid), paste(b$ai_rid, b$gold_rid))
})

test_that("matching does not disturb the caller's random number stream", {
  fx <- fixture_run()
  set.seed(1); expected <- runif(3)
  set.seed(1); invisible(align_records(fx$ai, fx$gold, fx$linkage, fx$paper_map))
  expect_equal(runif(3), expected)
})

test_that("a different seed is allowed to give a different answer", {
  fx <- fixture_run()
  a <- align_records(fx$ai, fx$gold, fx$linkage, fx$paper_map, seed = 1L)
  expect_s3_class(a, "tbl_df")
  expect_equal(nrow(a), nrow(fx$pairs))
})
