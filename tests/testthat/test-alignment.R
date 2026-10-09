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

test_that("a one-record paper still pairs", {
  ai <- tibble::tibble(.rid = "a1", .paper = "P", sp = "Myotis lucifugus")
  gold <- tibble::tibble(.rid = "g1", .paper = "P", sp = "Myotis lucifigus")
  p <- align_records(ai, gold, "sp")
  expect_equal(nrow(p), 1L)
  expect_equal(p$kind, "pair")
  expect_equal(p$matcher, "model")
})

test_that("records with no counterpart are left unpaired, not forced together", {
  ai <- tibble::tibble(.rid = c("a1", "a2"), .paper = "P",
                       sp = c("Myotis lucifugus", "Eptesicus fuscus"))
  gold <- tibble::tibble(.rid = c("g1", "g2"), .paper = "P",
                         sp = c("little brown bat", "big brown bat"))
  p <- align_records(ai, gold, "sp")
  expect_equal(sum(p$kind == "pair"), 0L)
  expect_setequal(p$kind, c("ai_only", "gold_only"))
})

test_that("a gold row the extraction missed stays gold-only", {
  # Each paper's leftover gold row shares nothing with the AI rows, so it must
  # not be paired with whichever AI row is free.
  species <- c("Myotis lucifugus", "Eptesicus fuscus", "Tadarida brasiliensis",
               "Nycticeius humeralis", "Perimyotis subflavus", "Lasiurus borealis")
  ai <- tibble::tibble(.rid = paste0("a", 1:6),
                       .paper = rep(c("P1", "P2", "P3"), each = 2),
                       sp = species,
                       loc = rep(c("Ohio", "Texas", "Florida"), each = 2))
  gold <- ai[c(1, 3, 5), ]
  gold$.rid <- paste0("g", 1:3)
  extra <- tibble::tibble(.rid = paste0("g", 4:6), .paper = c("P1", "P2", "P3"),
                          sp = c("Artibeus jamaicensis", "Desmodus rotundus",
                                 "Carollia perspicillata"),
                          loc = c("Peru", "Chile", "Brazil"))
  p <- align_records(ai, dplyr::bind_rows(gold, extra), c("sp", "loc"))
  expect_equal(sum(p$kind == "pair"), 3L)
  expect_setequal(p$gold_rid[p$kind == "gold_only"], c("g4", "g5", "g6"))
  expect_setequal(p$ai_rid[p$kind == "ai_only"], c("a2", "a4", "a6"))
})

test_that("identical records score as certain matches in every block", {
  # fastLink, applied per block, looked agreement probabilities up by the
  # order the levels appeared in the block, and scored such pairs near zero.
  fx <- fixture_run()
  p <- fx$pairs[fx$pairs$kind == "pair", ]
  expect_true(all(p$posterior > 0.9))
  expect_equal(p$gold_rid[match("a00011", p$ai_rid)], "g00010")
  # P10: the AI row is Rousettus aegyptiacus; so is g00012, not g00013.
  expect_equal(p$gold_rid[match("a00012", p$ai_rid)], "g00012")
})

test_that("min_posterior controls how sure a link must be", {
  fx <- fixture_run()
  none <- align_records(fx$ai, fx$gold, fx$linkage, fx$paper_map,
                        min_posterior = 1.01)
  expect_equal(sum(none$kind == "pair"), 0L)
})

test_that("the linkage model says what agreement on each field is worth", {
  fx <- fixture_run()
  m <- fit_linkage_model(fx$ai, fx$gold, fx$linkage, fx$paper_map)
  expect_s3_class(m, "ecoeval_linkage_model")
  expect_equal(m$fields, fx$linkage)
  for (k in seq_along(m$fields)) {
    expect_gt(m$m[[k]][["agree"]], m$u[[k]][["agree"]])
  }
  expect_output(print(m), "linkage model")
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

test_that("a rejection frees a record to pair with its real partner", {
  ai <- tibble::tibble(.rid = c("a1", "a2"), .paper = "P",
                       sp = c("Myotis lucifugus", "Myotis lucifugus"),
                       loc = c("Ohio", "Ohio"))
  gold <- tibble::tibble(.rid = "g1", .paper = "P", sp = "Myotis lucifugus",
                         loc = "Ohio")
  first <- align_records(ai, gold, c("sp", "loc"))
  taken <- first$ai_rid[first$kind == "pair"]
  out <- align_records(ai, gold, c("sp", "loc"),
                       rejected = tibble::tibble(ai_rid = taken, gold_rid = "g1"))
  expect_equal(out$ai_rid[out$kind == "pair"], setdiff(c("a1", "a2"), taken))
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
