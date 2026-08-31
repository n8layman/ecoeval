# Builds the synthetic fixtures in inst/extdata/.
#
# These are not a demo dataset -- each row exists to exercise a specific case
# the evaluation has to get right. Run with:  Rscript data-raw/make_fixtures.R
#
# Cases covered, and where to find them:
#   near-miss spelling            P03 Myotis lucifugus / lucifigus
#   semantic-only equivalence     P05 common name against binomial
#   recurring out-of-enum value   commensalism, 4 times in the gold standard
#   one-off out-of-enum value     "predatoin", once -- a typo, not a category
#   AI-only record (a real FP)    P07
#   gold-only record (a real FN)  P08
#   granularity collapse          P09, two gold rows differing only by year
#   date format disagreement      P04, ISO against DD/MM/YYYY
#   numeric formatting            P02, "10" against "10.0"
#   fill-rate asymmetry           detection_method, filled by humans, rarely by AI
#   gold column outside schema    habitat_notes
#   paper the AI processed empty  P11 (AI paper list only)
#   paper the human read empty    P12 (gold paper list only)

suppressPackageStartupMessages({
  library(tibble)
  library(dplyr)
  library(jsonlite)
})

out_dir <- "inst/extdata"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- schema ----------------------------------------------------------------

schema <- list(
  `$schema` = "https://json-schema.org/draft/2020-12/schema",
  title = "Bat host-pathogen interaction records",
  type = "object",
  properties = list(
    records = list(
      type = "array",
      items = list(
        type = "object",
        `x-unique-fields` = list("bat_species_scientific_name", "interaction_type"),
        required = list("bat_species_scientific_name", "interaction_type"),
        properties = list(
          bat_species_scientific_name = list(
            type = "string",
            description = "Accepted scientific binomial of the bat, Genus species."
          ),
          interaction_type = list(
            type = "string",
            description = "The kind of interaction reported.",
            enum = list("predation", "roosting", "parasitism", "competition")
          ),
          location_country = list(
            type = "string",
            description = "Country in which the observation was made."
          ),
          year_observed = list(
            type = "integer",
            description = "Four-digit year of the observation."
          ),
          sample_size = list(
            type = "number",
            description = "Number of individuals observed."
          ),
          detection_method = list(
            type = "string",
            description = "How the interaction was detected.",
            enum = list("direct observation", "PCR", "serology", "camera trap")
          ),
          observation_date = list(
            type = "string",
            format = "date",
            description = "Date of the observation."
          ),
          all_supporting_source_sentences = list(
            type = "string",
            description = "Verbatim sentences from the paper supporting this record."
          )
        )
      )
    )
  )
)
write_json(schema, file.path(out_dir, "schema.json"), auto_unbox = TRUE,
           pretty = TRUE)

# ---- helper ----------------------------------------------------------------

rec <- function(doi, sp, kind, country, year, n, method, date, sentence) {
  tibble(
    doi = doi,
    bat_species_scientific_name = sp,
    interaction_type = kind,
    location_country = country,
    year_observed = year,
    sample_size = n,
    detection_method = method,
    observation_date = date,
    all_supporting_source_sentences = sentence
  )
}

# ---- AI records ------------------------------------------------------------

ai <- bind_rows(
  # P01 -- clean agreement on everything.
  rec("10.1000/p01", "Eptesicus fuscus", "roosting", "United States", 2019L, 24,
      "direct observation", "2019-06-14",
      "Big brown bats roosted in the attic throughout the survey period."),
  rec("10.1000/p01", "Tadarida brasiliensis", "predation", "Mexico", 2018L, 7,
      "camera trap", "2018-09-02",
      "Free-tailed bats were observed taking moths at the cave mouth."),

  # P02 -- numeric formatting only.
  rec("10.1000/p02", "Lasiurus borealis", "parasitism", "Canada", 2020L, 10,
      "PCR", "2020-05-11",
      "Ectoparasite loads were quantified for ten captured individuals."),

  # P03 -- near-miss spelling on the linkage field.
  rec("10.1000/p03", "Myotis lucifugus", "roosting", "United States", 2017L, 40,
      "direct observation", "2017-07-30",
      "Little brown bats occupied the bridge expansion joints."),

  # P04 -- same date, different format.
  rec("10.1000/p04", "Nycticeius humeralis", "predation", "United States", 2021L, 3,
      "camera trap", "2021-03-08",
      "Evening bats foraged over the pond at dusk."),

  # P05 -- binomial against a common name: semantic equivalence only.
  rec("10.1000/p05", "Perimyotis subflavus", "roosting", "United States", 2016L, 12,
      NA_character_, "2016-11-19",
      "Tricoloured bats were counted in the mine adit during hibernation surveys."),

  # P06 -- the AI asserts a value the human left blank, and gets the kind wrong.
  rec("10.1000/p06", "Molossus molossus", "competition", "Panama", 2019L, 5,
      NA_character_, "2019-04-22",
      "Velvety free-tailed bats displaced smaller species from roost crevices."),

  # P07 -- an AI record with no gold counterpart: a real false positive.
  rec("10.1000/p07", "Artibeus jamaicensis", "roosting", "Costa Rica", 2015L, 9,
      NA_character_, "2015-08-05",
      "Jamaican fruit bats were recorded in tent roosts."),
  rec("10.1000/p07", "Carollia perspicillata", "predation", "Costa Rica", 2015L, 2,
      NA_character_, "2015-08-06",
      "Seba's short-tailed bat visited Piper infructescences."),

  # P08 -- the AI missed one the human found.
  rec("10.1000/p08", "Desmodus rotundus", "parasitism", "Brazil", 2022L, 31,
      "serology", "2022-01-17",
      "Common vampire bats fed on cattle at the study ranch."),

  # P09 -- granularity: the human split by year, the AI did not.
  rec("10.1000/p09", "Pipistrellus pipistrellus", "roosting", "United Kingdom", 2020L, 60,
      "direct observation", "2020-06-01",
      "Common pipistrelles used the church roof across both survey seasons."),

  # P10 -- the AI invents an interaction type outside the enum.
  rec("10.1000/p10", "Rousettus aegyptiacus", "predation", "Uganda", 2018L, 15,
      "PCR", "2018-10-12",
      "Egyptian fruit bats were sampled at the cave entrance.")
)

# ---- gold records ----------------------------------------------------------

gold <- bind_rows(
  rec("10.1000/p01", "Eptesicus fuscus", "roosting", "USA", 2019L, 24,
      "direct observation", "2019-06-14",
      "The attic roost was occupied by big brown bats for the whole survey."),
  rec("10.1000/p01", "Tadarida brasiliensis", "predation", "Mexico", 2018L, 7,
      "camera trap", "2018-09-02",
      "Moths were taken by free-tailed bats at the cave entrance."),

  rec("10.1000/p02", "Lasiurus borealis", "parasitism", "Canada", 2020L, 10.0,
      "PCR", "2020-05-11",
      "Ten individuals were examined for ectoparasites."),

  rec("10.1000/p03", "Myotis lucifigus", "roosting", "USA", 2017L, 40,
      "direct observation", "2017-07-30",
      "Bridge expansion joints held a maternity colony of little brown bats."),

  rec("10.1000/p04", "Nycticeius humeralis", "predation", "USA", 2021L, 3,
      "camera trap", "08/03/2021",
      "Evening bats were filmed foraging over the pond after sunset."),

  rec("10.1000/p05", "tricolored bat", "roosting", "USA", 2016L, 12,
      "direct observation", "2016-11-19",
      "Hibernation surveys counted tricoloured bats in the adit."),

  # The human recorded commensalism -- a category the schema has no slot for.
  rec("10.1000/p06", "Molossus molossus", "commensalism", "Panama", 2019L, 5,
      "direct observation", "2019-04-22",
      "Velvety free-tailed bats shared roost crevices with smaller species."),

  rec("10.1000/p08", "Desmodus rotundus", "parasitism", "Brazil", 2022L, 31,
      "serology", "2022-01-17",
      "Vampire bats were observed feeding on cattle."),
  # ... and one more the AI never produced.
  rec("10.1000/p08", "Artibeus lituratus", "commensalism", "Brazil", 2022L, 4,
      "direct observation", "2022-01-18",
      "Great fruit-eating bats fed at the same troughs."),

  # P09 -- two gold rows that collapse onto one key.
  rec("10.1000/p09", "Pipistrellus pipistrellus", "roosting", "United Kingdom", 2020L, 35,
      "direct observation", "2020-06-01",
      "The 2020 season counted 35 pipistrelles in the church roof."),
  rec("10.1000/p09", "Pipistrellus pipistrellus", "roosting", "United Kingdom", 2021L, 25,
      "direct observation", "2021-06-04",
      "The 2021 season counted 25 pipistrelles in the same roost."),

  rec("10.1000/p10", "Rousettus aegyptiacus", "commensalism", "Uganda", 2018L, 15,
      "PCR", "2018-10-12",
      "Egyptian fruit bats were swabbed at the cave entrance."),

  # A typo, once: not a missing category.
  rec("10.1000/p10", "Hipposideros caffer", "predatoin", "Uganda", 2018L, 2,
      "PCR", "2018-10-13",
      "Sundevall's roundleaf bat was also sampled.")
)

# The gold standard carries a column the schema knows nothing about.
gold$habitat_notes <- c(
  "suburban attic", "limestone cave", "riparian woodland", "concrete bridge",
  "farm pond", "abandoned mine", "urban roof crevice", "cattle pasture",
  "cattle pasture", "church roof", "church roof", "cave entrance",
  "cave entrance"
)

# The AI rarely fills detection_method; the human always does. That asymmetry
# is a field-description finding, not a model failure.

# ---- paper lists -----------------------------------------------------------

ai_papers <- tibble(
  doi = c(sprintf("10.1000/p%02d", 1:11)),
  title = c(
    "Attic roosting in Eptesicus fuscus", "Ectoparasites of eastern red bats",
    "Bridge roosts of Myotis lucifugus", "Foraging behaviour of evening bats",
    "Hibernacula surveys in Appalachia", "Roost competition in Molossus",
    "Tent roosts in lowland Costa Rica", "Vampire bat feeding on livestock",
    "Church roof monitoring of pipistrelles", "Cave sampling of Rousettus",
    "A review with no extractable records"
  ),
  year = c(2019L, 2020L, 2017L, 2021L, 2016L, 2019L, 2015L, 2022L, 2021L, 2018L, 2020L)
)

gold_papers <- tibble(
  doi = c(sprintf("10.1000/p%02d", c(1:10, 12))),
  title = c(
    "Attic roosting in Eptesicus fuscus", "Ectoparasites of eastern red bats",
    "Bridge roosts of Myotis lucifugus", "Foraging behavior of evening bats",
    "Hibernacula surveys in Appalachia", "Roost competition in Molossus",
    "Tent roosts in lowland Costa Rica", "Vampire bat feeding on livestock",
    "Church roof monitoring of pipistrelles", "Cave sampling of Rousettus",
    "A methods paper the reviewer read and found nothing in"
  ),
  year = c(2019L, 2020L, 2017L, 2021L, 2016L, 2019L, 2015L, 2022L, 2021L, 2018L, 2014L)
)

# ---- write -----------------------------------------------------------------

readr::write_csv(ai, file.path(out_dir, "ai_records.csv"))
readr::write_csv(gold, file.path(out_dir, "gold_records.csv"))
readr::write_csv(ai_papers, file.path(out_dir, "ai_papers.csv"))
readr::write_csv(gold_papers, file.path(out_dir, "gold_papers.csv"))

# An ecoextract-shaped SQLite database, so the database path is exercised too.
db_path <- file.path(out_dir, "ai_records.db")
if (file.exists(db_path)) unlink(db_path)
con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
DBI::dbWriteTable(con, "records", as.data.frame(ai))
DBI::dbWriteTable(con, "documents", as.data.frame(ai_papers))
DBI::dbDisconnect(con)

message("Fixtures written to ", out_dir)
