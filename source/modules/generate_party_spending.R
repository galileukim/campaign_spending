# ==============================================================================
# input: clean campaign spending data
# output: party-year aggregate spending
# ==============================================================================
source(
    here::here("source/aux/globals.R")
)

source(
    here("source/aux/utils.R")
)

# ---------------------------------------------------------------------------- #
message("importing data")
input_filenames <- list.files(
    here("data/clean/"),
    pattern = "campaign_spending",
    full.names = TRUE
)

# read in files
campaign <- input_filenames %>%
    map(fread)

election <- fread(
    here("data/raw/election_local.csv.gz"),
    select = c("cod_ibge_6", "election_year", "party", "vote")
)

election_2020 <- fread(
    here("data/raw/election_local_2020.csv"),
    encoding = "Latin-1"
) %>%
    transmute(
        cod_tse = SG_UE,
        party = SG_PARTIDO,
        vote = QT_VOTOS_NOMINAIS,
        year = 2020
    )

identifiers <- fread(
    here("data/raw/data_rosettastone.csv")
) %>%
    transmute(
        cod_tse = codetse,
        cod_ibge_6 = codeipea
    ) %>%
    mutate(
        across(where(is.character), str_to_lower)
    )

# ---------------------------------------------------------------------------- #
message("generate summary statistics")
campaign_party <- campaign %>% 
    map_dfr(summarise_party_spending)

# filter out na's (72 rows)
campaign_party <- campaign_party %>%
    filter(!is.na(cod_ibge_6))

# generate party vote by m unicipality year
vote_party <- election %>%
    rename(year = election_year) %>%
    filter(year >= 2008) %>%
    group_by(cod_ibge_6, party, year) %>%
    summarise(
        vote = sum(vote, na.rm = TRUE),
        .groups = "drop"
    )

vote_party <- vote_party %>%
    bind_rows(
        election_2020 %>% 
            left_join(
                identifiers,
                by = "cod_tse"
            ) %>%
            group_by(
                cod_ibge_6, party, year
            ) %>%
            summarise(
                vote = sum(vote, na.rm = TRUE),
                .groups = "drop"
            )
    ) %>%
    arrange(
        party, cod_ibge_6, year
    )

# join data
campaign_party_vote <- campaign_party %>%
    left_join(
        vote_party,
        by = c("cod_ibge_6", "party", "year")
    )

# ---------------------------------------------------------------------------- #
message("writing out data")

# write-out data
campaign_party_vote %>%
    fwrite(
        here("data/clean/campaign_party_vote.csv")
    )
