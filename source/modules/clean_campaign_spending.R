# ==============================================================================
# input: raw files for campaign spending by candidate from 2008 to 2020
# output: clean campaign spending 
# ==============================================================================
source(
    here::here("source/aux/globals.R")
)

source(
    here("source/aux/utils.R")
)

filenames <- list.files(
    here("data/raw/"),
    pattern = "campaign_spending",
    full.names = TRUE
)

campaign <- filenames %>%
    map(fread)

dictionary_cols <- list(
    cod_tse = c("sg_ue", "numero_ue", "sigla_da_ue")
) %>%
    create_dictionary()

campaign[[1]] %>%
    select(sg_ue) %>%
    rename_from_dictionary(dictionary_cols)
