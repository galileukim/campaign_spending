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

# ---------------------------------------------------------------------------- #
filenames <- list.files(
    here("data/raw/"),
    pattern = "campaign_spending",
    full.names = TRUE
)

# read in files
campaign <- filenames %>%
    map(fread, encoding = "Latin-1")

# rename columns to standard form
list_vars <- list(
    cod_tse = c("sg_ue", "numero_ue", "sigla_da_ue"),
    candidate_name = c("nm_candidato", "nome_candidato"),
    cpf_candidate = c("cd_cpf_adm", "cpf_do_candidato"),
    position = c("ds_cargo", "cargo"),
    cnpj = c("cd_cpf_cnpj_fornecedor", "cpf_cnpj_do_fornecedor"),
    name_beneficiary = c("nm_fornecedor", "nome_do_fornecedor_receita_federal"),
    party = c("sg_partido", "sigla_partido"),
    value_expense = c("vr_despesa", "valor_despesa"),
    date_expense = c("dt_despesa", "data_da_despesa")
)

dictionary_cols <- create_dictionary(list_vars)

campaign <- campaign %>%
    map(
        compose(
            ~ rename_from_dictionary(., dictionary_cols),
            ~ select(., names(list_vars)),
            ~ mutate(., across(where(is.character), ~ iconv(., "latin1", "ASCII"))),
            ~ mutate(., across(where(is.character), str_to_lower)),
            .dir = "forward"
        )
    )

