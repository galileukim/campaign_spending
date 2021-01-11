# ==============================================================================
# input: raw files for campaign spending by candidate from 2008 to 2020
# output: clean campaign spending, with deflated reais of 2002
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
    here("data/raw/"),
    pattern = "campaign_spending",
    full.names = TRUE
)

# read in files
campaign <- input_filenames %>%
    map(fread, encoding = "Latin-1")

ipca <- fread(
    here("data/raw/ipca.csv")
)

# ---------------------------------------------------------------------------- #
message("cleaning data")
# rename columns to standard form
list_vars <- list(
    cod_tse = c("sg_ue", "numero_ue", "sigla_da_ue"),
    candidate_name = c("nm_candidato", "nome_candidato"),
    cpf_candidate = c("cd_cpf_adm", "cpf_do_candidato", "nr_cpf_candidato"),
    position = c("ds_cargo", "cargo"),
    cpf_or_cnpj_supplier = c(
        "cd_cpf_cnpj_fornecedor", 
        "cpf_cnpj_do_fornecedor", 
        "nr_cpf_cnpj_fornecedor"
    ),
    name_beneficiary = c("nm_fornecedor", "nome_do_fornecedor"),
    party = c("sg_partido", "sigla_partido"),
    value_expense = c("vr_despesa", "valor_despesa", "vr_despesa_contratada"),
    date_expense = c("dt_despesa", "data_da_despesa")
)

dictionary_cols <- create_dictionary(list_vars)

# rename columns and convert special characters to ascii
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

# fix value of receipt joining with cpi (ipca)
campaign <- campaign %>%
    map2(
        seq(2008, 2020, 4),
        ~ mutate(.x, year = .y)
    ) %>%
    set_names(
        seq(2008, 2020, 4)
    )

campaign <- campaign %>%
    map(
        compose(
            ~ left_join(., ipca, by = "year"),
            ~ mutate(., 
                value_expense = str_replace(value_expense, ",", ".") %>%
                as.double
                )
        )
    )

# deflate to december 2002 reais
campaign <- campaign %>%
    map(
        ~ mutate(., value_expense = value_expense/ipca * 100) %>%
            select(-ipca) %>%
            round(2)
    )

# ---------------------------------------------------------------------------- #
message("writing out data")
# write-out
output_filenames <- str_replace(
    input_filenames,
    "raw",
    "clean"
)

pwalk(
    list(
        file = output_filenames,
        x = campaign
    ),
    fwrite
)