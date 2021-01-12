fread <- partial(
    data.table::fread,
    nThread = parallel::detectCores()
)

fwrite <- partial(
    data.table::fwrite,
    nThread = parallel::detectCores(),
    compress = "gzip"
)

read_data <- function(dir, filename) {
    data <- fread(
        sprintf("%s/%s.csv.gz", dir, filename),
        integer64 = "character"
    )

    return(data)
}

rename_from_dictionary <- function(data, dictionary) {
    vars_to_rename <- intersect(
        names(data),
        dictionary[["value"]]
    )

    temp_dictionary <- dictionary %>%
        filter(value %in% vars_to_rename) %>%
        deframe()

    data_renamed <- data %>%
        rename(!!temp_dictionary)

    return(data_renamed)
}

create_dictionary <- function(named_list_of_vars) {
    dictionary <- named_list_of_vars %>%
        enframe() %>%
        unnest(c(value))

    return(dictionary)
}

summarise_party_spending <- function(data){
    data_summary <- data %>%
        group_by(cod_ibge_6, year, party) %>%
        summarise(
            value_expense = sum(value_expense, na.rm = TRUE),
            .groups = "drop"
        )
    
    return(data_summary)
}
