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

summarise_party_spending <- function(data) {
    data_summary <- data %>%
        filter(position == "prefeito") %>%
        group_by(cod_ibge_6, year, party) %>%
        summarise(
            value_expense = sum(value_expense, na.rm = TRUE),
            .groups = "drop"
        )

    return(data_summary)
}

# ---------------------------------------------------------------------------- #
# plotting aids
gg_boxplot <- function(data, x, y, grouping, lims) {
    plot <- data %>%
        ggplot(
            aes({{x}}, {{y}}),
        ) +
        geom_boxplot(
            aes(color = {{grouping}}),
            outlier.shape = NA
        ) +
        # scale_x_discrete(forcats::fct_rev({{x}})) +
        coord_flip(
            ylim = lims
        )

    return(plot)
}

gg_point <- function(data, x, y, grouping, lims) {
    plot <- data %>%
     ggplot() + 
    geom_point(
        aes({{x}}, {{y}}, color = {{grouping}}),
        position = position_dodge(0.5),
        size = 3
    ) +
    # scale_x_discrete(forcats::fct_rev({{x}})) +
    coord_flip(
        # ylim = lims
    )
}

