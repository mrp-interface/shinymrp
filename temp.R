sample_data <- example_sample_data(
    is_timevar = TRUE,
    is_aggregated = TRUE,
    special_case = NULL,
    family = "binomial"
)
View(sample_data)
workflow <- mrp_workflow()

workflow$preprocess(
    sample_data,
    is_timevar = TRUE,
    is_aggregated = TRUE,
    special_case = NULL,
    family = "binomial"
)

View(workflow$preprocessed_data())
print("Preprocessing complete.")

pstrat_data <- example_pstrat_data()
workflow$load_pstrat(pstrat_data)