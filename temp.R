file_names <- c(
    "covid_binomial_fit.RDS",
    "timevarying_binomial_fit.RDS",
    "crosssectional_binomial_fit.RDS"
)

for (file_name in file_names) {
    file_path <- file.path("/Users/tntoan/Downloads", file_name)
    fit <- qs::qread(file_path)
    fit$params$other <- data.frame()
    qs::qsave(fit, file_path)
}