require(data.table)
source("./helper_functions.R")

correlate_draws <- function(draw_data, corr_vector=c(),
                            corr_matrix=matrix(), corr_vars=c()) {
    check_input_format(draw_data, corr_vars, corr_vector, corr_matrix)
    correlation_matrix <- get_corr_matrix(corr_vector, corr_matrix)
    stacked_data <- stack_data(draw_data, corr_vars)
    correlated_stack <- correlate_stack(stacked_data, correlation_matrix)
    correlated_data <- unstack_data(correlated_stack)
    return(correlated_data)
}
