require(data.table)
require(MASS)
require(stringr)
require(doParallel)
require(abind)
require(Matrix)

source("./helper_functions.R")


## Note: the datasets MUST be combined and melted first! The combined part more important 

## So far our function is only dealing with a single dataset, and that just might be easier: 
# to have the user supply a prepped data with very specific dims

#### Expected dataset for user:

## One dataset with the following variables, so that they can be passed on to our function:

## corr_vars : a (vector of) variable(s) which the user wants to induce correlation over
## other_vars : a (vector of) variable(s) which the user wants to not change correlation over
## a third variable: the dimension which the user wants to correlate over (usually draws)
## fourth variable: the actual data


correlate_draws <- function(draw_data, draw_names, corr_vector=c(),
                            corr_matrix=matrix(), corr_vars=c(), other_vars=c(), return_wide=F) {

	## Run checks
    check_input_format(draw_data, corr_vars, corr_vector, corr_matrix)
    
    ## Generating corr matrix
    correlation_matrix <- get_corr_matrix(corr_vector, corr_matrix)
    
    ## Stack the variables
    stacked_data <- stack_data(draw_data, corr_vars, other_vars)

    ## Convert to multidimensional array
    data_in_array <- to_array(stacked_data, corr_vars, other_vars)
    colnames(data_in_array) <- colnames(stacked_data)

    ## Correlate the data
    correlated_stack <- correlate_stack(data_in_array, correlation_matrix)
    
    ## Unstack the data
    correlated_data <- unstack_data(correlated_stack, corr_vars, other_vars, 
    								draw_name = colnames(data_in_array)[3], 
    								data_col_name = colnames(data_in_array)[4], 
    								wide_on_draws= return_wide)   

    return(correlated_data)
}
