require(assertthat)

check_input_format <- function(data, corr_vars, corr_vector, corr_matrix,
                               required_vars=c("location_id", "year_id",
                                               "age_group_id", "sex_id",
                                               "cause_id")) {
    # make sure all the required dimensions are present in the draw data
    cols <-  names(data)
    for (var in required_vars) {
      assert_that(var %in% cols, msg="Input data should contain location_id,
                                      year_id, age_group_id, sex_id, and
                                      cause_id for each draw")
    }
    # check that draws exist that are named staring with "draw_"
    draws <-  cols[grepl("draw_", cols)]
    assert_that(!is.null(draws), msg="Draw column names should be of the
                                      form 'draw_i', where i is an integer")
    # make sure the dimensions to correlate across are valid
    for (var in corr_vars) {
        assert_that(var %in% cols, msg="Possible dimensions to correlate over
                                        are location_id, year_id, age_group_id,
                                        sex_id, and cause_id")
    }
    if (!is.null(corr_vector)) {
        assert_that(setequal(names(corr_vector), corr_vars), msg="corr_vars
                                                                  should match
                                                                  the names in
                                                                  corr_vector")
        assert_that(is.numeric(corr_vector), msg="Correlation values should be
                                                  numeric")
    }
    # user should input either corr_vector or corr_matrix, but not both
    assert_that(is.null(corr_vector) | is.null(corr_matrix), msg="Input either
                                                                  corr_matrix or
                                                                  corr_vector,
                                                                  but not both")
    # check the format of the corr_matrix: row_names, order, etc
    if (!is.null(corr_matrix)) {
        # row names and column names should match and be in the same order
        assert_that(rownames(corr_matrix) ==  colnames(corr_matrix),
                    msg="Correlation matrix should be symmetric, so the row
                         names and column names should be the same")
        assert_that(isSymmetric(corr_matrix), msg="Correlation matrix should
                                                   be symmetric")
        # the dimensions in the corr_matrix should match the corresponding
        # dims in the data
        for (i in seq_along(corr_vars)) {
            var_vals <- unique(sapply(rownames(corr_matrix),
                                      function(x) {strsplit(x, ":")[[1]][i]}))
            var <- corr_vars[i]
            assert_that(unique(data[[var]]) ==  var_vals,
                        msg="Make sure that every combination of vars to corr
                             across is in your data is represented in your
                             correlation matrix")
        }
        expected_mat_size <- prod(sapply(corr_vars,
                                  function(x) {length(unique(data[[x]]))}))
        assert_that(expected_mat_size == nrow(corr_matrix))
    }
}


get_corr_matrix <- function(corr_vector, corr_matrix) {
    print("TODO: write this function")
}

stack_data <- function(data, corr_vars, other_vars) {    

	## This function will stack the variables we want to correlate and not correlate across

	## Collapse the vector of dimension names to be separated by '__' (variable names)
    corr_vars_name <- paste(corr_vars, collapse="__")
    other_vars_name <- paste(other_vars, collapse="__")
    
    ## Create those variables in the data table
    data[, paste0(corr_vars_name) := do.call(paste, c(data[, .SD, .SDcols = corr_vars], sep = "__")) ]
    data[, paste0(other_vars_name) := do.call(paste, c(data[, .SD, .SDcols = other_vars], sep = "__"))]
    
    ## Remove the non-stacked variables
    data[, paste0(corr_vars_name) := NULL]
    data[, paste0(other_vars_name) := NULL]

    ## Make sure we have a 4 column dataset
    assert_that(ncol(data) == 4, msg = "The dataset has other dimensions not used. Please make sure that there are no extra variables going in.")

    return(data)
}

to_array <- function(data, corr_vars, other_vars) {

	## Get a single vector with the names of the vars 
	var_melters <- c(paste(other_vars, collapse= "__"), paste(corr_vars, collapse= "__"))

	## Get colnames for each of the dimensions
	no_corr <- colnames(data)[1]
	yes_corr <- colnames(data)[2]
	corr_dim <- colnames(data)[3]
	data_col <- colnames(data)[4]

	## Convert to array
	return(reshape2::acast( get(no_corr) ~ get(yes_corr) ~ get(corr_dim), value.var = data_col))

}

correlate_stack <- function(data, corr_matrix) {
	    
	## Get dimension 2
	L <- dim(X)[2]

	## Get dimensions 3
	D <- dim(X)[3]

	## Sum of dims
	Xsum <- apply(X, c(2, 3), sum)

	## Simulate a multivariate normal distribution with the given correlation matrix from get_corr_matrix()
	mvdat <- mvrnorm(n=D, mu=0 * 1:L, Sigma=corr_matrix, empirical=TRUE)

	## Rank the simulated draws
	ranks <- apply(mvdat, 2, rank, ties.method="first")

	## Sort the sim draws
	sortedXsim <- apply(Xsum, 1, function(x) sort(x, index.return=TRUE)$ix)

	### Finally, rearrange our actual data according to those draws
	sortedX <- X
	for(i in 1:L){
	sortedX[,i,] <- X[,i,sortedXsim[,i]]
	}
	Xcorr <- sortedX
	for(i in 1:L){
	Xcorr[,i,] <- sortedX[,i,ranks[,i]]
	}

	return(data.table(melt(Xcorr)))

}



unstack_data <- function(data, corr_vars, other_vars, draw_name = "draw_num", data_col_name = "data", wide_on_draws = F) {
    
    
    ## Split the variables according to the length of the corring_over and not_corring_over vectors
        
    ## Clean up column names    
    setDT(data)[, paste0("not_corr", 1:length(other_vars)) := 
                                       tstrsplit(get(var_melters[1]), "__", type.convert = TRUE, fixed = TRUE)]
    setDT(data)[, paste0("to_corr", 1:length(corr_vars)) := 
                                       tstrsplit(get(var_melters[2]), "__", type.convert = TRUE, fixed = TRUE)]


    ## Keep what we need:
    data_processed <- data[, .SD, .SDcols = c(paste0("not_corr", 1:length(other_vars)),
                                  paste0("to_corr", 1:length(corr_vars)),
                                  paste0(draw_name), paste0(data) )]
    
    ## Rename columns back
    for(i in 1:length(c(other_vars, corr_vars))) {
        colnames(data_processed)[i] <- c(other_vars, corr_vars)[i] 
    }

    ## Cast the draws out wide?
    if(wide_on_draws) {
        data_processed <- dcast(data_processed, other_vars + corr_vars ~ draw_name, value.var = paste0(data) )
    }
    

    return(data_processed)
}





















