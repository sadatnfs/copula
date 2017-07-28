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
        assert_that(is.numeric(corr_vector), msg="correlation values should be
                                                  numeric")
    }
    # user should input either corr_vector or corr_matrix, but not both
    assert_that(is.null(corr_vector) | is.null(corr_matrix), msg="Input either
                                                                  corr_matrix or
                                                                  corr_vector,
                                                                  but not both")

    
}

get_corr_matrix <- function(corr_vector, corr_matrix) {
    print("TODO: write this function")
}

stack_data <- function(data, corr_vars) {
    print("TODO: write this function")
}

correlate_stack <- function(stack, corr_matrix) {
    print("TODO: write this function")
}

unstack_data <- function(stack) {
    print("TODO: write this function")
}
