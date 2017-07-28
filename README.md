
# Update notes:
Users: input vector correlation

Function: makes kron product

(Have a default value?)

“Advanced: option”: have the user input a kron matrix themselves (assert dimensions)


This has to be part of save_results! Get draws automatically has aggregated data.

De-parsing the strings and plot after aggregating to see correlated results

A viz tool to see output ^




Function changes:

[Return a list with all the stuff including: data and correlated matrix]

1. If data frame supplied, convert to array
2. Input a vector or matrix of correlation values Assert that the dimension of correlation input is equal to dimension of … dimensions being correlated 

   1. If NA, print warning of the defaults being used
    2. Assert that length of vector is equal to dimensions being correlated across (else: die) (If vector inputted, the function will create a matrix) 

   3. If matrix inputted, assert that dimensions match exactly to dimensions’ product (else:die)
3. Prepping: User will put in a data frame similar to get draws AND two lists of variables: one to correlate across, and one to keep intact. Our function will stack and create the 4 column data.table Automating the variable stacking for non-correlated columns

Users inputs:
(a) Get Draws like structure with: location_id, year_id, age, sex, cause, metric, measure, and draws (where assert that the name of the draws are pulled from a grepl of “draw”)

(b) two vectors, each with variable names of what to correlate and not correlate over

(c) Correlation stuff: 
Users inputting the proper dimensional vector and matrix. If missing, warning and use default.

Tasks:
Maddie: correlation stuff
David: visualizing the outputs from copula function
Nafis: input data.table to array; parsing out the original columns after copula (use unique concatenating apriori so that de-parsing is easier:: paste collapse with __)
Helena: ugh documentation. Sorry.




# Correlation in GBD coding challenge

Input: GBD outputs from CODEm (fatal), DisMod (non-fatal), or even just get_draws

Output: Same exact columns as inputs, but jointly correlated

Inpspiration: It's common knowledge that everything modelled in GBD does not specify any correlation structures, for e.g., we would expect a pretty high correlation between IHD and Diabetes, or for a given cause, we might expect to see correlation across adjacent age groups. Therefore, when we aggregate deaths across causes to a global level, we find the uncertainty intervals to be super small.

The purpose of this project is to be able to come up with a function which will take in marginal distributions and a (matrix of) correlation values, and output the same number of columns that went in, but the joint distribution of the marginals will be correlated. In terms of IHME application, aggregating a joint distribution will give wider uncertainty intervals (for e.g., if high draws are correlated with other high draws, and v.v. for low draws, you can expect the upper and lower intervals to be expansive).

How do we do that: Sklar's Theorem says that there always exists a function such that, for any two marginal distributions, the joint distribution can have a given correlation structure, such that the marginals in the joint are unchanged. 

A much smaller scaled version of this has been used and published by IHME last year, and therefore I think we have a great standing foot to work off of (see master branch).


## Challenges: 
-   large number of rows (scalibility and tractability computationally)
-   high dimensionality in categories (specifiying and being able to choose correlation across ages/sexes/causes)
-   keeping a dimension unchanged (induce correlation across ages and sexes, but keep the temporal correlation untouched)
-- (wrapper function around the already existing one, which will collapse non-sorts into a single dim, and to-sorts into another)

-   coming up with a prior correlation matrix
-- (an option: using older GBD results to be passed)

-   the impossible combinations (if we are dealing with four marginal distributions, and we have two pairs with perfect correlation, then it's impossible to ... )


## Progress:

1)	Getting comfy with the function by using example draws right now. 
2)	Creating a prepping data function to take get_draws and prep for the copulating function.

3)	Unit tests. 
4)	Stacking of dimensions. Most challenging. (7 modified business days) 
5)	Supplying correlation matrix. Build in a default AR(1) matrix if correlation not supplied. 
6)	Porting to different language. 
















