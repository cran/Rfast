#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP Rfast_logistic_only(SEXP xSEXP,SEXP ySEXP,SEXP tolSEXP);
SEXP Rfast_mahaCpp(SEXP X, SEXP mu, SEXP sigma, SEXP isChol);
SEXP Rfast_manhattan_dist(SEXP xSEXP);
SEXP Rfast_Match(SEXP xSEXP,SEXP keySEXP);
SEXP Rfast_max_dist(SEXP xSEXP);
SEXP Rfast_min_max(SEXP x,SEXP indexSEXP);
SEXP Rfast_med(SEXP xSEXP);
SEXP Rfast_nth(SEXP xSEXP,SEXP ySEXP);
SEXP Rfast_Order(SEXP xSEXP,SEXP stableSEXP);
SEXP Rfast_permutation(SEXP xSEXP,SEXP allSEXP,SEXP fnSEXP);
SEXP Rfast_permutation_next(SEXP xSEXP,SEXP all_nextSEXP,SEXP fnSEXP);
SEXP Rfast_permutation_prev(SEXP xSEXP,SEXP all_prevSEXP,SEXP fnSEXP);
SEXP Rfast_poisson_only(SEXP xSEXP,SEXP ySEXP,SEXP ylogySEXP,SEXP tolSEXP);
SEXP Rfast_regression(SEXP xSEXP,SEXP ySEXP);
SEXP Rfast_row_means(SEXP xSEXP);
SEXP Rfast_row_max(SEXP xSEXP);
SEXP Rfast_row_meds(SEXP xSEXP);
SEXP Rfast_row_min(SEXP xSEXP,SEXP valueSEXP);
SEXP Rfast_row_len_sort_un_int(SEXP xSEXP);
SEXP Rfast_sort_cor_vecs(SEXP xSEXP,SEXP ySEXP);
SEXP Rfast_sort_index(SEXP xSEXP,SEXP descendingSEXP);
SEXP Rfast_row_sums(SEXP xSEXP);
SEXP Rfast_sort_unique_double(SEXP xSEXP);
SEXP Rfast_sort_row(SEXP xSEXP,SEXP descendSEXP);
SEXP Rfast_sort_col(SEXP xSEXP,SEXP descendSEXP);
SEXP Rfast_sort_unique_int(SEXP xSEXP);
SEXP Rfast_symmetric(SEXP xSEXP);
SEXP Rfast_Trigamma(SEXP x);
SEXP Rfast_var_c(SEXP xSEXP);
SEXP Rfast_vec_comb_n(SEXP dataSEXP,SEXP nSEXP);
SEXP Rfast_which_isFactor(SEXP xSEXP);
SEXP Rfast_binarysearch(SEXP xSEXP,SEXP vSEXP);
SEXP Rfast_lowerbound(SEXP xSEXP,SEXP vSEXP);
SEXP Rfast_col_min_max(SEXP x);
SEXP Rfast_col_max_indices(SEXP xSEXP);
SEXP Rfast_col_max(SEXP x);
SEXP Rfast_col_means(SEXP xSEXP);
SEXP Rfast_is_element(SEXP xSEXP,SEXP elSEXP);
SEXP Rfast_Lgamma(SEXP x);
SEXP Rfast_col_meds(SEXP xSEXP);
SEXP Rfast_col_min_indices(SEXP xSEXP);
SEXP Rfast_col_min(SEXP x);
SEXP Rfast_col_sums(SEXP xSEXP);
SEXP Rfast_k_comb_n(SEXP nSEXP,SEXP kSEXP);
SEXP Rfast_count_value(SEXP xSEXP,SEXP valueSEXP);
SEXP Rfast_design_matrix(SEXP xSEXP,SEXP onesSEXP);
SEXP Rfast_Digamma(SEXP x);
SEXP Rfast_diri_nr_type2(SEXP a1SEXP,SEXP a2SEXP,SEXP maSEXP,SEXP pSEXP,SEXP tolSEXP);
SEXP Rfast_euclidean_dist(SEXP xSEXP,SEXP sqrSEXP);
SEXP Rfast_floyd_john(SEXP nSEXP,SEXP xSEXP);
SEXP Rfast_frame_to_matrix(SEXP xSEXP,SEXP as_numericSEXP);
SEXP Rfast_g2Test(SEXP dataSEXP,SEXP xSEXP,SEXP ySEXP,SEXP csSEXP,SEXP dcSEXP);
SEXP Rfast_g2Test_univariate_perm(SEXP dataSEXP,SEXP dcSEXP,SEXP npermSEXP);
SEXP Rfast_g2Test_univariate(SEXP dataSEXP,SEXP dcSEXP);
SEXP Rfast_g2Test_perm(SEXP dataSEXP,SEXP xSEXP,SEXP ySEXP,SEXP csSEXP,SEXP dcSEXP,SEXP npermSEXP);
SEXP Rfast_hash_find(SEXP xSEXP,SEXP valueSEXP);
SEXP Rfast_Hash_list(SEXP keySEXP,SEXP xSEXP);
SEXP Rfast_spat_med(SEXP xSEXP,SEXP tolSEXP);
SEXP Rfast_len_sort_unique_double(SEXP xSEXP);
SEXP Rfast_len_sort_unique_int(SEXP xSEXP);
SEXP Rfast_Log(SEXP x);
SEXP Rfast_rmdp(SEXP ySEXP,SEXP hSEXP,SEXP rndSEXP,SEXP itertimeSEXP);
SEXP Rfast_Sort(SEXP x,SEXP descendSEXP);
SEXP Rfast_col_nth(SEXP xSEXP,SEXP ySEXP);
SEXP Rfast_col_len_sort_un_int(SEXP xSEXP);
SEXP Rfast_kron(SEXP xSEXP,SEXP ySEXP);
SEXP Rfast_min_dist(SEXP xSEXP);
SEXP Rfast_min_max_perc(SEXP x);
SEXP Rfast_minkowski_dist(SEXP xSEXP,SEXP pSEXP);
SEXP Rfast_col_tabulate(SEXP xSEXP,SEXP nrowwSEXP);
SEXP Rfast_row_tabulate(SEXP xSEXP,SEXP ncollSEXP);
SEXP Rfast_rep_col(SEXP xSEXP,SEXP nSEXP);
SEXP Rfast_row_nth(SEXP xSEXP,SEXP ySEXP);
SEXP Rfast_row_min_max(SEXP x);
SEXP Rfast_row_shuffle(SEXP xSEXP);
SEXP Rfast_design_matrix_big(SEXP xSEXP);
SEXP Rfast_partial_sort(SEXP x,SEXP nSEXP,SEXP descendSEXP);
SEXP Rfast_sort_string(SEXP xSEXP,SEXP descendSEXP);
SEXP Rfast_stable_sort(SEXP x,SEXP descendSEXP);
SEXP Rfast_total_variation_dist(SEXP xSEXP);
SEXP Rfast_group_sum(SEXP xSEXP,SEXP groupSEXP);
SEXP Rfast_varcomps_mle(SEXP xSEXP,SEXP inaSEXP,SEXP nSEXP,SEXP tolSEXP);
SEXP Rfast_vecdist(SEXP x);
SEXP Rfast_dista(SEXP XnewSEXP,SEXP XSEXP,SEXP typeSEXP);
SEXP Rfast_col_shuffle(SEXP lenSEXP,SEXP nSEXP);
SEXP Rfast_glm_logistic(SEXP xSEXP,SEXP ySEXP,SEXP tolSEXP);
SEXP Rfast_glm_poisson(SEXP xSEXP,SEXP ySEXP,SEXP ylogySEXP,SEXP tolSEXP);
SEXP Rfast_canberra1_dist(SEXP xSEXP);
SEXP Rfast_canberra2_dist(SEXP xSEXP);
SEXP Rfast_mat_mat(SEXP xSEXP,SEXP ySEXP);
SEXP Rfast_hellinger_dist(SEXP xSEXP,SEXP sqrSEXP);
SEXP Rfast_prop_reg(SEXP xSEXP,SEXP ySEXP);
SEXP Rfast_prop_regs(SEXP xSEXP,SEXP ySEXP,SEXP tolSEXP,SEXP varbSEXP);
SEXP Rfast_Lbeta(SEXP x,SEXP y);
SEXP Rfast_Choose(SEXP x,SEXP kSEXP);
SEXP Rfast_Lchoose(SEXP x,SEXP kSEXP);
SEXP Rfast_Round(SEXP x,SEXP dgSEXP);
SEXP Rfast_fs_reg(SEXP ySEXP,SEXP dsSEXP,SEXP sigSEXP,SEXP tolSEXP,SEXP typeSEXP);
SEXP Rfast_add_to_namespace(SEXP dir_to_exportSEXP,SEXP dir_to_fileSEXP);
SEXP Rfast_read_directory(SEXP pathSEXP);
SEXP Rfast_logistic_only_b(SEXP xSEXP,SEXP ySEXP,SEXP tolSEXP);
SEXP Rfast_poisson_only_b(SEXP xSEXP,SEXP ySEXP,SEXP ylogySEXP,SEXP tolSEXP);
SEXP Rfast_squareform_c(SEXP xSEXP);
SEXP Rfast_rvmf_h(SEXP xSEXP,SEXP caSEXP,SEXP d1SEXP,SEXP x0SEXP,SEXP mSEXP,SEXP kSEXP,SEXP bSEXP);
SEXP Rfast_lower_tri(SEXP xSEXP);
SEXP Rfast_lower_tri_b(SEXP nclSEXP, SEXP nrwSEXP);
SEXP Rfast_row_true(SEXP xSEXP);
SEXP Rfast_Norm(SEXP xSEXP,SEXP typeSEXP);
SEXP Rfast_pmin(SEXP x,SEXP y);
SEXP Rfast_col_true(SEXP xSEXP);
SEXP Rfast_odds_helper(SEXP x);
SEXP Rfast_pmax(SEXP x,SEXP y);
SEXP Rfast_col_diffs(SEXP x);
SEXP Rfast_col_false(SEXP xSEXP);
SEXP Rfast_row_prods(SEXP xSEXP);
SEXP Rfast_row_false(SEXP xSEXP);
SEXP Rfast_col_order(SEXP xSEXP,SEXP stableSEXP);
SEXP Rfast_col_prods(SEXP xSEXP);
SEXP Rfast_row_order(SEXP xSEXP,SEXP stableSEXP);
SEXP Rfast_matrix_sum(SEXP x);
SEXP Rfast_diag_matrix(SEXP lenSEXP,SEXP vSEXP);
SEXP Rfast_diag_fill(SEXP xSEXP,SEXP vSEXP);
SEXP Rfast_upper_tri(SEXP xSEXP);
SEXP Rfast_upper_tri_b(SEXP nclSEXP, SEXP nrwSEXP);
SEXP Rfast_sum_XopY(SEXP x,SEXP y,SEXP operSEXP);
SEXP Rfast_sum_XopX(SEXP x,SEXP operSEXP);
SEXP Rfast_total_dist(SEXP xSEXP);
SEXP Rfast_total_dista(SEXP xSEXP,SEXP ySEXP);
SEXP Rfast_edist(SEXP xSEXP,SEXP ySEXP);
SEXP Rfast_g2tests_perm(SEXP dataSEXP,SEXP xSEXP,SEXP ySEXP,SEXP dcSEXP,SEXP npermSEXP);
SEXP Rfast_g2tests(SEXP dataSEXP,SEXP xSEXP,SEXP ySEXP,SEXP dcSEXP);
SEXP Rfast_spml_reg_helper(SEXP B1SEXP,SEXP B2SEXP,SEXP xSEXP,SEXP uSEXP,SEXP ciSEXP,SEXP siSEXP,SEXP conSEXP,SEXP tolSEXP);
SEXP Rfast_qpois_reg(SEXP xSEXP,SEXP ySEXP,SEXP ylogySEXP,SEXP tolSEXP);
SEXP Rfast_count_value_string(SEXP xSEXP,SEXP valueSEXP);
SEXP Rfast_check_namespace(SEXP dir_to_exportSEXP,SEXP dir_to_fileSEXP);
SEXP Rfast_table_string(SEXP xSEXP);
SEXP Rfast_table_double(SEXP xSEXP);
SEXP Rfast_hash2list(SEXP xSEXP,SEXP sortingSEXP);
SEXP Rfast_col_row_zero(SEXP xSEXP);
SEXP Rfast_check_aliases(SEXP dir_to_manSEXP,SEXP dir_to_fileSEXP);
SEXP Rfast_col_any(SEXP x);
SEXP Rfast_row_any(SEXP xSEXP);
SEXP Rfast_col_true_false(SEXP xSEXP);
SEXP Rfast_row_true_false(SEXP xSEXP);
SEXP Rfast_cholesky(SEXP x);
SEXP Rfast_cholesky_par(SEXP x);
SEXP Rfast_read_examples(SEXP path_manSEXP,SEXP no_readSEXP);
SEXP Rfast_row_mads(SEXP x);
SEXP Rfast_col_mads(SEXP x);
SEXP Rfast_row_max_indices(SEXP xSEXP);
SEXP Rfast_row_min_indices(SEXP xSEXP);
SEXP Rfast_pc_skel(SEXP dsSEXP,SEXP methodSEXP,SEXP sigSEXP,SEXP rSEXP);
SEXP Rfast_k_nn(SEXP ds_extraSEXP,SEXP ySEXP,SEXP dsSEXP,SEXP idxsSEXP,SEXP dist_typeSEXP,SEXP typeSEXP,SEXP methodSEXP,SEXP freq_optionSEXP);

static const R_CallMethodDef CallEntries[] = {
  {"Rfast_logistic_only", (DL_FUNC) &Rfast_logistic_only, 3},
  {"Rfast_mahaCpp", (DL_FUNC) &Rfast_mahaCpp, 4},
  {"Rfast_manhattan_dist", (DL_FUNC) &Rfast_manhattan_dist, 1},
  {"Rfast_Match", (DL_FUNC) &Rfast_Match, 2},
  {"Rfast_max_dist", (DL_FUNC) &Rfast_max_dist, 1},
  {"Rfast_min_max", (DL_FUNC) &Rfast_min_max, 2},
  {"Rfast_med", (DL_FUNC) &Rfast_med, 1},
  {"Rfast_nth", (DL_FUNC) &Rfast_nth, 2},
  {"Rfast_Order", (DL_FUNC) &Rfast_Order, 2},
  {"Rfast_permutation", (DL_FUNC) &Rfast_permutation, 3},
  {"Rfast_permutation_next", (DL_FUNC) &Rfast_permutation_next, 3},
  {"Rfast_permutation_prev", (DL_FUNC) &Rfast_permutation_prev, 3},
  {"Rfast_poisson_only", (DL_FUNC) &Rfast_poisson_only, 4},
  {"Rfast_regression", (DL_FUNC) &Rfast_regression, 2},
  {"Rfast_row_means", (DL_FUNC) &Rfast_row_means, 1},
  {"Rfast_row_max", (DL_FUNC) &Rfast_row_max, 1},
  {"Rfast_row_meds", (DL_FUNC) &Rfast_row_meds, 1},
  {"Rfast_row_min", (DL_FUNC) &Rfast_row_min, 1},
  {"Rfast_row_len_sort_un_int", (DL_FUNC) &Rfast_row_len_sort_un_int, 1},
  {"Rfast_sort_cor_vecs", (DL_FUNC) &Rfast_sort_cor_vecs, 2},
  {"Rfast_sort_index", (DL_FUNC) &Rfast_sort_index, 2},
  {"Rfast_row_sums", (DL_FUNC) &Rfast_row_sums, 1},
  {"Rfast_sort_unique_double", (DL_FUNC) &Rfast_sort_unique_double, 1},
  {"Rfast_sort_row", (DL_FUNC) &Rfast_sort_row, 2},
  {"Rfast_sort_col", (DL_FUNC) &Rfast_sort_col, 2},
  {"Rfast_sort_unique_int", (DL_FUNC) &Rfast_sort_unique_int, 1},
  {"Rfast_symmetric", (DL_FUNC) &Rfast_symmetric, 1},
  {"Rfast_Trigamma", (DL_FUNC) &Rfast_Trigamma, 1},
  {"Rfast_var_c", (DL_FUNC) &Rfast_var_c, 1},
  {"Rfast_vec_comb_n", (DL_FUNC) &Rfast_vec_comb_n, 2},
  {"Rfast_which_isFactor", (DL_FUNC) &Rfast_which_isFactor, 1},
  {"Rfast_binarysearch", (DL_FUNC) &Rfast_binarysearch, 2},
  {"Rfast_lowerbound", (DL_FUNC) &Rfast_lowerbound, 2},
  {"Rfast_col_min_max", (DL_FUNC) &Rfast_col_min_max, 1},
  {"Rfast_col_max_indices", (DL_FUNC) &Rfast_col_max_indices, 1},
  {"Rfast_col_max", (DL_FUNC) &Rfast_col_max, 1},
  {"Rfast_col_means", (DL_FUNC) &Rfast_col_means, 1},
  {"Rfast_is_element", (DL_FUNC) &Rfast_is_element, 2},
  {"Rfast_Lgamma", (DL_FUNC) &Rfast_Lgamma, 1},
  {"Rfast_col_meds", (DL_FUNC) &Rfast_col_meds, 1},
  {"Rfast_col_min_indices", (DL_FUNC) &Rfast_col_min_indices, 1},
  {"Rfast_col_min", (DL_FUNC) &Rfast_col_min, 1},
  {"Rfast_col_sums", (DL_FUNC) &Rfast_col_sums, 1},
  {"Rfast_k_comb_n", (DL_FUNC) &Rfast_k_comb_n, 2},
  {"Rfast_count_value", (DL_FUNC) &Rfast_count_value, 2},
  {"Rfast_design_matrix", (DL_FUNC) &Rfast_design_matrix, 2},
  {"Rfast_Digamma", (DL_FUNC) &Rfast_Digamma, 1},
  {"Rfast_diri_nr_type2", (DL_FUNC) &Rfast_diri_nr_type2, 5},
  {"Rfast_euclidean_dist", (DL_FUNC) &Rfast_euclidean_dist, 2},
  {"Rfast_floyd_john", (DL_FUNC) &Rfast_floyd_john, 2},
  {"Rfast_frame_to_matrix", (DL_FUNC) &Rfast_frame_to_matrix, 2},
  {"Rfast_g2Test", (DL_FUNC) &Rfast_g2Test, 5},
  {"Rfast_g2Test_univariate_perm", (DL_FUNC) &Rfast_g2Test_univariate_perm, 3},
  {"Rfast_g2Test_univariate", (DL_FUNC) &Rfast_g2Test_univariate, 2},
  {"Rfast_g2Test_perm", (DL_FUNC) &Rfast_g2Test_perm, 6},
  {"Rfast_hash_find", (DL_FUNC) &Rfast_hash_find, 2},
  {"Rfast_Hash_list", (DL_FUNC) &Rfast_Hash_list, 2},
  {"Rfast_spat_med", (DL_FUNC) &Rfast_spat_med, 2},
  {"Rfast_len_sort_unique_double", (DL_FUNC) &Rfast_len_sort_unique_double, 1},
  {"Rfast_len_sort_unique_int", (DL_FUNC) &Rfast_len_sort_unique_int, 1},
  {"Rfast_Log", (DL_FUNC) &Rfast_Log, 1},
  {"Rfast_rmdp", (DL_FUNC) &Rfast_rmdp, 4},
  {"Rfast_Sort", (DL_FUNC) &Rfast_Sort, 2},
  {"Rfast_col_nth", (DL_FUNC) &Rfast_col_nth, 2},
  {"Rfast_col_len_sort_un_int", (DL_FUNC) &Rfast_col_len_sort_un_int, 1},
  {"Rfast_kron", (DL_FUNC) &Rfast_kron, 2},
  {"Rfast_min_dist", (DL_FUNC) &Rfast_min_dist, 1},
  {"Rfast_min_max_perc", (DL_FUNC) &Rfast_min_max_perc, 1},
  {"Rfast_minkowski_dist", (DL_FUNC) &Rfast_minkowski_dist, 2},
  {"Rfast_row_tabulate", (DL_FUNC) &Rfast_row_tabulate, 2},
  {"Rfast_col_tabulate", (DL_FUNC) &Rfast_col_tabulate, 2},
  {"Rfast_rep_col", (DL_FUNC) &Rfast_rep_col, 2},
  {"Rfast_row_nth", (DL_FUNC) &Rfast_row_nth, 2},
  {"Rfast_row_min_max", (DL_FUNC) &Rfast_row_min_max, 1},
  {"Rfast_row_shuffle", (DL_FUNC) &Rfast_row_shuffle, 1},
  {"Rfast_design_matrix_big", (DL_FUNC) &Rfast_design_matrix_big, 1},
  {"Rfast_partial_sort", (DL_FUNC) &Rfast_partial_sort, 3},
  {"Rfast_sort_string", (DL_FUNC) &Rfast_sort_string, 2},
  {"Rfast_stable_sort", (DL_FUNC) &Rfast_stable_sort, 2},
  {"Rfast_total_variation_dist", (DL_FUNC) &Rfast_total_variation_dist, 1},
  {"Rfast_group_sum", (DL_FUNC) &Rfast_group_sum, 2},
  {"Rfast_varcomps_mle", (DL_FUNC) &Rfast_varcomps_mle, 4},
  {"Rfast_vecdist", (DL_FUNC) &Rfast_vecdist, 1},
  {"Rfast_dista", (DL_FUNC) &Rfast_dista, 3},
  {"Rfast_col_shuffle", (DL_FUNC) &Rfast_col_shuffle, 2},
  {"Rfast_glm_logistic", (DL_FUNC) &Rfast_glm_logistic, 3},
  {"Rfast_glm_poisson", (DL_FUNC) &Rfast_glm_poisson, 4},
  {"Rfast_canberra1_dist", (DL_FUNC) &Rfast_canberra1_dist, 1},
  {"Rfast_canberra2_dist", (DL_FUNC) &Rfast_canberra2_dist, 1},
  {"Rfast_mat_mat", (DL_FUNC) &Rfast_mat_mat, 2},
  {"Rfast_hellinger_dist", (DL_FUNC) &Rfast_hellinger_dist, 2},
  {"Rfast_prop_reg", (DL_FUNC) &Rfast_prop_reg, 2},
  {"Rfast_prop_regs", (DL_FUNC) &Rfast_prop_regs, 4},
  {"Rfast_Lbeta", (DL_FUNC) &Rfast_Lbeta, 2},
  {"Rfast_Choose", (DL_FUNC) &Rfast_Choose, 2},
  {"Rfast_Lchoose", (DL_FUNC) &Rfast_Lchoose, 2},
  {"Rfast_Round", (DL_FUNC) &Rfast_Round, 2},
  {"Rfast_fs_reg", (DL_FUNC) &Rfast_fs_reg, 5},
  {"Rfast_add_to_namespace", (DL_FUNC) &Rfast_add_to_namespace, 2},
  {"Rfast_read_directory", (DL_FUNC) &Rfast_read_directory, 1},
  {"Rfast_poisson_only_b", (DL_FUNC) &Rfast_poisson_only_b, 4},
  {"Rfast_logistic_only_b", (DL_FUNC) &Rfast_logistic_only_b, 3},
  {"Rfast_squareform_c", (DL_FUNC) &Rfast_squareform_c, 1},
  {"Rfast_rvmf_h", (DL_FUNC) &Rfast_rvmf_h, 7},
  {"Rfast_lower_tri", (DL_FUNC) &Rfast_lower_tri, 1},
  {"Rfast_lower_tri_b", (DL_FUNC) &Rfast_lower_tri_b, 2},
  {"Rfast_row_true", (DL_FUNC) &Rfast_row_true, 1},
  {"Rfast_Norm", (DL_FUNC) &Rfast_Norm, 2},
  {"Rfast_pmin", (DL_FUNC) &Rfast_pmin, 2},
  {"Rfast_col_true", (DL_FUNC) &Rfast_col_true, 1},
  {"Rfast_odds_helper", (DL_FUNC) &Rfast_odds_helper, 1},
  {"Rfast_pmax", (DL_FUNC) &Rfast_pmax, 2},
  {"Rfast_col_diffs", (DL_FUNC) &Rfast_col_diffs, 1},
  {"Rfast_col_false", (DL_FUNC) &Rfast_col_false, 1},
  {"Rfast_row_prods", (DL_FUNC) &Rfast_row_prods, 1},
  {"Rfast_row_false", (DL_FUNC) &Rfast_row_false, 1},
  {"Rfast_col_order", (DL_FUNC) &Rfast_col_order, 2},
  {"Rfast_row_order", (DL_FUNC) &Rfast_row_order, 2},
  {"Rfast_col_prods", (DL_FUNC) &Rfast_col_prods, 1},
  {"Rfast_matrix_sum", (DL_FUNC) &Rfast_matrix_sum, 1},
  {"Rfast_diag_matrix", (DL_FUNC) &Rfast_diag_matrix, 2},
  {"Rfast_diag_fill", (DL_FUNC) &Rfast_diag_fill, 2},
  {"Rfast_upper_tri", (DL_FUNC) &Rfast_upper_tri, 1},
  {"Rfast_upper_tri_b", (DL_FUNC) &Rfast_upper_tri_b, 2},
  {"Rfast_sum_XopY", (DL_FUNC) &Rfast_sum_XopY, 3},
  {"Rfast_sum_XopX", (DL_FUNC) &Rfast_sum_XopX, 2},
  {"Rfast_total_dist", (DL_FUNC) &Rfast_total_dist, 1},
  {"Rfast_total_dista", (DL_FUNC) &Rfast_total_dista, 2},
  {"Rfast_edist", (DL_FUNC) &Rfast_edist, 2},
  {"Rfast_g2tests_perm", (DL_FUNC) &Rfast_g2tests_perm, 5},
  {"Rfast_g2tests", (DL_FUNC) &Rfast_g2tests, 4},
  {"Rfast_spml_reg_helper", (DL_FUNC) &Rfast_spml_reg_helper, 8},
  {"Rfast_qpois_reg", (DL_FUNC) &Rfast_qpois_reg, 4},
  {"Rfast_count_value_string", (DL_FUNC) &Rfast_count_value_string, 2},
  {"Rfast_check_namespace", (DL_FUNC) &Rfast_check_namespace, 2},
  {"Rfast_table_string", (DL_FUNC) &Rfast_table_string, 1},
  {"Rfast_table_double", (DL_FUNC) &Rfast_table_double, 1},
  {"Rfast_hash2list", (DL_FUNC) &Rfast_hash2list, 2},
  {"Rfast_col_row_zero", (DL_FUNC) &Rfast_col_row_zero, 1},
  {"Rfast_check_aliases", (DL_FUNC) &Rfast_check_aliases, 2},
  {"Rfast_col_any", (DL_FUNC) &Rfast_col_any, 1},
  {"Rfast_row_any", (DL_FUNC) &Rfast_row_any, 1},
  {"Rfast_col_true_false", (DL_FUNC) &Rfast_col_true_false, 1},
  {"Rfast_row_true_false", (DL_FUNC) &Rfast_row_true_false, 1},
  {"Rfast_cholesky", (DL_FUNC) &Rfast_cholesky, 1},
  {"Rfast_cholesky_par", (DL_FUNC) &Rfast_cholesky_par, 1},
  {"Rfast_read_examples", (DL_FUNC) &Rfast_read_examples, 2},
  {"Rfast_row_mads", (DL_FUNC) &Rfast_row_mads, 1},
  {"Rfast_col_mads", (DL_FUNC) &Rfast_col_mads, 1},
  {"Rfast_row_max_indices", (DL_FUNC) &Rfast_row_max_indices, 1},
  {"Rfast_row_min_indices", (DL_FUNC) &Rfast_row_min_indices, 1},
  {"Rfast_pc_skel", (DL_FUNC) &Rfast_pc_skel, 4},
  {"Rfast_k_nn", (DL_FUNC) &Rfast_k_nn, 8},
  {NULL, NULL, 0}
};


void R_init_Rfast(DllInfo *info)
{
  R_registerRoutines(info, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
}
