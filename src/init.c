#include <Rinternals.h>
#include <R_ext/Rdynload.h>


SEXP Rfast_add_to_namespace(SEXP dir_to_exportSEXP,SEXP dir_to_fileSEXP);
SEXP Rfast_bcdcor(SEXP xSEXP,SEXP ySEXP);
SEXP Rfast_binarysearch(SEXP xSEXP,SEXP vSEXP);
SEXP Rfast_bic_fs_reg(SEXP ySEXP,SEXP dsSEXP,SEXP tolSEXP,SEXP typeSEXP);
SEXP Rfast_bincomb(SEXP xSEXP);
SEXP Rfast_col_meds(SEXP xSEXP);
SEXP Rfast_col_min_indices(SEXP xSEXP);
SEXP Rfast_col_min(SEXP x);
SEXP Rfast_col_sums(SEXP xSEXP);
SEXP Rfast_col_min_max(SEXP x);
SEXP Rfast_col_max_indices(SEXP xSEXP);
SEXP Rfast_col_max(SEXP x);
SEXP Rfast_col_means(SEXP xSEXP);
SEXP Rfast_col_nth(SEXP xSEXP,SEXP ySEXP);
SEXP Rfast_col_len_sort_un_int(SEXP xSEXP);
SEXP Rfast_col_tabulate(SEXP xSEXP,SEXP nrowwSEXP);
SEXP Rfast_col_shuffle(SEXP lenSEXP,SEXP nSEXP);
SEXP Rfast_col_true(SEXP xSEXP);
SEXP Rfast_col_diffs(SEXP x);
SEXP Rfast_col_false(SEXP xSEXP);
SEXP Rfast_col_order(SEXP xSEXP,SEXP stableSEXP,SEXP descendingSEXP);
SEXP Rfast_col_prods(SEXP xSEXP);
SEXP Rfast_count_value_string(SEXP xSEXP,SEXP valueSEXP);
SEXP Rfast_check_namespace(SEXP dir_to_exportSEXP,SEXP dir_to_fileSEXP);
SEXP Rfast_check_aliases(SEXP dir_to_manSEXP,SEXP dir_to_fileSEXP);
SEXP Rfast_col_any(SEXP x);
SEXP Rfast_cholesky(SEXP x);
SEXP Rfast_cholesky_par(SEXP x);
SEXP Rfast_col_mads(SEXP x);
SEXP Rfast_col_true_false(SEXP xSEXP);
SEXP Rfast_count_value(SEXP xSEXP,SEXP valueSEXP);
SEXP Rfast_Choose(SEXP x,SEXP kSEXP);
SEXP Rfast_dvar(SEXP xSEXP);
SEXP Rfast_dcor(SEXP xSEXP,SEXP ySEXP);
SEXP Rfast_dcov(SEXP xSEXP,SEXP ySEXP);
SEXP Rfast_diag_matrix_fill_scalar(SEXP xSEXP,SEXP vSEXP); 
SEXP Rfast_diag_matrix_fill_vec(SEXP lenSEXP,SEXP vSEXP);
SEXP Rfast_diag_fill_scalar(SEXP lenSEXP,SEXP vSEXP);
SEXP Rfast_diag_fill_vec(SEXP lenSEXP,SEXP vSEXP);
SEXP Rfast_design_matrix(SEXP xSEXP,SEXP onesSEXP);
SEXP Rfast_dist(SEXP xSEXP,SEXP methodSEXP,SEXP sqrSEXP,SEXP pSEXP);
SEXP Rfast_Digamma(SEXP x);
SEXP Rfast_design_matrix_big(SEXP xSEXP);
SEXP Rfast_dista(SEXP XnewSEXP,SEXP XSEXP,SEXP sqrSEXP,SEXP typeSEXP);
SEXP Rfast_diri_nr_type2(SEXP a1SEXP,SEXP a2SEXP,SEXP maSEXP,SEXP pSEXP,SEXP tolSEXP);
SEXP Rfast_edist(SEXP xSEXP,SEXP ySEXP);
SEXP Rfast_eachrow(SEXP x,SEXP y,SEXP operSEXP);
SEXP Rfast_floyd_john(SEXP nSEXP,SEXP xSEXP);
SEXP Rfast_frame_to_matrix(SEXP xSEXP,SEXP as_numericSEXP);
SEXP Rfast_fs_reg(SEXP ySEXP,SEXP dsSEXP,SEXP sigSEXP,SEXP tolSEXP,SEXP typeSEXP);
SEXP Rfast_g2Test(SEXP dataSEXP,SEXP xSEXP,SEXP ySEXP,SEXP csSEXP,SEXP dcSEXP);
SEXP Rfast_g2Test_univariate_perm(SEXP dataSEXP,SEXP dcSEXP,SEXP npermSEXP);
SEXP Rfast_g2Test_univariate(SEXP dataSEXP,SEXP dcSEXP);
SEXP Rfast_g2Test_perm(SEXP dataSEXP,SEXP xSEXP,SEXP ySEXP,SEXP csSEXP,SEXP dcSEXP,SEXP npermSEXP);
SEXP Rfast_g2tests_perm(SEXP dataSEXP,SEXP xSEXP,SEXP ySEXP,SEXP dcSEXP,SEXP npermSEXP);
SEXP Rfast_g2tests(SEXP dataSEXP,SEXP xSEXP,SEXP ySEXP,SEXP dcSEXP);
SEXP Rfast_group_sum(SEXP xSEXP,SEXP groupSEXP,SEXP max_nSEXP);
SEXP Rfast_group_all(SEXP xSEXP,SEXP groupSEXP,SEXP nSEXP);
SEXP Rfast_group_any(SEXP xSEXP,SEXP groupSEXP,SEXP nSEXP);
SEXP Rfast_group_mad(SEXP xSEXP,SEXP groupSEXP,SEXP methodSEXP);
SEXP Rfast_group_max(SEXP xSEXP,SEXP groupSEXP,SEXP max_nSEXP);
SEXP Rfast_group_mean(SEXP xSEXP,SEXP groupSEXP,SEXP max_nSEXP);
SEXP Rfast_group_med(SEXP xSEXP,SEXP groupSEXP);
SEXP Rfast_group_min(SEXP xSEXP,SEXP groupSEXP,SEXP max_nSEXP);
SEXP Rfast_group_min_max(SEXP xSEXP,SEXP groupSEXP,SEXP max_nSEXP);
SEXP Rfast_group_var(SEXP xSEXP,SEXP groupSEXP,SEXP nSEXP);
SEXP Rfast_glm_logistic(SEXP xSEXP,SEXP ySEXP,SEXP tolSEXP,SEXP maxitersSEXP);
SEXP Rfast_glm_poisson(SEXP xSEXP,SEXP ySEXP,SEXP ylogySEXP,SEXP tolSEXP);
SEXP Rfast_hash2list(SEXP xSEXP,SEXP sortingSEXP);
SEXP Rfast_hash_find(SEXP xSEXP,SEXP valueSEXP);
SEXP Rfast_Hash_list(SEXP keySEXP,SEXP xSEXP);
SEXP Rfast_is_element(SEXP xSEXP,SEXP elSEXP);
SEXP Rfast_is_element_string(SEXP xSEXP,SEXP elSEXP);
SEXP Rfast_k_nn(SEXP ds_extraSEXP,SEXP ySEXP,SEXP dsSEXP,SEXP idxsSEXP,SEXP dist_typeSEXP,SEXP typeSEXP,SEXP methodSEXP,SEXP freq_optionSEXP);
SEXP Rfast_k_nn_cv(SEXP foldsSEXP,SEXP ySEXP,SEXP dsSEXP,SEXP idxsSEXP,SEXP dist_typeSEXP,SEXP typeSEXP,SEXP methodSEXP,SEXP freq_optionSEXP,SEXP pred_retSEXP);
SEXP Rfast_k_comb_n(SEXP nSEXP,SEXP kSEXP);
SEXP Rfast_kron(SEXP xSEXP,SEXP ySEXP);
SEXP Rfast_lowerbound(SEXP xSEXP,SEXP vSEXP);
SEXP Rfast_Lgamma(SEXP x);
SEXP Rfast_len_sort_unique_double(SEXP xSEXP);
SEXP Rfast_len_sort_unique_int(SEXP xSEXP);
SEXP Rfast_Log(SEXP x);
SEXP Rfast_logistic_only(SEXP xSEXP,SEXP ySEXP,SEXP tolSEXP);
SEXP Rfast_logistic_only_b(SEXP xSEXP,SEXP ySEXP,SEXP tolSEXP);
SEXP Rfast_Lbeta(SEXP x,SEXP y);
SEXP Rfast_lower_tri(SEXP xSEXP);
SEXP Rfast_lower_tri_b(SEXP nclSEXP, SEXP nrwSEXP);
SEXP Rfast_Lchoose(SEXP x,SEXP kSEXP);
SEXP Rfast_mahaCpp(SEXP X, SEXP mu, SEXP sigma, SEXP isChol);
SEXP Rfast_matrix_sum(SEXP x);
SEXP Rfast_Match(SEXP xSEXP,SEXP keySEXP);
SEXP Rfast_mad2(SEXP xSEXP,SEXP methodSEXP);
SEXP Rfast_min_max(SEXP x,SEXP indexSEXP);
SEXP Rfast_mat_mat(SEXP xSEXP,SEXP ySEXP);
SEXP Rfast_med(SEXP xSEXP);
SEXP Rfast_min_max_perc(SEXP x);
SEXP Rfast_nth(SEXP xSEXP,SEXP ySEXP);
SEXP Rfast_Norm(SEXP xSEXP,SEXP typeSEXP);
SEXP Rfast_Order(SEXP xSEXP,SEXP stableSEXP,SEXP descendSEXP);
SEXP Rfast_odds_helper(SEXP x);
SEXP Rfast_poisson_only(SEXP xSEXP,SEXP ySEXP,SEXP ylogySEXP,SEXP tolSEXP);
SEXP Rfast_partial_sort(SEXP x,SEXP nSEXP,SEXP descendSEXP);
SEXP Rfast_prop_reg(SEXP xSEXP,SEXP ySEXP,SEXP tolSEXP,SEXP maxitersSEXP);
SEXP Rfast_prop_regs(SEXP xSEXP,SEXP ySEXP,SEXP tolSEXP,SEXP varbSEXP);
SEXP Rfast_pmin(SEXP x,SEXP y);
SEXP Rfast_pmax(SEXP x,SEXP y);
SEXP Rfast_pc_skel(SEXP dsSEXP,SEXP methodSEXP,SEXP sigSEXP,SEXP rSEXP);
SEXP Rfast_poisson_only_b(SEXP xSEXP,SEXP ySEXP,SEXP ylogySEXP,SEXP tolSEXP);
SEXP Rfast_permutation(SEXP xSEXP,SEXP allSEXP,SEXP fnSEXP);
SEXP Rfast_permutation_next(SEXP xSEXP,SEXP all_nextSEXP,SEXP fnSEXP);
SEXP Rfast_permutation_prev(SEXP xSEXP,SEXP all_prevSEXP,SEXP fnSEXP);
SEXP Rfast_qpois_reg(SEXP xSEXP,SEXP ySEXP,SEXP ylogySEXP,SEXP tolSEXP);
SEXP Rfast_qpois_regs(SEXP xSEXP,SEXP ySEXP,SEXP tolSEXP,SEXP ylogySEXP,SEXP mySEXP);
SEXP Rfast_regression(SEXP xSEXP,SEXP ySEXP);
SEXP Rfast_row_any(SEXP xSEXP);
SEXP Rfast_row_means(SEXP xSEXP);
SEXP Rfast_row_max(SEXP xSEXP);
SEXP Rfast_row_meds(SEXP xSEXP);
SEXP Rfast_row_min(SEXP xSEXP,SEXP valueSEXP);
SEXP Rfast_row_len_sort_un_int(SEXP xSEXP);
SEXP Rfast_row_sums(SEXP xSEXP);
SEXP Rfast_rmdp(SEXP ySEXP,SEXP hSEXP,SEXP rndSEXP,SEXP itertimeSEXP);
SEXP Rfast_row_tabulate(SEXP xSEXP,SEXP ncollSEXP);
SEXP Rfast_rep_col(SEXP xSEXP,SEXP nSEXP);
SEXP Rfast_rep_row(SEXP xSEXP,SEXP nSEXP);
SEXP Rfast_row_nth(SEXP xSEXP,SEXP ySEXP);
SEXP Rfast_row_min_max(SEXP x);
SEXP Rfast_row_shuffle(SEXP xSEXP);
SEXP Rfast_Round(SEXP x,SEXP dgSEXP);
SEXP Rfast_read_directory(SEXP pathSEXP);
SEXP Rfast_rvmf_h(SEXP xSEXP,SEXP caSEXP,SEXP d1SEXP,SEXP x0SEXP,SEXP mSEXP,SEXP kSEXP,SEXP bSEXP);
SEXP Rfast_row_true(SEXP xSEXP);
SEXP Rfast_row_prods(SEXP xSEXP);
SEXP Rfast_row_false(SEXP xSEXP);
SEXP Rfast_row_order(SEXP xSEXP,SEXP stableSEXP,SEXP descendingSEXP);
SEXP Rfast_row_true_false(SEXP xSEXP);
SEXP Rfast_read_examples(SEXP path_manSEXP,SEXP no_readSEXP);
SEXP Rfast_row_mads(SEXP x);
SEXP Rfast_row_max_indices(SEXP xSEXP);
SEXP Rfast_row_min_indices(SEXP xSEXP);
SEXP Rfast_sort_unique_double(SEXP xSEXP);
SEXP Rfast_sort_mat(SEXP xSEXP,SEXP descendSEXP,SEXP by_rowSEXP);
SEXP Rfast_stable_sort_mat(SEXP xSEXP,SEXP descendSEXP,SEXP by_rowSEXP);
SEXP Rfast_submatrix(SEXP xSEXP,SEXP rowstartSEXP,SEXP rowendSEXP,SEXP colstartSEXP,SEXP colendSEXP);
SEXP Rfast_sum_XopY(SEXP x,SEXP y,SEXP operSEXP);
SEXP Rfast_sum_XopX(SEXP x,SEXP operSEXP);
SEXP Rfast_sum_lower_tri(SEXP xSEXP);
SEXP Rfast_sum_upper_tri(SEXP xSEXP);
SEXP Rfast_sum_eachrow(SEXP x,SEXP y,SEXP operSEXP);
SEXP Rfast_sort_unique_int(SEXP xSEXP);
SEXP Rfast_symmetric(SEXP xSEXP);
SEXP Rfast_Sort(SEXP x,SEXP descendSEXP);
SEXP Rfast_sort_string(SEXP xSEXP,SEXP descendSEXP);
SEXP Rfast_stable_sort(SEXP x,SEXP descendSEXP);
SEXP Rfast_spat_med(SEXP xSEXP,SEXP tolSEXP);
SEXP Rfast_squareform_c(SEXP xSEXP);
SEXP Rfast_spml_reg_helper(SEXP B1SEXP,SEXP B2SEXP,SEXP xSEXP,SEXP uSEXP,SEXP ciSEXP,SEXP siSEXP,SEXP conSEXP,SEXP tolSEXP);
SEXP Rfast_total_dists(SEXP xSEXP,SEXP methodSEXP,SEXP sqrSEXP,SEXP pSEXP);
SEXP Rfast_total_dista(SEXP xSEXP,SEXP ySEXP,SEXP sqrSEXP);
SEXP Rfast_Trigamma(SEXP x);
SEXP Rfast_table_string(SEXP xSEXP);
SEXP Rfast_table_double(SEXP xSEXP);
SEXP Rfast_upper_tri(SEXP xSEXP);
SEXP Rfast_upper_tri_b(SEXP nclSEXP, SEXP nrwSEXP);
SEXP Rfast_var_c(SEXP xSEXP);
SEXP Rfast_vec_comb_n(SEXP dataSEXP,SEXP nSEXP);
SEXP Rfast_varcomps_mle(SEXP xSEXP,SEXP inaSEXP,SEXP nSEXP,SEXP tolSEXP);
SEXP Rfast_vecdist(SEXP x);
SEXP Rfast_which_isFactor(SEXP xSEXP);
SEXP Rfast_col_row_zero(SEXP xSEXP);

SEXP Rfast_rint_reg(SEXP YSEXP,SEXP XSEXP,SEXP BESEXP,SEXP tolSEXP,SEXP ranefSEXP,SEXP loggedSEXP);
SEXP Rfast_rint_regs(SEXP YSEXP,SEXP XSEXP,SEXP BESEXP,SEXP tolSEXP,SEXP loggedSEXP);
SEXP Rfast_normlog_regs(SEXP YSEXP,SEXP XSEXP,SEXP BESEXP,SEXP conSEXP,SEXP tolSEXP,SEXP loggedSEXP,SEXP parallelSEXP,SEXP maxitersSEXP);
SEXP Rfast_geom_regs(SEXP YSEXP,SEXP XSEXP,SEXP tolSEXP,SEXP loggedSEXP,SEXP typeSEXP,SEXP parallelSEXP,SEXP maxitersSEXP);
SEXP Rfast_normlog_reg(SEXP YSEXP,SEXP XSEXP,SEXP tolSEXP,SEXP maxitersSEXP);
SEXP Rfast_multinom_regs(SEXP Y0SEXP,SEXP X0SEXP,SEXP tolSEXP,SEXP loggedSEXP,SEXP parallelSEXP,SEXP maxitersSEXP);


  

SEXP Rfast_col_max_p(SEXP nSEXP);
SEXP Rfast_col_mean_p(SEXP xSEXP);
SEXP Rfast_col_meds_p(SEXP xSEXP);
SEXP Rfast_col_min_p(SEXP nSEXP);
SEXP Rfast_col_nth_p(SEXP xSEXP,SEXP ySEXP);
SEXP Rfast_col_sum_p(SEXP xSEXP);
SEXP Rfast_row_nth_p(SEXP xSEXP,SEXP ySEXP);
SEXP Rfast_stable_sort_mat_p(SEXP xSEXP,SEXP descendSEXP,SEXP by_rowSEXP);
SEXP Rfast_sort_mat_p(SEXP xSEXP,SEXP descendSEXP,SEXP by_rowSEXP);

static const R_CallMethodDef CallEntries[] = {
  {"Rfast_add_to_namespace", (DL_FUNC) &Rfast_add_to_namespace, 2},
  {"Rfast_bcdcor", (DL_FUNC) &Rfast_bcdcor, 2},
  {"Rfast_binarysearch", (DL_FUNC) &Rfast_binarysearch, 2},
  {"Rfast_bic_fs_reg", (DL_FUNC) &Rfast_bic_fs_reg, 4},
  {"Rfast_bincomb", (DL_FUNC) &Rfast_bincomb, 1},
  {"Rfast_col_meds", (DL_FUNC) &Rfast_col_meds, 1},
  {"Rfast_col_min_indices", (DL_FUNC) &Rfast_col_min_indices, 1},
  {"Rfast_col_min", (DL_FUNC) &Rfast_col_min, 1},
  {"Rfast_col_sums", (DL_FUNC) &Rfast_col_sums, 1},
  {"Rfast_col_min_max", (DL_FUNC) &Rfast_col_min_max, 1},
  {"Rfast_col_max_indices", (DL_FUNC) &Rfast_col_max_indices, 1},
  {"Rfast_col_max", (DL_FUNC) &Rfast_col_max, 1},
  {"Rfast_col_means", (DL_FUNC) &Rfast_col_means, 1},
  {"Rfast_col_nth", (DL_FUNC) &Rfast_col_nth, 2},
  {"Rfast_col_len_sort_un_int", (DL_FUNC) &Rfast_col_len_sort_un_int, 1},
  {"Rfast_col_tabulate", (DL_FUNC) &Rfast_col_tabulate, 2},
  {"Rfast_col_shuffle", (DL_FUNC) &Rfast_col_shuffle, 2},
  {"Rfast_col_true", (DL_FUNC) &Rfast_col_true, 1},
  {"Rfast_col_diffs", (DL_FUNC) &Rfast_col_diffs, 1},
  {"Rfast_col_false", (DL_FUNC) &Rfast_col_false, 1},
  {"Rfast_col_order", (DL_FUNC) &Rfast_col_order, 3},
  {"Rfast_col_prods", (DL_FUNC) &Rfast_col_prods, 1},
  {"Rfast_count_value_string", (DL_FUNC) &Rfast_count_value_string, 2},
  {"Rfast_check_namespace", (DL_FUNC) &Rfast_check_namespace, 2},
  {"Rfast_check_aliases", (DL_FUNC) &Rfast_check_aliases, 3},
  {"Rfast_col_any", (DL_FUNC) &Rfast_col_any, 1},
  {"Rfast_cholesky", (DL_FUNC) &Rfast_cholesky, 1},
  {"Rfast_cholesky_par", (DL_FUNC) &Rfast_cholesky_par, 1},
  {"Rfast_col_mads", (DL_FUNC) &Rfast_col_mads, 1},
  {"Rfast_col_true_false", (DL_FUNC) &Rfast_col_true_false, 1},
  {"Rfast_count_value", (DL_FUNC) &Rfast_count_value, 2},
  {"Rfast_Choose", (DL_FUNC) &Rfast_Choose, 2},
  {"Rfast_dvar", (DL_FUNC) &Rfast_dvar, 1},
  {"Rfast_dcor", (DL_FUNC) &Rfast_dcor, 2},
  {"Rfast_dcov", (DL_FUNC) &Rfast_dcov, 2},
  {"Rfast_diag_matrix_fill_scalar", (DL_FUNC) &Rfast_diag_matrix_fill_scalar, 2},
  {"Rfast_diag_matrix_fill_vec", (DL_FUNC) &Rfast_diag_matrix_fill_vec, 2},
  {"Rfast_diag_fill_scalar", (DL_FUNC) &Rfast_diag_fill_scalar, 2},
  {"Rfast_diag_fill_vec", (DL_FUNC) &Rfast_diag_fill_vec, 2},
  {"Rfast_design_matrix", (DL_FUNC) &Rfast_design_matrix, 2},
  {"Rfast_dist", (DL_FUNC) &Rfast_dist, 4},
  {"Rfast_Digamma", (DL_FUNC) &Rfast_Digamma, 1},
  {"Rfast_design_matrix_big", (DL_FUNC) &Rfast_design_matrix_big, 1},
  {"Rfast_dista", (DL_FUNC) &Rfast_dista, 4},
  {"Rfast_diri_nr_type2", (DL_FUNC) &Rfast_diri_nr_type2, 5},
  {"Rfast_edist", (DL_FUNC) &Rfast_edist, 2},
  {"Rfast_eachrow", (DL_FUNC) &Rfast_eachrow, 3},
  {"Rfast_floyd_john", (DL_FUNC) &Rfast_floyd_john, 2},
  {"Rfast_frame_to_matrix", (DL_FUNC) &Rfast_frame_to_matrix, 2},
  {"Rfast_fs_reg", (DL_FUNC) &Rfast_fs_reg, 5},
  {"Rfast_g2Test_univariate_perm", (DL_FUNC) &Rfast_g2Test_univariate_perm, 3},
  {"Rfast_g2Test_univariate", (DL_FUNC) &Rfast_g2Test_univariate, 2},
  {"Rfast_g2Test_perm", (DL_FUNC) &Rfast_g2Test_perm, 6},
  {"Rfast_g2Test", (DL_FUNC) &Rfast_g2Test, 5},
  {"Rfast_g2tests_perm", (DL_FUNC) &Rfast_g2tests_perm, 5},
  {"Rfast_g2tests", (DL_FUNC) &Rfast_g2tests, 4},
  {"Rfast_group_sum", (DL_FUNC) &Rfast_group_sum, 3},
  {"Rfast_group_all", (DL_FUNC) &Rfast_group_all, 3},
  {"Rfast_group_any", (DL_FUNC) &Rfast_group_any, 3},
  {"Rfast_group_mad", (DL_FUNC) &Rfast_group_mad, 3},
  {"Rfast_group_max", (DL_FUNC) &Rfast_group_max, 3},
  {"Rfast_group_mean", (DL_FUNC) &Rfast_group_mean, 3},
  {"Rfast_group_med", (DL_FUNC) &Rfast_group_med, 2},
  {"Rfast_group_min", (DL_FUNC) &Rfast_group_min, 3},
  {"Rfast_group_min_max", (DL_FUNC) &Rfast_group_min_max, 3},
  {"Rfast_group_var", (DL_FUNC) &Rfast_group_var, 3},
  {"Rfast_glm_logistic", (DL_FUNC) &Rfast_glm_logistic, 4},
  {"Rfast_glm_poisson", (DL_FUNC) &Rfast_glm_poisson, 4},
  {"Rfast_hash2list", (DL_FUNC) &Rfast_hash2list, 2},
  {"Rfast_hash_find", (DL_FUNC) &Rfast_hash_find, 2},
  {"Rfast_Hash_list", (DL_FUNC) &Rfast_Hash_list, 2},
  {"Rfast_is_element", (DL_FUNC) &Rfast_is_element, 2},
  {"Rfast_is_element_string", (DL_FUNC) &Rfast_is_element_string, 2},
  {"Rfast_k_nn", (DL_FUNC) &Rfast_k_nn, 8},
  {"Rfast_k_nn_cv", (DL_FUNC) &Rfast_k_nn_cv, 9},
  {"Rfast_k_comb_n", (DL_FUNC) &Rfast_k_comb_n, 2},
  {"Rfast_kron", (DL_FUNC) &Rfast_kron, 2},
  {"Rfast_lowerbound", (DL_FUNC) &Rfast_lowerbound, 2},
  {"Rfast_Lgamma", (DL_FUNC) &Rfast_Lgamma, 1},
  {"Rfast_len_sort_unique_double", (DL_FUNC) &Rfast_len_sort_unique_double, 1},
  {"Rfast_len_sort_unique_int", (DL_FUNC) &Rfast_len_sort_unique_int, 1},
  {"Rfast_Log", (DL_FUNC) &Rfast_Log, 1},
  {"Rfast_logistic_only_b", (DL_FUNC) &Rfast_logistic_only_b, 3},
  {"Rfast_logistic_only", (DL_FUNC) &Rfast_logistic_only, 3},
  {"Rfast_Lbeta", (DL_FUNC) &Rfast_Lbeta, 2},
  {"Rfast_lower_tri", (DL_FUNC) &Rfast_lower_tri, 1},
  {"Rfast_lower_tri_b", (DL_FUNC) &Rfast_lower_tri_b, 2},
  {"Rfast_Lchoose", (DL_FUNC) &Rfast_Lchoose, 2},
  {"Rfast_mahaCpp", (DL_FUNC) &Rfast_mahaCpp, 4},
  {"Rfast_matrix_sum", (DL_FUNC) &Rfast_matrix_sum, 1},
  {"Rfast_Match", (DL_FUNC) &Rfast_Match, 2},
  {"Rfast_mad2", (DL_FUNC) &Rfast_mad2, 2},
  {"Rfast_min_max", (DL_FUNC) &Rfast_min_max, 2},
  {"Rfast_mat_mat", (DL_FUNC) &Rfast_mat_mat, 2},
  {"Rfast_med", (DL_FUNC) &Rfast_med, 1},
  {"Rfast_min_max_perc", (DL_FUNC) &Rfast_min_max_perc, 1},
  {"Rfast_nth", (DL_FUNC) &Rfast_nth, 2},
  {"Rfast_Norm", (DL_FUNC) &Rfast_Norm, 2},
  {"Rfast_Order", (DL_FUNC) &Rfast_Order, 3},
  {"Rfast_odds_helper", (DL_FUNC) &Rfast_odds_helper, 1},
  {"Rfast_poisson_only", (DL_FUNC) &Rfast_poisson_only, 4},
  {"Rfast_partial_sort", (DL_FUNC) &Rfast_partial_sort, 3},
  {"Rfast_prop_reg", (DL_FUNC) &Rfast_prop_reg, 4},
  {"Rfast_prop_regs", (DL_FUNC) &Rfast_prop_regs, 4},
  {"Rfast_pmin", (DL_FUNC) &Rfast_pmin, 2},
  {"Rfast_pmax", (DL_FUNC) &Rfast_pmax, 2},
  {"Rfast_pc_skel", (DL_FUNC) &Rfast_pc_skel, 4},
  {"Rfast_poisson_only_b", (DL_FUNC) &Rfast_poisson_only_b, 4},
  {"Rfast_permutation", (DL_FUNC) &Rfast_permutation, 3},
  {"Rfast_permutation_next", (DL_FUNC) &Rfast_permutation_next, 3},
  {"Rfast_permutation_prev", (DL_FUNC) &Rfast_permutation_prev, 3},
  {"Rfast_qpois_reg", (DL_FUNC) &Rfast_qpois_reg, 4},
  {"Rfast_qpois_regs", (DL_FUNC) &Rfast_qpois_regs, 5},
  {"Rfast_regression", (DL_FUNC) &Rfast_regression, 2},
  {"Rfast_row_any", (DL_FUNC) &Rfast_row_any, 1},
  {"Rfast_row_means", (DL_FUNC) &Rfast_row_means, 1},
  {"Rfast_row_max", (DL_FUNC) &Rfast_row_max, 1},
  {"Rfast_row_meds", (DL_FUNC) &Rfast_row_meds, 1},
  {"Rfast_row_min", (DL_FUNC) &Rfast_row_min, 1},
  {"Rfast_row_len_sort_un_int", (DL_FUNC) &Rfast_row_len_sort_un_int, 1},
  {"Rfast_row_sums", (DL_FUNC) &Rfast_row_sums, 1},
  {"Rfast_rmdp", (DL_FUNC) &Rfast_rmdp, 4},
  {"Rfast_row_tabulate", (DL_FUNC) &Rfast_row_tabulate, 2},
  {"Rfast_rep_col", (DL_FUNC) &Rfast_rep_col, 2},
  {"Rfast_rep_row", (DL_FUNC) &Rfast_rep_row, 2},
  {"Rfast_row_nth", (DL_FUNC) &Rfast_row_nth, 2},
  {"Rfast_row_min_max", (DL_FUNC) &Rfast_row_min_max, 1},
  {"Rfast_row_shuffle", (DL_FUNC) &Rfast_row_shuffle, 1},
  {"Rfast_Round", (DL_FUNC) &Rfast_Round, 2},
  {"Rfast_read_directory", (DL_FUNC) &Rfast_read_directory, 1},
  {"Rfast_rvmf_h", (DL_FUNC) &Rfast_rvmf_h, 7},
  {"Rfast_row_true", (DL_FUNC) &Rfast_row_true, 1},
  {"Rfast_row_prods", (DL_FUNC) &Rfast_row_prods, 1},
  {"Rfast_row_false", (DL_FUNC) &Rfast_row_false, 1},
  {"Rfast_row_order", (DL_FUNC) &Rfast_row_order, 3},
  {"Rfast_row_true_false", (DL_FUNC) &Rfast_row_true_false, 1},
  {"Rfast_read_examples", (DL_FUNC) &Rfast_read_examples, 2},
  {"Rfast_row_mads", (DL_FUNC) &Rfast_row_mads, 1},
  {"Rfast_row_max_indices", (DL_FUNC) &Rfast_row_max_indices, 1},
  {"Rfast_row_min_indices", (DL_FUNC) &Rfast_row_min_indices, 1},
  {"Rfast_sort_unique_double", (DL_FUNC) &Rfast_sort_unique_double, 1},
  {"Rfast_sort_mat", (DL_FUNC) &Rfast_sort_mat, 3},
  {"Rfast_stable_sort_mat", (DL_FUNC) &Rfast_stable_sort_mat, 3},
  {"Rfast_submatrix", (DL_FUNC) &Rfast_submatrix, 5},
  {"Rfast_sum_XopY", (DL_FUNC) &Rfast_sum_XopY, 3},
  {"Rfast_sum_XopX", (DL_FUNC) &Rfast_sum_XopX, 2},
  {"Rfast_sum_lower_tri", (DL_FUNC) &Rfast_sum_lower_tri, 1},
  {"Rfast_sum_upper_tri", (DL_FUNC) &Rfast_sum_upper_tri, 1},
  {"Rfast_sum_eachrow", (DL_FUNC) &Rfast_sum_eachrow, 3},
  {"Rfast_sort_unique_int", (DL_FUNC) &Rfast_sort_unique_int, 1},
  {"Rfast_symmetric", (DL_FUNC) &Rfast_symmetric, 1},
  {"Rfast_Sort", (DL_FUNC) &Rfast_Sort, 2},
  {"Rfast_sort_string", (DL_FUNC) &Rfast_sort_string, 2},
  {"Rfast_stable_sort", (DL_FUNC) &Rfast_stable_sort, 2},
  {"Rfast_spat_med", (DL_FUNC) &Rfast_spat_med, 2},
  {"Rfast_squareform_c", (DL_FUNC) &Rfast_squareform_c, 1},
  {"Rfast_spml_reg_helper", (DL_FUNC) &Rfast_spml_reg_helper, 8},
  {"Rfast_total_dists", (DL_FUNC) &Rfast_total_dists, 4},
  {"Rfast_total_dista", (DL_FUNC) &Rfast_total_dista, 3},
  {"Rfast_Trigamma", (DL_FUNC) &Rfast_Trigamma, 1},
  {"Rfast_table_string", (DL_FUNC) &Rfast_table_string, 1},
  {"Rfast_table_double", (DL_FUNC) &Rfast_table_double, 1},
  {"Rfast_upper_tri", (DL_FUNC) &Rfast_upper_tri, 1},
  {"Rfast_upper_tri_b", (DL_FUNC) &Rfast_upper_tri_b, 2},
  {"Rfast_var_c", (DL_FUNC) &Rfast_var_c, 1},
  {"Rfast_varcomps_mle", (DL_FUNC) &Rfast_varcomps_mle, 4},
  {"Rfast_vecdist", (DL_FUNC) &Rfast_vecdist, 1},
  {"Rfast_vec_comb_n", (DL_FUNC) &Rfast_vec_comb_n, 2},
  {"Rfast_which_isFactor", (DL_FUNC) &Rfast_which_isFactor, 1},
  {"Rfast_col_row_zero", (DL_FUNC) &Rfast_col_row_zero, 1},


  {"Rfast_col_max_p", (DL_FUNC) &Rfast_col_max_p, 1},
  {"Rfast_col_mean_p", (DL_FUNC) &Rfast_col_mean_p, 1},
  {"Rfast_col_meds_p", (DL_FUNC) &Rfast_col_meds_p, 1},
  {"Rfast_col_min_p", (DL_FUNC) &Rfast_col_min_p, 1},
  {"Rfast_col_nth_p", (DL_FUNC) &Rfast_col_nth_p, 2},
  {"Rfast_col_sum_p", (DL_FUNC) &Rfast_col_sum_p, 1},
  {"Rfast_row_nth_p", (DL_FUNC) &Rfast_row_nth_p, 2},
  {"Rfast_stable_sort_mat_p", (DL_FUNC) &Rfast_sort_mat_p, 3},
  {"Rfast_sort_mat_p", (DL_FUNC) &Rfast_sort_mat_p, 3},

  {"Rfast_rint_reg", (DL_FUNC) &Rfast_rint_reg, 6},
  {"Rfast_rint_regs", (DL_FUNC) &Rfast_rint_regs, 7},
  {"Rfast_normlog_regs", (DL_FUNC) &Rfast_normlog_regs, 8},
  {"Rfast_geom_regs", (DL_FUNC) &Rfast_geom_regs, 7},
  {"Rfast_normlog_reg", (DL_FUNC) &Rfast_normlog_reg, 4},
  {"Rfast_multinom_regs", (DL_FUNC) &Rfast_multinom_regs, 6},
  {NULL, NULL, 0}
};


void R_init_Rfast(DllInfo *info)
{
  R_registerRoutines(info, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
}
