# simulate samples from 4 population balanced tree
baltree_4pop: 4pop_balancedtree_sim.R
    # module input and variables
    pop_sizes: rep(40, 4), c(20,50,30,60)
    n_genes: 1000
    branch_sds: rep(2, 7)
    indiv_sd: 1
    seed: R{1:10}
    # module decoration
    @ALIAS: args = List()
    # module output
    $data: data
    $true_L: data$LL
    $dat: data$YYt

# simulate samples from 10 group overlapping binary structure
group_overlap: group_overlapping_sim.R
    # module input and variables
    n: 100
    p: 1000
    k: 10
    group_sd: 1 
    indiv_sd: 1
    seed: R{1:10}
    pi0: 0.1, 0.3
    # module decoration
    @ALIAS: args = List()
    # module output
    $data: data
    $true_L: data$LL
    $dat: data$YYt

# simulate samples from non-overlapping binary structure
group_nonoverlap: group_nonoverlapping_sim.R
    # module input and variables
    pop_sizes: rep(40, 4), c(20,50,30,60)
    n_genes: 1000
    branch_sds: rep(2, 4)
    indiv_sd: 1
    seed: R{1:10}
    # module decoration
    @ALIAS: args = List()
    # module output
    $data: data
    $true_L: data$LL
    $dat: data$YYt

# simulate samples from 4 population unbalanced tree
unbaltree_4pop: 4pop_unbalancedtree_sim.R
    # module input and variables
    pop_sizes: rep(40, 4)
    n_genes: 1000
    branch_sds: rep(2, 7)
    indiv_sd: 1
    seed: R{1:10}
    # module decoration
    @ALIAS: args = List()
    # module output
    $data: data
    $true_L: data$LL
    $dat: data$YYt

# data split modules
rand_split_by_col: rand_split_by_col.R
    # module input and variables
    seed: 1
    # module decoration
    @ALIAS: args = List()
    input: $data
    # module output
    $data: data_col_split
    $split_type: data_col_split$split_type
    $split_idx: data_col_split$split_idx

rand_split_by_row: rand_split_by_row.R
    # module input and variables
    seed: 1
    @ALIAS: args = List()
    input: $data
    # module output
    $data: data_row_split
    $split_type: data_row_split$split_type
    $split_idx: data_row_split$split_idx

#initialization
pt_laplace_split: runpt_laplace_split_init.R
    # module input and variables
    K_factor: 1
    @ALIAS: args = List()
    input: $data
    # module output
    $init_L: pt_laplace_split_L

codesymnmf_init: runcodesymnmf_initialization.R
    # module input and variables
    K_factor: 2
    @ALIAS: args = List()
    input: $data
    # module output
    $init_L: codesymnmf_L

ebmfcov_greedy_refit: runebmfcov_greedy_refit_init.R
    # module input and variables
    K_factor: 2
    ebnm_fn: ebnm::ebnm_point_exponential
    @ALIAS: args = List()
    input: $data
    # module output
    $init_L: ebmfcov_greedy_refit_L

ebmfcov_greedy: runebmfcov_greedy_init.R
    # module input and variables
    K_factor: 2
    ebnm_fn: ebnm::ebnm_point_exponential
    @ALIAS: args = List()
    input: $data
    # module output
    $init_L: ebmfcov_greedy_L

true_init: set_true_init.R
    # module input and variables
    input: $data
    # module output
    $init_L: true_scaled_L

# analyze  
ebcd: runebcd_backfit.R
    # module input and variables
    # seed: 1
    ebnm_fn: ebnm::ebnm_generalized_binary, ebnm::ebnm_point_exponential
    # module decoration
    @ALIAS: args = List()
    input: $data
    init_L: $init_L
    # module output
    $fit_obj: ebcd_data$ebcd_fit
    $est_L: ebcd_data$est_L
    $est_F: ebcd_data$est_F
    $method: ebcd_data$method

simplified_gbcd: runsimplified_gbcd_backfit.R
    # module input and variables
    ebnm_fn: ebnm::ebnm_generalized_binary, ebnm::ebnm_point_exponential
    @ALIAS: args = List()
    input: $data
    init_L: $init_L
    # module output
    $fit_obj: ebmfcov_diag_data$ebmfcov_diag_fit
    $est_L: ebmfcov_diag_data$est_L
    $est_F: ebmfcov_diag_data$est_F
    $method: ebmfcov_diag_data$method

sindclus: runsindclus_update.R
    # module input and variables
    seed: 1
    additive_term: FALSE
    off_diagonal: FALSE
    # module decoration
    @ALIAS: args = List()
    input: $data
    init_L: $init_L
    # module output
    $fit_obj: sindclus_data$sindclus_fit
    $est_L: sindclus_data$est_L

sympres: runsympres_update.R
    # module input and variables
    seed: 1 
    additive_term: FALSE
    off_diagonal: FALSE
    # module decoration
    @ALIAS: args = List()
    input: $data
    init_L: $init_L
    # module output
    $fit_obj: sympres_data$sympres_fit
    $est_L: sympres_data$est_L

codesymnmf: runcodesymnmf_update.R
    # module input and variables
    input: $data
    init_L: $init_L
    # module output
    $fit_obj: codesymnmf_data$codesymnmf_fit
    $est_L: codesymnmf_data$est_L

symebcovmf: runsymebcovmf_backfit.R
   # mdoule input and variables
   ebnm_fn: ebnm::ebnm_generalized_binary, ebnm::ebnm_point_exponential
   @ALIAS: args = List()
   input: $data
   init_L: $init_L
   # module output
   $fit_obj: symebcovmf_data$symebcovmf_fit
   $est_L: symebcovmf_data$est_L

# baseline

pca: runpca.R
    # module input and variables
    Kmax_factor: 1
    @ALIAS: args = List()
    input: $data
    # module output
    $fit_obj: pca_data$pca_fit
    $est_L: pca_data$est_L

#stability selection

col_split_selection: runcol_split_selection.R
    threshold: 0.5, 0.8
    @ALIAS: args = List()
    input: $est_L
    # module output
    $est_L: col_split_results

row_split_selection: runrow_split_selection.R
    threshold: 0.5, 0.8
    @ALIAS: args = List()
    input: $est_F
    #module output
    $est_F: row_split_results$est_F
    $est_F_pairings: row_split_results$pairings

#methods to estimate L from row split stability selection results
concat_L: run_row_split_selection_concat_L.R
    # module input
    input: $est_L
    pairings: $est_F_pairings
    split_idx: $split_idx
    # module output
    $est_L: row_split_concat_results 

backfit_L: run_row_split_selection_backfit_L.R
    # module input
    ebnm_fn: ebnm::ebnm_generalized_binary
    @ALIAS: args = List()
    F_est: $est_F
    input: $data
    method: $method
    # module output
    $est_L: row_split_backfit_results

# score
prop_est_high_cos_sim: prop_est_high_cos_sim.R
    # module input and variables
    sim_threshold: 0.9, 0.95
    @ALIAS: args = List()
    est: $est_L
    truth: $true_L
    # module output
    $result: result

prop_true_high_cos_sim: prop_true_high_cos_sim.R
   # module input and variables
   sim_threshold: 0.9, 0.95
   @ALIAS: args = List()
   est: $est_L
   truth: $true_L
   # module output
   $result: result 

cov_L2_fit: score_L2_fit.R
   # module input and variables
   est: $est_L
   dat: $dat
   # module output
   $result: result

obj_func_val: get_obj_func_val.R
   # module input and variables
   input: $fit_obj
   # module output
   $result: result

DSC:
    # module ensembles
    define:
      simulate: baltree_4pop, group_overlap, group_nonoverlap, unbaltree_4pop
      data_split: rand_split_by_col, rand_split_by_row
      init: pt_laplace_split, codesymnmf_init, ebmfcov_greedy_refit, ebmfcov_greedy
      analyze_col_split_only: codesymnmf, sindclus, sympres, symebcovmf
      analyze_col_and_row_split: ebcd, simplified_gbcd
      stability_selection: col_split_selection, row_split_selection
      row_split_estimate_L: concat_L, backfit_L
      score: prop_est_high_cos_sim, prop_true_high_cos_sim, cov_L2_fit
      score_no_split: obj_func_val
    # pipelines
    run:
      baseline: simulate * pca * score
      no_stability_selection: simulate * init * (analyze_col_split_only, analyze_col_and_row_split) * (score, score_no_split)
      true_init_comparison: simulate * true_init * (analyze_col_split_only, analyze_col_and_row_split) * (score, score_no_split)
      column_split_selection: simulate * rand_split_by_col * init * (analyze_col_split_only, analyze_col_and_row_split) * col_split_selection * score
      row_split_selection: simulate * rand_split_by_row * init * analyze_col_and_row_split * row_split_selection * row_split_estimate_L * score
    output: full_cov_decomp_comparison

