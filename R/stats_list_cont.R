stats_list_cont <- function(
    data_df, cont_mean_cols, cont_median_cols, group_var = NULL
){
  # Takes the target data frame and list of continuous columns, then
  # generates a **list** of the categorical summary statistics (mean)
  # with 95% Confidence Intervals.
  #
  # Built with R version 4.2.2
  #
  # Requires: 
  #     'dplyr' - built with version 1.1.1
  #     'tibble' - built with version 3.2.1
  #     n_mean_ci()
  #     n_median_ci()
  #     n_median_ci_grouped()
  #     n_mean_ci_grouped()
  #
  # INPUT:
  #       'data_df' (data frame) - the data frame to process for the 
  #                                continuous column summary statistics
  #       'cont_mean_cols' (vector) - a list of column names in 'data_df',
  #                                using c() and string names of columns,
  #                                which should be processed as continuous 
  #                                columns for summary statistics using mean
  #       'cont_median_cols' (vector) - a list of column names in 'data_df',
  #                                using c() and string names of columns,
  #                                which should be processed as continuous 
  #                                columns for summary statistics using median
  #       'group_var' (string) -   a string indicating the name of the
  #                                factor column that contains the grouping
  #                                assignments. Default is NULL, for ungrouped
  #                                data
  #
  # OUTPUT:
  #       'cat_stats' (list) -     list of the summary statistics
  #                                (count, mean, 95% CI for %) for all
  #                                categories in each column in 
  #                                'cont_mean_cols' and 'cont_median_cols'
  #                                using data in 'data_df'. 
  #                                in list('var' = tibble::tibble(
  #                                           'var' = var, 
  #                                           'n' = count, 
  #                                           'formatted_stats' = 
  #                                                 mean (95% CI) from 
  #                                                    n_mean_ci() OR
  #                                                 median (95% CI) from
  #                                                    n_median_ci()
  #                                           )
  #                                       )
  #                                format, if group_var == NULL, or
  #                                in list('var' == tibble::tibble(
  #                                           'var' = var,
  #                                           'n_grouplabel' = count of a group,
  #                                           'formatted_stats_grouplabel' = 
  #                                                 mean (95% CI) from 
  #                                                     n_mean_ci_grouped() OR
  #                                                 median (95% CI) from
  #                                                    n_median_ci_grouped()
  #                                           )
  #                                       )
  
  # Check if required functions have been imported
  req_functions <- c(
    'n_mean_ci', 'n_mean_ci_grouped', 'n_median_ci', 'n_median_ci_grouped'
  )
  
  for (funct_name in req_functions){
    if (!exists(funct_name)){
      stop(
        "Requires the ", funct_name, "() function. ",
        "Please import and try again!")
    }
  }  
  
  # DATA CHECKS: 
  # 1) Ensure listed columns are actually present in the target data frame
  # 2) Attempt to convert factor or character vectors into numeric based
  #    on values - print message indicating this is happening
  # 3) Ensure all listed columns will be able to process as numeric, print
  #    warning if unable
  # 4) If group_var was given, ensure it exists and is a factor.
  
  for (t_var in unique(c(cont_mean_cols, cont_median_cols))){
    # DATA CHECK 1: Ensure all columns in 'cont_cols' are present in 
    #               'df_data'
    
    if ( !(t_var %in% colnames(data_df)) ){
      stop(
        "Column `", t_var, "` is not present in the entered data frame."
      )
    }
    
    # DATA CHECK 2: Attempt to convert factor or character columns that
    #               contain only numeric values into numerics
    
    if ( class(data_df[[t_var]]) %in% c('factor', 'character') ){
      
      
      if (class(data_df[[t_var]]) %in% c('factor') ){
        # If the column is a factor, check if all levels are only numeric 
        # digits allowing for decimals ('.').
        all_num <- sum(
          stringr::str_detect(levels(data_df[[t_var]]), '^[0-9\\.,]+$')
        ) == length(levels(data_df[[t_var]]))
        
      }
      
      if (class(data_df[[t_var]]) %in% c('character') ){
        # If the column is a character, check if all unique values are only 
        # numeric digits allowing for decimals ('.').
        all_num <- sum(
          stringr::str_detect(unique(data_df[[t_var]]), '^[0-9\\.]+$')
        ) == length(unique(data_df[[t_var]]))
        
      }
      
      if (!all_num){
        # If not able to convert into numeric, stop and throw an error 
        # message for the user.
        stop(
          "Column `", t_var, "` is requested as a continuous column, but ",
          "it is of class <" , class(data_df[[t_var]]), "> ",
          "with unique values that are unable to conver into numeric. ",
          "Please correct and retry."
        )
      }
      if (all_num){
        # If able to convert into numeric, do so, and print a message 
        # informing that this is happening.
        
        message(
          "Column `", t_var, "` is requested as a continuous column, ", 
          "but it is of class <", class(data_df[[t_var]]), ">. ", 
          "Attempting to convert to numeric based on values."
        )
        
        data_df[[t_var]] <- as.numeric(as.character(data_df[[t_var]]))  
        
      }
    }
    
    # DATA CHECK 3: Ensure continuous column is a numeric (processed or not).
    
    if( !(class(data_df[[t_var]]) %in% c('numeric')) ){
      # If still not a numeric (failed processing, otherwise not identified)
      # stop and throw a warning!
      
      stop(
        "Column `", t_var, "` is not a numeric, and failed processing. ",
        "It is of class <", class(data_df[[t_var]]), ">. ",
        "Please correct and retry."
      )
      
    }
  }
  
  # DATA CHECK 4: Ensure group_var, if given, exists in data_df as a factor
  if (!is.null(group_var)){
    # Check if the column exists in data_df
    if(!(group_var %in% colnames(data_df))){
      stop(
        "The given group variable `", group_var, "` does not exist in the ",
        "given data frame - unable to process grouped data!"
      )
    }
    # Check if the column is a factor in data_df
    if(class(data_df[[group_var]]) != 'factor'){
      stop(
        "The given group variable `", group_var, "` is not a factor in ",
        "given data frame - unable to process grouped data!"
      )
    }
    # Process the summary statistics for continuous columns, GROUPED
    cont_stats_list <- list()
    
    # if there are mean columns:
    if (length(cont_mean_cols > 0)){
      # Process the summary statistics for continuous mean columns, GROUPED
      mean_stats <- cont_mean_cols  |> 
        rlang::set_names(cont_mean_cols) |> 
        purrr::map(
          ~ data_df |> 
            dplyr::filter(!is.na(!!rlang::sym(group_var))) |> 
            group_by(across(all_of(group_var))) |> 
            n_mean_ci_grouped(!! rlang::sym(.x), 1)
        )
      
      cont_stats_list <- c(cont_stats_list, mean_stats)
    }
    
    # if there are median columns:
    if (length(cont_median_cols > 0)){
      # Process the summary statistics for continuous median columns, GROUPED
      median_stats <- cont_median_cols  |> 
        rlang::set_names(cont_median_cols) |> 
        purrr::map(
          ~ data_df |> 
            dplyr::filter(!is.na(!!rlang::sym(group_var))) |> 
            group_by(across(all_of(group_var))) |> 
            n_median_ci_grouped(!! rlang::sym(.x), 1)
        )
      
      cont_stats_list <- c(cont_stats_list, median_stats)
      
    }
  }
  
  # If group_var was not given...
  if (is.null(group_var)){
    
    # Process the summary statistics for continuous columns
    cont_stats_list <- c()
    
    # If there are mean columns...
    if (length(cont_mean_cols) > 0 ){
      mean_stats <- cont_mean_cols |>
        rlang::set_names(cont_mean_cols) |> 
        purrr::map(~ n_mean_ci(data_df, !! rlang::sym(.x), 1))
      
      cont_stats_list <- c(cont_stats_list, mean_stats)
    }
    
    # if there are median columns...
    if (length(cont_median_cols) > 0 ){
      median_stats <- cont_median_cols |>
        rlang::set_names(cont_median_cols) |> 
        purrr::map(~ n_median_ci(data_df, !! rlang::sym(.x), 1))
      
      cont_stats_list <- c(cont_stats_list, median_stats)
    }
    
  }
  
  # Return the list of continuous column summary statistics
  cont_stats_list
}