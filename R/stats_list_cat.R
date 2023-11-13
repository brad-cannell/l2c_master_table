stats_list_cat <- function(data_df, cat_cols, group_var = NULL){
  # Takes the target data frame and list of categorical columns, then
  # generates a **list** of the categorical summary statistics (freq tables)
  # with 95% Confidence Intervals.
  #
  # Built with R version 4.2.2
  #
  # Requires: 
  #     'dplyr' - built with version 1.1.1
  #     'tibble' - built with version 3.2.1
  #     n_percent_ci()
  #     n_percent_ci_grouped()
  #
  # INPUT:
  #       'data_df' (data frame) - the data frame to process for the 
  #                                categorical column summary statistics
  #       'cat_cols' (vector) -    a list of column names in 'data_df',
  #                                using c() and string names of columns,
  #                                which should be processed as categorical 
  #                                columns for summary statistics
  #       'group_var' (string) -   a string indicating the name of the
  #                                factor column that contains the grouping
  #                                assignments. Default is NULL, for ungrouped
  #                                data
  #
  # OUTPUT:
  #       'cat_stats' (list) -     list of the summary statistics
  #                                (count, %, 95% CI for %) for all
  #                                categories in each column in 'cat_cols'
  #                                using data in 'data_df'. 
  #                                in list('var' = tibble::tibble(
  #                                           'var' = var, 
  #                                           'cat' = cat_in_var, 
  #                                           'n' = count, 
  #                                           'formatted_stats' = 
  #                                                 % (95% CI) from 
  #                                                    n_percent_ci() 
  #                                           )
  #                                       )
  #                                format.
  
  # Check if required functions have been imported
  req_functions <- c('n_percent_ci', 'n_percent_ci_grouped')
  
  for (funct_name in req_functions){
    if (!exists(funct_name)){
      stop(
        "Requires the ", funct_name, "() function. ",
        "Please import and try again!")
    }
  }  

    # DATA CHECKS: 
    # 1) Ensure listed columns are actually present in the target data frame
    # 2) Attempt to convert character or numeric vectors into factor based
    #    on unique values - print message indicating this is happening
    # 3) Ensure all listed columns will be able to process as factor, print
    #    warning if unable
    # 4) If any column produces more than 15 levels, print a message indicating
    #    that this may cause excessively bulky frequency tables, requesting
    #    that operator be sure that all passed categorical columns are meant
    #    to be processed as categorical.
    # 5) If group_var was given, ensure it exists and is a factor.
    
    for (t_var in cat_cols){
      # DATA CHECK 1: Ensure all columns in 'cat_cols' are present in 
      #               'data_df'
      
      if ( !(t_var %in% colnames(data_df)) ){
        stop(
          "Column `", t_var, "` is not present in the entered data frame."
        )
      }
      
      # DATA CHECK 2: Attempt to convert character or numeric vectors into
      #               factors based on unique values - print message this is
      #               happening for the user
        
      if( class(data_df[[t_var]]) %in% c('character', 'numeric') ){
        
        message(
          "Column `", t_var, "` is requested as a categorical column, ", 
          "but it is of class <", class(data_df[[t_var]]), ">. ",
          "Attempting to convert to factor based on unique values."
        )
        
        data_df[[t_var]] <- factor(
          data_df[[t_var]], 
          levels = sort(unique(na.omit(data_df[[t_var]])))
        )
      }
      
      # DATA CHECK 3: Ensure categorical column is a factor (processed or not).
      
      if( !(class(data_df[[t_var]]) %in% c('factor')) ){
        # If still not a factor (failed processing, otherwise not identified)
        # stop and throw a warning!
        
        stop(
          "Column `", t_var, "` is not a factor, and failed processing. ",
          "It is of class <", class(data_df[[t_var]]), ">. ",
          "Please correct and retry."
        )
        
      }
    }
    
    # DATA CHECK 4: Display message if a factor has more than 15 levels
    if (length(levels(data_df[[t_var]])) > 15 ){
      message(
        "Column `", t_var, "` has more than 13 levels. This may create ",
        "excessively bulky frequency tables. Ensure all desired categorical ",
        "columns are meant to be interpreted as categoricals."
      )
    }
  
  # DATA CHECK 5: Ensure group_var, if given, exists in data_df as a factor
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
    
    # Process the summary statistics for categorical columns, GROUPED
    cat_stats_list <- cat_cols |>
      rlang::set_names(cat_cols) |> 
      purrr::map(
        ~ doc_data |> 
          filter(!is.na(!!rlang::sym(group_var))) |> 
          n_percent_ci_grouped(!! rlang::sym(.x), !!rlang::sym(group_var), 1))
    
  }
  
  # If group_var was not given...
  if (is.null(group_var)){
    
    # Process the summary statistics for categorical columns
    cat_stats_list <- cat_cols |>
      rlang::set_names(cat_cols) |> 
      purrr::map(~ n_percent_ci(data_df, !! rlang::sym(.x), 1))
  }

  # Return the list of categorical column summary statistics
  cat_stats_list
  
}
