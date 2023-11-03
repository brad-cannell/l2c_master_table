get_stats_table <- function(
    data_df, cont_mean_cols, cont_median_cols, cat_cols, 
    desired_col_order, group_var = NULL,
    key_df = NULL, shade_colors = c('white', '#ededed')
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
  #     'DT' - built with version 0.30
  #     stats_list_cont()
  #     stats_list_cat()
  #     add_shade_column()
  #
  # INPUT:
  #       'data_df' (data frame) - the data frame to process for the 
  #                                summary statistics
  #       'cont_mean_cols' (vector) - a list of column names in 'data_df',
  #                                using c() and string names of columns,
  #                                which should be processed as continuous 
  #                                columns for summary statistics using mean
  #       'cont_median_cols' (vector) - a list of column names in 'data_df',
  #                                using c() and string names of columns,
  #                                which should be processed as continuous 
  #                                columns for summary statistics using median
  #       'cat_cols' (vector) -    a list of column names in 'data_df',
  #                                using c() and string names of columns,
  #                                which should be processed as categorical 
  #                                columns for summary statistics
  #       'desired_col_order' (vector) - a list of column names using c() and
  #                                string names of columns, indicating the 
  #                                desired order of display for all columns 
  #                                named in 'cat_cols' and 'cont_cols'
  #       'group_var' (string) -   a string indicating the name of the
  #                                factor column that contains the grouping
  #                                assignments. Default is NULL, for un-grouped
  #                                data
  #       'key_df' (data frame) -  the data frame containing the variable key
  #                                for the document, which should be of the 
  #                                following minimum format:
  #                                   'doc_var' = name of the column in
  #                                           'data_df', for the document
  #                                   'source_var' = name of the column in
  #                                           the source data frame
  #                                Default is NULL.
  #       'shade_colors' (list) -  A c() list of two color codes (hexidecimal
  #                                or otherwise understood by base R) used to
  #                                assign the shading of alternating variables
  #                                in the stat_table display.
  #                                Default is c('white', '#ededed').
  #
  # OUTPUT:
  #       'stats_table' (data frame) - data frame of the summary statistics
  #                                for all variables entered, formatted and
  #                                shaded.
  
  # Check if required functions have been imported
  req_functions <- c('stats_list_cont', 'stats_list_cat', 'add_shade_column')
  
  for (funct_name in req_functions){
    if (!exists(funct_name)){
      stop(
        "Requires the ", funct_name, "() function. ",
        "Please import and try again!")
    }
  }
  
  # DATA CHECKS:
  #   1) Ensure all cols in cont_mean_calls, cont_median_cols, cat_cols, and 
  #      desired_col_order are present in data_df
  #   2) Ensure all cols in cont_mean_calls, cont_median_cols, and cat_cols 
  #      are present in desired_col_order
  #   3) Ensure all cols in desired_col_order are present in cat_cols,
  #      cont_mean_cols, and cont_median_cols
  #   4) Ensure no col is included in both cat_cols and either 
  #      cont_mean_cols or cont_median_cols
  
  
  # DATA CHECK 1: Ensure all cols in cont_mean_cols, cont_median_cols, 
  #               cat_cols, and desired_col_order are present in data_df
  
  missing_cols <- setdiff(
    unique(c(cont_mean_cols, cont_median_cols, cat_cols, desired_col_order)), 
    colnames(data_df)
  )
  
  if( length(missing_cols) > 0 ){
    stop(
      "ERROR: ", length(missing_cols), "columns not present in the data frame: ",
      "`", paste(missing_cols, sep = "`, `"), "`. Please check inputs and ",
      "and retry."
    )
  }
  
  # DATA CHECK 2: Ensure all cols in cont_mean_calls, cont_median_cols, and 
  #               cat_cols are present in desired_col_order
  
  missing_cols <- setdiff(
    unique(c(cont_mean_cols, cont_median_cols, cat_cols)), desired_col_order
    )
  
  if ( length(missing_cols) > 0){
    stop(
      "ERROR: ", length(missing_cols), "columns are requested but are not in ",
      "`desired_col_order`: ",
      "`", paste(missing_cols, sep = "`, `"), "`. Please ensure all desired ",
      "columns are assigned an order and retry."
    )
  }
  
  # DATA CHECK 3: Ensure all cols in desired_col_order are present in 
  #               cat_cols, cont_mean_cols, or cont_median_cols
  
  missing_cols <- setdiff(
    desired_col_order, 
    unique(c(cont_mean_cols, cont_median_cols, cat_cols))
    )
  
  if ( length(missing_cols) >0 ){
    stop(
      "ERROR: ", length(missing_cols), "columns are given an order but not ",
      "assigned within `cont_cols` or `cat_cols`",
      "`", paste(missing_cols, sep = "`, `"), "`. Please ensure all desired ",
      "columns are identified as continuous or categorical and retry."
    )
  }
  
  # DATA CHECK 4: Ensure no column appears in both cat_cols and either
  #               cont_median_cols or cont_mean_cols
  
  both_cols <- cat_cols[cat_cols %in% c(cont_mean_cols, cont_median_cols)]
  
  if ( length(both_cols) > 0 ){
    stop(
      "ERROR: ", length(both_cols), "columns are listed as both continuous ",
      "and categorical: ",
      "`", paste(both_cols, sep = "`, `"), "`. Please ensure columns ",
      "columns are identified as EITHER continuous or categorical and retry."
    )
  }
  
  stats_list <- list()
  
  # Get continuous column summary stats, if any continuous columns passed
  if (length(c(cont_mean_cols, cont_median_cols) > 0)) {
    stats_list <- c(
      stats_list, 
      stats_list_cont(data_df, cont_mean_cols, cont_median_cols, group_var)
      )
  }
  
  # Get categorical column summary stats, if any categorical columns passed
  if(length(cat_cols > 0)){
    stats_list <- c(stats_list, stats_list_cat(data_df, cat_cols, group_var))
  }
  
  # So long as stats_list has content....
  if (length(stats_list) >0 ){
    
    # Initiate the table in the desired order:
    stats_table <- purrr::map_dfr(
      .x = c(desired_col_order),
      .f = ~ dplyr::bind_rows(stats_list[[.x]])
    ) |>
      # Reorder the columns so that `cat` and `n` comes after `var`
      dplyr::select(var, cat, n, everything())
    
    # If the key is passed, and all desired_col_order vars are in key_df$doc_var,
    # rename all 'var' values in 'stats_list' to the 'source_var' for display.
    if( nrow(key) > 0 & 
        sum(desired_col_order %in% key_df$doc_var) == length(desired_col_order)){
      
      stats_table <- stats_table |>
        dplyr::rowwise() |>
        dplyr::mutate(
          var = key_df[key_df$doc_var == var,]$source_var
        ) |>
        dplyr::ungroup() 
      
    }
    
    # Add a column to facilitate shading every other 'var' in 'stats_table'
    stats_table <- add_shade_column(stats_table)
    
    stats_table <- DT::datatable(
      stats_table,
      colnames = c(
        "Shade", "Variable", "Category", "N", "Statistic Type", 
        "Statistic Value"
      ),
      #escape = FALSE, # So the HTML superscript in the column header will work
      options = list(
        pageLength = 20,
        columnDefs = list(
          # Center n and formatted stats
          list(className = 'dt-center', targets = 4:5),
          # Hide row numbers column from view
          list(targets = 0, visible = FALSE),
          # Hide "shade" column from view
          list(targets = 1, visible = FALSE)
        )
      )
    ) |> 
      DT::formatStyle(
        "shade",
        target = "row",
        backgroundColor = DT::styleEqual(c(0, 1), shade_colors)
      )
    
    # Return the summary statistics table
    stats_table
    
  }
}