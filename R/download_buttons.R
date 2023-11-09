download_buttons <- function(t_table, output_name){
  xlsx_button <- t_table |>
    downloadthis::download_this(
      output_name = output_name,
      output_extension = ".xlsx",
      button_label = "Download table as .xlsx",
      button_type = "success",
      has_icon = TRUE,
      icon = "fa fa-save"
    )
  ft_table <- flextable::flextable(t_table)
  flextable::save_as_docx(ft_table, path = paste0(output_name, ".docx"))
  
  docx_button <- 
    downloadthis::download_file(
      path = paste0(output_name, ".docx"),
      output_name = output_name,
      button_label = "Download table as .docx",
      button_type = "success",
      has_icon = TRUE,
      icon = "fa fa-save"
    )
  
  buttons <- list('xlsx' = xlsx_button, 'docx' = docx_button)
}