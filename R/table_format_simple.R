table_format_simple = function( TABLE, table.font.size = 12 ) {
  # format tabular data into a table for quarto documents
  out = gt::gt( TABLE ) 
  out = gt::tab_options( out,  
      table.font.size = table.font.size, 
      data_row.padding = gt::px(1), 
      summary_row.padding = gt::px(1), 
      grand_summary_row.padding = gt::px(1), 
      footnotes.padding = gt::px(1), 
      source_notes.padding = gt::px(1), 
      row_group.padding = gt::px(1)
    )
  return(out)
} 
