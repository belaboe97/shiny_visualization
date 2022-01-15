#https://clarewest.github.io/blog/post/making-tables-shiny/
dt_options = list(paging = TRUE, ## paginate the output
               pageLength = 15,  ## number of rows to output for each page
               scrollX = TRUE,   ## enable scrolling on X axis
               scrollY = TRUE,   ## enable scrolling on Y axis
               autoWidth = TRUE, ## use smart column width handling
               server = FALSE,   ## use client-side processing
               dom = 'Bfrtip',
               columnDefs = list(list(targets = '_all', className = 'dt-center'),
                                 list(targets = c(0, 8, 9), visible = FALSE)))