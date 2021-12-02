#For knitting the report and giving it a sensible name in the correct folder

month_num_str <- format(Sys.time(), '%m')
year_num_str <- format(Sys.time(), '%Y')

rmarkdown::render('report_dashboard.rmd',
                  output_file = paste0('../reports/disk_use_report', year_num_str, month_num_str, 
                                      '.html'))