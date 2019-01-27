save_summary <- function(line_ret, sub_file, out_file, n_trials)
{
  for (t in 1:n_trials)
  {
    slope <- line_ret[[t]]$params[1]
    voffset <- line_ret[[t]]$params[2]
    sd <- line_ret[[t]]$params[3]
    
    fit <- line_ret[[t]]$fit_measure
    
    n_total_fix <- nrow(line_ret[[t]]$fix_data)
    
    count_table <- table(line_ret[[t]]$fix_data$type)
    
    n_keep <-
      length(line_ret[[t]]$fix_data$type[line_ret[[t]]$fix_data$type == 'keep'])
    n_oob <-
      length(line_ret[[t]]$fix_data$type[line_ret[[t]]$fix_data$type == 'oob'])
    n_amb <-
      length(line_ret[[t]]$fix_data$type[line_ret[[t]]$fix_data$type == 'amb'])
    n_den <-
      length(line_ret[[t]]$fix_data$type[line_ret[[t]]$fix_data$type == 'den'])
    n_nit <-
      length(line_ret[[t]]$fix_data$type[line_ret[[t]]$fix_data$type == 'nit'])
    n_part <-
      length(line_ret[[t]]$fix_data$type[line_ret[[t]]$fix_data$type == 'part'])
    
    cat(
      paste(
        sub_file,
        t,
        slope,
        voffset,
        sd,
        fit,
        n_total_fix,
        n_keep,
        n_oob,
        n_amb,
        n_den,
        n_nit,
        n_part,
        '\n',
        sep = ' '
      ),
      file = out_file
    )
    
  }
}
