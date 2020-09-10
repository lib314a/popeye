do_trial_plots <- function (
  line_ret,
  t,
  sub_file,
  dur_five_num,
  show_image,
  save_trial_plots,
  fa_dir
){
  # Constants ------------------------------------------------------------------
  xy_buffer = .1
  pt_size_min = 1
  pt_size_max = 7

  # Handle data ----------------------------------------------------------------

  # Separate out the fixation data
  fix_data_all <- line_ret$fix_data

  # Point sizes based on duration
  m <-
    (pt_size_max - pt_size_min) / (dur_five_num[5] - dur_five_num[1])
  fix_data_all$pt_size <-
    m * (fix_data_all$dur - dur_five_num[1]) + pt_size_min

  # Separate out some more data
  fix_data_keep <- fix_data_all[fix_data_all$type == 'keep', ]
  cats_keep <- line_ret$cats[fix_data_all$type == 'keep']
  fix_data_oob <-  fix_data_all[fix_data_all$type == 'oob', ]
  fix_data_den <-  fix_data_all[fix_data_all$type == 'den', ]
  fix_data_amb <-  fix_data_all[fix_data_all$type == 'amb', ]

  # Plot parameters  ----------------------
  old.par <- par(no.readonly = TRUE)
  par(oma = c(0, 0, 3, 0))

  layout(c(1, 2))
  par(ask = TRUE)

  # x and y limits
  x_min <-
    min(fix_data_all$x) - xy_buffer * (max(fix_data_all$x) - min(fix_data_all$x))
  x_max <-
    max(fix_data_all$x) + xy_buffer * (max(fix_data_all$x) - min(fix_data_all$x))

  y_min <-
    min(fix_data_all$y) - xy_buffer * (max(fix_data_all$y) - min(fix_data_all$y))
  y_max <-
    max(fix_data_all$y) + xy_buffer * (max(fix_data_all$y) - min(fix_data_all$y))

  # Line info ----------------------
  slope <- line_ret$params[1]
  vert_offset <- line_ret$params[2]

  start_pts <- line_ret$start_pts

  n_lines <- nrow(start_pts)

  # Handle background images -----------------
  if (show_image) {
    if (!require('png')) library('png') # modified: bing ----------------------------------------
    if (!require('tiff')) library('tiff')
    # Determine image to show

    # Is there a specific image for this subject and trial?
    image_file_name_trial <-
      paste(substr(sub_file, 1, nchar(sub_file) - 4), '_', t, '.tiff', sep =
              '') # modified: bing -----------------------

    if (file.exists(image_file_name_trial)) {
      # Load the image
      t_image <- readTIFF(image_file_name_trial)

    } else {
      # Get the subject specific image
      image_file_name_sub <-
        paste(substr(sub_file, 1, nchar(sub_file) - 4),  '.tiff', sep = '') # modified: bing -----------------

      if (file.exists(image_file_name_sub)) {
        # Load the image
        t_image <- readTIFF(image_file_name_sub)

      } else {
        # If the image file is missing show the data without it
        warning(paste('Missing image for ', sub_file, ' trial ', t, '.', sep =
                        ''))
        show_image <- FALSE

      }

    }

    # modified: bing ------------------------------------------------------------------------
    # FOLLOWING: original codes
    # # Flip image upside-down
    # t_image@red <- t_image@red[t_image@size[1]:0,]
    # t_image@green <- t_image@green[t_image@size[1]:0,]
    # t_image@blue <- t_image@blue[t_image@size[1]:0,]
    #
    # t_image_width = t_image@size[2]
    # t_image_height = t_image@size[1]

    # FOLLOWING: modified
    t_image <- t_image[length(t_image[, 1, 1]):1, ,]
    t_image <- t_image[, , 1] + t_image[, , 2] + t_image[, , 3]
    t_image <- t_image / max(t_image)
    t_image_width <- length(t_image[1, ])
    t_image_height <- length(t_image[, 1])
    # modified: bing ------------------------------------------------------------------------
  }

  # Make sure the directory exists
  if (save_trial_plots)
    if (!file.exists(paste(fa_dir, 'Trial_Plots', sep = "/")))
      dir.create(paste(fa_dir, 'Trial_Plots', sep = "/"))

  # Plot original data with deletions ----------------------

  if (save_trial_plots)
    png(
      file = paste(
        fa_dir,
        '/Trial_Plots/Orig_',
        gsub('.asc', '', gsub('/', '_', sub_file)),
        '_',
        t,
        '.png',
        sep = ""
      ),
      width = 4267,
      height = 3200,
      units = "px",
      res = 800,
      pointsize = 6
    )

  if (show_image) {
    # If we're drawing an image
    # plot(t_image,
    # 		 main='Original Fixations with Deletions & Classifications',
    # 		 xlab='x', ylab='y',
    # 		 xlim=c(0, t_image_width), ylim=c(t_image_height, 0))
    plot(
      c(0, t_image_width),
      c(0, t_image_height),
      main = 'Original Fixations with Deletions & Classifications',
      xlab = 'x',
      ylab = 'y',
      xlim = c(0, t_image_width),
      ylim = c(t_image_height, 0)
    )
    rasterImage(t_image, 0, 0, t_image_width, t_image_height)
    axis(1)
    axis(2)

  } else {
    # Blank plot
    # Reverse y-limits so 0 at top
    plot(
      1,
      type = 'n',
      main = 'Original Fixations with Deletions & Classifications',
      xlab = 'x',
      ylab = 'y',
      xlim = c(x_min, x_max),
      ylim = c(y_max, y_min)
    )

  }

  ## Lines for fixation ordering
  points(
    fix_data_all$x,
    fix_data_all$y,
    cex = fix_data_all$pt_size,
    col = 'yellow',
    pch = 1,
    type = 'l',
    lty = 'dashed'
  )

  # The kept fixations
  for (i in 1:n_lines) {
    cat <- cats_keep == i

    # Alternate category colors
    if (i %% 2 == 1)
      col = 'black'
    else
      col = 'green'

    points(
      fix_data_keep$x[cat],
      fix_data_keep$y[cat],
      cex = fix_data_keep$pt_size[cat],
      col = col,
      pch = 1
    )

  }
  ## ----------------------------------------------------------------------------modified
  text(
    fix_data_keep$x,
    fix_data_keep$y,
    labels = 1:length(fix_data_keep$x),
    col = 'blue',
    pos = 1
  )

  ## The deleted fixations
  points(
    fix_data_oob$x,
    fix_data_oob$y,
    cex = fix_data_oob$pt_size,
    col = 'red',
    pch = 1
  )
  points(
    fix_data_den$x,
    fix_data_den$y,
    cex = fix_data_den$pt_size,
    col = 'orange',
    pch = 1
  )
  points(
    fix_data_amb$x,
    fix_data_amb$y,
    cex = fix_data_amb$pt_size,
    col = 'purple',
    pch = 1
  )

  # Show fitted lines
  for (i in 1:n_lines)
    lines(
      c(start_pts[i, 1], x_max),
      c(
        start_pts[i, 2] + vert_offset,
        start_pts[i, 2] + slope * (x_max - x_min) + vert_offset
      ),
      col = 'grey'
    )

  legend(
    x = 'bottomleft',
    legend = c('Kept', 'Kept', 'Out-of-bounds', 'Low density', 'Ambiguous'),
    pch = c(1, 1, 1, 1, 1),
    col = c('black', 'green', 'red', 'orange', 'purple'),
    bty = 'n'
  )

  # Show line parameters
  legend(
    x = 'bottomright',
    legend = c(
      paste('Slope =', round(line_ret$params[1], digits = 3)),
      paste('VOffset =', round(line_ret$params[2], digits = 3)),
      paste('SD =', round(line_ret$params[3], digits = 3)),
      paste('Fit =', round(line_ret$fit_measure, digits = 3))
    ),
    bty = 'n'
  )

  if (save_trial_plots)
    dev.off()

  # Plot reformatted data ----------------------

  if (save_trial_plots)
    png(
      file = paste(
        fa_dir,
        '/Trial_Plots/Reform_',
        gsub('.asc', '', gsub('/', '_', sub_file)),
        '_',
        t,
        '.png',
        sep = ""
      ),
      width = 4267,
      height = 3200,
      units = "px",
      res = 800,
      pointsize = 6
    )

  if (show_image) {
    # If we're drawing an image
    plot(
      #t_image,
      c(0, t_image_width),
      c(0, t_image_height),
      main = 'Reformatted Fixations',
      xlab = 'x',
      ylab = 'New y',
      xlim = c(0, t_image_width),
      ylim = c(t_image_height, 0)
    )
    rasterImage(t_image, 0, 0, t_image_width, t_image_height)
    axis(1)
    axis(2)

  } else {
    # Blank plot
    # Reverse y-limits so 0 at top
    plot(
      1,
      type = 'n',
      main = 'Reformatted Fixations',
      xlab = 'x',
      ylab = 'New y',
      xlim = c(x_min, x_max),
      ylim = c(y_max, y_min)
    )

  }

  # The fixations
  for (i in 1:n_lines) {
    cat <- cats_keep == i

    # Alternate category colors
    if (i %% 2 == 1)
      col = 'black'
    else
      col = 'green'

    points(
      fix_data_keep$x[cat],
      fix_data_keep$y_new[cat],
      cex = fix_data_keep$pt_size[cat],
      col = col,
      pch = 1
    )

  }

  # Show desired lines
  for (i in 1:n_lines)
    lines(c(start_pts[i, 1], x_max),
          c(start_pts[i, 2], start_pts[i, 2]),
          col = 'grey')

  # Duration scale information
  legend(
    x = 'bottomleft',
    legend = c(
      paste(dur_five_num[1], 'ms'),
      paste(dur_five_num[2], 'ms'),
      paste(dur_five_num[3], 'ms'),
      paste(dur_five_num[4], 'ms'),
      paste(dur_five_num[5], 'ms')
    ),
    pch = c(1, 1),
    pt.cex = c(
      m * (dur_five_num[1] - dur_five_num[1]) + pt_size_min,
      m * (dur_five_num[2] - dur_five_num[1]) + pt_size_min,
      m * (dur_five_num[3] - dur_five_num[1]) + pt_size_min,
      m * (dur_five_num[4] - dur_five_num[1]) + pt_size_min,
      m * (dur_five_num[5] - dur_five_num[1]) + pt_size_min
    ),
    col = c('black', 'black'),
    bty = 'n'
  )

  mtext(paste('File: ', sub_file, ', Trial: ', t, sep = ''), outer = TRUE)

  if (save_trial_plots)
    dev.off()

}
