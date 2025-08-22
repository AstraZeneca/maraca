required_packages <- c("dplyr", "ggplot2", "gganimate", "magick")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(dplyr)
library(ggplot2)
library(gganimate)
library(magick)


#' Generic function to animate the maraca object using animate_maraca().
#'
#' @param mar An object of S3 class 'maraca'.
#' @param continuous_grid_spacing_x The spacing of the x grid to use for the
#'        continuous section of the plot.
#' @param trans the transformation to apply to the x-axis scale for the last
#'        outcome. Possible values are "identity", "log" (only for continuous
#'        endpoint), "log10" (only for continuous endpoint), "sqrt" (only for
#'        continuous endpoint) and "reverse". The default value is "identity".
#' @param density_plot_type The type of plot to use to represent the density.
#'        Accepts "default", "violin", "box" and "scatter".
#' @param vline_type what the vertical dashed line should represent. Accepts
#'        "median" (only for continuous last endpoint), "mean", "none" and
#'        NULL (default). By default (vline_type = NULL), vline_type will be
#'        set to "median" for a continuous last endpoint and to "mean" for
#'        a binary last endpoint.
#' @param theme Choose theme to style the plot. The default theme is "maraca".
#'        Options are "maraca", "maraca_old", "color1", "color2" and none".
#'        For more details, check the vignette called
#'        "Maraca Plots - Themes and Styling".
#' @param frames_per_step The number of individual frames to produce for each 
#'        "step_outcome" (e.g. Outcome I, Outcome II) and continuous outcome. 
#'        Can either be a single integer if you want each segment to have the
#'        same number of frames, or a list of integers if you want them to
#'        differ. The length of the list must equal the length of step outcomes
#'        plus the continuous outcome. 
#' @param gif_duration The length of the animation in seconds. 
#' @param \dots not used
#' @return Returns Magick animation object 
#'
#' @examples
#' data(hce_scenario_a)
#' hce_test <- maraca(
#'   data = hce_scenario_a,
#'   step_outcomes = c("Outcome I", "Outcome II", "Outcome III", "Outcome IV"),
#'   last_outcome = "Continuous outcome",
#'   fixed_followup_days = 3 * 365,
#'   column_names = c(outcome = "GROUP", arm = "TRTP", value = "AVAL0"),
#'   arm_levels = c(active = "Active", control = "Control"),
#'   compute_win_odds = TRUE
#' )
#' animate_maraca(hce_test, frames_per_step=20, gif_duration=5)
#'
#' @export
animate_maraca <- function(mar, 
    continuous_grid_spacing_x = NULL,
    trans = c("identity", "log", "log10", "sqrt", "reverse")[1],
    density_plot_type = c("default", "violin", "box", "scatter")[1],
    vline_type = NULL,
    theme = "maraca", 
    frames_per_step = 20, 
    gif_duration = 5, 
    arm_animation_order = NULL
){

    #arm_animation_order <- c("Control", "Active")

    # Make sure %>% comes from dplyr
    `%>%` <- dplyr::`%>%`


    # --- Create Base Maraca Plot ---
    set.seed(123) # the seed is trying to keep jitter point placement consistent 
    p_base <- plot(mar, 
              continuous_grid_spacing_x=continuous_grid_spacing_x, 
              density_plot_type=density_plot_type, 
              vline_type=vline_type,
              theme=theme)

    # --- Set Non-animated Layers to Always Display --- 
    # This sets things like the grid, axes, and label layers to time=1
    p_base <- set_constant_layers(p_base)

    # Get mapping of group levels (e.g. Control/Active) to colors
    color_map <- get_plot_colormap(p_base)

    # needed!
    set.seed(123)
    p2_base  <- unserialize(serialize(p_base, NULL))
    set.seed(123)
    p2_final <- unserialize(serialize(p_base, NULL))
    # p2_base <- plot(mar, 
    #           continuous_grid_spacing_x=continuous_grid_spacing_x, 
    #           density_plot_type=density_plot_type, 
    #           vline_type=vline_type,
    #           theme=theme)
    # p2_base <- set_constant_layers(p2_base)


    # Number of plot segments (e.g. Outcome I/II/III) + continuous
    num_segments <- length(mar$step_outcomes) + 1

    # If user provided a single frames_per_step input, 
    # then expand it to a list with length=num_segments
    frames_per_step <- get_frames_per_step(frames_per_step, num_segments)

    # Set frame delay, i.e. how long each image should show
    frame_delay <- gif_duration/sum(frames_per_step)

    # List of frame delays that is generated as we 
    # apply animation timings to each plot layer
    frame_delays <- frame_delay

    # Important if we are revealing each arm separately
    alpha_scale <- ggplot2::scale_alpha_manual(
        values = c("alpha_full" = 1, "alpha_half" = 0.5, "alpha_zero" = 0),
        guide = "none"  # Hide legend if not needed
      )
    
    # Animate both arms simultaneously 
    if (is.null(arm_animation_order)){
      res <- add_layer_animation(mar, p_base, 
                                  include_arms=TRUE, 
                                  gif_duration=gif_duration, 
                                  frames_per_step=frames_per_step,
                                  arms_freeze=NULL,
                                  arms_hide=NULL
                                  )
      p1 <- res$p
      all_frame_delays <- res$frame_delays
      time_offset  <- res$time_offset

      p_animation <- animate_plot(p1, all_frame_delays, remove_last_frame = TRUE)
      
      #all_frame_delays <- frame_delays
    # Animate each arm separately
    } else {
      
      # Animate FIRST arm (hide second arm)
      set.seed(123)
      res <- add_layer_animation(mar, p_base, 
                                  include_arms=TRUE, 
                                  gif_duration=gif_duration, 
                                  frames_per_step=frames_per_step,
                                  arms_freeze=NULL,
                                  arms_hide=arm_animation_order[2]
                                  )
      p1           <- res$p + alpha_scale
      frame_delays_1 <- res$frame_delays
      time_offset_1  <- res$time_offset
  
      # Animate the SECOND arm (w/ the first arm frozen)
      #frame_delays <- 0
      set.seed(123)
      res_arm2 <- add_layer_animation(mar, p2_base, 
                                  include_arms=TRUE, 
                                  gif_duration=gif_duration, 
                                  frames_per_step=frames_per_step,
                                  arms_freeze=arm_animation_order[1],
                                  arms_hide=NULL
                                  )
      p2            <- res_arm2$p + alpha_scale
      frame_delays_2 <- res_arm2$frame_delays
      time_offset_2  <- res_arm2$time_offset

      # make the continuous endpoint transparent for arm 2
      # we only want it to show up on the last frame
      p2 <- p2 + ggplot2::scale_color_manual(
        values = color_map,
        na.value = "transparent",  # This makes NA values transparent
        breaks = names(color_map)   # Only show the named values in legend, excludes NA
      )      

      # Fix the legend for the SECOND plot so it matches that of the FIRST
      p2 <- p2 + guides(fill = guide_legend(override.aes = list(
              shape = NA, # No point shape
              linetype = c("solid", "solid"), # Whiskers and median line
              size = 0.5, # Line thickness
              color = c(alpha(color_map[1], 1), alpha(color_map[2], 1)),
              fill = alpha('white', 0.5)
            )))
      
      all_frame_delays <- c(frame_delays_1, frame_delays_2)
      all_frame_delays <- all_frame_delays/2

      # Animate FIRST arm
      p1_animation <- animate_plot(p1, frame_delays_1, remove_last_frame = TRUE)
      #anim_save("~/maraca_animation_base_arm1.gif", p1_animation)

      # Animate SECOND arm
      p2_animation <- animate_plot(p2, frame_delays_2, remove_last_frame = TRUE)
      #anim_save("~/maraca_animation_base_arm2.gif", p2_animation)

      # Combine
      p_animation <- c(p1_animation, p2_animation)
    }
    



    # --- Animate GIF with Final Image --- 
    # Many continuous outcome displays aren't compatable with gganimate::transition_time
    # for instance, violin plots just won't display correctly.
    # So we use gganimate::transition_layers to create a final gif with a properly
    # plotted final state (e.g. with violins)
    # Then we append the last image to the base GIF created above
    # It's important to use transition_layers here so the axes/grids line up perfectly.
    set.seed(123)
    last_frame   <- get_last_animation_frame(p2_final)
    total_frames <- sum(frames_per_step[1:(length(frames_per_step)-1)])

    # --- Combine Base GIF + Final Image --- 
    # also set pause length for the last frame
    last_pause          <- frames_per_step[length(frames_per_step)]
    last_frames         <- rep(last_frame, last_pause)
    final_frame_delays  <- c(all_frame_delays, last_pause*frame_delay)
    extended_animation  <- c(p_animation, last_frame)

    # --- Animate the final GIF ---
    # Convert frame delay units from s to ms
    final_animation <- image_animate(extended_animation, delay=final_frame_delays*100)
    
    return(final_animation)
}

#' Animate a ggplot object
#' 
#' This function takes a ggplot object and animates it over a series of frames.
#' It assumes that there is a "time" column in the data for each ggplot layer 
#'
#' @param p A ggplot object
#' @param frame_delays A vector of frame delays (in seconds)
#' @param remove_last_frame Logical, whether to remove the last frame
#' @return A magick image object
animate_plot <- function(p, 
                        frame_delays, 
                        remove_last_frame=TRUE){
    
    # --- Animate GIF -- 
    anim <- p + 
            transition_reveal(along=time, keep_last=TRUE) + 
            enter_appear()
    animation_dat <- animate(anim, 
                              renderer = gganimate::magick_renderer(loop = FALSE), 
                              nframes = length(frame_delays)+1)
    # Remove last frame
    if (remove_last_frame) {
      animation_dat <- animation_dat[1:(length(animation_dat))-1]
    }
    return(animation_dat)
}


#' Get the last frame of an animation
#' 
#' This function extracts the last frame from a ggplot animation.
#' 
#' @param p A ggplot object that has been animated
#' @return The last frame of the animation
get_last_animation_frame <- function(p){
    final_anim      <- p + transition_layers()
    final_animation <- animate(final_anim, renderer = gifski_renderer())
    
    # Get Last Frame 
    animated_layers_dat <- image_read(final_animation)
    last_frame          <- animated_layers_dat[length(animated_layers_dat)] 
    return (last_frame)
}

#' Get the last frame of an animation
#' 
#' This function extracts the last frame from a ggplot animation.
#' 
#' @param p A ggplot object that has been animated
#' @return The last frame of the animation
get_last_animation_frame <- function(p){
    final_anim      <- p + transition_layers()
    final_animation <- animate(final_anim, renderer = gifski_renderer())

    # Get Last Frame
    animated_layers_dat <- image_read(final_animation)
    last_frame          <- animated_layers_dat[length(animated_layers_dat)]
    return (last_frame)
}

#' Get the number of frames per maracan plotting step (i.e. Outcome I/II/III/etc)
#'
#' This function determines the number of frames to use for each step in the animation.
#'
#' @param frames_per_step A vector of frame counts for each step
#' @param num_segments The total number of segments (steps) in the animation
#' @return A vector of frame counts for each step
get_frames_per_step <- function(frames_per_step, num_segments){
  if (length(frames_per_step) == 1){
    frames_per_step = rep(frames_per_step, num_segments)
  } else if (length(frames_per_step) != num_segments){
    stop("frames_per_step must be a single integer or a list equal to the number of steps+1")
  }
  return(frames_per_step)
}


#' Extract the arm/group to color mapping from a maraca ggplot object
#' 
#' This function retrieves the color mapping for each arm/group in a maraca ggplot object.
#' @param p A maraca ggplot object
#' @return A named vector mapping each arm/group to its color
get_plot_colormap <- function(p){

  # Build plot 
  built_plot <- ggplot_build(p)

  # Get color mapping from constructured data
  built_data <- built_plot$data[[3]]
  plot_data  <- p$layers[[3]]$data

  # Create color mapping
  plot_data$mapped_color <- built_data$colour
  color_palette_tmp <- unique(plot_data[c('arm', 'mapped_color')])
  color_map <- setNames(color_palette_tmp$mapped_color, color_palette_tmp$arm)

  return(color_map)
}


#' Add animation layers to a maraca ggplot object
#'
#' This function adds animation layers to a maraca ggplot object.
#'
#' @param mar A maraca ggplot object
#' @param p A ggplot object
#' @param include_arms A logical vector indicating which arms to include
#' @param gif_duration The total duration of the GIF animation
#' @param frames_per_step A vector of frame counts for each step
#' @return A ggplot object with animation layers added
add_layer_animation <- function(mar, 
                                p, 
                                include_arms,
                                gif_duration,
                                frames_per_step, 
                                arms_freeze = NULL,
                                arms_hide = NULL
                                ) {

    # Set frame delay, i.e. how long each image should show
    frame_delay <- gif_duration/sum(frames_per_step)

    # List of frame delays that is generated as we 
    # apply animation timings to each plot layer
    frame_delays <- frame_delay

    # Time offset keeps track of when to start animation of next layer
    idx <- 1
    time_offset <- 0
    total_frames <- sum(frames_per_step[1:length(mar$step_outcomes)])
    # Iterate over each Outcome Step (i.e. binary outcomes)

    for (idx in 1:length(mar$step_outcomes)){

        # --- Step Information --- 
        outcome   <- as.character(mar$step_outcomes[idx]) # e.g. Outcome I
        type      <- mar$step_types[[idx]] # e.g. tte or binary
        layer_len <- frames_per_step[idx] # number of frames to animate
        
        # --- Get plot layers for this outcome --- 
        outcome_layers <- get_layer_info(outcome, p, type)

        # If we wan't to hide one arm during the animation,
        # set the alpha to 0 for that arm
        if (!is.null(arms_hide)){
          for (layer_idx in outcome_layers){
              p$layers[[layer_idx]]$data$alpha <- ifelse(
                  p$layers[[layer_idx]]$data$arm == arms_hide, 
                  'alpha_zero',  # alpha for HIDDEN arm
                  'alpha_full'   # alpha for DISPLAYED arm
              )              
              
              # Map alpha aesthetic
              p$layers[[layer_idx]]$mapping$alpha <- rlang::quo(alpha)
          }
        }

        # --- Add Time to Each Layer Data --- 
        for (layer_idx in outcome_layers){
            p <- set_layer_times(p, 
                                layer_idx=layer_idx, 
                                layer_len=layer_len, 
                                type=type, 
                                time_offset=time_offset,
                                arms_freeze=arms_freeze,
                                arms_hide=arms_hide, 
                                total_frames=total_frames)
        }
        time_offset <- time_offset + (layer_len)

        # for continuous (tte) steps, we have many images
        # for binary, we have one long pause on a single image
        if (type == 'tte'){
          time_arr <- p$layers[[layer_idx]]$data$time
          time_arr <- time_arr[time_arr != total_frames]
          time_arr <- time_arr[time_arr != 0]
          layer_nframes <- max(time_arr, na.rm=TRUE) - min(time_arr, na.rm=TRUE)
          frame_delays <- c(frame_delays, rep(frame_delay, layer_nframes))

        } else if (type == 'binary'){
          frame_delays <- c(frame_delays, frame_delay*layer_len)
        }
    }
    
    # --- Set Final Outcome Layer --- 
    # List of layer components that appear last
    last_layer_names <- c("continuous_vlines", 
                          "lastdata", 
                          "lastdata_meta", 
                          "last_horizontal", 
                          "continuous_horizontal") 
    # Set the last layer components to appear at max time
    max_frame = sum(frames_per_step[1:length(frames_per_step)-1])
    for (last_layer in last_layer_names){

        # Get the layers corresponding to this component
        outcome_layers <- get_layer_info(outcome=last_layer, 
                                         p=p, 
                                         type='final')

        # Set the time variable to max_frame+1
        for (layer_idx in outcome_layers){

            #p$layers[[layer_idx]]$data$time <- as.numeric(sum(frames_per_step))
            p$layers[[layer_idx]]$data$time <- max_frame + 1

            if ( !is.null(arms_freeze) ){
                p$layers[[layer_idx]]$data$time <- NULL

                # reset alpha to NULL
                p$layers[[layer_idx]]$aes_params$alpha <- NULL

                # Modify Alpha levels
                layer_class <- class(p$layers[[layer_idx]]$geom)[[1]]
                if (layer_class %in% c("GeomBoxplot", "GeomViolin", "GeomPolygon")){

                  # ALPHA 
                  p$layers[[layer_idx]]$data$alpha <- ifelse(
                      p$layers[[layer_idx]]$data$arm == arms_freeze, 
                      'alpha_half',  # alpha for Control arm
                      'alpha_zero'   # alpha for Active arm
                  )    

                  # RESET COLOR MAPPING
                  p$layers[[layer_idx]]$data$line_map <- p$layers[[layer_idx]]$data$arm
                  p$layers[[layer_idx]]$data$line_map <- ifelse(
                      p$layers[[layer_idx]]$data$arm == arms_freeze, 
                      arms_freeze,  # color for Control arm
                      NA    # color for Active arm
                  )

                  p$layers[[layer_idx]]$mapping$alpha <- rlang::quo(alpha)
                  p$layers[[layer_idx]]$mapping$colour <- rlang::quo(line_map)

                } else {

                  # ALPHA
                  p$layers[[layer_idx]]$data$alpha <- ifelse(
                      p$layers[[layer_idx]]$data$arm == arms_freeze, 
                      'alpha_full',  # alpha for Control arm
                      'alpha_zero'   # alpha for Active arm
                  )   
                }
                p$layers[[layer_idx]]$mapping$alpha <- rlang::quo(alpha)
            }
        }
    }
    # Set last frame to have no delay
    frame_delays[length(frame_delays)] <- 0

  return(list(p=p, 
              frame_delays=frame_delays, 
              time_offset=time_offset))
}


#' Set time of appear for a layer in a ggplot2 layer
#' 
#' @param p A ggplot2 object produced by maraca's native plot function
#' @param layer_idx The index of the layer that we're adding the time variable to.
#'                  e.g. p$layers[[layer_idx]]
#' @param layer_len How many frames to divide a layer up into
#' @param type Either tte (continuous) or binary
#' @param time_offset We're building animations layer by layer, so we need offset
#'                    the start time for each plot step/segment. 
#' @return Returns ggplot2 plot of the maraca object.
set_layer_times <- function(p, 
                            layer_idx, 
                            layer_len, 
                            type, 
                            time_offset=NULL, 
                            arms_freeze=NULL,
                            arms_hide=NULL, 
                            total_frames=NULL){
  # make sure %>% comes from dplyr
  `%>%` <- dplyr::`%>%`

  # --- Continuous Step --- 
  if ( type == 'tte'){
    # layer data
    data <- p$layers[[layer_idx]]$data
  
    # separate the horizontal lines in continuous outcome part of plot
    # this really only applies to the final "step" before the continuous outcome
    # in a maraca plot.
    # We do not want to animate the final horizontal line, we want it to appear
    # with whatever else it is being plotted with (e.g. violine, boxplot, etc)
    d100 <- data[data$x == 100, ]
    data <- data[data$x != 100, ]

    # add time component
    data_fmt <- data %>%
      dplyr::mutate(
        time = cut(x, breaks = seq(min(x), max(x), length.out = layer_len + 1),
                    labels = 1:layer_len, 
                    include.lowest = TRUE)
      )
    data_fmt$time <- as.numeric(data_fmt$time)

    # If there is a d100, we need to add it back in
    # so that the final frame has the last data point
    # at x=100
    if (length(d100) > 0){
      d100$time <- max(data_fmt$time) 
      data_use  <- dplyr::bind_rows(data_fmt, d100)
    } else {
      data_use <- data_fmt
    }

    geom_class <- class(p$layers[[layer_idx]]$geom)[1]

    # update the data with a df that has the "time" column
    p$layers[[layer_idx]]$data <- data_use

    # layers are built sequentially, so offset time to account for previously built layers
    p$layers[[layer_idx]]$data$time <- as.numeric(p$layers[[layer_idx]]$data$time) + time_offset
  }
  
  # --- Binary Step --- 
  if ( type == 'binary' ){
    # Get the original data
    original_data <- p$layers[[layer_idx]]$data
    
    # The binary data are plotted as geom segments, but we need it to remain on the 
    # screen for longer than it would stay by default. 
    # So we just duplicate the rows for binary maraca steps however long is needed
    expanded_data <- original_data %>%
      dplyr::group_by(arm) %>%
      dplyr::group_modify(~ {
        num_rows_group <- nrow(.x)
        replicated_data <- .x[rep(1:num_rows_group, each = layer_len), ]
        replicated_data$time <- as.numeric((time_offset + 1):(time_offset + nrow(replicated_data)))
        replicated_data
      }) %>%
      dplyr::ungroup()

    # Update layer with df that has the "time" column
    p$layers[[layer_idx]]$data <- expanded_data
  }

  # Freeze arms by setting time=0
  if ( !is.null(arms_freeze) ){
    p$layers[[layer_idx]]$data[(p$layers[[layer_idx]]$data$arm %in% arms_freeze),]$time <- 0
  }

  # hide arms by setting the time to the last frame, which gets censored from the gif
  if ( !is.null(arms_hide) ){
    p$layers[[layer_idx]]$data[(p$layers[[layer_idx]]$data$arm %in% arms_hide),]$time <- total_frames
  }
  return(p)
}


#' Initialize time feature in each dataframe in the plot
#' 
#' @param p A ggplot2 object produced by maraca's native plot function
#' @return p Returns ggplot2 plot of the maraca object.
#' 
#' @examples 
#' data(hce_scenario_a)
#' hce_test <- maraca(
#'   data = hce_scenario_a,
#'   step_outcomes = c("Outcome I", "Outcome II", "Outcome III", "Outcome IV"),
#'   last_outcome = "Continuous outcome",
#'   fixed_followup_days = 3 * 365,
#'   column_names = c(outcome = "GROUP", arm = "TRTP", value = "AVAL0"),
#'   arm_levels = c(active = "Active", control = "Control"),
#'   compute_win_odds = TRUE
#' )
#' p <- plot(hce_test)
#' p <- set_constant_layers(p)
#' 
#' @export
set_constant_layers <- function(p){

  # --- Add time variable to plot data --- 
  # the time feature determines when plot 
  # components  will be displayed
  # A value of 1 means the component will always be present
  p$data$time <- as.numeric(1)

  # Iterate over each layer and set time=1 
  # The layers will not display if time is not present
  for (i in seq_along(p$layers)){
    p$layers[[i]]$data$time <- as.numeric(1)
    if ( !"source" %in% colnames(p$layers[[i]]$data) ){
        p$layers[[i]]$data$time <- NULL
    }
  }
  return(p)
}


#' Get the layer indices corresponding to a particular plot type.
#' plot.maraca adds a "source" column to mark which layers correspond to 
#' which data types and plot segments. For instance, "source" could be 
#' "Outcome I" or "continuous_vlines"
#' 
#' @param outcome string that is in the p$layers[[i]]$data$source column
#' @param p A ggplot2 object produced by maraca's native plot function
#' @param type Either 'tte', 'binary', or 'final'
#' @return Returns an array of indices for the matching layers 
get_layer_info <- function(outcome, p, type) {
  outcome_layers <- integer()  # Initialize an integer vector to store indices
  
  if (!is.character(outcome)) {
    stop("The 'outcome' argument must be a character string.") # Ensure 'outcome' is a character
  }
  
  for (i in seq_along(p$layers)) {
    if ("source" %in% colnames(p$layers[[i]]$data)) {
      source <- p$layers[[i]]$data$source[[1]]

      # Use 'grepl' with properly passed character pattern
      if (type == 'tte'){
        if ( source == outcome ) {
          outcome_layers <- c(outcome_layers, i)  # Append index to vector
        }
      } else if (type == 'binary'){
        if (grepl(outcome, source)) {
          outcome_layers <- c(outcome_layers, i)  # Append index to vector
        }
      } else if (type == 'final'){
        if (grepl(outcome, source)) {
          outcome_layers <- c(outcome_layers, i)  # Append index to vector
        }
      }
    }
  }
  return(outcome_layers)
}






