
#' Generic function to animate the maraca object using animate_maraca().
#'
#' @param x An object of S3 class 'maraca'.
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
    gif_duration = 5
){

    # Make sure %>% comes from dplyr
    `%>%` <- dplyr::`%>%`

    # --- Create Base Maraca Plot ---
    p <- plot(mar, 
              continuous_grid_spacing_x=continuous_grid_spacing_x, 
              density_plot_type=density_plot_type, 
              vline_type=vline_type,
              theme=theme)

    # --- Set Non-animated Layers to Always Display --- 
    # This corresponds to time=1
    p <- set_constant_layers(p)
    
    # Number of plot segments + continuous
    num_segments <- length(mar$step_outcomes) + 1

    # If user provided a single frames_per_step feature
    # expand to a list with length=num_segments
    if (length(frames_per_step) == 1){
      frames_per_step = rep(frames_per_step, num_segments)
    } else if (length(frames_per_step) != num_segments){
      stop("frames_per_step must be a single integer or a list equal to the number of steps+1")
    }
    
    # Set frame delay, i.e. how long each image should show
    frame_delay  <- gif_duration/sum(frames_per_step)

    # List of frame delays that is generated as we 
    # apply animation timings to each plot layer
    frame_delays <- frame_delay

    # Time offset keeps track of when to start animation of next layer
    time_offset  <- 0
    for (idx in 1:length(mar$step_outcomes)){

        # --- Step Information --- 
        outcome   <- as.character(mar$step_outcomes[idx]) # e.g. Outcome I
        type      <- mar$step_types[[idx]] # e.g. tte or binary
        layer_len <- frames_per_step[idx] # number of frames to animate
        
        # --- Get plot layers for this outcome --- 
        outcome_layers <- get_layer_info(outcome, p, type)

        # --- Add Time to Each Layer Data --- 
        for (layer_idx in outcome_layers){
            p <- set_layer_times(p, layer_idx, layer_len, type, time_offset=time_offset)
        }
        time_offset <- time_offset + (layer_len)

        # for continuous (tte) steps, we have many images
        # for binary, we have one long pause on a single image
        if (type == 'tte'){
          layer_nframes <- max(p$layers[[layer_idx]]$data$time) - min(p$layers[[layer_idx]]$data$time)
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
    for (last_layer in last_layer_names){
        outcome_layers <- get_layer_info(outcome=last_layer, p=p, type='final')
        for (layer_idx in outcome_layers){
            p$layers[[layer_idx]]$data$time <- as.numeric(sum(frames_per_step))
        }
    }
    # Set last frame to have no delay
    frame_delays[length(frame_delays)] <- 0

    
    # --- Animate Base GIF -- 
    anim          <- p + transition_reveal(along=time, keep_last=TRUE) + enter_appear()
    animation_dat <- animate(anim, renderer = magick::magick_renderer(loop = FALSE), nframes=length(frame_delays)+1)
    
    # Remove last frame
    animation_dat <- animation_dat[1:(length(animation_dat))-1]
    
    # --- Animate GIF with Final Image --- 
    # Many continuous outcome displays aren't compatable with gganimate::transition_time
    # for instance, violin plots just won't display
    # So we use gganimate::transition_layers to craete a second gif with a properly
    # plotted final state (e.g. with violins)
    # Then we append the last image to the base GIF created above
    # It's important to use transition_layers here so the axes/grids line up perfectly.
    final_anim      <- p + transition_layers()
    total_frames    <- sum(frames_per_step[1:(length(frames_per_step)-1)])
    final_animation <- animate(final_anim, renderer = gifski_renderer())

    # Get Last Frame 
    animated_layers_dat <- image_read(final_animation)
    last_frame          <- animated_layers_dat[length(animated_layers_dat)] 

    # Combine Base GIF + Final Image
    last_pause    <- frames_per_step[length(frames_per_step)]
    last_frames   <- rep(last_frame, last_pause)
    frame_delays  <- c(frame_delays, last_pause*frame_delay)
    extended_animation <- c(animation_dat, last_frame)

    # --- Animate the final GIF ---
    # Convert delay untis from s to ms
    final_animation <- image_animate(extended_animation, delay=frame_delays*100)
    
    return(final_animation)
}



#' Set time of appear for data in a ggplot2 layer
#' 
#' @param p A ggplot2 object produced by maraca's native plot function
#' @param layer_idx The index of the layer that we're adding the time variable to.
#'                  e.g. p$layers[[layer_idx]]
#' @param layer_len How many frames to divide a layer up into
#' @param type Either tte (continuous) or binary
#' @param time_offset We're building animations layer by layer, so we need offset
#'                    the start time for each plot step/segment. 
#' @return Returns ggplot2 plot of the maraca object.
set_layer_times <- function(p, layer_idx, layer_len, type, time_offset=NULL){
  library(dplyr)
  # make sure %>% comes from dplyr
  `%>%` <- dplyr::`%>%`

  # --- Continuous Step --- 
  if ( type == 'tte'){
    p$layers[[layer_idx]]$data <- p$layers[[layer_idx]]$data %>%
      dplyr::group_by(arm) %>%  
      dplyr::mutate(
        time = cut(x, breaks = seq(min(x), max(x), length.out = layer_len + 1),
                   labels = 1:layer_len, 
                   include.lowest = TRUE)
      ) %>%
      dplyr::ungroup()

    p$layers[[layer_idx]]$data$time <- as.numeric(p$layers[[layer_idx]]$data$time) + time_offset
  }
  
  # --- Binary Step --- 
  if ( type == 'binary' ){
    p$layers[[layer_idx]]$data <- p$layers[[layer_idx]]$data %>%
      dplyr::group_by(arm) %>%   # Grouping by 'arm'
      dplyr::do({
        # Replicate data within each group
        num_rows_group <- nrow(.)
        expanded_data <- .[rep(1:num_rows_group, each = layer_len), ]
        
        # Modify the time within each group
        expanded_data$time <- as.numeric((time_offset + 1):(time_offset + nrow(expanded_data)))
        
        expanded_data
      }) %>% 
      dplyr::ungroup()
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






