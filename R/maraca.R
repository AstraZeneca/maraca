library(ggfortify)

`%>%` <- dplyr::`%>%`

# Function that scale data to a range
rangeab <- function(x, a, b) {
  (b - a) * (x - min(x)) / (max(x) - min(x)) + a
}

##########################
# Create the HCE dataset #
##########################

### Read in the data ###
HCE <- read.csv("tests/fixtures/HCE scenario A.csv")

vars <- dplyr::vars

# Input parameters
ep.order <- c("Outcome I", "Outcome II", "Outcome III", "Outcome IV", "Continuous outcome")  # Order of the endpoints. The continuous endpoint should always be last.
treatments <- c("Active", "Control")
fixed.follow.up.days <- 3*365

# Remove unwanted endpoints and treatments
HCE <- HCE %>% dplyr::filter(GROUP %in% ep.order) %>%
  dplyr::mutate_at(vars(GROUP), factor, levels=ep.order) %>%
  dplyr::mutate_at(vars(TRTP), factor, levels=treatments)




##############################################
# Function to calculate the Win-Odds results #
##############################################
# calcWinOdds <- function(data, ordinalVal, treatments, reference)
# {
#   fit <- sanon(ordinalVal ~ grp(treatments, ref = reference), data = HCE)
#   CI0 <- confint(fit)$ci
#   CI <- CI0/(1-CI0)
#   p <- fit$p
#
#   wo.result <- c(CI, p)
#   names(wo.result) <- c("estimate", "lower", "upper", "p-value")
#
#   return(wo.result)
# }
#
# wo.result <- calcWinOdds(data = HCE, ordinalVal = "AVAL", group = "TRTP", reference = "Control")


grp <- sanon::grp
fit <- sanon::sanon(AVAL ~ grp(TRTP, ref = "Control"), data = HCE)
CI0 <- confint(fit)$ci
CI <- CI0/(1-CI0)
p <- fit$p

wo.result <- c(CI, p)
names(wo.result) <- c("estimate", "lower", "upper", "p-value")




##############################################
### Calculations that support the ploting  ###
##############################################


# Maraca <- function(data, ep.order, treatments) {


### Calculate meta information from the entire HCE dataset needed for plotting ###
##################################################################################
  n <- dplyr::n

  meta1 <- HCE %>% dplyr::group_by(GROUP) %>%
    dplyr::summarise(n = n(), proportion = n/dim(HCE)[1]*100, maxday = max(AVAL0)) %>%
    dplyr::mutate(fixed.followup = fixed.follow.up.days, startx = c(0, cumsum(head(proportion, -1))), endx = cumsum(proportion), starty = 0, n.groups = length(unique(GROUP)))

  meta2 <- HCE %>% dplyr::group_by(GROUP, TRTP) %>% dplyr::summarise(n = n(), proportion = n/dim(HCE)[1]*100) %>% tidyr::pivot_wider(names_from = TRTP, values_from = c(n, proportion))


  meta <- dplyr::left_join(meta1, meta2, "GROUP")




### Prepare data and calculate meta information about the time-to-event data ###
################################################################################

  HCE$kmday <- max(meta[meta$GROUP %in% head(ep.order, -1),]$maxday)     # Use the largest value across the hard endpoints if fixed.follow.up.days is not specified
  HCE$kmday <- meta$fixed.followup[1]                                    # Use the specified length of the fixed-follow-up trial if specified

  HCE[HCE$GROUP %in% head(ep.order, -1),]$kmday <- HCE[HCE$GROUP %in% head(ep.order, -1),]$AVAL0

  Surv <- survival::Surv
  # Create survival model dataset
  survmod.data <- cbind(ggplot2::fortify( with(HCE, survival::survfit(Surv(time = kmday, event = GROUP == ep.order[1]) ~ TRTP))), GROUP = ep.order[1])   # Kolla TRTP vs treatments !!
  for(i in 2:length(ep.order)-1) {
    survmod.data <- rbind(survmod.data, cbind(ggplot2::fortify( with(HCE, survival::survfit(Surv(time = kmday, event = GROUP == ep.order[i]) ~ TRTP)) ), GROUP = ep.order[i] ))
  }

  survmod.data <- survmod.data %>% dplyr::mutate_at(vars(GROUP), factor, levels=ep.order) %>% dplyr::mutate_at(vars(strata), factor, levels=treatments)

  survmod.data$adjusted.time <- 0
  for(i in head(ep.order, -1)) {
    survmod.data[survmod.data$GROUP == i,]$adjusted.time <- meta[meta$GROUP == i,]$startx + survmod.data[survmod.data$GROUP == i,]$time/max(survmod.data[survmod.data$GROUP == i,]$time)*meta[meta$GROUP == i,]$proportion
  }

#  { (b - a)*(x - min(x))/(max(x) - min(x)) + a } Could use this for the adjustment of time? !!

  survmod.data <- survmod.data %>% dplyr::mutate(km.y = 1-surv)

  survmod.meta <- survmod.data %>% dplyr::group_by(strata, GROUP) %>%
    dplyr::summarise(max = 100*max(1-surv), sum.event = sum(n.event)) %>%
    dplyr::mutate(km.start = c(0, cumsum(head(max, -1))), km.end = cumsum(max))


  survmod.data <- survmod.data %>% dplyr::left_join(survmod.meta, by = c("strata", "GROUP") )



### Calculate meta information from the continuous dataset needed for plotting ###
##################################################################################
  slopedata <- HCE[HCE$GROUP == tail(ep.order, 1),]

  start.cont.ep <- meta[meta$GROUP == tail(ep.order, 1),]$startx

  to.rangeab <- function(x) { (100 - start.cont.ep)*(x - min(slopedata$AVAL0))/(max(slopedata$AVAL0) - min(slopedata$AVAL0)) + start.cont.ep }
  from.rangeab <- function(y) {(y - start.cont.ep)/(100 - start.cont.ep)*(max(slopedata$AVAL0) - min(slopedata$AVAL0)) + min(slopedata$AVAL0)}

  zeroposition <- to.rangeab(0)
  slopedata$x = to.rangeab(slopedata$AVAL0)


  slope.meta <- slopedata %>% dplyr::group_by(TRTP) %>% dplyr::summarise(n = n(), median = median(x), average = mean(x))


  slopedata$violinx <- 0
  slopedata[slopedata$TRTP == treatments[1],]$violinx <- seq(from = start.cont.ep, to = 100, length.out = slope.meta$n[1])
  slopedata[slopedata$TRTP == treatments[2],]$violinx <- seq(from = start.cont.ep, to = 100, length.out = slope.meta$n[2])
  slopedata$violiny <- survmod.meta[survmod.meta$strata == treatments[1] & survmod.meta$GROUP == ep.order[length(ep.order)-1],]$km.end
  slopedata[slopedata$TRTP == treatments[2], ]$violiny <- survmod.meta[survmod.meta$strata == treatments[2] & survmod.meta$GROUP == ep.order[length(ep.order)-1],]$km.end


# ggplot(slopedata) + geom_violin(aes(x = x, y = violiny, color = TRTP))




#####################
# Plotting the data #
#####################

  ## Simple plot of TTE endpoints side by side
  tte.trellis <- function(survmod.data) {
    ggplot(survmod.data) + geom_line(aes(x=time, y=km.y*100, color=strata)) + facet_grid(cols = vars(GROUP))
  }
  tte.trellis(survmod.data)



  ## Actual Maraca plot ##

  # Calculate the gridlines for the continuous outcome scale to be the best fit for a separation of 10 units (could be an input to the function)
  minorGrid <- seq(sign(min(slopedata$AVAL0))*floor(abs(min(slopedata$AVAL0))/10)*10, sign(max(slopedata$AVAL0))*floor(abs(max(slopedata$AVAL0))/10)*10, by=10)


`plot.maraca::maraca` <- function(obj) {
  survmod.data
  TRTP
  meta
  zeroposition
  slope.meta
  aes <- ggplot2::aes
  slopedata
  ep.order
  minorGrid

  # Plot the information in the Maraca plot
  ggplot2::ggplot(survmod.data, aes(colour = TRTP)) +
    ggplot2::geom_vline(
      xintercept = cumsum(c(0, meta$proportion)), color = "grey80"
    ) +
    ggplot2::geom_vline(
      xintercept = zeroposition, color = "white", size = 1
    ) +
    ggplot2::geom_vline(
      xintercept = slope.meta$median, color = c("#F8766D", "#00BFC4"),
      linetype = "dashed", size = 0.3
    ) +
    ggplot2::geom_line(
      data = slopedata,
      aes(x = violinx, y = violiny, color = TRTP)
    ) +
    ggplot2::geom_line(
      data = survmod.data,
      aes(x = adjusted.time, y = km.start + km.y * 100, color = strata)
    ) +
    ggplot2::geom_violin(
      data = slopedata,
      aes(x = x, y = violiny, fill = factor(violiny)), alpha = 0.5
    ) +
    ggplot2::geom_boxplot(
      data = slopedata,
      aes(x = x, y = violiny, fill = factor(violiny)), alpha = 0.5, width = 2
    ) +
    ggplot2::xlab("Type of endpoint") +
    ggplot2::ylab("Cumulative proportion") +
    ggplot2::scale_x_continuous(
      limits = c(0, 100),
      breaks = c(meta$proportion / 2 + meta$startx),
      labels = ep.order,
      minor_breaks = to.rangeab(minorGrid)
    ) +
    ggplot2::annotate(
      geom = "text",
      x = to.rangeab(minorGrid),
      y = 0,
      label = minorGrid, color = "grey60"
    ) +
    ggplot2::annotate(
      geom = "label",
      x = 0,
      y = Inf,
      label = paste(
        "Win odds (95% CI): ", round(wo.result[1], 2),
        " (", round(wo.result[2], 2), ", ", round(wo.result[3], 2), ")", "\n",
        "p-value: ", format.pval(wo.result[4], digits = 3, eps = 0.001), sep=""
      ),
      hjust = 0, vjust = 1.4, size = 3
    ) +
    ggplot2::theme(
      axis.text.x.bottom = ggplot2::element_text(
        angle = c(rep(90, length(ep.order)-1), 0),
        vjust = c(rep(0.5, length(ep.order)-1), 0),
        hjust = c(rep(1, length(ep.order)-1), 0.5)
      ),
      axis.ticks.x.bottom = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.title.x.bottom =  ggplot2::element_blank()
    ) +
    ggplot2::guides(fill = FALSE)

}

###########################################################################################################################
###########################################################################################################################

