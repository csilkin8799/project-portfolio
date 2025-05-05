#### Load packages that have repeated use in this script ####

library(magrittr)
library(ggplot2)
library(grid)

#### Plot functions with the same specifications as minfi's ####

##### Pass/fail plot #####

#' A scatterplot displaying the LOG2 of the median unmethylated intensity on the x-axis and the LOG2 of the median methylated intensity on 
#' the y-axis.
#' 
#' The specifications of the plot are such that it will look exactly like the plot created by minfi's plotQC function, except using ggplot 
#' instead of base R plotting.
#' 
#' https://github.com/hansenlab/minfi/blob/devel/R/minfiQC.R
#' 
#' @param qc The object created after calling minfi's getQC function on a MethylSet.
#' @param badSampleCutoff The cutoff where if the average of the LOG2 median methylated intensity and the LOG2 median unmethylated 
#' intensity is below this value, the sample fails quality control.
#' Default = 10.5
#' @param qc_plot_title The title of the plot.
#' @return A modified version of minfi's plotQC output.
modified_QCpassfail_plot <- function(qc, badSampleCutoff = 10.5, qc_plot_title = "") {
  
  # Determine whether samples pass or fail quality control
  qc <- data.frame(qc)%>%
    ## Minfi's getQC function puts sample names as the row ID's... put them in their own column
    tibble::rownames_to_column(var = "Sample_Name")%>%
    ## Create a column with the row numbers
    tibble::rowid_to_column()%>%
    ## Calculate the average of the LOG2(median methylated intensity) and LOG2(median unmethylated intensity)
    dplyr::mutate(
      avg_log2_med = (mMed+uMed)/2
    )%>%
    ## Create column named "badSamp" with indication whether the average intensity is below the cutoff or not
    dplyr::mutate(
      badSamp = dplyr::case_when(
        avg_log2_med < badSampleCutoff ~ TRUE, # If the average intensity is less than the cutoff, then the sample is a bad sample
        avg_log2_med >= badSampleCutoff ~ FALSE # Else, the sample is not a bad sample
      )
    )
  
  # Create ggplot
  qc%>%
    ## Establish x-axis and y-axis, samples will have different colors depending on if they pass or fail QC 
    ggplot(aes(x = mMed, y = uMed, col = badSamp)) +
    ## Plot points, with shape as empty circle with a border
    geom_point(shape = 1) +
    ## Use geom_text() to annotate just the samples that fail QC with the row ID's, replacing the circles initially created
    geom_text(
      data = subset(qc, badSamp == TRUE), ### Subset to only include samples that fail QC
      aes(label = rowid), ### Labels = row ID's (same as what minfi does)
      show.legend = FALSE, ### Don't show the extra legend that appears, it is not relevant
      vjust = 1.5 ### Increase the vertical justification; this will push the text below the points
    ) +
    ## Create a dashed diagonal line along the line y = (cutoff * 2) - x
    geom_abline(
      intercept = badSampleCutoff * 2,
      slope = -1,
      linetype = "dashed"
    ) +
    ## Change the theme to be classic theme
    theme_classic() +
    ## Add labels
    labs(
      x = "Meth median intensity (log2)",
      y = "Unmeth median intensity (log2)",
      title = qc_plot_title,
      col = ""
    ) +
    ## Modify the scale of the x-axis
    scale_x_continuous(
      ### Establish increments of 2 within the range of the methylated and unmethylated intensities +/- 2 to give some wiggle room
      breaks = seq(
        from = min(floor(min(qc$mMed)), floor(min(qc$uMed))) - 2, 
        to = max(ceiling(max(qc$mMed)), ceiling(max(qc$uMed))) + 2, 
        by = 2
      ),
      ### Set the bounds of the x-axis to be the range of the methylated and unmethylated intensities +/- 2 to give some wiggle room
      limits = c(
        min(floor(min(qc$mMed)), floor(min(qc$uMed))) - 2, 
        max(ceiling(max(qc$mMed)), ceiling(max(qc$uMed))) + 2
      )
    ) +
    ## Modify the scale of the y-axis
    scale_y_continuous(
      ### Establish increments of 2 within the range of the methylated and unmethylated intensities +/- 2 to give some wiggle room
      breaks = seq(
        from = min(floor(min(qc$mMed)), floor(min(qc$uMed))) - 2, 
        to = max(ceiling(max(qc$mMed)), ceiling(max(qc$uMed))) + 2, 
        by = 2
      ),
      ### Set the bounds of the y-axis to be the range of the methylated and unmethylated intensities +/- 2 to give some wiggle room
      limits = c(
        min(floor(min(qc$mMed)), floor(min(qc$uMed))) - 2, 
        max(ceiling(max(qc$mMed)), ceiling(max(qc$uMed))) + 2
      )
    ) +
    ## Manually scale the colors
    scale_color_manual(
      ### Samples that pass QC have circles with black borders; samples that fail will have the row ID's with the font color as red
      values = c("FALSE" = "black", "TRUE" = "red"), 
      ### Add labels that will show up on the legend of the graph
      labels = c("FALSE" = "good", "TRUE" = "bad, with sample index")
    ) +
    ## Change thematic elements
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), ### Center title, make the font bigger and bold
    )
}

##### Sex at birth prediction plot #####

#' A scatterplot displaying the median of the LOG2(methylated + unmethylated intensity) at the X chromosome probes against the 
#' LOG2(methylated + unmethylated intensity) at the Y chromosome.
#' 
#' The specifications of the plot are such that it will look exactly like the plot created by minfi's plotQC function, except using ggplot 
#' instead of base R plotting.
#' 
#' https://github.com/hansenlab/minfi/blob/devel/R/getSex.R
#' 
#' @param getSex_out The object created after calling minfi's getSex function on a GenomicRatioSet.
#' @param plot_title The title of the plot.
#' @param id The labels of the samples that will be displayed on the plot.
#' @return A modified version of minfi's plotSex output.
modified_sexatbirthprediction_plot <- function(getSex_out, plot_title = "", id = NULL) {
  
  # Convert minfi's getSex() output to a data frame
  getSex_out <- data.frame(getSex_out)
  
  # Checks to make sure all of the necessary variables are in the function
  
  ## If at least one of "xMed", "yMed", or "predictedSex" is not a column name in the minfi::getSex() output, stop
  if (FALSE %in% (c("xMed", "yMed", "predictedSex") %in% colnames(getSex_out))) {
    stop("One of 'xMed', 'yMed', or 'predictedSex' must be a column name present.")
  }
  
  ## If the "id" parameter (probably sample names) isn't defined, make it the row number
  if (is.null(id)) {
    id <- seq_along(getSex_out$predictedSex)
  }
  
  ## If the length of the "id" parameter isn't the same as the number of predicted sex values, stop
  if (length(id) != length(getSex_out$predictedSex)) {
    stop("id length must match number of samples.")
  }
  
  # Create ggplot
  getSex_out%>%
    ## Create a column with the row numbers
    tibble::rowid_to_column()%>%
    ## Establish x-axis and y-axis, predicted males and females will have different colors
    ggplot(
      aes(x = xMed, y = yMed, col = predictedSex)
    ) +
    ## Tricks to just plot row ID numbers in place of shapes
    geom_point(alpha = 0) + ### Use geom_point(), setting the opacity to zero
    geom_text(aes(label = id), show.legend = FALSE) + ### Use geom_text() to annotate the plot with the row ID's
    ## Set the theme to classic
    theme_classic() +
    ## Add labels
    labs(
      x = "X chr, median total intensity (log2)",
      y = "Y chr, median total intensity (log2)",
      title = plot_title,
      subtitle = "Predicting Sex at Birth Using K-Means Clustering",
      caption = "K-means conducted via getSex() function from 'minfi' package.",
      col = ""
    ) +
    ## Edit thematic elements
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), ### Center title, adjust font size and make bold
      plot.subtitle = element_text(size = 10, hjust = 0.5, face = "bold"), ### Center title, adjust font size and make bold
      plot.caption = element_text(size = 7) ### Adjust size of caption
    ) +
    ## Manually scale the colors based on male/female
    scale_color_manual(
      values = c("M" = "deepskyblue", "F" = "deeppink3"), 
      labels = c("M" = "Male", "F" = "Female")
    ) +
    ## Manually create a legend
    guides(
      color = guide_legend(override.aes = list(shape = 16, alpha = 1, size = 3))
    )
}

##### Violin plots of methylation ratios #####

#' Violin plots which plot the methylation ratios of an approximate subset of all the probes for each sample.
#' 
#' Minfi's version: https://github.com/hansenlab/minfi/blob/devel/R/qc.R
#' 
#' @param plot_dat The processed DNA-methylation data to extract the betas from.
#' @param sampGroups The groups to categorize the samples, if applicable. Default = NULL.
#' @param sampNames The names of the samples, if applicable. Default = NULL.
#' @param plot_title The title of the plot. Default = NULL.
#' @param max_cols The maximum number of colors used on the plot based on the number of groups. Default = 8.
#' @param col_palette The color palette used on the plot based on the number of groups. Default = "Dark2".
#' @param seed A random seed used in the determination of which random probe indeces will be displayed in the plot. Default = 42.
#' @param numPositions The number of probes whose methylation ratios will be plotted. Default = 10000.
#' @return A ggplot containing violin plots of the methylation ratios of a subset of CpG sites/probes corresponding to each sample.
modified_density_violin_plot <- function(plot_dat, 
                                         sampGroups = NULL, sampNames = NULL,
                                         plot_title = NULL,
                                         max_cols = 8, col_palette = "Dark2",
                                         seed = 42,
                                         numPositions = 10000){
  
  # Obtain the methylation ratios to plot
  
  ## If the input is an RGChannelSet or a MethylSet, use minfi::getBeta() to obtain the methylation ratios (also called betas)
  if (class(plot_dat) == "RGChannelSet" | class(plot_dat) == "MethylSet") {
    b <- minfi::getBeta(plot_dat)
  }
  
  ## If the input is the methylation ratios themselves, assign them to a local variable
  else if (class(plot_dat) == "matrix" | class(plot_dat) == "DelayedMatrix") {
    b <- plot_dat
  }
  
  ## Otherwise, stop running the function
  else {
    stop(
      "argument 'dat' must be an 'RGChannelSet', a 'MethylSet', ",
      "a 'matrix', or a 'DelayedMatrix'."
    )
  }
  
  # Set the random seed
  set.seed(seed)
  
  # Create other variables if not established
  
  ## Number of samples
  n <- ncol(b)
  
  ## If no sample names provided, use the column names (convert to characters if they are the row ID's)
  if (!is.null(sampNames)) {
    base::colnames(b) <- as.character(sampNames)
  }
  
  ## If no plot title provided, use this one
  if (is.null(plot_title)) {
    plot_title <- "Violin Plots of Individual Sample Beta Values"
  }
  
  ## If no sample groups, assign all samples to one category and convert to factor
  if(is.null(sampGroups)) {
    sampGroups <- rep(1,n)
  }
  
  sampGroups <- as.factor(sampGroups)
  
  ## Create a data frame containing the sample groups assigned to each sample
  sample_info <- data.frame(
    sample = as.character(sampNames),
    group = sampGroups
  )
  
  ## Create an "idx" variable for the number of probes
  
  ### If there isn't a number specified, use the number of rows in the input dataset
  if (is.null(numPositions)) {
    idx <- 1:dim(plot_dat)[1]
  }
  
  ### Otherwise, randomly select "numPositions" rows from the betas.
  else {
    idx <- sample(nrow(b), numPositions)
  }
  
  # Subset the methylation ratios to only contain the row indeces in the "idx" variable
  b_subset <- as.matrix(b[idx,])
  
  # Transform the subsetted methylation ratios matrix such that all of the methylation ratios associated with a specific CpG site and sample
  # are in their own column.
  
  # EX: instead of  |     | Sample            |, the data will now be displayed as | CpG | Sample | methylation ratio
  #                | CpG | methylation_ratio |
  x <- reshape2::melt(b_subset, varnames = c("cpg", "sample"))
  
  # Create violin plot
  violin_plot <- data.frame(x)%>%
    ## Convert sample ID's to characters
    dplyr::mutate(sample = as.character(sample))%>%
    ## Add sample groups
    dplyr::left_join(
      sample_info, by = dplyr::join_by(sample)
    )%>%
    ## Establish x-axis and y-axis, different sample groups will have different colors
    ggplot(
      aes(x = value, y = sample, fill = group)
    ) +
    ## Create violin plot
    geom_violin(
      trim = TRUE,
      scale = "area",
      adjust = 1.5,
      color = NA,
      linewidth = 0.5
    ) +
    ## Create a vertical line corresponding to the mean methylation ratio of the probes for each sample
    stat_summary(
      fun = mean,
      geom = "crossbar",
      width = 0.7,
      linewidth = 0.3,
      aes(color = group)
    ) +
    ## Set the theme to black and white
    theme_bw() +
    ## Add labels
    labs(
      x = "Beta",
      title = plot_title
    ) +
    ## Edit thematic elements
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), ### Center title, make the font bigger and bold
      legend.position = "none", ### Remove legend
      panel.grid.major = element_blank(), ### Remove gridlines at labelled axis points
      panel.grid.minor = element_blank()  ### Remove gridlines between labelled axis points
    ) +
    ## Add dotted horizontal lines to place in between each sample (the original gridlines go through the violins)
    geom_hline(mapping=NULL, yintercept= seq(1, length(unique(x$sample))) - 0.5 ,colour='grey70', linetype = "dotted") +
    ## Scale the x-axis to be between 0 and 1, with labels defined at every 0.2
    scale_x_continuous(
      limits = c(0,1),
      breaks = seq(0,1,by=0.2)
    )
  
  # Color the violins (if less than the number of colors in the pre-defined palette)
  if(length(unique(sample_info$group)) <= max_cols) {
    
    ## Create a color palette object
    pal <- RColorBrewer::brewer.pal(max_cols, col_palette)
    
    ## Assign colors from the palette to each group based on the order of the categories
    violin_plot <- violin_plot +
      scale_fill_manual(
        values = setNames(pal, levels(sampGroups))
      ) +
      scale_color_manual(
        values = setNames(pal, levels(sampGroups))
      )
  }
  
  # Print the violin plot
  print(violin_plot)
}

##### Plots of the control probes #####

#' Plots of the LOG2 of red and green intensities at control probes.
#' 
#' This function also resolves the bug in minfi's function where green probes are colored red and red probes are colored green.
#' 
#' Minfi's version: https://github.com/hansenlab/minfi/blob/devel/R/qc.R
#' 
#' @param rgSet The RGChannelSet containing the processed red and green intensities from the iDAT files.
#' @param controls The specific control probes to plot. Default is all control probes.
#' @param sampNames What to name the samples.
#' @param xlim The range of the x-axis of the plot. Default is 5 to 17.
#' @return A ggplot of the LOG2 of the red and green intensities at the control probes for each sample.
modified_control_strip_plot <- function(
    rgSet, 
    controls = c("BISULFITE CONVERSION I",
                 "BISULFITE CONVERSION II", "EXTENSION",
                 "HYBRIDIZATION", "NON-POLYMORPHIC",
                 "SPECIFICITY I", "SPECIFICITY II",
                 "TARGET REMOVAL"),
    sampNames = NULL,
    xlim = c(5,17)){
  
  # If the rgSet input isn't an RGChannelSet, stop running the function
  minfi:::.isRGOrStop(rgSet)
  
  # Extract the red and green intensities
  redVals <- minfi::getRed(rgSet)
  greenVals <- minfi::getGreen(rgSet)
  
  # For each control probe type...
  for (controlType in controls) {
    
    ## Get the row numbers of the probes that are specific to the indicated control type
    ctrlAddress <- minfi::getControlAddress(rgSet, controlType = controlType)
    
    ### Extract red intensities
    ctlWide <- as.matrix(log2(redVals[ctrlAddress, , drop = FALSE]))
    
    ### If sample names present, change the column names from the sample ID's to the sample names
    if(!is.null(sampNames)) {
      colnames(ctlWide) = sampNames
    }
    
    ### Put the red values corresponding to each sample-control probe combo in one column
    ctlR <- reshape2::melt(ctlWide, varnames = c("address", "sample"))
    
    ### Green
    ctlWide <- as.matrix(log2(greenVals[ctrlAddress, , drop = FALSE]))
    
    ### If sample names present, change the column names from the sample ID's to the sample names
    if(!is.null(sampNames)) {
      colnames(ctlWide) = sampNames
    }
    
    ### Put the green values corresponding to each sample-control probe combo in one column
    ctlG <- reshape2::melt(ctlWide, varnames = c("address", "sample"))
    
    ## Create plot
    ctl <- ctlR%>%
      dplyr::mutate(
        channel = "Red", ### Starting with red values, indicate that these are red probes
        sample = as.character(sample) ### Make sure sample names are characters
      )%>%
      ### Combine red values and green values into one data frame
      dplyr::bind_rows(
        ctlG%>%
          dplyr::mutate(
            channel = "Green", ### Indicate that these probes are green probes
            sample = as.character(sample) ### Make sure sample names are characters
          )
      )
    
    ## If there are any red/green intensities outside the plot range, print a warning
    if (any((ctl$value < xlim[1]) | (ctl$value > xlim[2]))) {
      message("Warning: ", controlType, " probes outside plot range")
    }
    
    ## Create ggplot
    ctrlPlot <- ctl%>%
      ### Establish x-axis and y-axis, different sample groups will have different colors
      ggplot(
        aes(x = value, y = sample, col = channel)
      ) +
      ### Display points
      geom_point(shape = 18, size = 2.5) +
      ### Separate red and green values on the plot
      facet_wrap(~channel) +
      ### Set theme to black and white
      theme_bw() +
      ### Add labels
      labs(
        x = "Log2 Intensity",
        title = paste("Control:", controlType)
      ) +
      ### Edit thematic elements
      theme(
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), #### Center title, make the font bigger and bold
        legend.position = "none", #### Remove legend
        panel.grid.major = element_blank(), #### Remove gridlines at labelled axis points
        panel.grid.minor = element_blank()  #### Remove gridlines between labelled axis points
      ) +
      ### Add dotted horizontal lines to place in between each sample (the original gridlines go through the points)
      geom_hline(mapping=NULL, yintercept= seq(1, length(unique(ctl$sample))) - 0.5 ,colour='grey70', linetype = "dotted") +
      ### Assign colors to green and red manually
      scale_color_manual(
        values = c("Green" = "darkgreen", "Red" = "darkred")
      ) +
      ### Set the range of the plot, with labels set at increments of 2
      scale_x_continuous(
        limits = c(xlim[1], xlim[2]),
        breaks = seq(xlim[1] + xlim[1]%%2, xlim[2] - xlim[2]%%2, by = 2)
      )
    
    ## Print the plot
    print(ctrlPlot)
  }
}

#### Quality control plots modified to meet the specifications used in the HRS report ####

##### Pass/fail plot #####

#' A scatterplot displaying the LOG2 of the median unmethylated intensity on the x-axis and the LOG2 of the median methylated intensity on 
#' the y-axis.
#' 
#' This function enhance's minfi's by adding differentiation between control probes and sample probes (and also making the plot look nicer)
#' 
#' Inspiration plot on page 6 of HRS report: https://hrs.isr.umich.edu/sites/default/files/genetic/HRS_DNAm_OCT2023.pdf
#' 
#' @param qc The object created after calling minfi's getQC function on a MethylSet.
#' @param badSampleCutoff The cutoff where if the average of the LOG2 median methylated intensity and the LOG2 median unmethylated 
#' intensity is below this value, the sample fails quality control.
#' Default = 10.5
#' @param qc_plot_title The title of the plot.
#' @return A modified version of minfi's plotQC output with approximate specifications of the plot used in the HRS report.
QC_passfailplot_HRS_version <- function(qc, badSampleCutoff = 10.5, qc_plot_title = "") {
  
  # Determine whether samples pass or fail quality control
  qc <- data.frame(qc)%>%
    ## Minfi's getQC function puts sample names as the row ID's... put them in their own column
    tibble::rownames_to_column(var = "Sample_Name")%>%
    ## Create a column with the row numbers
    tibble::rowid_to_column()%>%
    ## Calculate the average of the LOG2(median methylated intensity) and LOG2(median unmethylated intensity)
    dplyr::mutate(
      avg_log2_med = (mMed+uMed)/2
    )%>%
    ## Create column named "badSamp" with indication whether the average intensity is below the cutoff or not
    dplyr::mutate(
      badSamp = dplyr::case_when(
        avg_log2_med < badSampleCutoff ~ TRUE, # If the average intensity is less than the cutoff, then the sample is a bad sample
        avg_log2_med >= badSampleCutoff ~ FALSE # Else, the sample is not a bad sample
      )
    )%>%
    ## Distinguish between control samples and actual samples based on if there is "NA" in the sample name (i.e. "NA_10858")
    dplyr::mutate(
      Sample_Type = dplyr::case_when(
        stringr::str_detect(Sample_Name, "NA") ~ "Control",
        TRUE ~ "Sample"
      )
    )%>%
    ## Create a factor based on the combination of whether a sample passes or fails QC and whether the sample is a control sample or not.
    dplyr::mutate(
      Sample_Qual_and_Type = dplyr::case_when(
        badSamp == FALSE & Sample_Type == "Control" ~ 1, ### Control samples that pass QC
        badSamp == FALSE & Sample_Type == "Sample" ~ 2, ### Primary samples that pass QC
        badSamp == TRUE & Sample_Type == "Control" ~ 3, ### Control samples that fail QC
        badSamp == TRUE & Sample_Type == "Sample" ~ 4 ### Primary samples that fail QC
      )
    )%>%
    dplyr::mutate(
      Sample_Qual_and_Type = factor(Sample_Qual_and_Type, levels = c(1,2,3,4))
    )
  
  # Create ggplot
  qc%>%
    ## Establish x-axis and y-axis
    ## Samples will have different colors and shapes based on the combination of quality and type of the sample
    ## Different opacities depending on whether a sample passes or fails QC
    ggplot(aes(x = mMed, y = uMed, col = Sample_Qual_and_Type, shape = Sample_Qual_and_Type, alpha = badSamp)) +
    ## Create points
    geom_point(size = 2) +
    ## Create a dashed diagonal line along the line y = (cutoff * 2) - x
    geom_abline(
      intercept = badSampleCutoff * 2,
      slope = -1,
      linetype = "dashed"
    ) +
    ## Change theme to black and white theme
    theme_bw() +
    ## Add labels
    labs(
      x = "Meth median intensity (log2)",
      y = "Unmeth median intensity (log2)",
      title = qc_plot_title,
      col = ""
    ) +
    ## Modify the scale of the x-axis
    scale_x_continuous(
      ### Establish increments of 2 within the range of the methylated and unmethylated intensities +/- 2 to give some wiggle room
      breaks = seq(
        from = min(floor(min(qc$mMed)), floor(min(qc$uMed))) - 2, 
        to = max(ceiling(max(qc$mMed)), ceiling(max(qc$uMed))) + 2, 
        by = 2
      ),
      ### Set the bounds of the x-axis to be the range of the methylated and unmethylated intensities +/- 2 to give some wiggle room
      limits = c(
        min(floor(min(qc$mMed)), floor(min(qc$uMed))) - 2, 
        max(ceiling(max(qc$mMed)), ceiling(max(qc$uMed))) + 2
      )
    ) +
    ## Modify the scale of the y-axis
    scale_y_continuous(
      ### Establish increments of 2 within the range of the methylated and unmethylated intensities +/- 2 to give some wiggle room
      breaks = seq(
        from = min(floor(min(qc$mMed)), floor(min(qc$uMed))) - 2, 
        to = max(ceiling(max(qc$mMed)), ceiling(max(qc$uMed))) + 2, 
        by = 2
      ),
      ### Set the bounds of the y-axis to be the range of the methylated and unmethylated intensities +/- 2 to give some wiggle room
      limits = c(
        min(floor(min(qc$mMed)), floor(min(qc$uMed))) - 2, 
        max(ceiling(max(qc$mMed)), ceiling(max(qc$uMed))) + 2
      )
    ) +
    ## Manually assign colors
    scale_color_manual(
      name = "Median Intensity Filter Type",
      labels = c("1" = "(PASS, Control)", "2" = "(PASS, Sample)", "3" = "(LOW QUALITY, Control)", "4" = "(LOW QUALITY, Sample)"),
      values = c("1" = "steelblue1", "2" = "steelblue1", "3" = "lightgoldenrod2", "4" = "lightgoldenrod2")
    ) +
    ## Manually assign shapes
    scale_shape_manual(
      name = "Median Intensity Filter Type",
      labels = c("1" = "(PASS, Control)", "2" = "(PASS, Sample)", "3" = "(LOW QUALITY, Control)", "4" = "(LOW QUALITY, Sample)"),
      values = c("1" = 16, "2" = 17, "3" = 16, "4" = 17)
    ) +
    ## Reduce opacity of samples that pass QC
    scale_alpha_manual(
      values = c("FALSE" = 0.5, "TRUE" = 1)
    ) +
    ## Edit thematic elements
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), ### Center title, make the font bigger and bold
      legend.justification = c(0,1) ### Move the legend to be aligned with the top of the graph instead of the center
    ) +
    ## Remove legend indicating that samples that pass QC have different opacity value
    guides(
      alpha = "none"
    )
}

##### Sex at birth prediction plot #####

#' A scatterplot displaying the median of the LOG2(methylated + unmethylated intensity) at the X chromosome probes against the median
#' LOG2(methylated + unmethylated intensity) at the Y chromosome.
#' 
#' This plot enhances minfi's by indicating whether actual sex is different from predicted sex, if available.
#' 
#' Inspiration plot on page 5 of HRS report: https://hrs.isr.umich.edu/sites/default/files/genetic/HRS_DNAm_OCT2023.pdf
#' 
#' @param gSet A GenomicRatioSet containing the chromosome information for each probe.
#' @param plot_title The title of the plot.
#' @param cutoff The threshold of median LOG2 at Y-chromosome probes minus median LOG2 of X-chromosome probes.
#' @param actual_sex A vector containing the actual sex at birth values, if available.
#' @return A modified version of minfi's plotSex output with approximate specifications of the plot used in the HRS report.
sex_at_birth_plot_HRS_version <- function(gSet, plot_title = "", cutoff = -2, actual_sex = NULL) {
  
  # Stop running the function if the input object isn't a GenomicRatioSet
  minfi:::.isGenomicOrStop(gSet)
  
  # If at least one of "xMed", "yMed", or "predictedSex" is not a column name in the minfi::getSex() output, add them
  if (FALSE %in% (c("xMed", "yMed", "predictedSex") %in% colnames(gSet@colData))) {
    gSet <- minfi::addSex(
      gSet,
      sex = minfi::getSex(gSet, cutoff = cutoff)
    )
  }
  
  # Create a dataframe with the median LOG2 intensities at x-chromosome probes, y-chromosome probes, and predicted sex values
  getSex_out <- data.frame(
    xMed = gSet$xMed,
    yMed = gSet$yMed,
    predictedSex = gSet$predictedSex
  )
  
  # If there isn't an "actual sex" to compare to, just output my modified version of the standard minfi sex plot function
  if (is.null(actual_sex)) {
    modified_sex_at_birth_plot(getSex_out, plot_title)
  }
  
  # Add actual sex info to the getSex() output
  getSex_out <- getSex_out%>%
    ## Add actual sex
    dplyr::mutate(actSex = actual_sex)%>%
    ## Create boolean whether actual sex and predicted sex are the same
    dplyr::mutate(
      act_eq_pred = predictedSex == actSex
    )%>%
    ## Create factor based on whether actual sex and predicted sex are the same and female/male.
    dplyr::mutate(
      Sex_ID_and_Match = dplyr::case_when(
        predictedSex == actSex & actSex == "F" ~ 1, ### Predicted sex = Female, actual sex = female
        predictedSex == actSex & actSex == "M" ~ 2, ### Predicted sex = Male, actual sex = male
        predictedSex == "F" & actSex == "M" ~ 3, ### Predicted sex = Female, actual sex = male
        predictedSex == "M" & actSex == "F" ~ 4, ### Predicted sex = Male, actual sex = female
      )
    )%>%
    dplyr::mutate(
      Sex_ID_and_Match = factor(Sex_ID_and_Match)
    )
  
  # Create ggplot
  getSex_out%>%
    ggplot() +
    ## Step 1: Plot only samples where actual sex is the same as predicted sex
    geom_point(
      aes(x = xMed, y = yMed, col = Sex_ID_and_Match, shape = Sex_ID_and_Match),
      data = ~subset(., Sex_ID_and_Match %in% c(1,2)),
      size = 2,
      alpha = 0.5
    ) +
    ## Step 2: Plot only samples where actual sex is NOT the same as predicted sex
    geom_point(
      aes(x = xMed, y = yMed, col = Sex_ID_and_Match, shape = Sex_ID_and_Match),
      data = ~subset(., Sex_ID_and_Match %in% c(3,4)),
      size = 2,
      alpha = 0.5
    ) +
    ## Create dashed y=x line with the y-intercept at the cutoff
    geom_abline(
      intercept = cutoff,
      slope = 1,
      linetype = "dashed"
    ) +
    ## Change theme to black and white
    theme_bw() +
    ## Add labels
    labs(
      x = "X chr, median total intensity (log2)",
      y = "Y chr, median total intensity (log2)",
      title = plot_title,
      subtitle = "Predicting Sex at Birth Using K-Means Clustering",
      caption = "K-means conducted via getSex() function from 'minfi' package.",
      col = ""
    ) +
    ## Edit thematic elements
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), ### Center title, adjust font size and make bold
      plot.subtitle = element_text(size = 10, hjust = 0.5, face = "bold"), ### Center title, adjust font size and make bold
      plot.caption = element_text(size = 7), ### Adjust size of caption
      legend.justification = c(0,1) ### Move the legend to be aligned with the top of the graph instead of the center
    ) +
    ## Manually assign colors
    scale_color_manual(
      name = "Predicted Mismatch",
      labels = c("1" = "(Correct, F)", "2" = "(Correct, M)", "3" = "(Mismatch, F)", "4" = "(Mismatch, M)"),
      values = c("1" = "#949494", "2" = "#949494", "3" = "red", "4" = "red")
    ) +
    ## Manually assign shapes
    scale_shape_manual(
      name = "Predicted Mismatch",
      labels = c("1" = "(Correct, F)", "2" = "(Correct, M)", "3" = "(Mismatch, F)", "4" = "(Mismatch, M)"),
      values = c("1" = 16, "2" = 17, "3" = 16, "4" = 17)
    )
}

#### Consolidated report ####

#' Combine the HRS version of the QC pass/fail plot, the HRS version of the sex at birth prediction plot, the modified violin plot,
#' and the modified version of the control probe plots into one PDF
#' 
#' @param rgSet The RGChannelSet containing the processed red and green intensities from the iDAT files.
#' @param MSet The MethylSet containing the methylated and unmethylated intensities, if created previously.
#' @param gSet The GenomicRatioSet containing the genomic coordinates of each probe, if created previously.
#' @param qcPlotTitle The title of the pass/fail plot.
#' @param SexAtBirthPlotTitle The title of the sex at birth prediction plot.
#' @param DensityPlotTitle The title of the density plot (no need to modify minfi's default).
#' @param sampGroups The groups to categorize the samples, if applicable. Default = NULL.
#' @param sampNames The names of the samples, if applicable. Default = NULL.
#' @param actual_sex A vector containing the actual sex at birth values, if available.
#' @param pdf_filename The name of the PDF file.
#' @param page_dims The width x height of the pages of the PDF file. Default = 8x11.
#' @param maxSamplesperPage The number of samples per page (for the violin and control plots). Default = 24.
#' @param controls The specific control probes to plot. Default is all control probes.
#' @return PDF containing the QC plots.
new_DNAm_qc_report <- function(rgSet, MSet = NULL, gSet = NULL,
                               qcPlotTitle = "", SexAtBirthPlotTitle = "", DensityPlotTitle = "Density Plot of Beta Values",
                               sampGroups = NULL, sampNames = NULL, actual_sex = NULL,
                               pdf_filename = "detailed_QC_report.pdf",
                               page_dims = c(8,11),
                               maxSamplesperPage = 24,
                               controls = c(
                                 "BISULFITE CONVERSION I",
                                 "BISULFITE CONVERSION II", "EXTENSION",
                                 "HYBRIDIZATION", "NON-POLYMORPHIC",
                                 "SPECIFICITY I", "SPECIFICITY II",
                                 "TARGET REMOVAL"
                               )){
  
  # If the input dataset isn't an RGChannelSet, stop the function.
  minfi:::.isRGOrStop(rgSet)
  
  # Create MethylSet and GenomicRatioSet if not previously created.
  if (minfi:::.isMethylOrRatio(MSet) == FALSE) {
    MSet <- minfi::preprocessRaw(rgSet)
  }
  
  if (minfi:::.isMethylOrRatio(gSet) == FALSE) {
    gSet <- minfi::mapToGenome(MSet)
  }
  
  # Create variable with the number of columns in the rgSet
  n <- ncol(rgSet)
  
  # If no sample names present, use the column names of the rgSet
  if(is.null(sampNames)) {
    sampNames <- colnames(rgSet)
  }
  
  # If no sample groups present, establish only one category and convert to factor
  if(is.null(sampGroups)) {
    sampGroups <- rep(1,n)
  }
  
  sampGroups <- as.factor(sampGroups)
  
  # Define number of pages and number of samples per page.
  numPages <- ceiling(n/maxSamplesperPage)
  samplesPerPage <- ceiling(n/numPages)
  
  sampleIdxs <- suppressWarnings(
    split(seq_len(n), rep(seq_len(numPages), each = samplesPerPage))
  )
  
  # Define dimensions of the pdf
  pdf(file = pdf_filename, width = page_dims[1], height = page_dims[2])
  
  # Print pass/fail plot
  print(
    ## Create a grid, putting the pass/fail plot in between two rectangles
    cowplot::plot_grid(
      rectGrob(gp = gpar(fill = NA, col = NA)),
      QC_passfailplot_HRS_version(minfi::getQC(MSet), qc_plot_title = qcPlotTitle),
      rectGrob(gp = gpar(fill = NA, col = NA)),
      nrow = 3, rel_heights = c(0.02, 0.49, 0.49)
    )
  )
  
  # On a new page, print thesex at birth plot
  print(
    ## Create a grid, putting the sex at birth prediction plot in between two rectangles
    cowplot::plot_grid(
      rectGrob(gp = gpar(fill = NA, col = NA)),
      sex_at_birth_plot_HRS_version(gSet, plot_title = SexAtBirthPlotTitle, actual_sex = actual_sex),
      rectGrob(gp = gpar(fill = NA, col = NA)),
      nrow = 3, rel_heights = c(0.02, 0.49, 0.49)
    )
  )
  
  # On a new page, print minfi's version of a density plot which plots the methylation ratios of all the samples on one plot.
  par(mfrow = c(2,1))
  
  minfi::densityPlot(rgSet, sampGroups = sampGroups, plot_title = DensityPlotTitle, xlab = "Beta")
  
  # Print the violin plots and control probe plots
  plot.new()
  par(mfrow = c(1, 1), oma = c(2, 10, 1, 1))
  
  for (sampleIdx in sampleIdxs) {
    modified_density_violin_plot(rgSet[,sampleIdx], sampGroups = sampGroups[sampleIdx], sampNames = sampNames[sampleIdx])
  }
  
  for (controlType in controls) {
    for (sampleIdx in sampleIdxs) {
      modified_control_strip_plot(rgSet[,sampleIdx], sampNames = sampNames[sampleIdx], controls = controlType)
    }
  }
  
  # Close the pdf
  dev.off()
}