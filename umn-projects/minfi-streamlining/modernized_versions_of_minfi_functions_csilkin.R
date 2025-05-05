library(magrittr)
library(ggplot2)
library(grid)

# GitHub for minfi's plotQC() function: https://github.com/hansenlab/minfi/blob/devel/R/minfiQC.R

mod_QC_passfailplot <- function(qc, badSampleCutoff = 10.5, qc_plot_title = "") {
  qc <- data.frame(qc)%>%
    tibble::rownames_to_column(var = "Sample_Name")%>%
    tibble::rowid_to_column()%>%
    dplyr::mutate(
      avg_log2_med = (mMed+uMed)/2
    )%>%
    dplyr::mutate(
      badSamp = dplyr::case_when(
        avg_log2_med < badSampleCutoff ~ TRUE,
        avg_log2_med >= badSampleCutoff ~ FALSE
      )
    )
  
  qc%>%
    ggplot(aes(x = mMed, y = uMed, col = badSamp)) +
    geom_point(shape = 1) +
    geom_text(
      data = subset(qc, badSamp == TRUE),
      aes(label = rowid),
      show.legend = FALSE,
      vjust = 1.5
    ) +
    geom_abline(
      intercept = badSampleCutoff * 2,
      slope = -1,
      linetype = "dashed"
    ) +
    theme_classic() +
    labs(
      x = "Meth median intensity (log2)",
      y = "Unmeth median intensity (log2)",
      title = qc_plot_title,
      col = ""
    ) +
    scale_x_continuous(
      breaks = seq(
        from = min(floor(min(qc$mMed)), floor(min(qc$uMed))) - 2, 
        to = max(ceiling(max(qc$mMed)), ceiling(max(qc$uMed))) + 2, 
        by = 2
      ),
      limits = c(
        min(floor(min(qc$mMed)), floor(min(qc$uMed))) - 2, 
        max(ceiling(max(qc$mMed)), ceiling(max(qc$uMed))) + 2
      )
    ) +
    scale_y_continuous(
      breaks = seq(
        from = min(floor(min(qc$mMed)), floor(min(qc$uMed))) - 2, 
        to = max(ceiling(max(qc$mMed)), ceiling(max(qc$uMed))) + 2, 
        by = 2
      ),
      limits = c(
        min(floor(min(qc$mMed)), floor(min(qc$uMed))) - 2, 
        max(ceiling(max(qc$mMed)), ceiling(max(qc$uMed))) + 2
      )
    ) +
    scale_color_manual(
      values = c("FALSE" = "black", "TRUE" = "red"), 
      labels = c("FALSE" = "good", "TRUE" = "bad, with sample index")
    ) +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), # Center title, make the font bigger and bold
    )
}

QC_passfailplot_HRS_version <- function(qc, badSampleCutoff = 10.5, qc_plot_title = "") {
  qc <- data.frame(qc)%>%
    tibble::rownames_to_column(var = "Sample_Name")%>%
    tibble::rowid_to_column()%>%
    dplyr::mutate(
      avg_log2_med = (mMed+uMed)/2
    )%>%
    dplyr::mutate(
      badSamp = dplyr::case_when(
        avg_log2_med < badSampleCutoff ~ TRUE,
        avg_log2_med >= badSampleCutoff ~ FALSE
      )
    )%>%
    dplyr::mutate(
      Sample_Type = dplyr::case_when(
        stringr::str_detect(Sample_Name, "NA") ~ "Control",
        TRUE ~ "Sample"
      )
    )%>%
    dplyr::mutate(
      Sample_Qual_and_Type = dplyr::case_when(
        badSamp == FALSE & Sample_Type == "Control" ~ 1,
        badSamp == FALSE & Sample_Type == "Sample" ~ 2,
        badSamp == TRUE & Sample_Type == "Control" ~ 3,
        badSamp == TRUE & Sample_Type == "Sample" ~ 4
      )
    )%>%
    dplyr::mutate(
      Sample_Qual_and_Type = factor(Sample_Qual_and_Type, levels = c(1,2,3,4))
    )
  
  qc%>%
    ggplot(aes(x = mMed, y = uMed, col = Sample_Qual_and_Type, shape = Sample_Qual_and_Type, alpha = badSamp)) +
    geom_point(size = 2) +
    geom_abline(
      intercept = badSampleCutoff * 2,
      slope = -1,
      linetype = "dashed"
    ) +
    theme_bw() +
    labs(
      x = "Meth median intensity (log2)",
      y = "Unmeth median intensity (log2)",
      title = qc_plot_title,
      col = ""
    ) +
    scale_x_continuous(
      breaks = seq(
        from = min(floor(min(qc$mMed)), floor(min(qc$uMed))) - 2, 
        to = max(ceiling(max(qc$mMed)), ceiling(max(qc$uMed))) + 2, 
        by = 2
      ),
      limits = c(
        min(floor(min(qc$mMed)), floor(min(qc$uMed))) - 2, 
        max(ceiling(max(qc$mMed)), ceiling(max(qc$uMed))) + 2
      )
    ) +
    scale_y_continuous(
      breaks = seq(
        from = min(floor(min(qc$mMed)), floor(min(qc$uMed))) - 2, 
        to = max(ceiling(max(qc$mMed)), ceiling(max(qc$uMed))) + 2, 
        by = 2
      ),
      limits = c(
        min(floor(min(qc$mMed)), floor(min(qc$uMed))) - 2, 
        max(ceiling(max(qc$mMed)), ceiling(max(qc$uMed))) + 2
      )
    ) +
    scale_color_manual(
      name = "Median Intensity Filter Type",
      labels = c("1" = "(PASS, Control)", "2" = "(PASS, Sample)", "3" = "(LOW QUALITY, Control)", "4" = "(LOW QUALITY, Sample)"),
      values = c("1" = "steelblue1", "2" = "steelblue1", "3" = "lightgoldenrod2", "4" = "lightgoldenrod2")
    ) +
    scale_shape_manual(
      name = "Median Intensity Filter Type",
      labels = c("1" = "(PASS, Control)", "2" = "(PASS, Sample)", "3" = "(LOW QUALITY, Control)", "4" = "(LOW QUALITY, Sample)"),
      values = c("1" = 16, "2" = 17, "3" = 16, "4" = 17)
    ) +
    scale_alpha_manual(
      values = c("FALSE" = 0.5, "TRUE" = 1)
    ) +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), # Center title, make the font bigger and bold
      legend.justification = c(0,1)
    ) +
    guides(
      alpha = "none"
    )
}

# minfi version of plotSex function: https://github.com/hansenlab/minfi/blob/devel/R/getSex.R

modified_sex_at_birth_plot <- function(getSex_out, main = "", id = NULL) {
  
  getSex_out <- data.frame(getSex_out)
  
  # If at least one of "xMed", "yMed", or "predictedSex" is not a column name in the minfi::getSex() output, stop
  if (FALSE %in% (c("xMed", "yMed", "predictedSex") %in% colnames(getSex_out))) {
    stop("One of 'xMed', 'yMed', or 'predictedSex' must be a column name present.")
  }
  
  # If the "id" parameter (probably sample names) isn't defined, make it the row number
  if (is.null(id)) {
    id <- seq_along(getSex_out$predictedSex)
  }
  
  # If the length of the "id" parameter isn't the same as the number of predicted sex values, stop
  if (length(id) != length(getSex_out$predictedSex)) {
    stop("id length must match number of samples.")
  }
  
  getSex_out%>%
    tibble::rowid_to_column()%>%
    ggplot(
      aes(x = xMed, y = yMed, col = predictedSex)
    ) +
    geom_point(alpha = 0) +
    geom_text(aes(label = id), show.legend = FALSE) +
    theme_classic() +
    labs(
      x = "X chr, median total intensity (log2)",
      y = "Y chr, median total intensity (log2)",
      title = main,
      subtitle = "Predicting Sex at Birth Using K-Means Clustering",
      caption = "K-means conducted via getSex() function from 'minfi' package.",
      col = ""
    ) +
    ## Edit theme
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), # Center title, adjust font size and make bold
      plot.subtitle = element_text(size = 10, hjust = 0.5, face = "bold"), # Center title, adjust font size and make bold
      plot.caption = element_text(size = 7) # Adjust size of caption
    ) +
    scale_color_manual(
      values = c("M" = "deepskyblue", "F" = "deeppink3"), 
      labels = c("M" = "Male", "F" = "Female")
    ) +
    guides(
      color = guide_legend(override.aes = list(shape = 16, alpha = 1, size = 3))
    )
}

sex_at_birth_plot_HRS_version <- function(gSet, main = "", cutoff = -2, actual_sex = NULL) {
  
  minfi:::.isGenomicOrStop(gSet)
  
  # If at least one of "xMed", "yMed", or "predictedSex" is not a column name in the minfi::getSex() output, add them
  if (FALSE %in% (c("xMed", "yMed", "predictedSex") %in% colnames(gSet@colData))) {
    gSet <- minfi::addSex(
      gSet,
      sex = minfi::getSex(gSet, cutoff = cutoff)
    )
  }
  
  getSex_out <- data.frame(
    xMed = gSet$xMed,
    yMed = gSet$yMed,
    predictedSex = gSet$predictedSex
  )
  
  # If there isn't an "actual sex" to compare to, just do the standard minfi sex plot function
  if (is.null(actual_sex)) {
    modified_sex_at_birth_plot(getSex_out, main)
  }
  
  getSex_out <- getSex_out%>%
    dplyr::mutate(actSex = actual_sex)%>%
    dplyr::mutate(
      act_eq_pred = predictedSex == actSex
    )%>%
    dplyr::mutate(
      Sex_ID_and_Match = dplyr::case_when(
        predictedSex == actSex & actSex == "F" ~ 1,
        predictedSex == actSex & actSex == "M" ~ 2,
        predictedSex == "F" & actSex == "M" ~ 3,
        predictedSex == "M" & actSex == "F" ~ 4,
      )
    )%>%
    dplyr::mutate(
      Sex_ID_and_Match = factor(Sex_ID_and_Match)
    )

  getSex_out%>%
    ggplot() +
    geom_point(
      aes(x = xMed, y = yMed, col = Sex_ID_and_Match, shape = Sex_ID_and_Match),
      data = ~subset(., Sex_ID_and_Match %in% c(1,2)),
      size = 2,
      alpha = 0.5
    ) +
    geom_point(
      aes(x = xMed, y = yMed, col = Sex_ID_and_Match, shape = Sex_ID_and_Match),
      data = ~subset(., Sex_ID_and_Match %in% c(3,4)),
      size = 2,
      alpha = 0.5
    ) +
    geom_abline(
      intercept = -2,
      slope = 1,
      linetype = "dashed"
    ) +
    theme_bw() +
    labs(
      x = "X chr, median total intensity (log2)",
      y = "Y chr, median total intensity (log2)",
      title = "main",
      subtitle = "Predicting Sex at Birth Using K-Means Clustering",
      caption = "K-means conducted via getSex() function from 'minfi' package.",
      col = ""
    ) +
    ## Edit theme
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), # Center title, adjust font size and make bold
      plot.subtitle = element_text(size = 10, hjust = 0.5, face = "bold"), # Center title, adjust font size and make bold
      plot.caption = element_text(size = 7) # Adjust size of caption
    ) +
    scale_color_manual(
      name = "Predicted Mismatch",
      labels = c("1" = "(Correct, F)", "2" = "(Correct, M)", "3" = "(Mismatch, F)", "4" = "(Mismatch, M)"),
      values = c("1" = "#949494", "2" = "#949494", "3" = "red", "4" = "red")
    ) +
    scale_shape_manual(
      name = "Predicted Mismatch",
      labels = c("1" = "(Correct, F)", "2" = "(Correct, M)", "3" = "(Mismatch, F)", "4" = "(Mismatch, M)"),
      values = c("1" = 16, "2" = 17, "3" = 16, "4" = 17)
    ) +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), # Center title, make the font bigger and bold
      legend.justification = c(0,1)
    )
}

# Minfi's version of controlStripPlot(), densityBeanPlot(), and qcReport(): https://github.com/hansenlab/minfi/blob/devel/R/qc.R

modified_density_violin_plot <- function(plot_dat, 
                                         sampGroups = NULL, sampNames = NULL,
                                         main = NULL,
                                         max_cols = 8, col_palette = "Dark2",
                                         seed = 42,
                                         numPositions = 10000){
  
  if (class(plot_dat) == "RGChannelSet" | class(plot_dat) == "MethylSet") {
    b <- minfi::getBeta(plot_dat)
  }
  
  else if (class(plot_dat) == "matrix" | class(plot_dat) == "DelayedMatrix") {
    b <- plot_dat
  }
  
  else {
    stop(
      "argument 'dat' must be an 'RGChannelSet', a 'MethylSet', ",
      "a 'matrix', or a 'DelayedMatrix'."
    )
  }
  
  set.seed(seed)
  
  n <- ncol(b)
  
  if (!is.null(sampNames)) {
    base::colnames(b) <- as.character(sampNames)
  }
  
  if (is.null(main)) {
    main <- "Violin Plots of Individual Sample Beta Values"
  }
  
  if(is.null(sampGroups)) {
    sampGroups <- rep(1,n)
  }
  
  sampGroups <- as.factor(sampGroups)
  
  sample_info <- data.frame(
    sample = as.character(sampNames),
    group = sampGroups
  )
  
  if (is.null(numPositions)) {
    idx <- 1:dim(plot_dat)[1]
  } 
  
  else {
    idx <- sample(nrow(b), numPositions)
  }
  
  b_subset <- as.matrix(b[idx,])
  x <- reshape2::melt(b_subset, varnames = c("cpg", "sample"))
  
  violin_plot <- data.frame(x)%>%
    dplyr::mutate(sample = as.character(sample))%>%
    dplyr::left_join(
      sample_info, by = dplyr::join_by(sample)
    )%>%
    ggplot(
      aes(x = value, y = sample, fill = group)
    ) +
    geom_violin(
      trim = TRUE,
      scale = "area",
      adjust = 1.5,
      color = NA,
      linewidth = 0.5
    ) +
    stat_summary(
      fun = mean,
      geom = "crossbar",
      width = 0.7,
      linewidth = 0.3,
      aes(color = group)
    ) +
    theme_bw() +
    labs(
      x = "Beta",
      title = main
    ) +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), # Center title, make the font bigger and bold
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank() 
    ) +
    geom_hline(mapping=NULL, yintercept= seq(1, length(unique(x$sample))) - 0.5 ,colour='grey70', linetype = "dotted") +
    scale_x_continuous(
      limits = c(0,1),
      breaks = seq(0,1,by=0.2)
    )
  
  if(length(unique(sample_info$group)) <= max_cols) {
    
    pal <- RColorBrewer::brewer.pal(max_cols, col_palette)
    
    violin_plot <- violin_plot +
      scale_fill_manual(
        values = setNames(pal, levels(sampGroups))
      ) +
      scale_color_manual(
        values = setNames(pal, levels(sampGroups))
      )
  }
  
  print(violin_plot)
}

modified_control_strip_plot <- function(
    rgSet, 
    controls = c("BISULFITE CONVERSION I",
                 "BISULFITE CONVERSION II", "EXTENSION",
                 "HYBRIDIZATION", "NON-POLYMORPHIC",
                 "SPECIFICITY I", "SPECIFICITY II",
                 "TARGET REMOVAL"),
    sampNames = NULL,
    xlim = c(5,17)){
  
  minfi:::.isRGOrStop(rgSet)
  
  redVals <- minfi::getRed(rgSet)
  greenVals <- minfi::getGreen(rgSet)
  
  for (controlType in controls) {
    
    ## Get the row numbers of the probes that are specific to the indicated control type
    ctrlAddress <- minfi::getControlAddress(rgSet, controlType = controlType)
    
    ### Red
    ctlWide <- as.matrix(log2(redVals[ctrlAddress, , drop = FALSE]))
    
    if(!is.null(sampNames)) {
      colnames(ctlWide) = sampNames
    }
    
    ctlR <- reshape2::melt(ctlWide, varnames = c("address", "sample"))
    
    ### Green
    ctlWide <- as.matrix(log2(greenVals[ctrlAddress, , drop = FALSE]))
    
    if(!is.null(sampNames)) {
      colnames(ctlWide) = sampNames
    }
    
    ctlG <- reshape2::melt(ctlWide, varnames = c("address", "sample"))
    
    ## Create plot
    ctl <- ctlR%>%
      dplyr::mutate(
        channel = "Red",
        sample = as.character(sample)
      )%>%
      dplyr::bind_rows(
        ctlG%>%
          dplyr::mutate(
            channel = "Green",
            sample = as.character(sample)
          )
      )
    
    if (any((ctl$value < xlim[1]) | (ctl$value > xlim[2]))) {
      message("Warning: ", controlType, " probes outside plot range")
    }
    
    ctrlPlot <- ctl%>%
      ggplot(
        aes(x = value, y = sample, col = channel)
      ) +
      geom_point(shape = 18, size = 2.5) +
      facet_wrap(~channel) +
      theme_bw() +
      labs(
        x = "Log2 Intensity",
        title = paste("Control:", controlType)
      ) +
      theme(
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), # Center title, make the font bigger and bold
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() 
      ) +
      geom_hline(mapping=NULL, yintercept= seq(1, length(unique(ctl$sample))) - 0.5 ,colour='grey70', linetype = "dotted") +
      scale_color_manual(
        values = c("Green" = "darkgreen", "Red" = "darkred")
      ) +
      scale_x_continuous(
        limits = c(xlim[1], xlim[2]),
        breaks = seq(xlim[1] + xlim[1]%%2, xlim[2] - xlim[2]%%2, by = 2)
      )
    
    print(ctrlPlot)
  }
}

new_DNAm_qc_report <- function(rgSet, MSet = NULL, gSet = NULL,
                               qcPlotTitle = "", SexAtBirthPlotTitle = "", DensityPlotTitle = "Density Plot of Beta Values",
                               sampNames = NULL, sampGroups = NULL, actual_sex = NULL,
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
  
  minfi:::.isRGOrStop(rgSet)
  
  if (minfi:::.isMethylOrRatio(MSet) == FALSE) {
    MSet <- minfi::preprocessRaw(rgSet)
  }
  
  if (minfi:::.isMethylOrRatio(gSet) == FALSE) {
    gSet <- minfi::mapToGenome(MSet)
  }
  
  if(is.null(sampNames)) {
    sampNames <- colnames(rgSet)
  }
  
  n <- ncol(rgSet)
  
  if(is.null(sampGroups)) {
    sampGroups <- rep(1,n)
  }
  
  sampGroups <- as.factor(sampGroups)
  
  numPages <- ceiling(n/maxSamplesperPage)
  samplesPerPage <- ceiling(n/numPages)
  
  sampleIdxs <- suppressWarnings(
    split(seq_len(n), rep(seq_len(numPages), each = samplesPerPage))
  )
  
  pdf(file = pdf_filename, width = page_dims[1], height = page_dims[2])
  
  print(
    cowplot::plot_grid(
      rectGrob(gp = gpar(fill = NA, col = NA)),
      QC_passfailplot_HRS_version(minfi::getQC(MSet), qc_plot_title = qcPlotTitle),
      rectGrob(gp = gpar(fill = NA, col = NA)),
      nrow = 3, rel_heights = c(0.02, 0.49, 0.49)
    )
  )
  
  print(
    cowplot::plot_grid(
      rectGrob(gp = gpar(fill = NA, col = NA)),
      sex_at_birth_plot_HRS_version(gSet, main = SexAtBirthPlotTitle, actual_sex = actual_sex),
      rectGrob(gp = gpar(fill = NA, col = NA)),
      nrow = 3, rel_heights = c(0.02, 0.49, 0.49)
    )
  )
  
  par(mfrow = c(2,1))
  
  minfi::densityPlot(rgSet, sampGroups = sampGroups, main = DensityPlotTitle, xlab = "Beta")
  
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
  
  dev.off()
}

# Minfi's original detection P function: https://github.com/hansenlab/minfi/blob/devel/R/detectionP.R

minfi_detP_original <- function(rgSet, failed_probes = ""){
  
  minfi:::.isRGOrStop(rgSet)
  
  locusNames <- minfi::getManifestInfo(rgSet, type = "locusNames")
  locusNames <- setdiff(locusNames, failed_probes)
  
  controlIndex <- minfi::getControlAddress(rgSet, controlType = "NEGATIVE")
  
  rVals <- minfi::getRed(rgSet)
  gVals <- minfi::getGreen(rgSet)
  
  TypeI.Red <- minfi::getProbeInfo(rgSet, type = "I-Red")
  TypeI.Green <- minfi::getProbeInfo(rgSet, type = "I-Green")
  TypeII <- minfi::getProbeInfo(rgSet, type = "II")
  
  TypeI.Red <- TypeI.Red[!(TypeI.Red$Name %in% failed_probes), ,drop = FALSE]
  TypeI.Green <- TypeI.Green[!(TypeI.Green$Name %in% failed_probes), ,drop = FALSE]
  TypeII <- TypeII[!(TypeII$Name %in% failed_probes), ,drop = FALSE]
  
  detP_matrix <- matrix(
    data = NA_real_,
    nrow = length(locusNames),
    ncol = ncol(rVals),
    dimnames = list(locusNames, colnames(rVals))
  )
  
  rBg <- rVals[controlIndex, , drop = FALSE]
  rMu <- matrixStats::colMedians(rBg)
  rSd <- matrixStats::colMads(rBg)
  
  gBg <- gVals[controlIndex, , drop = FALSE]
  gMu <- matrixStats::colMedians(gBg)
  gSd <- matrixStats::colMads(gBg)
  
  for (i in seq_len(ncol(detP_matrix))){
    
    ## Type I Red
    intensity <- rVals[TypeI.Red$AddressA, i] + rVals[TypeI.Red$AddressB, i]
    detP_matrix[TypeI.Red$Name, i] <- pnorm(
      q = intensity,
      mean = 2 * rMu[i],
      sd = 2 * rSd[i],
      lower.tail = FALSE
    )
    
    ## Type I Green
    intensity <- gVals[TypeI.Green$AddressA, i] + gVals[TypeI.Green$AddressB, i]
    detP_matrix[TypeI.Green$Name, i] <- pnorm(
      q = intensity,
      mean = 2 * gMu[i],
      sd = 2 * gSd[i],
      lower.tail = FALSE
    )
    
    ## Type II
    intensity <- rVals[TypeII$AddressA, i] + gVals[TypeII$AddressA, i]
    detP_matrix[TypeII$Name, i] <- pnorm(
      q = intensity,
      mean = rMu[i] + gMu[i],
      sd = rSd[i] + gSd[i],
      lower.tail = FALSE
    )
  }
  
  detP_matrix
}

minfi_detP_fixed <- function(rgSet, failed_probes = ""){
  
  minfi:::.isRGOrStop(rgSet)
  
  locusNames <- minfi::getManifestInfo(rgSet, type = "locusNames")
  locusNames <- setdiff(locusNames, failed_probes)
  
  controlIndex <- minfi::getControlAddress(rgSet, controlType = "NEGATIVE")
  
  rVals <- minfi::getRed(rgSet)
  gVals <- minfi::getGreen(rgSet)
  
  TypeI.Red <- minfi::getProbeInfo(rgSet, type = "I-Red")
  TypeI.Green <- minfi::getProbeInfo(rgSet, type = "I-Green")
  TypeII <- minfi::getProbeInfo(rgSet, type = "II")
  
  TypeI.Red <- TypeI.Red[!(TypeI.Red$Name %in% failed_probes), ,drop = FALSE]
  TypeI.Green <- TypeI.Green[!(TypeI.Green$Name %in% failed_probes), ,drop = FALSE]
  TypeII <- TypeII[!(TypeII$Name %in% failed_probes), ,drop = FALSE]
  
  detP_matrix <- matrix(
    data = NA_real_,
    nrow = length(locusNames),
    ncol = ncol(rVals),
    dimnames = list(locusNames, colnames(rVals))
  )
  
  rBg <- rVals[controlIndex, , drop = FALSE]
  rMu <- matrixStats::colMedians(rBg)
  rSd <- matrixStats::colMads(rBg)
  
  gBg <- gVals[controlIndex, , drop = FALSE]
  gMu <- matrixStats::colMedians(gBg)
  gSd <- matrixStats::colMads(gBg)
  
  for (i in seq_len(ncol(detP_matrix))){
    
    ## Type I Red
    intensity <- rVals[TypeI.Red$AddressA, i] + rVals[TypeI.Red$AddressB, i]
    detP_matrix[TypeI.Red$Name, i] <- pnorm(
      q = intensity,
      mean = 2 * rMu[i],
      sd = 2 * rSd[i],
      lower.tail = FALSE
    )
    
    ## Type I Green
    intensity <- gVals[TypeI.Green$AddressA, i] + gVals[TypeI.Green$AddressB, i]
    detP_matrix[TypeI.Green$Name, i] <- pnorm(
      q = intensity,
      mean = 2 * gMu[i],
      sd = 2 * gSd[i],
      lower.tail = FALSE
    )
    
    ## Type II
    intensity <- rVals[TypeII$AddressA, i] + gVals[TypeII$AddressA, i]
    detP_matrix[TypeII$Name, i] <- pnorm(
      q = intensity,
      mean = rMu[i] + gMu[i],
      sd = sqrt((rSd[i])^2 + (gSd[i])^2), ### fixed bug from minfi's function
      lower.tail = FALSE
    )
  }
  
  detP_matrix
}

modified_ewastools_detP <- function(rgSet, failed_probes = ""){
  
  minfi:::.isRGOrStop(rgSet)
  
  locusNames <- minfi::getManifestInfo(rgSet, type = "locusNames")
  locusNames <- setdiff(locusNames, failed_probes)
  
  detP_matrix <- matrix(
    data = NA_real_,
    nrow = length(locusNames),
    ncol = ncol(rgSet),
    dimnames = list(locusNames, colnames(rgSet))
  )
  
  iR <- minfi::getProbeInfo(rgSet, type = "I-Red")
  iG <- minfi::getProbeInfo(rgSet, type = "I-Green")
  i2 <- minfi::getProbeInfo(rgSet, type = "II")
  
  iR <- iR[!(iR$Name %in% failed_probes), ,drop = FALSE]
  iG <- iG[!(iG$Name %in% failed_probes), ,drop = FALSE]
  i2 <- i2[!(i2$Name %in% failed_probes), ,drop = FALSE]
  
  iR <- data.table::as.data.table(as.data.frame(iR))
  iG <- data.table::as.data.table(as.data.frame(iG))
  i2 <- data.table::as.data.table(as.data.frame(i2))
  
  betas <- minfi::getBeta(rgSet)
  betasIr <- betas[iR$Name,]
  betasIg <- betas[iG$Name,]
  rm(betas)
  
  rVals <- minfi::getRed(rgSet)
  gVals <- minfi::getGreen(rgSet)
  rm(rgSet)

  muUR = muMR = muUG = muMG = numeric(ncol(detP_matrix))
  sdUR = sdMR = sdUG = sdMG = numeric(ncol(detP_matrix))
  
  for (i in seq_len(ncol(detP_matrix))) {
    sR = ewastools:::summits(betasIr[,i])
    sG = ewastools:::summits(betasIg[,i])
    
    ## Red
    bkgU = order(abs(betasIr[,i] - sR[2]))[1:1000]
    bkgM = order(abs(betasIr[,i] - sR[1]))[1:1000]
    
    bkgU = iR$AddressA[bkgU]
    bkgM = iR$AddressB[bkgM]
    
    bkgU = rVals[bkgU, i]
    bkgM = rVals[bkgM, i]
    
    muUR[i] = median(bkgU, na.rm = TRUE)
    muMR[i] = median(bkgM, na.rm = TRUE)
    
    sdUR[i] = mad(bkgU, na.rm = TRUE)
    sdMR[i] = mad(bkgM, na.rm = TRUE)
    
    ## Green
    bkgU = order(abs(betasIg[,i] - sG[2]))[1:1000]
    bkgM = order(abs(betasIg[,i] - sG[1]))[1:1000]
    
    bkgU = iG$AddressA[bkgU]
    bkgM = iG$AddressB[bkgM]
    
    bkgU = gVals[bkgU, i]
    bkgM = gVals[bkgM, i]
    
    muUG[i] = median(bkgU, na.rm = TRUE)
    muMG[i] = median(bkgM, na.rm = TRUE)
    
    sdUG[i] = mad(bkgU, na.rm = TRUE)
    sdMG[i] = mad(bkgM, na.rm = TRUE)
  }
  
  detP_matrix[iR$Name,] = pnorm(rVals[iR$AddressA,]+rVals[iR$AddressB,], mean=rep(muUR+muMR,each=nrow(iR)), sd=rep(sqrt(sdUR^2+sdMR^2),each=nrow(iR)), lower.tail=FALSE)
  detP_matrix[iG$Name,] = pnorm(gVals[iG$AddressA,]+gVals[iG$AddressB,], mean=rep(muUG+muMG,each=nrow(iG)), sd=rep(sqrt(sdUG^2+sdMG^2),each=nrow(iG)), lower.tail=FALSE)
  detP_matrix[i2$Name,] = pnorm(rVals[i2$AddressA,]+gVals[i2$AddressA,], mean=rep(muUR+muMG,each=nrow(i2)), sd=rep(sqrt(sdUR^2+sdMG^2),each=nrow(i2)), lower.tail=FALSE)
  
  detP_matrix
}

minfi_preprocess_noob_filtered_RGSet <- function(
    rgSet, offset = 15, dyeCorr = TRUE, verbose = FALSE, dyeMethod = c("single", "reference"),
    passedProbesOnly = NULL, new_colnames = NULL){
  
  minfi:::.isRGOrStop(rgSet)
  dyeMethod <- base::match.arg(dyeMethod)
  
  oob = minfi::getOOB(rgSet)
  GreenOOB <- oob$Grn
  RedOOB <- oob$Red
  
  MSet <- minfi::preprocessRaw(rgSet)
  
  if (!is.null(passedProbesOnly)) {
    MSet <- MSet[passedProbesOnly, , drop = FALSE]
  }
  
  anno <- minfi::getAnnotation(MSet) # HERE: Probes with no genomic coordinates auto-dropped
  
  probe.type <- paste0(anno$Type, anno$Color)
  Green_probes <- base::intersect(base::rownames(MSet), anno$Name[probe.type == "IGrn"])
  Red_probes <- base::intersect(base::rownames(MSet), anno$Name[probe.type == "IRed"])
  d2.probes <- base::intersect(base::rownames(MSet), anno$Name[probe.type == "II"])
  
  Meth <- minfi::getMeth(MSet)
  Unmeth <- minfi::getUnmeth(MSet)
  
  control_probes <- minfi::getProbeInfo(rgSet, type = "Control")
  control_probes <- control_probes[control_probes$Address %in% rownames(rgSet),]
  
  Red <- minfi::getRed(rgSet)
  Green <- minfi::getGreen(rgSet)
  array_type <- rgSet@annotation[["array"]]
  
  rm(rgSet)
  
  M_and_U <- minfi:::.preprocessNoob(
    Meth = Meth,
    Unmeth = Unmeth,
    GreenOOB = GreenOOB,
    RedOOB = RedOOB,
    Green_probes = Green_probes,
    Red_probes = Red_probes,
    d2.probes = d2.probes,
    offset = offset,
    dyeCorr = dyeCorr,
    Red = Red,
    Green = Green,
    control_probes = control_probes,
    array_type = array_type,
    dyeMethod = dyeMethod,
    verbose = verbose
  )
  
  SummarizedExperiment::assay(MSet, "Meth") <- M_and_U[["Meth"]]
  SummarizedExperiment::assay(MSet, "Unmeth") <- M_and_U[["Unmeth"]]
  
  MSet@preprocessMethod <- c(
    mu.norm = sprintf("Noob, dyeCorr=%s, dyeMethod=%s", dyeCorr, dyeMethod)
  )
  
  if (!is.null(new_colnames)) {
    colnames(MSet) = new_colnames
  }
  
  MSet
}