#### Plot generating function definitions ####

generate_plot <- function(dataset, indep_var, dep_var=NA, stratify_var=NA, stratify_flag=F,
                          labels_flag = F, legend_flag = F, legend_position = "right", destination="screen", palette="Set1",
                          trend_resolution = NA){

  indep_var_type <- class(dataset[[indep_var]])[1]
  dep_var_type <- NA

  if(!is.na(dep_var)){

    dep_var_type <- class(dataset[[dep_var]])

    if(dep_var_type %in% c("factor","logical")){
      if(indep_var_type %in% c("factor","logical")){
        # generate bar plot, categorical vs categorical, counts
        return_plot <- generate_bar_plot(dataset = dataset,
                                         indep_var = indep_var,
                                         dep_var = dep_var,
                                         labels_flag = labels_flag,
                                         legend_flag = legend_flag)
      }
      else if(indep_var_type %in% c("numeric","integer")){
        # generate horizontal boxplot, numeric vs categorical
        # flip the variables so it plots right, but then flip the plot
        temp_var <- indep_var
        indep_var <- dep_var
        dep_var <- temp_var
        return_plot <- generate_boxplot(dataset = dataset,
                                        dep_var = dep_var,
                                        indep_var = indep_var,
                                        labels_flag = labels_flag,
                                        flip_axes_flag = T)
      } else if (indep_var_type == "POSIXct"){
        ## this is now a trend plot, make it a line graph
        return_plot <- generate_scatter_line_plot(dataset = dataset,
                                                  indep_var = indep_var,
                                                  dep_var = dep_var,
                                                  stratify_flag = stratify_flag,
                                                  labels_flag = labels_flag,
                                                  legend_flag = legend_flag,
                                                  destination = destination)
      }
    } else if (dep_var_type %in% c("numeric","integer")){
      if(indep_var_type %in% c("factor","logical")){
        # generate vertical boxplot, categorical vs numeric
        return_plot <- generate_boxplot(dataset = dataset,
                                        dep_var = dep_var,
                                        indep_var = indep_var,
                                        labels_flag = labels_flag,
                                        flip_axes_flag = F)
      } else if(indep_var_type %in% c("numeric","integer")){
        return_plot <- generate_scatter_plot(dataset = dataset,
                                             indep_var = indep_var,
                                             dep_var = dep_var)
      } else if (indep_var_type == "POSIXct"){
        ## this is now a trend plot, make it a line graph
        return_plot <- generate_scatter_line_plot(dataset = dataset,
                                                  indep_var = indep_var,
                                                  dep_var = dep_var,
                                                  stratify_flag = stratify_flag,
                                                  labels_flag = labels_flag,
                                                  legend_flag = legend_flag,
                                                  destination = destination)
      }
    }
  }
  # single variable summaries, only used for bar plot for categorical variables for counts
  else if (is.na(dep_var)){
    # generate bar plot for counts
    if(indep_var_type %in% c("factor","logical")){
      return_plot <- generate_bar_plot(dataset, indep_var=indep_var, dep_var=dep_var, labels_flag=labels_flag)
    }
    else if (indep_var_type %in% c("numeric","integer")){
      return(NULL)
    }
  }

  # style plot differently for different plots
  if (indep_var_type == "POSIXct"){
    # trend graphs
    return_plot <- style_plot(plot = return_plot,
                              indep_var = paste0("Trend over ",trend_resolution),
                              dep_var = dep_var,
                              dep_var_type = dep_var_type,
                              stratify_var = stratify_var,
                              stratify_flag = stratify_flag,
                              legend_flag = T,
                              legend_position = legend_position,
                              destination = destination,
                              plot_type = "trend",
                              palette = palette
    )
  } else if (indep_var_type %in% c("factor","logical") && is.na(dep_var)){
    # single variable bar plot
    return_plot <- style_plot(plot = return_plot,
                              indep_var = indep_var,
                              indep_var_type = indep_var_type,
                              stratify_var = stratify_var,
                              stratify_flag = stratify_flag,
                              legend_flag = legend_flag,
                              legend_position = legend_position,
                              plot_title = paste0("Summary of ",indep_var),
                              destination = destination,
                              plot_type = "count",
                              palette = palette
    )
  } else {
    # all correlations
    return_plot <- style_plot(plot = return_plot,
                              indep_var = indep_var,
                              indep_var_type = indep_var_type,
                              dep_var = dep_var,
                              dep_var_type = dep_var_type,
                              stratify_var = stratify_var,
                              stratify_flag = stratify_flag,
                              legend_flag = legend_flag,
                              legend_position = legend_position,
                              destination = destination,
                              plot_type = "correlation",
                              palette = palette
    )
  }

  return(return_plot)
}

generate_bar_plot <- function(dataset, indep_var, dep_var=NA, labels_flag=F, legend_flag = F){
  if(!is.na(dep_var)){
    # compare two variables boxplot,
    # generate plot labels
    plot_labels <- dataset %>% group_by(!!as.symbol(indep_var), stratify_by, !!as.symbol(dep_var)) %>%
      summarise(cnt = n()) %>% mutate(value = cnt/sum(cnt), cnt_all=sum(cnt)) %>% ungroup() %>%
      complete(!!as.symbol(indep_var), stratify_by, !!as.symbol(dep_var), fill=list(cnt=0, value=0, cnt_all=0))

    # generate plot, bar plot
    plot_data <-   dataset %>% group_by(!!as.symbol(indep_var), stratify_by, !!as.symbol(dep_var)) %>%
      summarise(cnt = n()) %>% mutate(pct_cnt=cnt/sum(cnt), cnt_all = sum(cnt)) %>% ungroup() %>%
      complete(!!as.symbol(indep_var), stratify_by, !!as.symbol(dep_var), fill=list(cnt=0, pct_cnt=0, cnt_all=0))

    return_plot <- plot_data %>% ggplot() +
      geom_bar(aes(x=!!as.symbol(indep_var), y=pct_cnt, fill=!!as.symbol(dep_var)),
               color="black", position=position_dodge(0.9, preserve = 'total'), stat="identity", show.legend=legend_flag) +
      coord_cartesian(ylim=c(-0.05,1.1)) +
      facet_wrap(~stratify_by, labeller=label_value)

    # add plot labels
    if(labels_flag == T){
      return_plot <- return_plot +
        geom_label(data=plot_labels,
                   aes(x=!!as.symbol(indep_var), y=value, group=!!as.symbol(dep_var), label=scales::percent(value, accuracy=2)),
                   alpha=0.7, size=label_size, position=position_dodge(0.9), show.legend = FALSE) +
        geom_text(data=plot_labels,
                  aes(x=!!as.symbol(indep_var), y=-Inf, fill=!!as.symbol(dep_var),
                      label=paste0("n=",cnt,"/",cnt_all)),
                  position=position_dodge(0.9, preserve = 'total'), size=label_size, angle=45, hjust="left", show.legend = FALSE)
    }
  } else if(is.na(dep_var)){
    # single variable summary bar plot
    # generate plot labels
    plot_labels <- dataset %>% group_by(!!as.symbol(indep_var), stratify_by) %>%
      summarise(cnt = n()) %>% ungroup() %>%  mutate(value = cnt/sum(cnt), cnt_all=sum(cnt)) %>%
      complete(!!as.symbol(indep_var), stratify_by, fill=list(cnt=0, value=0, cnt_all=0))

    # generate plot, bar plot
    plot_data <-   dataset %>% group_by(!!as.symbol(indep_var), stratify_by) %>%
      summarise(cnt = n()) %>% ungroup() %>% mutate(pct_cnt=cnt/sum(cnt), cnt_all = sum(cnt)) %>%
      complete(!!as.symbol(indep_var), stratify_by, fill=list(cnt=0, pct_cnt=0, cnt_all=0))

    return_plot <- plot_data %>% ggplot() +
      geom_bar(aes(x=!!as.symbol(indep_var), y=pct_cnt, fill=!!as.symbol(indep_var)),
               color="black", position=position_dodge(0.9, preserve = 'total'), stat="identity", show.legend = FALSE) +
      coord_cartesian(ylim=c(-0.05,1.1)) +
      facet_wrap(~stratify_by, labeller=label_value)

    # add plot labels
    if(labels_flag == T){
      return_plot <- return_plot +
        geom_label(data=plot_labels,
                   aes(x=!!as.symbol(indep_var), y=value, group=!!as.symbol(indep_var), label=scales::percent(value, accuracy=2)),
                   alpha=0.7, size=label_size, position=position_dodge(0.9), show.legend = FALSE) +
        geom_text(data=plot_labels,
                  aes(x=!!as.symbol(indep_var), y=-Inf, fill=!!as.symbol(indep_var),
                      label=paste0("n=",cnt,"/",cnt_all)),
                  position=position_dodge(0.9, preserve = 'total'), size=label_size, angle=45, hjust="left", show.legend = FALSE)
    }
  }
  return(return_plot)
}

generate_boxplot <- function(dataset, indep_var, dep_var, stratify_flag = F, flip_axes_flag=F, labels_flag){
  # labels

  plot_labels <- dataset %>% group_by(!!as.symbol(indep_var), stratify_by) %>%
    summarise(cnt=n(), value = quantile(!!as.symbol(dep_var), c(0.25,0.5,0.75), na.rm=TRUE), probs=c(0.25,0.5,0.75))
  # plot -  side by side grouped bar plot
  return_plot <- dataset %>% ggplot() +
    geom_violin(aes(x=get(indep_var), y=get(dep_var), fill=get(indep_var)),
                na.rm=TRUE, trim=FALSE) +
    geom_boxplot(aes(x=get(indep_var), y=get(dep_var)), fill="white",
                 width = 0.2, na.rm=TRUE, outlier.shape=NA) +
    # jitter extendfs the yaxis too much
    # geom_jitter(data = subset(dataset, get(variable_of_interest) > quantile(get(variable_of_interest), 0.75) + 1.5 * IQR(get(variable_of_interest))),
    #             aes(x=get(correlate_var), y=get(variable_of_interest)),
    #             width = 0.2, height = 0.1, color = "black") +
    # coord_cartesian(ylim=c(NA,ylim_max)) +
    facet_wrap(~stratify_by, labeller=label_value)

  if(flip_axes_flag == T) return_plot <- return_plot + coord_flip()

  if(labels_flag==T){
    return_plot <- return_plot +
      geom_richtext(data=plot_labels,
                    aes(x=!!as.symbol(indep_var), y=value*1.01, label=value),
                    fill="white", size=label_size, position=position_dodge(0.9), angle=45*flip_axes_flag)+
      geom_text(data=plot_labels,
                aes(x=!!as.symbol(indep_var), y=-Inf, label=paste0("n=",cnt)),
                position=position_dodge(0.9), size=label_size, angle=45, hjust="left")
  }

  return(return_plot)
}

generate_scatter_plot <- function(dataset, indep_var, dep_var, stratify_flag = F, stratify_var = NA){
  return_plot <- dataset %>% ggplot() +
    geom_point(aes(x=get(indep_var), y=get(dep_var)), na.rm=TRUE) +
    geom_smooth(aes(x=get(indep_var), y=get(dep_var))) +
    # coord_cartesian(xlim=c(NA,xlim_max),ylim=c(NA,ylim_max)) +
    stat_cor(aes(x=get(indep_var), y=get(dep_var)), method = "pearson", label.x.npc = "right", label.y.npc="top", hjust=1, size=4) +
    facet_wrap(~stratify_by, labeller=label_value)

  return(return_plot)
}

generate_qq_plot <- function(dataset, variable_of_interest, stratify_var, stratify_flag, destination="screen"){
  return_plot <- ggplot(dataset, aes(sample = !!as.symbol(variable_of_interest))) +
    geom_qq_line() +
    geom_qq() +
    facet_wrap(~stratify_by)

  return_plot <- style_plot(plot = return_plot,
                            indep_var = variable_of_interest,
                            stratify_var = stratify_var,
                            stratify_flag = stratify_flag,
                            plot_title="Q-Q Plot",
                            x_axis_title = "Theoretical",
                            y_axis_title = "Sample",
                            destination = destination)

  return(return_plot)
}

generate_histogram_plot <- function(dataset, variable_of_interest, stratify_var, stratify_flag, destination="screen"){
  if(length(unique(dataset[[variable_of_interest]]))<10){
    return_plot <- dataset %>% ggplot() +
      geom_histogram(aes(x=!!as.symbol(variable_of_interest)), bins=length(unique(dataset[[variable_of_interest]])), fill="lightblue", color="black") +
      facet_wrap(~stratify_by)
  }
  else {
    binmin <- min(dataset[[variable_of_interest]], na.rm=T)
    binmax <- max(dataset[[variable_of_interest]], na.rm=T)
    bwidth <- (binmax - binmin) / 10
    return_plot <- dataset %>% ggplot() + geom_histogram(aes(x=!!as.symbol(variable_of_interest)), binwidth=bwidth, fill="lightblue", color="black") +
      facet_wrap(~stratify_by)
  }

  return_plot <- style_plot(plot = return_plot,
                            indep_var = variable_of_interest,
                            stratify_var = stratify_var,
                            stratify_flag = stratify_flag,
                            plot_title="Histogram",
                            destination = destination,
                            plot_type = "count")

  return(return_plot)
}

generate_scatter_line_plot <- function(dataset, indep_var, dep_var, stratify_flag = F, labels_flag, legend_flag = T, destination){

  # init variables
  dep_var_type <- class(dataset[[dep_var]])
  legend_flag_func <- stratify_flag == T && legend_flag == T
  point_size <- 2
  line_width <- 4

  if(destination == "screen"){
    point_size <- 4
    line_width <- 2
  }
  else if(destination == "slide"){
    point_size <- 6
    line_width <- 3
  }


  dataset_summary <-
    # take dataset, group by column whos name is stored in indep_var, average the values in the column whos name is
    # stored in dep_var, plot a scatter and line, color=column whos name is stored in stratify_var
    if(dep_var_type %in% c("numeric","integer")){

      plot_data <- dataset %>% group_by(!!as.symbol(indep_var), stratify_by) %>%
        summarize(mean_val = mean(!!as.symbol(dep_var)), sd_val = sd(!!as.symbol(dep_var), na.rm=T))

      return_plot <- ggplot(data=plot_data) +
        geom_point(aes(x = !!as.symbol(indep_var), y = mean_val, color = stratify_by), size=point_size, shape=18, show.legend = F) +
        geom_line(aes(x = !!as.symbol(indep_var), y = mean_val, color = stratify_by), linewidth=line_width, show.legend = legend_flag_func)
      # geom_errorbar(aes(x=!!as.symbol(indep_var), ymin = mean_val-sd_val, ymax = mean_val+sd_val, color=stratify_by),width = 0.2)

      if(labels_flag){ return_plot <- return_plot + geom_label(data=plot_data, aes(x = !!as.symbol(indep_var),
                                                                                   y = mean_val, label = mean_val, color = stratify_by))}
    }
  else if (dep_var_type %in% c("factor","logical")){

    plot_data <- dataset %>% group_by(stratify_by, !!as.symbol(indep_var), !!as.symbol(dep_var)) %>%
      summarize(cnt = n())

    return_plot <- ggplot(data=plot_data) +
      geom_point(aes(x = !!as.symbol(indep_var), y = cnt, color = !!as.symbol(dep_var)), size=point_size, shape=18, show.legend = F) +
      geom_line(aes(x = !!as.symbol(indep_var), y = cnt, color = !!as.symbol(dep_var)), linewidth=line_width, show.legend = legend_flag_func) +
      facet_wrap(~stratify_by)

    if(labels_flag){ return_plot <- return_plot + geom_label(data=plot_data, aes(x = !!as.symbol(indep_var), y = cnt,
                                                                                 label = cnt, color = stratify_by))}
  }

  return_plot <- return_plot +
    theme(axis.text.x = element_text(angle = 45))

  return(return_plot)
}

style_plot <- function(plot,
                       indep_var=NA, indep_var_type=NA,
                       dep_var=NA, dep_var_type=NA,
                       stratify_var=NA, stratify_flag=F,
                       legend_flag = F, legend_position = "right",
                       plot_title=NA, x_axis_title=NA, y_axis_title=NA,
                       destination="screen", plot_type="correlation", palette = NA){
  #3 possibilities

  # single variable bar plot
  if(plot_type == "count"){
    # plot titles config
    indep_var_title_func <- str_to_title(gsub("_", " ", indep_var))
    stratify_var_title_func <- str_to_title(gsub("_"," ",stratify_var))
    x_axis_title_func <- indep_var_title_func
    y_axis_title_func <- "Count"
    legend_title_func <- indep_var_title_func
    plot_title_func <- indep_var_title_func
    plot_title_func <- paste0(plot_title_func," stratified by ",stratify_var_title_func)
    plot_title_func <- str_to_title(plot_title_func)

    # no legend config for single variable bar plot
  }
  # trend graph
  else if (plot_type == "trend"){
    # plot titles config
    indep_var_title_func <- "Trend period"
    dep_var_title_func <- str_to_title(gsub("_", " ", dep_var))
    stratify_var_title_func <- str_to_title(gsub("_"," ",stratify_var))
    x_axis_title_func <- indep_var_title_func
    y_axis_title_func <- dep_var_title_func
    legend_title_func <- stratify_var_title_func
    plot_title_func <- indep_var # paste0("Trend of ", dep_var_title_func)
    if(stratify_flag == T) plot_title_func <- paste0(plot_title_func," stratified by ",stratify_var_title_func)
    plot_title_func <- str_to_title(plot_title_func)

    #legend config
    legend_flag_local <- ifelse(stratify_flag==T,"legend","none")
    # plot <- plot + guides(color = legend_flag_local)
  }

  # dual variable - make some choices based on dep_var_type factor/logical
  else if (plot_type == "correlation") {
    # plot titles config
    dep_var_title_func <- str_to_title(gsub("_", " ", dep_var))
    indep_var_title_func <- str_to_title(gsub("_", " ", indep_var))
    stratify_var_title_func <- str_to_title(gsub("_"," ",stratify_var))
    x_axis_title_func <- indep_var_title_func
    y_axis_title_func <- ifelse(dep_var_type %in% c("factor","logical"),
                                paste0(dep_var_title_func,": % of cases"),
                                dep_var_title_func)
    plot_title_func <- paste0(indep_var_title_func, " vs ", dep_var_title_func)
    if(stratify_flag == T) plot_title_func <- paste0(plot_title_func," stratified by ",stratify_var_title_func)
    plot_title_func <- str_to_title(plot_title_func)

    #legend config
    legend_title_func <- ifelse(dep_var_type %in% c("factor","logical"),
                                dep_var_title_func,
                                indep_var_title_func)
    legend_flag_local <- ifelse(legend_flag==T,"legend","none")
    # if(coalesce(indep_var_type,"none") == "factor" && coalesce(dep_var_type,"none") == "factor")
    #     plot <- plot + guides(fill = guide_legend(legend_flag_local))
  }
  # earmark this as it works for the QQ plot but if other plot types arise, then this would be the template
  # as it is, it will never trigger because of default plot type parameter = "correlation"
  else {
    # plot titles config
    dep_var_title_func <- str_to_title(gsub("_", " ", dep_var))
    indep_var_title_func <- str_to_title(gsub("_", " ", indep_var))
    stratify_var_title_func <- str_to_title(gsub("_"," ",stratify_var))
    x_axis_title_func <- indep_var_title_func
    y_axis_title_func <- ifelse(dep_var_type %in% c("factor","logical"),
                                paste0(dep_var_title_func,": % of cases"),
                                dep_var_title_func)
    plot_title_func <- paste0(indep_var_title_func, " vs ", dep_var_title_func)
    if(stratify_flag == T) plot_title_func <- paste0(plot_title_func," stratified by ",stratify_var_title_func)
    plot_title_func <- str_to_title(plot_title_func)

    #legend config
    legend_title_func <- ifelse(dep_var_type %in% c("factor","logical"),
                                dep_var_title_func,
                                indep_var_title_func)
    legend_flag_local <- ifelse(legend_flag==T,"legend","none")
    # if(coalesce(indep_var_type,"none") == "factor" && coalesce(dep_var_type,"none") == "factor")
    #     plot <- plot + guides(fill = guide_legend(legend_flag_local))
  }
  #

  if(!is.na(x_axis_title)) x_axis_title_func <- x_axis_title
  if(!is.na(y_axis_title)) y_axis_title_func <- y_axis_title
  if(!is.na(plot_title)) plot_title_func <- plot_title

  #variables are now established based on variable and plot type
  #first, basic theming, so we can overwrite later on; if we do this at the end, things like legend position get overridden

  if(destination=="slide"){
    plot <- plot + theme_slide
  }
  else if(destination == "screen"){
    plot <- plot + theme_screen
  }

  # final modification to overrwrite any strict parameters passed to the function
  plot <- plot +
    ylab(y_axis_title_func) +
    xlab(x_axis_title_func) +
    scale_fill_brewer(name=legend_title_func, palette=palette, ) +
    scale_color_brewer(name=legend_title_func, palette=palette) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.position = legend_position) +
    ggtitle(plot_title_func)

  return(plot)
}
