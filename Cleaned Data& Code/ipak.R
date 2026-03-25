
# ipak function: install and load multiple R packages----

# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak = function(pkg){
  new.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}


# usage----

ipak(c("tidyverse", "haven", "readxl", "lubridate", "data.table", "modelr", "broom", "leaflet", "stargazer", "AER", "ggmap", "geosphere", "Hmisc", "janitor", "patchwork", "DT"))

#devtools::install_github("rstudio/sparklyr")

#tidyverse_update()

# personal theme settings----

per_theme_set = theme_gray(base_size = 20, base_family = "") +
  theme(line = element_line(linewidth = 1.5), 
        text = element_text(family = NULL, size = NULL, face = "plain", colour = "black"),
        legend.key = element_rect(fill = "white"), 
        legend.key.width = unit(5, "line"), 
        legend.key.height = unit(1, "cm"),
        legend.text = element_text(family = NULL,size = 15, face = "plain"),
        legend.title = element_blank())

#### cormat function ####

cormat <- function(x){ 
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## truncate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 3))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}

#### standard deviation function ####

stdev = function(x)
{ 
  x = x[!is.na(x)]
  return(round(sqrt(sum((x-mean(x))^2)/length(x)), 4))   
}


#### Make Plots ####
make_plots <- function(df, group_var, title_label, by_status = FALSE) {
  # Grouping
  group_quo = enquo(group_var)
  group_var_str = as.character(rlang::quo_get_expr(group_quo))
  color_var <- if (by_status) "status" else rlang::as_label(rlang::enquo(group_var))
  # Combined Gantt
  p1 <- df %>%
    ggplot(aes(x = start, y = {{ group_var }}, 
               color = if (by_status) status else {{ group_var }})) +
    geom_segment(aes(xend = end, yend = {{ group_var }}), linewidth = 2) +
    labs(title = paste("Events Over Time -", title_label), x = "Date", y = "") +
    theme_minimal()
  
  # Combined daily count
  p2 <- df %>%
    mutate(day = as.Date(start)) %>%
    {if (by_status) count(., day, {{ group_var }}, status) 
      else count(., day, {{ group_var }})} %>%
    ggplot(aes(x = day, y = n, 
               color = {{ group_var }},
               linetype = if (by_status) status else NULL)) +
    geom_line() +
    labs(title = paste("Daily Count -", title_label), x = "Date", y = "# Events") +
    theme_minimal()
  
  combined <- p1 / p2
  print(combined)
  # Save combined patchwork
  ggsave(paste0("Images/",gsub(" ", "_", title_label), "_combined.png"),
         plot = combined, width = 12, height = 8, dpi = 300)
  
  # Individual group plots
  groups <- df %>% pull(!!group_quo) %>% unique()
  
  for (g in groups) {
    df_g <- df[df[[as.character(substitute(group_var))]] == g, ]
    
  
  # Individuals per group
    groups <- df %>% pull(!!group_quo) %>% unique()
    
    for (g in groups) {
      df_g <- df[df[[as.character(substitute(group_var))]] == g, ]
    
    pi1 <- df_g %>%
      ggplot(aes(x = start, y = {{ group_var }},
                 color = if (by_status) status else {{ group_var }})) +
      geom_segment(aes(xend = end, yend = {{ group_var }}), linewidth = 2) +
      labs(title = paste("Events -", title_label, "-", g), x = "Date", y = "") +
      theme_minimal()
    
    pi2 <- df_g %>%
      mutate(day = as.Date(start)) %>%
      {if (by_status) count(., day, status) else count(., day)} %>%
      ggplot(aes(x = day, y = n,
                 color = if (by_status) status else NULL)) +
      geom_line() +
      labs(title = paste("Daily Count -", title_label, "-", g), x = "Date", y = "# Events") +
      theme_minimal()
    
    ind_plot <- pi1 / pi2
    print(ind_plot)
    
    # Save each individual plot
    ggsave(paste0("Images/", gsub(" ", "_", title_label), "_", gsub(" ", "_", g), ".png"),
           plot = ind_plot, width = 12, height = 8, dpi = 300)
  }
  
  invisible(combined)  # return combined silently
  }
}

#### Make Summary ####
# Function to generate summary stats
  
  make_summary <- function(df, group_var, time_var = start) {
    df %>%
      group_by({{ group_var }}) %>%
      summarise(
        total = n(),
        avg_dur_secs = mean(dur_secs, na.rm = TRUE),
        median_dur_secs = median(dur_secs, na.rm = TRUE),
        min_dur_secs = min(dur_secs, na.rm = TRUE),
        max_dur_secs = max(dur_secs, na.rm = TRUE),
        total_dur_secs = sum(dur_secs, na.rm = TRUE),
        first_event = min(start),
        last_event = max(start),
        total_days = as.numeric(difftime(max(start), min(start), units = "days")),
        avg_per_day = n() / as.numeric(difftime(max(start), min(start), units = "days")),
        mtbi_secs = as.numeric(difftime(max(start), min(start), units = "secs")) / (n() - 1),
        mtbi_mins = mtbi_secs / 60,
        avg_dur_mins = avg_dur_secs / 60,
        .groups = "drop"
      )
  }
  
  

