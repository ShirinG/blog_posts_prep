load("diff_table_bind.RData")
load("dataset_fem.RData")
load("dataset_male.RData")
load("wmap_countries_smaller_df_final.RData")

load("measures.RData")

load("year_table_last_val.RData")

country.inf <- subset(wmap_countries_df_final[, c(17, 25, 26, 31, 32, 42, 43, 47, 48, 62)], !continent == "Antarctica")
country.inf <- country.inf[!duplicated(country.inf), ]


library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)

map_theme <- list(theme(panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.background = element_blank(),
                        plot.background = element_rect(fill = "white"),
                        panel.border = element_blank(),
                        axis.line = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y = element_blank(),
                        axis.ticks = element_blank(),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        plot.title = element_text(size = 18)))

my_theme <- function(base_size = 12, base_family = "sans"){
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      panel.grid.major = element_line(color = "grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white"),
      strip.background = element_rect(fill = "royalblue", color = "grey", size = 1),
      strip.text = element_text(face = "bold", size = 12, color = "white"),
      legend.position = "right",
      panel.border = element_rect(color = "grey", fill = NA, size = 0.5)
    )
}

colfunc <- colorRampPalette(c("yellow", "red"))

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  # Drop-down selection box for which measurement
  output$choose_measure <- renderUI({
    selectInput("measure", "Statistic", measures)
  })

  # Check years
  output$choose_years <- renderUI({
    measure = input$measure
    subs <- diff_table_bind[which(diff_table_bind$Indicator.Name == measure), -c(1:4)]
    keep <- gsub("X", "", names(which(colSums(subs, na.rm = TRUE) > 0)))

    selectInput("year", "Year",
                choices = rev(keep))
  })


  # World map

  output$map <- renderPlot({

    measure <- input$measure
    year <- input$year

    if (input$sex == "ratio") {

      diff_table_map <- diff_table_bind[which(diff_table_bind$Indicator.Name == measure),
                                        c(1:3, which(colnames(diff_table_bind) == paste0("X", year)))]
      colnames(diff_table_map)[ncol(diff_table_map)] <- "value"

      map <- left_join(subset(wmap_countries_df_final, !continent == "Antarctica"), diff_table_map, by = c("gu_a3" = "Country.Code"))
      map$value_log2 <- log2(as.numeric(map$value))

      ggplot(map, aes(long, lat, group = group, fill = value_log2)) +
        map_theme +
        geom_polygon() +
        geom_path(color = "white", size = 0.5) +
        labs(title = paste(measure),
             subtitle = paste(year),
             fill = "log2 of\nmale /\nfemale") +
        scale_fill_gradient2(low = "blue", midpoint = 0, mid = "yellow", high = "red", na.value = "grey30")

    } else if (input$sex == "F") {

      fem_table_map <- dataset_fem[which(dataset_fem$Indicator.Name == measure), c(1:3, which(colnames(dataset_fem) == paste0("X", year)))]
      colnames(fem_table_map)[ncol(fem_table_map)] <- "value"

      map_fem <- left_join(subset(wmap_countries_df_final, !continent == "Antarctica"), fem_table_map, by = c("gu_a3" = "Country.Code"))

      ggplot(map_fem, aes(long, lat, group = group, fill = value)) +
        map_theme +
        geom_polygon() +
        geom_path(color = "white", size = 0.5) +
        labs(title = paste(measure),
             subtitle = paste(year),
             fill = "Value") +
        scale_fill_gradientn(colours = colfunc(100), na.value = "grey30")

    } else if (input$sex == "M") {

      male_table_map <- dataset_male[which(dataset_male$Indicator.Name == measure), c(1:3, which(colnames(dataset_male) == paste0("X", year)))]
      colnames(male_table_map)[ncol(male_table_map)] <- "value"

      map_male <- left_join(subset(wmap_countries_df_final, !continent == "Antarctica"), male_table_map, by = c("gu_a3" = "Country.Code"))

      ggplot(map_male, aes(long, lat, group = group, fill = value)) +
        map_theme +
        geom_polygon() +
        geom_path(color = "white", size = 0.5) +
        labs(title = paste(measure),
             subtitle = paste(year),
             fill = "Value") +
        scale_fill_gradientn(colours = colfunc(100), na.value = "grey30")

    }
  }, height = function() {
    0.6 * session$clientData$output_map_width
  })


  # Country table

  output$info <- renderTable({
    table <- nearPoints(wmap_countries_df_final[, c(1, 2, 26, 42, 47, 48, 63)], input$plot_click, xvar = "long", yvar = "lat", maxpoints = 1, threshold = 100)

    colnames(table) <- c("longitude", "latitude", "country", "population est.", "economy", "income group", "region")

    table
  })


  # Last values

  output$last_vals <- renderPlot({

    measure <- input$measure

    map <- left_join(subset(wmap_countries_df_final, !continent == "Antarctica"), subset(year_table_last_val, Indicator.Name == measure), by = c("adm0_a3" = "Country.Code"))

    ggplot(map, aes(long, lat, group = group, fill = log2(unlist.last_val.))) +
      coord_equal() +
      map_theme +
      geom_polygon() +
      geom_path(color = "white", size = 0.5) +
      labs(title = paste(measure),
           subtitle = "2014 or 2015",
           fill = "log2 of male / female") +
      scale_fill_gradient2(low = "blue", midpoint = 0, mid = "yellow", high = "red", na.value = "grey30")
  }, height = function() {
    0.6 * session$clientData$output_map_width
  })

  output$bias1 <- renderTable({

    measure <- input$measure

    subs <- subset(year_table_last_val, Indicator.Name == measure)

    table <- arrange(subs, desc(log2(unlist.last_val.)))[1:10, -c(2, 3, 5)]
    table$unlist.last_val. <- log2(table$unlist.last_val.)
    table$na.omit.first_val. <- log2(table$na.omit.first_val.)
    table$difference <- log2(table$difference)

    colnames(table) <- c("Country name", "Last value", "First value", "Difference")
    table
  })

  output$bias2 <- renderTable({

    measure <- input$measure

    subs <- subset(year_table_last_val, Indicator.Name == measure)

    table <- arrange(subs, log2(unlist.last_val.))[1:10, -c(2, 3, 5)]
    table$unlist.last_val. <- log2(table$unlist.last_val.)
    table$na.omit.first_val. <- log2(table$na.omit.first_val.)
    table$difference <- log2(table$difference)

    colnames(table) <- c("Country name", "Last value", "First value", "Difference")
    table
  })

  output$diff1 <- renderTable({

    measure <- input$measure

    subs <- subset(year_table_last_val, Indicator.Name == measure)

    table <- arrange(subs, desc(log2(difference)))[1:10, -c(2, 3, 5)]
    table$unlist.last_val. <- log2(table$unlist.last_val.)
    table$na.omit.first_val. <- log2(table$na.omit.first_val.)
    table$difference <- log2(table$difference)

    colnames(table) <- c("Country name", "Last value", "First value", "Difference")
    table
  })

  output$diff2 <- renderTable({

    measure <- input$measure

    subs <- subset(year_table_last_val, Indicator.Name == measure)

    table <- arrange(subs, log2(difference))[1:10, -c(2, 3, 5)]
    table$unlist.last_val. <- log2(table$unlist.last_val.)
    table$na.omit.first_val. <- log2(table$na.omit.first_val.)
    table$difference <- log2(table$difference)

    colnames(table) <- c("Country name", "Last value", "First value", "Difference")
    table
  })


  # Timelines

  output$timeline <- renderPlot({

    measure <- input$measure
    country <- input$country

    diff_table_timeline <- rbind(dataset_fem, dataset_male) %>%
      subset(Indicator.Name == measure) %>%
      subset(Country.Code %in% wmap_countries_df_final$adm0_a3) %>%
      subset(as.character(Country.Name) == country)

    diff_table_timeline_gather <- gather(diff_table_timeline, year, value, X1960:X2015)
    diff_table_timeline_gather$year <- gsub("X", "", diff_table_timeline_gather$year)

    ggplot(diff_table_timeline_gather, aes(x = year, y = value, color = gender, group = gender)) +
      geom_line(size = 1, alpha = 0.7) +
      geom_point(size = 2, alpha = 0.7) +
      my_theme() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      scale_color_brewer(palette = "Set1") +
      labs(title = paste(measure),
           subtitle = paste(country),
           x = "Year",
           y = "Value",
           color = "Gender")
  }, height = function() {
    0.4 * session$clientData$output_timeline_width
  })

  output$timeline2 <- renderPlot({

    measure <- input$measure
    country <- input$country

    diff_table_timeline <- rbind(dataset_fem, dataset_male) %>%
      subset(Indicator.Name == measure) %>%
      subset(Country.Code %in% wmap_countries_df_final$gu_a3) %>%
      subset(as.character(Country.Name) == country)

    diff_table_timeline_2 <- cbind(diff_table_timeline[, c(1:4, 62)], t(apply(diff_table_timeline[, grep("^X[0-9]+$", colnames(diff_table_timeline))], 1, function(x)
      diff(na.omit(x))
    )))

    if (ncol(diff_table_timeline_2[, grep("^X[0-9]+$", colnames(diff_table_timeline_2))]) > 0) {
      diff_table_timeline_gather <- gather(diff_table_timeline_2, year, value, -Country.Name, -Country.Code, -Indicator.Name, -Indicator.Code, -gender)
      diff_table_timeline_gather$year <- gsub("X", "", diff_table_timeline_gather$year)

      ggplot(diff_table_timeline_gather, aes(x = year, y = value, color = gender, group = gender)) +
        geom_line(size = 1, alpha = 0.7) +
        geom_point(size = 2, alpha = 0.7) +
        my_theme() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        scale_color_brewer(palette = "Set1") +
        labs(title = paste("Change in", measure),
             subtitle = paste(country),
             x = "Year",
             y = "Value",
             color = "Gender")
    }
  }, height = function() {
    0.4 * session$clientData$output_timeline_width
  })


  # Country information

  output$income <- renderPlot({
    ggplot(subset(wmap_countries_df_final, !continent == "Antarctica"), aes(long, lat, group = group, fill = income_grp)) +
      coord_equal() +
      map_theme +
      geom_polygon() +
      geom_path(color = "white", size = 0.5) +
      labs(title = "Income group",
           fill = "")
  }, height = function() {
    0.6 * session$clientData$output_income_width
  })

  output$economy <- renderPlot({
    ggplot(subset(wmap_countries_df_final, !continent == "Antarctica"), aes(long, lat, group = group, fill = economy)) +
      coord_equal() +
      map_theme +
      geom_polygon() +
      geom_path(color = "white", size = 0.5) +
      labs(title = "Economy",
           fill = "")
  }, height = function() {
    0.6 * session$clientData$output_economy_width
  })

  output$population <- renderPlot({
    ggplot(subset(wmap_countries_df_final, !continent == "Antarctica"), aes(long, lat, group = group, fill = pop_est)) +
      coord_equal() +
      map_theme +
      geom_polygon() +
      geom_path(color = "white", size = 0.5) +
      labs(title = "Population",
           fill = "") +
      scale_fill_gradient2(low = "blue", mid = "blue", high = "red", na.value = "grey30")
  }, height = function() {
    0.6 * session$clientData$output_population_width
  })

  output$gdp <- renderPlot({
    ggplot(subset(wmap_countries_df_final, !continent == "Antarctica"), aes(long, lat, group = group, fill = gdp_md_est)) +
      coord_equal() +
      map_theme +
      geom_polygon() +
      geom_path(color = "white", size = 0.5) +
      labs(title = "Gross Domestic Product (GDP)",
           fill = "") +
      scale_fill_gradient2(low = "blue", mid = "blue", high = "red", na.value = "grey30")
  }, height = function() {
    0.6 * session$clientData$output_gdp_width
  })


  ## Statistics
  ### Plots

  output$density <- renderPlot({

    measure <- input$measure

    stats_test <- left_join(subset(year_table_last_val, Indicator.Name == measure), country.inf, by = c("Country.Code" = "adm0_a3"))

    stats_test %>%
      rename(last_value = unlist.last_val.) %>%
      rename(first_value = na.omit.first_val.) %>%
      gather(x, y, last_value, first_value) %>%
      ggplot(aes(x = log2(y), color = x, fill = x)) +
      geom_vline(xintercept = 0, color = "red", size = 2, alpha = 0.3) +
      my_theme() +
      geom_density(alpha = 0.3) +
      labs(title = paste(measure),
           x = "log2 of male / female",
           y = "Density",
           color = "",
           fill = "") +
      scale_color_brewer(palette = "Set1")
  })

  output$distribution1 <- renderPlot({

    measure <- input$measure

    stats_test <- left_join(subset(year_table_last_val, Indicator.Name == measure), country.inf, by = c("Country.Code" = "adm0_a3"))

    labels <- c(na.omit.first_val. = "first value", unlist.last_val. = "last value")

    stats_test %>%
      gather(x, y, unlist.last_val., na.omit.first_val.) %>%
      ggplot(aes(x = economy, y = log2(y), fill = economy, color = economy)) +
      geom_hline(yintercept = 0, color = "red", size = 2, alpha = 0.3) +
      geom_boxplot() +
      geom_violin(alpha = 0.5) +
      my_theme() +
      coord_flip() +
      scale_color_hue(l = 50) +
      guides(fill=FALSE, color = FALSE) +
      labs(title = paste(measure),
           y = "log2 of male / female",
           x = "Economy",
           color = "",
           fill = "") +
      facet_grid(~ x, scales = "free", labeller = labeller(x = labels))
  })

  output$distribution2 <- renderPlot({

    measure <- input$measure

    stats_test <- left_join(subset(year_table_last_val, Indicator.Name == measure), country.inf, by = c("Country.Code" = "adm0_a3"))

    labels <- c(na.omit.first_val. = "first value", unlist.last_val. = "last value")

    stats_test %>%
      gather(x, y, unlist.last_val., na.omit.first_val.) %>%
      ggplot(aes(x = income_grp, y = log2(y), fill = income_grp, color = income_grp)) +
      geom_hline(yintercept = 0, color = "red", size = 2, alpha = 0.3) +
      geom_boxplot() +
      geom_violin(alpha = 0.5) +
      my_theme() +
      coord_flip() +
      scale_color_hue(l = 50) +
      guides(fill = FALSE, color = FALSE) +
      labs(title = paste(measure),
           y = "log2 of male / female",
           x = "Income group") +
      facet_grid(~ x, scales = "free", labeller = labeller(x = labels))
  })

  output$distribution3 <- renderPlot({

    measure <- input$measure

    stats_test <- left_join(subset(year_table_last_val, Indicator.Name == measure), country.inf, by = c("Country.Code" = "adm0_a3"))

    labels <- c(na.omit.first_val. = "first value", unlist.last_val. = "last value")

    stats_test %>%
      gather(x, y, unlist.last_val., na.omit.first_val.) %>%
      ggplot(aes(x = continent, y = log2(y), fill = continent, color = continent)) +
      geom_hline(yintercept = 0, color = "red", size = 2, alpha = 0.3) +
      geom_boxplot() +
      geom_violin(alpha = 0.5) +
      my_theme() +
      coord_flip() +
      scale_color_hue(l = 50) +
      guides(fill = FALSE, color = FALSE) +
      labs(title = paste(measure),
           y = "log2 of male / female",
           x = "Continent") +
      facet_grid(~ x, scales = "free", labeller = labeller(x = labels))
  })

  output$cor1 <- renderPlot({

    measure <- input$measure

    stats_test <- left_join(subset(year_table_last_val, Indicator.Name == measure), country.inf, by = c("Country.Code" = "adm0_a3"))

    stats_test %>%
      ggplot(aes(x = log2(unlist.last_val.), y = pop_est)) +
      geom_vline(xintercept = 0, color = "red", size = 2, alpha = 0.3) +
      geom_smooth() +
      geom_point(alpha = 0.5, color = "navyblue", size = 2) +
      my_theme() +
      labs(title = paste(measure),
           x = "log2 of male / female",
           y = "Population estimate")
  })

  output$cor2 <- renderPlot({

    measure <- input$measure

    stats_test <- left_join(subset(year_table_last_val, Indicator.Name == measure), country.inf, by = c("Country.Code" = "adm0_a3"))

    stats_test %>%
      ggplot(aes(x = log2(unlist.last_val.), y = gdp_md_est)) +
      geom_vline(xintercept = 0, color = "red", size = 2, alpha = 0.3) +
      geom_smooth() +
      geom_point(alpha = 0.5, color = "navyblue", size = 2) +
      my_theme() +
      labs(title = paste(measure),
           x = "log2 of male / female",
           y = "Gross Domestic Product")
  })


  ## Tests

  output$wilcox <- renderTable({
    measure <- input$measure

    stats_test <- left_join(subset(year_table_last_val, Indicator.Name == measure), country.inf, by = c("Country.Code" = "adm0_a3"))

    table <- wilcox.test(log2(stats_test$na.omit.first_val.), log2(stats_test$unlist.last_val.), paired=TRUE)
    str(table)

    table <- data.frame(row.names = table$method,
                        V = table$statistic,
                        p.value = table$p.value,
                        alternative = table$alternative)

    table
  })

  output$kruskal <- renderTable({

    measure <- input$measure

    stats_test <- left_join(subset(year_table_last_val, Indicator.Name == measure), country.inf, by = c("Country.Code" = "adm0_a3"))

    stats_test$economy_rank <- as.numeric(as.character(gsub("(^[1-7])(.*)", "\\1", stats_test$economy)))
    stats_test$income_grp_rank <- as.numeric(as.character(gsub("(^[1-5])(.*)", "\\1", stats_test$income_grp)))

    k_economy_rank <- kruskal.test(log2(unlist.last_val.) ~ economy_rank, data = stats_test)
    k_income_rank <- kruskal.test(log2(unlist.last_val.) ~ income_grp_rank, data = stats_test)
    k_pop <- kruskal.test(log2(unlist.last_val.) ~ pop_est, data = stats_test)
    k_gdp <- kruskal.test(log2(unlist.last_val.) ~ gdp_md_est, data = stats_test)
    k_continent <- kruskal.test(log2(unlist.last_val.) ~ continent, data = stats_test)

    kruskal_last_val <- data.frame(
      group = c("economy (rank)", "income_grp (rank)", "pop_est", "gdp_md_est", "continent"),
      p.val = c(k_economy_rank$p.value, k_income_rank$p.value, k_pop$p.value, k_gdp$p.value, k_continent$p.value))

    kruskal_last_val$p.adj <- p.adjust(kruskal_last_val$p.val, method = "fdr")
    kruskal_last_val$significance <- ifelse(kruskal_last_val$p.adj < 0.1, "significant", "")
    kruskal_last_val
  })

  output$aovSummary = renderTable({

    measure <- input$measure

    stats_test <- left_join(subset(year_table_last_val, Indicator.Name == measure), country.inf, by = c("Country.Code" = "adm0_a3"))

    stats_test$economy_rank <- as.numeric(as.character(gsub("(^[1-7])(.*)", "\\1", stats_test$economy)))
    stats_test$income_grp_rank <- as.numeric(as.character(gsub("(^[1-5])(.*)", "\\1", stats_test$income_grp)))

    mod <- lm(log2(unlist.last_val.) ~ gdp_md_est * pop_est * economy_rank * income_grp_rank * continent, data = stats_test)
    anova <- data.frame(Variables = rownames(anova(mod)),
                        anova(mod))
    anova$significance <- ifelse(anova$Pr..F. < 0.05, "significant",
                                ifelse(anova$Pr..F. < 0.1, "trend", ""))
    anova
  })
})
