analysis_error <- function(error) {
  structure(
    list(message = conditionMessage(error), call = conditionCall(error)),
    class = c("datasus_analysis_error", "list")
  )
}

require_analysis_value <- function(analysis) {
  shiny::validate(shiny::need(!is.null(analysis), "Configure os filtros e clique em Analisar."))
  shiny::validate(shiny::need(
    !inherits(analysis, "datasus_analysis_error"),
    if (inherits(analysis, "datasus_analysis_error")) analysis$message else ""
  ))
  analysis
}

chart_theme <- function() {
  ggplot2::theme_minimal(base_size = 12, base_family = "sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = APP_COLORS$navy, size = 15),
      plot.subtitle = ggplot2::element_text(color = "#547079", size = 10),
      plot.caption = ggplot2::element_text(color = "#6C8087", hjust = 0),
      axis.title = ggplot2::element_text(color = APP_COLORS$ink),
      axis.text = ggplot2::element_text(color = "#3F5962"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(10, 14, 10, 10)
    )
}

make_series_plot <- function(analysis) {
  data <- analysis$series
  data$period <- factor(data$label, levels = unique(data$label))
  valid_values <- sum(!is.na(data$display_value))
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data$period, y = .data$display_value, group = 1))
  if (valid_values > 1L) {
    plot <- plot +
      ggplot2::geom_line(color = APP_COLORS$teal, linewidth = 1.1, na.rm = TRUE) +
      ggplot2::geom_point(
        color = APP_COLORS$gold,
        fill = APP_COLORS$navy,
        shape = 21,
        size = 2.8,
        na.rm = TRUE
      )
  } else {
    plot <- plot + ggplot2::geom_col(fill = APP_COLORS$teal, width = 0.55, na.rm = TRUE)
  }
  plot +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(big.mark = ".", decimal.mark = ","),
      limits = if (valid_values <= 1L) c(0, NA) else NULL
    ) +
    ggplot2::labs(
      title = "Evolução no período selecionado",
      subtitle = analysis$metric_label,
      x = NULL,
      y = analysis$metric_label,
      caption = "Fonte: DATASUS. Competências recentes podem ser preliminares."
    ) +
    chart_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = if (nrow(data) > 8L) 45 else 0,
        hjust = if (nrow(data) > 8L) 1 else 0.5
      )
    )
}

make_ranking_plot <- function(analysis) {
  data <- analysis$ranking
  data <- data[!is.na(data$display_value), , drop = FALSE]
  data <- utils::head(data, 15L)
  data$label_wrapped <- stringr::str_wrap(data$label, width = 42)
  data$label_wrapped <- stats::reorder(data$label_wrapped, data$display_value)
  ranking_title <- if (analysis$ranking_kind == "condition") {
    "Principais condições, procedimentos ou categorias"
  } else {
    "Territórios com maiores valores"
  }

  ggplot2::ggplot(data, ggplot2::aes(x = .data$display_value, y = .data$label_wrapped)) +
    ggplot2::geom_col(fill = APP_COLORS$teal, width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = format_pt_number(.data$display_value, accuracy = 0.1)),
      hjust = -0.08,
      color = APP_COLORS$ink,
      size = 3.2
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::label_number(big.mark = ".", decimal.mark = ","),
      expand = ggplot2::expansion(mult = c(0, 0.18))
    ) +
    ggplot2::labs(
      title = ranking_title,
      subtitle = analysis$metric_label,
      x = analysis$metric_label,
      y = NULL
    ) +
    chart_theme()
}

map_palette <- function(values) {
  leaflet::colorNumeric(
    palette = c("#EAF5F3", "#A9D9D4", "#4AA8A6", "#087E8B", "#0B2D3A"),
    domain = values,
    na.color = "#D9E1E1"
  )
}

leaflet_number_format <- function(digits = 1L) {
  fallback <- leaflet::labelFormat()
  function(type, ...) {
    if (identical(type, "numeric")) {
      cuts <- list(...)[[1L]]
      return(format(
        round(cuts, digits),
        trim = TRUE,
        scientific = FALSE,
        big.mark = ".",
        decimal.mark = ","
      ))
    }
    fallback(type, ...)
  }
}

build_choropleth_map <- function(analysis) {
  data <- analysis$map_sf
  if (is.null(data) || !inherits(data, "sf")) return(NULL)
  data <- sf::st_transform(data, 4326)
  palette <- map_palette(data$display_value)
  popup <- paste0(
    "<strong>", htmltools::htmlEscape(data$geo_name), "</strong><br>",
    htmltools::htmlEscape(analysis$metric_label), ": ",
    ifelse(
      is.na(data$display_value),
      "Sem dado",
      format_pt_number(data$display_value, accuracy = if (analysis$metric == "rate") 0.1 else 1)
    )
  )

  leaflet::leaflet(data, options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
    leaflet::addProviderTiles("OpenStreetMap.Mapnik", options = leaflet::providerTileOptions(noWrap = TRUE)) |>
    leaflet::addPolygons(
      fillColor = ~palette(display_value),
      fillOpacity = 0.78,
      color = "#FFFFFF",
      weight = 0.65,
      opacity = 0.9,
      popup = popup,
      highlightOptions = leaflet::highlightOptions(
        weight = 2,
        color = APP_COLORS$gold,
        fillOpacity = 0.9,
        bringToFront = TRUE
      )
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      pal = palette,
      values = ~display_value,
      title = htmltools::htmlEscape(analysis$metric_label),
      opacity = 0.9,
      labFormat = leaflet_number_format(if (analysis$metric == "rate") 1L else 0L)
    )
}

build_facility_map <- function(facilities) {
  data <- sf::st_transform(facilities, 4326)
  popup <- paste0(
    "<strong>", htmltools::htmlEscape(data$facility_name), "</strong>",
    if ("name_muni" %in% names(data)) paste0("<br>", htmltools::htmlEscape(data$name_muni)) else "",
    if ("co_cnes" %in% names(data)) paste0("<br>CNES: ", htmltools::htmlEscape(data$co_cnes)) else ""
  )
  leaflet::leaflet(data, options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
    leaflet::addProviderTiles("OpenStreetMap.Mapnik") |>
    leaflet::addCircleMarkers(
      radius = 4,
      stroke = TRUE,
      weight = 1,
      color = "#FFFFFF",
      fillColor = APP_COLORS$coral,
      fillOpacity = 0.82,
      popup = popup,
      clusterOptions = leaflet::markerClusterOptions(
        showCoverageOnHover = FALSE,
        spiderfyOnMaxZoom = TRUE
      )
    )
}

make_static_map_plot <- function(analysis) {
  data <- analysis$map_sf
  ggplot2::ggplot(data) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$display_value), color = "white", linewidth = 0.08) +
    ggplot2::scale_fill_gradientn(
      colours = c("#EAF5F3", "#A9D9D4", "#4AA8A6", "#087E8B", "#0B2D3A"),
      na.value = "#D9E1E1",
      labels = scales::label_number(big.mark = ".", decimal.mark = ","),
      name = analysis$metric_label
    ) +
    ggplot2::labs(
      title = analysis$query$domain_label,
      subtitle = paste(analysis$metric_label, "· geometria", analysis$geometry_year),
      caption = "Fonte: DATASUS; limites territoriais: geobr/IBGE."
    ) +
    ggplot2::coord_sf(datum = NA) +
    chart_theme() +
    ggplot2::theme(axis.text = ggplot2::element_blank(), axis.title = ggplot2::element_blank())
}
