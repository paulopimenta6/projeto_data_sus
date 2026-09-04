analysis_error <- function(error) {
  structure(
    list(message = conditionMessage(error), call = conditionCall(error)),
    class = c("datasus_analysis_error", "list")
  )
}

require_analysis_value <- function(analysis) {
  shiny::req(!is.null(analysis), cancelOutput = TRUE)
  shiny::req(!inherits(analysis, "datasus_analysis_error"), cancelOutput = TRUE)
  analysis
}

chart_theme <- function(compact = FALSE) {
  ggplot2::theme_minimal(base_size = if (compact) 10 else 12, base_family = "sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold", color = APP_COLORS$navy, size = if (compact) 12 else 15
      ),
      plot.subtitle = ggplot2::element_text(
        color = "#547079", size = if (compact) 8.5 else 10, lineheight = 1
      ),
      plot.caption = ggplot2::element_text(
        color = "#6C8087", hjust = 0, size = if (compact) 7.5 else 9
      ),
      axis.title = ggplot2::element_text(color = APP_COLORS$ink),
      axis.text = ggplot2::element_text(color = "#3F5962"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.title.position = if (compact) "plot" else "panel",
      plot.caption.position = if (compact) "plot" else "panel",
      plot.margin = if (compact) {
        ggplot2::margin(8, 8, 8, 8)
      } else {
        ggplot2::margin(10, 14, 10, 10)
      }
    )
}

make_series_plot <- function(analysis, compact = FALSE) {
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
    ggplot2::scale_x_discrete(
      breaks = function(values) {
        maximum_ticks <- if (compact) 5L else 12L
        step <- max(1L, ceiling(length(values) / maximum_ticks))
        values[seq.int(1L, length(values), by = step)]
      }
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(big.mark = ".", decimal.mark = ","),
      limits = if (valid_values <= 1L) c(0, NA) else NULL
    ) +
    ggplot2::labs(
      title = if (compact) "Evolução" else "Evolução temporal",
      subtitle = stringr::str_wrap(
        paste(
          analysis$metric_label,
          analysis$geography_semantics %||% "território da fonte",
          sep = " · "
        ),
        width = if (compact) 30 else 90
      ),
      x = NULL,
      y = analysis$metric_label,
      caption = stringr::str_wrap(
        "Fonte: DATASUS. Competências recentes podem ser preliminares.",
        width = if (compact) 34 else 90
      )
    ) +
    chart_theme(compact) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = if (compact || nrow(data) > 12L) 35 else 0,
        hjust = if (compact || nrow(data) > 12L) 1 else 0.5,
        size = if (compact) 7.5 else ggplot2::rel(0.8)
      ),
      axis.title.y = ggplot2::element_text(size = if (compact) 9 else ggplot2::rel(1))
    )
}

make_ranking_plot <- function(analysis, compact = FALSE) {
  data <- analysis$ranking
  data <- data[!is.na(data$display_value), , drop = FALSE]
  top_n <- as.integer(analysis$query$top_n %||% 15L)
  data <- utils::head(data, top_n)
  data$label_wrapped <- if (compact) {
    stringr::str_trunc(
      paste0(seq_len(nrow(data)), ". ", data$label),
      width = 16L
    )
  } else {
    stringr::str_wrap(data$label, width = 42L)
  }
  data$label_wrapped <- stats::reorder(data$label_wrapped, data$display_value)
  ranking_title <- if (analysis$ranking_kind == "condition") {
    if (compact) "Ranking" else "Ranking de condições"
  } else {
    "Ranking territorial"
  }

  ggplot2::ggplot(data, ggplot2::aes(x = .data$display_value, y = .data$label_wrapped)) +
    ggplot2::geom_col(fill = APP_COLORS$teal, width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = format_pt_number(
        .data$display_value,
        accuracy = if (compact) 1 else 0.1
      )),
      hjust = -0.08,
      color = APP_COLORS$ink,
      size = if (compact) 2.8 else 3.2
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::label_number(big.mark = ".", decimal.mark = ","),
      expand = ggplot2::expansion(mult = c(0, if (compact) 0.4 else 0.18))
    ) +
    ggplot2::labs(
      title = paste(ranking_title, "· Top", min(top_n, nrow(data))),
      subtitle = stringr::str_wrap(
        paste(analysis$metric_label, analysis$geography_semantics %||% "", sep = " · "),
        width = if (compact) 30 else 90
      ),
      x = if (compact) NULL else analysis$metric_label,
      y = NULL,
      caption = if (compact) "Rótulos integrais na tabela de dados." else NULL
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    chart_theme(compact) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(
        size = if (compact) 7.5 else 10,
        lineheight = if (compact) 1 else 0.95
      ),
      plot.margin = if (compact) {
        ggplot2::margin(10, 24, 10, 6)
      } else {
        ggplot2::margin(12, 34, 12, 12)
      }
    )
}

MAP_COLORS <- c("#D8F0EB", "#A9D9D4", "#65BAB5", "#2B9598", "#087E8B", "#0B2D3A")

classify_map_values <- function(values, method = "quantile", fixed_breaks = numeric(), bins = 6L) {
  method <- match.arg(method, c("quantile", "equal", "log", "fixed"))
  positive <- values[is.finite(values) & values > 0]
  if (length(positive) == 0L) {
    breaks <- c(0, 1)
  } else if (method == "fixed") {
    breaks <- sort(unique(as.numeric(fixed_breaks[is.finite(fixed_breaks)])))
    if (length(breaks) < 2L) stop("A classificação fixa exige ao menos dois limites.", call. = FALSE)
    breaks[[1L]] <- min(breaks[[1L]], min(positive))
    breaks[[length(breaks)]] <- max(breaks[[length(breaks)]], max(positive))
  } else if (length(unique(positive)) == 1L) {
    value <- unique(positive)
    breaks <- c(max(0, value * 0.99), value * 1.01)
  } else if (method == "quantile") {
    breaks <- unique(as.numeric(stats::quantile(
      positive, probs = seq(0, 1, length.out = bins + 1L), na.rm = TRUE, names = FALSE
    )))
  } else if (method == "equal") {
    breaks <- seq(min(positive), max(positive), length.out = bins + 1L)
  } else {
    breaks <- expm1(seq(log1p(min(positive)), log1p(max(positive)), length.out = bins + 1L))
  }
  if (length(breaks) < 2L || !all(is.finite(breaks))) breaks <- range(c(0, positive), finite = TRUE)
  list(
    method = method,
    method_label = c(
      quantile = "Quantis", equal = "Intervalos iguais",
      log = "Escala logarítmica", fixed = "Limites fixos"
    )[[method]],
    breaks = breaks,
    bins = length(breaks) - 1L,
    zero_count = sum(values == 0, na.rm = TRUE),
    missing_count = sum(!is.finite(values))
  )
}

map_palette <- function(values, classification = classify_map_values(values)) {
  colors <- grDevices::colorRampPalette(MAP_COLORS)(classification$bins)
  leaflet::colorBin(
    palette = colors,
    domain = values[is.finite(values) & values > 0],
    bins = classification$breaks,
    na.color = "#B8C3C5",
    pretty = FALSE,
    right = FALSE
  )
}

format_popup_value <- function(value, metric) {
  if (is.null(value) || length(value) == 0L || is.na(value)) return("Sem dado")
  format_pt_number(value, accuracy = if (metric %in% c("rate", "age_standardized_rate")) 0.1 else 1)
}

optional_popup_line <- function(label, values, index, metric = "count") {
  if (is.null(values) || length(values) < index || is.na(values[[index]])) return("")
  paste0("<br><span>", label, ": </span><strong>", format_popup_value(values[[index]], metric), "</strong>")
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
  classification <- analysis$classification %||% classify_map_values(data$display_value)
  palette <- map_palette(data$display_value, classification)
  fill_colors <- palette(data$display_value)
  fill_colors[!is.na(data$display_value) & data$display_value == 0] <- "#F5F7F7"
  comparisons <- analysis$comparisons %||% data.frame()
  comparison_index <- if (
    nrow(comparisons) > 0L && all(c("geo_code", "reference") %in% names(comparisons))
  ) {
    match(as.character(data$geo_code), as.character(comparisons$geo_code))
  } else {
    rep(NA_integer_, nrow(data))
  }
  popup <- vapply(seq_len(nrow(data)), function(index) {
    comparison_row <- comparison_index[[index]]
    comparison_lines <- if (is.na(comparison_row)) {
      ""
    } else {
      paste0(
        "<hr><span>Referência (", htmltools::htmlEscape(comparisons$reference[[comparison_row]]),
        "): </span><strong>",
        format_popup_value(comparisons$reference_value[[comparison_row]], analysis$metric),
        "</strong>",
        optional_popup_line(
          "Diferença", comparisons$difference, comparison_row, analysis$metric
        ),
        if (is.na(comparisons$ratio[[comparison_row]])) "" else paste0(
          "<br><span>Razão: </span><strong>",
          format_pt_number(comparisons$ratio[[comparison_row]], 0.01), " vez(es)</strong>"
        )
      )
    }
    paste0(
      "<div class='map-popup'><strong>", htmltools::htmlEscape(data$geo_name[[index]]), "</strong>",
      "<br>", htmltools::htmlEscape(analysis$metric_label), ": <strong>",
      format_popup_value(data$display_value[[index]], analysis$metric), "</strong>",
      optional_popup_line("Total", data$value, index),
      optional_popup_line("Denominador", data$denominator, index),
      optional_popup_line("Taxa bruta", data$rate, index, "rate"),
      optional_popup_line("IC95% bruto inferior", data$rate_low, index, "rate"),
      optional_popup_line("IC95% bruto superior", data$rate_high, index, "rate"),
      optional_popup_line("Taxa padronizada", data$age_standardized_rate, index, "age_standardized_rate"),
      optional_popup_line(
        "IC95% padronizado inferior", data$age_standardized_low, index, "age_standardized_rate"
      ),
      optional_popup_line(
        "IC95% padronizado superior", data$age_standardized_high, index, "age_standardized_rate"
      ),
      comparison_lines,
      "<hr><small>", htmltools::htmlEscape(analysis$geography_semantics %||% "Território da fonte"),
      "<br>Período: ", htmltools::htmlEscape(paste(analysis$query$periods$id, collapse = ", ")),
      "<br>Geometria: ", htmltools::htmlEscape(analysis$geometry_year %||% "indisponível"),
      "</small></div>"
    )
  }, character(1))

  widget <- leaflet::leaflet(data, options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
    leaflet::addProviderTiles(
      "OpenStreetMap.Mapnik",
      options = leaflet::providerTileOptions(noWrap = TRUE),
      group = "Mapa-base (requer internet)"
    ) |>
    leaflet::addPolygons(
      fillColor = fill_colors,
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
    )
  positive <- data$display_value[is.finite(data$display_value) & data$display_value > 0]
  if (length(positive) > 0L) {
    widget <- widget |>
      leaflet::addLegend(
        position = "bottomright",
        pal = palette,
        values = positive,
        title = htmltools::HTML(paste0(
          htmltools::htmlEscape(analysis$metric_label), "<br><small>",
          htmltools::htmlEscape(classification$method_label), "</small>"
        )),
        opacity = 0.9,
        labFormat = leaflet_number_format(
          if (analysis$metric %in% c("rate", "age_standardized_rate", "proportion")) 1L else 0L
        )
      )
  }
  status_colors <- character()
  status_labels <- character()
  if (classification$zero_count > 0L) {
    status_colors <- c(status_colors, "#F5F7F7")
    status_labels <- c(status_labels, paste("Zero (", classification$zero_count, ")"))
  }
  if (classification$missing_count > 0L) {
    status_colors <- c(status_colors, "#B8C3C5")
    status_labels <- c(status_labels, paste("Sem dado (", classification$missing_count, ")"))
  }
  if (length(status_colors) > 0L) {
    widget <- leaflet::addLegend(
      widget, position = "bottomleft", colors = status_colors,
      labels = status_labels, title = "Situação", opacity = 0.9
    )
  }
  widget
}

build_facility_map <- function(facilities) {
  data <- sf::st_transform(facilities, 4326)
  popup <- paste0(
    "<strong>", htmltools::htmlEscape(data$facility_name), "</strong>",
    if ("name_muni" %in% names(data)) paste0("<br>", htmltools::htmlEscape(data$name_muni)) else "",
    if ("co_cnes" %in% names(data)) paste0("<br>CNES: ", htmltools::htmlEscape(data$co_cnes)) else "",
    "<br>Competência: ", htmltools::htmlEscape(data$facility_date),
    "<br>Origem da coordenada: ", htmltools::htmlEscape(data$coordinate_source),
    "<br>Precisão: ", htmltools::htmlEscape(data$coordinate_precision),
    "<br><small>", htmltools::htmlEscape(data$coordinate_quality), "</small>"
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
  classification <- analysis$classification %||% classify_map_values(data$display_value)
  interval <- cut(
    data$display_value,
    breaks = classification$breaks,
    include.lowest = TRUE,
    right = FALSE
  )
  interval <- as.character(interval)
  interval[!is.na(data$display_value) & data$display_value == 0] <- "Zero"
  interval[is.na(data$display_value)] <- "Sem dado"
  positive_levels <- unique(interval[!interval %in% c("Zero", "Sem dado")])
  status_levels <- intersect(c("Zero", "Sem dado"), unique(interval))
  data$map_interval <- factor(interval, levels = c(positive_levels, status_levels))
  positive_colors <- if (length(positive_levels) == 0L) {
    character()
  } else {
    grDevices::colorRampPalette(MAP_COLORS)(length(positive_levels))
  }
  colors <- stats::setNames(
    c(positive_colors, c(Zero = "#F5F7F7", `Sem dado` = "#B8C3C5")[status_levels]),
    c(positive_levels, status_levels)
  )
  ggplot2::ggplot(data) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$map_interval), color = "white", linewidth = 0.08) +
    ggplot2::scale_fill_manual(
      values = colors,
      drop = FALSE,
      name = paste(analysis$metric_label, classification$method_label, sep = "\n")
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

make_comparison_plot <- function(analysis, compact = FALSE) {
  data <- analysis$comparisons
  data <- data[is.finite(data$value) & is.finite(data$reference_value), , drop = FALSE]
  data <- utils::head(data[order(data$value, decreasing = TRUE), , drop = FALSE], 15L)
  if (nrow(data) == 0L) return(NULL)
  comparison_labels <- if (compact) {
    stringr::str_trunc(
      paste0(seq_len(nrow(data)), ". ", data$geo_name),
      width = 16L
    )
  } else {
    stringr::str_wrap(data$geo_name, 38L)
  }
  data$geo_name <- stats::reorder(comparison_labels, data$value)
  ggplot2::ggplot(data, ggplot2::aes(.data$value, .data$geo_name)) +
    ggplot2::geom_vline(
      xintercept = unique(data$reference_value)[[1L]],
      color = APP_COLORS$coral,
      linewidth = 0.8,
      linetype = 2
    ) +
    ggplot2::geom_point(color = APP_COLORS$teal, size = 3) +
    ggplot2::labs(
      title = if (compact) "Comparação" else "Comparação territorial",
      subtitle = stringr::str_wrap(
        paste("Referência:", unique(data$reference)[[1L]], "·", analysis$metric_label),
        width = if (compact) 30 else 90
      ),
      x = if (compact) NULL else analysis$metric_label,
      y = NULL,
      caption = stringr::str_wrap(
        "Linha tracejada: referência calculada com os mesmos filtros.",
        width = if (compact) 34 else 90
      )
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::label_number(big.mark = ".", decimal.mark = ","),
      breaks = if (compact) scales::breaks_pretty(n = 3L) else ggplot2::waiver(),
      expand = ggplot2::expansion(mult = c(0.02, 0.04))
    ) +
    chart_theme(compact) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = if (compact) 7.5 else 10)
    )
}
