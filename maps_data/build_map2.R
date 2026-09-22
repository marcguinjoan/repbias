# Builds maps/widgets/ethnicity_map.html from maps_data/ethnicity.csv
# (country averages) and maps_data/ethnicity_series.csv (country-year series).
# Lower-chamber only, matching Map.R in the paper's replication package.
# Requires: plotly, htmltools, jsonlite.

suppressPackageStartupMessages({
  library(plotly)
  library(htmltools)
  library(jsonlite)
})

args <- commandArgs(trailingOnly = FALSE)
here <- normalizePath(dirname(sub("--file=", "", args[grep("--file=", args)])))
setwd(here)

dat <- read.csv("ethnicity.csv", stringsAsFactors = FALSE)
dat <- dat[!is.na(dat$iso3c) & dat$iso3c != "", ]
ser <- read.csv("ethnicity_series.csv", stringsAsFactors = FALSE)

metrics <- list(
  MAL2 = list(label = "Malapportionment"),
  ethn_scale = list(label = "Ethnic minority presence (share of districts)"),
  DM = list(label = "District magnitude (avg. seats per district)")
)

reserved <- dat[dat$reserved_seat == "True" | dat$reserved_seat == TRUE, ]

data_list <- list()
for (m in names(metrics)) {
  ok <- !is.na(dat[[m]])
  data_list[[m]] <- list(
    iso3 = as.list(dat$iso3c[ok]),
    z = as.list(round(dat[[m]][ok], 4)),
    ctr = as.list(dat$country[ok])
  )
}

by_iso <- split(ser, ser$iso3)
series_data <- lapply(by_iso, function(d) {
  d <- d[order(d$year), ]
  list(
    year = as.list(d$year),
    MAL2 = as.list(round(d$MAL2, 4)),
    ethn_scale = as.list(round(d$ethn_scale, 4)),
    DM = as.list(round(d$DM, 4))
  )
})

init <- data_list[["MAL2"]]

map_widget <- plot_ly(
  type = "choropleth",
  locations = unlist(init$iso3), z = unlist(init$z), text = unlist(init$ctr),
  locationmode = "ISO-3",
  colorscale = list(list(0, "#ccece6"), list(1, "#00441b")),
  zmin = 0, zauto = FALSE,
  customdata = unlist(init$ctr),
  hovertemplate = "<b>%{customdata}</b><br>%{z}<extra></extra>",
  marker = list(line = list(color = "rgba(150,150,150,0.6)", width = 0.5)),
  colorbar = list(title = "Malapportionment", len = 0.7)
) %>%
  add_trace(inherit = FALSE,
    type = "choropleth", locations = list(), z = list(),
    colorscale = list(list(0, "rgba(0,0,0,0)"), list(1, "rgba(0,0,0,0)")),
    showscale = FALSE, showlegend = FALSE, hoverinfo = "skip",
    marker = list(line = list(color = "#FF6B00", width = 3))
  ) %>%
  add_trace(inherit = FALSE,
    type = "scattergeo", locationmode = "ISO-3",
    locations = reserved$iso3c, text = paste0(reserved$country, " — reserved seats for minorities"),
    mode = "markers",
    marker = list(symbol = "x", size = 10, color = "#e03131", line = list(width = 2, color = "#e03131")),
    hovertemplate = "%{text}<extra></extra>",
    name = "Reserved seats", showlegend = TRUE
  ) %>%
  layout(
    geo = list(
      projection = list(type = "natural earth"),
      showframe = FALSE, showcoastlines = FALSE,
      showcountries = TRUE, countrycolor = "rgba(160,160,160,0.6)",
      showland = TRUE, landcolor = "rgba(210,210,210,0.35)",
      bgcolor = "rgba(0,0,0,0)"
    ),
    paper_bgcolor = "rgba(0,0,0,0)",
    margin = list(l = 0, r = 0, t = 10, b = 0),
    font = list(color = "#444444"),
    legend = list(x = 0.02, y = 0.02, bgcolor = "rgba(255,255,255,0.6)")
  ) %>%
  config(responsive = TRUE) %>%
  htmlwidgets::onRender("
    function(el, x) {
      el.classList.add('repbias-geo-plot-2');
      el.on('plotly_click', function(d) {
        if (!d || !d.points || !d.points.length) return;
        var iso3 = d.points[0].location;
        if (iso3) { repbiasMap2SelectCountry(iso3, el); }
      });
      var loader = document.getElementById('map2-loading');
      if (loader) loader.style.display = 'none';
    }
  ")

controls <- tags$div(
  style = "display:flex; align-items:center; gap:0.75rem; margin-bottom:8px; flex-wrap:wrap; font-family:system-ui,sans-serif;",
  tags$label(style="font-size:0.82rem; font-weight:600;", "Metric:",
    tags$select(id = "map2-metric", style = "margin-left:6px; padding:3px 6px; border-radius:6px;",
      tags$option(value = "MAL2", "Malapportionment"),
      tags$option(value = "ethn_scale", "Ethnic minority presence"),
      tags$option(value = "DM", "District magnitude")
    )
  ),
  tags$input(
    id = "map2-country-search", list = "map2-country-search-list", type = "search",
    placeholder = "Search a country and press Enter…",
    onkeydown = "if (event.key === 'Enter') repbiasMap2SearchSubmit();",
    style = "padding:4px 8px; border-radius:6px; border:1px solid #99999966; min-width:220px; background:transparent; color:inherit;"
  ),
  tags$datalist(id = "map2-country-search-list")
)

caption <- tags$p(
  style = "font-size:0.78rem; opacity:0.7; margin:0 0 8px; font-family:system-ui,sans-serif;",
  "Map shows the average across all available election years (lower chamber only). Click a country for its evolution by legislature. Red crosses mark countries with ethnic reserved seats. Grey = no data. Data: Guinjoan, Mas & Roura, “Electoral institutions, malapportionment and the representation of ethnic minorities”."
)

popup <- tags$div(
  id = "map2-popup",
  style = paste(
    "display:none; position:absolute; inset:0; z-index:50;",
    "background:rgba(255,255,255,0.92); border-radius:10px;",
    "padding:1.25rem; box-sizing:border-box; font-family:system-ui,sans-serif; overflow:auto;"
  ),
  tags$button("✕", onclick = "repbiasMap2ClosePopup()",
    style = "position:absolute; top:10px; right:14px; background:none; border:none; font-size:1.3rem; line-height:1; cursor:pointer; color:#333; z-index:2;"),
  tags$h4(id = "map2-popup-title", style = "margin:0 0 0.4rem; color:#222;"),
  tags$p(id = "map2-popup-detail", style = "font-size:0.95rem; color:#333;"),
  tags$div(id = "map2-popup-chart", style = "width:100%; height:230px; margin-top:0.5rem;")
)

loading_overlay <- tags$div(
  id = "map2-loading",
  style = paste(
    "position:absolute; inset:0; z-index:60; display:flex; align-items:center;",
    "justify-content:center; background:rgba(255,255,255,0.6); font-family:system-ui,sans-serif;",
    "font-size:0.9rem; color:#444;"
  ),
  "Loading map…"
)

script <- tags$script(HTML(paste0("
var repbiasMap2Data = ", jsonlite::toJSON(data_list, auto_unbox = TRUE, null = 'null'), ";
var repbiasMap2Series = ", jsonlite::toJSON(series_data, auto_unbox = TRUE, null = 'null'), ";
var repbiasMap2Metrics = ", jsonlite::toJSON(lapply(metrics, function(m) m$label), auto_unbox = TRUE), ";
var repbiasMap2Selected = null;
var repbiasMap2ColorScales = {
  MAL2: [[0, '#ccece6'], [1, '#00441b']],
  ethn_scale: [[0, '#fde725'], [0.5, '#21918c'], [1, '#440154']],
  DM: [[0, '#fff5eb'], [1, '#8c2d04']]
};

function repbiasMap2El() { return document.querySelector('.repbias-geo-plot-2'); }

function repbiasMap2Current() {
  var metric = document.getElementById('map2-metric').value;
  return { metric: metric, d: repbiasMap2Data[metric] };
}

function repbiasMap2Redraw() {
  var cur = repbiasMap2Current();
  var el = repbiasMap2El();
  if (!el || !cur.d) return;
  var zvals = cur.d.z.map(Number);
  var update = {
    locations: [cur.d.iso3],
    z: [zvals],
    text: [cur.d.ctr],
    customdata: [cur.d.ctr],
    colorscale: [repbiasMap2ColorScales[cur.metric]],
    zmin: [0],
    zmax: [Math.max.apply(null, zvals.concat([0.01]))]
  };
  Plotly.restyle(el, update, [0]);

  var dl = document.getElementById('map2-country-search-list');
  dl.innerHTML = '';
  cur.d.ctr.slice().sort().forEach(function(n) {
    var opt = document.createElement('option'); opt.value = n; dl.appendChild(opt);
  });
  if (repbiasMap2Selected) repbiasMap2DrawChart(repbiasMap2Selected);
}

function repbiasMap2DrawChart(iso3) {
  var cur = repbiasMap2Current();
  var s = repbiasMap2Series[iso3];
  var chartEl = document.getElementById('map2-popup-chart');
  if (!s || !s.year || s.year.length < 2) {
    chartEl.innerHTML = '<p style=\"font-size:0.82rem; opacity:0.65;\">Not enough election-year observations for a trend.</p>';
    return;
  }
  var yvals = s[cur.metric].map(function(v) { return v === null ? null : Number(v); });
  Plotly.newPlot(chartEl, [{
    x: s.year, y: yvals, type: 'scatter', mode: 'lines+markers',
    line: { color: '#4dabf7', width: 2 }, marker: { size: 6, color: '#4dabf7' },
    connectgaps: false
  }], {
    margin: { l: 40, r: 10, t: 8, b: 30 },
    paper_bgcolor: 'rgba(0,0,0,0)', plot_bgcolor: 'rgba(0,0,0,0)',
    font: { color: '#333', size: 11 },
    xaxis: { title: 'Election year', tickformat: 'd' },
    yaxis: { title: repbiasMap2Metrics[cur.metric] }
  }, { displayModeBar: false, responsive: true });
}

function repbiasMap2SelectCountry(iso3) {
  if (repbiasMap2Selected === iso3) { repbiasMap2ClosePopup(); return; }
  repbiasMap2Selected = iso3;
  var cur = repbiasMap2Current();
  var idx = cur.d.iso3.indexOf(iso3);
  var el = repbiasMap2El();
  if (el && idx > -1) { Plotly.restyle(el, { locations: [[iso3]], z: [[1]] }, [1]); }
  var name = idx > -1 ? cur.d.ctr[idx] : iso3;
  var val = idx > -1 ? cur.d.z[idx] : null;
  document.getElementById('map2-popup-title').textContent = name;
  document.getElementById('map2-popup-detail').textContent = (val !== null)
    ? ('Average ' + repbiasMap2Metrics[cur.metric] + ': ' + Number(val).toFixed(3))
    : 'No data for this country.';
  document.getElementById('map2-popup').style.display = 'block';
  repbiasMap2DrawChart(iso3);
}

function repbiasMap2SearchSubmit() {
  var input = document.getElementById('map2-country-search');
  var name = input.value.trim();
  if (!name) return;
  var cur = repbiasMap2Current();
  var idx = cur.d.ctr.findIndex(function(n) { return n.toLowerCase() === name.toLowerCase(); });
  if (idx === -1) return;
  repbiasMap2SelectCountry(cur.d.iso3[idx]);
  input.value = '';
}

function repbiasMap2ClosePopup() {
  repbiasMap2Selected = null;
  document.getElementById('map2-popup').style.display = 'none';
  document.getElementById('map2-popup-chart').innerHTML = '';
  var el = repbiasMap2El();
  if (el) Plotly.restyle(el, { locations: [[]], z: [[]] }, [1]);
}

document.addEventListener('DOMContentLoaded', function() {
  document.getElementById('map2-metric').addEventListener('change', repbiasMap2Redraw);
  repbiasMap2Redraw();
});
window.addEventListener('resize', function() {
  var el = repbiasMap2El();
  if (el) { try { Plotly.Plots.resize(el); } catch (e) {} }
});
")))

fill_css <- tags$style(HTML("
  html, body { height: 100%; margin: 0; }
  .repbias-map-page { display: flex; flex-direction: column; height: 100vh; font-family: system-ui, sans-serif; }
  .repbias-map-controls { flex: 0 0 auto; }
  .repbias-map-area { flex: 1 1 auto; min-height: 0; position: relative; }
  .repbias-map-area .repbias-geo-plot-2 { width: 100% !important; height: 100% !important; }
  .repbias-map-area .js-plotly-plot, .repbias-map-area .plot-container { width: 100% !important; height: 100% !important; }
"))

page <- tagList(
  fill_css,
  tags$div(class = "repbias-map-page",
    tags$div(class = "repbias-map-controls", controls, caption),
    tags$div(class = "repbias-map-area", map_widget, popup, loading_overlay),
    script
  )
)

out_file <- "widgets/ethnicity_map.html"
dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
save_html(browsable(page), out_file, libdir = "lib")
cat("saved widget:", out_file, "\n")
