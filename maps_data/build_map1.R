# Builds maps/widgets/distorted_democracies_map.html from
# maps_data/distorted_democracies.csv (country-chamber averages) and
# maps_data/distorted_democracies_series.csv (country-chamber-year series).
# Interactive choropleth with a metric selector (Malapportionment / MRB / TRB /
# District magnitude) and a chamber selector (Lower / Upper), country search,
# click-to-zoom, and a per-country line chart of the metric's evolution by
# election year.
# Requires: plotly, htmltools, jsonlite.

suppressPackageStartupMessages({
  library(plotly)
  library(htmltools)
  library(jsonlite)
})

args <- commandArgs(trailingOnly = FALSE)
here <- normalizePath(dirname(sub("--file=", "", args[grep("--file=", args)])))
setwd(here)

dat <- read.csv("distorted_democracies.csv", stringsAsFactors = FALSE)
ser <- read.csv("distorted_democracies_series.csv", stringsAsFactors = FALSE)

metrics <- list(
  MAL2 = list(label = "Malapportionment"),
  MRB  = list(label = "MRB — malapportionment's share of bias"),
  TRB  = list(label = "TRB — total representation bias"),
  DM   = list(label = "District magnitude (avg. seats per district)")
)
chambers <- c("lower", "upper")

# Choropleth fill: country-chamber AVERAGE across all available years
map_data <- list()
for (ch in chambers) {
  sub <- dat[dat$chamber == ch & !is.na(dat$iso3), ]
  entry <- list()
  for (m in names(metrics)) {
    ok <- !is.na(sub[[m]])
    entry[[m]] <- list(
      iso3 = as.list(sub$iso3[ok]),
      z = as.list(round(sub[[m]][ok], 4)),
      ctr = as.list(sub$ctr[ok])
    )
  }
  map_data[[ch]] <- entry
}

# Per-country-chamber-year series, for the click-through line chart
series_data <- list()
for (ch in chambers) {
  sub <- ser[ser$chamber == ch, ]
  by_iso <- split(sub, sub$iso3)
  series_data[[ch]] <- lapply(by_iso, function(d) {
    d <- d[order(d$year), ]
    list(
      year = as.list(d$year),
      MAL2 = as.list(round(d$MAL2, 4)),
      MRB  = as.list(round(d$MRB, 4)),
      TRB  = as.list(round(d$TRB, 4)),
      DM   = as.list(round(d$DM, 4))
    )
  })
}

init <- map_data[["lower"]][["MAL2"]]

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
    font = list(color = "#444444")
  ) %>%
  config(responsive = TRUE) %>%
  htmlwidgets::onRender("
    function(el, x) {
      el.classList.add('repbias-geo-plot');
      el.on('plotly_click', function(d) {
        if (!d || !d.points || !d.points.length) return;
        var iso3 = d.points[0].location;
        if (iso3) { repbiasMap1SelectCountry(iso3, el); }
      });
      var loader = document.getElementById('map1-loading');
      if (loader) loader.style.display = 'none';
    }
  ")

controls <- tags$div(
  style = "display:flex; align-items:center; gap:0.75rem; margin-bottom:8px; flex-wrap:wrap; font-family:system-ui,sans-serif;",
  tags$label(style="font-size:0.82rem; font-weight:600;", "Metric:",
    tags$select(id = "map1-metric", style = "margin-left:6px; padding:3px 6px; border-radius:6px;",
      tags$option(value = "MAL2", "Malapportionment"),
      tags$option(value = "MRB", "MRB — malapportionment's share of bias"),
      tags$option(value = "TRB", "TRB — total representation bias"),
      tags$option(value = "DM", "District magnitude")
    )
  ),
  tags$label(style="font-size:0.82rem; font-weight:600;", "Chamber:",
    tags$select(id = "map1-chamber", style = "margin-left:6px; padding:3px 6px; border-radius:6px;",
      tags$option(value = "lower", "Lower / single chamber"),
      tags$option(value = "upper", "Upper chamber")
    )
  ),
  tags$input(
    id = "map1-country-search", list = "map1-country-search-list", type = "search",
    placeholder = "Search a country and press Enter…",
    onkeydown = "if (event.key === 'Enter') repbiasMap1SearchSubmit();",
    style = "padding:4px 8px; border-radius:6px; border:1px solid #99999966; min-width:220px; background:transparent; color:inherit;"
  ),
  tags$datalist(id = "map1-country-search-list")
)

caption <- tags$p(
  id = "map1-caption",
  style = "font-size:0.78rem; opacity:0.7; margin:0 0 8px; font-family:system-ui,sans-serif;",
  "Map shows the average across all available election years. Click a country for its evolution by legislature. Malapportionment: Samuels & Snyder (2001) index. MRB/TRB: positive = rightward bias, negative = leftward bias. Grey = no data. Data: Beramendi, Boix, Guinjoan & Rogers, “Distorted Democracies”."
)

popup <- tags$div(
  id = "map1-popup",
  style = paste(
    "display:none; position:absolute; inset:0; z-index:50;",
    "background:rgba(255,255,255,0.92); border-radius:10px;",
    "padding:1.25rem; box-sizing:border-box; font-family:system-ui,sans-serif; overflow:auto;"
  ),
  tags$button("✕", onclick = "repbiasMap1ClosePopup()",
    style = "position:absolute; top:10px; right:14px; background:none; border:none; font-size:1.3rem; line-height:1; cursor:pointer; color:#333; z-index:2;"),
  tags$h4(id = "map1-popup-title", style = "margin:0 0 0.4rem; color:#222;"),
  tags$p(id = "map1-popup-detail", style = "font-size:0.95rem; color:#333;"),
  tags$div(id = "map1-popup-chart", style = "width:100%; height:230px; margin-top:0.5rem;")
)

loading_overlay <- tags$div(
  id = "map1-loading",
  style = paste(
    "position:absolute; inset:0; z-index:60; display:flex; align-items:center;",
    "justify-content:center; background:rgba(255,255,255,0.6); font-family:system-ui,sans-serif;",
    "font-size:0.9rem; color:#444;"
  ),
  "Loading map…"
)

script <- tags$script(HTML(paste0("
var repbiasMap1Data = ", jsonlite::toJSON(map_data, auto_unbox = TRUE, null = 'null'), ";
var repbiasMap1Series = ", jsonlite::toJSON(series_data, auto_unbox = TRUE, null = 'null'), ";
var repbiasMap1Metrics = ", jsonlite::toJSON(lapply(metrics, function(m) m$label), auto_unbox = TRUE), ";
var repbiasMap1Selected = null;
var repbiasMap1ColorScales = {
  MAL2: [[0, '#ccece6'], [1, '#00441b']],
  MRB:  [[0, '#b2182b'], [0.5, '#f7f7f7'], [1, '#2166ac']],
  TRB:  [[0, '#b2182b'], [0.5, '#f7f7f7'], [1, '#2166ac']],
  DM:   [[0, '#fff5eb'], [1, '#8c2d04']]
};

function repbiasMap1El() { return document.querySelector('.repbias-geo-plot'); }

function repbiasMap1CurrentData() {
  var chamber = document.getElementById('map1-chamber').value;
  var metric = document.getElementById('map1-metric').value;
  return { chamber: chamber, metric: metric, d: repbiasMap1Data[chamber][metric] };
}

function repbiasMap1Redraw() {
  var cur = repbiasMap1CurrentData();
  var el = repbiasMap1El();
  if (!el || !cur.d) return;
  var zvals = cur.d.z.map(Number);
  var maxAbs = Math.max.apply(null, zvals.map(Math.abs).concat([0.001]));
  var isDiverging = (cur.metric === 'MRB' || cur.metric === 'TRB');
  var update = {
    locations: [cur.d.iso3],
    z: [zvals],
    text: [cur.d.ctr],
    customdata: [cur.d.ctr],
    colorscale: [repbiasMap1ColorScales[cur.metric]],
    'marker.line.width': [0.5]
  };
  if (isDiverging) { update.zmin = [-maxAbs]; update.zmax = [maxAbs]; }
  else { update.zmin = [0]; update.zmax = [Math.max.apply(null, zvals.concat([0.01]))]; }
  Plotly.restyle(el, update, [0]);

  var dl = document.getElementById('map1-country-search-list');
  dl.innerHTML = '';
  cur.d.ctr.slice().sort().forEach(function(n) {
    var opt = document.createElement('option'); opt.value = n; dl.appendChild(opt);
  });
  if (repbiasMap1Selected) repbiasMap1DrawChart(repbiasMap1Selected);
}

function repbiasMap1DrawChart(iso3) {
  var cur = repbiasMap1CurrentData();
  var s = (repbiasMap1Series[cur.chamber] || {})[iso3];
  var chartEl = document.getElementById('map1-popup-chart');
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
    xaxis: { title: 'Election year', dtick: 'auto', tickformat: 'd' },
    yaxis: { title: repbiasMap1Metrics[cur.metric] }
  }, { displayModeBar: false, responsive: true });
}

function repbiasMap1SelectCountry(iso3) {
  if (repbiasMap1Selected === iso3) { repbiasMap1ClosePopup(); return; }
  repbiasMap1Selected = iso3;
  var cur = repbiasMap1CurrentData();
  var idx = cur.d.iso3.indexOf(iso3);
  var el = repbiasMap1El();
  if (el && idx > -1) { Plotly.restyle(el, { locations: [[iso3]], z: [[1]] }, [1]); }
  var name = idx > -1 ? cur.d.ctr[idx] : iso3;
  var val = idx > -1 ? cur.d.z[idx] : null;
  document.getElementById('map1-popup-title').textContent = name;
  document.getElementById('map1-popup-detail').textContent = (val !== null)
    ? ('Average ' + repbiasMap1Metrics[cur.metric] + ': ' + Number(val).toFixed(3))
    : 'No data for this country in the current view.';
  document.getElementById('map1-popup').style.display = 'block';
  repbiasMap1DrawChart(iso3);
}

function repbiasMap1SearchSubmit() {
  var input = document.getElementById('map1-country-search');
  var name = input.value.trim();
  if (!name) return;
  var cur = repbiasMap1CurrentData();
  var idx = cur.d.ctr.findIndex(function(n) { return n.toLowerCase() === name.toLowerCase(); });
  if (idx === -1) return;
  repbiasMap1SelectCountry(cur.d.iso3[idx]);
  input.value = '';
}

function repbiasMap1ClosePopup() {
  repbiasMap1Selected = null;
  document.getElementById('map1-popup').style.display = 'none';
  document.getElementById('map1-popup-chart').innerHTML = '';
  var el = repbiasMap1El();
  if (el) Plotly.restyle(el, { locations: [[]], z: [[]] }, [1]);
}

document.addEventListener('DOMContentLoaded', function() {
  document.getElementById('map1-metric').addEventListener('change', repbiasMap1Redraw);
  document.getElementById('map1-chamber').addEventListener('change', function() { repbiasMap1ClosePopup(); repbiasMap1Redraw(); });
  repbiasMap1Redraw();
});
window.addEventListener('resize', function() {
  var el = repbiasMap1El();
  if (el) { try { Plotly.Plots.resize(el); } catch (e) {} }
});
")))

page <- tagList(
  tags$div(style = "font-family: system-ui, sans-serif; position:relative;",
    controls, caption,
    tags$div(style = "position:relative;", map_widget, popup, loading_overlay),
    script
  )
)

out_file <- "widgets/distorted_democracies_map.html"
dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
save_html(browsable(page), out_file, libdir = "lib")
cat("saved widget:", out_file, "\n")
