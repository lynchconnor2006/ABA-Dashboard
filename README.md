# ABA Scouting Dashboard

A multi-team Shiny web application for the American Basketball Association (ABA) that provides live-scraped analytics, game predictions, coaching tools, and pre-game scouting reports for every team in the league.

Built on top of [mystatsonline.com](https://www.mystatsonline.com) (League ID: 64225), the dashboard scrapes all team and player data on first run, caches it locally, and refreshes incrementally on subsequent launches.

---

## Features

| Tab | What it does |
|---|---|
| **League Overview** | Live standings race, top players/teams, playoff bracket (Monte Carlo simulation), league-wide trends |
| **Division** | Division standings, head-to-head comparison, divisional win % |
| **Team** | Stat boxes with percentile coloring, full roster, scoring breakdown, advanced metrics, team comparison |
| **Player** | Per-game stats, game log with trend arrows, scoring donut, impact rating with league rank, player comparison radar |
| **Coaching** | Interactive court visualization with bench swapping, four factors analysis, situational success prediction, win probability, player matchups with predicted stats |
| **Schedule** | Monthly calendar view with opponent difficulty ratings, win/loss predictions, key games watchlist, division games tracker, projected final record |
| **Scouting Report** | Full pre-game breakdown — key players, scoring distribution, tendencies, strategy points, head-to-head comparison, auto-generated game narrative |

---

## Requirements

### R version
R 4.1 or higher is recommended.

### R packages
Install all dependencies before running:

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "tidyverse",
  "rvest",
  "lubridate",
  "plotly",
  "DT",
  "future",
  "furrr",
  "parallelly"
))
```

---

## Getting Started

### 1. Clone the repository

```bash
git clone https://github.com/YOUR_USERNAME/aba-dashboard.git
cd aba-dashboard
```

### 2. Run the app

Open `ABA_Dashboard.R` in RStudio and click **Run App**, or run from the console:

```r
shiny::runApp("ABA_Dashboard.R")
```

### 3. First-run data scrape

On the very first launch the app will scrape all team rosters, player stats, game logs, and schedules from mystatsonline.com. **This takes approximately 15–20 minutes.** A progress indicator is shown in the UI.

Once complete, the data is saved to `aba_data_cache.rds` in the same directory as the script. Every subsequent launch loads from this cache in a few seconds, with an incremental update that checks for new scores and roster changes (no full re-scrape needed).

To force a full re-scrape at any time, click the **🔄 Refresh Data** button in the dashboard header.

---

## Login System

The dashboard uses a team-based login. Every ABA team in the league gets an account automatically generated from its name:

| Team name | Username | Password |
|---|---|---|
| Ohio Kings | `Ohio` | `Kings` |
| Nashville Aces | `Nashville` | `Aces` |
| Pittsburgh Ravens | `Pittsburgh` | `Ravens` |
| *(any team)* | First word | Last word |

Credentials are built dynamically from the scraped team list, so any team added to the league mid-season will automatically get a login without any code changes.

---

## Customization

### Adding a team color override

By default, teams are assigned colors from a pool based on a hash of their name. To pin specific brand colors for a team, add an entry to `team_color_overrides` near the top of `ABA_Dashboard.R`:

```r
team_color_overrides <- list(
  "Ohio Kings"      = list(primary = "#C8102E", secondary = "#041E42", accent = "#FDBB30"),
  "Your Team Name"  = list(primary = "#YOUR_HEX", secondary = "#YOUR_HEX", accent = "#YOUR_HEX")
)
```

### Changing the league

The app is built for ABA League ID `64225`. To point it at a different league on mystatsonline.com, replace `IDLeague=64225` throughout the scraping functions with your league's ID.

> **Note:** Two URLs also contain `IDSeason=104460` (the 2025–26 season ID). If you are using a different league or a future ABA season, you will need to update that value as well. The season ID can be found in the URL when browsing your league's team stats page on mystatsonline.com.

---

## Deploying to shinyapps.io

### Prerequisites
- A free or paid [shinyapps.io](https://www.shinyapps.io) account
- The `rsconnect` package: `install.packages("rsconnect")`
- Your account credentials configured: `rsconnect::setAccountInfo(name, token, secret)`

### Important: run locally first

**Generate the cache before deploying.** Run the app locally and let the full scrape complete so `aba_data_cache.rds` exists. Shinyapps.io free-tier instances have a 60-second startup time limit and will time out if they have to do a cold scrape on launch.

### Deploy

```r
rsconnect::deployApp(
  appDir       = "path/to/your/folder",
  appFiles     = c("ABA Dashboard.R", "aba_data_cache.rds"),
  appPrimaryDoc = "ABA Dashboard.R",
  appName      = "aba-dashboard",
  appTitle     = "ABA Dashboard",
  forceUpdate  = TRUE
)
```

Replace `path/to/your/folder` with the directory containing both files.

### Keeping data fresh on shinyapps.io

The deployed app can refresh its own cache at runtime using the Refresh button, but on a free-tier instance the cache does not persist between sessions (the container resets). For always-fresh data on free tier, re-deploy with a newly generated cache periodically. On paid tiers with persistent storage, the in-app refresh button works across sessions.

---

## Data Source

All data is scraped live from **[mystatsonline.com](https://www.mystatsonline.com)**, the official stats platform used by the American Basketball Association. No API key is required. Data availability depends on the league's stats being kept up to date by team scorekeepers.

---

## Project Structure

```
aba-dashboard/
├── ABA_Dashboard.R       # Single-file Shiny app (UI + server)
├── aba_data_cache.rds    # Auto-generated on first run, not committed to git
└── README.md
```

> `aba_data_cache.rds` is listed in `.gitignore` by default (or should be) since it is large and regenerated locally. Each user generates their own cache on first run.

---

## .gitignore recommendation

Add this to your `.gitignore` to avoid committing the large cache file:

```
aba_data_cache.rds
.Rhistory
.RData
.Rproj.user/
```

---

## License

This project is provided as-is for personal and organizational use within the ABA. Stats data is the property of mystatsonline.com and the American Basketball Association.
