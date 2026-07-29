# ImmuteR — New Machine Setup

## 1. Install R and RStudio

- **R**: Download from [cran.r-project.org](https://cran.r-project.org/) — install the latest version (4.4+)
- **RStudio**: Download from [posit.co/download/rstudio-desktop](https://posit.co/download/rstudio-desktop/)

---

## 2. Open the Project

Open **RStudio**, then go to **File → Open Project** and select `ImmuteR.Rproj` from this folder. Always work from the project (not just a loose R file) so relative paths resolve correctly.

---

## 3. Install Dependencies

In the RStudio console, run:

```r
install.packages("devtools")
devtools::install_deps()
```

This reads `DESCRIPTION` and installs all required packages:
`googlesheets4`, `data.table`, `salesforcer`, `dplyr`, `stringr`, `reactable`, `htmltools`, `gt`, `scales`, `arrow`, `tidyr`, `purrr`, `rmarkdown`, `httr2`, and the suggested packages (`plumber`, `plotly`, etc.).

---

## 4. Set Up Your `.Renviron`

Create or edit your user-level `.Renviron` file:

```r
usethis::edit_r_environ()
```

Add your Asana token:

```
ASANA_TOKEN=your_token_here
```

Get a token at [app.asana.com/0/my-apps](https://app.asana.com/0/my-apps). Save the file and restart R (`Session → Restart R`).

---

## 5. Authenticate with Salesforce and Google

**Salesforce** — the cached token (`.httr-oauth-salesforcer`) in the project folder should carry over automatically. If it doesn't work, re-authenticate once:

```r
library(salesforcer)
sf_auth()
```

**Google Sheets** — Google tokens are stored in `~/.config/gargle/` on your old machine. You can copy that folder, or just re-authenticate once:

```r
library(googlesheets4)
gs4_auth(email = "matt.benigni@immuta.com")
```

A browser window will open — sign in and grant access. The token is then cached and you won't need to do this again.

---

## 6. Run It

**Interactive / one-off run** — open `scripts/main.R` and run it section by section (or source the whole file):

```r
devtools::load_all()
source("scripts/main.R")
```

**Dashboard with live refresh** — start the Plumber server first, then open the HTML dashboard:

```r
source("scripts/serve.R")   # keep this R session running
```

Then open the HTML file in `output/` and use the Refresh button to trigger a data run.

---

## Quick Checklist

- [ ] R 4.4+ installed
- [ ] RStudio installed
- [ ] Project opened via `ImmuteR.Rproj`
- [ ] `devtools::install_deps()` completed without errors
- [ ] `ASANA_TOKEN` added to `.Renviron`
- [ ] Salesforce auth working (`sf_auth()`)
- [ ] Google Sheets auth working (`gs4_auth()`)
- [ ] `source("scripts/main.R")` runs successfully
