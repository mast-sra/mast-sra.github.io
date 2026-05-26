# build_carousel.R
#
# Produces carousel_tabs.html — a Bootstrap tab strip with one carousel per
# year.  Drop images in images/datafestYYYY/ (jpg) and add the year to the
# `years` vector below.  Re-run the script (or let Quarto's pre-render hook
# call it) and then include the output in any .qmd with:
#
#   ```{=html}
#   {{< include carousel_tabs.html >}}
#   ```

years <- c(2026)   # <-- add future years here

# ── HEIC → JPEG conversion ────────────────────────────────────────────────────
# Converts any .HEIC/.heic files found in images/datafestYYYY/ folders.
# Requires either:
#   - sips   (built into macOS — no install needed)
#   - magick (ImageMagick); install with: brew install imagemagick (mac)
#                                         winget install ImageMagick (windows)
# Already-converted files are skipped (won't reconvert if .jpg exists).

convert_heic <- function(year) {
  folder <- file.path("images", paste0(year))
  if (!dir.exists(folder)) return(invisible(NULL))
  
  heic_files <- list.files(folder, pattern = "\\.HEIC$|\\.heic$",
                           full.names = TRUE, ignore.case = TRUE)
  if (length(heic_files) == 0) return(invisible(NULL))
  
  has_sips   <- nchar(Sys.which("sips"))       > 0
  has_magick <- nchar(Sys.which("magick"))     > 0
  has_ps     <- nchar(Sys.which("powershell")) > 0
  
  if (!has_sips && !has_magick && !has_ps) {
    warning(
      "HEIC files found in ", folder, " but no conversion tool is available.\n",
      "  macOS:   sips is built in (should always work)\n",
      "  Windows: install ImageMagick via `winget install ImageMagick` or\n",
      "           from https://imagemagick.org/script/download.php#windows\n",
      "           (tick 'Add to PATH' during install)\n",
      "  Manual:  convert files to .jpg before rendering.",
      call. = FALSE
    )
    return(invisible(NULL))
  }
  
  for (heic in heic_files) {
    jpg <- sub("\\.[Hh][Ee][Ii][Cc]$", ".jpg", heic)
    if (file.exists(jpg)) next   # already converted — skip
    
    ok <- if (has_sips) {
      system2("sips", args = c("-s", "format", "jpeg", shQuote(heic),
                               "--out", shQuote(jpg)),
              stdout = FALSE, stderr = FALSE) == 0
    } else if (has_magick) {
      system2("magick", args = c(shQuote(heic), shQuote(jpg)),
              stdout = FALSE, stderr = FALSE) == 0
    } else {
      # PowerShell fallback — uses Windows Imaging Component (no install needed)
      ps_cmd <- sprintf(
        '[System.Drawing.Image]::FromFile("%s").Save("%s", [System.Drawing.Imaging.ImageFormat]::Jpeg)',
        normalizePath(heic, winslash = "/"),
        normalizePath(jpg,  winslash = "/")
      )
      system2("powershell", args = c("-Command", shQuote(ps_cmd)),
              stdout = FALSE, stderr = FALSE) == 0
    }
    
    if (ok) {
      message("Converted: ", basename(heic), " -> ", basename(jpg))
    } else {
      warning("Conversion failed for: ", heic, call. = FALSE)
    }
  }
}

message("Checking for HEIC files to convert...")
invisible(lapply(years, convert_heic))

# ── helpers ───────────────────────────────────────────────────────────────────

make_carousel <- function(year) {
  folder <- file.path("images", paste0(year))
  imgs   <- sort(list.files(folder, pattern = "\\.jpg$", full.names = FALSE))
  id     <- paste0("carousel", year)
  
  if (length(imgs) == 0) {
    return(sprintf(
      '<p class="text-muted fst-italic">No photos available yet for %d.</p>',
      year
    ))
  }
  
  indicators <- paste(mapply(function(img, i) {
    active <- if (i == 0) 'class="active" aria-current="true" ' else ""
    sprintf(
      '<button type="button" data-bs-target="#%s" data-bs-slide-to="%d" %saria-label="Slide %d"></button>',
      id, i, active, i + 1
    )
  }, imgs, seq_along(imgs) - 1), collapse = "\n      ")
  
  items <- paste(mapply(function(img, i) {
    active <- if (i == 1) " active" else ""
    sprintf(
      '<div class="carousel-item%s">\n        <img src="%s/%s" class="d-block w-100" alt="DataFest %d">\n      </div>',
      active, folder, img, year
    )
  }, imgs, seq_along(imgs)), collapse = "\n      ")
  
  thumbnails <- paste(mapply(function(img, i) {
    active_class <- if (i == 0) " thumb-active" else ""
    sprintf(
      '<img src="%s/%s" class="carousel-thumb%s" data-bs-target="#%s" data-bs-slide-to="%d" alt="Thumbnail %d">',
      folder, img, active_class, id, i, i + 1
    )
  }, imgs, seq_along(imgs) - 1), collapse = "\n    ")
  
  sprintf('
    <div id="%s" class="carousel slide" data-bs-ride="carousel" data-bs-interval="4000">
      <div class="carousel-indicators">
      %s
      </div>
      <div class="carousel-inner">
      %s
      </div>
      <button class="carousel-control-prev" type="button" data-bs-target="#%s" data-bs-slide="prev">
        <span class="carousel-control-prev-icon" aria-hidden="true"></span>
        <span class="visually-hidden">Previous</span>
      </button>
      <button class="carousel-control-next" type="button" data-bs-target="#%s" data-bs-slide="next">
        <span class="carousel-control-next-icon" aria-hidden="true"></span>
        <span class="visually-hidden">Next</span>
      </button>
    </div>
    <div class="carousel-thumbnails mt-2">
    %s
    </div>',
          id,
          indicators,
          items,
          id, id,
          thumbnails
  )
}

# ── tab strip ─────────────────────────────────────────────────────────────────

tab_buttons <- paste(mapply(function(year, i) {
  active   <- if (i == 1) ' active' else ''
  selected <- if (i == 1) 'true' else 'false'
  sprintf(
    '<button class="nav-link%s" id="tab-%d" data-bs-toggle="tab" data-bs-target="#pane-%d" type="button" role="tab" aria-controls="pane-%d" aria-selected="%s">%d</button>',
    active, year, year, year, selected, year
  )
}, years, seq_along(years)), collapse = "\n    ")

tab_panes <- paste(mapply(function(year, i) {
  active <- if (i == 1) ' show active' else ''
  sprintf(
    '<div class="tab-pane fade%s" id="pane-%d" role="tabpanel" aria-labelledby="tab-%d">\n%s\n  </div>',
    active, year, year, make_carousel(year)
  )
}, years, seq_along(years)), collapse = "\n  ")

# ── consolidated thumbnail-sync script (one block, at top level) ──────────────
# Keeping this out of make_carousel() avoids a Quarto post-processor bug that
# chokes on <script> tags nested inside raw HTML carousel divs.

thumb_script <- paste(sapply(years, function(year) {
  id <- paste0("carousel", year)
  sprintf('
    (function() {
      var el = document.getElementById("%s");
      if (!el) return;
      var thumbs = el.parentElement.querySelectorAll(".carousel-thumb");
      el.addEventListener("slid.bs.carousel", function(e) {
        thumbs.forEach(function(t) { t.classList.remove("thumb-active"); });
        if (thumbs[e.to]) thumbs[e.to].classList.add("thumb-active");
      });
    })();', id)
}), collapse = "\n")

html <- sprintf(
  '<!-- carousel_tabs.html — generated by build_carousel.R -->
<ul class="nav nav-tabs mb-3" id="carouselTabs" role="tablist">
  %s
</ul>
<div class="tab-content" id="carouselTabsContent">
  %s
</div>
<script>
  document.addEventListener("DOMContentLoaded", function() {
    %s
  });
</script>',
  tab_buttons,
  tab_panes,
  thumb_script
)

writeLines(html, "carousel_tabs.html")
message("Written: carousel_tabs.html")