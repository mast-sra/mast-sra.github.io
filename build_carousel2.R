# build_carousel.R
#
# Produces carousel_tabs.html — a Bootstrap tab strip with one carousel per
# tab.  Drop images in images/<tab name>/ (jpg) and add the name to the
# `tabs` vector below.  Tab names can be any character string ("Button Bay",
# "2024", "Spring Retreat", ...) — the folder name and tab label match the
# string exactly, and a URL/ID-safe "slug" is generated internally for the
# Bootstrap element IDs.  Re-run the script (or let Quarto's pre-render hook
# call it) and then include the output in any .qmd with:
#
#   ```{=html}
#   {{< include carousel_tabs.html >}}
#   ```

tabs <- c("Button Bay")   # <-- add future tabs here

# Optional text shown above the carousel in each tab.  Names must match
# entries in `tabs` exactly; tabs without an entry simply show no text.
tab_text <- c(
  "Button Bay" = "Photos from our day trip to Button Bay State Park in Vergennes. Photos provided by Alexandra Gannon."
)

# ── slug helper ───────────────────────────────────────────────────────────────
# HTML id attributes can't contain spaces (and are happier without other
# punctuation), so each tab name gets a lowercase, hyphenated slug used for
# all element IDs: "Button Bay" -> "button-bay".

slugify <- function(x) {
  s <- tolower(x)
  s <- gsub("[^a-z0-9]+", "-", s)   # any run of non-alphanumerics -> hyphen
  s <- gsub("^-+|-+$", "", s)       # trim leading/trailing hyphens
  s
}

# ── HEIC → JPEG conversion ────────────────────────────────────────────────────
# Converts any .HEIC/.heic files found in images/<tab name>/ folders.
# Requires either:
#   - sips   (built into macOS — no install needed)
#   - magick (ImageMagick); install with: brew install imagemagick (mac)
#                                         winget install ImageMagick (windows)
# Already-converted files are skipped (won't reconvert if .jpg exists).

convert_heic <- function(tab) {
  folder <- file.path("images", tab)
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
invisible(lapply(tabs, convert_heic))

# ── helpers ───────────────────────────────────────────────────────────────────

make_carousel <- function(tab) {
  slug   <- slugify(tab)
  folder <- file.path("images", tab)
  imgs   <- sort(list.files(folder, pattern = "\\.(jpe?g|png)$",
                            full.names = FALSE, ignore.case = TRUE))
  id     <- paste0("carousel-", slug)
  
  if (length(imgs) == 0) {
    return(sprintf(
      '<p class="text-muted fst-italic">No photos available yet for %s.</p>',
      tab
    ))
  }
  
  indicators <- paste(mapply(function(img, i) {
    active <- if (i == 0) 'class="active" aria-current="true" ' else ""
    sprintf(
      '<button type="button" data-bs-target="#%s" data-bs-slide-to="%d" %saria-label="Slide %d"></button>',
      id, i, active, i + 1
    )
  }, imgs, seq_along(imgs) - 1), collapse = "\n")
  
  items <- paste(mapply(function(img, i) {
    active <- if (i == 1) " active" else ""
    sprintf(
      '<div class="carousel-item%s">\n<img src="%s/%s" class="d-block w-100" alt="DataFest %s">\n</div>',
      active, folder, img, tab
    )
  }, imgs, seq_along(imgs)), collapse = "\n")
  
  thumbnails <- paste(mapply(function(img, i) {
    active_class <- if (i == 0) " thumb-active" else ""
    sprintf(
      '<img src="%s/%s" class="carousel-thumb%s" data-bs-target="#%s" data-bs-slide-to="%d" alt="Thumbnail %d">',
      folder, img, active_class, id, i, i + 1
    )
  }, imgs, seq_along(imgs) - 1), collapse = "\n")
  
  # NOTE: deliberately flat formatting — no blank lines, no 4+ space indents.
  # When Quarto/Pandoc parses included HTML as markdown, a blank line ends the
  # HTML block and 4-space-indented lines become literal code blocks.
  sprintf('<div id="%s" class="carousel slide" data-bs-ride="carousel" data-bs-interval="4000">
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

slugs <- slugify(tabs)

tab_buttons <- paste(mapply(function(tab, slug, i) {
  active   <- if (i == 1) ' active' else ''
  selected <- if (i == 1) 'true' else 'false'
  sprintf(
    '<button class="nav-link%s" id="tab-%s" data-bs-toggle="tab" data-bs-target="#pane-%s" type="button" role="tab" aria-controls="pane-%s" aria-selected="%s">%s</button>',
    active, slug, slug, slug, selected, tab
  )
}, tabs, slugs, seq_along(tabs)), collapse = "\n")

tab_panes <- paste(mapply(function(tab, slug, i) {
  active <- if (i == 1) ' show active' else ''
  blurb  <- if (tab %in% names(tab_text) && nzchar(tab_text[[tab]])) {
    sprintf('<p class="carousel-blurb">%s</p>\n', tab_text[[tab]])
  } else ""
  sprintf(
    '<div class="tab-pane fade%s" id="pane-%s" role="tabpanel" aria-labelledby="tab-%s">\n%s%s\n</div>',
    active, slug, slug, blurb, make_carousel(tab)
  )
}, tabs, slugs, seq_along(tabs)), collapse = "\n")

# ── consolidated thumbnail-sync script (one block, at top level) ──────────────
# Keeping this out of make_carousel() avoids a Quarto post-processor bug that
# chokes on <script> tags nested inside raw HTML carousel divs.

thumb_script <- paste(sapply(slugs, function(slug) {
  id <- paste0("carousel-", slug)
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
