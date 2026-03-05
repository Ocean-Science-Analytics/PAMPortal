# =============================================================================
# PAMPortal - Production Dockerfile for EKS
# Multi-stage build optimized for production deployments
# =============================================================================

# -----------------------------------------------------------------------------
# Stage 1: Build stage - Install R packages with renv
# -----------------------------------------------------------------------------
FROM rocker/r-ver:4.3.3 AS builder

# Set environment variables for reproducible builds
ENV RENV_VERSION=1.0.11 \
    RENV_PATHS_CACHE=/renv/cache \
    RENV_CONFIG_REPOS_OVERRIDE=https://cran.rstudio.com

# Install system dependencies required for R packages
# These include libraries for: seewave, tuneR (audio), sf (geospatial),
# PAMpal (acoustic analysis), RSQLite, curl, openssl, xml2, tcltk, etc.
RUN apt-get update && apt-get install -y --no-install-recommends \
    # Build tools
    build-essential \
    pkg-config \
    # Audio processing (seewave, tuneR)
    libsndfile1-dev \
    libfftw3-dev \
    # Geospatial (sf, leaflet dependencies)
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    # Database (RSQLite, DBI)
    libsqlite3-dev \
    # Network/SSL (curl, openssl, httr)
    libcurl4-openssl-dev \
    libssl-dev \
    # XML parsing (xml2)
    libxml2-dev \
    # Image processing (png, magick)
    libpng-dev \
    libjpeg-dev \
    # Font rendering and text shaping
    libfontconfig1-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    # Tcl/Tk (required by PAMmisc)
    tcl8.6-dev \
    tk8.6-dev \
    # NetCDF (required by ncdf4)
    libnetcdf-dev \
    # Git (required by gert)
    libgit2-dev \
    # Timezone data
    tzdata \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build

# Copy renv files first for layer caching
COPY renv.lock renv.lock

# Install renv and restore packages
RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cran.rstudio.com'))" \
    && R -e "renv::consent(provided = TRUE)" \
    && R -e "renv::restore(lockfile = 'renv.lock', library = '/usr/local/lib/R/site-library', prompt = FALSE)"

# -----------------------------------------------------------------------------
# Stage 2: Production runtime image
# -----------------------------------------------------------------------------
FROM rocker/r-ver:4.3.3 AS production

# Labels for container metadata
LABEL org.opencontainers.image.title="PAMPortal" \
      org.opencontainers.image.description="Passive Acoustic Monitoring Portal - Shiny Application" \
      org.opencontainers.image.vendor="Ocean Science Analytics" \
      org.opencontainers.image.source="https://github.com/Ocean-Science-Analytics/PAMPortal"

# Install only runtime system dependencies (no -dev packages)
# Package versions are for Ubuntu 22.04 (Jammy) used by rocker/r-ver:4.3.3
RUN apt-get update && apt-get install -y --no-install-recommends \
    # Audio processing runtime
    libsndfile1 \
    libfftw3-double3 \
    libfftw3-single3 \
    # Geospatial runtime (Ubuntu 22.04 versions)
    libgdal30 \
    libgeos-c1v5 \
    libgeos3.10.2 \
    libproj22 \
    libudunits2-0 \
    # Database runtime
    libsqlite3-0 \
    # Network/SSL runtime
    libcurl4 \
    libssl3 \
    # XML runtime
    libxml2 \
    # Image runtime
    libpng16-16 \
    libjpeg8 \
    # Font runtime and text shaping
    libfontconfig1 \
    libfreetype6 \
    libharfbuzz0b \
    libfribidi0 \
    # Tcl/Tk runtime (required by PAMmisc)
    libtcl8.6 \
    libtk8.6 \
    # NetCDF runtime
    libnetcdf19 \
    # Git runtime (for gert)
    libgit2-1.1 \
    # Timezone data
    tzdata \
    # Process management
    tini \
    # curl for healthcheck
    curl \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean

# Create non-root user for security (EKS best practice)
# Create home directory for cache files (sass, etc.)
RUN groupadd -r shiny && useradd -r -g shiny -m -d /home/shiny shiny

# Set environment variables
ENV SHINY_PORT=3838 \
    GOLEM_CONFIG_ACTIVE=production \
    R_CONFIG_ACTIVE=production

# Copy R libraries from builder stage
COPY --from=builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library

# Set working directory
WORKDIR /app

# Copy application code
COPY --chown=shiny:shiny . /app

# Create required directories with proper permissions
RUN mkdir -p /app/temp_audio /tmp/shiny \
    && chown -R shiny:shiny /app /tmp/shiny

# Switch to non-root user
USER shiny

# Expose Shiny port
EXPOSE 3838

# Health check for EKS/Kubernetes readiness probes
HEALTHCHECK --interval=30s --timeout=10s --start-period=60s --retries=3 \
    CMD curl -f http://localhost:3838/ || exit 1

# Use tini as init system to handle signals properly in containers
ENTRYPOINT ["/usr/bin/tini", "--"]

# Run Shiny app with production settings
CMD ["R", "-e", "options(shiny.port=3838, shiny.host='0.0.0.0'); pkgload::load_all(export_all=FALSE, helpers=FALSE, attach_testthat=FALSE); PAMPortal::run_app()"]

