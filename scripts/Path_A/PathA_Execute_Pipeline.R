# Executing Path A Pipeline
# Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D.
# 27 October 2026

#   It took two Ph.D.s to run some R code. Thanks Claude.
    setwd("/workspace/caffeine_citation")

#	Project Root
	PA_ROOT    <- Sys.getenv("CAFFEINE_DIR", "/workspace/caffeine_citation")
	PA_SCRIPTS <- file.path(PA_ROOT, "scripts")
	PA_TESTS   <- file.path(PA_ROOT, "/data/tie_tests")

#   Sourcing Pipeline
    source("scripts/Path_A/PathA_Pipeline.R")

	res <- pa_pipeline(preflight_only = TRUE)   # check inputs, seconds
	res <- pa_pipeline()                        # granularity, tie_layer, test
	res <- pa_pipeline(stages = "tie_layer")    # one stage

#   Running with Sytem Commands
    out <- system2("Rscript", shQuote(PA_TEST_FILE),
				   stdout = TRUE, stderr = TRUE,
				   env = paste0("CAFFEINE_DIR=", PA_ROOT))
	code <- attr(out, "status")