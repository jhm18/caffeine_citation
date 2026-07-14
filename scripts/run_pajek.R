#	Calling Pajek from R — Helper Functions + User-Facing Wrapper
#	Refactor of the system()-based Pajek launcher into small, single-purpose
#	helpers (developer-only style) followed by run_pajek() (user-facing style).
#	Style: Tab-Comment R Style Guide (literal tabs; headings above code).

#	-----------------------------------------------------------------------------
#	Helper Functions
#	-----------------------------------------------------------------------------

#	Find Macro File
	find_mcr_file <- function(data_dir, pattern = "\\.MCR$") {
		#	"""
		#	Args:
		#		data_dir: directory to search for a Pajek macro (.MCR) file
		#		pattern: regex identifying macro files (default "\\.MCR$")
		#	Returns:
		#		character(1), full normalized path to the first matching file
		#	Notes:
		#		Case-insensitive. Errors if the directory is missing or empty.
		#	"""

		#	Validation
			data_dir <- normalizePath(path.expand(data_dir), mustWork = TRUE)

		#	Collect candidates
			hits <- list.files(data_dir, pattern = pattern, ignore.case = TRUE, full.names = TRUE)
			if (length(hits) == 0L) stop("No macro file matching '", pattern, "' in ", data_dir)

		#	Assemble result
			return(normalizePath(hits[[1L]]))
	}

#	Resolve Pajek Executable
	resolve_pajek_exe <- function(pajek_dir, pajek_exe = "pajek.exe") {
		#	"""
		#	Args:
		#		pajek_dir: folder containing the Pajek binary
		#		pajek_exe: executable file name (default "pajek.exe")
		#	Returns:
		#		character(1), full normalized path to the executable
		#	Notes:
		#		Errors if the executable cannot be found on disk.
		#	"""

		#	Build candidate path
			exe <- file.path(path.expand(pajek_dir), pajek_exe)

		#	Validation
			if (!file.exists(exe)) stop("Pajek executable not found: ", exe)

		#	Assemble result
			return(normalizePath(exe))
	}

#	Translate Path for Pajek
	to_pajek_path <- function(path, wine_cmd) {
		#	"""
		#	Args:
		#		path: a host (Unix) path to translate
		#		wine_cmd: Wine launcher ("wine"/"wine64"), or "" for native Windows
		#	Returns:
		#		character(1), a path string Pajek can open
		#	Notes:
		#		Under Wine, uses `winepath -w` against the active WINEPREFIX.
		#		On native Windows (wine_cmd == ""), returns the path unchanged.
		#	"""

		#	Early return
			if (!nzchar(wine_cmd)) return(path)

		#	Validation
			if (!nzchar(Sys.which("winepath"))) stop("`winepath` not found on PATH; is Wine installed?")

		#	Translate via winepath
			win <- system2("winepath", c("-w", shQuote(path)), stdout = TRUE)

		#	Assemble result
			return(trimws(win))
	}

#	Build Pajek Call
	build_pajek_call <- function(exe, mcr_for_pajek, wine_cmd) {
		#	"""
		#	Args:
		#		exe: full path to the Pajek executable
		#		mcr_for_pajek: macro path already translated for Pajek
		#		wine_cmd: Wine launcher ("wine"/"wine64"), or "" for native Windows
		#	Returns:
		#		list(command, args) ready to hand to system2()
		#	Notes:
		#		Each path is shQuote()d so embedded spaces survive the shell.
		#	"""

		#	Native Windows: Pajek itself is the command
			if (!nzchar(wine_cmd)) {
				return(list(command = exe, args = shQuote(mcr_for_pajek)))
			}

		#	Under Wine: the launcher is the command, Pajek is its first argument
			return(list(command = wine_cmd, args = c(shQuote(exe), shQuote(mcr_for_pajek))))
	}

#	-----------------------------------------------------------------------------
#	User-Facing Wrapper
#	-----------------------------------------------------------------------------

#' @title run_pajek
#' @description Launch Pajek on a macro (.MCR) file from R using base-R system
#'   calls, with optional Wine support for running the Windows build on Linux or
#'   macOS. Works in plain R, Rscript, and batch jobs (no RStudio dependency).
#'
#' @param mcr_file Path to the Pajek macro (.MCR) file to run.
#' @param pajek_dir Folder containing the Pajek executable.
#' @param pajek_exe Executable file name. Default "pajek.exe".
#' @param wine_cmd Wine launcher ("wine" or "wine64"); "" for native Windows.
#'   Default NULL selects "" on Windows and "wine" elsewhere.
#' @param wait Logical; if TRUE, block R until Pajek returns. Default FALSE.
#'
#' @details The macro path is translated for Pajek by \code{to_pajek_path}
#'   (\code{winepath -w} under Wine), the executable is validated by
#'   \code{resolve_pajek_exe}, and the call is assembled by
#'   \code{build_pajek_call} before being passed to \code{system2}.
#'
#' @return Invisibly, the integer status returned by \code{system2}.
#'
#' @examples
#' \dontrun{
#' mcr <- find_mcr_file("~/Desktop/DNAC/Pendant_Paper/Data and Scripts")
#' run_pajek(mcr, pajek_dir = "~/Pajek64", wine_cmd = "wine")
#' }
#'
#' @export
run_pajek <- function(mcr_file, pajek_dir, pajek_exe = "pajek.exe", wine_cmd = NULL, wait = FALSE) {
	#	"""
	#	Args:
	#		mcr_file: path to the Pajek macro (.MCR) file
	#		pajek_dir: folder containing the Pajek executable
	#		pajek_exe: executable file name (default "pajek.exe")
	#		wine_cmd: Wine launcher ("wine"/"wine64"), "" native Windows, NULL auto
	#		wait: block until Pajek returns? (default FALSE)
	#	Returns:
	#		invisible integer status from system2()
	#	Notes:
	#		Pure orchestration; all path logic lives in the helpers above.
	#	"""

	#	Defaults
		if (is.null(wine_cmd)) wine_cmd <- if (.Platform$OS.type == "windows") "" else "wine"

	#	Validation
		mcr_file <- normalizePath(path.expand(mcr_file), mustWork = TRUE)
		if (nzchar(wine_cmd) && !nzchar(Sys.which(wine_cmd))) stop("Wine command '", wine_cmd, "' not found on PATH.")

	#	Resolve executable
		exe <- resolve_pajek_exe(pajek_dir, pajek_exe)

	#	Translate macro path for Pajek
		mcr_for_pajek <- to_pajek_path(mcr_file, wine_cmd)

	#	Build the call
		pajek_call <- build_pajek_call(exe, mcr_for_pajek, wine_cmd)

	#	Launch
		message("Launching: ", pajek_call$command, " ", paste(pajek_call$args, collapse = " "))
		status <- system2(pajek_call$command, args = pajek_call$args, wait = wait)

	#	Assemble result
		return(invisible(status))
}

#	-----------------------------------------------------------------------------
#	Example Usage (guarded so sourcing only defines the functions)
#	-----------------------------------------------------------------------------

#	Example Usage
	#if (FALSE) {
		#	Locate the macro file
	#		mcr <- find_mcr_file("~/Desktop/DNAC/Pendant_Paper/Data and Scripts")

		#	Non-default Wine prefix? Set it before running:
		#	Sys.setenv(WINEPREFIX = path.expand("~/.wine-pajek"))

		#	Which launcher do you have? Sys.which(c("wine", "wine64"))

		#	Run (wine_cmd: "wine", "wine64", or "" on native Windows)
	#		run_pajek(mcr, pajek_dir = "~/Pajek64", wine_cmd = "wine", wait = FALSE)
#	}

#	Test 1
#	Locate the macro file
		#	mcr <- "/workspace/caffeine_citation/pajek_files/Era22/net1.MCR"

		#	Non-default Wine prefix? Set it before running:
		#	Sys.setenv(WINEPREFIX = path.expand("~/.wine-pajek"))

		#	Which launcher do you have? Sys.which(c("wine", "wine64"))

		#	Run (wine_cmd: "wine", "wine64", or "" on native Windows)
		#	run_pajek(mcr, pajek_dir = "/root/.wine/drive_c/Program Files/Pajek", pajek_exe = "Pajek.exe", wine_cmd = "wine", wait = FALSE)


