is_rstudio = function () {
	if (requireNamespace("rstudioapi", quietly = TRUE)) {
		rstudioapi::isAvailable() && rstudioapi::versionInfo()$mode !=
			"vscode"
	}
	else {
		FALSE
	}
}

get_ide = function () {
	if (is_rstudio()) {
		return("rstudio")
	}
	else if (is_vscode()) {
		return("vscode")
	}
	else {
		"other"
	}
}

## need to assign global variable .vsc
utils::globalVariables(c(".vsc"))

is_vscode = function() {
	exists(".vsc") && exists("attach", envir = .vsc)
}

get_url_dir = function (url) gsub("file://|/index.html", "", url)
