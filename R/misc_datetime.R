# from LLM, to be improved
pretty_datetime_seq <- function(x, n, rounding_method_start = "floor", rounding_method_end = "ceiling") {
	# --- Input Validation ---
	if (!inherits(x, c("POSIXct", "POSIXlt", "Date"))) {
		stop("Input 'x' must be a POSIXct, POSIXlt, or Date vector.")
	}
	if (!is.numeric(n) || length(n) != 1 || n <= 0) {
		stop("Input 'n' must be a positive numeric value.")
	}
	rounding_methods <- c("none", "floor", "ceiling", "round")
	if (!(rounding_method_start %in% rounding_methods) || !(rounding_method_end %in% rounding_methods)) {
		stop("Invalid rounding_method. Choose from 'none', 'floor', 'ceiling', or 'round'.")
	}

	# --- Determine Start and End Times (handling different classes, timezones, and NAs) ---
	if (inherits(x, "Date")) {
		# Use na.rm = TRUE to handle potential NA values
		start_time <- as.POSIXct(min(x, na.rm = TRUE), tz = "UTC")
		end_time <- as.POSIXct(max(x, na.rm = TRUE), tz = "UTC")
		original_class <- "Date"
	} else {
		# Use na.rm = TRUE and explicitly set the timezone
		start_time <- as.POSIXct(min(x, na.rm = TRUE), tz = attr(x, "tzone"))
		end_time <- as.POSIXct(max(x, na.rm = TRUE), tz = attr(x, "tzone"))
		original_class <- class(x)[1]
	}

	# Check if start_time or end_time are NA (meaning ALL values in x were NA)
	if (is.na(start_time) || is.na(end_time)) {
		return(NA)  # Or throw an error: stop("Input vector 'x' contains only NA values.")
	}


	# --- Rounding Function (internal) ---
	round_datetime <- function(dt, interval_text, method) {

		interval_unit <- gsub("[0-9. ]", "", interval_text)

		if (interval_unit %in% c("months", "years")){
			dt_lt <- as.POSIXlt(dt)

			if (interval_unit == "months") {
				if(method == "floor"){
					dt_lt$mday <- 1
					dt_lt$hour <- 0
					dt_lt$min <- 0
					dt_lt$sec <- 0
				} else if (method == "ceiling"){
					dt_lt$mon <- dt_lt$mon + as.numeric(gsub("[^0-9.]", "", interval_text))
					dt_lt$mday <- 1
					dt_lt$hour <- 0
					dt_lt$min <- 0
					dt_lt$sec <- 0
				} else if (method == "round"){
					#Get next month
					next_mon <- as.POSIXlt(dt)
					next_mon$mon <- next_mon$mon + as.numeric(gsub("[^0-9.]", "", interval_text))
					next_mon$mday <- 1
					next_mon$hour <- 0
					next_mon$min <- 0
					next_mon$sec <- 0
					#Calculate distances
					dist_up <- as.numeric(difftime(next_mon, dt, units="secs"))
					dist_down <- as.numeric(difftime(dt, round_datetime(dt, interval_text, "floor"), units="secs"))
					if(dist_up < dist_down) {
						dt_lt <- next_mon
					} else{
						dt_lt$mday <- 1
						dt_lt$hour <- 0
						dt_lt$min <- 0
						dt_lt$sec <- 0
					}

				}
			} else if (interval_unit == "years"){
				if (method == "floor") {
					dt_lt$mon <- 0  # January
					dt_lt$mday <- 1
					dt_lt$hour <- 0
					dt_lt$min <- 0
					dt_lt$sec <- 0
				} else if (method == "ceiling") {
					dt_lt$year <- dt_lt$year + as.numeric(gsub("[^0-9.]", "", interval_text))
					dt_lt$mon <- 0  # January
					dt_lt$mday <- 1
					dt_lt$hour <- 0
					dt_lt$min <- 0
					dt_lt$sec <- 0
				} else if (method == "round") {
					#Get next year
					next_year <- as.POSIXlt(dt)
					next_year$year <- next_year$year + as.numeric(gsub("[^0-9.]", "", interval_text))
					next_year$mon <- 0 # January
					next_year$mday <- 1
					next_year$hour <- 0
					next_year$min <- 0
					next_year$sec <- 0
					#Calculate distances
					dist_up <- as.numeric(difftime(next_year, dt, units = "secs"))
					dist_down <- as.numeric(difftime(dt, round_datetime(dt, interval_text, "floor"), units = "secs"))
					if(dist_up < dist_down){
						dt_lt <- next_year
					} else {
						dt_lt$mon <- 0  # January
						dt_lt$mday <- 1
						dt_lt$hour <- 0
						dt_lt$min <- 0
						dt_lt$sec <- 0
					}
				}
			}
			return(as.POSIXct(dt_lt)) #Convert back
		} else {
			#For secs, mins, hours, days and weeks, we use numeric operations
			interval_num <- as.numeric(gsub("[^0-9.]", "", interval_text))
			interval_seconds <- switch(interval_unit,
									   "secs" = interval_num,
									   "mins" = interval_num * 60,
									   "hours" = interval_num * 3600,
									   "days" = interval_num * 86400,
									   "weeks" = interval_num * 86400 * 7,
									   stop("Unsupported interval unit")
			)
			dt_numeric <- as.numeric(dt)
			rounded_numeric <- switch(method,
									  "floor" = floor(dt_numeric / interval_seconds) * interval_seconds,
									  "ceiling" = ceiling(dt_numeric / interval_seconds) * interval_seconds,
									  "round" = round(dt_numeric / interval_seconds) * interval_seconds,
									  dt_numeric # none
			)
			return(as.POSIXct(rounded_numeric, origin = "1970-01-01", tz = attr(dt, "tzone")))
		}
	}

	# --- Determine Ideal Interval ---
	# Calculate total_seconds ACCURATELY using numeric representation
	total_seconds <- as.numeric(end_time) - as.numeric(start_time)  # KEY CHANGE
	target_interval_seconds <- total_seconds / n

	# --- Find "Pretty" Interval ---
	pretty_intervals <- c(
		1, 2, 5, 10, 15, 30,
		60, 120, 300, 600, 900, 1800, 3600,
		3600 * 2, 3600 * 3, 3600 * 4, 3600 * 6, 3600 * 8, 3600 * 12,
		86400, 86400 * 2, 86400 * 7,
		86400 * 30, 86400 * 30 * 2, 86400 * 30 * 3, 86400 * 30 * 4, 86400 * 30 * 6,
		86400 * 365, 86400 * 365 * 2, 86400 * 365 * 5, 86400 * 365 * 10
	)
	best_interval_seconds <- pretty_intervals[which.min(abs(pretty_intervals - target_interval_seconds))]

	interval_text <- if (best_interval_seconds < 60) {
		paste(best_interval_seconds, "secs")
	} else if (best_interval_seconds < 3600) {
		paste(best_interval_seconds / 60, "mins")
	} else if (best_interval_seconds < 86400) {
		paste(best_interval_seconds / 3600, "hours")
	} else if (best_interval_seconds < (86400 * 7)) {
		paste(best_interval_seconds / 86400, "days")
	} else if (best_interval_seconds < (86400 * 30)) {
		paste(best_interval_seconds / (86400 * 7), "weeks")
	} else if (best_interval_seconds < (86400 * 365)) {
		#Months is approximate, so we use the interval_text to calculate
		interval_text <- paste(round(best_interval_seconds / (86400*30)), "months")
		best_interval_seconds <- as.numeric(gsub("[^0-9.]", "", interval_text)) * 86400*30
		interval_text #return
	} else {
		#Years is approximate, so we use the interval_text to calculate
		interval_text <- paste(round(best_interval_seconds / (86400 * 365)), "years")
		best_interval_seconds <- as.numeric(gsub("[^0-9.]", "", interval_text)) * 86400 * 365
		interval_text #return
	}

	# --- Round Start and End Times ---
	rounded_start_time <- round_datetime(start_time, interval_text, rounding_method_start)
	rounded_end_time <- round_datetime(end_time, interval_text, rounding_method_end)

	# --- Generate Sequence ---
	date_time_sequence <- seq.POSIXt(from = rounded_start_time, to = rounded_end_time, by = interval_text)

	# --- Convert back to original class ---
	if (original_class == "Date") {
		date_time_sequence <- as.Date(date_time_sequence)
	} else if (original_class == "POSIXlt") {
		date_time_sequence <- as.POSIXlt(date_time_sequence)
	}

	return(date_time_sequence)
}


pretty_datetime_seq3 = function(x, by) {
	s = seq(min(x), max(x), by = by)
	pretty_datetime_seq(x, n = length(s))
}
