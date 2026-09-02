# The empirical distribution primitives in this module are shared by
# quantile-based signals while leaving each method's transfer equation intact.

# Define interpolated empirical-CDF anchors with average-rank ties. For a
# sample x(1)...x(n), each distinct value receives
# p = (average_rank - 1) / (n - 1), with a constant sample placed at p = 0.5.
quantile__cdf_anchors <- function(sample) {
    checkmate::assert_numeric(
        sample,
        min.len = 1L,
        finite = TRUE,
        any.missing = FALSE
    )
    ordered <- sort(as.numeric(sample))
    runs <- rle(ordered)
    end_rank <- cumsum(runs$lengths)
    start_rank <- end_rank - runs$lengths + 1L
    average_rank <- (start_rank + end_rank) / 2
    probability <- if (length(ordered) == 1L) {
        0.5
    } else {
        (average_rank - 1) / (length(ordered) - 1)
    }
    data.frame(
        value = runs$values,
        probability = probability
    )
}

# Evaluate the explicit empirical CDF and clamp values outside the sample to
# endpoint probabilities rather than extrapolating a new tail.
quantile__empirical_cdf <- function(sample, values) {
    checkmate::assert_numeric(
        values,
        finite = TRUE,
        any.missing = FALSE
    )
    anchors <- quantile__cdf_anchors(sample)
    probability <- if (nrow(anchors) == 1L) {
        rep.int(anchors$probability, length(values))
    } else {
        stats::approx(
            x = anchors$value,
            y = anchors$probability,
            xout = values,
            method = "linear",
            rule = 2,
            ties = "ordered"
        )$y
    }
    lower_tail <- values < min(anchors$value)
    upper_tail <- values > max(anchors$value)
    # Strictly external values receive endpoint probabilities even when tied
    # sample endpoints have interior average-rank probabilities.
    probability[lower_tail] <- 0
    probability[upper_tail] <- 1
    list(
        probability = probability,
        lower_tail = lower_tail,
        upper_tail = upper_tail,
        tied_sample_values = length(sample) - nrow(anchors)
    )
}

# Evaluate an inverse empirical CDF with R's type-7 linear quantile rule.
quantile__inverse_cdf <- function(sample, probability) {
    checkmate::assert_numeric(
        probability,
        lower = 0,
        upper = 1,
        finite = TRUE,
        any.missing = FALSE
    )
    as.numeric(stats::quantile(
        sample,
        probs = probability,
        names = FALSE,
        type = 7
    ))
}

# Derive a stable group-specific seed so stochastic preprocessing remains
# reproducible without assigning identical sequences to every location.
quantile__group_seed <- function(seed, key, variable) {
    text <- paste(
        c(
            variable,
            unlist(Map(
                function(name, value) {
                    paste0(name, "=", as.character(value))
                },
                names(key),
                key
            ), use.names = FALSE)
        ),
        collapse = "\u001f"
    )
    modulus <- .Machine$integer.max - 1
    hash <- (as.double(seed) %% modulus) + 1
    for (code in utf8ToInt(enc2utf8(text))) {
        hash <- (hash * 131 + code) %% modulus
    }
    as.integer(hash + 1)
}

# Generate reproducible uniform variates with the Park-Miller
# state[i] = 16807 * state[i-1] mod 2147483647 recurrence.
quantile__uniform <- function(n, seed) {
    checkmate::assert_count(n)
    checkmate::assert_int(
        seed,
        lower = 1L,
        upper = .Machine$integer.max - 1L
    )
    modulus <- as.double(.Machine$integer.max)
    state <- as.double(seed)
    out <- numeric(n)
    for (index in seq_len(n)) {
        state <- (16807 * state) %% modulus
        out[[index]] <- state / modulus
    }
    out
}
