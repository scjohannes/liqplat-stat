# Anonymous candidate-level V4 accounting; no linkage to the clinical cohort.
consort_accounting <- function(data) {
  required <- c("screened", "not_screened_reason", "exclusion_reason",
                "qualified_for_random_selection", "group_assignment",
                "invitation_status", "accepted_invitation")
  if (!setequal(names(data), required)) stop("Expected the seven-column CONSORT V4 contract.")
  exclusion_reasons <- c("No solid tumor", "Primary brain tumor", "Resectable tumor",
    "No indication for medical treatment", "Prior systemic treatment",
    "GRC declined or unclear", "Not eligible - reason not recorded")
  not_screened_reasons <- c("Competing trial",
    "Three patients already selected for random invitation that week")
  statuses <- c("Approached and accepted intervention",
    "Approached and declined intervention", "Not approached",
    "Approached with decision missing")
  check <- function(ok, message) {
    if (anyNA(ok) || !all(ok)) stop(message, call. = FALSE)
  }
  check(data$screened %in% 0:1 & data$qualified_for_random_selection %in% 0:1,
        "Screening and qualification must be complete binary indicators.")
  check(is.na(data$accepted_invitation) | data$accepted_invitation %in% 0:1,
        "Acceptance must be 0, 1, or missing.")
  for (field in c("not_screened_reason", "exclusion_reason", "group_assignment", "invitation_status")) {
    allowed <- switch(field, not_screened_reason = not_screened_reasons,
      exclusion_reason = exclusion_reasons, group_assignment = c("ctDNA + SOC", "SOC"),
      invitation_status = statuses)
    check(is.na(data[[field]]) | data[[field]] %in% allowed,
          paste("Unrecognized value in", field))
  }
  screened <- data$screened == 1
  eligible <- data$qualified_for_random_selection == 1
  allocated <- !is.na(data$group_assignment)
  selected <- data$group_assignment %in% "ctDNA + SOC"
  check(is.na(data$not_screened_reason) == screened,
        "Screening and not-screened reasons disagree.")
  check(screened | is.na(data$exclusion_reason),
        "Clinical exclusions cannot apply to not-screened candidates.")
  check(!screened | eligible | !is.na(data$exclusion_reason),
        "Screened ineligible candidates require a clinical exclusion reason.")
  check(!allocated | is.na(data$exclusion_reason),
        "Allocated candidates cannot have clinical exclusion reasons.")
  check(!eligible | screened, "Eligible candidates must have been screened.")
  check(!allocated | eligible, "Allocated candidates must be eligible.")
  check(selected | (is.na(data$invitation_status) & is.na(data$accepted_invitation)),
        "Invitation outcomes apply only to the selected SAT arm.")
  accepted <- data$invitation_status %in% statuses[1]
  declined <- data$invitation_status %in% statuses[2]
  check(ifelse(accepted, data$accepted_invitation == 1,
               ifelse(declined, data$accepted_invitation == 0, is.na(data$accepted_invitation))),
        "Invitation status and acceptance disagree.")
  counts <- c(candidates = nrow(data), not_screened = sum(!screened),
    assessed = sum(screened), excluded = sum(screened & !eligible),
    eligible = sum(eligible), unallocated = sum(eligible & !allocated),
    randomized = sum(allocated), selected = sum(selected),
    usual_care = sum(data$group_assignment %in% "SOC"),
    not_approached = sum(data$invitation_status %in% statuses[3]),
    status_missing = sum(selected & is.na(data$invitation_status)),
    approached = sum(data$invitation_status %in% statuses[c(1, 2, 4)]),
    accepted = sum(accepted), declined = sum(declined),
    decision_missing = sum(data$invitation_status %in% statuses[4]))
  check(c(counts["candidates"] == counts["not_screened"] + counts["assessed"],
    counts["assessed"] == counts["excluded"] + counts["eligible"],
    counts["eligible"] == counts["unallocated"] + counts["randomized"],
    counts["randomized"] == counts["selected"] + counts["usual_care"],
    counts["selected"] == counts["approached"] + counts["not_approached"] + counts["status_missing"],
    counts["approached"] == counts["accepted"] + counts["declined"] + counts["decision_missing"]),
    "CONSORT branches do not reconcile.")
  parents <- c("candidates", "candidates", "candidates", "assessed", "assessed",
    "eligible", "eligible", "randomized", "randomized", "selected", "selected",
    "selected", "approached", "approached", "approached")
  reason_counts <- c(vapply(not_screened_reasons, function(x) sum(data$not_screened_reason %in% x), integer(1)),
    vapply(exclusion_reasons, function(x) sum(!eligible & data$exclusion_reason %in% x), integer(1)))
  result <- data.frame(step = c(names(counts), names(reason_counts)),
    n = unname(c(counts, reason_counts)),
    denominator = unname(c(counts[parents], rep(counts["not_screened"], 2), rep(counts["excluded"], 7))))
  result$proportion <- with(result, ifelse(denominator > 0, n / denominator, NA_real_))
  result
}

plot_consort <- function(accounting) {
  n <- setNames(accounting$n, accounting$step)
  label <- function(title, key) paste0(title, "\n(n = ", format(n[[key]], big.mark = ",", trim = TRUE), ")")
  reasons <- function(keys) paste(paste0(keys, " (n = ", n[keys], ")"), collapse = "\n")
  boxes <- data.frame(x = c(3, 8, 3, 8, 3, 8, 3, 3, 8, 3, 8, 3),
    y = c(16, 14.8, 13, 11.6, 9.5, 9.5, 7.5, 5.5, 5.5, 3.3, 3.3, 1),
    width = c(4.1, 4.4, 4.1, 4.4, 4.1, 4.4, 4.1, 4.1, 4.4, 4.1, 4.4, 4.1),
    height = c(1, 1.9, 1, 2.6, 1, 1, 1, 1.2, 1.2, 1, 1.4, 1.5),
    text = c(label("Candidates recorded", "candidates"),
      paste0(label("Not screened", "not_screened"), "\nCompeting trial (n = ", n["Competing trial"],
        ")\nWeekly invitation limit reached (n = ", n["Three patients already selected for random invitation that week"], ")"),
      label("Assessed for eligibility", "assessed"),
      paste0(label("Excluded", "excluded"), "\n", reasons(accounting$step[18:24])),
      label("Eligible for random selection", "eligible"),
      label("No allocation recorded", "unallocated"),
      label("Randomized", "randomized"),
      label("Selected for SAT invitation", "selected"),
      label("Not selected for invitation\nUsual care", "usual_care"),
      label("Approached / offered invitation", "approached"),
      paste0(label("Not approached / not offered", "not_approached"),
        if (n["status_missing"] > 0) paste0("\nInvitation status missing (n = ", n["status_missing"], ")") else ""),
      paste0("Accepted invitation (n = ", n["accepted"], ")\nDeclined invitation (n = ", n["declined"], ")",
        if (n["decision_missing"] > 0) paste0("\nDecision missing (n = ", n["decision_missing"], ")") else "")))
  # Fixed publication layout; all labels and counts come from validated accounting.
  segments <- data.frame(
    x = c(3, 3, 3, 3, 3, 3, 3, 3, 5.05, 3, 3, 3, 3),
    y = c(15.5, 14.8, 12.5, 11.3, 9, 7, 6.6, 6.6, 9.5, 4.9, 4.3, 4.3, 2.8),
    xend = c(3, 5.8, 3, 5.8, 3, 3, 3, 8, 5.8, 3, 3, 8, 3),
    yend = c(13.5, 14.8, 10, 11.3, 8, 6.6, 6.1, 6.6, 9.5, 4.3, 3.8, 4.3, 1.75),
    arrow = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE))
  segments <- rbind(segments, data.frame(x = c(8, 8), y = c(6.6, 4.3),
    xend = c(8, 8), yend = c(6.1, 4), arrow = TRUE))
  ggplot2::ggplot() +
    ggplot2::geom_segment(data = segments[!segments$arrow, ],
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend), linewidth = 0.45, colour = "#46545B") +
    ggplot2::geom_segment(data = segments[segments$arrow, ],
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend), linewidth = 0.45,
      colour = "#46545B", arrow = grid::arrow(length = grid::unit(2, "mm"), type = "closed")) +
    ggplot2::geom_rect(data = boxes, ggplot2::aes(xmin = x - width / 2,
      xmax = x + width / 2, ymin = y - height / 2, ymax = y + height / 2),
      fill = "white", colour = "#426473", linewidth = 0.5) +
    ggplot2::geom_text(data = boxes, ggplot2::aes(x = x, y = y, label = text),
      size = 3.4, lineheight = 1.15, colour = "#182D38") +
    ggplot2::coord_cartesian(xlim = c(0.6, 10.4), ylim = c(0, 16.7), expand = FALSE) +
    ggplot2::theme_void() + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}
