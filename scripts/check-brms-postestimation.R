# Run explicitly: this numerical regression check compiles a small Stan model.
source(here::here("R", "model-helpers.R"))

set.seed(20260911)
model_data <- data.frame(
  id = rep(seq_len(12), each = 5),
  age = runif(60, 30, 80),
  gender = factor(rep(c("Men", "Women"), each = 30))
)
model_data$outcome <- rbinom(
  60, 1, plogis(-2 + 0.04 * model_data$age + rnorm(12)[model_data$id])
)
fit <- brms::brm(
  outcome ~ age + gender + (1 | id), data = model_data,
  family = brms::bernoulli("logit"),
  chains = 2, cores = 1, iter = 300, warmup = 150,
  seed = 20260911, refresh = 0
)

# The old calculation is the reference, not the production implementation.
expected <- rowMeans(brms::posterior_epred(
  fit, newdata = model_data, re_formula = NA
))
actual <- standardized_marginal_probability(fit, model_data)
stopifnot(
  identical(names(actual), "draw"),
  isTRUE(all.equal(actual$draw, expected, tolerance = 1e-12)),
  isTRUE(all.equal(
    standardized_marginal_probability(fit, model_data, ndraws = 7)$draw,
    expected[1:7], tolerance = 1e-12
  ))
)

# Execute each notebook's actual post-estimation expression on the same fit.
attempted <- model_data
for (notebook in c(
  "01-invitation-offered.qmd", "02-invitation-accepted.qmd",
  "04-technical-validity.qmd"
)) {
  code <- knitr::purl(
    here::here("analysis", "04-implementation", notebook),
    output = tempfile(fileext = ".R"), quiet = TRUE
  )
  expressions <- parse(code)
  block <- expressions[[which(vapply(expressions, function(x) {
    is.call(x) && identical(x[[1]], as.name("if"))
  }, logical(1)))]]
  assignment <- Filter(function(x) {
    is.call(x) && identical(x[[1]], as.name("<-")) &&
      identical(x[[2]], as.name("draws"))
  }, as.list(block[[3]])[-1])
  stopifnot(length(assignment) == 1L)
  eval(assignment[[1]])
  stopifnot(
    identical(names(draws), "draw"),
    isTRUE(all.equal(draws$draw, expected, tolerance = 1e-12))
  )
  gender_assignment <- Filter(function(x) {
    is.call(x) && identical(x[[1]], as.name("<-")) &&
      identical(x[[2]], as.name("gender_draws"))
  }, as.list(expressions))
  if (!identical(notebook, "04-technical-validity.qmd")) {
    stopifnot(length(gender_assignment) == 1L)
    eval(gender_assignment[[1]])
    for (gender in levels(model_data$gender)) {
      expected_gender <- rowMeans(brms::posterior_epred(
        fit, newdata = model_data[model_data$gender == gender, ], re_formula = NA
      ))
      stopifnot(isTRUE(all.equal(
        gender_draws$draw[gender_draws$gender == gender], expected_gender,
        tolerance = 1e-12
      )))
    }
  }
  unlink(code)
}
cat("brms post-estimation equivalence checks passed.\n")
