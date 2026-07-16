test_that("registry is flattened without hiding failures", {
  path <- make_registry_fixture()
  registry <- glc_packages(path, refresh = TRUE)

  expect_s3_class(registry, "glc_registry")
  expect_equal(nrow(registry), 3)
  expect_equal(registry$current_status, c("pass", "fail", "fail"))
  expect_equal(registry$has_latest_pass, c(TRUE, TRUE, FALSE))
  expect_true(registry$attestation_verified[[1]])
  expect_equal(registry$current_errors, c(0L, 2L, 3L))
})

test_that("registry search filters text, status, and passing availability", {
  registry <- glc_packages(make_registry_fixture(), refresh = TRUE)

  expect_equal(glc_search_packages("older", registry)$id, "older-pass")
  expect_equal(
    glc_search_packages(packages = registry, status = "pass")$id,
    "passing"
  )
  expect_equal(
    glc_search_packages(packages = registry, has_pass = FALSE)$id,
    "no-pass"
  )
})

test_that("malformed registries fail clearly", {
  path <- tempfile(fileext = ".json")
  write_fixture_json(list(generated_at_utc = "now"), path)
  expect_error(
    glc_packages(path, refresh = TRUE),
    "datasets",
    class = "glcdp_registry_error"
  )
})

test_that("remote opening selects immutable registry revisions", {
  registry_path <- make_registry_fixture()
  descriptor <- charToRaw('{"schema_version":"2.0.0","resources":[]}')
  response <- httr2::response(status_code = 200, body = descriptor)
  httr2::local_mocked_responses(list(response))

  expect_message(
    package <- glc_open(
      "older-pass",
      registry = registry_path,
      quiet = FALSE
    ),
    "latest passing"
  )
  expect_equal(package$commit, paste(rep("c", 40), collapse = ""))
  expect_true(package$verified)
})

test_that("packages without passing revisions require an explicit ref", {
  registry_path <- make_registry_fixture()
  expect_error(
    glc_open("no-pass", registry = registry_path),
    "No passing revision",
    class = "glcdp_no_passing_revision"
  )
})

test_that("current failures warn and arbitrary commits remain unverified", {
  registry_path <- make_registry_fixture()
  descriptor <- charToRaw('{"schema_version":"2.0.0","resources":[]}')
  httr2::local_mocked_responses(list(
    httr2::response(status_code = 200, body = descriptor)
  ))
  expect_warning(
    current <- glc_open(
      "no-pass",
      ref = "current",
      registry = registry_path,
      quiet = TRUE
    ),
    "status is.*fail",
    class = "glcdp_nonpassing_revision"
  )
  expect_true(current$verified)

  httr2::local_mocked_responses(list(
    httr2::response(status_code = 200, body = descriptor)
  ))
  unverified <- glc_open(
    "example/passing",
    ref = paste(rep("e", 40), collapse = ""),
    registry = registry_path,
    quiet = TRUE
  )
  expect_false(unverified$verified)
})
