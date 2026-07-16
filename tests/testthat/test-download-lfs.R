test_that("metadata and selected data downloads are reproducible local packages", {
  package <- glc_open(make_glc_fixture("2.0.0"), quiet = TRUE)
  metadata_destination <- tempfile("metadata-download-")
  metadata_result <- glc_download(package, metadata_destination)

  expect_true(file.exists(file.path(metadata_destination, "datapackage.json")))
  expect_true(file.exists(file.path(
    metadata_destination,
    "glcdp-manifest.json"
  )))
  expect_false(file.exists(file.path(
    metadata_destination,
    "data",
    "files",
    "light.csv"
  )))
  expect_true(all(metadata_result$storage == "git"))
  reopened_metadata <- glc_open(metadata_destination, quiet = TRUE)
  expect_equal(reopened_metadata$schema_version, "2.0.0")

  data_destination <- tempfile("data-download-")
  data_result <- glc_download(
    package,
    data_destination,
    include = "data",
    dataset_id = "DS1"
  )
  expect_true(file.exists(file.path(
    data_destination,
    "data",
    "files",
    "light.csv"
  )))
  expect_true(nzchar(data_result$sha256[[1]]))
  reopened <- glc_open(data_destination, quiet = TRUE)
  expect_equal(nrow(glc_read(reopened, dataset_id = "DS1")$data[[1]]), 2)

  all_destination <- tempfile("all-download-")
  expect_no_error(glc_download(package, all_destination, include = "all"))
  expect_true(file.exists(file.path(all_destination, "data", "notes.csv")))
})

test_that("downloads protect existing files and unsafe package paths", {
  package <- glc_open(make_glc_fixture("2.0.0"), quiet = TRUE)
  destination <- tempfile("download-existing-")
  glc_download(package, destination)
  expect_error(
    glc_download(package, destination),
    "already contains",
    class = "glcdp_file_exists"
  )
  expect_no_error(glc_download(package, destination, overwrite = TRUE))

  expect_error(
    glcdp:::glc_safe_path("../secret.csv"),
    "Unsafe",
    class = "rlang_error"
  )
})

test_that("Git LFS pointers are parsed and verified", {
  content <- charToRaw("large data contents")
  file <- tempfile()
  writeBin(content, file)
  oid <- digest::digest(file, algo = "sha256", serialize = FALSE, file = TRUE)
  pointer <- paste(
    "version https://git-lfs.github.com/spec/v1",
    paste0("oid sha256:", oid),
    paste0("size ", length(content)),
    sep = "\n"
  )
  parsed <- glcdp:::glc_parse_lfs_pointer(pointer)

  expect_equal(parsed$oid, oid)
  expect_equal(parsed$size, length(content))
  expect_no_error(glcdp:::glc_verify_lfs(file, oid, length(content)))
  expect_error(
    glcdp:::glc_verify_lfs(
      file,
      paste(rep("0", 64), collapse = ""),
      length(content)
    ),
    class = "glcdp_lfs_hash_mismatch"
  )
})

make_lfs_package <- function(repo = "example/data") {
  package <- glcdp:::new_glc_package(
    source_type = "remote",
    repo = repo,
    commit = paste(rep("a", 40), collapse = ""),
    ref_kind = "commit",
    verified = FALSE,
    descriptor = list(resources = list()),
    schema_version = "2.0.0"
  )
  package$transport$tree <- tibble::tibble(
    path = character(),
    type = character(),
    size = numeric(),
    sha = character()
  )
  package
}

lfs_batch_response <- function(
  oid,
  size,
  href = "https://objects.example/data"
) {
  value <- list(
    objects = list(list(
      oid = oid,
      size = size,
      actions = list(
        download = list(href = href, header = list("X-Test" = "yes"))
      )
    ))
  )
  httr2::response(
    status_code = 200,
    body = charToRaw(jsonlite::toJSON(value, auto_unbox = TRUE))
  )
}

test_that("Git LFS objects are streamed through the batch API and verified", {
  content <- charToRaw("large data contents")
  source <- tempfile()
  writeBin(content, source)
  oid <- digest::digest(source, algo = "sha256", serialize = FALSE, file = TRUE)
  package <- make_lfs_package()
  info <- list(lfs_oid = oid, expected_size = length(content))
  destination <- tempfile()
  httr2::local_mocked_responses(list(
    lfs_batch_response(oid, length(content)),
    httr2::response(status_code = 200, body = content)
  ))

  expect_no_error(glcdp:::glc_download_lfs(package, info, destination))
  expect_identical(readBin(destination, "raw", n = length(content)), content)
})

test_that("expired LFS actions are renegotiated once", {
  content <- charToRaw("large data contents")
  source <- tempfile()
  writeBin(content, source)
  oid <- digest::digest(source, algo = "sha256", serialize = FALSE, file = TRUE)
  package <- make_lfs_package()
  info <- list(lfs_oid = oid, expected_size = length(content))
  destination <- tempfile()
  httr2::local_mocked_responses(list(
    lfs_batch_response(oid, length(content)),
    httr2::response(status_code = 403),
    lfs_batch_response(oid, length(content)),
    httr2::response(status_code = 200, body = content)
  ))

  expect_no_error(glcdp:::glc_download_lfs(package, info, destination))
})

test_that("missing, quota-limited, and size-mismatched LFS objects fail clearly", {
  oid <- paste(rep("a", 64), collapse = "")
  package <- make_lfs_package()
  missing <- list(
    objects = list(list(
      oid = oid,
      size = 10,
      error = list(code = 404, message = "Object does not exist")
    ))
  )
  httr2::local_mocked_responses(list(httr2::response(
    status_code = 200,
    body = charToRaw(jsonlite::toJSON(missing, auto_unbox = TRUE))
  )))
  expect_error(
    glcdp:::glc_lfs_batch_action(package, oid, 10),
    class = "glcdp_lfs_missing_object"
  )

  quota <- list(
    objects = list(list(
      oid = oid,
      size = 10,
      error = list(code = 403, message = "Bandwidth quota exceeded")
    ))
  )
  httr2::local_mocked_responses(list(httr2::response(
    status_code = 200,
    body = charToRaw(jsonlite::toJSON(quota, auto_unbox = TRUE))
  )))
  expect_error(
    glcdp:::glc_lfs_batch_action(package, oid, 10),
    class = "glcdp_lfs_auth_or_quota"
  )

  content <- charToRaw("short")
  source <- tempfile()
  writeBin(content, source)
  real_oid <- digest::digest(
    source,
    algo = "sha256",
    serialize = FALSE,
    file = TRUE
  )
  httr2::local_mocked_responses(list(
    lfs_batch_response(real_oid, 10),
    httr2::response(status_code = 200, body = content)
  ))
  expect_error(
    glcdp:::glc_download_lfs(
      package,
      list(lfs_oid = real_oid, expected_size = 10),
      tempfile()
    ),
    class = "glcdp_lfs_size_mismatch"
  )
})

test_that("LFS errors do not expose authentication tokens", {
  package <- make_lfs_package()
  package$transport$token <- "super-secret-token"
  httr2::local_mocked_responses(list(httr2::response(status_code = 403)))

  condition <- expect_error(
    glcdp:::glc_lfs_batch_action(
      package,
      paste(rep("a", 64), collapse = ""),
      10
    ),
    class = "glcdp_http_403"
  )
  expect_false(grepl(
    "super-secret-token",
    conditionMessage(condition),
    fixed = TRUE
  ))
})

test_that("external LFS configuration and oversized ordinary blobs are rejected", {
  package <- make_lfs_package()
  package$transport$tree <- tibble::tibble(
    path = ".lfsconfig",
    type = "blob",
    size = 10,
    sha = "blob"
  )
  expect_error(
    glcdp:::glc_lfs_batch_action(
      package,
      paste(rep("a", 64), collapse = ""),
      10
    ),
    class = "glcdp_external_lfs"
  )

  package <- make_lfs_package()
  package$transport$tree <- tibble::tibble(
    path = "data/large.csv",
    type = "blob",
    size = 101 * 1024^2,
    sha = "blob"
  )
  info <- glcdp:::glc_file_info_internal(package, "data/large.csv")
  expect_error(
    glcdp:::glc_transfer_to(package, "data/large.csv", tempfile(), info),
    class = "glcdp_unsupported_storage"
  )
})
