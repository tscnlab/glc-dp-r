factor_contract_variable <- function(
  values,
  labels = values,
  descriptions = rep(NA_character_, length(values)),
  ordered = FALSE,
  name = "EVENT"
) {
  list(
    name = name,
    type = "factor",
    ordered = ordered,
    factor_values = values,
    factor_labels = labels,
    factor_descriptions = descriptions
  )
}

test_that("factor unions have deterministic declaration-respecting order", {
  contracts <- list(
    factor_contract_variable(c("0", "1", "2", "3", "5", "10", "11", "12")),
    factor_contract_variable(c("0", "1", "2", "3", "4")),
    factor_contract_variable(c("0", "1", "2", "3"))
  )

  union <- glcdp:::glc_factor_union(contracts)
  reordered <- glcdp:::glc_factor_union(rev(contracts))

  expect_true(union$compatible)
  expect_true(union$harmonization_required)
  expect_identical(union$code, "factor_level_union")
  expect_identical(
    union$variable$factor_values,
    c("0", "1", "2", "3", "4", "5", "10", "11", "12")
  )
  expect_identical(reordered$variable, union$variable)
})

test_that("factor unions retain compatible labels and descriptions", {
  first <- factor_contract_variable(
    c("0", "1"),
    c(NA, "On"),
    c(NA, "Active")
  )
  second <- factor_contract_variable(
    c("0", "1", "2"),
    c("0", "On", "Off"),
    c("Zero", "Active", "Inactive")
  )

  union <- glcdp:::glc_factor_union(list(first, second))

  expect_true(union$compatible)
  expect_identical(union$variable$factor_labels, c("0", "On", "Off"))
  expect_identical(
    union$variable$factor_descriptions,
    c("Zero", "Active", "Inactive")
  )
})

test_that("conflicting factor meaning remains blocking", {
  label_conflict <- glcdp:::glc_factor_union(list(
    factor_contract_variable(c("0", "1"), c("Off", "On")),
    factor_contract_variable(c("0", "1"), c("Absent", "On"))
  ))
  ambiguous_label <- glcdp:::glc_factor_union(list(
    factor_contract_variable(c("0", "1"), c("State", "State"))
  ))
  description_conflict <- glcdp:::glc_factor_union(list(
    factor_contract_variable(c("0", "1"), descriptions = c("Off", "On")),
    factor_contract_variable(c("0", "1"), descriptions = c("Absent", "On"))
  ))

  expect_false(label_conflict$compatible)
  expect_identical(label_conflict$code, "factor_label_conflict")
  expect_false(ambiguous_label$compatible)
  expect_identical(ambiguous_label$code, "factor_label_ambiguous")
  expect_false(description_conflict$compatible)
  expect_identical(
    description_conflict$code,
    "factor_description_conflict"
  )
})

test_that("duplicate values and cyclic or ordered contracts remain blocking", {
  duplicate <- glcdp:::glc_factor_union(list(
    factor_contract_variable(c("0", "0"))
  ))
  cyclic <- glcdp:::glc_factor_union(list(
    factor_contract_variable(c("a", "b")),
    factor_contract_variable(c("b", "a"))
  ))
  ordered <- glcdp:::glc_factor_union(list(
    factor_contract_variable(c("a", "b"), ordered = TRUE)
  ))

  expect_false(duplicate$compatible)
  expect_identical(duplicate$code, "factor_value_duplicate")
  expect_false(cyclic$compatible)
  expect_identical(cyclic$code, "factor_order_conflict")
  expect_false(ordered$compatible)
  expect_identical(ordered$code, "ordered_factor_unsupported")
})
