do_package_checks(error_on = "error")

get_stage("install") %>%
  add_step(step_install_cran("gam"))

if (ci_has_env("BUILD_PKGDOWN")) {
  do_pkgdown(orphan = TRUE)
}
