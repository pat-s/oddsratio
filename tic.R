if (Sys.getenv("NB") != "w/ covr" | Sys.getenv("NB") != "w/ lintr") {
  add_package_checks()
}

if (Sys.getenv("id_rsa") != "") {
  
  get_stage("before_deploy") %>%
    add_step(step_setup_ssh())
  
  get_stage("deploy") %>%
    add_step(step_build_pkgdown()) %>%
    add_step(step_push_deploy(orphan = TRUE, path = "docs", branch = "gh-pages"))
  
} 

if (Sys.getenv("NB") == "w/ covr" | Sys.getenv("NB") == "w/ lintr") {
  
  if (Sys.getenv("NB") == "w/ lintr") {
    
    get_stage("after_success") %>% 
      step_run_code(lintr::lint_package(linters = with_defaults(commented_code_linter = NULL, 
                                                                closed_curly_linter = closed_curly_linter(allow_single_line = TRUE), 
                                                                open_curly_linter = open_curly_linter(allow_single_line = TRUE))))
  } else {
    get_stage("after_success") %>% 
      step_run_code(covr::codecov(quiet = FALSE))
  }
}