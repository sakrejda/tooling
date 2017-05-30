cmake_minimum_required (VERSION 3.2.2)
project (stan-environment VERSION 0.0.0.9000)

set(CMDSTAN_DIR ${PROJECT_BINARY_DIR}/code/stan-dev/cmdstan)
set(CMDSTAN_FIX_DIR ${PROJECT_BINARY_DIR}/code/stan-dev/bugfix-cmdstan)
set(SIMULATION_DIR ${PROJECT_BINARY_DIR}/simulation)
set(FIT_DIR ${PROJECT_BINARY_DIR}/fit)
set(STAN_MODEL_DIR ${PROJECT_BINARY_DIR}/models/stan-lang/full)
set(STAN_MODEL_FIX_DIR ${PROJECT_BINARY_DIR}/models/stan-lang/full)
#set(ENV{CC} /share/pkg/gcc/5.1.0/bin/gcc)
#set(ENV{CXX} /share/pkg/gcc/5.1.0/bin/g++)
set(R_LIB_DIR ${PROJECT_BINARY_DIR}/R/library)
set(ENV{R_LIBS_USER} ${R_LIB_DIR})

add_custom_command(
  OUTPUT R/library/knitr
  COMMAND mkdir -p ${R_LIB_DIR}
  COMMAND Rscript ${PROJECT_SOURCE_DIR}/packages.R ${PROJECT_SOURCE_DIR} noupdate
  VERBATIM
  DEPENDS packages.R R-libraries.txt
)

add_custom_target(
  r-environment ALL DEPENDS R/library/knitr
)

add_subdirectory(code)
add_subdirectory(models)
