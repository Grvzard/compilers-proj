cmake_minimum_required(VERSION 3.10)

project(
    compilers
)

file(GLOB P1_SRC "${PROJECT_SOURCE_DIR}/proj1/*.c")
add_executable(proj1 ${P1_SRC})
target_include_directories(proj1 PUBLIC ${PROJECT_SOURCE_DIR}/proj1)

# file(GLOB P2_SRC "${PROJECT_SOURCE_DIR}/proj2/*.c")
# add_executable(proj2 ${P2_SRC})
# target_include_directories(proj2 PUBLIC ${PROJECT_SOURCE_DIR}/proj2)
