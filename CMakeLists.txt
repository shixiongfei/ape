# CMakeLists.txt
#
# Copyright (c) 2021-2025 Xiongfei Shi
#
# Author: Xiongfei Shi <xiongfei.shi(a)icloud.com>
# License: Apache-2.0
#
# https://github.com/shixiongfei/ape
#

cmake_minimum_required(VERSION 3.15)
project(ape LANGUAGES C)

set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "" FORCE)

file(GLOB SOURCE_FILES CONFIGURE_DEPENDS "${PROJECT_SOURCE_DIR}/*.c")

add_executable(${PROJECT_NAME} ${SOURCE_FILES})

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE "Release" CACHE STRING "" FORCE)
endif()

set(TARGET_OUTPUT_DIR "${CMAKE_SOURCE_DIR}/bin")
set_target_properties(${PROJECT_NAME} PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${TARGET_OUTPUT_DIR}"
    ARCHIVE_OUTPUT_DIRECTORY "${TARGET_OUTPUT_DIR}"
    LIBRARY_OUTPUT_DIRECTORY "${TARGET_OUTPUT_DIR}"
    OUTPUT_NAME $<$<CONFIG:Debug>:${PROJECT_NAME}d>$<$<CONFIG:Release>:${PROJECT_NAME}>
)

target_compile_definitions(${PROJECT_NAME} PRIVATE
  _UNICODE
  $<$<CONFIG:Debug>:_DEBUG>
  $<$<CONFIG:Release>:_NDEBUG>
)

if(WIN32)
  target_compile_definitions(${PROJECT_NAME} PRIVATE
    _CRT_SECURE_NO_WARNINGS
    _CRT_SECURE_NO_DEPRECATE
    _CRT_NONSTDC_NO_DEPRECATE
    _WINSOCK_DEPRECATED_NO_WARNINGS
  )
else()
  target_link_libraries(${PROJECT_NAME} m)
endif()

if(MSVC)
  target_compile_options(${PROJECT_NAME} PRIVATE
    $<$<CONFIG:Debug>:/Zi /Od>
    $<$<CONFIG:Release>:/O2>
  )
else()
  target_compile_options(${PROJECT_NAME} PRIVATE
    $<$<CONFIG:Debug>:-g -O0>
    $<$<CONFIG:Release>:-O2 -march=native -flto>
  )
endif()
