#!/usr/bin/env Rscript

#### ---- Attach packages ---- ####

library(jhcutils)
library(mustashe)
library(glue)
library(ggtext)
library(patchwork)
library(nakedpipe)
library(magrittr)
library(tidyverse)



#### ---- Plotting ---- ####

theme_set(theme_minimal(base_size = 11, base_family = "Arial"))



#### ---- Prepare environment ---- ####

source_all_files <- function(dir) {
  lib_files <- list.files(dir, pattern = "R$", full.names = TRUE)
  lib_files <- sort(lib_files)
  for (f in lib_files) {
    source(f)
  }
  invisible(NULL)
}

for (dir in c("lib", "src")) {
  source_all_files(dir)
}

source_lib <- function() {
  source_all_files("lib")
}

source_src <- function() {
  source_all_files("src")
}



#### ---- Options ---- ####

options(dplyr.summarise.inform = FALSE)
