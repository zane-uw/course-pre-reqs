rm(list = ls())
gc()

# setup -------------------------------------------------------------------

library(tidyverse)
library(dbplyr)
library(odbc)
library(igraph)

options(tibble.print_max = 100)

source("config.R")
con <- dbConnect(odbc::odbc(), config$dns, Database = config$db, UID = config$uid, PWD = rstudioapi::askForPassword("pwd-"))

dat <- tbl(con, in_schema("sec", "sr_course_prereq")) %>% filter(last_eff_yr == 9999, course_number < 500) %>% collect()
course.info <- tbl(con, in_schema("sec", "sr_course_titles")) %>% filter(last_eff_yr == 9999) %>% collect()

dbDisconnect(con); rm(con, config)

# cleanup data ---------------------------------------------------------------

dat <- dat %>% mutate_if(is.character, str_trim)
dat$course.to <- paste(dat$department_abbrev, dat$course_number, sep = " ")
dat$course.from <- paste(dat$pr_curric_abbr, dat$pr_course_no, sep = " ")
f <- if_else(dat$course.to == dat$course.from, T, F); table(f)                      # loops
dat <- dat %>% filter(dat$course.to != dat$course.from) %>% select(-contains('_spare_'), -pr_last_update_dt, -starts_with('last_eff'))
rm(f)

course.info <- course.info %>%
  mutate_if(is.character, str_trim) %>%
  mutate(course = paste(department_abbrev, course_number, sep = " ")) %>%
  select(-approved_dt, -changed_dt, -dropped_dt, -sr_crs_dl_appr_dt, -course_comment, -prior_dept_abbr, -course_prio_list, -default_sect_type,
         -contains('spare'),
         -starts_with('fee_'),
         -starts_with('prq_'),
         -starts_with('first_'),
         -starts_with('last_'),
         -starts_with('resp_'),
         -starts_with('course_flag'),
         -starts_with('distrib_'))


# remove lines with inactive pre-reqs from dat
table(dat$course.from %in% course.info$course)
dat <- dat[dat$course.from %in% course.info$course,]

# oddly enough!
table(dat$course.to %in% course.info$course)
dat[(dat$course.to %in% course.info$course) == F, 'course.to']
dat <- dat[dat$course.to %in% course.info$course,]

# create matrix, net
# el <- as.matrix(dat[,c('course.from', 'course.to')])
# n <- graph.edgelist(el, T)    # or just graph.data.frame
# E(n)$concurrent <- mdat[,14]

# vertex metadata:
clist <- sort(unique(c(dat$course.to, dat$course.from)))
attribs <- course.info %>% filter(course %in% clist) %>% select(course, everything()) %>% arrange(course) %>%
  mutate(level = ifelse(course_number %% 100 < 50, (course_number%/%100) * 10, (course_number %/% 100) * 10 + 5),
         course.level = (course_number %/% 100) * 100)

el <- dat %>% select(course.from, course.to, pr_seq_no, pr_group_no, pr_and_or, pr_grade_min, pr_cr_s, pr_concurrency)
n <- graph_from_data_frame(el, directed = T, vertices = attribs)
table(which_loop(n))  # good; check
summary(n)

save(dat, attribs, n, file = "data/pre-req-clean.RData")
