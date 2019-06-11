rm(list = ls())
gc()

# setup -------------------------------------------------------------------

library(dbplyr)
library(odbc)
library(igraph)
library(tidyverse)

options(tibble.print_max = 100)

source("config.R")
con <- dbConnect(odbc::odbc(), config$dns, Database = config$db, UID = config$uid, PWD = rstudioapi::askForPassword("pwd-"))

dat <- tbl(con, in_schema("sec", "sr_course_prereq")) %>% filter(course_number < 500) %>% collect() # last_eff_yr == 9999,
course.info <- tbl(con, in_schema("sec", "sr_course_titles")) %>% collect()                        # filter(last_eff_yr == 9999)

# cleanup data ---------------------------------------------------------------

dat <- dat %>% mutate_if(is.character, str_trim) %>%
  mutate(course.to = paste(department_abbrev, course_number, sep = " "),
         course.from = paste(pr_curric_abbr, pr_course_no, sep = " "))
f <- if_else(dat$course.to == dat$course.from, T, F); table(f)                      # self-loops
self.loops <- dat[f,]
dat <- dat %>% filter(dat$course.to != dat$course.from) # %>% select(-contains('_spare_'), -pr_last_update_dt, -starts_with('last_eff'))
rm(f)

course.info <- course.info %>%
  mutate_if(is.character, str_trim) %>%
  group_by(department_abbrev, course_number) %>%
  slice(n()) %>%
  ungroup() %>%
  mutate(course = paste(department_abbrev, course_number, sep = " "))

# FLAG (no longer remove) inactive courses and prereqs
dat$course.inactive <- if_else(dat$course.to %in% course.info$course, 0, 1)
dat$prereq.inactive <- if_else(dat$course.from %in% course.info$course, 0, 1)
table(dat$course.inactive)
table(dat$prereq.inactive)

# # oddly enough!
# table(dat$course.to %in% course.info$course)
# dat[(dat$course.to %in% course.info$course) == F, 'course.to']
# dat <- dat[dat$course.to %in% course.info$course,]

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

dbDisconnect(con); rm(con, config)
save(dat, attribs, n, file = "data/pre-req-clean.RData")
