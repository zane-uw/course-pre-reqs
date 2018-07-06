rm(list = ls())
gc()

# setup -------------------------------------------------------------------

library(tidyverse)
library(dbplyr)
library(odbc)
library(igraph)

source("config.R")
con <- dbConnect(odbc::odbc(), config$dns, Database = config$db, UID = config$uid, PWD = rstudioapi::askForPassword("pwd-"))

dat <- tbl(con, in_schema("sec", "sr_course_prereq")) %>% collect()

dbDisconnect(con); rm(con, config)

# cleanup data ---------------------------------------------------------------

dat <- dat %>% mutate_if(is.character, str_trim) %>% filter(last_eff_yr == 9999, course_number < 500)
dat$course.to <- paste0(dat$department_abbrev, dat$course_number)
dat$course.from <- paste0(dat$pr_curric_abbr, dat$pr_course_no)
dat <- dat %>% filter(dat$course.to != dat$course.from)

# attrib <- dat %>% select(course_branch, department_abbrev, course_number, pr_seq_no, pr_group_no, pr_and_or,
#                          pr_not_excl, pr_grade_min, pr_cr_s, pr_concurrency)

# create matrix, net
mdat <- as.matrix(dat)
n <- graph.edgelist(mdat[,c(19, 18)], T)    # or just graph.data.frame
E(n)$concurrent <- mdat[,14]

table(which_loop(n))

save(dat, n, file = "data/raw/pre-req-raw.RData")

# to do -------------------------------------------------------------------

# capture all unique pre-fixes that exist here and pile subsets into a list to plot them all
# think about how to represent qualitative info about the ties (concurrent courses, min grade, etc.)

# ! # get the list of current courses to filter on the campus/dept and also to be able to
#     filter out the list of pre-reqs that aren't current anymore