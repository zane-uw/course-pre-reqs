#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Oct 30 13:34:36 2018

@author: zane
"""

import pandas as pd

course_data = pd.read_pickle("data/course-data.pkl")
prereqs = pd.read_pickle("data/prereq-data.pkl")

# strip whitespace from strings
prereqs = prereqs.apply(lambda x: x.str.strip() if x.dtype == "object" else x)
course_data = course_data.apply(lambda x: x.str.strip() if x.dtype == "object" else x)

# create readable course from dept + #
prereqs['course_to'] = prereqs['department_abbrev'] + " " + prereqs['course_number'].map(str)
prereqs['course_from'] = prereqs['pr_curric_abbr'] + " " + prereqs['pr_course_no']

# create a single key for course data and then keep only last observation
course_data['course'] = course_data['department_abbrev'] + " " + course_data['course_number'].map(str)
course_data['ld'] = course_data['last_eff_yr'] * 10 + course_data['last_eff_qtr']
course_data = course_data.loc[course_data.groupby('course').ld.idxmax()]


# remove self-loops and delete some extraneous fields
prereqs.drop(prereqs[(prereqs.course_to == prereqs.course_from)].index, inplace = True)
prereqs.drop(list(prereqs.filter(regex = '_spare')), axis = 1, inplace = True)
# prereqs.drop(columns = ['pr_last_update_dt'], inplace = True)

course_data = course_data.loc[:, ['course', 'department_abbrev', 'course_number',
                                  'last_eff_yr', 'last_eff_qtr', 'course_branch',
                                  'course_college', 'long_course_title',
                                  'prq_lang_of_adm', 'prq_check_grads', 'pre_cancel_req',
                                  'course_cat_omit', 'writing_crs', 'diversity_crs',
                                  'english_comp', 'qsr', 'vis_lit_perf_arts',
                                  'indiv_society', 'natural_world']]

# remove inactive courses from prereqs
prereqs = prereqs[prereqs['course_from'].isin(course_data['course'])]
prereqs = prereqs[prereqs['course_to'].isin(course_data['course'])]

# vertex metadata
clist = prereqs[['course_to', 'course_from']].drop_duplicates()
clist.sort_values(['course_to', 'course_from'], inplace = True)

attribs = course_data[course_data['course'].isin(prereqs['course_to']) | course_data['course'].isin(prereqs['course_from'])]
