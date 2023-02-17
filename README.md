
# Course Resources

The **stat430materials** GitHub repository contains all information about our STAT 430 course.

See the lectures directory for course notes and slides. See the labs directory for lab assignments.


# News

 - Credit to Aidan Glickman: For people on Mac, you don’t actually have to build chadwick from source. First, install Homebrew (https://brew.sh/) Then run `brew install chadwick` from the terminal. For Mac and PC people, Jim Albert made an [updated blogpost](https://bayesball.github.io/VB/Getting_Retrosheet_Files.html) with better tools for pulling retro sheet data.

 - Lab 3 is now posted. It is due on 03/10 at 11:59 pm.

 - Slides from Jim Albert's talk are now posted in the root directory.

 - I have added a document that goes over the submission workflow, GitHub, R markdown code chunks, and terminal commands in detail. This document is called week1_supplemental_materials.md and it is available to you in 

```
lectures/week1_data
```

 - [Here](https://github.com/yihui/knitr-examples/blob/master/114-load-cache.md) is a game changing document for loading in quantities computed in the notes.


## Good Habits

 - pull my **stat430materials** repo before class starts
 - NEVER edit or remove any files in my **stat430materials** repo
 - load in datasets and run the code in the notes before class starts
 - make sure that the notes compile 
 - use your computer's terminal, it is less user-friendly but it is MUCH faster
 - Stay centered and remember Hofstadter's Law, which describes the widely experienced difficulty of accurately estimating the time it will take to complete tasks of substantial complexity. **Hofstadter's Law**: It always takes longer than you expect, even when you take into account Hofstadter's Law.
 

## Lab Hints

 - can work on labs in small groups of 2-3
 - get a jump start on the labs, do not wait until the last minute
 - use caching when appropriate
 - make sure that your lab report compiles when you complete a problem or stop working for the day
 - push often
 - do not jeopardize the entire lab over one problem


# Course Syllabus

### Greetings

<img src="https://stat.illinois.edu/sites/default/files/styles/directory_profile/public/profile-photos/dje13.png.jpg?itok=j2rsX2F_" 
     width="150" 
     height="150"
     hspace="25"
     vspace="10"
     align = "left"
     /> I am **Daniel J. Eck** and I will be the instructor for STAT 430: Baseball Analytics. Baseball research is a primary interest of mine. I am interested in developing useful statistical methodology for performance evaluation and am interested in the statistical history of baseball. Several of the examples and reading materials in this course will come from projects that I have worked on or that I find interesting and useful. 

**Email**: dje13@illinois.edu

<BR CLEAR=”left” /> 
<BR CLEAR=”left” /> 

**Course time:** Tuesdays and Thursdays from 11am - 12:20pm
	
**Course location:** 2078 Natural History Building

**Office hours**: 12pm - 1pm on Wednesdays (Zoom link: https://illinois.zoom.us/j/82646322420?pwd=TURMWG1HVU1ab3grOHI3aUhJZVkxQT09)

**TA**: Shen Yan (shenyan3@illinois.edu)

**TA Office hours**: 1pm - 3pm on Fridays (Zoom link: https://illinois.zoom.us/j/6544597856?pwd=enp4SmNUVjJEelFsazZjRjhFSHFpQT09)

**Course website**: https://github.com/illinois-stat430/stat430materials


My course website is a GitHub repository. You can click  [here](https://github.com/illinois-stat430/stat430materials) to access this website. Note that this Syllabus is a Markdown document. Open the .md file to view the basic Markdown syntax used to build your Syllabus.


***


### Course Materials

The course materials will largely consist of instructor notes, papers, articles, and software. The course will also use the textbook **[Analyzing Baseball Data with R](https://www.routledge.com/Analyzing-Baseball-Data-with-R-Second-Edition/Marchi-Albert-Baumer/p/book/9780815353515)** (second edition)

#### Papers, Articles, and Instructor Notes

Instructor notes will be posted and updated on GitHub. Relevant papers and articles will either be provided on GitHub or be referenced in course notes.



#### Software Used in the Course (all free)

- **R** https://cran.r-project.org/

- **RStudio** https://www.rstudio.com/products/rstudio/download/

- **RMarkdown** (packaged within RStudio) https://rmarkdown.rstudio.com/lesson-1.html  

- **GitHub** https://web.uillinois.edu/github
  - Details: see setup.md. 

***

### Course Information

This is a reading, seminar, and project based course on the intersection of baseball, statistics, and data science. In this course you will learn how to conduct relevant data analyses with a focus on how to quantify and visualize aspects of baseball play associated with winning games. You will also learn about the statistical history of baseball with an emphasis on comparing players across eras. Founding principles, intensive data analysis, and advanced statistical methods will be discussed for both directions. The analyses that you conduct will also develop your coding ability and critical thinking skills as a statistician and data scientist. Furthermore, practical advantages, limitations, and comparisons of methods will be discussed. If you are interested in quantifying how good Mike Trout is or in ranking the careers of Barry Bonds, Willie Mays, and Babe Ruth, then this is the course for you.  

**Prerequisites**: STAT 385 or equivalent experience with R, STAT 425, and STAT 410.  Familiarity with Git and GitHub is helpful, but not required.




### Student Learning Outcomes

Upon successful completion of this course students will be able to conduct methodologically strong data analyses that can answer questions of scientific interest, specifically the students will: 
 
  - Analyze and investigate databases of baseball statistics 
  
  - Utilize and understand procedures for quantifying success in baseball
  
  - Develop a solid practical understanding of procedures which compare players across eras
  
  - Develop data processing and visualization skills
  
  - Critique estimation procedures and modeling development strategies
  
  - Apply version control software and develop reproducible technical reports


### Getting Help in STAT 430

You have many options to get help for this class:

 * Canvas- you can post questions in the discussion forum.

 * Email me at dje13@illinois.edu from your Illinois email account (@illinois.edu). I will not respond to a non-Illinois email account. Be specific with the topic of your question in the subject line.

 * Office hours - you can go to my office hours as described above.
 

***

### Grading Breakdown

| category | notes | points
| :-- | :---- | :---- |
| Attendance | 5 points each course; four free misses  | 130 points
| Labs | 100 points each | 500 points
| Final Project | 50 points for approved project; 70 points for presentation (recorded summary); 250 for project materials and writeup | 370 points
| total | | 1000 points


I will be using a +/- grading schema. The grade distributions will be: 

| Lower bound | Upper bound | Letter grade |
| :--- | :--- | :--- |
| 980 | 1000 | A+ |
| 933 | 979 | A |
| 900 | 932 | A- |
| 867 | 899 | B+ |
| 833 | 866 | B |
| 800 | 832 | B-|
and so on 


#### Attendance and Course Content

Attendance will be a course requirement. There will be 15 lectures and you will receive 10 points for each lecture you attend, up to 130 points. Thus you are allowed to miss up to 2 lectures with no penalty.  There will be a signup sheet that is distributed at the beginning of class, you need to sign your name to verify your presence in class. Let your instructor know in advance if you cannot attend any class. 


#### Labs

There are 5 labs total. When completing the assignment, read it carefully, and follow the directions. These labs can be completed in groups of 2-3 students. Every group member will submit their own lab report. Make sure to list your collaborators in your lab report.

For each lab assignment, you will submit two files - .Rmd and .html (or .pdf) - saving your files with your name and lab assignment number. **The lab naming convention is netid_lab#**.  For a student with netid *abc123* that is submitting lab4 files, their files would be saved as *abc123_lab4.Rmd* and *abc123_lab4.html* (or *abc123_lab4.pdf*). **Failure to adopt this lab naming convention will result in point deductions and headaches.**  Make sure your lab is professional and reproducible containing only relevant derivations, code, results, and explanations. **Questions about the grading should be directed to the TA**.


Labs should be stored in a lab# directory in your GitHub repo that is a sub directory of a labs directory in your GitHub repo. For example, your fourth homework assignment should be saved in the directory 

```
labs/lab4/
```

within your personal GitHub repo.  Failure to do this will result in point deductions.

**Late lab submissions will be accepted with a penalty.** There will be a 20 point deduction if a lab is submitted 48 hours after the deadline. Labs submitted later than 48 hours after the deadline will not be considered.

**Students must use GitHub to start, finish, and submit their labs. More details will be discussed in class.**

Labs will be worth 100 points unless otherwise noted.


#### Project

This course will have a final project instead of an exam. This will be a group project comprised of 2-3 students. Final projects for this course will be fairly open-ended and not subject to a one-size-fits all standard. That being said, all projects need instructor approval. 

Details and project rubrics are TBD.

Possible projects include: 

  - Writing a report on an interesting player. This can a player past or present, and the reason that said player is interesting can vary from being successful in a newly understood analytic, possessing great underlying metrics which should translate to future great performances, or a HOF argument for a controversial candidate who you think has a misunderstood case.
  
  - Estimate the value of different players (possibly hypothetical players).  For example, who is more valuable: a pitcher who pitchers 200 innings with a 4.75 ERA or a pitcher who pitches 125 innings with a 3.50 ERA?
  
  - Examine interesting batter-pitcher matchups concerning one particular player or a few players.
  
  - Estimate marginal handedness-stadium specific spray chart distributions.

  - Development of a new performance metric, or a combination of metrics, which you can argue is valuable. Examples include expected "x" stats based on Statcast data.
  
  - Development of a player or team projection system, or a comparison of projection systems.
  
  - Creation of an era-adjusted JAWS-type statistic to rank players' careers.
  
  - Era-adjusting statistics that do not yet exist on the current version of the Era-Adjustment App. Examples include: SB/CS, WHIP and its inputs, and SLG and its inputs.
  
  - An investigation into the existence of *clutch* which includes matchup information.
  
  - Creating a Shiny app that does something interesting.
  
  - Develop a "causal" WAR stat computes the difference in team winning percentage when a player is available vs when the player is not available.
  
  - Something else!


#### Exams

There will be no exams!





***


### Policies

#### Disability Accommodations
To obtain disability-related academic adjustments and/or auxiliary aids, students with disabilities must contact the course instructor and the Disability Resources and Educational Services (DRES) as soon as possible. To contact DRES, you may visit 1207 S. Oak St., Champaign, call 333-1970, e-mail disability@illinois.edu or go to the DRES website.  If you are concerned you have a disability-related condition that is impacting your academic progress, you can talk with someone at the Counseling Center, McKinley Mental Health, or DRES about how to see a provider in order to obtain a diagnosis or get your questions answered.

#### Academic Integrity
It is expected that all students abide by the campus regulations on academic integrity [http://studentcode.illinois.edu/article1_part4_1-401.html].  Intentional violations of academic integrity can be found at http://studentcode.illinois.edu/article1_part4_1-402.html and include, but are not limited to, copying any part of another student's homework, allowing another student to copy any part of your homework, or submitting a review or summary of a presentation not attended.

<!-- #### Attendance and Course Content -->
<!-- Attendance in this course is a requirement. There will be a signup sheet that is distributed at the beginning of class, you need to sign your name to verify your presence in class. Let your instructor know in advance if you cannot attend any class. Course content - notes, code, required readings, and data sets - will be found on the Course Website. Do check the Course Website often for updates. Students are encouraged to take notes in class and read materials outside of class. Students are encouraged to save your work in individual GitHub repositories. -->
<!-- Please follow the Student Code https://studentcode.illinois.edu/docs/18.001.FullCodeInside.vf.pdf if you do have absences.  -->



#### For Your Safety
We have been asked by Public Safety https://police.illinois.edu/emergency-preparedness/run-hide-fight/ to share the following information in case of weather or security emergencies. See the links:

- [Emergency Response Recommendations](https://police.illinois.edu/emergency-preparedness/run-hide-fight/)
- [Video on Emergency Response](https://mediaspace.illinois.edu/media/t/1_bbti3ec5)

#### Sexual Misconduct Policy and Reporting
The University of Illinois is committed to combating sexual misconduct. Faculty and staff members are required to report any instances of sexual misconduct to the University's Title IX and Disability Office. In turn, an individual with the Title IX and Disability Office will provide information about rights and options, including accommodations, support services, the campus disciplinary process, and law enforcement options. 

A list of the designated University employees who, as counselors, confidential advisors, and medical professionals, do not have this reporting responsibility and can maintain confidentiality, can be found at https://wecare.illinois.edu/resources/students/#confidential. Other information about resources and reporting is available at https://wecare.illinois.edu.


***




<!-- **Possible Papers**:  

1. [A mixed effects multinomial logistic-normal model for forecasting baseball performance](https://www.researchgate.net/publication/348453795_A_mixed_effects_multinomial_logistic-normal_model_for_forecasting_baseball_performance)

2. [openWAR: An open source system for evaluating overall player performance in major league baseball](https://www.degruyter.com/document/doi/10.1515/jqas-2014-0098/html)

3. [Hierarchical Bayesian modeling of hitting performance in baseball](https://projecteuclid.org/journals/bayesian-analysis/volume-4/issue-4/Hierarchical-Bayesian-modeling-of-hitting-performance-in-baseball/10.1214/09-BA424.pdf)

4. [Enhancing strategic defensive positioning and performance in the outfield](https://link.springer.com/article/10.1007/s10109-021-00367-1)

 [Underestimating the Fog](https://sabr.org/research/article/underestimating-the-fog/), A Revised Look at Clutch Hitting parts [1](https://www.baseballprospectus.com/news/article/38398/prospectus-feature-revised-look-clutch-hitting-part-1/) and [2](https://www.baseballprospectus.com/news/article/38519/prospectus-feature-revised-look-clutch-hitting-part-2/), and [Clutch Hitting Revisited](https://sabr.org/journal/article/clutch-hitting-revisited/) -->


### Schedule (subject to change)

* Week 1 [01/17 (lecture), 01/19 (lab)]
  + First day of class on 01/17. We will discuss the course overview, syllabus, and Git/GitHub
  + **Reading**: course slides and setup.md
  + **Reading**: CH 1 and 2 in Analyzing Baseball Data with R (data sets and data wrangling)
  + **Lab 1** assigned this week, due on 02/03 at 11:59 pm

* Week 2 [01/24 (lecture), 01/26 (lab)]
  + **Reading**: course slides
  + **Reading**: CH 1 and 2 in Analyzing Baseball Data with R (data sets and data wrangling)
  + **Reading**: [A Statistical Look at Roger Clemens’ Pitching Career](https://repository.upenn.edu/cgi/viewcontent.cgi?article=1044&context=statistics_papers)
  
* Week 3 [01/31 (lecture), 02/02 (lab)]
  + **Reading**: course slides
  + **Reading**: CH 3 in Analyzing Baseball Data with R (common visualization)
  + **Reading**: CH 4: The Relation Between Runs and Wins in Analyzing Baseball Data with R
  + **Lab 1** due on 02/03 at 11:59 pm
  + **Lab 2** assigned this week, due on 02/17 at 11:59 pm

* Week 4 [02/07 (lecture), 02/09 (lab)]
  + **Reading**: course slides
  + **Reading**: CH 5: Value of Plays Using Run Expectancy in Analyzing Baseball Data with R

* Week 5 [02/14 (speaker), 02/16 (lab)]
  + **Lab 2** due on 02/17 at 11:59 pm
  + **Lab 3** assigned this week, due on 03/10 at 11:59 pm
  + **Speaker**: Jim Albert on Tuesday, February 14th at 11 am.

* Week 6 [02/21 (lecture), 02/23 (speaker)]
  + **Reading**: course slides
  + **Reading**: CH 9: Simulation in Analyzing Baseball Data with R 
  + **Speaker**: Alan Nathan on Thursday, February 23rd at 11 am.
  
* Week 7 [02/28 (lecture), 03/02 (lab)]
  + **Reading**: course slides
  + **Reading**: CH 12: Batted Ball Data from Statcast in Analyzing Baseball Data with R
  
* Week 8 [03/07 (lab), 03/09 (lab)]
  + **Lab 3** due on 03/10 at 11:59 pm
  + **Lab 4** assigned this week, due on 03/31 at 11:59 pm

* Week 9 [03/14, 03/16]  
  + **Spring Break**
  
* Week 10 [03/21 (lab), 03/23 (speaker)]
  + **Reading**: course slides
  + **Reading**: CH 12: Batted Ball Data from Statcast in Analyzing Baseball Data with R
  + **Speaker**: Abraham Wyner on Thursday, March 23rd at 11 am

* Week 11 [03/28 (lecture), 03/30 (lab)]
  + **Reading**: Introduction to R Shiny 
  + **Lab 4** due on 04/14 at 11:59 pm
  + **Lab 5** assigned this week, due on 11/11 at 11:59 pm

* Week 12 [04/04 (lab), 04/06 (speaker)]
  + **Reading**: Introduction to R Shiny
  + **Speaker**: Ehsan Bokhari on Thursday, April 6th at 11 am

* Week 13 [04/11 (lecture), 04/13 (speaker)]
  + **Reading**: course slides
  + **Reading**: SEAM Method
  + **Reading**: [Enhancing strategic defensive positioning and performance in the outfield](https://link.springer.com/article/10.1007/s10109-021-00367-1)
  + **Speaker**: Charlie Young on Thursday, April 13th at 11 am.
  + **Lab 5** due on 04/14 at 11:59 pm  
  + Project proposal assigned this week, due on 04/21 at 11:59 pm
  
* Week 14 [04/18 (lecture), 04/20 (lab)]
  + **Reading**: course slides
  + **Reading**: CH 8: Career Trajectories in Analyzing Baseball Data with R
  + Project proposal due on 04/21 at 11:59 pm

* Week 15 [04/25 (lecture), 04/27 (lab)]
  + **Reading**: course slides
  + **Reading**: The Full House Model  

* Week 16 [05/02 (lab)]
  + Open house for project discussions
  + Last day of instruction on 05/02


* Projects due on 05/11 at 11:59 pm.


