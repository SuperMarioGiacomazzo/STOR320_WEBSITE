---
title: '**STOR 320: Intro to Data Science**'
geometry: margin=2
output:
  html_document:
    theme: cosmo
    toc: yes
    toc_float: yes
    includes:
      in_header: "favicon.html"
  pdf_document:
    toc: yes
---

<style type="text/css">

div {
color: #13294B;
}

#TOC {
  color: #4B9CD3;
}

.list-group-item.active, .list-group-item.active:focus, .list-group-item.active:hover {
  color: #4B9CD3;
  background-color: #13294B;
}

a:link {
    color: #4B9CD3;
    text-decoration: none;
}

a:visited {
        text-decoration: none;
color: #4B9CD3;

}
a:hover {
color: #4B9CD3;
background-color: #13294B;
}

.main-container {
  max-width: 300px;
  margin-left: auto;
  margin-right: auto;
}

.column {
  float: left;
  width: 50%;
}

.row:after {
  content: "";
  display: table;
  clear: both;
} 

</style>

This course is an application-driven introduction to data science. Statistical and computational tools are valued throughout the modern workplace from Silicon Valley startups, to marine biology labs, to Wall Street firms. These tools require technical skills such as programming and statistics. They also require professional skills such as communication, teamwork, problem solving, and critical thinking.

<div class="row">
  <div class="column">

- Instructor: **[Mario Giacomazzo](http://www.supermariogiacomazzo.com/)**

- Lab Instructors: 
    - **[Ian Ferer](http://stat-or.unc.edu/people/graduate-students-department)**
    - **[Henry Flury](http://stat-or.unc.edu/people/graduate-students-department)**
    - **[Zichao Li](http://stat-or.unc.edu/people/graduate-students-department)**

- Course Syllabus: 
    - **[Section 1](Syllabi/STOR 320 Section 1 Syllabus.pdf)**: TTh, 2:00PM – 3:15PM, Gardner 105
    - **[Section 2](Syllabi/STOR 320 Section 2 Syllabus.pdf)**: TTh, 3:30PM – 4:45PM, Gardner 105

- Lab Sections for Section 1:
    - 320.400: W, 4:40PM - 5:30PM, Hanes 107, Ian
    - 320.401: F 4:40PM - 5:30PM, Hanes 107, Ian
    - 320.402: W, 5:45PM - 6:35PM, Murray G205, Ian
    - 320.403: F, 2:30PM - 3:20PM, Hamilton 452, Henry

- Lab Sections for Section 2:
    - 320.404: W, 4:40PM - 5:30PM, Hanes 112, Zichao
    - 320.405: F, 4:40PM - 5:30PM, Dey 203, Zichao
    - 320.406: F, 5:45PM - 6:35PM, Dey 203, Zichao
    - 320.407: W, 2:30PM - 3:20PM, Peabody 2066, Henry
   
- **[University Approved Absences](https://unc-ch.formstack.com/forms/university_approved_absence_request?sso=5f3154c3179f5)**: Fill out the online form.

- Office Hours and Zoom Links:
    - **[Dr. Mario:](https://unc.zoom.us/j/98181811021)** M, 8AM - 2PM
    - **[Ian:](https://unc.zoom.us/j/6062060059)** Th, 11AM - 1PM
    - **[Hank:](https://unc.zoom.us/j/96051493738)** TTh, 1PM - 2PM
    - **[Zichao:](https://unc.zoom.us/j/6459419100?pwd=UEtCWjc5WUw1K2N2bUM3cFFJNWlzQT09)** F, 2PM - 4PM, Passcode: Stor 320

  </div>
  <div class="column">
  <img src="UNC_Logo.png" width="140px" height="auto">
  </div>
</div> 


# **Course Material**

| Date | Lecture | Slides | Supplement |
|------|---------|--------:|---------:|
|Jan 11| [Introduction](https://uncch.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=70ba750d-6c05-48e2-897d-ae1b000620b5) | [Slides](Lecture/Introduction/Intro_Lecture.pdf)||
|Jan 13|[Data Visualization](https://uncch.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=8ce00bd2-fcae-4599-a617-ae1c013922dd)|[Slides](Lecture/Data Visualization/Data_Visualization_Lecture.pdf)|[Preview](Supplement/Data Visualization/Supplement.html)([.zip](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Supplement/Data%20Visualization/Supplement.zip))|
|| [Workflow in RMarkdown](https://uncch.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=8ce00bd2-fcae-4599-a617-ae1c013922dd) | [Slides](Lecture/Workflow/Workflow_Lecture.pdf) ||
|Jan 18| [Data Transformation I](https://uncch.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=eeb3862a-4620-4329-9f90-ae210151ed11) | [Slides](Lecture/Data Transformation 1/Data_Transformation_1_Lecture.pdf) ||
|| [Data Transformation II](https://uncch.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=eeb3862a-4620-4329-9f90-ae210151ed11) | [Slides](Lecture/Data Transformation 2/Data_Transformation_2_Lecture.pdf)||
|Jan 20| [Data Transformation III](https://uncch.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=29e86eed-823b-45a2-b1eb-ae23013980b6) | [Slides](Lecture/Data Transformation 3/Data_Transformation_3_Lecture.pdf)||
|| [Data Transformation IV](https://uncch.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=29e86eed-823b-45a2-b1eb-ae23013980b6) | [Slides](Lecture/Data Transformation 4/Data_Transformation_4_Lecture.pdf)||
|Jan 25| [Exploratory Data Analysis I](https://uncch.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=0454cc6d-2269-4480-8e31-ae280138aae5)  | [Slides](Lecture/Exploratory Data Analysis 1/Exploratory_Data_Analysis_1_Lecture.pdf) ||
|| [Exploratory Data Analysis II](https://uncch.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=0454cc6d-2269-4480-8e31-ae280138aae5) |[Slides](Lecture/Exploratory Data Analysis 2/Exploratory_Data_Analysis_2_Lecture.pdf)||
|Jan 27| [Final Project I](https://uncch.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=420fd93e-ce8f-422f-8eeb-ae2a0138a8ed) | [Slides](Lecture/Final Project 1/Final_Project_1_Lecture.pdf)||
|| [Data Import](https://uncch.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=420fd93e-ce8f-422f-8eeb-ae2a0138a8ed) | [Slides](Lecture/Data Import/Data_Import_Lecture.pdf)||
|Feb 1|Work on Proposal| [Zoom Link](https://unc.zoom.us/j/98181811021) ||
|Feb 3| [Data Import (Cont.)](https://uncch.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=4ab19c80-513e-4786-b1e5-ae3101397291) | [Slides](Lecture/Data Import/Data_Import_Lecture.pdf)||
|| [Tidy Data 1](https://uncch.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=4ab19c80-513e-4786-b1e5-ae3101397291) | [Slides](Lecture/Tidy Data 1/Tidy_Data_1_Lecture.pdf) ||
|Feb 8| [Tidy Data 2](https://uncch.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=ba5bb3ed-5d47-44c6-b5d4-ae36015205ba) | [Slides](Lecture/Tidy Data 2/Tidy_Data_2_Lecture.pdf) ||
|| [Web Scraping I](https://uncch.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=ba5bb3ed-5d47-44c6-b5d4-ae36015205ba) | [Slides](Lecture/Web Scraping 1/Web_Scraping_1_Lecture.pdf) | [Preview](Supplement/Web Scraping 1/Supplement.html)([.zip](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Supplement/Web%20Scraping%201/Supplement.zip)) |
|Feb 10| [Web Scraping II](https://uncch.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=0848a774-f138-4ed3-b3aa-ae3801529a5c) | [Slides](Lecture/Web Scraping 2/Web_Scraping_2_Lecture.pdf) | [Preview](Supplement/Web Scraping 2/Supplement.html)([.zip](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Supplement/Web%20Scraping%202/Supplement.zip)) |
|Feb 15| [Joins I](https://uncch.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=99d7cb89-8c15-4a1a-ab17-ae3d0139b235) | [Slides](Lecture/Joins 1/Joins_1_Lecture.pdf) ||
|| [Joins II](https://uncch.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=99d7cb89-8c15-4a1a-ab17-ae3d0139b235) | [Slides](Lecture/Joins 2/Joins_2_Lecture.pdf) ||
|Feb 17| [Final Project II](https://uncch.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=370393ce-6326-4327-b04d-ae3f0139779f) | [Slides](Lecture/Final Project 2/Final_Project_2_Lecture.pdf) ||
|Feb 22| [Factors](https://uncch.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=2d1b1095-7de6-4bfe-b3a5-ae440139a60f) | [Slides](Lecture/Factors/Factors_Lecture.pdf) ||
|Feb 24| [Programming I](https://uncch.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=5f316131-4c3c-4a24-9584-ae46013906bf) | [Slides](Lecture/Programming 1/Programming_1_Lecture.pdf) | [Preview](Supplement/Programming 1/Supplement.html)([.zip](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Supplement/Programming%201/Supplement.zip))  |
|Mar 1| [Programming II](https://uncch.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=b9100e86-01e9-44c5-a604-ae4b013923d2) | [Slides](Lecture/Programming 2/Programming_2_Lecture.pdf) | [Preview](Supplement/Programming 2/Supplement.html)([.zip](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Supplement/Programming%202/Supplement.zip)) |
|| [Programming III](https://uncch.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=b9100e86-01e9-44c5-a604-ae4b013923d2) | [Slides](Lecture/Programming 3/Programming_3_Lecture.pdf) ||
|Mar 3| [Modeling 1](https://uncch.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=9121aab6-6218-4d9c-b5f4-ae4d0138f7be) | [Slides](Lecture/Modeling 1/Modeling_1_Lecture.pdf) ||
|Mar 8| [Programming III (Cont.)](https://uncch.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=b66c5423-dff1-44d8-b81c-ae52013a386b) | [Slides](Lecture/Programming 3/Programming_3_Lecture.pdf) ||
|| [Modeling 1 (Cont.)](https://uncch.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=b66c5423-dff1-44d8-b81c-ae52013a386b) | [Slides](Lecture/Modeling 1/Modeling_1_Lecture.pdf) ||
|Mar 10| [Modeling 2](https://uncch.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=f20e2410-513c-4cd8-8e54-ae540139946a) | [Slides](Lecture/Modeling 2/Modeling_2_Lecture.pdf) | [Preview](Supplement/Modeling 2/Supplement.html)([.zip](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Supplement/Modeling%202/Supplement.zip)) |
|March 15|Spring Brizzeak|||
|March 17|Spring Brizzeak|||
|Mar 22|Work on EDA|||
|Mar 24| Modeling 3 | [Slides](Lecture/Modeling 3/Modeling_3_Lecture.pdf) | [Preview](Supplement/Modeling 3/Supplement.html)([.zip](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Supplement/Modeling%203/Supplement.zip)) |
|| Modeling 4 | [Slides](Lecture/Modeling 4/Modeling_4_Lecture.pdf) | [Preview](Supplement/Modeling 4/Supplement.html)([.zip](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Supplement/Modeling%204/Supplement.zip)) |
|Mar 29| Modeling 5 | [Slides](Lecture/Modeling 5/Modeling_5_Lecture.pdf) ||
|Mar 31| Modeling 6 | [Slides](Lecture/Modeling 6/Modeling_6_Lecture.pdf) ||
|Apr 5| Modeling 7 | [Slides](Lecture/Modeling 7/Modeling_7_Lecture.pdf) | [Preview](Supplement/Modeling 7/Supplement.html)([.Rmd](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Supplement/Modeling%207/Supplement.zip)) |
|Apr 7| Modeling 8 | [Slides](Lecture/Modeling 8/Modeling_8_Lecture.pdf) | [Preview](Supplement/Modeling 8/Supplement.html)([.Rmd](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Supplement/Modeling%208/Supplement.zip)) |
|Apr 12| Modeling 9 | [Slides](Lecture/Modeling 9/Modeling_9_Lecture.pdf) | [Preview](Supplement/Modeling 9/Supplement.html)([.Rmd](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Supplement/Modeling%209/Supplement.zip)) |
|Apr 14| Best Thursday |||
|Apr 19| R Shiny | [Slides](Lecture/R Shiny/R_Shiny_Lecture.pdf) | Preview([.zip](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Supplement/R%20Shiny/Supplement.zip))|
|Apr 21| Data Ethics | [Slides](Lecture/Data Ethics/Data_Ethics_Lecture.pdf) ||
|Apr 26|Work on Final Paper|||
|||||

# **Lab Schedule**

| Date | 320.400 | 320.401 |320.402| 320.403 | 320.404 | 320.405 | 320.406 | 320.407 |
|----------|:--------:|:--------:|:--------:|:--------:|:--------:|:--------:|:--------:|:--------:|
| Jan 19 | Lab 1  |  | Lab 1  |  | Lab 1 |  |  | Lab 1   |
| Jan 21 |   | Lab 1  |   | Lab 1 |  | Lab 1  |  Lab 1  |   |
| Jan 26 | Lab 2  |  | Lab 2  |  | Lab 2 |  |  | Lab 2   |
| Jan 28 |   | Lab 2  |   | Lab 2 |  | Lab 2  |  Lab 2  |   |
| Feb 2 | Lab 3  |  | Lab 3  |  | Lab 3 |  |  | Lab 3   |
| Feb 4 |   | Lab 3  |   | Lab 3 |  | Lab 3  |  Lab 3  |   |
| Feb 9 | Lab 4  |  | Lab 4  |  | Lab 4 |  |  | Lab 4   |
| Feb 11 |   | Lab 4  |   | Lab 4 |  | Lab 4  |  Lab 4  |   |
| Feb 16 | Lab 5  |  | Lab 5  |  | Lab 5 |  |  | Lab 5   |
| Feb 18 |   | Lab 5  |   | Lab 5 |  | Lab 5  |  Lab 5  |   |
| Feb 23 | Lab 6  |  | Lab 6  |  | Lab 6 |  |  | Lab 6   |
| Feb 25 |   | Lab 6  |   | Lab 6 |  | Lab 6  |  Lab 6  |   |
| Mar 2 | Lab 7  |  | Lab 7  |  | Lab 7 |  |  | Lab 7   |
| Mar 4 |   | Lab 7  |   | Lab 7 |  | Lab 7  |  Lab 7  |   |
| Mar 9 | Lab 8  |  | Lab 8  |  | Lab 8 |  |  | Lab 8   |
| Mar 11 |   | Lab 8  |   | Lab 8 |  | Lab 8  |  Lab 8  |   |
| Mar 23 | Lab 9  |  | Lab 9  |  | Lab 9 |  |  | Lab 9   |
| Mar 25 |   | Lab 9  |   | Lab 9 |  | Lab 9  |  Lab 9  |   |
| Mar 30 | Lab 10  |  | Lab 10  |  | Lab 10 |  |  | Lab 10   |
| Apr 1 |   | Lab 10  |   | Lab 10 |  | Lab 10  |  Lab 10  |   |
| Apr 6 | Lab 11  |  | Lab 11  |  | Lab 11 |  |  | Lab 11   |
| Apr 8 |   | Lab 11  |   | Lab 11 |  | Lab 11  |  Lab 11  |   |
| Apr 20 | Lab 12  |  | Lab 12  |  | Lab 12 |  |  | Lab 12   |
| Apr 22 |   | Lab 12  |   | Lab 12 |  | Lab 12  |  Lab 12  |   |
||||||||||

# **Assignment Tracker**

All HW and Analysis assignments are to be submitted via **[Sakai](https://sakai.unc.edu/welcome/)**. Unzip folder and complete your homework using Rmd file. The table below shows all the assignments sorted by Due Date

| Assigned | Lab (L) |Homework (HW) | Analysis (A) | Due Date (Time) |
|----------|:--------:|:--------:|:-------------:|----------------:|
| Jan 13  | |[HW1](Homework/HW 1/HW-1.html)([.zip](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Homework/HW%201/HW%201.zip))       |       |   Jan 20 (5:00 PM)     |
| Jan 19  | [L1](Lab/Lab 1/Lab-1.html)([.zip](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Lab/Lab%201/Lab%201.zip))|       |       |    Jan 26 (2:30 PM)     |
| Jan 20  | |[HW2](Homework/HW 2/HW-2.html)([.zip](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Homework/HW%202/HW%202.zip))       |       |   Jan 27 (5:00 PM)     |
| Jan 26  | [L2](Lab/Lab 2/Lab-2.html)([.zip](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Lab/Lab%202/Lab%202.zip))|       |       |     Feb 2 (2:30 PM)     |
| Feb 2  | [L3](Lab/Lab 3/Lab-3.html)([.zip](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Lab/Lab%203/Lab%203.zip))|       |       |     Feb 9 (2:30 PM)     |
| Jan 27  |      |  | [A1](Analysis/Analysis 1/Analysis-1.html)([.zip](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Analysis/Analysis%201/Analysis%201.zip))      |     Feb 10 (5:00 PM)     |
| Feb 9  | [L4](Lab/Lab 4/Lab-4.html)([.zip](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Lab/Lab%204/Lab%204.zip))|       |       |     Feb 16 (2:30 PM)     |
| Feb 10  | |[HW3](Homework/HW 3/HW-3.html)([.zip](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Homework/HW%203/HW%203.zip))       |       |    Feb 17  (5:00 PM)     |
| Feb 16  | [L5](Lab/Lab 5/Lab-5.html)([.zip](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Lab/Lab%205/Lab%205.zip))|       |       |     Feb 23 (2:30 PM)     |
| Feb 17 | |[HW4](Homework/HW 4/HW-4.html)([.zip](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Homework/HW%204/HW%204.zip))       |                    |   Feb 24 (5:00 PM)     |
| Feb 23  | [L6](Lab/Lab 6/Lab-6.html)([.zip](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Lab/Lab%206/Lab%206.zip))|       |       |     Mar 2 (2:30 PM)     |
| Mar 2  | [L7](Lab/Lab 7/Lab-7.html)([.zip](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Lab/Lab%207/Lab%207.zip))|       |       |     Mar 9 (2:30 PM)     |
| Feb 24  | | | [A2](Analysis/Analysis 2/Analysis-2.html)([.zip](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Analysis/Analysis%202/Analysis%202.zip))        |    Mar 10 (5:00 PM)     |
| Mar 9  | [L8](Lab/Lab 8/Lab-8.html)([.zip](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Lab/Lab%208/Lab%208.zip))|       |       |     Mar 23 (2:30 PM)     |
| Mar 10  | |[HW5](Homework/HW 5/HW-5.html)([.zip](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Homework/HW%205/HW%205.zip))       |       |    Mar 24 (5:00 PM)     |
| Mar 23  | [L9](Lab/Lab 9/Lab-9.html)([.zip](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Lab/Lab%209/Lab%209.zip))|       |       |     Mar 30 (2:30 PM)     |
| Mar 24  | | | [A3](Analysis/Analysis 3/Analysis-3.html)([.zip](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Analysis/Analysis%203/Analysis%203.zip))                  |   Mar 31 (5:00 PM)     |
| Mar 30  | [L10]()([.zip]())|       |       |     Apr 6 (2:30 PM)     |
| Mar 31  | |[HW6](Homework/HW 6/HW-6.html)([.zip](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Homework/HW%206/HW%206.zip))       |        |    Apr 7 (5:00 PM)     |
| Apr 6  | [L11]()([.zip]())|       |       |     Apr 13 (2:30 PM)     |
| Apr 7  | |[HW7](Homework/HW 7/HW-7.html)([.zip](https://github.com/SuperMarioGiacomazzo/STOR320_WEBSITE/raw/master/Homework/HW%207/HW%207zip))       |                   |   Apr 14 (5:00 PM)     |
| Apr 14  |   |   | [A4]()([.zip]())          |   Apr 21 (5:00 PM)     |
| Apr 20  | [L12]()([.zip]())|       |       |     Apr 27 (2:30 PM)     |
|          |   |     |             |           |      




# **Final Project Details**

For the final project, each section of STOR 320 will be divided (ideally) into research groups of size 5 or 6. To ensure fairness, students will be assigned randomly based on the `sample` function in **R**. If you don't get in a group you like, try being better at luck.

To find your research group, see the tables below:

## Group Assignments for Section 1

<!--html_preserve--><div id="htmlwidget-ed873d8a0454c693baf7" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-ed873d8a0454c693baf7">{"x":{"filter":"none","data":[["Lab 400","Lab 400","Lab 400","Lab 400","Lab 400","Lab 400","Lab 400","Lab 400","Lab 400","Lab 400","Lab 400","Lab 400","Lab 400","Lab 400","Lab 400","Lab 400","Lab 400","Lab 400","Lab 400","Lab 400","Lab 400","Lab 400","Lab 400","Lab 400","Lab 400","Lab 400","Lab 400","Lab 400","Lab 400","Lab 400","Lab 400","Lab 401","Lab 401","Lab 401","Lab 401","Lab 401","Lab 401","Lab 401","Lab 401","Lab 401","Lab 401","Lab 401","Lab 401","Lab 401","Lab 401","Lab 401","Lab 401","Lab 401","Lab 402","Lab 402","Lab 402","Lab 402","Lab 402","Lab 402","Lab 402","Lab 402","Lab 402","Lab 402","Lab 402","Lab 402","Lab 402","Lab 402","Lab 402","Lab 402","Lab 402","Lab 402","Lab 402","Lab 402","Lab 402","Lab 402","Lab 402","Lab 402","Lab 402","Lab 402","Lab 403","Lab 403","Lab 403","Lab 403","Lab 403","Lab 403","Lab 403","Lab 403","Lab 403","Lab 403","Lab 403","Lab 403","Lab 403","Lab 403","Lab 403","Lab 403","Lab 403","Lab 403","Lab 403","Lab 403","Lab 403","Lab 403","Lab 403","Lab 403"],["Darnell, Cade","Martin, William","Bacon, Katherine","Reid, Alexander","Le, Long","Ambardar, Avish","Przykucki, Emily","Paschal, Sarah","Wang, Hongze","Patel, Pooja","Myoung, Sooho","Coffey, Duncan","Kazazian, Haig","Brown, Bryson","Xu, Zoe","Hassell, Miles","Eubanks, Miles","Callihan, Lindsay","Dean, Tyler","Hwang, Jack","Coleman, Rachel","Chung, Shen","Boyd, Hunter","Walker, Max","Duffy, Madison","Brain, Irena","Boudreau, Katherine","Liu, Andy","Yu, Junyan","Willse, Jada","Srebnik, Emma Rudy","Lan, Kaiwen","Binley, Dylan","Arora, Manas","Chen, Andy","Jimenez-Lemus, Uriel","Wang, Vivi","Buchner, Bradley","Ross, Alex","Gasia, Carter","Do, Linh","Nguyen, Anh","Yang, Andrew","Liu, Shihang","De Oliveira, Leonardo","Yang, Thea","Cooper, Brian","Huang, Zeyan","Kang, Minsoo","Low, Simon","Brown, Hannah","Wang, Kiki","Winborne, Paul","Miller, Eric","Wood, Ethan","Westmoreland, Kara","Kasuganti, Sarika","Sun, Mingling","Dickinson, John","Yao, Yudi","Almodovar, Fabian","Helms, Ryan","Thomas, Athena","Rao, Arya","Yang, Yuan","Gill, Keerthan","Vora, Rohan","Selle, John","Kacvinsky, Matt","Wall, Avery","Mankad, Akshay","Sigafose, Sig","Stringer, Wilson","Bryant, Isaac","Santana-Garcia, Daniel","Ye, Tianrui","Malone, Elizabeth","Cao, Zhi","Gandhi, Kavish","Garg, Ananya","Almvide Lundberg, David","Mahavadi, Soumya","Murali, Anita","Lindogan, Peyton","Pai, Shefali","Brooks, Sarah","Hu, Wenqiong","Fan, Yiyang","Zhang, Zihan","Straight, Julia","Burroughs, Joseph","Sudhini, Rindha","Groce, Carson","Sridhar, Koushik","Steiner, Annabel","Hyde, John","Barrow, Julia","Shi, Yujia"],[1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5,6,6,6,6,6,6,7,7,7,7,7,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,10,10,11,11,11,11,11,12,12,12,12,12,13,13,13,13,13,14,14,14,14,14,14,15,15,15,15,15,15,16,16,16,16,16,16,17,17,17,17,17,17,18,18,18,18,18,18],["O","C","D","I","O","O","O","I","D","C","I","O","C","O","D","O","D","C","O","I","I","D","O","O","C","I","I","D","O","C","O","I","C","O","O","D","I","I","O","O","C","D","I","D","O","O","C","I","D","O","O","C","I","I","D","O","O","C","I","D","O","O","C","I","D","C","O","O","I","D","O","C","O","I","O","I","O","D","I","C","I","I","D","O","O","C","O","I","I","D","C","O","D","I","O","I","O","C"],["3/21/2022","3/21/2022","3/21/2022","3/21/2022","3/21/2022","","","","","","","","","","","","","","","","3/25/2022","3/25/2022","3/25/2022","3/25/2022","3/25/2022","","","","","","","","","","","","","","","","","","","","","","","","","","","","","3/30/2022","3/30/2022","3/30/2022","3/30/2022","3/30/2022","","","","","","","","","","","","","","","","","3/23/2022","3/23/2022","3/23/2022","3/23/2022","3/23/2022","3/23/2022","","","","","","","","","","","","","","","","","",""],["12:00PM - 12:10PM","12:00PM - 12:10PM","12:00PM - 12:10PM","12:00PM - 12:10PM","12:00PM - 12:10PM","","","","","","","","","","","","","","","","9:00AM - 9:10AM","9:00AM - 9:10AM","9:00AM - 9:10AM","9:00AM - 9:10AM","9:00AM - 9:10AM","","","","","","","","","","","","","","","","","","","","","","","","","","","","","11:00AM - 11:10AM","11:00AM - 11:10AM","11:00AM - 11:10AM","11:00AM - 11:10AM","11:00AM - 11:10AM","","","","","","","","","","","","","","","","","1:30PM - 1:40PM","1:30PM - 1:40PM","1:30PM - 1:40PM","1:30PM - 1:40PM","1:30PM - 1:40PM","1:30PM - 1:40PM","","","","","","","","","","","","","","","","","",""]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th>Lab<\/th>\n      <th>Name<\/th>\n      <th>Group<\/th>\n      <th>Role<\/th>\n      <th>EDA.Date<\/th>\n      <th>EDA.Time<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"scrollX":false,"columnDefs":[{"className":"dt-center","targets":[0,1,2,3,4,5]}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

## Group Assignments for Section 2

<!--html_preserve--><div id="htmlwidget-f091ec3f9f16d0d938aa" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-f091ec3f9f16d0d938aa">{"x":{"filter":"none","data":[["Lab 404","Lab 404","Lab 404","Lab 404","Lab 404","Lab 404","Lab 404","Lab 404","Lab 404","Lab 404","Lab 404","Lab 404","Lab 404","Lab 404","Lab 404","Lab 404","Lab 404","Lab 404","Lab 404","Lab 404","Lab 404","Lab 404","Lab 404","Lab 404","Lab 404","Lab 404","Lab 404","Lab 404","Lab 404","Lab 404","Lab 405","Lab 405","Lab 405","Lab 405","Lab 405","Lab 405","Lab 405","Lab 405","Lab 405","Lab 405","Lab 405","Lab 405","Lab 405","Lab 405","Lab 405","Lab 405","Lab 405","Lab 405","Lab 405","Lab 405","Lab 405","Lab 405","Lab 405","Lab 405","Lab 406","Lab 406","Lab 406","Lab 406","Lab 406","Lab 406","Lab 406","Lab 406","Lab 406","Lab 406","Lab 406","Lab 406","Lab 406","Lab 406","Lab 406","Lab 407","Lab 407","Lab 407","Lab 407","Lab 407","Lab 407","Lab 407","Lab 407","Lab 407","Lab 407","Lab 407","Lab 407","Lab 407","Lab 407","Lab 407","Lab 407","Lab 407","Lab 407","Lab 407","Lab 407","Lab 407","Lab 407","Lab 407","Lab 407","Lab 407","Lab 407","Lab 407","Lab 407","Lab 407","Lab 407"],["Ma, Erin","Xiao, Jiyuan","Morton, Alexander","Mcdowell, Allison","Ahmed, Ghasan","Li, Zijun","Kenney, Ryan","Zhang, Aijia","Calikoglu, Serdar","Dulaney, Casey","Zhang, Weiye","Mccraw, Christian","Tuz, Alex","Mukherji, Nishtha","Mo, Junchao","Muckenfuss, Robert","Rammani, Davindra","Le, Trang","Goroshnik, Benjamin","Garine, Amit","Dong, Andy","Gonzalez, Isabella","Wynne, Matthew","Manion, Jeffrey","Polsky, Matthew","Yilmaz, Beliz","Schmitt, Logan","Rahman, Rashika","Lavelle, Kristen","Rodriguez Olmedo, Jesus-Javier","Walavalkar, Akshay","Le, Thi","Hu, Jing","Mohamed, Halima","Schmitt, Patrick","Bean, Jordan","Yang, Jiacheng","So, Jay","Vaught, William","Nassif, David","Franklin, Walker","Dumpala, Aniketh","Best, Tristen","Li, Kaiyan","Hellwege, Dalton","Wu, Minglong","Jones, Thomas","Singh, Simi","Nemani, Rohit","Kadackal, Amelie","Baker, Carson","Fagan, Conor","Liu, Peiyi","Weindorf, Lawrence","Lim, Prisca","Marcellus, Jack","Zhou, Xiangyu","Woody, Lauren","Gao, Shan","Hu, Yue","Qiu, Yutong","Xu, Haorong","Sun, Hao","Iyer, Megha","Nemani, Karthik","Gandecha, Ajay","Chang, Iris","Zheng, Franklin","Xu, Yinglei","Pimentel, Alexander","Ventura, Cole","Hynes, Jenna","Smith, Ryan","Gotur, Shreyas","Khismatova, Ideliya","Stanley, Sherin","Cochell, Kenneth","Jaikumar, Sneha","Chao, Judy","Ma, Xinyi","Qi, Xiaolin","Huang, Adam","Alapati, Jahnavi","Kshirsagar, Siona","Ozhakanat, Alex","Wooster, Sarah","Patil, Rishab","Shelton, Mallory","Hawkins, Haley","Hoglund, James","Chen, Seanna","Philizaire, Mishka","Wingo, Abby","Clauser, William","Gil, Andrew","Moskalik, Kathleen","Shoemaker, Macon","Su, Gloria","Liu, Joyce"],[1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6,7,7,7,7,7,7,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,10,10,11,11,11,11,11,12,12,12,12,12,13,13,13,13,13,13,14,14,14,14,14,14,15,15,15,15,15,15,16,16,16,16,16,16,17,17,17,17,17,17],["I","C","D","O","O","I","O","D","I","C","I","O","C","O","I","O","I","D","D","I","I","O","C","O","O","C","I","O","I","D","D","O","C","I","I","O","D","I","O","I","C","O","D","I","O","C","O","I","","I","C","D","O","O","O","O","D","I","C","C","O","D","I","O","O","O","I","C","D","D","O","O","I","C","I","C","O","D","I","O","O","D","C","I","O","I","O","I","C","O","D","I","O","O","O","I","I","D","C"],["","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","3/21/2022","3/21/2022","3/21/2022","3/21/2022","3/21/2022","3/21/2022","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","3/21/2022","3/21/2022","3/21/2022","3/21/2022","3/21/2022","3/21/2022","","","","","","","","","","","","","","","","","","","","","","","",""],["","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","11:00AM - 11:10AM","11:00AM - 11:10AM","11:00AM - 11:10AM","11:00AM - 11:10AM","11:00AM - 11:10AM","11:00AM - 11:10AM","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","12:50PM - 1:00PM","12:50PM - 1:00PM","12:50PM - 1:00PM","12:50PM - 1:00PM","12:50PM - 1:00PM","12:50PM - 1:00PM","","","","","","","","","","","","","","","","","","","","","","","",""]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th>Lab<\/th>\n      <th>Name<\/th>\n      <th>Group<\/th>\n      <th>Role<\/th>\n      <th>EDA.Date<\/th>\n      <th>EDA.Time<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"scrollX":false,"columnDefs":[{"className":"dt-center","targets":[0,1,2,3,4,5]}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

## Four Roles

Although everyone is responsible for the entire project, each member of the group will be assigned a specific role for accountability and consistency. These four specific roles are described as follows:

- **The Creator:** Schedule and Meet with Dr. Mario to Propose Your Group's Research Idea, Lead Designer in Slides

- **The Interpreter(s):** Schedule and Meet with Dr. Mario to Share Findings from Exploratory Analysis, Evaluate Practice Presentation

- **The Orator(s):** Give a Captivating 3-5 Minute Slideshow Presentation During Final Exam Day

- **The Deliverer:** Deliver Your Group from Evil by Editing and Submitting all Deliverables to Sakai

## Four Parts Including Point Values

This final project will be divided into four parts worth a total of 100 points. Each part will have a clear rubric as non-subjective as possible. The parts along with total point values are found below:

* **[P1: Project Proposal](Final Project/Project-Proposal.html)** *(10 Points)*
    + [Template](Final Project/Project-Proposal-Template.html)([.Rmd](https://drive.google.com/file/d/1m5pgDs0NvqdgSMkMR18OCWL6l1egqqYm/view?usp=sharing))
    + [Data World](https://data.world/)
    + [US Government Data](https://www.data.gov/)
    + [Kaggle](https://www.kaggle.com/)
    + [Free Data Info I](https://www.forbes.com/sites/bernardmarr/2016/02/12/big-data-35-brilliant-and-free-data-sources-for-2016/#63f06f60b54d)
    + [Free Data Info II](https://r-dir.com/reference/datasets.html)


* **[P2: Exploratory Data Analysis](Final Project/Exploratory_Data_Analysis.html)** *(20 Points)*
    + [Template](Final Project/EDA-Template.html) ([.Rmd](https://drive.google.com/file/d/1zNI0hX9CTg9d77mHOKxebgdwDEmqYebf/view?usp=sharing))
    
* **[P3: Final Written Paper](Final Project/Final-Paper.html)** *(40 Points)*
    + [Template](Final Project/Final-Paper-Template.html)([.Rmd](https://drive.google.com/file/d/19oG11IaUnAkPe8BUE7SAXS2BOeVLIZdT/view?usp=sharing))
    
* **[P4: Final Presentation](Final Project/Final_Presentation.html)** *(30 Points)*

    
## Due Dates of Individual Parts

| Part | Description |Method of Submission| Involvement Survey |Due Date (Time) |
|------|-------------|-------------------|-------------------|----------------:|
| P1  | Project Proposal| Zoom + Sakai | [Survey 1](https://docs.google.com/forms/d/e/1FAIpQLScslh7rwnjd2VZ99VH66_4PJM7c8oJ_4olJt7-34hRW82Nq2A/viewform?usp=sf_link) |February 10 (11:59PM)|
| P2  | Exploratory Data Analysis | Zoom + Sakai | [Survey 2](https://docs.google.com/forms/d/e/1FAIpQLScY8EYu2GtgiHcbP3kVHGosSCx89rIc76WgW_Tx950HMzLM8g/viewform?usp=sf_link) |March 24 (11:55PM)|
| P3  | Final Written Paper|Sakai| [Survey 3](https://docs.google.com/forms/d/e/1FAIpQLSdQMrAQV_pjERKMsV3mXyC4unrdrlfFVQ_MYWhKma7rP1yo1A/viewform?usp=sf_link) |April 26 (11:55PM)|
| P4  | Final Presentation (Section 1) |Sakai + Class| [Survey 4](https://docs.google.com/forms/d/e/1FAIpQLSdTW_KLZWWWVpNWVudyXXwkQztOUJmZdlcFtJZovoTzRwfwLw/viewform?usp=sf_link) | May 5 (3:00PM) |
| P4  | Final Presentation (Section 2) |Sakai + Class| [Survey 4](https://docs.google.com/forms/d/e/1FAIpQLSdTW_KLZWWWVpNWVudyXXwkQztOUJmZdlcFtJZovoTzRwfwLw/viewform?usp=sf_link) | April 30 (7:00PM) |
||||

## Above Average Final Projects

- **[Pokemon](Final Project/SP2019_EX1.html)**
- **[Seattle Housing](Final Project/SP2019_EX2.html)**
- **[Missing Migrants](Final Project/SU2019_EX1.html)**
- **[Youtube](Final Project/FA2019_EX1.html)**
- **[CDC](Final Project/FA2019_EX2.html)**


# **Reading**

- **[R for Data Science](R for Data Science- Import, Tidy, Transform, Visualize, and Model Data.pdf)** (R4DS)

- **[R Programming for Data Science](Using R at UNC for Mac Users.pdf)** (RP4DS)

- **[The Art of R Programming](The Art of R Programming.pdf)** (AoRP)

- **[A First Course in Statistical Programming with R](A First Course in Statistical Programming with R.pdf)** (FCSPR)

- **[ModernDive](https://moderndive.com/index.html)** (MD)

- **[An Introduction to Statistical Learning](An Introduction to Statistical Learning.pdf)** (ISLR)


# **Additional resources**

- **[Installing R](https://cran.r-project.org/)**

- **[Installing R Studio](https://www.rstudio.com/products/rstudio/#Desktop)**

- **[Additional Necessary Tools for Mac Users](https://mac.r-project.org/tools/)**

- **[Instructions for Accessing R Studio on the UNC Server](https://drive.google.com/file/d/1ATZO-wpuqzHbXVnJqm7wXpt-yfMbo2Jy/view?usp=sharing)**

- **[Rmarkdown Cheat Sheet](Cheat Sheet/rmarkdown.pdf)**

- **[ggplot2 Cheat Sheet](Cheat Sheet/data-visualization.pdf)**

- **[Color Options](https://paulvanderlaken.files.wordpress.com/2017/08/ggplot2-color-colours-names-cheatsheet.png?w=559&amp;h=1024)**

- **[Data Wrangling Cheat Sheet](https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf)**

- **[Regular Expressions Cheat Sheet](https://rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf)**

- **[R Shiny Cheat Sheet](https://shiny.rstudio.com/images/shiny-cheatsheet.pdf)**

This page was last updated on 2022-03-21 21:10:41 Eastern Time.
