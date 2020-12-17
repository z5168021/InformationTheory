# BINF6112 project
Developed by:
- z5168021 – Maxine Halbheer
- z5207750 – Eric Urng
- z5165326– Sanjana Eluru
- z5164906 – Sheril Jose Sao

## Installation Process
This application can be accessed via two methods depending on how the user wishes to use
it. The application can be readily accessed and used through a web link or alternatively by
downloading and hosting on a private server. Both methods are detailed below. 

### Accessing from web
This educational tool is readily available for the public. The product has been deployed to the
shinyapps.io server which is freely accessible. This method does not have dependencies other
than access to the internet. The web address is:
 https://evolutionaryecology.shinyapps.io/learningEE

### Hosting from a private server.
The product is on GitHub at https://github.com/z5168021/InformationTheory
Please contact z5168021@student.unsw.edu.au or z5164906@student.unsw.edu.au if you
have any questions regarding setting up and accessing the web app.

To host the app on your own device you must run it through R-Studio (https://rstudio.com/products/rstudio/download/). 
Once in the applicationyou can select the New Project field from the File drop down menu. After 
selecting the link amodule will appear, select the Version Control option and then Git Repository. 
Enter the URLof the repository and select Create Project. R-Studio will clone all files into the 
directory specified.

Once the repository is cloned the following libraries/packages must be installed:
Install the following packages
```
install.packages(“shiny”)
install.packages(“shiny.router”)
install.packages(“ggplot2”)
install.packages(“RColorBrewer”)
install.packages(“rmarkdown”)
install.packages(“shinyFeedback”)
install.packages(“knitr”)
install.packages(“DT”)
install.packages(“shinyBS”)
install.packages(“plotly”)`
```
Once these packages have been installed the application is ready to be run. This can be done
by simply running the app.R file and selecting the ‘Run App’ button on the top right corner of
the screen. This should open a browser page taking you to the homepage.


## Usage

### Introduction
The application is designed to be a teaching tool to increase the users understanding of the
effects of time on genetic variation. The application contains a variety of modelling tools which
can be used to explore evolutionary ecology. Due to this application being a teaching tool, the
users understanding and ease of use is highly important. 
Usage
#### Basic Info
The default screen contains summarised background information for the equations used by
the application. This page also provides links to the research papers which the equations used
by the application have been taken form. To further assist the user, detailed help videos on
how to use each function can be found in the ‘Instructional Video’ tab under the relevant
headings.
#### Sample populations
The application has a small database of sample populations that the user can use to test and
explore each of the calculators. These populations contain different distributions of haplotypes
which produce interesting results. Information about these sample populations can be found
in the sample population tab. The user can download FASTA files of each of these populations
from this tab or either of the calculators from the view population module.

### Calculators and Tools
#### Build a Population
This tab allows the user to design their own model populations of any size and haploid
distribution. To use this function the user must first enter the name of the population. The
output FASTA file will also be named this.
The user can then enter a nucleotide sequence in the input field labelled ‘Enter Nucleotide
Sequence’. Note that the sequence can only contain A, G, C, or T’s if the input contains any
other character an error message will be produced. The user can then enter the number of
occurrences of that haplotype they would like to observe in their population. This number has
to be an integer greater than zero. This number is set to 1 by default. 

After entering the initial sequence, the user selects the ‘Add New Haplotype’ button. This will
generate a new row of input fields. The ‘Nucleotide Sequence’ field will already contain a
default nucleotide sequence which is mutated from the initial input sequence. The user is able
to modify this sequence however they wish. This process can be repeated until the initial
sequence can no longer be used to produce a unique haplotype. As the user is creating their 
population a histogram displaying the haplotype distribution can be observed on the right. This
will update every time the new haplotype button is selected.
Once the user is satisfied with their population, they can download a FASTA file containing
the population data by selecting the ‘Generate File’ button. The FASTA file contains each of
the haplotypes repeated the number of times specified by the user. The sequences are
labelled as follows; “<population_name>.<num>”

#### qD-Profile 
This page allows the user to explore the equations responsible for entropy and qd-values to
extend their understanding of q-profiles and their effectiveness for analysing genetic diversity.
##### SNP (Single View)
###### Input
The user can modify the distribution of nucleotides within a population with sliders positioned
on the left. Optionally, the maximum values for each nucleotide can also be modified by
clicking “Settings”. This will create a modal with entry fields for the new maximums for each
nucleotide. Their respective maximums will be modified after entering a new value and clicking
“Update”.
###### Results
The q-profile for the sample data inputted will appear on the right-hand side. This bar plot will
update dynamically as values given by the sliders are modified. Similarly, equations for
entropy values and qD-values are displayed and change dynamically as well.
##### SNP (Comparative View)
###### Input
The user can modify sample data by modifying slider positions. Clicking “Add” will record the
sample distribution within a table.
###### Results
The user can click “Show Response” to generate the q-profiles. In this view, the q-profiles will
be superimposed, for ease of comparison. Deselecting a sample from the plot’s legend will
hide the q-profile for that sample.
###### Reset
The user can click “Reset” to reload the application. Note that this will cause sample data to
be lost.
##### Haplotype (Single View)
###### Input
The user is provided with 4 sample population distributions. Optionally, the user can upload a
FASTA file with their own population distribution. Viewing the spread of the population is
possible by clicking “View Population”
###### Results
A q-profile for the chosen population will be dynamically created as the input data changes.

##### Haplotype (Comparative View)
###### Input
Similarly, the user is again given 4 sample populations with the option to supply their own.
Each time a population is added, it is recorded in a table. The table contains the fields
population name and type. Type refers to whether the population data was supplied by the
user) or by default (false).
###### Results
Clicking “Generate” will display the q-profiles for each sample superimposed. Again,
deselecting sample names in the legend will cause them to no longer show.

##### Download
For each mode, SNP or haplotype, and view, single or comparative, the user can download
the results. The downloaded file format can be either HTML or Word. By default, the file will
contain the methods and equations used to generate a q-profile and the qD-values for the
relevant sample(s). Optionally, the user can select to include the nucleotide distribution and
the graph of the q-profile. This will insert a bar plot of the sample distributions and a bar plot
of the q-profile into the default download format. 

#### Variation Over Time
##### Input
This table allows the user to observe how Shannon molecular diversity changes over time.
This mode uses haploid population data. The user can choose to select a population from the
drop-down menu or provide their own data in the form of a FASTA file. Note that when
supplying a file it must end in either ‘.fasta’ or ‘.FNA’. To view the population distribution the
‘View Population' button can be selected. This will produce a modal with a bar graph of the
haplotype distribution. If the population is a sample population, there is also an option to
download the file.
Once a population is selected the sliders can be used to select the Effective Population size
and the Number of Generations. There is also a third slider which allows the user to select
what range of generations should be plotted. If the user wishes to update any of the maximums
of these parameters, they can select the ‘Settings’ button. After altering the parameter in the
relevant field, the Update button should be selected. After exiting the setting modal, the
maximums are updated.
##### Results
Once satisfied with the input the ‘Calculate’ button can be selected if the user has selected
Single Population the results will appear on the right of the screen. Altering the Effective
population and Number of generation sliders will dynamically update the results.
If Comparative view is selected the selected parameters will appear in a table on the right
under the Results heading. The user can then change the parameters and add new entries to
the table. Once satisfied with the table the user can select Plot results, this will create a plot
with all the samples for the range specified.
##### Download
Under the Results for both modes the download options are visible. The user can select the
relevant information they wish to download. The file will be called ‘my-Report.HTML’ or ‘myReport.word’ depending on the file type selected.


