# Visualisations of Popularity
## Investigating popularity trends across music features


# Popularity visualisations for INF4000
 
<img 
src = "https://raw.githubusercontent.com/G-Imola/Visualisations-of-Popularity/main/Plots/Readme%20composite%20visualisation.png"
 alt = "GitHub README Image">

## Introduction

#### _An overview of popularity trends and musical features across various genres_

Identifying trends in popular music can provide insight on what is expected of a 'hit' song, and can lead to higher commercial power in the music industry.
Although the popularity of a song is inherently contextual, various academic studies have highlighted that musical features can determine success[1] 

This project intended to generate a composite visualisation to display popularity trends in relation to various musical features and genres. 
The specific research questions were listed as follows:

1. What features are most prevalent across various popularity categories?
2. What features distinguish high – and low – popularity tracks?


## Sourcing

The dataset was sourced from the <a href="https://hf-proxy-cf.effarig.site/datasets/maharshipandya/spotify-tracks-dataset" target="_blank"> Maharshipandya Dataset</a>.

The dataset contains 114,000 tracks with distinct musical features spanning across 114 genres, with each track containing unique numerical features with ranging scales and values.

## Packages

The investigation was carried out using R programming language Version 4.4.1 (2024-06-14 ucrt) and software  RStudio Desktop for winOS version 2024.12.0+467. Besides the base R packages, the following packages were also used:

* `tidyverse`
* `dplyr`
* `RColorBrewer`
* `scales`
* `ggalt`
* `GGally`
* `ggtext`
* `gridExtra`
* `forcats`
* `extrafont`
* `devtools`
  * `ricardo-bion/ggradar`
<br>
<details>
  <summary><h2>Prerequisites and instructions</h2></summary>

### 1. **Prerequisites**

Before running the code, ensure the following software is installed:

* R(Version 4.0 or later)
* RStudio (Integrated Development Environment, **IDE**)
* Git (to clone the repository)


### **2.Clone the repository and verify branch**

Download the project files by cloning the repository.
This can be performed by running the following command
in your **IDE**:

`git clone https://github.com/G-Imola/Visualisations-of-Popularity.git`

After cloning, verify that the active Git branch is set to **main**.

To check the branch, run the following command:

`git branch`

You've done this correctly if the output shows ***main**.

If the branch is not set to main, you can switch to the **main**
branch by following these steps:

```
#enter the terminal on your selected IDE and input the command below:

cd Visualisations-of-Popularity


#Following this, type the code below:

git checkout main


#Finally, test to verify "main" branch has been selected:

git branch
```
This ensures that you're working on the correct branch for the project.

After following the steps above, your IDE should display the repository, alongside all other data that comes with it!



 ### 3. Dataset Placement
Ensure that the <a href="https://github.com/G-Imola/Visualisations-of-Popularity/blob/main/Original%20Dataset" target="_blank">Original Dataset</a> is downloaded and placed in the root
directory of the cloned repository.

 ### 4. Execute the script
Open `Data Visualisations.R` (<a href ="">link</a>) in Rstudio (or your preferred IDE), and run the script sequentially to generate:
1. Radar Chart
2. Parallel Coordinate plot
3. Violin plot
4. Additional descriptive plots

 ### 5.Outputs
Generated visualisations are all stored in the `plots` directory.

The outputs include:

* `.`

Additionally, generated `.csv` files are stored in the `csv ouputs` folder, which contain various outputs from the dataset, including:
* 
* and more!

</details>
<br/>

## key findings

1.
2.
3.

## License
This project is licensed under <a href="https://creativecommons.org/licenses/by/4.0/?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">CC BY 4.0<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1" alt=""></a>

See `license` file for more details.

## Contacts

For queries or further information, please contact:

Name: Gianmarco Imola
Email: gian.imola2003@gmail.com
GitHub: https://github.com/G-Imola


[^1]: https://ajosr.org/papers/volume-2/issue-4/uncovering-audio-features-shaping-popularity-in-chart-topping-songs-a-statistical-approach/