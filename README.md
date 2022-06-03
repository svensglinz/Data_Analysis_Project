## Description 

This project was done as part of a group project for the Course Big Data for Social Analysis at NCCU. 

We look at gender related aspects in donations to the presidential Candidate Committees of Donald Trump and Joe Biden in the 2020 Presidential Election. 
We put a focus on the share of donations as well as the share of donors which are women. 

Our analysis reveals that significantly more women donated to Biden than to Trump (donation amount wise as well as in terms of number of donors). Further, around the announcement of Biden's choice to pick Kammala Harris as VP, donations to Biden shot up relative to those to Trump (both from Male and Female) and the contribution ratio from Females also increased. 

A regression analysis which aims to analyze the relative amount female donors and donations per US State further reveals that the relative share of female donors is strongly dependent on the % of female which live in a us state, but surprisingly is also influenced by the conservativeness of a US State. 

## Data 

Data which was used in this code and which should be part of the folder "files" but is too large to upload on github can be found under: 
- a
- b
- c


## Output Examples

<table style="text-align:center", align = "center"><tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Gender</td><td>Candidate</td><td>average donation</td><td>total donations (MIO)</td><td>total donors (T)</td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">F</td><td>BIDEN</td><td>451</td><td>179</td><td>397</td></tr>
<tr><td style="text-align:left">F</td><td>TRUMP</td><td>313</td><td>106</td><td>340</td></tr>
<tr><td style="text-align:left">M</td><td>BIDEN</td><td>587</td><td>222</td><td>377</td></tr>
<tr><td style="text-align:left">M</td><td>TRUMP</td><td>385</td><td>184</td><td>477</td></tr>
<tr><td style="text-align:left">NA</td><td>BIDEN</td><td>483</td><td>16</td><td>33</td></tr>
<tr><td style="text-align:left">NA</td><td>TRUMP</td><td>400</td><td>15</td><td>38</td></tr>
<tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr></table>

<br>
<br>

<p float="left", align = "center">
  <img src="plots/rel_share_donations.png" width="300", height = "350" />
    &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp; 
  <img src="plots/evolution_female_donations.png" width="350", height = "350"/> 
</p>
