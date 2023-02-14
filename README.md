---
editor_options: 
  markdown: 
    wrap: sentence
---

# RB8_ShinyDashboard

A Shiny dashboard for the Salinization thresholds for the Santa Ana Watershed project from the SCCWRP Biology department (Jan, Raphael and Abel).

This dashboard is intended to help support waterboard staff identify thresholds for ionic parameters based on biological response models.
Users should select one item from each drop-down menu, and then push the filter button.
A map showing average thresholds for each segment in the Santa Ana watershed will be rendered, along with a table containing the plotted data.
For each segment, we report n (the number of months fitting the selected criteria), the minimum, maximum, average and standard deviation of E (i.e., the predicted natural background level of the parameter in the stream segment), and threshold.
The download button will download a CSV file of the resulting rows, which may be joined to an NHD+ shapefile based on the unique stream segment identifier (COMID).
For additional information, contact Raphael Mazor ([raphaelm\@sccwrp.org](mailto:raphaelm@sccwrp.org){.email})

![dashboard screenshot](images/Screenshot%202023-02-13%20at%207.21.58%20PM.png)
