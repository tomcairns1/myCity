# My City in Space and Time

## Overview
A project inspired by a homework assignment to visualize the street trees of the
city of San Francisco. An Rshiny app was created to allow a user to select a
specific genus, which would update the app to show the number of species for
the selected genus, a map showing the location of each tree of that genus
colored by species, and a line chart showing the history of the planting of the
trees of that genus.

## Methods
The first file, `myCity_analysis.Rmd`, goes through some basic analysis of the
data and provides some summary visualizations including a circular bar chart
of the most common genuses with the number of individuals of each species
within that genus.

The second file, `app.R`, contains the code for an Rshiny app which will allow
the user to change the genus and see updated figures including a bar chart
showing the number of individuals of each species, an interactive map of the
locations of each tree colored by species, and a line chart showing the number
of trees of the selected genus planted each year.

