**Goal:** Today, you are going to construct your own data and built an interactive
graph from it. 

### Step 1: Download R

If you have not already, download and install R for your computer (most of you should
already have done this). To do so, following the following link and proceed with
the prompts:

- [https://cran.r-project.org/](https://cran.r-project.org/)

Once downloaded, install the dmg or exe file as you would any software.

### Step 2: Install R packages

You'll need the following R packages (if you did this last time, no need to
reinstall them):

```{r}
install.packages("igraph")
install.packages("dplyr")
install.packages("readr")
install.packages("plotly")
install.packages("viridis")
install.packages("stringi")
```

It may ask you some questions and I'll help you answer those. Note that this may
take a few minutes as each of these packages requires other packages, none of
which you have yet.

### Step 3: Grab functions and data

This time we can grab all of the functions and datasets needed for our visualizations
directly from within R. No need to download any files or copy large amount of code.
Simply run the following line in R to download the required functions:

```{r}
source("https://statsmaths.github.io/dh-network-lab/funs.R")
```

Once this is run, you'll have the functions needed to load your own data into R.

### Step 4: Create network data

Now, open a spreadsheet program and create two tabs. Call one "nodes" and 
the other "edges". In both, the first line of the file needs to include
a header row. The first column of the nodes will be your id column. The first 
and second columns of edges will give pairs of ids that should be joined.
You can also include additional information as extra columns of the nodes 
table. You'll be able to plot and see these in the graph.

Once you create the node and edge datasets, export them as CSV files and 
save to a folder on your computer (perhaps the Desktop). Make sure that they
are called "nodes.csv" and "edges.csv" (capitalization matters!). 

For examples of what the files should look like, see here:

- [nodes.csv](https://github.com/statsmaths/dh-network-lab/blob/master/edges.csv)
- [edges.csv](https://github.com/statsmaths/dh-network-lab/blob/master/nodes.csv)

### Step 5: Read the data into R

We want to tell R where to save the output of our models. We can do this by running
the following command (to save at the Desktop on a Mac):

```{r}
setwd("~/Desktop")
```

You can use the GUI menus as well if you are having trouble with that. Then, we read
in the edge and node datasets that you just created using the following functions:

```{r}
load_csv()
```

If no errors appear (warnings are fine) you are all set.

### Step 6: Plot the data

You can now create an interactive plot of your network!
To see available plots, run the command:

```{r}
list_plots()
```

The list of plots should include derived network measures
as well as the metadata you included in the raw node data.

Then, you can run the corresponding plot by selecting the
number. You'll need to determine whether the variable should
be shown as a categorical variable (such as cluster):

```{r}
plot_network_category(5)
```

Or a numeric one (such as the eigenvalue centrality):

```{r}
plot_network_numeric(2)
```



