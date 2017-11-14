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

- [nodes.csv]()
- [edges.csv]()

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
To see available plots



There are several functions available for plotting the network using
different coloring schemes for the nodes. The graph shows links between
two cases where one case cited the other case in the decision.
Try these one at a time:

```{r}
plot_year()
plot_vote()
plot_issue()
plot_centrality()
plot_cluster()
plot_gatekeeper()
```

When you run the function, it should open a web browser with an interactive
graph. Hovoring over a node will show the name of the case, the year of the
case, and an id called USID. The latter is useful to looking up the case online.
You should find that the most central cases have lots of third-party information
about them. Wikipedia is a good place to look if a link shows up. For all cases,
the USID search should at a minimum turn up the original decision text.

The following are ways to color code the network based on metadata:

- **Year**: Color coded the node according to the year of the case. 
- **Issue**: Color coded the node according to the area of law. 
- **Vote**: Colod coded based on the number of justices casting a dissenting votes for the case.

The following are calculations based on network properties: 

- **Centrality**:  Identifies the most important node/ vertices in a network.   
- **Cluster**: A set of nodes that cluster together within a larger network. They are also known as communities. There can be muliple communities in one network.  The clusters are algorithmically learning groupings of nodes that are are cited by one another.
- **Gatekeeper**: Find nodes that links disparate parts of the citation network. They tend to be the node or set of nodes that connect different communities.  

### Step 6: Co-citations

Finally, you can also run the function `select_topic_cocites` instead of 
`select_topic_cites`, as such:

```{r}
select_topic_cocites(20040, 20050)
```

Graphics made after calling this data will link any two cases that were 
both cited by a third case a minimum of 9 different times. This is called
a co-citation graph (rather than a citation graph). Compare this with the
citation graph; which one do you think tells a more cohesive or interesting
story? Can you see different patterns in the two



