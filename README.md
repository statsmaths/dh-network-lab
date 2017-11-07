**Goal:** Today, we are going to build an interactive network of citations between
legal decisions constructed by the Supreme Court. The current data stretches from
1947 to 2001 and includes over 7000 cases.

### Step 1: Download R

The next step is to download and install R for your computer (most of you should
already have done this). To do so, following the following link and proceed with
the prompts:

- [https://cran.r-project.org/](https://cran.r-project.org/)

Once downloaded, install the dmg or exe file as you would any software.

### Step 2: Install R packages

Now, we need to install several add-ons to the R language. We can do this from
within R. Open the program. Then, copy and paste the following lines one by one
into the R terminal:

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

Notice that I don't have to run these lines on my machine. These only has to be
run once and then they are installed for good.

### Step 3: Grab functions and data

This time we can grab all of the functions and datasets needed for our visualizations
directly from within R. No need to download any files or copy large amount of code.
Simply run the following line in R to download the required functions:

```{r}
source("https://statsmaths.github.io/dh_network_lab/funs.R")
```

Once that is done, the next bit of code will load the entire corpus of Supreme
Court Citations into R:

```{r}
load_data()
```

If there are no errors (warnings are okay) or messages about missing packages, you
should be ready to go.

### Step 4: Select topics of interest

```{r}
select_topic_cites(20040, 20050)
```

### Step 5: Plot the data

There are several functions available for plotting the network using
different coloring schemes for the nodes. Try these one at a time.

```{r}
plot_centrality()
plot_issue()
plot_cluster()
plot_year()
plot_vote()
plot_gatekeeper()
```

The gatekeeper visualization attempts to find nodes that links disparate
parts of the citation network.

### Step 6: Co-citations

```{r}
select_topic_cocites(20040, 20050)
```





