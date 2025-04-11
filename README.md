# Hockey Network Analysis
This repository holds my final project for the Bates Network Analysis class!

---

# Introduction

Networks are everywhere! I was warned at the beginning of the network analysis course that once I had learned about them, there was no going back. I laughed, but I had no idea how true it was. A network, at its most base definition, can be any two things connected by something else. In more technical terms, networks consist of nodes that are connected to one another by a given relationship, or edge ([Weingart](https://journalofdigitalhumanities.org/1-1/demystifying-networks-by-scott-weingart/), 2011). Anything can be a network--4th-grade classroom dynamics, the neurons in your brain connected through synapses, actors connected by working together on films (see: Six Degrees of Kevin Bacon ([Linked](https://www.researchgate.net/publication/220694277_Linked_How_Everything_Is_Connected_to_Everything_Else_and_What_It_Means_for_Business_Science_and_Everyday_Life), page 41)), web pages linking to other web pages, etc, etc. The list goes on and on. I tried to escape from the wild, wild world of networks by sitting down with my knitting (stitches connected through yarn) and turning on a Colorado Avalanche game, but alas! All I could see were networks -- the players became nodes, the puck drawing literal, physical connections between them as it passed from stick to stick.

Before long, I started wondering if there were any hockey questions that could be answered through network analysis. While data analysis can be useful in every sport, hockey is particularly difficult to study because it is so reliant on team performance. Having one or two good players cannot guarantee success. To win the Stanley Cup, every teammate must be making an impact. This is where individual player statistics often fall short--they do not tell the full story of what is happening on the ice. Does having the most points in the league ([116, as of April 9th, 2025](https://www.espn.com/nhl/stats)) make Nathan MacKinnon the best player in the league, or is he just a good player being buoyed by the players around him? (In any case, he was a great addition to my fantasy league.) Many believe that statistics are useless in hockey--the only way to understand the game is to observe the dynamics for yourself. As hockey analyst Brendan Collins [notes](https://neutralzone.com/2023/03/15/a-journey-in-hockey-analytics-and-the-shortcomings-of-the-discipline/), one of the biggest pitfalls of modern hockey analytics is "focusing too much on the individual player output and not the relationship to the other players on the ice". Relationships... sounds familiar! I was curious if looking at a team's performance as a network would illuminate anything that traditional hockey statistics may have missed. Network analysis has been publicly used in two major instances--in a [data analysis](https://hockey-graphs.com/2015/11/09/the-2015-ohl-final-part-one-erie-otters-passing-network/) of the 2015 Ontario Hockey League Final using shot attempts and first and second shot assists done by RIT Men's Hockey Co-Director of Analytics and Athletic journalist [Ryan Stimson](https://www.nytimes.com/athletic/author/ryan-stimson/), and in a [presentation](https://hockey-graphs.com/2015/10/13/rithac-recap-with-slides/) on centrality measures at the 2017 RIT Hockey Analytics Conference by Sportsnet analyst [Stephen Burtch](https://www.sportsnet.ca/author/stephen-burtch/). Both noted the potential of network analysis as a way of determining team strategy and highlighting top hockey prospects.

In particular, I wanted to focus on the Colorado Avalanche's 2022 Stanley Cup Final win against the Tampa Bay Lightning. This was somewhat selfishly chosen--I am a huge Colorado Avalanche fan, and wanted an excuse to bask in their past win before the playoffs this year. However, I thought it would be an interesting look at how chemistry can differ between individual teams. These were two *good* teams--Tampa Bay had won the Stanley Cup for the past two years, and the Colorado Avalanche were the odds-on favorite going into the playoffs. I was curious if network analysis would reveal any observable differences that could hint at why the Colorado Avalanche won.

While there are so many fascinating questions that could be explored through network analysis (the success of passing formations in power play units, impacts of removing players from teams, defensive weak points based on locations of points scored, to name a few), I was constrained by the data that is made publicly available for individual games. For this reason, I chose to focus on the relationships formed between players from goals, primary assists, and secondary assists. In particular, I wanted to understand which players were most central to the network, which passing combinations were most likely to result in goals, and if players tended to pass to the same people.


# The Data

Network data typically comes in one of two formats. 

One of these is a sociomatrix, where every node is a row and a column, forming a grid of all possible connections. Connections between nodes are represented with a number where they intersect. If two nodes are not connected, there will be a zero where they intersect. 

The other data format is called an edgelist, which is a two-column list where connections are represented by one node being in column 1 and the node it is connected to being in column 2 ([U.G.N.A.R.](https://link.springer.com/book/10.1007/978-3-319-23883-8), pages 17-20).

My data was sourced from the [Scoring Summaries](https://www.espn.com/nhl/game/_/gameId/401445848/lightning-avalanche) posted by ESPN after each game, which includes a record of each goal scored, and its assists. These records were then put into edgelist form by hand, making a dataframe that looked like this:

```{r}
#read in the hockey csv file
hockeydata <- read.csv("2022_stanley_cup_final.csv")

#show the first 6 rows
head(hockeydata)
```

![A Google Sheets spreadsheet with columns for player_1 (the passer), player_2 (the reciever), the game number, and the team of the players.](data_example.png)


Here, the player *making* the pass is player 1, and the player *receiving* the pass is player 2. If a goal is scored, than the player receiving the pass is the goalie from the opposing team, marked with "G". This edgelist format was inspired by hockey analyst Ryan Stimson's 2015 passing [network analysis](https://hockey-graphs.com/2015/11/09/the-2015-ohl-final-part-one-erie-otters-passing-network/).


# Total Points Per Player

Now that we have the data, let's look at a brief overview of each player's individual statistics! In particular, we will look at which players scored points during the 6 games that made up the 2022 Stanley Cup Final. This will give us a look at the highest-impact players in terms of points-scoring. We can compare these players with those highlighted in our network analysis later on.

After some formatting of our data, we get two barplots:

![Barplot of Tampa Bay Lightning players's points scored, sorted by goals and assists.](tb_points.png)

Here, we can see that the top goal scorer was Ondrej Palat, with 3 goals. He and Victor Hedman tied for the most points scored, with each scoring 5. Victor Hedman is a defenseman, so his offensive impact is notable. Tampa Bay's highest point-scorers had less points than Colorado's highest-point scorers, which makes sense because Colorado scored more points in the series. Fewer people on Tampa Bay also scored points, with 16 point-scorers as opposed to Colorado's 18.

![Barplot of Colorado Avalanche players's points scored, sorted by goals and assists.](avs_points.png)

For the Avalanche, the top scorer was Valeri Nichuskin, with 4 goals. The two top points scorers were Mikko Rantanen, with 8 points, and Cale Makar, with 7. Cale Makar, like Victor Hedman, is a defenseman, so his offensive skill is something to watch out for. Another unique player is goalie Darcy Kuemper, who managed to score an assist on an empty-net goal. Colorado had more overall point-scorers, with 18 as opposed to Tampa Bay's 16, and its' leading point-scorers scored more points overall (8 for Colorado and 5 for Tampa Bay). 


(Note: Burgundys and blues were used in the creation of these graphs to symbolize team colors. However, to make it more colorblind-friendly, differing shades of the same color were used for contrast, and the colors in the two bar graphs were checked using this [colorblind checker](https://www.color-blindness.com/coblis-color-blindness-simulator/) to ensure that they were differentiable.)


# Network Visualizations

Now, let's go a little deeper! We want to visualize all points scored as a network.

### Subset dataframe

We can do this by subsetting the dataframe to just have the Colorado Avalanche players, then making a table. This makes a list of all passer-receiver combinations, and adds a frequency column of how often they appear in the edgelist.

```{r}
#make a dataframe of all passer-receiver combinations
pass_counts <-as.data.frame(table(avs_edgelist$player_1, avs_edgelist$player_2))

#set the column names so they make more sense
colnames(pass_counts) <- c("player_1", "player_2", "weight")

```

But we don't want all possible combinations, we just want the combinations we actually have. Let's get rid of all the rows where the frequency is 0.

```{r}
#remove rows where frequency is 0
pass_counts <- pass_counts[pass_counts$weight > 0, ]
```

### Create a graph object of just the Colorado Avalanche players

Now, let's turn this new edgelist into a graph object! Graph objects are data formats that R can read as networks. Specifically, we want a directed graph, since it matters who made the pass and who recieved it ( [U.G.N.A.R](https://link.springer.com/book/10.1007/978-3-319-23883-8), page 14). Directed graphs are networks that capture relationship flows. They are marked with arrows connecting one node to another, rather than a line connecting two nodes.

```{r}
#make a directed graph object from a dataframe
avs_g <- graph_from_data_frame(pass_counts, directed = TRUE)
```

Now, let's plot the network!

![A network showing every Colorado Avalanche player that scored a point during the Stanley Cup Final, connected by the passes they made to score the points.](avs_points.png)

Here, Valeri Nichushkin stands out as someone who is able to receive passes from many different people and convert them into goals. Additionally, Cale Makar, Mikko Rantanen, Nathan MacKinnon, and Gabriel Landeskog form a strong network of connected passes that are typically converted into goals. This makes sense, because they typically play on a top line together. As an opposing coach looking at this network, I would try to cut off Nichushkin's path to the goal--he typically stations himself nearby it so that he can easily tap goals in. I would also focus on cutting off passing lanes from Mikko Rantanen to other players, since this indicates that many of his passes are converted into goals.


Now, let's do the same for Tampa Bay.

![A network showing every Tampa Bay player that scored a point during the Stanley Cup Final, connected by the passes they made to score the points.](tb_points.png)

This network looks notably sparser than the Colorado Avalanche's, which would align with the difference in goals. Steven Stamkos, Ondrej Palay, Anthony Cirelli, and Nick Paul seem to be driving the most goals, but interestingly, they are not strongly connected in the same way that Colorado's top line is. Instead, most of their assists are coming from players with less goals and assists.

# Five-Number Summary

### Compare the five-number summaries for each team's network.

Now, let's compare 5-number summaries between the two teams. The five-number summary is a set of five statistics that describe characteristics about a network, similarly to how minimum, first quartile, median, third quartile, and maximum can describe the distribution of a dataset ([U.G.N.A.R](https://link.springer.com/book/10.1007/978-3-319-23883-8), page 12). Comparing these statistics will give us more insight into how the networks differ.

### Number of Nodes

We'll start with the number of nodes in each network:

```{r}
#number of nodes
print(paste("Number of Nodes in the Colorado Avalanche Points Network: ", vcount(avs_g)))
print(paste("Number of Nodes in the Tampa Bay Lightning Points Network: ", vcount(tb_g)))
```

The number of nodes in the Colorado Avalanche points network is 19. This means that out of their [23 rostered players](https://www.eliteprospects.com/team/57/colorado-avalanche/stats/2021-2022/playoffs) in the playoffs, only 18 scored points (19 nodes minus opposing goalie).

The number of nodes in the Tampa Bay points network is 17. Out of their [23 rostered players](https://www.eliteprospects.com/team/75/tampa-bay-lightning/stats/2021-2022/playoffs) in the playoffs, this means that only 16 scored points (17 nodes minus opposing goalie).

While this may be partially because Tampa Bay scored fewer points overall, it may be a sign that their depth players were not as effective, meaning that they had to rely more strongly on their top players.

### Density

Now, let's look at the networks' density:

```{r}
#density
print(paste("Density of the Colorado Avalanche Points Network: ", edge_density(avs_g)))
print(paste("Density of the Tampa Bay Lightning Points Network: ", edge_density(tb_g)))
```

A network's density value is the proportion of the number of connections in the network/the number of possible connections ( [U.G.N.A.R](https://link.springer.com/book/10.1007/978-3-319-23883-8), page 14). Because the network is directed, there are two possible connections between every two nodes--a pass from Makar to MacKinnon and a pass from MacKinnon to Makar are counted as two separate connections. In both cases, the networks are not very dense; Colorado has a density score of 0.123 and Tampa Bay has a score of 0.125. This is probably a sign that the number of goals scored compared to the number of players involved is relatively small--if we were looking at goals and assists over an entire season, the density would be significantly higher. This low density is consistent with a study by [Clemente et al., (2015)](https://www.researchgate.net/profile/Filipe-Clemente/publication/282443736_How_team_sports_behave_as_a_team_General_network_metrics_applied_to_sports_analysis/links/561056ef08ae48337519f1cf/How-team-sports-behave-as-a-team-General-network-metrics-applied-to-sports-analysis.pdf) which showed that of all sports passing networks, ice hockey had the lowest density scores. It was hypothesized that this is due to the goaltender not participating in passing plays.



### Number of Components

The number of components in a network is the number of distinct subgroups in that network. To be specific, a component is comprised of nodes that are all connected, either directly or indirectly ([U.G.N.A.R](https://link.springer.com/book/10.1007/978-3-319-23883-8), page 15). Having more components would indicate that players tend to only pass to the same people. For instance, if the Tampa Bay top line only played with each other and the lines were never rearranged, there would be more distinct components in the network. Conversely, if the Tampa Bay players were always switching up who they play with, then there would be only one large component.


```{r}
#number of components
print(paste("Number of Components in the Colorado Avalanche Points Network: ", count_components(avs_g)))
print(paste("Number of Components in the Tampa Bay Lightning Points Network: ", count_components(tb_g)))
```

In this case, both networks have only 1 component, indicating that there is no significant separation between players on either team.

### Diameter

The diameter of a network is a measure of the network's compactness. This value is found by finding the fewest number of steps required to get from node A to node B in a network. Once this number is found for every node A to get to every node B, then the diameter is the *longest* of all *shortest* paths in the network ([U.G.N.A.R](https://link.springer.com/book/10.1007/978-3-319-23883-8), page 15). 

```{r}
#diameter
print(paste("Diameter of the Colorado Avalanche Points Network: ", diameter(avs_g)))
print(paste("Diameter of the Tampa Bay Lightning Points Network: ", diameter(tb_g)))
```


The diameters for the Colorado Avalanche and Tampa Bay Lightning are 7 and 6, respectively. This means that Tampa Bay's passing network is more compact in terms of the players participating in scoring points. This could indicate that Tampa Bay's passing structure is more efficient, or it could also be a mark of having less depth scoring.


### Transitivity

Clustering is the tendency to form closed triangles in social networks. Transitivity, the measure of clustering, is the proportion of closed triangles (triads with all 3 ties) / total number of open and closed triangles (triads with 2 or 3 ties) ([U.G.N.A.R](https://link.springer.com/book/10.1007/978-3-319-23883-8) page 16).

```{r}
#transitivity
print(paste("Transitivity of the Colorado Avalanche Points Network: ", transitivity(avs_g)))
print(paste("Transitivity of the Tampa Bay Lightning Points Network: ", transitivity(tb_g)))
```

For the Colorado Avalanche and Tampa Bay Lightning, the transitivity values are 0.379 and 0.403, respectively. This indicates that Tampa Bay has a stronger tendency of forming clusters within players. This, along with Tampa Bay's smaller diameter and higher density, points to Tampa Bay having less depth in who is passing to one another. These also may be artifacts of Tampa Bay having less point-scorers and fewer points overall.


### Eigenvector Centrality

![Colorado Avalanche Players' Eigenvector Centrality Scores, sorted from highest to lowest.](tb_points.png)

Eigenvector centrality calculates the extent to which nodes are connected to other well-connected nodes. Cale Makar has the highest Eigenvector centrality, which means that he is often connected to plays with other top scorers. Cale Makar won the [Conn Smythe](https://www.nhlpa.com/news/2-31722/avalanche-d-man-cale-makar-wins-conn-smythe-as-playoff-mvp) (most valuable player in the playoffs award) for his outstanding defensive and offensive ability despite not having the highest points or goals total. His high Eigenvector centrality shows that this award was well deserved--he was the most central player in all of the Avalanche's successful plays.

Mikko Rantanen, the Avalanche's top points scorer (8 points) and Valeri Nichuskin, the Avalanche's top goal scorer (4 goals) are next, indicating that they make up a significant portion of successful goals and plays as well. 

![Tampa Bay Lightning Players' Eigenvector Centrality Scores, sorted from highest to lowest.](tb_points.png)


Ondrej Palat has the highest Eigenvector centrality score on the Tampa Bay Lightning, which makes sense since he scored the most goals (3) and is tied for the most points (5). He is part of the most plays, and his plays typically involve other high scorers such as Steven Stamkos and Nikita Kucherov. Additionally, Ondrej Palat scored many goals at key times for Tampa Bay, most notably scoring the [game-winning goal](https://www.nytimes.com/athletic/3489675/2022/06/25/2022-stanley-cup-final-ondrej-palats-late-goal-lifts-lightning-to-game-5-win/) in Game 5 to stave off Tampa Bay's elimination.

### Betweenness

![Colorado Players' Betweenness Centrality Scores, sorted from highest to lowest.](tb_points.png)

Betweenness centrality is a measure of the extent that a node is present on the shortest path between other nodes. If a node has the highest betweenness centrality score, that means that it is the strongest connector of other nodes. In the 2017 RIT Hockey Analytics Conference, hockey analyst Stephen Burtch [highlighted](https://hockey-graphs.com/2015/11/09/the-2015-ohl-final-part-one-erie-otters-passing-network/) betweenness centrality in passing networks as a marker of how the team would suffer if the player was removed.

It is fascinating that Andrew Cogliano is the person with the highest betweenness! He was brought onto the Colorado Avalanche mainly to be a [morale booster](https://milehighsticking.com/3-deals-that-made-2022-trade-deadline-the-best-for-colorado-avalanche-in-recent-history-01jmg3t291a9/2#:~:text=Andrew%20Cogliano%20from%20San%20Jose,player%20has%20on%20the%20NHL.). He was a veteran player who had never won the Stanley Cup after 15 years in the NHL. While he only scored 3 points in the series, his betweenness score highlights that he served an incredibly valuable role as a connector for other depth players. Because the five-number summary highlights that Colorado had more depth scoring than Tampa Bay, Cogliano's betweenness score highlights that he played a major part in that.

Cale Makar's betweenness centrality score is also notable--as an offensive defenseman, his role is typically to take the puck away from the opposing team and either score or give it to someone else who can. His high betweenness score demonstrates his effectiveness as a connector in many successful plays, further making a case for playoff MVP.

![Tampa Bay Players' Betweenness Centrality Scores, sorted from highest to lowest.](tb_points.png)

Most notably, the betweenness centrality scores for Tampa Bay are much lower than those of the Colorado Avalanche. This may imply that players are less well-connected to one another and have used their passing less effectively. It is notable that two defenseman are part of their top 3, though. This highlights how good defensemen are not only able to keep goals from being scored, but they are also able to effectively set up offensive plays. Hedman and Sergachev are standout defensemen in the league for precicely this reason. 

In the end, this project gave me a new way to watch the game I already love. By mapping out who passed to whom and who finished the job, I could actually see the flow of each team’s offense and understand the dynamics beneath the surface. Players who racked up goals weren’t always the most central in the network, and some of the key playmakers didn’t light up the scoreboard but were essential in connecting the dots. Turning a playoff series into a network peeled back the layers of team strategy and chemistry—something the box score alone could never capture. And honestly, it made me think differently about what it means to “contribute” in a team sport.
