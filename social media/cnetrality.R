#degree centrality

# A 1
# B 2
# C 3
# D 4
# E 5
# F 6
# G 7
# H 8

g1 <- graph( edges=c(1,7, 1,6, 1,2, 1,8, 2,7, 2,4, 3,4, 5,8), n=8, directed=F ) 
plot(g1) # A simple plot of the network - we'll talk more about plots later

#degree centralization
degree(g1, mode="in")
degree(g1, mode="in")/7

#closeness
closeness(g1, mode="all", weights=NA) 

#betweeness
betweenness(g1, directed=T, weights=NA)
betweenness(g1, directed=T, weights=NA)/21

#My network


g2 <- graph( edges=c(1,2, 1,3, 1,5, 1,6, 1,7, 1,8, 1,9, 1,11, 1,12, 2,7, 2,11, 2,12, 3,4, 4,10, 6,10, 7,12, 11,12), n=12, directed=F ) 
plot(g2)
degree(g2, mode="in")
closeness(g2, mode="all", weights=NA) 
betweenness(g2, directed=T, weights=NA)





