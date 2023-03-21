# Social-Network-Analysis
The purpose of this project is to evaluate in detail the relationships within the Italian parliament, identifying possible influential and central figures.

Starting from the data collected on the signing of legal acts by the deputies of the Chamber in the last legislature, and from the personal information concerning the parliamentarians themselves, a social network was built, i.e. a structure characterized by nodes (individuals) and arcs (the relationships between individuals).
The initial adjacency matrix was then transformed into an indirect and unweighted network.  
Moreover, it is of interest to determine whether the network of signatories presents homophilia with respect to the political party to which they belong and with respect to the gender of the parliamentarian. To predict a member of parliament who changes political group during the legislature, we want to investigate whether some figures present behaviors (in terms of participation in a bill) consistent with other electoral groups, different from that to which they belong.

The "Y.txt" file contains the adjacency matrix among the 655 MPs, some personal information on the parliamentarians themselves such as the political party they belong to, age and gender are contained in the "X.txt" file.
