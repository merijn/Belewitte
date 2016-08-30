from collections import defaultdict

class Naming(dict):
    def __missing__(self, key):
        return key

    def map(self, fn):
        return Naming(((k, fn(v)) for k, v in self.items()))

names = defaultdict(Naming)
names['graph'] = Naming({
    "chain_1000000" : ("chain_1000000", 1),
    "star_1000000" : ("star_1000000", 2),
    "degree4_1000" : ("degree4_1000", 3),
    "degree6_100" : ("degree6_100", 4),
    "degree_5_16" : ("degree_5_16", 5),
    "degree_10_4" : ("degree_10_4", 6),
    "degree_20_2" : ("degree_20_2", 7),
    "as-skitter-undirected" : ("as-Skitter", 8),
    "cit-Patents" : ("cit-Patents", 9),
    "email-EuAll" : ("email-EuAll", 10),
    "facebook_combined-undirected" : ("Facebook", 11),
    "gplus_combined" : ("Gplus", 12),
    "roadNet-CA-undirected" : ("roadNet-CA", 13),
    "roadNet-TX-undirected" : ("roadNet-TX", 14),
    "soc-LiveJournal1" : ("soc-LiveJournal1", 15),
    "twitter_combined" : ("Twitter", 16),
    "web-BerkStan" : ("web-BerkStan", 17),
    "web-Google" : ("web-Google", 18),
    "wiki-Talk" : ("wiki-Talk", 19)
})

names['algorithm'] = Naming({
    "bfs" : "BFS",
    "pagerank" : "PageRank"
})

names['implementation'] = Naming({
    "edge-list" : "Edge List",
    "vertex-push" : "Vertex Push",
    "vertex-pull" : "Vertex Pull",
    "vertex-push-warp" : "Vertex Push Warp",
    "vertex-pull-warp" : "Vertex Pull Warp",
    "vertex-pull-nodiv" : "Vertex Pull NoDiv",
    "vertex-pull-warp-nodiv" : "Vertex Pull Warp NoDiv"
})
