from collections import defaultdict

def depthToNum(s):
    return int(s[len("bfsLevel"):])

class Naming(dict):
    def __init__(self, *args, **kwargs):
        super(Naming, self).__init__(*args, **kwargs)
        self.transform = lambda k: k

    def __missing__(self, key):
        try:
            key = self.transform(key)
        except:
            pass

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
    "wiki-Talk" : ("wiki-Talk", 19),
    'actor-collaboration' : ("", 1),
    'ca-cit-HepPh' : ("", 2),
    'cfinder-google' : ("", 3),
    'dbpedia-starring' : ("", 4),
    'discogs_affiliation' : ("", 5),
    'opsahl-ucsocial' : ("", 6),
    'prosper-loans' : ("", 7),
    'web-NotreDame' : ("", 8),
    'wikipedia_link_en' : ("", 9),
    'wikipedia_link_fr' : ("", 10),
    'zhishi-hudong-internallink' : ("", 11)
})

names['algorithm'] = Naming({
    "bfs" : "BFS",
    "pagerank" : "PageRank"
})

names['implementation'] = Naming({
    "edge-list" : "Edge List",
    "rev-edge-list" : "Reverse Edge List",
    "vertex-push" : "Vertex Push",
    "vertex-pull" : "Vertex Pull",
    "vertex-push-warp" : "Vertex Push Warp",
    "vertex-pull-warp" : "Vertex Pull Warp",
    "vertex-pull-nodiv" : "Vertex Pull NoDiv",
    "vertex-pull-warp-nodiv" : "Vertex Pull Warp NoDiv",
    "optimal" : "Optimal",
    "predicted" : "Predicted",
    "best" : "Non-switching Best",
    "lonestar" : "Lonestar",
    "gunrock" : "Gunrock"
})

names['timer'] = Naming()
names['timer'].transform = depthToNum

names['x-axis'] = Naming({
    "timer" : "BFS Level"
})
names['x-axis'].transform = lambda s: s.capitalize()
