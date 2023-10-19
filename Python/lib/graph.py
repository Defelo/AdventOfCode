class UnionFind:
    def __init__(self, n):
        self.parents = [*range(n)]

    def find(self, x):
        if self.parents[x] == x:
            return x
        self.parents[x] = self.find(self.parents[x])
        return self.parents[x]

    def merge(self, x, y):
        self.parents[self.find(x)] = self.find(y)

    def connected(self, x, y):
        return self.find(x) == self.find(y)

    def count(self):
        return len(set(map(self.find, range(len(self.parents)))))

    def components(self):
        out = {}
        for parent, i in zip(map(self.find, range(len(self.parents))), range(len(self.parents))):
            out.setdefault(parent, []).append(i)

        return out
