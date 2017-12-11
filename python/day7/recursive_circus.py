from sys import argv, stdin
from collections import deque, Counter
import re


class DiscNode(object):
    def __init__(self, name, weight):
        self.name = name
        self.weight = weight
        self.outgoing = []
        self.incoming = []
        super(DiscNode, self).__init__()

    @property
    def total_weight(self):
        return self.weight + sum(x.total_weight for x in self.outgoing)

    @property
    def unbalanced_node(self):
        hist = Counter([x.total_weight for x in self.outgoing])
        target = hist.most_common()
        for m in self.outgoing:
            if target != m.total_weight:
                return m.unbalanced_node
        return self

    def __repr__(self):
        out = ', '.join([n.name for n in self.outgoing])
        inc = ', '.join([n.name for n in self.incoming])
        return '{DiscNode %s (%d) [%s] [%s]}' % (self.name, self.weight, inc, out)


def read_discs_to_nodes(discs):
    matcher = re.compile('(\w+) \((\d+)\)(?: -> ((?:\w+(?:, |$))+))?')
    adjacencyMap = {}
    for desc in discs:
        if desc.strip() == '':
            continue
        name, weight, neighbors = matcher.findall(desc)[0]
        adjacencyMap[name] = {
            'weight': int(weight),
            'outgoing': [x for x in neighbors.split(', ') if x != '']
        }

    nodeMap = {k: DiscNode(k, v['weight']) for k, v in adjacencyMap.iteritems()}
    for k, v in adjacencyMap.iteritems():
        for n in v['outgoing']:
            nodeMap[k].outgoing.append(nodeMap[n])
            nodeMap[n].incoming.append(nodeMap[k])

    return nodeMap.values()


def topo_sort(nodes):
    degrees = {n.name: len(n.incoming) for n in nodes}
    ordered = []
    dq = deque([n for n in nodes if degrees[n.name] == 0])

    while len(dq) > 0:
        curr = dq.popleft()
        ordered.append(curr)

        for m in curr.outgoing:
            degrees[m.name] -= 1
            if degrees[m.name] == 0:
                dq.append(m)

    return ordered


def get_base_disc(nodes):
    return topo_sort(nodes)[0].name


def find_balance_resolving_weight(nodes):
    ordered = topo_sort(nodes)
    curr = ordered[0]
    target_weight = 0

    while True:
        weights = Counter([x.total_weight for x in curr.outgoing])
        common_weight = weights.most_common(1)[0][0]
        for n in curr.outgoing:
            if n.total_weight != common_weight:
                curr = n
                target_weight = common_weight
                break
        else:
            break
        continue

    return curr.weight + (target_weight - curr.total_weight)


if __name__ == '__main__':
    def read_input():
        return read_discs_to_nodes([x.strip() for x in stdin.readlines() if x.strip() != ''])
    if '-p1' in argv:
        print get_base_disc(read_input())
    elif '-p2' in argv:
        print find_balance_resolving_weight(read_input())
    else:
        print 'Please specify -p1 or -p2 for Part 1 or Part 2, respectively.'
