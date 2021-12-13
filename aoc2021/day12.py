#!/usr/bin/env python3
import fileutils


def part1(lst):
    graph = build_graph(lst)
    count = count_paths(graph["vertices"]["start"], graph["vertices"]["end"], graph["vertices"], 1)
    return count


def part2(lst):
    graph = build_graph(lst)
    count = count_paths(graph["vertices"]["start"], graph["vertices"]["end"], graph["vertices"], 2)
    return count


def count_paths(s, d, vertices, m):
    path_count = [0]
    count_paths_until(s, d, path_count, vertices, [], False, m)
    return path_count[0]


def count_paths_until(u, d, path_count, vertices, visited, has_visited, m):
    visited.append(u)
    if is_restricted(u['id']) is True and visited.count(u) == 2 and has_visited is False:
        has_visited = True
    if u['id'] == d['id']:
        path_count[0] += 1
    else:
        for adj in u["adj"]:
            curr = vertices[adj]
            if curr['id'] == 'start' or \
                    (is_restricted(curr['id']) is True and visited.count(curr) >= m - 1 and has_visited is True):
                continue
            count_paths_until(curr, d, path_count, vertices, visited, has_visited, m)
    visited.remove(u)


def add_neighbour(frm, to):
    frm["adj"].append(to["id"])


def add_edge(e1, e2, graph):
    vertices = graph["vertices"]
    if e1 not in vertices:
        add_vertex(e1, vertices)
    if e2 not in vertices:
        add_vertex(e2, vertices)
    add_neighbour(vertices[e1], vertices[e2])
    add_neighbour(vertices[e2], vertices[e1])


def is_restricted(e):
    return all([c.islower() for c in e])


def add_vertex(e, vertices):
    if e not in vertices:
        vertices[e] = {"id": e, "adj": []}


def build_graph(lst):
    graph = {"vertices": {}}
    for line in lst:
        edges = line.split('-')
        add_edge(edges[0], edges[1], graph)
    return graph


if __name__ == "__main__":
    challenge_input = fileutils.read_lines("inputs/day12.txt")

    print("=== Day 12 ===")
    print("Part 1:", part1(challenge_input))
    print("Part 2:", part2(challenge_input))
