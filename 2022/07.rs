use rustc_hash::FxHashMap;

type Input<'a> = Vec<Node<'a>>;

struct Node<'a> {
    size: u64,
    parent_id: Option<usize>,
    names: FxHashMap<&'a str, usize>,
    visited: bool,
}

impl<'a> Node<'a> {
    fn new(parent_id: Option<usize>) -> Self {
        Self {
            size: 0,
            parent_id,
            names: FxHashMap::default(),
            visited: false,
        }
    }
}

fn setup(input: &str) -> Input {
    let mut nodes = vec![Node::new(None)];
    let mut pwd = 0;
    let mut visiting = None;
    for line in input.trim().lines() {
        let mut split = line.split(' ');
        match (split.next(), split.next(), split.next()) {
            (Some("$"), Some("cd"), Some(dir)) => {
                if dir == "/" {
                    pwd = 0;
                } else if dir == ".." {
                    pwd = nodes[pwd].parent_id.unwrap_or(0)
                } else {
                    pwd = match nodes[pwd].names.get(dir) {
                        Some(id) => *id,
                        None => {
                            let id = nodes.len();
                            nodes.push(Node::new(Some(pwd)));
                            nodes[pwd].names.insert(dir, id);
                            id
                        }
                    }
                }
            }
            (Some(size), Some(_), None) => {
                if let Ok(size) = size.parse::<u64>() {
                    if nodes[pwd].visited {
                        continue;
                    }
                    visiting = Some(pwd);
                    nodes[pwd].size += size;
                }
            }
            _ => {
                if let Some(v) = visiting {
                    nodes[v].visited = true;
                    visiting = None;
                }
            }
        }
    }
    for i in (1..nodes.len()).rev() {
        if let Node {
            size,
            parent_id: Some(parent_id),
            ..
        } = nodes[i]
        {
            nodes[parent_id].size += size;
        }
    }
    nodes
}

fn part1(nodes: &Input) -> u64 {
    nodes
        .iter()
        .map(|node| node.size)
        .filter(|x| *x <= 100000)
        .sum()
}

fn part2(nodes: &Input) -> u64 {
    let free = 70000000 - nodes[0].size;
    nodes
        .iter()
        .map(|node| node.size)
        .filter(|x| *x >= 30000000 - free)
        .min()
        .unwrap()
}

aoc::main!(2022, 7);
aoc::example!(ex01, "07.1.txt", 95437, 24933642);
