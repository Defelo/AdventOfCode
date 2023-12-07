#![feature(test)]

use counter::Counter;
use itertools::Itertools;

type Input = Vec<Hand>;

#[derive(Debug)]
struct Hand {
    cards: [Card; 5],
    bid: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Card(u8);

impl Card {
    fn from_byte(b: u8) -> Self {
        Self(match b {
            b'2'..=b'9' => b - b'0',
            b'T' => 10,
            b'J' => 11,
            b'Q' => 12,
            b'K' => 13,
            b'A' => 14,
            _ => panic!(),
        })
    }

    fn is_joker(&self) -> bool {
        self.0 == 11
    }

    fn remap_joker(self) -> Self {
        if self.is_joker() {
            Self(0)
        } else {
            self
        }
    }
}

impl Hand {
    fn sort_key<const JOKERS: bool>(&self) -> impl Ord + std::fmt::Debug {
        let cnt = self
            .cards
            .into_iter()
            .filter(|card| !JOKERS || !card.is_joker())
            .collect::<Counter<_>>()
            .into_map()
            .into_values()
            .sorted_unstable()
            .collect_vec();

        let ty = match &cnt[..] {
            [] | [_] => 6,
            [1, _] => 5,
            [2, _] => 4,
            [1, 1, _] => 3,
            [1, 2, _] => 2,
            [1, 1, 1, _] => 1,
            _ => 0,
        };

        let cards = if JOKERS {
            self.cards.map(Card::remap_joker)
        } else {
            self.cards
        };

        (ty, cards)
    }
}

fn setup(input: &str) -> Input {
    input
        .lines()
        .map(|line| {
            let mut line = line.split_whitespace();
            let cards = line
                .next()
                .unwrap()
                .bytes()
                .map(Card::from_byte)
                .collect_vec()
                .try_into()
                .unwrap();
            let bid = line.next().unwrap().parse().unwrap();
            Hand { cards, bid }
        })
        .collect()
}

fn solve<O: Ord>(input: &Input, sort_key: fn(&Hand) -> O) -> u64 {
    input
        .iter()
        .sorted_by_cached_key(|&hand| sort_key(hand))
        .enumerate()
        .map(|(i, hand)| (i as u64 + 1) * hand.bid)
        .sum()
}

fn part1(input: &Input) -> u64 {
    solve(input, Hand::sort_key::<false>)
}

fn part2(input: &Input) -> u64 {
    solve(input, Hand::sort_key::<true>)
}

aoc::main!(2023, 7, ex: 1);
