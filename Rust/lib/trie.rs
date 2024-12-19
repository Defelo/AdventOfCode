use std::hash::Hash;

use rustc_hash::FxHashMap;

#[derive(Debug, Clone)]
pub struct Trie<T>(Vec<TrieNode<T>>);

#[derive(Debug, Clone)]
struct TrieNode<T> {
    flag: bool,
    next: FxHashMap<T, usize>,
}

impl<T> Trie<T> {
    pub fn new() -> Self {
        Self::default()
    }
}

impl<T: Eq + Hash> Trie<T> {
    pub fn insert(&mut self, item: impl IntoIterator<Item = T>) -> bool {
        let mut node = 0;
        for x in item {
            match self.0[node].next.get(&x) {
                Some(&next) => node = next,
                None => {
                    let next = self.0.len();
                    self.0.push(TrieNode::default());
                    self.0[node].next.insert(x, next);
                    node = next;
                }
            }
        }
        !std::mem::replace(&mut self.0[node].flag, true)
    }

    pub fn contains(&self, item: impl IntoIterator<Item = T>) -> bool {
        let mut node = 0;
        for x in item {
            match self.0[node].next.get(&x) {
                Some(&next) => node = next,
                None => return false,
            }
        }
        self.0[node].flag
    }

    pub fn prefix_matches<U: IntoIterator<Item = T>>(
        &self,
        item: U,
    ) -> impl Iterator<Item = usize> + use<'_, T, U> {
        self.0[0].flag.then_some(0).into_iter().chain(
            item.into_iter()
                .scan(0, |node, x| {
                    self.0[*node].next.get(&x).map(|&next| {
                        *node = next;
                        self.0[*node].flag
                    })
                })
                .enumerate()
                .flat_map(|(i, flag)| flag.then_some(i + 1)),
        )
    }
}

impl<T> Default for Trie<T> {
    fn default() -> Self {
        Self(vec![TrieNode::default()])
    }
}

impl<T> Default for TrieNode<T> {
    fn default() -> Self {
        Self {
            flag: false,
            next: Default::default(),
        }
    }
}

impl<I2> FromIterator<I2> for Trie<I2::Item>
where
    I2: IntoIterator,
    I2::Item: Eq + Hash,
{
    fn from_iter<I1: IntoIterator<Item = I2>>(iter: I1) -> Self {
        let mut trie = Self::new();
        for item in iter {
            trie.insert(item);
        }
        trie
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn trie() {
        let mut t = ["foo", "bar", "baz"]
            .into_iter()
            .map(|x| x.chars())
            .collect::<Trie<_>>();

        assert!(t.contains("foo".chars()));
        assert!(t.contains("bar".chars()));
        assert!(t.contains("baz".chars()));
        assert!(!t.contains("test".chars()));
        assert!(!t.contains("baa".chars()));

        assert!(t.insert("test".chars()));
        assert!(t.contains("test".chars()));
        assert!(!t.insert("test".chars()));
        assert!(t.contains("test".chars()));
    }

    #[test]
    fn prefix_matches() {
        let t = ["", "123", "12345", "1234567", "test", "12xy"]
            .into_iter()
            .map(|x| x.chars())
            .collect::<Trie<_>>();

        assert_eq!(
            t.prefix_matches("123456789".chars()).collect::<Vec<_>>(),
            [0, 3, 5, 7]
        );

        assert_eq!(t.prefix_matches("".chars()).collect::<Vec<_>>(), [0]);
    }
}
