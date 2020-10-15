#[derive(Debug, Clone)]
struct PvebU {
    u: u8,
    u_sqrt: u8,
    lower_capacity: Option<Capacity>,
    summary: Option<Box<Summary>>,
    clusters: Vec<Cluster>,
}

#[derive(Debug, Clone)]
struct Summary {
    n: usize,
    pveb: PvebU,
}

#[derive(Debug, Clone)]
enum Cluster {
    NonLeaf(PvebU),
    Leaf(bool),
}

#[derive(Debug, Copy, Clone)]
enum Capacity {
    Base,
    C2,
    C4,
    C16,
    C64,
}

impl Capacity {
    fn sqrt(self) -> Option<Self> {
        match self {
            Capacity::Base => None,
            Capacity::C2 => Some(Capacity::Base),
            Capacity::C4 => Some(Capacity::C2),
            Capacity::C16 => Some(Capacity::C4),
            Capacity::C64 => Some(Capacity::C16),
        }
    }
}

impl From<Capacity> for u8 {
    fn from(capacity: Capacity) -> u8 {
        match capacity {
            Capacity::Base => 1,
            Capacity::C2 => 2,
            Capacity::C4 => 4,
            Capacity::C16 => 16,
            Capacity::C64 => 64,
        }
    }
}

impl PvebU {
    pub fn new(capacity: Capacity) -> Self {
        let u: u8 = u8::from(capacity);
        let (lower_capacity, u_sqrt) = if let Some(c) = capacity.sqrt() {
            (c, u8::from(c))
        } else {
            return Self {
                u: 1,
                u_sqrt: 1,
                summary: None,
                lower_capacity: None,
                clusters: vec![],
            };
        };

        let summary = if u_sqrt > 1 {
            Some(Box::new(Summary {
                n: 0,
                pveb: PvebU::new(lower_capacity),
            }))
        } else {
            None
        };

        let clusters = match capacity {
            Capacity::C2 => vec![Cluster::Leaf(false); 2],
            _ => vec![Cluster::NonLeaf(PvebU::new(lower_capacity)); u_sqrt as usize],
        };

        Self {
            u,
            u_sqrt,
            summary,
            lower_capacity: Some(lower_capacity),
            clusters,
        }
    }

    pub fn min(&self) -> Option<u8> {
        self.min_(0)
    }

    fn min_cluster(&self) -> Option<(u8, &PvebU)> {
        self.clusters
            .iter()
            .enumerate()
            .filter_map(|(idx, cluster)| match cluster {
                Cluster::NonLeaf(pveb) => Some((idx, pveb)),
                _ => None,
            })
            .find(|(_, pveb)| {
                pveb.summary
                    .as_ref()
                    .map(|summary| summary.n > 0)
                    .unwrap_or(false)
            })
            .map(|(idx, pveb)| (idx as u8, pveb))
    }

    fn min_leaf(&self) -> Option<u8> {
        self.clusters
            .iter()
            .map(|cluster| match cluster {
                Cluster::Leaf(switch) => *switch,
                _ => false,
            })
            .position(std::convert::identity)
            .map(|i| i as u8)
    }

    fn min_(&self, path_key: u8) -> Option<u8> {
        let min_cluster = self.min_cluster();
        let min_leaf = self.min_leaf();
        match (min_cluster, min_leaf) {
            (Some((cluster_idx, cluster)), None) => {
                let path_key = path_key + self.u_sqrt * cluster_idx;
                cluster.min_(path_key)
            }
            (None, Some(leaf_idx)) => Some(path_key + leaf_idx),
            (None, None) => None,
            (Some(_), Some(_)) => panic!("Pveb has both leaf and nonleaf children: {:#?}", self),
        }
    }

    pub fn set(&mut self, key: u8) {
        assert!(key < self.u);

        *self.find_mut(key) = true;

        let cluster_idx = self.high_bits(key);
        self.summary.iter_mut().for_each(|summary| {
            summary.n += 1;
            summary.pveb.set(cluster_idx);
        });
    }

    pub fn unset(&mut self, key: &u8) {
        let key = *key;
        assert!(key < self.u);

        *self.find_mut(key) = false;

        let cluster_idx = self.high_bits(key);
        self.summary.iter_mut().for_each(|summary| {
            summary.n = summary.n.saturating_sub(1);
            if summary.n == 0 {
                summary.pveb.unset(&cluster_idx);
            }
        });
    }

    pub fn is_set(&self, key: u8) -> bool {
        assert!(key < self.u);

        *self.find(key)
    }

    fn find_mut(&mut self, key: u8) -> &mut bool {
        assert!(key < self.u);

        let cluster_idx = self.high_bits(key) as usize;
        let element_idx = self.low_bits(key);

        match &mut self.clusters[cluster_idx] {
            Cluster::NonLeaf(pveb) => pveb.find_mut(element_idx),
            Cluster::Leaf(switch) => switch,
        }
    }

    fn find(&self, key: u8) -> &bool {
        assert!(key < self.u);

        let cluster_idx = self.high_bits(key) as usize;
        let element_idx = self.low_bits(key);

        match &self.clusters[cluster_idx] {
            Cluster::NonLeaf(pveb) => pveb.find(element_idx),
            Cluster::Leaf(switch) => switch,
        }
    }

    fn high_bits(&self, key: u8) -> u8 {
        key / self.u_sqrt
    }

    fn low_bits(&self, key: u8) -> u8 {
        key % self.u_sqrt
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::{anyhow, Result};
    use quickcheck::{Arbitrary, Gen};
    use quickcheck_macros::quickcheck;
    use std::collections::{BTreeSet, HashMap};
    use std::iter;
    use std::ops::Rem;

    #[derive(Clone, Debug)]
    enum MemberOp {
        Set(u8),
        Unset(u8),
    }

    impl Rem<u8> for MemberOp {
        type Output = Self;
        fn rem(mut self, rhs: u8) -> Self {
            let key = match &mut self {
                MemberOp::Set(key) => key,
                MemberOp::Unset(key) => key,
            };
            *key %= rhs;
            self
        }
    }

    impl Arbitrary for MemberOp {
        fn arbitrary<G: Gen>(g: &mut G) -> Self {
            let key = u8::arbitrary(g);
            match bool::arbitrary(g) {
                true => MemberOp::Set(key),
                false => MemberOp::Unset(key),
            }
        }
    }

    #[derive(Clone, Debug)]
    struct InsertionInput {
        u: usize,
        member_ops: Vec<MemberOp>,
    }

    impl Arbitrary for InsertionInput {
        fn arbitrary<G: Gen>(g: &mut G) -> Self {
            let m = u32::arbitrary(g) % 3;
            let u: usize = 2u32.pow(2u32.pow(m)) as usize;
            let key_cap = u as u8;

            let op_count = usize::arbitrary(g) % (u * 2);
            let op_gen = || Some(MemberOp::arbitrary(g) % key_cap);
            let op_gen = iter::from_fn(op_gen);
            let member_ops = op_gen.take(op_count).collect();

            InsertionInput { u, member_ops }
        }

        fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
            let u = self.u;
            Box::new(
                self.member_ops
                    .shrink()
                    .map(move |member_ops| InsertionInput { member_ops, u }),
            )
        }
    }

    #[derive(Debug)]
    struct Report<'a> {
        member_ops: &'a [MemberOp],
        expected: BTreeSet<u8>,
    }

    impl InsertionInput {
        /// Applies sets and unsets to the tree. Returns the set of what
        /// ought to be set in the tree at the end.
        fn apply(&self, pveb: &mut PvebU) -> Report {
            let mut expected = BTreeSet::new();
            self.member_ops.iter().cloned().for_each(|op| match op {
                MemberOp::Set(key) => {
                    pveb.set(key);
                    expected.insert(key);
                }
                MemberOp::Unset(key) => {
                    pveb.unset(&key);
                    expected.remove(&key);
                }
            });
            Report {
                member_ops: self.member_ops.as_slice(),
                expected,
            }
        }
    }

    #[quickcheck]
    fn is_set(insertion_input: InsertionInput) -> Result<()> {
        let mut pv = PvebU::new(Capacity::C16);

        let report = insertion_input.apply(&mut pv);

        let mut false_positive = vec![];
        let mut false_negative = vec![];
        for i in (0..(insertion_input.u)).map(|i| i as u8) {
            let expected_positive = report.expected.contains(&i);

            match (expected_positive, pv.is_set(i)) {
                (true, false) => false_negative.push(i),
                (false, true) => false_positive.push(i),
                _ => (),
            }
        }

        if !false_positive.is_empty() || !false_negative.is_empty() {
            return Err(anyhow!(
                concat!(
                    "Incorrect state after configuration {:#?}. ",
                    "False negatives: {:?}. ",
                    "False positives: {:?}."
                ),
                report,
                false_negative,
                false_positive
            ));
        }

        Ok(())
    }
}
