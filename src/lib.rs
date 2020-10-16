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

impl Cluster {
    pub fn is_empty(&self) -> bool {
        match self {
            Cluster::NonLeaf(pveb) => pveb.is_empty(),
            Cluster::Leaf(switch) => !*switch,
        }
    }
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
        let mut path_key = 0;
        let mut nav: Option<&PvebU> = Some(self);

        while let Some(pveb) = nav {
            let i = pveb.clusters.iter().position(|c| !c.is_empty())?;
            path_key += (i as u8) * pveb.u_sqrt;
            nav = match &pveb.clusters[i] {
                Cluster::Leaf(switch) => {
                    assert!(switch, "This is a requirement for !is_empty");
                    return Some(path_key);
                }
                Cluster::NonLeaf(pveb) => Some(pveb),
            };
        }

        None
    }

    pub fn is_empty(&self) -> bool {
        match &self.summary {
            Some(summary) => summary.n == 0,
            _ => {
                assert!(self.u == 2);
                self.clusters.iter().all(Cluster::is_empty)
            }
        }
    }

    pub fn set(&mut self, key: u8) {
        assert!(key < self.u);

        let was_set = {
            let leaf = self.search_mut(key, |_, _| {});
            let was_set = *leaf;
            *leaf = true;
            was_set
        };

        if !was_set {
            self.search_mut(key, |cluster_idx, summary| {
                summary.n += 1;
                summary.pveb.set(cluster_idx);
            });
        }
    }

    pub fn unset(&mut self, key: &u8) {
        let key = *key;
        assert!(key < self.u);

        let was_set = {
            let leaf = self.search_mut(key, |_, _| {});
            let was_set = *leaf;
            *leaf = false;
            was_set
        };

        if was_set {
            self.search_mut(key, |cluster_idx, summary| {
                summary.n = summary.n.saturating_sub(1);
                if summary.n == 0 {
                    summary.pveb.unset(&cluster_idx);
                }
            });
        }
    }

    pub fn is_set(&self, key: u8) -> bool {
        assert!(key < self.u);

        *self.find(key)
    }

    /// Searches the tree for the given key. Returns mutable reference to
    /// the appropriate leaf.
    ///
    /// On the descent, calls f with each summary block and the index of the
    /// cluster the algorithm will descend to in the next iteration.
    fn search_mut<F: Fn(u8, &mut Summary)>(&mut self, key: u8, f: F) -> &mut bool {
        assert!(key < self.u);

        let mut key = key;
        let mut nav: Option<&mut PvebU> = Some(self);

        while let Some(mut pveb) = nav {
            let cluster_idx = pveb.high_bits(key) as usize;
            key = pveb.low_bits(key);

            nav = match &mut pveb.clusters[cluster_idx] {
                Cluster::NonLeaf(lower_pveb) => {
                    if let Some(summary) = pveb.summary.as_mut() {
                        f(cluster_idx as u8, summary);
                    }
                    Some(lower_pveb)
                }
                Cluster::Leaf(switch) => return switch,
            }
        }

        unreachable!()
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
                    "False positives: {:?}. ",
                    "State: {:#?}"
                ),
                report,
                false_negative,
                false_positive,
                pv
            ));
        }

        Ok(())
    }

    #[quickcheck]
    fn min(insertion_input: InsertionInput) -> Result<()> {
        let mut pv = PvebU::new(Capacity::C16);

        let report = insertion_input.apply(&mut pv);
        let expected_min = report.expected.iter().next().cloned();
        let actual_min = pv.min();

        if actual_min != expected_min {
            return Err(anyhow!(
                concat!(
                    "Incorrect state after configuration: {:#?}. ",
                    "Expected minimum element: {:?}. ",
                    "Got minimum element: {:?}. ",
                    "State: {:#?}"
                ),
                report,
                expected_min,
                actual_min,
                pv
            ));
        }

        Ok(())
    }
}
