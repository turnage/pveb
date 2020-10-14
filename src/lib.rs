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

    pub fn set(&mut self, key: u8) {
        assert!(key < self.u);

        *self.find_mut(key) = true;

        let cluster_idx = self.high_bits(key);
        self.summary.iter_mut().for_each(|summary| {
            summary.n += 1;
            summary.pveb.set(cluster_idx);
        });
    }

    pub fn unset(&mut self, key: u8) {
        assert!(key < self.u);

        *self.find_mut(key) = false;

        let cluster_idx = self.high_bits(key);
        self.summary.iter_mut().for_each(|summary| {
            summary.n = summary.n.saturating_sub(1);
            if summary.n == 0 {
                summary.pveb.unset(cluster_idx);
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

    #[test]
    fn is_set_c2() {
        let mut pv = PvebU::new(Capacity::C2);

        assert!(!pv.is_set(0));
        assert!(!pv.is_set(1));

        pv.set(1);
        assert!(!pv.is_set(0));
        assert!(pv.is_set(1));

        pv.set(0);
        assert!(pv.is_set(0));
        assert!(pv.is_set(1));
    }

    #[test]
    fn is_set_c4() {
        let mut pv = PvebU::new(Capacity::C4);

        assert!(!pv.is_set(0));
        assert!(!pv.is_set(1));

        pv.set(1);
        assert!(!pv.is_set(0));
        assert!(pv.is_set(1), "{:#?}", pv);

        pv.set(3);
        assert!(pv.is_set(3));
        assert!(pv.is_set(1));
    }

    #[test]
    fn is_set_c16() {
        let mut pv = PvebU::new(Capacity::C16);

        assert!(!pv.is_set(0));
        assert!(!pv.is_set(1));

        pv.set(1);
        assert!(!pv.is_set(0));
        assert!(pv.is_set(1), "{:#?}", pv);

        pv.set(12);
        assert!(pv.is_set(12));
        assert!(pv.is_set(1));
    }
}
