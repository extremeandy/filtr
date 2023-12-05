use std::{convert, marker::PhantomData};

use super::Predicate;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A recursive tree structure representing a logical predicate expression.
///
/// `PredicateTree` allows combining individual predicates using logical operators
/// to form complex expressions. Each node in the tree represents either a logical
/// operation (`All`, `Any`, `Not`) or a leaf predicate.
///
/// # Variants
///
/// - `All`
///   Represents a logical AND of all child predicates. Evaluates to `true` if
///   every child predicate evaluates to `true`.
///
/// - `Any`
///   Represents a logical OR of all child predicates. Evaluates to `true` if
///   at least one child predicate evaluates to `true`.
///
/// - `Not`
///   Represents a logical NOT of a single child predicate. Evaluates to the
///   negation of the child predicate.
///
/// - `Leaf`
///   A leaf node containing a single predicate of type `T`. This is the atomic
///   unit of evaluation.
///
/// # Type Parameter
///
/// - `T`: The type of atomic predicates stored in leaf nodes. Typically, `T`
///   should implement a trait such as `Predicate<Item>` to provide evaluation
///   logic.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[cfg_attr(
    feature = "serde",
    derive(Serialize, Deserialize),
    serde(tag = "op", content = "node")
)]
pub enum PredicateTree<T> {
    All(Vec<Self>),
    Any(Vec<Self>),
    Not(Box<Self>),
    Leaf(T),
}

impl<T> PredicateTree<T> {
    pub const TRUE: PredicateTree<T> = Self::All(vec![]);
    pub const FALSE: PredicateTree<T> = Self::Any(vec![]);

    #[inline]
    pub fn all(nodes: impl IntoIterator<Item = impl Into<Self>>) -> Self {
        Self::All(nodes.into_iter().map(Into::into).collect())
    }

    #[inline]
    pub fn any(nodes: impl IntoIterator<Item = impl Into<Self>>) -> Self {
        Self::Any(nodes.into_iter().map(Into::into).collect())
    }

    #[inline]
    pub fn not(node: impl Into<Self>) -> Self {
        Self::Not(Box::new(node.into()))
    }

    #[inline]
    pub fn leaf(value: T) -> Self {
        Self::Leaf(value)
    }

    #[inline]
    pub fn fold_into<C, I, F, R>(self, combine: C, invert: I, mut transform: F) -> R
    where
        for<'i> C: Fn(Box<dyn Iterator<Item = R> + 'i>, CombinationOperatorKind) -> R,
        I: Fn(R) -> R,
        F: FnMut(T) -> R,
    {
        match self {
            Self::All(nodes) => {
                let transformed_nodes = Box::new(
                    nodes
                        .into_iter()
                        .map(|node| node.fold_into(&combine, &invert, &mut transform)),
                );

                combine(transformed_nodes, CombinationOperatorKind::All)
            }
            Self::Any(nodes) => {
                let transformed_nodes = Box::new(
                    nodes
                        .into_iter()
                        .map(|node| node.fold_into(&combine, &invert, &mut transform)),
                );

                combine(transformed_nodes, CombinationOperatorKind::Any)
            }
            Self::Not(node) => {
                let transformed_node = node.fold_into(combine, &invert, transform);

                invert(transformed_node)
            }
            Self::Leaf(leaf) => transform(leaf),
        }
    }

    #[inline]
    pub fn fold<C, I, F, R>(&self, combine: C, invert: I, mut transform: F) -> R
    where
        for<'i> C: Fn(Box<dyn Iterator<Item = R> + 'i>, CombinationOperatorKind) -> R,
        I: Fn(R) -> R,
        for<'l> F: FnMut(&'l T) -> R,
    {
        match self {
            Self::All(nodes) => {
                let transformed_nodes = Box::new(
                    nodes
                        .iter()
                        .map(|node| node.fold(&combine, &invert, &mut transform)),
                );

                combine(transformed_nodes, CombinationOperatorKind::All)
            }
            Self::Any(nodes) => {
                let transformed_nodes = Box::new(
                    nodes
                        .iter()
                        .map(|node| node.fold(&combine, &invert, &mut transform)),
                );

                combine(transformed_nodes, CombinationOperatorKind::Any)
            }
            Self::Not(node) => {
                let transformed_node = node.fold(combine, &invert, transform);

                invert(transformed_node)
            }
            Self::Leaf(leaf) => transform(leaf),
        }
    }

    #[inline]
    pub fn map<F, R>(self, mut f: F) -> PredicateTree<R>
    where
        F: FnMut(T) -> R,
    {
        match self {
            Self::All(nodes) => PredicateTree::all(nodes.into_iter().map(|node| node.map(&mut f))),
            Self::Any(nodes) => PredicateTree::any(nodes.into_iter().map(|node| node.map(&mut f))),
            Self::Not(node) => PredicateTree::not(node.map(f)),
            Self::Leaf(leaf) => PredicateTree::leaf(f(leaf)),
        }
    }
}

impl<F, FN, Op, T> From<Combination<F, FN, Op>> for PredicateTree<T>
where
    F: Into<PredicateTree<T>>,
    FN: IntoIterator<Item = F>,
    Op: Into<CombinationOperatorKind>,
{
    #[inline]
    fn from(value: Combination<F, FN, Op>) -> Self {
        let Combination { nodes, op, _marker } = value;
        let nodes: Vec<_> = nodes.into_iter().map(Into::into).collect();

        let op: CombinationOperatorKind = op.into();

        match op {
            CombinationOperatorKind::All => Self::All(nodes),
            CombinationOperatorKind::Any => Self::Any(nodes),
        }
    }
}

impl<T> From<Not<Self>> for PredicateTree<T> {
    #[inline]
    fn from(value: Not<Self>) -> Self {
        Self::Not(Box::new(value.into_inner()))
    }
}

impl<T> From<Leaf<T>> for PredicateTree<T> {
    #[inline]
    fn from(value: Leaf<T>) -> Self {
        Self::Leaf(value.into_inner())
    }
}

impl<T> Default for PredicateTree<T>
where
    T: Default,
{
    #[inline]
    fn default() -> Self {
        Self::Leaf(T::default())
    }
}

impl<T> PredicateTree<T>
where
    T: Ord,
{
    #[inline]
    pub fn sort(&mut self) {
        match self {
            Self::All(nodes) => nodes.sort(),
            Self::Any(nodes) => nodes.sort(),
            Self::Not(node) => node.as_mut().sort(),
            Self::Leaf(_) => {}
        }
    }

    #[inline]
    pub fn sort_unstable(&mut self) {
        match self {
            Self::All(nodes) => nodes.sort_unstable(),
            Self::Any(nodes) => nodes.sort_unstable(),
            Self::Not(node) => node.as_mut().sort_unstable(),
            Self::Leaf(_) => {}
        }
    }
}

impl<T> std::fmt::Display for PredicateTree<T>
where
    T: std::fmt::Display,
{
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PredicateTree::All(nodes) => Combination::<_, _, All>::all(nodes).fmt(f),
            PredicateTree::Any(nodes) => Combination::<_, _, Any>::any(nodes).fmt(f),
            PredicateTree::Not(node) => Not::new(node).fmt(f),
            PredicateTree::Leaf(filter) => filter.fmt(f),
        }
    }
}

impl<T, Item> Predicate<Item> for PredicateTree<T>
where
    T: Predicate<Item>,
{
    #[inline]
    fn eval(&self, value: &Item) -> bool {
        match self {
            Self::All(nodes) => Combination::all(nodes).eval(value),
            Self::Any(nodes) => Combination::any(nodes).eval(value),
            Self::Not(node) => Not::new(node.as_ref()).eval(value),
            Self::Leaf(term) => term.eval(value),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Combination<T, N = Vec<T>, Op = CombinationOperatorKind> {
    pub nodes: N,
    pub op: Op,
    _marker: PhantomData<T>,
}

impl<T, N, Op> Combination<T, N, Op> {
    #[inline]
    pub fn new(nodes: N, op: Op) -> Self {
        Self {
            nodes,
            op,
            _marker: PhantomData,
        }
    }
}

impl<T, N> FromIterator<T> for Combination<T, N, All>
where
    N: FromIterator<T>,
{
    #[inline]
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = T>,
    {
        Self {
            nodes: iter.into_iter().collect(),
            op: All,
            _marker: PhantomData,
        }
    }
}

impl<T, N> Combination<T, N, All> {
    #[inline]
    pub fn all(nodes: N) -> Self {
        Self {
            nodes,
            op: All,
            _marker: PhantomData,
        }
    }
}

impl<T, N> FromIterator<T> for Combination<T, N, Any>
where
    N: FromIterator<T>,
{
    #[inline]
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = T>,
    {
        Self {
            nodes: iter.into_iter().collect(),
            op: Any,
            _marker: PhantomData,
        }
    }
}

impl<T, N> Combination<T, N, Any> {
    #[inline]
    pub fn any(nodes: N) -> Self {
        Self {
            nodes,
            op: Any,
            _marker: PhantomData,
        }
    }
}

impl<T, N, Op> Combination<T, N, Op>
where
    N: IntoIterator<Item = T>,
{
    #[inline]
    pub fn map<R, RN>(self, f: impl FnMut(T) -> R) -> Combination<R, RN, Op>
    where
        RN: FromIterator<R>,
    {
        let Self {
            nodes,
            op,
            _marker: _,
        } = self;

        let nodes = nodes.into_iter().map(f).collect();

        Combination::new(nodes, op)
    }
}

impl<T, N> From<Combination<T, N, All>> for Combination<T, N, CombinationOperatorKind> {
    #[inline]
    fn from(value: Combination<T, N, All>) -> Self {
        let Combination {
            nodes,
            op,
            _marker: _,
        } = value;

        Self::new(nodes, CombinationOperatorKind::from(op))
    }
}

impl<T, N> From<Combination<T, N, Any>> for Combination<T, N, CombinationOperatorKind> {
    #[inline]
    fn from(value: Combination<T, N, Any>) -> Self {
        let Combination {
            nodes,
            op,
            _marker: _,
        } = value;

        Self::new(nodes, CombinationOperatorKind::from(op))
    }
}

impl<T, N, Op> Combination<T, N, Op>
where
    T: Ord,
    N: AsMut<[T]>,
{
    #[inline]
    pub fn sort(&mut self) {
        self.nodes.as_mut().sort();
    }

    #[inline]
    pub fn sort_unstable(&mut self) {
        self.nodes.as_mut().sort_unstable();
    }
}

impl<T, N, Op, Item> Predicate<Item> for Combination<T, N, Op>
where
    T: Predicate<Item>,
    N: AsRef<[T]>,
    Op: CombinationOperator,
{
    #[inline]
    fn eval(&self, value: &Item) -> bool {
        self.op
            .eval(self.nodes.as_ref().iter().map(|node| node.eval(value)))
    }
}

impl<T, N, Op> std::fmt::Display for Combination<T, N, Op>
where
    T: std::fmt::Display,
    N: AsRef<[T]>,
    for<'o> &'o Op: Into<CombinationOperatorKind>,
{
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self { nodes, op, _marker } = self;
        let op = op.into();

        let mut nodes = nodes.as_ref().iter();

        let Some(first_node) = nodes.next() else {
            return match op {
                CombinationOperatorKind::All => f.write_str("true"),
                CombinationOperatorKind::Any => f.write_str("false"),
            };
        };

        f.write_str("(")?;
        first_node.fmt(f)?;

        let join = match op {
            CombinationOperatorKind::All => " && ",
            CombinationOperatorKind::Any => " || ",
        };

        for node in nodes {
            f.write_str(join)?;
            node.fmt(f)?;
        }

        f.write_str(")")
    }
}

trait CombinationOperator {
    fn eval<I>(&self, iter: I) -> bool
    where
        I: Iterator<Item = bool>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, derive_more::Display)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct All;

impl CombinationOperator for All {
    #[inline]
    fn eval<I>(&self, mut iter: I) -> bool
    where
        I: Iterator<Item = bool>,
    {
        iter.all(convert::identity)
    }
}

impl From<All> for CombinationOperatorKind {
    #[inline]
    fn from(_: All) -> Self {
        Self::All
    }
}

impl From<&All> for CombinationOperatorKind {
    #[inline]
    fn from(_: &All) -> Self {
        Self::All
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, derive_more::Display)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Any;

impl CombinationOperator for Any {
    #[inline]
    fn eval<I>(&self, mut iter: I) -> bool
    where
        I: Iterator<Item = bool>,
    {
        iter.any(convert::identity)
    }
}

impl From<Any> for CombinationOperatorKind {
    #[inline]
    fn from(_: Any) -> Self {
        Self::Any
    }
}

impl From<&Any> for CombinationOperatorKind {
    #[inline]
    fn from(_: &Any) -> Self {
        Self::Any
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, derive_more::Display)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum CombinationOperatorKind {
    All,
    Any,
}

impl CombinationOperator for CombinationOperatorKind {
    #[inline]
    fn eval<I>(&self, iter: I) -> bool
    where
        I: Iterator<Item = bool>,
    {
        match self {
            CombinationOperatorKind::All => All.eval(iter),
            CombinationOperatorKind::Any => Any.eval(iter),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Not<T>(T);

impl<T> Not<T> {
    #[inline]
    pub fn new(term: T) -> Self {
        Self(term)
    }

    #[inline]
    pub fn inner(&self) -> &T {
        &self.0
    }

    #[inline]
    pub fn into_inner(self) -> T {
        self.0
    }

    #[inline]
    pub fn map<R>(self, f: impl FnOnce(T) -> R) -> Not<R> {
        Not(f(self.0))
    }
}

impl<F> std::fmt::Display for Not<F>
where
    F: std::fmt::Display,
{
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self(node) = self;
        f.write_str("!(")?;
        node.fmt(f)?;
        f.write_str(")")
    }
}

impl<T, Item> Predicate<Item> for Not<T>
where
    T: Predicate<Item>,
{
    #[inline]
    fn eval(&self, value: &Item) -> bool {
        !self.inner().eval(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Leaf<T>(T);

impl<T> Leaf<T> {
    #[inline]
    pub fn new(node: T) -> Self {
        Self(node)
    }

    #[inline]
    pub fn inner(&self) -> &T {
        &self.0
    }

    #[inline]
    pub fn into_inner(self) -> T {
        self.0
    }

    #[inline]
    pub fn map<R>(self, f: impl FnOnce(T) -> R) -> Leaf<R> {
        Leaf(f(self.0))
    }
}

impl<T, Item> Predicate<Item> for Leaf<T>
where
    T: Predicate<Item>,
{
    #[inline]
    fn eval(&self, value: &Item) -> bool {
        self.inner().eval(value)
    }
}

#[cfg(test)]
mod tests {
    use std::ops::RangeInclusive;

    use super::*;

    #[derive(Debug)]
    #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
    struct RangeFilter(RangeInclusive<i32>);

    impl Predicate<i32> for RangeFilter {
        fn eval(&self, value: &i32) -> bool {
            self.0.contains(value)
        }
    }

    impl From<RangeFilter> for Leaf<RangeFilter> {
        fn from(value: RangeFilter) -> Self {
            Leaf::new(value)
        }
    }

    #[test]
    fn test() {
        let filter_5_to_10 = RangeFilter(5..=10);
        let filter_8_to_15 = RangeFilter(8..=15);

        let filter_9_to_12 = RangeFilter(9..=12);
        let filter_11 = RangeFilter(11..=11);

        // The expression will be true for values that are:
        // 1. Between 5 to 10 OR 8 to 15
        // 2. Between 9 to 12
        // 3. Not equal to 11
        let filter: PredicateTree<RangeFilter> = PredicateTree::all([
            PredicateTree::any([Leaf::new(filter_5_to_10), Leaf::new(filter_8_to_15)]),
            PredicateTree::leaf(filter_9_to_12),
            PredicateTree::not(Leaf::new(filter_11)),
        ]);

        let values = 9..=12;

        let filtered_values = values
            .into_iter()
            .filter(|v| filter.eval(&v))
            .collect::<Vec<_>>();

        let expected_values = vec![9, 10, 12];

        assert_eq!(filtered_values, expected_values);
    }

    #[cfg(feature = "serde")]
    #[test]
    fn test_serde_serialization() {
        use serde_json;

        let filter_5_to_10 = RangeFilter(5..=10);
        let filter_8_to_15 = RangeFilter(8..=15);

        let filter_9_to_12 = RangeFilter(9..=12);
        let filter_11 = RangeFilter(11..=11);

        let filter: PredicateTree<RangeFilter> = PredicateTree::all([
            PredicateTree::any([Leaf::new(filter_5_to_10), Leaf::new(filter_8_to_15)]),
            PredicateTree::leaf(filter_9_to_12),
            PredicateTree::not(Leaf::new(filter_11)),
        ]);

        // Test serialization
        let serialized = serde_json::to_string(&filter).expect("Failed to serialize");
        println!("Serialized filter: {}", serialized);

        // Test deserialization
        let deserialized: PredicateTree<RangeFilter> =
            serde_json::from_str(&serialized).expect("Failed to deserialize");

        // Verify the deserialized filter works the same
        let values = 1..=20;
        let filtered_values = values
            .into_iter()
            .filter(|v| deserialized.eval(&v))
            .collect::<Vec<_>>();

        let expected_values = vec![9, 10, 12];
        assert_eq!(filtered_values, expected_values);
    }
}
