use crate::Not;

pub trait Predicate<Item> {
    fn eval(&self, value: &Item) -> bool;
}

impl<Item> Predicate<Item> for () {
    #[inline]
    fn eval(&self, _: &Item) -> bool {
        true
    }
}

impl<P, Item> Predicate<Item> for Option<P>
where
    P: Predicate<Item>,
{
    #[inline]
    fn eval(&self, value: &Item) -> bool {
        self.as_ref().is_none_or(|p| p.eval(value))
    }
}

impl<T, Item> Predicate<Item> for &T
where
    T: Predicate<Item>,
{
    #[inline]
    fn eval(&self, value: &Item) -> bool {
        T::eval(self, value)
    }
}

pub trait PredicateExt<Item>: Predicate<Item> + Sized {
    fn invert(self) -> Not<Self>;
}

impl<T, Item> PredicateExt<Item> for T
where
    T: Predicate<Item>,
{
    #[inline]
    fn invert(self) -> Not<Self> {
        Not::new(self)
    }
}
