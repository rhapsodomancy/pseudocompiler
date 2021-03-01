pub mod var_to_u32;

pub trait Transformer<FROM, TO> {
    fn transform(&mut self, from: FROM) -> TO;
}

pub trait FallibleTransformer<FROM, TO> {
    type Error;
    fn try_transform(
        &mut self,
        from: FROM,
    ) -> Result<TO, <Self as FallibleTransformer<FROM, TO>>::Error>;
}
