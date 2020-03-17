pub trait Precedence {
    fn precedence(&self) -> u8;
}

impl<T: Precedence> Precedence for Box<T> {
    fn precedence(&self) -> u8 {
        (**self).precedence()
    }
}
