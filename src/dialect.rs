use crate::ir::{Attribute, Context, OpaqueAttr, OpaqueOperation, Operation};

pub mod builtin;

pub trait Dialect {
    fn register(&self, ctx: &mut impl Context);
}

pub trait OperationKind<C: Context + ?Sized> {
    type Access<'a>;
    type Opaque<'rewrite>;

    fn access<'a>(ctx: &C, op: impl Operation<'a, C>) -> Option<Self::Access<'a>>;
    fn opaque<'rewrite>(ctx: &C, op: impl OpaqueOperation<'rewrite, C>) -> Option<Self::Opaque<'rewrite>>;
}

pub trait AttributeKind<C: Context + ?Sized> {
    type Access<'a>;
    type Opaque<'rewrite>;

    fn access<'a>(ctx: &C, op: impl Attribute<'a, C>) -> Option<Self::Access<'a>>;
    fn opaque<'rewrite>(ctx: &C, op: impl OpaqueAttr<'rewrite, C>) -> Option<Self::Opaque<'rewrite>>;
}
