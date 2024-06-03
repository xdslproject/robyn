use crate::ir::{Attribute, Context, OpaqueAttr, OpaqueOperation, Operation};

pub mod builtin;

pub trait Dialect {
    fn register(&self, ctx: &mut impl Context);
}

pub trait OperationKind: 'static {
    type Dialect: Dialect;

    type Access<'a, C>: OperationKindAccess<'a, C, Self> + AsRef<C::Operation<'a>>
    where
        C: Context + ?Sized;
    type Opaque<'rewrite, C>: AsRef<C::OpaqueOperation<'rewrite>>
    where
        C: Context + ?Sized;

    /// Updgrades an operation access into a kinded operation access, to provide
    /// helpers for accessing the content of the operation.
    ///
    /// This method must check that the provided operation is indeed of the
    /// expected kind and satisfies the expectations of the operation kind.
    fn access<'a, C: Context + ?Sized>(op: impl Operation<'a, C>) -> Option<Self::Access<'a, C>>;
}

pub trait OperationKindAccess<'a, C: Context + ?Sized, O: OperationKind + ?Sized> {
    fn opaque<'rewrite>(op: impl OpaqueOperation<'rewrite, C>) -> Option<O::Opaque<'rewrite, C>>;
}

pub trait AttributeKind: 'static {
    type Dialect: Dialect;

    type Access<'a, C>: AttributeKindAccess<'a, C, Self> + AsRef<C::Attribute<'a>>
    where
        C: Context + ?Sized;
    type Opaque<'rewrite, C>: AsRef<C::OpaqueAttr<'rewrite>>
    where
        C: Context + ?Sized;

    /// Updgrades an attribute access into a kinded attribute access, to provide
    /// helpers for accessing the content of the attribute.
    ///
    /// This method must check that the provided attribute is indeed of the
    /// expected kind and satisfies the expectations of the attribute kind.
    fn access<'a, C: Context + ?Sized>(op: impl Attribute<'a, C>) -> Option<Self::Access<'a, C>>;
}

pub trait AttributeKindAccess<'a, C: Context + ?Sized, A: AttributeKind + ?Sized> {
    fn opaque<'rewrite>(op: impl OpaqueAttr<'rewrite, C>) -> Option<A::Opaque<'rewrite, C>>;
}
