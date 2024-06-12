use std::ops::Deref;

use crate::ir::{Attribute, Context, OpaqueAttr, OpaqueOperation, Operation};

pub mod builtin;

pub trait Dialect {
    fn register(ctx: &mut impl Context);
}

pub trait OperationKind: 'static {
    type Dialect: Dialect;

    type Access<'rewrite, 'a, C>: OperationKindAccess<'rewrite, 'a, C, Self>
        + AsRef<C::Operation<'rewrite, 'a>>
    where
        C: Context;
    type Opaque<'rewrite, C>: AsRef<C::OpaqueOperation<'rewrite>>
    where
        C: Context;

    /// Upgrades an operation access into a kinded operation access, to provide
    /// helpers for accessing the content of the operation.
    ///
    /// This method must check that the provided operation is indeed of the
    /// expected kind and satisfies the expectations of the operation kind.
    fn access<'rewrite, 'a, C: Context>(
        op: C::Operation<'rewrite, 'a>,
    ) -> Option<Self::Access<'rewrite, 'a, C>>;

    /// Returns true if the provided operation's structure is sufficiently valid
    /// to be accessed as this operation.
    fn valid_access<'rewrite, 'a, C: Context>(attr: C::Operation<'rewrite, 'a>) -> bool;
}

pub trait OperationKindAccess<'rewrite, 'a, C: Context, O: OperationKind + ?Sized> {
    fn opaque(&self) -> O::Opaque<'rewrite, C>;
}

pub trait AttributeKind: 'static {
    type Dialect: Dialect;

    type Access<'rewrite, 'a, C>: AttributeKindAccess<'rewrite, 'a, C, Self>
        + AsRef<C::Attribute<'rewrite, 'a>>
    where
        C: Context;
    type Opaque<'rewrite, C>: AsRef<C::OpaqueAttr<'rewrite>>
    where
        C: Context;

    /// Upgrades an attribute access into a kinded attribute access, to provide
    /// helpers for accessing the content of the attribute.
    ///
    /// This method must check that the provided attribute is indeed of the
    /// expected kind and satisfies the expectations of the attribute kind.
    fn access<'rewrite, 'a, C: Context>(
        attr: C::Attribute<'rewrite, 'a>,
    ) -> Option<Self::Access<'rewrite, 'a, C>>;

    /// Returns true if the provided attribute's structure is sufficiently valid
    /// to be accessed as this operation.
    fn valid_access<'rewrite, 'a, C: Context>(attr: C::Attribute<'rewrite, 'a>) -> bool;
}

pub trait AttributeKindAccess<'rewrite, 'a, C: Context, A: AttributeKind + ?Sized> {
    fn opaque(&self) -> A::Opaque<'rewrite, C>;
}

macro_rules! declare_operation {
    ($name:ident, $access:ident, $opaque:ident) => {
        pub struct $name;
        pub struct $access<'rewrite, 'a, C: Context>(C::Operation<'rewrite, 'a>);
        pub struct $opaque<'rewrite, C: Context>(C::OpaqueOperation<'rewrite>);

        impl<'rewrite, 'a, C: Context> OperationKindAccess<'rewrite, 'a, C, $name>
            for $access<'rewrite, 'a, C>
        {
            fn opaque(&self) -> $opaque<'rewrite, C> {
                $opaque(self.0.opaque())
            }
        }

        impl<'rewrite, 'a, C: Context> AsRef<C::Operation<'rewrite, 'a>>
            for $access<'rewrite, 'a, C>
        {
            fn as_ref(&self) -> &C::Operation<'rewrite, 'a> {
                &self.0
            }
        }

        impl<'rewrite, C: Context> AsRef<C::OpaqueOperation<'rewrite>> for $opaque<'rewrite, C> {
            fn as_ref(&self) -> &C::OpaqueOperation<'rewrite> {
                &self.0
            }
        }
    };
}

macro_rules! operation_defaults {
    ($dialect:ident, $access:ident, $opaque:ident) => {
        type Dialect = $dialect;

        type Access<'rewrite, 'a, C> = $access<'rewrite, 'a, C> where C: Context;
        type Opaque<'rewrite, C> = $opaque<'rewrite, C> where C: Context;

        fn access<'rewrite, 'a, C: Context>(
            op: C::Operation<'rewrite, 'a>,
        ) -> Option<Self::Access<'rewrite, 'a, C>> {
            Self::valid_access::<C>(op.clone()).then(|| $access(op))
        }
    };
}

macro_rules! declare_attr {
    ($name:ident, $access:ident, $opaque:ident) => {
        pub struct $name;
        pub struct $access<'rewrite, 'a, C: Context>(C::Attribute<'rewrite, 'a>);
        pub struct $opaque<'rewrite, C: Context>(C::OpaqueAttr<'rewrite>);

        impl<'rewrite, 'a, C: Context> AttributeKindAccess<'rewrite, 'a, C, $name>
            for $access<'rewrite, 'a, C>
        {
            fn opaque(&self) -> $opaque<'rewrite, C> {
                use crate::ir::Attribute;
                $opaque(self.0.opaque())
            }
        }

        impl<'rewrite, 'a, C: Context> AsRef<C::Attribute<'rewrite, 'a>>
            for $access<'rewrite, 'a, C>
        {
            fn as_ref(&self) -> &C::Attribute<'rewrite, 'a> {
                &self.0
            }
        }

        impl<'rewrite, C: Context> AsRef<C::OpaqueAttr<'rewrite>> for $opaque<'rewrite, C> {
            fn as_ref(&self) -> &C::OpaqueAttr<'rewrite> {
                &self.0
            }
        }
    };
}

macro_rules! attr_defaults {
    ($dialect:ident, $access:ident, $opaque:ident) => {
        type Dialect = $dialect;

        type Access<'rewrite, 'a, C> = $access<'rewrite, 'a, C> where C: Context;
        type Opaque<'rewrite, C> = $opaque<'rewrite, C> where C: Context;

        fn access<'rewrite, 'a, C: Context>(
            attr: C::Attribute<'rewrite, 'a>,
        ) -> Option<Self::Access<'rewrite, 'a, C>> {
            Self::valid_access::<C>(attr.clone()).then(|| $access(attr))
        }
    };
}

pub(crate) use {attr_defaults, declare_attr, declare_operation, operation_defaults};
