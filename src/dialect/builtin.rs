use std::marker::PhantomData;

use crate::ir::Context;

use super::{Dialect, OperationKind};

pub struct BuiltinDialect;

impl Dialect for BuiltinDialect {
    fn register(&self, ctx: &mut impl Context) {
        todo!()
    }
}

pub struct ModuleOp;

pub struct ModuleOpAccess<'a, C: Context + ?Sized> {

}

impl OperationKind for ModuleOp {
    type Dialect = BuiltinDialect;

    type Access<'a, C> = ModuleOpAccess<'a, C> where C: Context + ?Sized;
    type Opaque<'rewrite, C>;

    fn access<'a, C: Context + ?Sized>(op: impl crate::ir::Operation<'a, C>) -> Option<Self::Access<'a>> {
        todo!()
    }
}
