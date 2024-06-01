use std::marker::PhantomData;

use crate::ir::Context;

use super::OperationKind;

pub struct ModuleOp<C: Context>(PhantomData<C>);

pub struct ModuleOpAccess<'a> {

}

impl<C: Context> OperationKind<C> for ModuleOp<C> {
    type Access<'a>;

    type Opaque<'rewrite>;

    fn access<'a>(op: impl crate::ir::Operation<'a, C>) -> Option<Self::Access<'a>> {
        todo!()
    }

    fn opaque<'rewrite>(op: impl crate::ir::OpaqueOperation<'rewrite, C>) -> Option<Self::Opaque<'rewrite>> {
        todo!()
    }
}