use std::{marker::PhantomData, ops::Deref};

use crate::ir::{Context, OpaqueOperation, Operation, Region, Rewriter};

use super::{declare_operation, operation_defaults, Dialect, OperationKind, OperationKindAccess};

pub struct BuiltinDialect;

impl Dialect for BuiltinDialect {
    fn register(ctx: &mut impl Context) {
        ctx.register_operation::<ModuleOp>();
    }
}

declare_operation!(ModuleOp, ModuleOpAccess, ModuleOpOpaque);

impl OperationKind for ModuleOp {
    operation_defaults!(BuiltinDialect, ModuleOpAccess, ModuleOpOpaque);

    fn access<'rewrite, 'a, C: Context + ?Sized>(
        op: C::Operation<'rewrite, 'a>,
    ) -> Option<Self::Access<'rewrite, 'a, C>> {
        (op.isa::<ModuleOp>() && op.get_region(0).and_then(|r| r.get_block(0)).is_some())
            .then(|| ModuleOpAccess(op))
    }
}

impl ModuleOp {
    pub fn create<'rewrite, C: Context>(
        rewriter: &C::Rewriter<'rewrite>,
    ) -> C::OpaqueOperation<'rewrite> {
        let module_block = rewriter.create_block(&[], &[]);
        let module_region = rewriter.create_region(&[module_block]);
        rewriter.create_op::<ModuleOp>(&[], &[], &[], &[], &[module_region])
    }
}

impl<'rewrite, 'a, C: Context> ModuleOpAccess<'rewrite, 'a, C> {
    pub fn get_body(&self) -> C::Block<'rewrite, 'a> {
        self.0
            .get_region(0)
            .expect("verified")
            .get_block(0)
            .expect("verified")
    }
}
