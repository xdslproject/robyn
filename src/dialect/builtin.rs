use crate::{
    ir::{AttrData, Attribute, AttributeData, Context, OpaqueAttributeData, Operation, Region, Rewriter},
    utils::bitbox::{BitBox, TooManyBytesError},
};

use super::{
    attr_defaults, declare_attr, declare_operation, operation_defaults, AttributeKind, Dialect, OperationKind,
    OperationKindAccess,
};
use crate::dialect::AttributeKindAccess;
pub struct BuiltinDialect;

impl Dialect for BuiltinDialect {
    fn register(ctx: &mut impl Context) {
        ctx.register_operation::<ModuleOp>();

        ctx.register_attribute::<StringAttr>();
    }
}

//============================================================================//
// ModuleOp
//============================================================================//

declare_operation!(ModuleOp, ModuleOpAccess, ModuleOpOpaque);

impl OperationKind for ModuleOp {
    operation_defaults!(BuiltinDialect, ModuleOpAccess, ModuleOpOpaque);

    fn valid_access<'rewrite, 'a, C: Context>(op: C::Operation<'rewrite, 'a>) -> bool {
        op.isa::<ModuleOp>() && op.get_region(0).and_then(|r| r.get_block(0)).is_some()
    }
}

impl ModuleOp {
    pub fn create<'rewrite, C: Context>(
        rewriter: &C::Rewriter<'rewrite>,
        content: &[C::OpaqueOperation<'rewrite>],
    ) -> C::OpaqueOperation<'rewrite> {
        let module_block = rewriter.create_block(&[], content);
        let module_region = rewriter.create_region(&[module_block]);
        rewriter.create_op::<ModuleOp>(&[], &[], &[], &[], &[module_region])
    }
}

impl<'rewrite, 'a, C: Context> ModuleOpAccess<'rewrite, 'a, C> {
    pub fn get_body(&self) -> C::Block<'rewrite, 'a> {
        self.as_ref()
            .get_region(0)
            .expect("verified")
            .get_block(0)
            .expect("verified")
    }
}

//============================================================================//
// Standard MLIR attributes
//============================================================================//

declare_attr!(StringAttr, StringAttrAccess, StringAttrOpaque);

impl AttributeKind for StringAttr {
    attr_defaults!(BuiltinDialect, StringAttrAccess, StringAttrOpaque);

    fn valid_access<'rewrite, 'a, C: Context>(attr: C::Attribute<'rewrite, 'a>) -> bool {
        attr.isa::<StringAttr>()
            && match attr.data() {
                AttributeData::Bits(bits) => bits.len() % 8 == 0,
                _ => false,
            }
    }
}

impl StringAttr {
    pub fn create<'rewrite, C: Context>(
        rewriter: &C::Rewriter<'rewrite>,
        data: &[u8],
    ) -> Result<C::OpaqueAttr<'rewrite>, TooManyBytesError> {
        Ok(rewriter.create_attribute::<StringAttr>(OpaqueAttributeData::Bits(BitBox::from_bytes(data)?)))
    }
}

impl<'rewrite, 'a, C: Context + 'a> StringAttrAccess<'rewrite, 'a, C> {
    pub fn as_bytes<'data>(&'data self) -> impl AttrData<'data, [u8]> {
        let AttributeData::Bits(bits) = self.as_ref().data() else {
            unreachable!("verified")
        };
        bits.map_ref(|x| x.as_bytes().expect("verified"))
    }
}
