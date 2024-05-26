#[cfg(feature = "gc")]
mod gc;
#[cfg(feature = "gc")]
pub use gc::GcContext;

use crate::utils::ApInt;

pub type OperationID = u32;
pub type AttributeID = u32;

pub type OperandPosition = u32;
pub type SuccessorPosition = u32;
pub type RegionPosition = u32;

pub trait Context {
    // IR
    type Operation<'a>: Operation<'a, Self>;
    type Attribute<'a>: Attribute<'a, Self>;
    type Block<'a>: Block<'a, Self>;
    type Region<'a>: Region<'a, Self>;
    type Value<'a>: Value<'a, Self>;

    // Rewriting
    type Accessor<'rewrite>: Accessor<'rewrite, Self>;
    type Rewriter<'rewrite>: Rewriter<'rewrite, Self>;

    type OpaqueOperation<'rewrite>: OpaqueOperation<'rewrite, Self>;
    type OpaqueAttr<'rewrite>;
    type OpaqueBlock<'rewrite>;
    type OpaqueRegion<'rewrite>;
    type OpaqueValue<'rewrite>;
}

//============================================================================//
// Rewriting
//============================================================================//

pub trait RewritePattern<C: Context + ?Sized> {
    fn match_and_rewrite(&self, accessor: C::Accessor<'_>);
}

pub trait Accessor<'rewrite, C: Context + ?Sized> {
    fn get_root(&self) -> C::Operation<'_>;

    fn get_placeholder_value(&self, r#type: C::Attribute<'_>) -> C::Value<'_>;

    fn get_int_data_attr(&self, data: ApInt) -> C::Attribute<'_>;
    fn get_string_attr(&self, data: impl ToString) -> C::Attribute<'_>;

    fn rewrite(self) -> C::Rewriter<'rewrite>;
    fn apply_pattern<P: RewritePattern<C>>(
        &mut self,
        pattern: &P,
        operation: C::OpaqueOperation<'rewrite>,
    );
    fn apply_pattern_dyn(
        &mut self,
        pattern: &dyn RewritePattern<C>,
        operation: C::OpaqueOperation<'rewrite>,
    );
}

pub trait Rewriter<'rewrite, C: Context + ?Sized> {
    fn replace_op(&self, op: C::OpaqueOperation<'rewrite>, with: C::OpaqueOperation<'rewrite>);
    fn erase_op(&self, op: C::OpaqueOperation<'rewrite>);
    fn insert_op_before(
        &self,
        op: C::OpaqueOperation<'rewrite>,
        other: C::OpaqueOperation<'rewrite>,
    );
    fn insert_op_after(
        &self,
        op: C::OpaqueOperation<'rewrite>,
        other: C::OpaqueOperation<'rewrite>,
    );
    fn insert_op_at_start(
        &self,
        op: C::OpaqueOperation<'rewrite>,
        at_start_of: C::OpaqueBlock<'rewrite>,
    );
    fn insert_op_at_end(
        &self,
        op: C::OpaqueOperation<'rewrite>,
        at_end_of: C::OpaqueBlock<'rewrite>,
    );

    fn create_op(
        &self,
        operation: OperationID,
        operands: &[C::OpaqueValue<'rewrite>],
        result_types: &[C::OpaqueAttr<'rewrite>],
        attributes: &[(impl ToString, C::OpaqueAttr<'rewrite>)],
        successors: &[C::OpaqueBlock<'rewrite>],
        regions: &[C::OpaqueRegion<'rewrite>],
    ) -> C::OpaqueOperation<'rewrite>;

    fn create_attribute(
        &self,
        attribute: AttributeID,
        parameters: &[C::OpaqueAttr<'rewrite>],
    ) -> C::OpaqueAttr<'rewrite>;

    fn create_block(
        &self,
        arguments: &[C::OpaqueAttr<'rewrite>],
        operations: &[C::OpaqueOperation<'rewrite>],
    ) -> C::OpaqueBlock<'rewrite>;

    fn create_region(&self, blocks: &[C::OpaqueBlock<'rewrite>]) -> C::OpaqueRegion<'rewrite>;

    fn set_attribute(
        &self,
        op: C::OpaqueOperation<'rewrite>,
        attr_name: &str,
        attr: C::OpaqueAttr<'rewrite>,
    );
    fn set_operand(
        &self,
        op: C::OpaqueOperation<'rewrite>,
        operand_pos: OperandPosition,
        operand: C::OpaqueValue<'rewrite>,
    );
    fn set_successor(
        &self,
        op: C::OpaqueOperation<'rewrite>,
        successor_pos: SuccessorPosition,
        successor: C::OpaqueBlock<'rewrite>,
    );
    fn set_region(
        &self,
        op: C::OpaqueOperation<'rewrite>,
        region_pos: RegionPosition,
        region: C::OpaqueRegion<'rewrite>,
    );
}

//============================================================================//
// IR Structure
//============================================================================//

pub trait Operation<'p, C: Context + ?Sized> {
    fn get_id(&self) -> OperationID;

    fn get_parent_op(&self) -> Option<C::Operation<'p>>;
    fn get_parent_region(&self) -> Option<C::Region<'p>>;
    fn get_parent_block(&self) -> Option<C::Block<'p>>;

    fn get_prev(&self) -> Option<C::Operation<'p>>;
    fn get_next(&self) -> Option<C::Operation<'p>>;

    fn opaque<'rewrite>(&self, accessor: &C::Accessor<'rewrite>) -> C::OpaqueOperation<'rewrite>;
}

pub trait Block<'p, C: Context + ?Sized> {}

pub trait Attribute<'p, C: Context + ?Sized> {
    fn get_id(&self) -> AttributeID;
}

pub trait Region<'p, C: Context + ?Sized> {}

pub trait Value<'p, C: Context + ?Sized> {}

pub enum ValueOwner<'p, C: Context> {
    BlockArgument(C::Block<'p>),
    Operation(C::Operation<'p>),
}

//============================================================================//
// Opaque IR Structure
//============================================================================//

pub trait OpaqueOperation<'rewrite, C: Context + ?Sized> {
    fn access<'a>(&self, accessor: &'a C::Accessor<'rewrite>) -> C::Operation<'a>;
}
