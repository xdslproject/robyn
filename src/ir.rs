#[cfg(feature = "gc")]
mod gc;
#[cfg(feature = "gc")]
pub use gc::GcContext;

use crate::{dialect::{AttributeKind, Dialect, OperationKind}, utils::ApInt};

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
    type OpaqueAttr<'rewrite>: OpaqueAttr<'rewrite, Self>;
    type OpaqueBlock<'rewrite>;
    type OpaqueRegion<'rewrite>;
    type OpaqueValue<'rewrite>;

    fn register_dialect<D: Dialect>(&mut self);
}

//============================================================================//
// Rewriting
//============================================================================//

pub trait RewritePattern<C: Context + ?Sized> {
    fn match_and_rewrite(&self, accessor: C::Accessor<'_>);
}

pub trait Accessor<'rewrite, C: Context + ?Sized> {
    fn get_root(&self) -> C::Operation<'_>;

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
    fn get_placeholder_value(&self, r#type: C::Attribute<'_>) -> C::Value<'_>;

    fn get_int_data_attr(&self, data: ApInt) -> C::Attribute<'_>;
    fn get_string_attr(&self, data: impl ToString) -> C::Attribute<'_>;

    /// Replaces an operation with the provided operation. 
    /// After this, the replaced operation can no longer be used.
    fn replace_op(&self, op: C::OpaqueOperation<'rewrite>, with: C::OpaqueOperation<'rewrite>);

    /// Erases an operation. 
    /// After this, the erased operation can no longer be used.
    fn erase_op(&self, op: C::OpaqueOperation<'rewrite>);

    /// Inserts an operation in the same block of an other operation,
    /// before it. The other operation must already be inserted in a block.
    fn insert_op_before(
        &self,
        op: C::OpaqueOperation<'rewrite>,
        other: C::OpaqueOperation<'rewrite>,
    );

    /// Inserts an operation in the same block of an other operation,
    /// after it. The other operation must already be inserted in a block.
    fn insert_op_after(
        &self,
        op: C::OpaqueOperation<'rewrite>,
        other: C::OpaqueOperation<'rewrite>,
    );

    /// Inserts an operation at the start of a block.
    fn insert_op_at_start(
        &self,
        op: C::OpaqueOperation<'rewrite>,
        at_start_of: C::OpaqueBlock<'rewrite>,
    );

    /// Inserts an operation at the end of a block.
    fn insert_op_at_end(
        &self,
        op: C::OpaqueOperation<'rewrite>,
        at_end_of: C::OpaqueBlock<'rewrite>,
    );

    /// Creates a free standing operation.
    fn create_op(
        &self,
        operation: OperationID,
        operands: &[C::OpaqueValue<'rewrite>],
        result_types: &[C::OpaqueAttr<'rewrite>],
        attributes: &[(impl ToString, C::OpaqueAttr<'rewrite>)],
        successors: &[C::OpaqueBlock<'rewrite>],
        regions: &[C::OpaqueRegion<'rewrite>],
    ) -> C::OpaqueOperation<'rewrite>;

    /// Creates a parametrized attribute.
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

    /// Inserts or replace an attribute in the operation's dictionnary.
    fn set_attribute(
        &self,
        op: C::OpaqueOperation<'rewrite>,
        attr_name: &str,
        attr: C::OpaqueAttr<'rewrite>,
    );
    
    /// Sets an existing operand of an operation to be a specific value.
    fn set_operand(
        &self,
        op: C::OpaqueOperation<'rewrite>,
        operand_pos: OperandPosition,
        operand: C::OpaqueValue<'rewrite>,
    );

    /// Sets an existing successor of an operation to be a specific block.
    fn set_successor(
        &self,
        op: C::OpaqueOperation<'rewrite>,
        successor_pos: SuccessorPosition,
        successor: C::OpaqueBlock<'rewrite>,
    );

    /// Sets an existing region of an operation to be a specific region.
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

pub trait Operation<'a, C: Context + ?Sized> {
    fn get_id(&self) -> OperationID;
    fn dyn_cast<K: OperationKind<C>>(&self) -> Option<K::Access<'a>>;

    fn get_parent_op(&self) -> Option<C::Operation<'a>>;
    fn get_parent_region(&self) -> Option<C::Region<'a>>;
    fn get_parent_block(&self) -> Option<C::Block<'a>>;

    fn get_prev(&self) -> Option<C::Operation<'a>>;
    fn get_next(&self) -> Option<C::Operation<'a>>;

    fn opaque<'rewrite>(&self, accessor: &C::Accessor<'rewrite>) -> C::OpaqueOperation<'rewrite>;
}

pub trait Block<'a, C: Context + ?Sized> {}

pub trait Attribute<'a, C: Context + ?Sized> {
    fn dyn_cast<K: AttributeKind<C>>(&self) -> Option<K::Access<'a>>;
}

pub trait Region<'a, C: Context + ?Sized> {}

pub trait Value<'a, C: Context + ?Sized> {}

pub enum ValueOwner<'a, C: Context> {
    Placeholder,
    BlockArgument(C::Block<'a>),
    Operation(C::Operation<'a>),
}

//============================================================================//
// Opaque IR Structure
//============================================================================//

pub trait OpaqueOperation<'rewrite, C: Context + ?Sized> {
    fn access<'a>(&self, accessor: &'a C::Accessor<'rewrite>) -> C::Operation<'a>;
    fn copy(&self) -> Self;
}

pub trait OpaqueAttr<'rewrite, C: Context + ?Sized> {
    fn access<'a>(&self, accessor: &'a C::Accessor<'rewrite>) -> C::Attribute<'a>;
    fn copy(&self) -> Self;
}
