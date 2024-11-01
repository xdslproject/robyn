#[cfg(feature = "gc")]
mod gc;
#[cfg(feature = "gc")]
pub use gc::GcContext;

use ariadne::{Report, ReportBuilder, ReportKind};

use std::ops::{Deref, Range};

use crate::{
    dialect::{AttributeKind, OperationKind},
    utils::bitbox::BitBox,
};

pub type OperandPosition = usize;
pub type ResultPosition = usize;
pub type SuccessorPosition = usize;
pub type RegionPosition = usize;
pub type BlockPosition = usize;

pub type Diagnostic = ReportBuilder<'static, Range<usize>>;

pub struct Span(usize);

pub trait Context: Sized {
    // IR
    type Operation<'rewrite, 'a>: Operation<'rewrite, 'a, Self>
    where
        Self: 'rewrite;
    type Attribute<'rewrite, 'a>: Attribute<'rewrite, 'a, Self>
    where
        Self: 'rewrite;
    type Block<'rewrite, 'a>: Block<'rewrite, 'a, Self>
    where
        Self: 'rewrite;
    type Region<'rewrite, 'a>: Region<'rewrite, 'a, Self>
    where
        Self: 'rewrite;
    type Value<'rewrite, 'a>: Value<'rewrite, 'a, Self>
    where
        Self: 'rewrite;

    type Program<'ctx>
    where
        Self: 'ctx;

    /// Handle to access the data storage of an attribute.
    type AttrData<'data, T>: AttrData<'data, T>
    where
        T: ?Sized + 'data;
    type DictionaryData<'rewrite, 'a>: DictionaryData<'rewrite, 'a, Self>;

    // Rewriting
    type Accessor<'rewrite>: Accessor<'rewrite, Self>
    where
        Self: 'rewrite;
    type Rewriter<'rewrite>: Rewriter<'rewrite, Self>
    where
        Self: 'rewrite;

    type OpaqueOperation<'rewrite>: OpaqueOperation<'rewrite, Self>
    where
        Self: 'rewrite;
    type OpaqueAttr<'rewrite>: OpaqueAttr<'rewrite, Self>
    where
        Self: 'rewrite;
    type OpaqueBlock<'rewrite>
    where
        Self: 'rewrite;
    type OpaqueRegion<'rewrite>
    where
        Self: 'rewrite;
    type OpaqueValue<'rewrite>
    where
        Self: 'rewrite;

    fn register_operation<O: OperationKind>(&mut self);
    fn register_attribute<A: AttributeKind>(&mut self);

    /// Creates a program containing an empty module operation.
    fn module_program<'ctx>(&'ctx self) -> Self::Program<'ctx>;

    /// Applies the provided pattern to the top level operation of the provided program.
    fn apply_pattern<'ctx, P: RewritePattern<Self>>(
        &'ctx self,
        program: &mut Self::Program<'ctx>,
        pattern: &P,
    ) -> PatternResult;

    /// Applies the provided pattern to the top level operation of the provided program.
    fn apply_single_use_pattern<'ctx, P: SingleUseRewritePattern<Self>>(
        &'ctx self,
        program: &mut Self::Program<'ctx>,
        pattern: P,
    ) -> PatternResult;
}

pub trait AttrData<'data, T: ?Sized>: Deref<Target = T> {
    /// Maps a handle to a value referencing attribute data to another value
    /// referencing attribute data.
    fn map_ref<F, U>(self, f: F) -> impl AttrData<'data, U>
    where
        F: FnOnce(&T) -> &U,
        U: 'data + ?Sized;
}

pub trait DictionaryData<'rewrite, 'a, C: Context> {
    fn get(&self, data: &str) -> Option<C::Attribute<'rewrite, 'a>>;
}

//============================================================================//
// Rewriting
//============================================================================//

pub trait RewritePattern<C: Context> {
    fn match_and_rewrite(&self, accessor: C::Accessor<'_>) -> PatternResult;
}

pub trait SingleUseRewritePattern<C: Context> {
    fn match_and_rewrite(self, accessor: C::Accessor<'_>) -> PatternResult;
}

impl<C: Context, P> SingleUseRewritePattern<C> for &P
where
    P: RewritePattern<C>,
{
    fn match_and_rewrite(self, accessor: <C as Context>::Accessor<'_>) -> PatternResult {
        self.match_and_rewrite(accessor)
    }
}

pub enum PatternResult {
    Success,
    Failure,
}

pub trait Accessor<'rewrite, C: Context> {
    fn get_root(&self) -> C::Operation<'rewrite, '_>;

    fn rewrite(self) -> C::Rewriter<'rewrite>;

    fn apply_pattern<P: RewritePattern<C>>(
        &mut self,
        pattern: &P,
        operation: C::OpaqueOperation<'rewrite>,
    ) -> PatternResult;
    fn apply_single_use_pattern<P: SingleUseRewritePattern<C>>(
        &mut self,
        pattern: P,
        operation: C::OpaqueOperation<'rewrite>,
    ) -> PatternResult;
    fn apply_pattern_dyn(
        &mut self,
        pattern: &dyn RewritePattern<C>,
        operation: C::OpaqueOperation<'rewrite>,
    ) -> PatternResult;
}

pub trait Rewriter<'rewrite, C: Context> {
    fn get_placeholder_value(&self, r#type: C::OpaqueAttr<'rewrite>) -> C::OpaqueValue<'rewrite>;

    fn get_string_attr(&self, data: &[u8]) -> C::OpaqueAttr<'rewrite>;

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
    fn create_op<O: OperationKind>(
        &self,
        operands: &[C::OpaqueValue<'rewrite>],
        result_types: &[C::OpaqueAttr<'rewrite>],
        attributes: &[(&str, C::OpaqueAttr<'rewrite>)],
        successors: &[C::OpaqueBlock<'rewrite>],
        regions: &[C::OpaqueRegion<'rewrite>],
    ) -> C::OpaqueOperation<'rewrite>;

    /// Creates an attribute.
    fn create_attribute<'data, A: AttributeKind>(
        &self,
        data: OpaqueAttributeData<'data, 'rewrite, C>,
    ) -> C::OpaqueAttr<'rewrite>;

    fn create_block(
        &self,
        arguments: &[C::OpaqueAttr<'rewrite>],
        operations: &[C::OpaqueOperation<'rewrite>],
    ) -> C::OpaqueBlock<'rewrite>;

    fn create_region(&self, blocks: &[C::OpaqueBlock<'rewrite>]) -> C::OpaqueRegion<'rewrite>;

    /// Inserts or replace an attribute in the operation's dictionary.
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

pub trait Operation<'rewrite, 'a, C: Context>: Clone {
    fn isa<K: OperationKind>(&self) -> bool;
    fn dyn_cast<K: OperationKind>(&self) -> Option<K::Access<'rewrite, 'a, C>>;

    fn get_parent_op(&self) -> Option<C::Operation<'rewrite, 'a>>;
    fn get_parent_region(&self) -> Option<C::Region<'rewrite, 'a>>;
    fn get_parent_block(&self) -> Option<C::Block<'rewrite, 'a>>;

    fn get_prev(&self) -> Option<C::Operation<'rewrite, 'a>>;
    fn get_next(&self) -> Option<C::Operation<'rewrite, 'a>>;

    fn get_num_regions(&self) -> RegionPosition;
    fn get_region(&self, region: RegionPosition) -> Option<C::Region<'rewrite, 'a>>;

    fn get_num_operands(&self) -> OperandPosition;
    fn get_operand(&self, operand: OperandPosition) -> Option<C::Value<'rewrite, 'a>>;

    fn get_num_results(&self) -> ResultPosition;
    fn get_result(&self, result: ResultPosition) -> Option<C::Value<'rewrite, 'a>>;

    fn get_num_successors(&self) -> SuccessorPosition;
    fn get_successor(&self, successor: RegionPosition) -> Option<C::Block<'rewrite, 'a>>;

    fn opaque(&self) -> C::OpaqueOperation<'rewrite>;
}

pub trait Block<'rewrite, 'a, C: 'rewrite + Context>: Clone {
    fn ops(&self) -> impl Iterator<Item = C::Operation<'rewrite, 'a>>;

    fn opaque(&self) -> C::OpaqueBlock<'rewrite>;
}

pub enum AttributeData<'data, 'rewrite, 'a, C: 'rewrite + Context>
where
    C::Attribute<'rewrite, 'a>: 'data,
    C::DictionaryData<'rewrite, 'a>: 'data,
{
    Bits(C::AttrData<'data, BitBox>),
    Array(C::AttrData<'data, [C::Attribute<'rewrite, 'a>]>),
    Dictionary(C::AttrData<'data, C::DictionaryData<'rewrite, 'a>>),
}

pub enum OpaqueAttributeData<'data, 'rewrite, C: 'rewrite + Context> {
    Bits(BitBox),
    Array(&'data [C::OpaqueAttr<'rewrite>]),
    Dictionary(&'data [(&'data str, C::OpaqueAttr<'rewrite>)]),
}

pub trait Attribute<'rewrite, 'a, C: 'rewrite + Context>: Clone {
    fn isa<K: AttributeKind>(&self) -> bool;
    fn dyn_cast<K: AttributeKind>(&self) -> Option<K::Access<'rewrite, 'a, C>>;

    fn data<'data>(&'data self) -> AttributeData<'data, 'rewrite, 'a, C>;

    fn opaque(&self) -> C::OpaqueAttr<'rewrite>;
}

pub trait Region<'rewrite, 'a, C: 'rewrite + Context>: Clone {
    fn get_block(&self, block: BlockPosition) -> Option<C::Block<'rewrite, 'a>>;

    fn opaque(&self) -> C::OpaqueRegion<'rewrite>;
}

pub trait Value<'rewrite, 'a, C: 'rewrite + Context>: Clone {
    fn opaque(&self) -> C::OpaqueValue<'rewrite>;
}

pub enum ValueOwner<'rewrite, 'a, C: 'rewrite + Context> {
    Placeholder,
    BlockArgument(C::Block<'rewrite, 'a>),
    Operation(C::Operation<'rewrite, 'a>),
}

//============================================================================//
// Opaque IR Structure
//============================================================================//

pub trait OpaqueOperation<'rewrite, C: Context>: Clone {
    fn access<'a>(&self, accessor: &'a C::Accessor<'rewrite>) -> C::Operation<'rewrite, 'a>;
}

pub trait OpaqueAttr<'rewrite, C: Context>: Clone {
    fn access<'a>(&self, accessor: &'a C::Accessor<'rewrite>) -> C::Attribute<'rewrite, 'a>;
}
