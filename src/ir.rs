use crate::concrete_ir::ConcreteContext;

pub type OperationID = u32;
pub type AttributeID = u32;

pub type OperandPosition = u32;
pub type SuccessorPosition = u32;
pub type RegionPosition = u32;

pub trait Context {
    // IR
    type Operation<'p>: Operation<'p, Self>;
    type Attribute<'p>: Attribute<'p, Self>;
    type Block<'p>: Block<'p, Self>;
    type Region<'p>: Region<'p, Self>;
    type Value<'p>: Value<'p, Self>;

    // Rewriting
    type Accessor<'rewrite>: Accessor<'rewrite, Self>;
    type Rewriter<'rewrite>: Rewriter<'rewrite, Self>;
    type OpaqueOperation<'rewrite>;
    type OpaqueAttr<'rewrite>;
    type OpaqueBlock<'rewrite>;
    type OpaqueRegion<'rewrite>;
    type OpaqueValue<'rewrite>;

    fn apply_pattern<P: RewritePattern<Self>>(&mut self) -> P::Result;
}

//============================================================================//
// Rewriting
//============================================================================//

pub trait RewritePattern<C: Context + ?Sized> {
    type Result;

    fn match_and_rewrite(accessor: C::Accessor<'_>) -> Self::Result;
}

pub trait Accessor<'rewrite, C: Context + ?Sized> {
    fn get_op(&self) -> C::Operation<'_>;

    fn get_placeholder_value(&self, r#type: C::Attribute<'_>) -> C::Value<'_>;

    fn get_integer_type(&self, width: u32) -> C::Attribute<'_>;
    fn get_string_type(&self) -> C::Attribute<'_>;

    fn create_op<'a>(
        &'a self,
        operation: OperationID,
        operands: &[C::Value<'a>],
        attributes: &[(&str, C::Attribute<'a>)],
        result_types: &[C::Attribute<'a>],
        successors: &[C::Block<'a>],
        regions: &[C::Region<'a>],
    ) -> C::Operation<'a>;

    fn create_attribute<'a>(
        &'a self,
        attribute: AttributeID,
        parameters: &[C::Attribute<'_>],
    ) -> C::Attribute<'a>;

    fn create_block<'a>(&'a self, operations: &[C::Operation<'a>]) -> C::Block<'a>;

    fn create_region<'a>(&'a self, blocks: &[C::Block<'_>]) -> C::Region<'a>;

    fn rewrite(self) -> C::Rewriter<'rewrite>;
}

pub trait Rewriter<'rewrite, C: Context + ?Sized> {
    fn replace_op(&self, op: C::OpaqueOperation<'rewrite>, with: C::OpaqueOperation<'rewrite>);
    fn erase_op(&self, op: C::OpaqueOperation<'rewrite>);
    fn insert_op_before(
        &self,
        op: C::OpaqueOperation<'rewrite>,
        before: C::OpaqueOperation<'rewrite>,
    );
    fn insert_op_after(
        &self,
        op: C::OpaqueOperation<'rewrite>,
        after: C::OpaqueOperation<'rewrite>,
    );

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

pub trait Operation<'p, C: Context + ?Sized = ConcreteContext>: Clone {
    fn get_id(&self) -> OperationID;

    fn get_parent_op(&self) -> Option<C::Operation<'p>>;
    fn get_parent_region(&self) -> Option<C::Region<'p>>;
    fn get_parent_block(&self) -> Option<C::Block<'p>>;

    fn get_prev(&self) -> Option<C::Operation<'p>>;
    fn get_next(&self) -> Option<C::Operation<'p>>;
}

pub trait Block<'p, C: Context + ?Sized = ConcreteContext> {}

pub trait Attribute<'p, C: Context + ?Sized = ConcreteContext> {
    fn get_id(&self) -> AttributeID;
}

pub trait Region<'p, C: Context + ?Sized = ConcreteContext> {}

pub trait Value<'p, C: Context + ?Sized = ConcreteContext> {}

pub enum ValueOwner<'p, C: Context = ConcreteContext> {
    BlockArgument(C::Block<'p>),
    Operation(C::Operation<'p>),
}
