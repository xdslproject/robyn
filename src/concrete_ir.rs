use std::cell::RefCell;

use dumpster::{unsync::Gc, Collectable};

use crate::ir::{Attribute, Block, Context, Operation, Region, Value};

pub struct ConcreteContext {}

impl Context for ConcreteContext {
    type Operation = OperationRef;

    type Attribute = AttributeRef;

    type Block = BlockRef;

    type Region = RegionRef;

    type Value = ValueRef;
}

#[derive(Collectable)]
struct ConcreteOperation {
    previous: Option<OperationRef>,
    next: Option<OperationRef>,

    parent: Option<BlockRef>,
}

#[derive(Collectable, Clone)]
pub struct OperationRef(Gc<RefCell<ConcreteOperation>>);

impl Operation<ConcreteContext> for OperationRef {
    fn get_parent_op(&self) -> Option<OperationRef> {
        todo!()
    }

    fn get_parent_block(&self) -> Option<BlockRef> {
        todo!()
    }

    fn get_next() -> Option<OperationRef> {
        todo!()
    }
}

#[derive(Collectable)]
pub struct ConcreteAttribute;

#[derive(Collectable, Clone)]
pub struct AttributeRef(Gc<RefCell<ConcreteAttribute>>);

impl Attribute<ConcreteContext> for AttributeRef {}

#[derive(Collectable)]
pub struct ConcreteBlock {
    parent: RegionRef,
    first: OperationRef,
    last: OperationRef,

    arguments: Vec<ValueRef>,
}

#[derive(Collectable, Clone)]
pub struct BlockRef(Gc<RefCell<ConcreteBlock>>);

impl Block<ConcreteContext> for BlockRef {}

#[derive(Collectable)]
pub struct ConcreteRegion {
    parent: OperationRef,
    blocks: Vec<BlockRef>,
}

#[derive(Collectable, Clone)]
pub struct RegionRef(Gc<RefCell<ConcreteRegion>>);

impl Region<ConcreteContext> for RegionRef {}

#[derive(Collectable)]
pub struct ConcreteValue {
    owner: ConcreteValueOwner,
    r#type: AttributeRef,
}

#[derive(Collectable)]
pub enum ConcreteValueOwner {
    BlockArgument(BlockRef),
    Operation(OperationRef),
}

#[derive(Collectable, Clone)]
pub struct ValueRef(Gc<RefCell<ConcreteValue>>);

impl Value<ConcreteContext> for ValueRef {}
