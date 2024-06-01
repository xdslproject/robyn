use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
};

use dumpster::{unsync::Gc, Collectable};
use itertools::Itertools;

use crate::{
    dialect::{AttributeKind, Dialect, OperationKind},
    ir::{
        Accessor, Attribute, Block, Context, OpaqueOperation, Operation, OperationID, Region, Value,
    },
    utils::ApInt,
};

use super::{AttributeID, OpaqueAttr, OperandPosition, Rewriter, ValueOwner};

// TODO: proper error handling instead of assert

#[derive(Collectable)]
pub struct GcContext {}

#[derive(Collectable, Clone)]
pub struct GcContextRef(Gc<RefCell<GcContextRef>>);

impl Context for GcContextRef {
    type Operation<'a> = GcOperationRef;
    type Attribute<'a> = GcAttributeRef;
    type Block<'a> = GcBlockRef;
    type Region<'a> = GcRegionRef;
    type Value<'a> = GcValueRef;

    type Accessor<'rewrite> = GcAccessor;
    type Rewriter<'rewrite> = GcRewriter;

    type OpaqueOperation<'rewrite> = GcOperationRef;
    type OpaqueAttr<'rewrite> = GcAttributeRef;
    type OpaqueBlock<'rewrite> = GcBlockRef;
    type OpaqueRegion<'rewrite> = GcRegionRef;
    type OpaqueValue<'rewrite> = GcValueRef;
    
    fn register_dialect<D: Dialect>(&mut self) {
        todo!()
    }
}

//============================================================================//
// Utils
//============================================================================//

fn link_ops(op1: &GcOperationRef, op2: &GcOperationRef) {
    if let Some(other) = &op1.get().next {
        other.get_mut().previous = None;
    }
    if let Some(other) = &op2.get().previous {
        other.get_mut().next = None;
    }
    op1.get_mut().next = Some(op2.clone());
    op2.get_mut().previous = Some(op1.clone());
}

//============================================================================//
// Rewriting
//============================================================================//

pub struct GcAccessor {
    ctx: GcContextRef,
    root: GcOperationRef,
}

impl<'rewrite> Accessor<'rewrite, GcContextRef> for GcAccessor {
    fn get_root(&self) -> GcOperationRef {
        self.root.clone()
    }

    fn rewrite(self) -> GcRewriter {
        GcRewriter { ctx: self.ctx }
    }

    fn apply_pattern<P: crate::ir::RewritePattern<GcContextRef>>(
        &mut self,
        pattern: &P,
        operation: GcOperationRef,
    ) {
        pattern.match_and_rewrite(Self {
            ctx: self.ctx.clone(),
            root: operation,
        })
    }

    fn apply_pattern_dyn(
        &mut self,
        pattern: &dyn crate::ir::RewritePattern<GcContextRef>,
        operation: GcOperationRef,
    ) {
        pattern.match_and_rewrite(Self {
            ctx: self.ctx.clone(),
            root: operation,
        })
    }
}

pub struct GcRewriter {
    ctx: GcContextRef,
}

impl<'rewrite> Rewriter<'rewrite, GcContextRef> for GcRewriter {
    fn get_placeholder_value(&self, r#type: GcAttributeRef) -> GcValueRef {
        GcValueRef::new(GcValue {
            valid: true,
            r#type: r#type,
            owner: GcValueOwner::Placeholder,
            uses: HashSet::new(),
        })
    }

    fn get_int_data_attr(&self, data: ApInt) -> GcAttributeRef {
        GcAttributeRef::new(GcAttribute {
            ctx: self.ctx.clone(),
            data: GcAttributeData::IntData(data),
        })
    }

    fn get_string_attr(&self, data: impl ToString) -> GcAttributeRef {
        GcAttributeRef::new(GcAttribute {
            ctx: self.ctx.clone(),
            data: GcAttributeData::StringAttr(data.to_string()),
        })
    }

    fn replace_op(&self, op: GcOperationRef, with: GcOperationRef) {
        assert!(op.valid());
        assert!(with.valid());
        assert!(op.get().parent.is_some());
        if let Some(previous) = &op.get().previous {
            link_ops(previous, &with);
        }
        if let Some(next) = &op.get().next {
            link_ops(&with, next);
        }
        with.get_mut().parent = op.get().parent.clone();
        op.invalidate();
    }

    fn erase_op(&self, op: GcOperationRef) {
        assert!(op.valid());
        if let Some(previous) = &op.get().previous {
            previous.get_mut().next = op.get().next.clone();
        }
        if let Some(next) = &op.get().next {
            next.get_mut().previous = op.get().previous.clone();
        }
        op.invalidate();
    }

    fn insert_op_before(&self, op: GcOperationRef, other: GcOperationRef) {
        assert!(op.valid());
        assert!(other.valid());
        assert!(op.get().parent.is_none());
        assert!(other.get().parent.is_some());
        if let Some(previous) = &other.get().previous {
            link_ops(previous, &op);
        }
        link_ops(&op, &other);
        op.get_mut().parent = other.get().parent.clone();
    }

    fn insert_op_after(&self, op: GcOperationRef, other: GcOperationRef) {
        assert!(op.valid());
        assert!(other.valid());
        assert!(op.get().parent.is_none());
        assert!(other.get().parent.is_some());
        if let Some(next) = &other.get().next {
            link_ops(&op, next);
        }
        link_ops(&other, &op);
        op.get_mut().parent = other.get().parent.clone();
    }

    fn insert_op_at_start(&self, op: GcOperationRef, at_start_of: GcBlockRef) {
        assert!(op.valid());
        assert!(at_start_of.valid());
        if let Some(first) = &at_start_of.get().first {
            link_ops(&op, &first);
        }
        op.get_mut().parent = Some(at_start_of.clone());
        at_start_of.get_mut().first = Some(op);
    }

    fn insert_op_at_end(&self, op: GcOperationRef, at_end_of: GcBlockRef) {
        assert!(op.valid());
        assert!(at_end_of.valid());
        if let Some(last) = &at_end_of.get().last {
            link_ops(&last, &op);
        }
        op.get_mut().parent = Some(at_end_of.clone());
        at_end_of.get_mut().last = Some(op);
    }

    fn create_op(
        &self,
        operation: OperationID,
        operands: &[GcValueRef],
        result_types: &[GcAttributeRef],
        attributes: &[(impl ToString, GcAttributeRef)],
        successors: &[GcBlockRef],
        regions: &[GcRegionRef],
    ) -> GcOperationRef {
        assert!(operands.iter().all(|x| x.valid()));
        assert!(successors.iter().all(|x| x.valid()));
        assert!(regions.iter().all(|x| x.valid()));

        let op = GcOperationRef::new(GcOperation {
            id: operation,
            ctx: self.ctx.clone(),
            valid: true,
            previous: None,
            next: None,
            parent: None,
            operands: operands.into(),
            results: result_types
                .iter()
                .map(|x| {
                    GcValueRef::new(GcValue {
                        valid: true,
                        owner: GcValueOwner::Placeholder,
                        r#type: x.clone(),
                        uses: HashSet::new(),
                    })
                })
                .collect(),
            attributes: attributes
                .iter()
                .map(|(k, v)| (k.to_string(), v.clone()))
                .collect(),
            successors: successors.into(),
            regions: regions.into(),
        });

        op.get_mut()
            .results
            .iter_mut()
            .for_each(|x| x.get_mut().owner = GcValueOwner::Operation(op.clone()));

        op
    }

    fn create_attribute<'a>(
        &'a self,
        attribute: crate::ir::AttributeID,
        parameters: &[GcAttributeRef],
    ) -> GcAttributeRef {
        GcAttributeRef::new(GcAttribute {
            ctx: self.ctx.clone(),
            data: GcAttributeData::Parameterized(attribute, parameters.into()),
        })
    }

    fn create_block<'a>(
        &'a self,
        arguments: &[GcAttributeRef],
        operations: &[GcOperationRef],
    ) -> GcBlockRef {
        assert!(operations.iter().all(|x| x.valid()));

        if let Some(first) = operations.first() {
            first.get_mut().previous = None;
        }

        if let Some(last) = operations.last() {
            last.get_mut().next = None;
        }

        operations
            .iter()
            .tuple_windows()
            .for_each(|(x, y)| link_ops(x, y));

        let block = GcBlockRef::new(GcBlock {
            valid: true,
            parent: None,
            first: operations.first().cloned(),
            last: operations.last().cloned(),
            arguments: arguments
                .iter()
                .map(|x| {
                    GcValueRef::new(GcValue {
                        valid: true,
                        owner: GcValueOwner::Placeholder,
                        r#type: x.clone(),
                        uses: HashSet::new(),
                    })
                })
                .collect(),
        });

        block
            .get_mut()
            .arguments
            .iter_mut()
            .for_each(|v| v.get_mut().owner = GcValueOwner::BlockArgument(block.clone()));

        block
    }

    fn create_region<'a>(&'a self, blocks: &[GcBlockRef]) -> GcRegionRef {
        assert!(blocks.iter().all(|b| b.valid()));
        GcRegionRef::new(GcRegion {
            valid: true,
            parent: None,
            blocks: blocks.into(),
        })
    }

    fn set_attribute(&self, op: GcOperationRef, attr_name: &str, attr: GcAttributeRef) {
        let attributes = &mut op.get_mut().attributes;
        if let Some(value) = attributes.get_mut(attr_name) {
            *value = attr;
        } else {
            attributes.insert(attr_name.into(), attr);
        }
    }

    fn set_operand(
        &self,
        op: GcOperationRef,
        operand_pos: super::OperandPosition,
        operand: GcValueRef,
    ) {
        assert!(op.get().operands.len() > operand_pos as usize);
        op.get_mut().operands[operand_pos as usize] = operand;
    }

    fn set_successor(
        &self,
        op: GcOperationRef,
        successor_pos: super::SuccessorPosition,
        successor: GcBlockRef,
    ) {
        assert!(op.get().successors.len() > successor_pos as usize);
        op.get_mut().successors[successor_pos as usize] = successor;
    }

    fn set_region(
        &self,
        op: GcOperationRef,
        region_pos: super::RegionPosition,
        region: GcRegionRef,
    ) {
        assert!(op.get().regions.len() > region_pos as usize);
        op.get_mut().regions[region_pos as usize] = region;
    }
}

//============================================================================//
// IR Structure
//============================================================================//

#[derive(Collectable)]
pub struct GcOperation {
    ctx: GcContextRef,
    id: OperationID,

    valid: bool,

    previous: Option<GcOperationRef>,
    next: Option<GcOperationRef>,

    parent: Option<GcBlockRef>,

    operands: Vec<GcValueRef>,
    results: Vec<GcValueRef>,
    attributes: HashMap<String, GcAttributeRef>,
    successors: Vec<GcBlockRef>,
    regions: Vec<GcRegionRef>,
}

impl GcOperation {
    fn invalidate(&mut self) {
        self.valid = false;
        self.regions.iter().for_each(|r| r.invalidate());
        self.results.iter().for_each(|v| v.invalidate());
    }
}

#[derive(Collectable, Clone)]
pub struct GcOperationRef(Gc<RefCell<GcOperation>>);

impl GcOperationRef {
    fn new(val: GcOperation) -> Self {
        Self(Gc::new(RefCell::new(val)))
    }

    fn get(&self) -> std::cell::Ref<'_, GcOperation> {
        self.0.borrow()
    }

    fn get_mut(&self) -> std::cell::RefMut<'_, GcOperation> {
        self.0.borrow_mut()
    }

    fn valid(&self) -> bool {
        self.get().valid
    }

    fn invalidate(&self) {
        self.get_mut().invalidate();
    }
}

impl<'a> Operation<'a, GcContextRef> for GcOperationRef {
    fn get_id(&self) -> OperationID {
        self.get().id
    }

    fn dyn_cast<K: OperationKind<GcContextRef>>(&self) -> Option<K::Access<'a>> {
        K::access(&self.get().ctx, self.clone())
    }

    fn get_parent_op(&self) -> Option<GcOperationRef> {
        self.get()
            .parent
            .as_ref()?
            .get()
            .parent
            .as_ref()?
            .get()
            .parent
            .clone()
    }

    fn get_parent_region(&self) -> Option<GcRegionRef> {
        self.get().parent.as_ref()?.get().parent.clone()
    }

    fn get_parent_block(&self) -> Option<GcBlockRef> {
        self.get().parent.clone()
    }

    fn get_prev(&self) -> Option<GcOperationRef> {
        self.get().previous.clone()
    }

    fn get_next(&self) -> Option<GcOperationRef> {
        self.get().next.clone()
    }

    fn opaque<'rewrite>(
        &self,
        _: &<GcContextRef as Context>::Accessor<'rewrite>,
    ) -> <GcContextRef as Context>::OpaqueOperation<'rewrite> {
        self.clone()
    }
}

impl<'rewrite> OpaqueOperation<'rewrite, GcContextRef> for GcOperationRef {
    fn access<'a>(
        &self,
        _: &'a <GcContextRef as Context>::Accessor<'rewrite>,
    ) -> <GcContextRef as Context>::Operation<'a> {
        self.clone()
    }

    fn copy(&self) -> Self {
        self.clone()
    }
}

#[derive(Collectable)]
pub struct GcAttribute {
    ctx: GcContextRef,
    data: GcAttributeData,
}

#[derive(Collectable)]
pub enum GcAttributeData {
    IntData(ApInt),
    StringAttr(String),
    ArrayAttr(Vec<GcAttributeRef>),
    DictionaryAttr(HashMap<String, GcAttributeRef>),
    Parameterized(AttributeID, Vec<GcAttributeRef>),
}

#[derive(Collectable, Clone)]
pub struct GcAttributeRef(Gc<RefCell<GcAttribute>>);

impl GcAttributeRef {
    fn new(val: GcAttribute) -> Self {
        Self(Gc::new(RefCell::new(val)))
    }

    fn get(&self) -> std::cell::Ref<'_, GcAttribute> {
        self.0.borrow()
    }

    fn get_mut(&self) -> std::cell::RefMut<'_, GcAttribute> {
        self.0.borrow_mut()
    }
}

impl<'a> Attribute<'a, GcContextRef> for GcAttributeRef {
    fn dyn_cast<K: AttributeKind<GcContextRef>>(&self) -> Option<K::Access<'a>> {
        K::access(&self.get().ctx, self.clone())
    }
}

impl<'rewrite> OpaqueAttr<'rewrite, GcContextRef> for GcAttributeRef {
    fn access<'a>(
        &self,
        _: &'a <GcContextRef as Context>::Accessor<'rewrite>,
    ) -> <GcContextRef as Context>::Attribute<'a> {
        self.clone()
    }

    fn copy(&self) -> Self {
        self.clone()
    }
}

#[derive(Collectable)]
pub struct GcBlock {
    valid: bool,

    parent: Option<GcRegionRef>,
    first: Option<GcOperationRef>,
    last: Option<GcOperationRef>,

    arguments: Vec<GcValueRef>,
}

impl GcBlock {
    fn ops(&self) -> impl Iterator<Item = GcOperationRef> {
        struct BlockIterator {
            next: Option<GcOperationRef>,
        }

        impl Iterator for BlockIterator {
            type Item = GcOperationRef;

            fn next(&mut self) -> Option<Self::Item> {
                let next = self.next.as_ref().and_then(|x| x.get().next.clone());
                std::mem::replace(&mut self.next, next)
            }
        }

        BlockIterator {
            next: self.first.clone(),
        }
    }

    fn invalidate(&mut self) {
        self.valid = false;
        self.ops().for_each(|op| op.invalidate());
        self.arguments.iter().for_each(|v| v.invalidate());
    }
}

#[derive(Collectable, Clone)]
pub struct GcBlockRef(Gc<RefCell<GcBlock>>);

impl GcBlockRef {
    fn new(val: GcBlock) -> Self {
        Self(Gc::new(RefCell::new(val)))
    }

    fn get(&self) -> std::cell::Ref<'_, GcBlock> {
        self.0.borrow()
    }

    fn get_mut(&self) -> std::cell::RefMut<'_, GcBlock> {
        self.0.borrow_mut()
    }

    fn valid(&self) -> bool {
        self.get().valid
    }

    fn invalidate(&self) {
        self.get_mut().invalidate();
    }
}

impl<'a> Block<'a, GcContextRef> for GcBlockRef {}

#[derive(Collectable)]
pub struct GcRegion {
    valid: bool,
    parent: Option<GcOperationRef>,
    blocks: Vec<GcBlockRef>,
}

impl GcRegion {
    fn invalidate(&mut self) {
        self.valid = false;
        self.blocks.iter().for_each(|b| b.invalidate());
    }
}

#[derive(Collectable, Clone)]
pub struct GcRegionRef(Gc<RefCell<GcRegion>>);

impl GcRegionRef {
    fn new(val: GcRegion) -> Self {
        Self(Gc::new(RefCell::new(val)))
    }

    fn get(&self) -> std::cell::Ref<'_, GcRegion> {
        self.0.borrow()
    }

    fn get_mut(&self) -> std::cell::RefMut<'_, GcRegion> {
        self.0.borrow_mut()
    }

    fn valid(&self) -> bool {
        self.get().valid
    }

    fn invalidate(&self) {
        self.get_mut().invalidate();
    }
}

impl<'a> Region<'a, GcContextRef> for GcRegionRef {}

#[derive(Collectable)]
pub struct GcValue {
    valid: bool,

    owner: GcValueOwner,
    r#type: GcAttributeRef,

    uses: HashSet<(GcAttributeRef, OperandPosition)>,
}

#[derive(Collectable)]
pub enum GcValueOwner {
    Placeholder,
    BlockArgument(GcBlockRef),
    Operation(GcOperationRef),
}

impl<'a> From<ValueOwner<'a, GcContextRef>> for GcValueOwner {
    fn from(value: ValueOwner<'a, GcContextRef>) -> Self {
        match value {
            ValueOwner::Placeholder => Self::Placeholder,
            ValueOwner::BlockArgument(x) => Self::BlockArgument(x),
            ValueOwner::Operation(x) => Self::Operation(x),
        }
    }
}

impl<'a> From<GcValueOwner> for ValueOwner<'a, GcContextRef> {
    fn from(value: GcValueOwner) -> Self {
        match value {
            GcValueOwner::Placeholder => Self::Placeholder,
            GcValueOwner::BlockArgument(x) => Self::BlockArgument(x),
            GcValueOwner::Operation(x) => Self::Operation(x),
        }
    }
}

impl GcValue {
    fn invalidate(&mut self) {
        self.valid = false;
    }
}

#[derive(Collectable, Clone)]
pub struct GcValueRef(Gc<RefCell<GcValue>>);

impl GcValueRef {
    fn new(val: GcValue) -> Self {
        Self(Gc::new(RefCell::new(val)))
    }

    fn get(&self) -> std::cell::Ref<'_, GcValue> {
        self.0.borrow()
    }

    fn get_mut(&self) -> std::cell::RefMut<'_, GcValue> {
        self.0.borrow_mut()
    }

    fn valid(&self) -> bool {
        self.get().valid
    }

    fn invalidate(&self) {
        self.get_mut().invalidate();
    }
}

impl<'a> Value<'a, GcContextRef> for GcValueRef {}
