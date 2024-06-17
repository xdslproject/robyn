use std::{
    any::TypeId,
    borrow::Borrow,
    cell::{Ref, RefCell},
    collections::{HashMap, HashSet},
    ops::Deref,
};

use dumpster::{unsync::Gc, Collectable};

use crate::{
    dialect::{
        builtin::{BuiltinDialect, ModuleOp, StringAttr},
        AttributeKind, Dialect, OperationKind,
    },
    ir::{
        Accessor, Attribute, Block, Context, OpaqueAttr, OpaqueOperation, OperandPosition,
        Operation, Region, RewritePattern, Rewriter, Value, ValueOwner,
    },
    utils::bitbox::BitBox,
};

use super::{AttrData, AttributeData, BlockPosition, DictionaryData, OpaqueAttributeData};

// TODO: proper error handling instead of assert

#[derive(Collectable, Debug)]
struct GcContextInner {
    op_kinds: HashMap<TypeId, Gc<GcOpKindVtable>>,
    attr_kinds: HashMap<TypeId, Gc<GcAttrKindVtable>>,
}

#[derive(Collectable, Clone)]
pub struct GcContext(Gc<RefCell<GcContextInner>>);

impl GcContext {
    pub fn new() -> GcContext {
        let mut ctx = GcContext(Gc::new(RefCell::new(GcContextInner {
            op_kinds: HashMap::new(),
            attr_kinds: HashMap::new(),
        })));

        BuiltinDialect::register(&mut ctx);

        ctx
    }

    fn get(&self) -> std::cell::Ref<'_, GcContextInner> {
        (*self.0).borrow()
    }

    fn get_mut(&self) -> std::cell::RefMut<'_, GcContextInner> {
        self.0.borrow_mut()
    }
}

impl Context for GcContext {
    type Operation<'rewrite, 'a> = GcOperationRef;
    type Attribute<'rewrite, 'a> = GcAttributeRef;
    type Block<'rewrite, 'a> = GcBlockRef;
    type Region<'rewrite, 'a> = GcRegionRef;
    type Value<'rewrite, 'a> = GcValueRef;

    type Accessor<'rewrite> = GcAccessor;
    type Rewriter<'rewrite> = GcRewriter;

    type Program<'ctx> = GcProgram;

    type AttrData<'data, T> = GcAttrData<'data, T> where T: ?Sized + 'data;
    type DictionaryData<'rewrite, 'a> = HashMap<String, GcAttributeRef>;

    type OpaqueOperation<'rewrite> = GcOperationRef;
    type OpaqueAttr<'rewrite> = GcAttributeRef;
    type OpaqueBlock<'rewrite> = GcBlockRef;
    type OpaqueRegion<'rewrite> = GcRegionRef;
    type OpaqueValue<'rewrite> = GcValueRef;

    fn register_operation<O: OperationKind>(&mut self) {
        self.get_mut().op_kinds.insert(
            TypeId::of::<O>(),
            Gc::new(GcOpKindVtable {
                operation: TypeId::of::<O>(),
            }),
        );
    }

    fn register_attribute<A: AttributeKind>(&mut self) {
        self.get_mut().attr_kinds.insert(
            TypeId::of::<A>(),
            Gc::new(GcAttrKindVtable {
                attr: TypeId::of::<A>(),
            }),
        );
    }

    fn module_program<'ctx>(&'ctx self) -> GcProgram {
        GcProgram {
            op: ModuleOp::create::<GcContext>(&GcRewriter { ctx: self.clone() }),
        }
    }

    fn apply_pattern<'ctx, P: RewritePattern<Self>>(
        &'ctx self,
        program: &mut Self::Program<'ctx>,
        pattern: &P,
    ) {
        pattern.match_and_rewrite(GcAccessor {
            ctx: self.clone(),
            root: program.op.clone(),
        })
    }
}

pub struct GcAttrData<'data, T: ?Sized + 'data>(Ref<'data, T>);

impl<'data, T: ?Sized> Deref for GcAttrData<'data, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0.borrow()
    }
}

impl<'data, T: ?Sized> AttrData<'data, T> for GcAttrData<'data, T> {
    fn map_ref<F, U>(self, f: F) -> impl AttrData<'data, U>
    where
        F: FnOnce(&T) -> &U,
        U: 'data + ?Sized,
    {
        GcAttrData(Ref::map::<U, F>(self.0, f))
    }
}

impl<'rewrite, 'a> DictionaryData<'rewrite, 'a, GcContext> for HashMap<String, GcAttributeRef> {
    fn get(&self, data: &str) -> Option<GcAttributeRef> {
        self.get(data).cloned()
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
// Program
//============================================================================//

pub struct GcProgram {
    op: GcOperationRef,
}

//============================================================================//
// Rewriting
//============================================================================//

pub struct GcAccessor {
    ctx: GcContext,
    root: GcOperationRef,
}

impl<'rewrite> Accessor<'rewrite, GcContext> for GcAccessor {
    fn get_root(&self) -> GcOperationRef {
        self.root.clone()
    }

    fn rewrite(self) -> GcRewriter {
        GcRewriter { ctx: self.ctx }
    }

    fn apply_pattern<P: RewritePattern<GcContext>>(
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
        pattern: &dyn RewritePattern<GcContext>,
        operation: GcOperationRef,
    ) {
        pattern.match_and_rewrite(Self {
            ctx: self.ctx.clone(),
            root: operation,
        })
    }
}

pub struct GcRewriter {
    ctx: GcContext,
}

impl<'rewrite> Rewriter<'rewrite, GcContext> for GcRewriter {
    fn get_placeholder_value(&self, r#type: GcAttributeRef) -> GcValueRef {
        GcValueRef::new(GcValue {
            valid: true,
            r#type: r#type,
            owner: ValueOwner::Placeholder,
            uses: HashSet::new(),
        })
    }

    fn get_string_attr(&self, data: &[u8]) -> GcAttributeRef {
        self.create_attribute::<StringAttr>(OpaqueAttributeData::Bits(
            BitBox::from_bytes(data).expect("TODO"),
        ))
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

    fn create_op<O: OperationKind>(
        &self,
        operands: &[GcValueRef],
        result_types: &[GcAttributeRef],
        attributes: &[(&str, GcAttributeRef)],
        successors: &[GcBlockRef],
        regions: &[GcRegionRef],
    ) -> GcOperationRef {
        assert!(operands.iter().all(|x| x.valid()));
        assert!(successors.iter().all(|x| x.valid()));
        assert!(regions.iter().all(|x| x.valid()));
        assert!(self.ctx.get().op_kinds.contains_key(&TypeId::of::<O>()));

        let op = GcOperationRef::new(GcOperation {
            kind: self
                .ctx
                .get()
                .op_kinds
                .get(&TypeId::of::<O>())
                .expect("already checked")
                .clone(),
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
                        owner: ValueOwner::Placeholder,
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
            .for_each(|x| x.get_mut().owner = ValueOwner::Operation(op.clone()));

        op
    }

    fn create_attribute<'data, A: AttributeKind>(
        &self,
        data: OpaqueAttributeData<'data, 'rewrite, GcContext>,
    ) -> GcAttributeRef {
        assert!(self.ctx.get().attr_kinds.contains_key(&TypeId::of::<A>()));
        GcAttributeRef::new(GcAttribute {
            kind: self
                .ctx
                .get()
                .attr_kinds
                .get(&TypeId::of::<A>())
                .expect("already checked")
                .clone(),
            data: match data {
                OpaqueAttributeData::Bits(bits) => GcAttributeData::Bits(bits),
                OpaqueAttributeData::Array(array) => GcAttributeData::Array(array.to_owned()),
                OpaqueAttributeData::Dictionary(dict) => GcAttributeData::Dictionary(
                    dict.iter()
                        .map(|(k, v)| (k.to_string(), v.clone()))
                        .collect(),
                ),
            },
        })
    }

    fn create_block(
        &self,
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

        {
            use itertools::Itertools;
            operations
                .iter()
                .tuple_windows()
                .for_each(|(x, y)| link_ops(x, y));
        }

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
                        owner: ValueOwner::Placeholder,
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
            .for_each(|v| v.get_mut().owner = ValueOwner::BlockArgument(block.clone()));

        block
    }

    fn create_region(&self, blocks: &[GcBlockRef]) -> GcRegionRef {
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
    kind: Gc<GcOpKindVtable>,

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
        (*self.0).borrow()
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

impl<'rewrite, 'a> Operation<'rewrite, 'a, GcContext> for GcOperationRef {
    fn isa<K: OperationKind>(&self) -> bool {
        self.get().kind.operation == TypeId::of::<K>()
    }

    fn dyn_cast<K: OperationKind>(&self) -> Option<K::Access<'rewrite, 'a, GcContext>> {
        K::access(self.clone())
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

    fn opaque(&self) -> <GcContext as Context>::OpaqueOperation<'rewrite> {
        self.clone()
    }

    fn get_num_regions(&self) -> super::RegionPosition {
        self.get().regions.len()
    }

    fn get_region(
        &self,
        region: super::RegionPosition,
    ) -> Option<<GcContext as Context>::Region<'rewrite, 'a>> {
        self.get().regions.get(region).cloned()
    }

    fn get_num_operands(&self) -> OperandPosition {
        self.get().operands.len()
    }

    fn get_operand(
        &self,
        operand: OperandPosition,
    ) -> Option<<GcContext as Context>::Value<'rewrite, 'a>> {
        self.get().operands.get(operand).cloned()
    }

    fn get_num_results(&self) -> super::ResultPosition {
        self.get().results.len()
    }

    fn get_result(
        &self,
        result: super::ResultPosition,
    ) -> Option<<GcContext as Context>::Value<'rewrite, 'a>> {
        self.get().results.get(result).cloned()
    }

    fn get_num_successors(&self) -> super::SuccessorPosition {
        self.get().successors.len()
    }

    fn get_successor(
        &self,
        successor: super::RegionPosition,
    ) -> Option<<GcContext as Context>::Block<'rewrite, 'a>> {
        self.get().successors.get(successor).cloned()
    }
}

impl<'rewrite> OpaqueOperation<'rewrite, GcContext> for GcOperationRef {
    fn access<'a>(
        &self,
        _: &'a <GcContext as Context>::Accessor<'rewrite>,
    ) -> <GcContext as Context>::Operation<'rewrite, 'a> {
        self.clone()
    }
}

#[derive(Collectable)]
enum GcAttributeData {
    Bits(BitBox),
    Array(Vec<GcAttributeRef>),
    Dictionary(HashMap<String, GcAttributeRef>),
}

#[derive(Collectable)]
pub struct GcAttribute {
    kind: Gc<GcAttrKindVtable>,
    data: GcAttributeData,
}

#[derive(Collectable, Clone)]
pub struct GcAttributeRef(Gc<RefCell<GcAttribute>>);

impl GcAttributeRef {
    fn new(val: GcAttribute) -> Self {
        Self(Gc::new(RefCell::new(val)))
    }

    fn get(&self) -> std::cell::Ref<'_, GcAttribute> {
        (*self.0).borrow()
    }

    fn get_mut(&self) -> std::cell::RefMut<'_, GcAttribute> {
        self.0.borrow_mut()
    }
}

impl<'rewrite, 'a> Attribute<'rewrite, 'a, GcContext> for GcAttributeRef {
    fn isa<K: AttributeKind>(&self) -> bool {
        self.get().kind.attr == TypeId::of::<K>()
    }

    fn dyn_cast<K: AttributeKind>(&self) -> Option<K::Access<'rewrite, 'a, GcContext>> {
        K::access(self.clone())
    }

    fn data<'data>(&'data self) -> AttributeData<'data, 'rewrite, 'a, GcContext> {
        #[inline(always)]
        fn force_extract_bits(r: Ref<'_, GcAttribute>) -> Ref<'_, BitBox> {
            Ref::map(r, |r| match &r.data {
                GcAttributeData::Bits(bits) => bits,
                _ => unreachable!(),
            })
        }

        #[inline(always)]
        fn force_extract_array(r: Ref<'_, GcAttribute>) -> Ref<'_, [GcAttributeRef]> {
            Ref::map(r, |r| match &r.data {
                GcAttributeData::Array(arr) => arr.as_ref(),
                _ => unreachable!(),
            })
        }

        #[inline(always)]
        fn force_extract_dict(r: Ref<'_, GcAttribute>) -> Ref<'_, HashMap<String, GcAttributeRef>> {
            Ref::map(r, |r| match &r.data {
                GcAttributeData::Dictionary(arr) => arr,
                _ => unreachable!(),
            })
        }

        match &self.get().data {
            GcAttributeData::Bits(_) => {
                AttributeData::Bits(GcAttrData(force_extract_bits(self.get())))
            }
            GcAttributeData::Array(_) => {
                AttributeData::Array(GcAttrData(force_extract_array(self.get())))
            }
            GcAttributeData::Dictionary(_) => {
                AttributeData::Dictionary(GcAttrData(force_extract_dict(self.get())))
            }
        }
    }

    fn opaque(&self) -> <GcContext as Context>::OpaqueAttr<'rewrite> {
        todo!()
    }
}

impl<'rewrite> OpaqueAttr<'rewrite, GcContext> for GcAttributeRef {
    fn access<'a>(
        &self,
        _: &'a <GcContext as Context>::Accessor<'rewrite>,
    ) -> <GcContext as Context>::Attribute<'rewrite, 'a> {
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
        (*self.0).borrow()
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

impl<'rewrite, 'a> Block<'rewrite, 'a, GcContext> for GcBlockRef {
    fn ops(&self) -> impl Iterator<Item = GcOperationRef> {
        self.get().ops()
    }

    fn opaque(&self) -> GcBlockRef {
        self.clone()
    }
}

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
        (*self.0).borrow()
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

impl<'rewrite, 'a> Region<'rewrite, 'a, GcContext> for GcRegionRef {
    fn get_block(&self, block: BlockPosition) -> Option<GcBlockRef> {
        self.get().blocks.get(block).cloned()
    }

    fn opaque(&self) -> GcRegionRef {
        self.clone()
    }
}

#[derive(Collectable)]
pub struct GcValue {
    valid: bool,

    owner: ValueOwner<'static, 'static, GcContext>,
    r#type: GcAttributeRef,

    uses: HashSet<(GcAttributeRef, OperandPosition)>,
}

unsafe impl<'rewrite, 'a> Collectable for ValueOwner<'rewrite, 'a, GcContext> {
    fn accept<V: dumpster::Visitor>(&self, visitor: &mut V) -> Result<(), ()> {
        match self {
            Self::Placeholder => Ok(()),
            Self::BlockArgument(x) => x.accept(visitor),
            Self::Operation(x) => x.accept(visitor),
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
        (*self.0).borrow()
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

impl<'rewrite, 'a> Value<'rewrite, 'a, GcContext> for GcValueRef {
    fn opaque(&self) -> GcValueRef {
        self.clone()
    }
}

//============================================================================//
// Vtables
//============================================================================//

#[derive(Collectable, Debug)]
pub struct GcOpKindVtable {
    /// Type ID of the associated OperationKind.
    operation: TypeId,
}

#[derive(Collectable, Debug)]
pub struct GcAttrKindVtable {
    /// Type ID of the associated AttributeKind.
    attr: TypeId,
}
