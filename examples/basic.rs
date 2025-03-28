use robyn::{
    dialect::builtin::ModuleOp,
    ir::{Accessor, Block, Context, GcContext, Operation, PatternResult, RewritePattern, Rewriter},
};

struct FooPattern;

impl<C: Context> RewritePattern<C> for FooPattern {
    fn match_and_rewrite(&self, accessor: C::Accessor<'_>) -> PatternResult {
        let op = accessor
            .get_root()
            .dyn_cast::<ModuleOp>()
            .expect("don't worry about it");
        let to_insert_into = op.get_body().opaque();
        drop(op);

        let rewriter = accessor.rewrite();
        let new_op = ModuleOp::create::<C>(&rewriter, &[]);
        rewriter.insert_op_at_start(new_op, to_insert_into);

        PatternResult::Success
    }
}

struct BarPattern;

impl<C: Context> RewritePattern<C> for BarPattern {
    fn match_and_rewrite(&self, accessor: C::Accessor<'_>) -> PatternResult {
        let block_len = accessor
            .get_root()
            .dyn_cast::<ModuleOp>()
            .expect("don't worry about it")
            .get_body()
            .ops()
            .count();
        dbg!(block_len);
        PatternResult::Success
    }
}

fn main() {
    let ctx = GcContext::new();

    let mut program = ctx.module_program();

    ctx.apply_pattern(&mut program, &FooPattern);
    ctx.apply_pattern(&mut program, &BarPattern);
}
