#![feature(phase)]
#[phase(plugin, link)] extern crate dispatch_trait;

#[allow(dead_code)]
struct Extends<T>;

#[dispatch_trait(DefaultFnTable)]
trait FnTable {
    fn incr<M>(&self, src: &Owner<M>, x: uint) -> uint { x + 1 }
    fn decr<M>(&self, src: &Owner<M>, x: uint) -> uint { x - 1 }
}

struct OverrideFnTable;
impl FnTable<Extends<DefaultFnTable>> for OverrideFnTable {
    fn incr<M>(&self, src: &Owner<M>, x: uint) -> uint {
        self.decr(src, self.base()._incr(src, x))
    }
}

pub struct OverrideFnTable2;
impl FnTable<Extends<OverrideFnTable>> for OverrideFnTable2 {
    fn incr<M>(&self, src: &Owner<M>, x: uint) -> uint {
        self.base()._incr(src, x) + 1
    }
}

pub struct OverrideFnTable3;
impl FnTable<Extends<OverrideFnTable>> for OverrideFnTable3 {
    fn decr<M>(&self, src: &Owner<M>, x: uint) -> uint {
        self.base()._decr(src, x) - 1
    }
}


struct Owner<M> {
    meth: M
}

impl<__:FnTable_Base, M:FnTable<__>> Owner<M> {
    fn incr(&self, x: uint) -> uint {
        self.meth.incr(self, x)
    }

    fn decr(&self, x: uint) -> uint {
        self.meth.decr(self, x)
    }
}

#[test]
fn base_impl() {
    let o = Owner { meth:DefaultFnTable };
    assert!(o.incr(10) == 11);
    assert!(o.decr(2) == 1);
}

#[test]
fn default_base_call() {
    let o = Owner { meth:OverrideFnTable };
    assert!(o.decr(4) == 3);
}


#[test]
fn override_call() {
    let o = Owner { meth:OverrideFnTable };
    assert!(o.incr(5) == 5);
}

#[test]
fn override2_call() {
    let o2 = Owner { meth:OverrideFnTable2 };
    let o3 = Owner { meth:OverrideFnTable3 };
    assert!(o2.incr(12) == 13);
    assert!(o3.decr(10) == 8);
}