#![feature(globs)]
#![feature(phase)]
#[phase(plugin, link)] extern crate dispatch_trait;

use m2::{DefaultFnTable, FnTable, FnTable_Base, Extends};

mod m1 {
    #[dispatch_trait(DefaultTestTable)]
    trait TestTable {
        fn foo(&self, x: uint) { }
        fn decr(&self, x: uint) { }
    }
}

mod m2 {
    use super::Owner;

    #[dispatch_trait(DefaultFnTable)]
    pub trait FnTable<D:'static> {
        fn incr<M>(&self, src: &Owner<D, M>, x: uint) -> uint { x + 1 }
        fn decr<M>(&self, src: &Owner<D, M>, x: uint) -> uint { x - 1 }
    }
}

struct OverrideFnTable<D>;
impl<D:'static> FnTable<D, Extends<DefaultFnTable<D>>> for OverrideFnTable<D> {
    fn incr<M>(&self, src: &Owner<D, M>, x: uint) -> uint {
        self.decr(src, self.base()._incr(src, x))
    }
}

pub struct OverrideFnTable2<D>;
impl<D:'static> FnTable<D, Extends<OverrideFnTable<D>>> for OverrideFnTable2<D> {
    fn incr<M>(&self, src: &Owner<D, M>, x: uint) -> uint {
        self.base()._incr(src, x) + 1
    }
}

pub struct OverrideFnTable3<D>;
impl<D:'static> FnTable<D, Extends<OverrideFnTable<D>>> for OverrideFnTable3<D> {
    fn decr<M>(&self, src: &Owner<D, M>, x: uint) -> uint {
        self.base()._decr(src, x) - 1
    }
}


struct Owner<D, M> {
    data: D,
    meth: M
}

impl<D:'static, __:FnTable_Base<D>, M:FnTable<D, __>> Owner<D, M> {
    fn incr(&self, x: uint) -> uint {
        self.meth.incr(self, x)
    }

    fn decr(&self, x: uint) -> uint {
        self.meth.decr(self, x)
    }
}

#[test]
fn base_impl() {
    let o : Owner<uint, DefaultFnTable<uint>> = Owner { data: 0, meth:DefaultFnTable };
    assert!(o.incr(10) == 11);
    assert!(o.decr(2) == 1);
}

#[test]
fn default_base_call() {
    let o : Owner<uint, OverrideFnTable<uint>> = Owner { data: 0, meth:OverrideFnTable };
    assert!(o.decr(4) == 3);
}


#[test]
fn override_call() {
    let o : Owner<uint, OverrideFnTable<uint>> = Owner { data: 0, meth:OverrideFnTable };
    assert!(o.incr(5) == 5);
}

#[test]
fn override2_call() {
    let o2 : Owner<uint, OverrideFnTable2<uint>> = Owner { data: 0, meth:OverrideFnTable2 };
    let o3 : Owner<uint, OverrideFnTable3<uint>> = Owner { data: 0, meth:OverrideFnTable3 };
    assert!(o2.incr(12) == 13);
    assert!(o3.decr(10) == 8);
}