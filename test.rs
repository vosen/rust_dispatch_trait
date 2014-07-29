#![feature(phase)]
#[phase(plugin, link)] extern crate dispatch_trait;

#[allow(dead_code)]
struct Extends<T>;

#[dispatch_trait(DefaultFnTable)]
trait FnTable {
	fn incr(&self, x: uint) -> uint { x + 1 }
	fn decr(&self, x: uint) -> uint { x - 1 }
}

struct OverrideFnTable;
impl FnTable<Extends<DefaultFnTable>> for OverrideFnTable {
	fn incr(&self, x: uint) -> uint {
		self.decr(self.base()._incr(x))
	}
}

pub struct OverrideFnTable2;
impl FnTable<Extends<OverrideFnTable>> for OverrideFnTable2 {
    fn incr(&self, x: uint) -> uint { self.base()._incr(x) + 1 }
}

pub struct OverrideFnTable3;
impl FnTable<Extends<OverrideFnTable>> for OverrideFnTable3 {
    fn decr(&self, x: uint) -> uint { self.base()._decr(x) - 1 }
}

#[test]
fn base_impl() {
	assert!(DefaultFnTable.incr(10) == 11);
	assert!(DefaultFnTable.decr(2) == 1);
}

#[test]
fn default_base_call() {
	assert!(OverrideFnTable.decr(4) == 3);
}


#[test]
fn override_call() {
	assert!(OverrideFnTable.incr(5) == 5);
}

#[test]
fn override2_call() {
	assert!(OverrideFnTable2.incr(12) == 13);
	assert!(OverrideFnTable3.decr(10) == 8);
}