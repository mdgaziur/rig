/// The following type ids are special case.
///
/// The type checker should handle them separately as they
/// don't have similar properties as user defined types.
/// That means these type ids doesn't contain valid module id,
/// scope id, and visibility indicator. So, the type checker should
/// never *ever* treat these as normal types.
use crate::{ModuleId, ScopeId, Type, TypeId};

pub const INT_TYPEID: TypeId = TypeId(ScopeId(ModuleId(0), 0), 0, false, Type::Integer);
pub const FLOAT_TYPEID: TypeId = TypeId(ScopeId(ModuleId(0), 0), 0, false, Type::Float);
pub const NULL_TYPEID: TypeId = TypeId(ScopeId(ModuleId(0), 0), 0, false, Type::Null);
pub const STRING_TYPEID: TypeId = TypeId(ScopeId(ModuleId(0), 0), 0, false, Type::String);
pub const BOOL_TYPEID: TypeId = TypeId(ScopeId(ModuleId(0), 0), 0, false, Type::Boolean);
pub const UNDEFINED_TYPEID: TypeId = TypeId(ScopeId(ModuleId(0), 0), 0, false, Type::Undefined);
