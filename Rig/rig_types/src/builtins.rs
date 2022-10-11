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

pub fn builtin_typeid_to_string(ty: TypeId) -> String {
    match ty {
        INT_TYPEID => "int".to_string(),
        FLOAT_TYPEID => "float".to_string(),
        NULL_TYPEID => "null".to_string(),
        STRING_TYPEID => "string".to_string(),
        BOOL_TYPEID => "bool".to_string(),
        UNDEFINED_TYPEID => "undefined".to_string(),
        _ => panic!("Not a builtin type id"),
    }
}