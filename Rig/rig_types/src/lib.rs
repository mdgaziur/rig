use crate::checked_stmt::CheckedStmt;
use rig_ast::stmt::Stmt;
use rig_ast::visibility::Visibility;
use rig_error::ErrorCode;
use rig_span::Span;

use crate::builtins::{
    builtin_typeid_to_string, BOOL_TYPEID, FLOAT_TYPEID, INT_TYPEID, NULL_TYPEID, STRING_TYPEID,
    UNDEFINED_TYPEID,
};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

pub mod builtins;
pub mod checked_expr;
pub mod checked_stmt;

#[derive(Default, Debug, Clone)]
pub struct Module {
    pub id: ModuleId,
    pub parent_module: Option<ModuleId>,
    pub location: Vec<String>,
    pub absolute_path: String,
    pub source_code: String,
    pub functions: Vec<FunctionType>,
    pub structs: Vec<StructType>,
    pub enums: Vec<EnumType>,
    pub types: Vec<Type>,
    pub scopes: Vec<Scope>,
    pub ast: Vec<Stmt>,
    pub checked_ast: Vec<CheckedStmt>,
    pub imports: HashMap<String, Import>,
}

impl Module {
    pub fn new(
        absolute_path: String,
        location: Vec<String>,
        source_code: String,
        ast: Vec<Stmt>,
    ) -> Self {
        Self {
            absolute_path,
            location,
            source_code,
            ast,
            ..Default::default()
        }
    }

    pub fn new_scope(&mut self) -> ScopeId {
        let scope_id = ScopeId(self.id, self.scopes.len());

        self.scopes.push(Scope {
            id: scope_id,
            ..Default::default()
        });

        scope_id
    }

    pub fn get_scope(&self, scope_id: ScopeId) -> &Scope {
        &self.scopes[scope_id.1]
    }

    pub fn get_scope_mut(&mut self, scope_id: ScopeId) -> &mut Scope {
        &mut self.scopes[scope_id.1]
    }

    pub fn try_resolve(
        &self,
        modules: &[Module],
        path: &[String],
    ) -> Result<TypeIdOrModuleId, ResolutionError> {
        if path.len() == 1 {
            match path.first().unwrap().as_str() {
                "int" => return Ok(TypeIdOrModuleId::TypeId(INT_TYPEID, Visibility::NotPub)),
                "float" => return Ok(TypeIdOrModuleId::TypeId(FLOAT_TYPEID, Visibility::NotPub)),
                "null" => return Ok(TypeIdOrModuleId::TypeId(NULL_TYPEID, Visibility::NotPub)),
                "string" => return Ok(TypeIdOrModuleId::TypeId(STRING_TYPEID, Visibility::NotPub)),
                "bool" => return Ok(TypeIdOrModuleId::TypeId(BOOL_TYPEID, Visibility::NotPub)),
                "undefined" => {
                    return Ok(TypeIdOrModuleId::TypeId(
                        UNDEFINED_TYPEID,
                        Visibility::NotPub,
                    ))
                }
                _ => (),
            }
        }

        if path.len() > 1 {
            let resolved = self.try_resolve(modules, &path[0..1])?;

            return match resolved {
                TypeIdOrModuleId::TypeId(id, vis) => {
                    if vis == Visibility::NotPub && id.get_module_id() != self.id {
                        Err(ResolutionError::AttemptToImportPrivateType)
                    } else if path.len() > 2 {
                        Err(ResolutionError::InvalidImport)
                    } else {
                        Ok(resolved)
                    }
                }
                TypeIdOrModuleId::ModuleId(id, vis) => {
                    if vis == Visibility::NotPub && id.get_importer() != self.id {
                        Err(ResolutionError::AttemptToImportPrivateType)
                    } else if path.len() >= 2 {
                        let module = &modules[id.get_id().0];

                        module.try_resolve(modules, &path[1..path.len()])
                    } else {
                        Ok(resolved)
                    }
                }
            };
        }

        let name = path.first().unwrap();
        if let Some(import) = self.imports.get(name) {
            // it is caller's responsibility to make sure it's not importing private type from outside of module
            return Ok(match import {
                Import::Module(id, vis) => TypeIdOrModuleId::ModuleId(*id, *vis),
                Import::TypeId(id, vis) => TypeIdOrModuleId::TypeId(*id, *vis),
            });
        }

        for scope in self.scopes.iter().rev() {
            // it is caller's responsibility to make sure it's not importing private type from outside of module
            let function = scope.find_function(name);
            let enum_ = scope.find_enum(name);
            let struct_ = scope.find_struct(name);

            match (function, enum_, struct_) {
                (Some(_), Some(_), Some(_)) => return Err(ResolutionError::AmbiguousImport),
                _ => (),
            }

            if let Some(function) = function {
                return Ok(TypeIdOrModuleId::TypeId(function.1, function.0));
            }
            if let Some(enum_) = enum_ {
                return Ok(TypeIdOrModuleId::TypeId(enum_.1, enum_.0));
            }
            if let Some(struct_) = struct_ {
                return Ok(TypeIdOrModuleId::TypeId(struct_.1, struct_.0));
            }
        }

        Err(ResolutionError::FailedToResolve)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ResolutionError {
    AttemptToImportPrivateType,
    AmbiguousImport,
    InvalidImport,
    FailedToResolve,
}

impl Display for ResolutionError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolutionError::AttemptToImportPrivateType => {
                write!(f, "Attempt to import private type")
            }
            ResolutionError::AmbiguousImport => {
                write!(f, "Ambiguous import(different types exist with same name")
            }
            ResolutionError::InvalidImport => write!(f, "Cannot import from type"),
            ResolutionError::FailedToResolve => write!(f, "Failed to resolve type"),
        }
    }
}

impl ResolutionError {
    pub fn to_error_code(&self) -> ErrorCode {
        match self {
            ResolutionError::AttemptToImportPrivateType => ErrorCode::E0010,
            ResolutionError::AmbiguousImport => ErrorCode::E0011,
            ResolutionError::InvalidImport => ErrorCode::E0012,
            ResolutionError::FailedToResolve => ErrorCode::E0013,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TypeIdOrModuleId {
    TypeId(TypeId, Visibility),
    ModuleId(ImportedModuleId, Visibility),
}

impl TypeIdOrModuleId {
    pub fn get_typeid(&self) -> Option<TypeId> {
        match self {
            TypeIdOrModuleId::TypeId(id, _) => Some(*id),
            TypeIdOrModuleId::ModuleId(_, _) => None,
        }
    }

    pub fn get_visibility(&self) -> Visibility {
        match self {
            TypeIdOrModuleId::TypeId(_, vis) => *vis,
            TypeIdOrModuleId::ModuleId(_, vis) => *vis,
        }
    }

    pub fn get_moduleid(&self) -> ModuleId {
        match self {
            TypeIdOrModuleId::TypeId(ty, _) => ty.get_module_id(),
            TypeIdOrModuleId::ModuleId(mod_, _) => mod_.0,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Import {
    Module(ImportedModuleId, Visibility),
    TypeId(TypeId, Visibility),
}

/// First value: The module that imported this
/// Second value: Imported module
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ImportedModuleId(pub ModuleId, pub ModuleId);

impl ImportedModuleId {
    pub fn get_importer(&self) -> ModuleId {
        self.0
    }

    pub fn get_id(&self) -> ModuleId {
        self.1
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(pub usize);

impl ModuleId {
    pub fn get_id(&self) -> usize {
        self.0
    }
}

#[derive(Default, Debug, Clone)]
pub struct Scope {
    pub id: ScopeId,
    pub functions: HashMap<String, (Visibility, TypeId)>,
    pub structs: HashMap<String, (Visibility, TypeId)>,
    pub enums: HashMap<String, (Visibility, TypeId)>,
    pub imports: Vec<(Visibility, ImportedModuleId)>,
    pub variables: HashMap<String, TypeId>,
    pub parent: Option<ScopeId>,
    pub children: Vec<ScopeId>,
    pub module_id: ModuleId,
}

impl Scope {
    pub fn find_function(&self, name: &str) -> Option<&(Visibility, TypeId)> {
        self.functions.get(name)
    }

    pub fn find_struct(&self, name: &str) -> Option<&(Visibility, TypeId)> {
        self.structs.get(name)
    }

    pub fn find_enum(&self, name: &str) -> Option<&(Visibility, TypeId)> {
        self.enums.get(name)
    }

    pub fn insert_type(&mut self, name: &str, type_id: TypeId) {
        match type_id.3 {
            Type::Function => self
                .functions
                .insert(name.to_string(), (Visibility::from(type_id.2), type_id)),
            Type::Struct => self
                .structs
                .insert(name.to_string(), (Visibility::from(type_id.2), type_id)),
            Type::Enum => self
                .enums
                .insert(name.to_string(), (Visibility::from(type_id.2), type_id)),
            _ => None, // Builtin types don't need to be added manually as they are special case.
                       // Also, `None` doesn't matter as we aren't returning anything
        };
    }

    pub fn insert_import(&mut self, visibility: Visibility, module_id: ImportedModuleId) {
        self.imports.push((visibility, module_id));
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(pub ModuleId, pub usize);

impl ScopeId {
    pub fn get_module_id(&self) -> ModuleId {
        self.0
    }

    pub fn get_id(&self) -> usize {
        self.1
    }
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub ScopeId, pub usize, pub bool, pub Type);

impl TypeId {
    pub fn is_builtin(&self) -> bool {
        self.3.is_builtin()
    }

    pub fn get_scope_id(&self) -> ScopeId {
        self.0
    }

    pub fn get_module_id(&self) -> ModuleId {
        self.get_scope_id().get_module_id()
    }

    pub fn get_id(&self) -> usize {
        self.1
    }

    pub fn typeid_to_string(&self, modules: &[Module]) -> String {
        let module = &modules[self.get_module_id().0];

        match self.3 {
            Type::Function => module.functions[self.1].location.join("::"),
            Type::Struct => module.structs[self.1].location.join("::"),
            Type::Enum => module.enums[self.1].location.join("::"),
            _ => builtin_typeid_to_string(*self),
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    Integer,
    Float,
    Null,
    String,
    Boolean,
    Function,
    Struct,
    Enum,

    #[default]
    Undefined,
}

impl Type {
    pub fn is_builtin(&self) -> bool {
        match self {
            Type::Integer
            | Type::Float
            | Type::Null
            | Type::String
            | Type::Boolean
            | Type::Undefined => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionType {
    /// `mylibrary::mymodule::my_function` -> `["mylibrary", "mymodule", "my_function"]`
    pub location: Vec<String>,
    pub args: Vec<FunctionArgument>,
    pub return_ty: Option<TypeId>,
    pub visibility: Visibility,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FunctionArgument {
    pub name: String,
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StructType {
    pub location: Vec<String>,
    pub fields: Vec<StructFieldType>,
    pub methods: Vec<FunctionType>,
    pub helpers: Vec<FunctionType>,
    pub visibility: Visibility,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StructFieldType {
    pub name: String,
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumType {
    pub location: Vec<String>,
    pub variants: Vec<EnumVariantType>,
    pub visibility: Visibility,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumVariantType {
    pub name: String,
    pub value: Vec<EnumVariantValueType>,
}

#[derive(Debug, Clone)]
pub struct EnumVariantValueType {
    pub name: String,
    pub value_ty: TypeId,
}
