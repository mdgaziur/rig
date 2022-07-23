use crate::checked_stmt::{CheckedBlockStmt, CheckedStmt};
use rig_ast::stmt::Stmt;
use rig_ast::visibility::Visibility;
use rig_error::ErrorCode;
use rig_span::Span;

use std::collections::HashMap;
use std::fmt::{Display, Formatter};

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

    pub fn try_import(
        &self,
        modules: &[Module],
        path: &[String],
    ) -> Result<TypeIdOrModuleId, ResolutionError> {
        if path.len() > 1 {
            // definitely a path referring to a module
            let ty = self.try_import(modules, &path[0..1])?;

            return match ty {
                TypeIdOrModuleId::TypeId(_, Visibility::Pub) => {
                    if path.len() >= 2 {
                        Err(ResolutionError::InvalidImport)
                    } else {
                        Ok(ty)
                    }
                }
                TypeIdOrModuleId::ModuleId(mod_id, Visibility::Pub) => {
                    if path.len() >= 2 {
                        let module = modules.get(mod_id.0).unwrap();
                        module.try_import(modules, &path[1..path.len()])
                    } else {
                        Ok(ty)
                    }
                }
                _ => Err(ResolutionError::AttemptToImportPrivateType),
            };
        }

        let name = path.first().unwrap();
        if let Some(id) = self.imports.get(name) {
            return match id {
                Import::TypeId(id, Visibility::Pub) => {
                    Ok(TypeIdOrModuleId::TypeId(id.clone(), Visibility::Pub))
                }
                Import::Module(id, Visibility::Pub) => {
                    Ok(TypeIdOrModuleId::ModuleId(id.clone(), Visibility::Pub))
                }
                _ => Err(ResolutionError::AttemptToImportPrivateType),
            };
        }

        for scope in self.scopes.iter().rev() {
            let function = scope.find_function(name);
            let enum_ = scope.find_enum(name);
            let struct_ = scope.find_struct(name);

            match (function, enum_, struct_) {
                (Some(_), Some(_), Some(_)) => return Err(ResolutionError::AmbiguousImport),
                _ => (),
            }

            // don't care about visibility for now
            if let Some(function) = function {
                if function.0 == Visibility::NotPub {
                    return Err(ResolutionError::AttemptToImportPrivateType);
                }
                return Ok(TypeIdOrModuleId::TypeId(function.1, Visibility::Pub));
            }
            if let Some(enum_) = enum_ {
                if enum_.0 == Visibility::NotPub {
                    return Err(ResolutionError::AttemptToImportPrivateType);
                }

                return Ok(TypeIdOrModuleId::TypeId(enum_.1, Visibility::Pub));
            }
            if let Some(struct_) = struct_ {
                if struct_.0 == Visibility::NotPub {
                    return Err(ResolutionError::AttemptToImportPrivateType);
                }

                return Ok(TypeIdOrModuleId::TypeId(struct_.1, Visibility::Pub));
            }
        }

        Err(ResolutionError::FailedToImport)
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum ResolutionError {
    AttemptToImportPrivateType,
    AmbiguousImport,
    InvalidImport,
    FailedToImport,
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
            ResolutionError::FailedToImport => write!(f, "Failed to import type/module"),
        }
    }
}

impl ResolutionError {
    pub fn to_error_code(&self) -> ErrorCode {
        match self {
            ResolutionError::AttemptToImportPrivateType => ErrorCode::E0010,
            ResolutionError::AmbiguousImport => ErrorCode::E0011,
            ResolutionError::InvalidImport => ErrorCode::E0012,
            ResolutionError::FailedToImport => ErrorCode::E0009,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TypeIdOrModuleId {
    TypeId(TypeId, Visibility),
    ModuleId(ModuleId, Visibility),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Import {
    Module(ModuleId, Visibility),
    TypeId(TypeId, Visibility),
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
    pub imports: Vec<(Visibility, ModuleId)>,
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

    pub fn insert_import(&mut self, visibility: Visibility, module_id: ModuleId) {
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
    pub fn get_scope_id(&self) -> ScopeId {
        self.0
    }

    pub fn get_id(&self) -> usize {
        self.1
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

#[derive(Debug, Clone)]
pub struct FunctionType {
    /// `mylibrary::mymodule::my_function` -> `["mylibrary", "mymodule", "my_function"]`
    pub location: Vec<String>,
    pub args: Vec<FunctionArgument>,
    pub return_ty: Option<TypeId>,
    pub body: CheckedBlockStmt,
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
