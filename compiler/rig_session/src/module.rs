/*
 * The RIG Programming Language
 * Copyright (C) 2024  MD Gaziur Rahman Noor
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

use rig_ast::stmt::Stmt;
use rig_intern::{intern, InternedString};

#[derive(Debug, Clone)]
pub struct Module {
    pub file_path: InternedString,
    pub file_content: InternedString,
    pub ast: Vec<Stmt>,
}

impl Module {
    pub fn new(path: &str, file_content: &str) -> Self {
        Self {
            file_path: intern!(path),
            file_content: intern!(file_content),
            ast: vec![],
        }
    }
}
