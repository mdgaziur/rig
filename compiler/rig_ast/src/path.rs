/*
 * The RIG Programming Language
 * Copyright (C) 2023  MD Gaziur Rahman Noor
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

use rig_intern::InternedString;
use rig_span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct TyPath {
    pub segments: Vec<PathSegment>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PathSegment {
    Ident(PathIdentSegment),
    Generic(PathGenericSegment),
}

#[derive(Debug, Clone, PartialEq)]
pub struct PathIdentSegment {
    pub ident: InternedString,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PathGenericSegment {
    pub generic_params: Vec<GenericSegmentType>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GenericSegmentType {
    pub ident: InternedString,
    pub trait_bound: Option<TyPath>,
    pub span: Span,
}
