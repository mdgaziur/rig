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

use crate::Parser;
use rig_ast::path::{
    GenericSegmentType, PathGenericSegment, PathIdentSegment, PathSegment, TyPath,
};
use rig_ast::token::TokenKind;
use rig_errors::CodeError;

pub fn parse_ty_path(parser: &mut Parser, in_expr: bool) -> Result<TyPath, CodeError> {
    let mut segments = vec![];

    loop {
        match parser.peek().kind {
            TokenKind::Ident(ident) => {
                segments.push(PathSegment::Ident(PathIdentSegment {
                    ident,
                    span: parser.current_span(),
                }));
                parser.advance();
            }
            TokenKind::Less => segments.push(PathSegment::Generic(parse_generic_params(
                parser, !in_expr,
            )?)),
            _ => {
                return Err(CodeError::unexpected_token_with_hint(
                    parser.current_span(),
                    "expected an identifier or a list of generic parameters here",
                ))
            }
        }

        if !parser.is_eof() {
            match parser.peek().kind {
                TokenKind::PathSep => {
                    parser.advance_without_eof()?;

                    if !in_expr && parser.peek().kind == TokenKind::Less {
                        parser.diags.push(CodeError::warning(
                            "unnecessary path separator. \
                        Path separator is unnecessary in type path in this position when \
                        it's outside expressions.",
                            parser.previous().span,
                        ));
                    }
                }
                TokenKind::Less if !in_expr => {}
                _ => {
                    break;
                }
            }
        } else {
            break;
        }
    }

    Ok(TyPath { segments })
}

pub fn parse_generic_params(
    parser: &mut Parser,
    allow_trait_bound: bool,
) -> Result<PathGenericSegment, CodeError> {
    let start_span = parser.current_span();
    let mut generic_params = vec![];
    parser.advance_without_eof()?;

    while let TokenKind::Ident(ident) = parser.peek().kind {
        let ident_span = parser.current_span();
        let trait_bound;

        parser.advance_without_eof()?;

        if allow_trait_bound {
            if parser.peek().kind == TokenKind::Colon {
                parser.advance_without_eof()?;

                if let TokenKind::Ident(_) = parser.peek().kind {
                    trait_bound = Some(parse_ty_path(parser, false)?);
                } else {
                    trait_bound = None;
                }
            } else {
                trait_bound = None;
            }
        } else {
            trait_bound = None;
        }

        generic_params.push(GenericSegmentType {
            ident,
            trait_bound,
            span: ident_span.merge(parser.previous().span),
        });

        if parser.peek().kind != TokenKind::Comma {
            if let Some(token) = parser.try_peek()
                && let TokenKind::Ident(_) = token.kind
            {
                parser.diags.push(CodeError::unexpected_token_with_hint(
                    parser.current_span(),
                    "expected a `,` here",
                ));
            }
        } else {
            parser.advance_without_eof()?;
        }
    }
    let end_span;
    if parser.peek().kind != TokenKind::Greater {
        end_span = parser.previous().span;
        parser.diags.push(CodeError::unexpected_token_with_hint(
            parser.current_span(),
            "expected a `>` here",
        ));
    } else {
        end_span = parser.current_span();
        parser.advance();
    }

    Ok(PathGenericSegment {
        generic_params,
        span: start_span.merge(end_span),
    })
}

#[cfg(test)]
mod test {
    use crate::ty::parse_generic_params;
    use crate::ty::parse_ty_path;
    use crate::Parser;
    use parking_lot::RwLock;
    use rig_ast::path::{
        GenericSegmentType, PathGenericSegment, PathIdentSegment, PathSegment, TyPath,
    };
    use rig_ast::token::LexicalToken;
    use rig_intern::{intern, Interner, INTERNER};
    use rig_lexer::Lexer;
    use rig_session::Session;
    use rig_span::Span;

    #[test]
    fn test_ident_path() {
        INTERNER.init_once(|| RwLock::new(Interner::new()));
        let file_name = intern!("<test>");
        let source = "s1::s2::s3::s4::<T1, T2>";
        let mut lexer = Lexer::new(source.chars(), file_name);
        let tokens = lexer
            .lex()
            .iter()
            .map(|v| v.clone().unwrap())
            .collect::<Vec<LexicalToken>>();
        let mut parser = Parser::new(&tokens);
        assert_eq!(
            parse_ty_path(&mut parser, true).unwrap(),
            TyPath {
                segments: vec![
                    PathSegment::Ident(PathIdentSegment {
                        ident: intern!("s1"),
                        span: Span {
                            lo: 0,
                            hi: 1,
                            file_path: intern!("<test>"),
                        },
                    }),
                    PathSegment::Ident(PathIdentSegment {
                        ident: intern!("s2"),
                        span: Span {
                            lo: 4,
                            hi: 5,
                            file_path: intern!("<test>"),
                        },
                    },),
                    PathSegment::Ident(PathIdentSegment {
                        ident: intern!("s3"),
                        span: Span {
                            lo: 8,
                            hi: 9,
                            file_path: intern!("<test>"),
                        },
                    },),
                    PathSegment::Ident(PathIdentSegment {
                        ident: intern!("s4"),
                        span: Span {
                            lo: 12,
                            hi: 13,
                            file_path: intern!("<test>"),
                        },
                    }),
                    PathSegment::Generic(PathGenericSegment {
                        generic_params: vec![
                            GenericSegmentType {
                                ident: intern!("T1"),
                                trait_bound: None,
                                span: Span {
                                    lo: 17,
                                    hi: 18,
                                    file_path: intern!("<test>")
                                }
                            },
                            GenericSegmentType {
                                ident: intern!("T2"),
                                trait_bound: None,
                                span: Span {
                                    lo: 21,
                                    hi: 22,
                                    file_path: intern!("<test>")
                                }
                            }
                        ],
                        span: Span {
                            lo: 16,
                            hi: 23,
                            file_path: intern!("<test>")
                        }
                    })
                ],
            }
        )
    }
    #[test]
    fn test_ident_path_not_in_expr() {
        INTERNER.init_once(|| RwLock::new(Interner::new()));
        let file_name = intern!("<test>");
        let source = "s1::s2::s3::s4<T1, T2>";
        let mut lexer = Lexer::new(source.chars(), file_name);
        let tokens = lexer
            .lex()
            .iter()
            .map(|v| v.clone().unwrap())
            .collect::<Vec<LexicalToken>>();
        let mut parser = Parser::new(&tokens);
        assert_eq!(
            parse_ty_path(&mut parser, false).unwrap(),
            TyPath {
                segments: vec![
                    PathSegment::Ident(PathIdentSegment {
                        ident: intern!("s1"),
                        span: Span {
                            lo: 0,
                            hi: 1,
                            file_path: intern!("<test>"),
                        },
                    },),
                    PathSegment::Ident(PathIdentSegment {
                        ident: intern!("s2"),
                        span: Span {
                            lo: 4,
                            hi: 5,
                            file_path: intern!("<test>"),
                        },
                    },),
                    PathSegment::Ident(PathIdentSegment {
                        ident: intern!("s3"),
                        span: Span {
                            lo: 8,
                            hi: 9,
                            file_path: intern!("<test>"),
                        },
                    },),
                    PathSegment::Ident(PathIdentSegment {
                        ident: intern!("s4"),
                        span: Span {
                            lo: 12,
                            hi: 13,
                            file_path: intern!("<test>"),
                        },
                    },),
                    PathSegment::Generic(PathGenericSegment {
                        generic_params: vec![
                            GenericSegmentType {
                                ident: intern!("T1"),
                                trait_bound: None,
                                span: Span {
                                    lo: 15,
                                    hi: 16,
                                    file_path: intern!("<test>"),
                                },
                            },
                            GenericSegmentType {
                                ident: intern!("T2"),
                                trait_bound: None,
                                span: Span {
                                    lo: 19,
                                    hi: 20,
                                    file_path: intern!("<test>"),
                                },
                            },
                        ],
                        span: Span {
                            lo: 14,
                            hi: 21,
                            file_path: intern!("<test>"),
                        },
                    },),
                ],
            }
        );
    }

    #[test]
    fn test_path_with_trait_bound() {
        INTERNER.init_once(|| RwLock::new(Interner::new()));
        let file_name = intern!("<test>");
        let source = "<T1: mod1::A, T2: B>";
        let mut lexer = Lexer::new(source.chars(), file_name);
        let tokens = lexer
            .lex()
            .iter()
            .map(|v| v.clone().unwrap())
            .collect::<Vec<LexicalToken>>();
        let mut parser = Parser::new(&tokens);
        let mut session = Session::new();
        let res = parse_generic_params(&mut parser, true).unwrap();
        session.insert_file("<test>", source);
        for diag in parser.get_diags() {
            diag.display(&session);
        }
        assert_eq!(
            res,
            PathGenericSegment {
                generic_params: vec![
                    GenericSegmentType {
                        ident: intern!("T1"),
                        trait_bound: Some(TyPath {
                            segments: vec![
                                PathSegment::Ident(PathIdentSegment {
                                    ident: intern!("mod1"),
                                    span: Span {
                                        lo: 5,
                                        hi: 8,
                                        file_path: intern!("<test>")
                                    }
                                }),
                                PathSegment::Ident(PathIdentSegment {
                                    ident: intern!("A"),
                                    span: Span {
                                        lo: 11,
                                        hi: 11,
                                        file_path: intern!("<test>")
                                    }
                                })
                            ]
                        }),
                        span: Span {
                            lo: 1,
                            hi: 11,
                            file_path: intern!("<test>")
                        }
                    },
                    GenericSegmentType {
                        ident: intern!("T2"),
                        trait_bound: Some(TyPath {
                            segments: vec![PathSegment::Ident(PathIdentSegment {
                                ident: intern!("B"),
                                span: Span {
                                    lo: 18,
                                    hi: 18,
                                    file_path: intern!("<test>")
                                }
                            })]
                        }),
                        span: Span {
                            lo: 14,
                            hi: 18,
                            file_path: intern!("<test>")
                        }
                    }
                ],
                span: Span {
                    lo: 0,
                    hi: 19,
                    file_path: intern!("<test>")
                }
            }
        );
    }
}
