use crate::parser::{
    ExprToken, ExprTree, Instruction, OperationToken, Program, Statement, Type, ValueToken,
    Variable,
};
use std::fmt::Display;
use std::{collections::HashMap, rc::Rc};

const REGISTERS: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
const REGISTERS_8: [&str; 6] = ["dil", "sil", "dl", "cl", "r8b", "r9b"];

#[derive(Debug)]
pub enum GenerationError {
    IdentifierNotDeclared,
    TypeError,
    OperationNotSupportedByType,
    NotCharacterLiteral,
    CharacterNotSupported,
    CouldntInferType,
    CannotReadBooleans,
}

impl Display for GenerationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IdentifierNotDeclared => {
                write!(f, "Há um identificador não declarado a ser usado.")
            }
            Self::TypeError => write!(f, "Há uma incompatibilidade de tipos."),
            Self::OperationNotSupportedByType => {
                write!(f, "Um tipo de dados não suporta essa operação")
            }
            Self::NotCharacterLiteral => {
                write!(f, "Esperava-se um caractere, encontrou-se uma cadeia.")
            }
            Self::CharacterNotSupported => write!(f, "Caractere não faz parte da tabela ASCII"),
            Self::CouldntInferType => {
                write!(f, "Não foi possível determinar o tipo de uma expressão")
            }
            Self::CannotReadBooleans => write!(
                f,
                "Não se pode executar a instrução 'Ler' em valores lógicos."
            ),
        }
    }
}

fn type_inference(
    expression: &ExprTree,
    variables: &HashMap<Rc<str>, Variable>,
    result_type: Option<Type>,
) -> Option<Type> {
    match &expression.token {
        ExprToken::Val(ValueToken::Identifier(ident)) => Some(variables.get(ident)?.var_type),
        ExprToken::Val(ValueToken::StringLiteral(content)) => Some(if content.len() != 1 {
            Type::CharacterChain(false)
        } else {
            Type::Character
        }),
        ExprToken::Val(ValueToken::IntLiteral(_)) => Some(Type::Integer),
        ExprToken::Val(ValueToken::RealLiteral(_)) => Some(Type::Real),
        ExprToken::Val(ValueToken::True)
        | ExprToken::Val(ValueToken::False)
        | ExprToken::Op(OperationToken::LessThan)
        | ExprToken::Op(OperationToken::LessThanOrEqual)
        | ExprToken::Op(OperationToken::GreaterThan)
        | ExprToken::Op(OperationToken::GreaterThanOrEqual)
        | ExprToken::Op(OperationToken::Equality)
        | ExprToken::Op(OperationToken::Inequality) => Some(Type::Boolean),
        _ => expression
            .left
            .as_ref()
            .and_then(|expr| type_inference(expr, variables, result_type))
            .or_else(|| {
                expression
                    .right
                    .as_ref()
                    .and_then(|expr| type_inference(expr, variables, result_type))
            })
            .or(result_type),
    }
}

fn generate_expression(
    expression: ExprTree,
    variables: &HashMap<Rc<str>, Variable>,
    instructions: &mut Vec<Box<str>>,
    program_data: &mut ProgramData,
    result_type: Type,
) -> Result<(), GenerationError> {
    match result_type {
        Type::Integer => match expression.token {
            ExprToken::Val(ValueToken::Identifier(ident)) => {
                let var = variables
                    .get(&ident)
                    .ok_or(GenerationError::IdentifierNotDeclared)?;
                match var.var_type {
                    Type::Integer => instructions.push(
                        format!("\tmov rax, qword ptr [rbp-{}]", var.offset).into_boxed_str(),
                    ),
                    Type::Boolean | Type::Character => {
                        instructions.push(
                            format!("\tmovzx rax, byte ptr [rbp-{}]", var.offset).into_boxed_str(),
                        );
                    }
                    _ => return Err(GenerationError::OperationNotSupportedByType),
                }
                instructions.push(Box::from("\tpush rax"));
            }
            ExprToken::Val(ValueToken::StringLiteral(content)) => {
                if content.len() != 1 {
                    return Err(GenerationError::NotCharacterLiteral);
                }
                let i = unsafe { content.chars().next().unwrap_unchecked() } as u32;
                if i > u8::MAX as u32 {
                    return Err(GenerationError::CharacterNotSupported);
                }
                instructions.push(format!("\tmov rax, {i}").into_boxed_str());
                instructions.push(Box::from("\tpush rax"));
            }
            ExprToken::Val(ValueToken::IntLiteral(x)) => {
                instructions.push(format!("\tmov rax, {x}").into_boxed_str());
                instructions.push(Box::from("\tpush rax"));
            }
            ExprToken::Op(OperationToken::Pow) => {
                program_data.pow = true;
                generate_expression(
                    unsafe { *expression.left.unwrap_unchecked() },
                    variables,
                    instructions,
                    program_data,
                    result_type,
                )?;
                generate_expression(
                    unsafe { *expression.right.unwrap_unchecked() },
                    variables,
                    instructions,
                    program_data,
                    result_type,
                )?;
                instructions.pop();
                instructions.push(Box::from("\tmov rsi, rax"));
                instructions.push(Box::from("\tpop rdi"));
                instructions.push(Box::from("\tcall pow"));
                instructions.push(Box::from("\tpush rax"));
            }
            ExprToken::Op(OperationToken::Not) => {
                generate_expression(
                    unsafe { *expression.left.unwrap_unchecked() },
                    variables,
                    instructions,
                    program_data,
                    result_type,
                )?;
                instructions.pop();
                instructions.push(Box::from("\tnot rax"));
                instructions.push(Box::from("\tpush rax"));
            }
            ExprToken::Op(OperationToken::Mul) => {
                generate_expression(
                    unsafe { *expression.left.unwrap_unchecked() },
                    variables,
                    instructions,
                    program_data,
                    result_type,
                )?;
                generate_expression(
                    unsafe { *expression.right.unwrap_unchecked() },
                    variables,
                    instructions,
                    program_data,
                    result_type,
                )?;
                instructions.pop();
                instructions.push(Box::from("\tpop rdi"));
                instructions.push(Box::from("\timul rax, rdi"));
                instructions.push(Box::from("\tpush rax"));
            }
            ExprToken::Op(OperationToken::Div) => unimplemented!(),
            ExprToken::Op(OperationToken::IDiv) => {
                generate_expression(
                    unsafe { *expression.left.unwrap_unchecked() },
                    variables,
                    instructions,
                    program_data,
                    result_type,
                )?;
                generate_expression(
                    unsafe { *expression.right.unwrap_unchecked() },
                    variables,
                    instructions,
                    program_data,
                    result_type,
                )?;
                instructions.pop();
                instructions.push(Box::from("\tmov rdi, rax"));
                instructions.push(Box::from("\tpop rax"));
                instructions.push(Box::from("\txor rdx, rdx"));
                instructions.push(Box::from("\tidiv rdx"));
                instructions.push(Box::from("\tpush rax"));
            }
            ExprToken::Op(OperationToken::Mod) => {
                generate_expression(
                    unsafe { *expression.left.unwrap_unchecked() },
                    variables,
                    instructions,
                    program_data,
                    result_type,
                )?;
                generate_expression(
                    unsafe { *expression.right.unwrap_unchecked() },
                    variables,
                    instructions,
                    program_data,
                    result_type,
                )?;
                instructions.pop();
                instructions.push(Box::from("\tmov rdi, rax"));
                instructions.push(Box::from("\tpop rax"));
                instructions.push(Box::from("\txor rdx, rdx"));
                instructions.push(Box::from("\tidiv rdx"));
                instructions.push(Box::from("\tmov rax, rdx"));
                instructions.push(Box::from("\tpush rax"));
            }
            ExprToken::Op(OperationToken::Add) => {
                generate_expression(
                    unsafe { *expression.left.unwrap_unchecked() },
                    variables,
                    instructions,
                    program_data,
                    result_type,
                )?;
                generate_expression(
                    unsafe { *expression.right.unwrap_unchecked() },
                    variables,
                    instructions,
                    program_data,
                    result_type,
                )?;
                instructions.pop();
                instructions.push(Box::from("\tpop rdi"));
                instructions.push(Box::from("\tadd rax, rdi"));
                instructions.push(Box::from("\tpush rax"));
            }
            ExprToken::Op(OperationToken::Sub) => {
                generate_expression(
                    unsafe { *expression.left.unwrap_unchecked() },
                    variables,
                    instructions,
                    program_data,
                    result_type,
                )?;
                generate_expression(
                    unsafe { *expression.right.unwrap_unchecked() },
                    variables,
                    instructions,
                    program_data,
                    result_type,
                )?;
                instructions.pop();
                instructions.push(Box::from("\tmov rdi, rax"));
                instructions.push(Box::from("\tpop rax"));
                instructions.push(Box::from("\tsub rax, rdi"));
                instructions.push(Box::from("\tpush rax"));
            }
            ExprToken::Op(OperationToken::And) => {
                generate_expression(
                    unsafe { *expression.left.unwrap_unchecked() },
                    variables,
                    instructions,
                    program_data,
                    result_type,
                )?;
                generate_expression(
                    unsafe { *expression.right.unwrap_unchecked() },
                    variables,
                    instructions,
                    program_data,
                    result_type,
                )?;
                instructions.pop();
                instructions.push(Box::from("\tpop rdi"));
                instructions.push(Box::from("\tand rax, rdi"));
                instructions.push(Box::from("\tpush rax"));
            }
            ExprToken::Op(OperationToken::Or) => {
                generate_expression(
                    unsafe { *expression.left.unwrap_unchecked() },
                    variables,
                    instructions,
                    program_data,
                    result_type,
                )?;
                generate_expression(
                    unsafe { *expression.right.unwrap_unchecked() },
                    variables,
                    instructions,
                    program_data,
                    result_type,
                )?;
                instructions.pop();
                instructions.push(Box::from("\tpop rdi"));
                instructions.push(Box::from("\tor rax, rdi"));
                instructions.push(Box::from("\tpush rax"));
            }
            ExprToken::Op(OperationToken::XOr) => {
                generate_expression(
                    unsafe { *expression.left.unwrap_unchecked() },
                    variables,
                    instructions,
                    program_data,
                    result_type,
                )?;
                generate_expression(
                    unsafe { *expression.right.unwrap_unchecked() },
                    variables,
                    instructions,
                    program_data,
                    result_type,
                )?;
                instructions.pop();
                instructions.push(Box::from("\tpop rdi"));
                instructions.push(Box::from("\txor rax, rdi"));
                instructions.push(Box::from("\tpush rax"));
            }
            _ => return Err(GenerationError::OperationNotSupportedByType),
        },
        Type::CharacterChain(_) => match expression.token {
            ExprToken::Val(ValueToken::Identifier(ident)) => {
                let var = variables
                    .get(&ident)
                    .ok_or(GenerationError::IdentifierNotDeclared)?;
                if let Type::CharacterChain(_) = var.var_type {
                    instructions.push(
                        format!("\tmov rax, qword ptr [rbp-{}]", var.offset).into_boxed_str(),
                    );
                    instructions.push(Box::from("\tpush rax"));
                } else {
                    return Err(GenerationError::TypeError);
                }
            }
            ExprToken::Val(ValueToken::StringLiteral(content)) => {
                let i = program_data.add_string((*content).into());
                instructions.push(format!("\tlea rax, [str{i}]").into_boxed_str());
                instructions.push(Box::from("\tpush rax"));
            }
            _ => return Err(GenerationError::OperationNotSupportedByType),
        },
        Type::Character => match expression.token {
            ExprToken::Val(ValueToken::Identifier(ident)) => {
                let var = variables
                    .get(&ident)
                    .ok_or(GenerationError::IdentifierNotDeclared)?;
                if let Type::Character = var.var_type {
                    instructions
                        .push(format!("\tmov al, byte ptr [rbp-{}]", var.offset).into_boxed_str());
                    instructions.push(Box::from("\tsub rsp, 1"));
                    instructions.push(Box::from("\tmov byte ptr [rsp], al"));
                } else {
                    return Err(GenerationError::TypeError);
                }
            }
            ExprToken::Val(ValueToken::StringLiteral(content)) => {
                if content.len() != 1 {
                    return Err(GenerationError::NotCharacterLiteral);
                }
                let i = unsafe { content.chars().next().unwrap_unchecked() } as u32;
                if i > u8::MAX as u32 {
                    return Err(GenerationError::CharacterNotSupported);
                }
                instructions.push(format!("\tmov al, {i}").into_boxed_str());
                instructions.push(Box::from("\tsub rsp, 1"));
                instructions.push(Box::from("\tmov byte ptr [rsp], al"));
            }
            _ => return Err(GenerationError::OperationNotSupportedByType),
        },
        Type::Boolean => match expression.token {
            ExprToken::Val(ValueToken::Identifier(ident)) => {
                let var = variables
                    .get(&ident)
                    .ok_or(GenerationError::IdentifierNotDeclared)?;
                if let Type::Boolean = var.var_type {
                    instructions
                        .push(format!("\tmov al, byte ptr [rbp-{}]", var.offset).into_boxed_str());
                    instructions.push(Box::from("\tsub rsp, 1"));
                    instructions.push(Box::from("\tmov byte ptr [rsp], al"));
                } else {
                    return Err(GenerationError::TypeError);
                }
            }
            ExprToken::Val(ValueToken::True) => {
                instructions.push(Box::from("\tmov al, 1"));
                instructions.push(Box::from("\tsub rsp, 1"));
                instructions.push(Box::from("\tmov byte ptr [rsp], al"));
            }
            ExprToken::Val(ValueToken::False) => {
                instructions.push(Box::from("\txor al, al"));
                instructions.push(Box::from("\tsub rsp, 1"));
                instructions.push(Box::from("\tmov byte ptr [rsp], al"));
            }
            ExprToken::Op(OperationToken::Not) => {
                generate_expression(
                    unsafe { *expression.left.unwrap_unchecked() },
                    variables,
                    instructions,
                    program_data,
                    result_type,
                )?;
                instructions.pop();
                instructions.pop();
                instructions.push(Box::from("\txor al, 1"));
                instructions.push(Box::from("\tsub rsp, 1"));
                instructions.push(Box::from("\tmov byte ptr [rsp], al"));
            }
            ExprToken::Op(OperationToken::And) => {
                generate_expression(
                    unsafe { *expression.left.unwrap_unchecked() },
                    variables,
                    instructions,
                    program_data,
                    result_type,
                )?;
                generate_expression(
                    unsafe { *expression.right.unwrap_unchecked() },
                    variables,
                    instructions,
                    program_data,
                    result_type,
                )?;
                instructions.pop();
                instructions.pop();
                instructions.push(Box::from("\tmov dil, byte ptr [rsp]"));
                instructions.push(Box::from("\tadd rsp, 1"));
                instructions.push(Box::from("\tand al, dil"));
                instructions.push(Box::from("\tsub rsp, 1"));
                instructions.push(Box::from("\tmov byte ptr [rsp], al"));
            }
            ExprToken::Op(OperationToken::Or) => {
                generate_expression(
                    unsafe { *expression.left.unwrap_unchecked() },
                    variables,
                    instructions,
                    program_data,
                    result_type,
                )?;
                generate_expression(
                    unsafe { *expression.right.unwrap_unchecked() },
                    variables,
                    instructions,
                    program_data,
                    result_type,
                )?;
                instructions.pop();
                instructions.pop();
                instructions.push(Box::from("\tmov dil, byte ptr [rsp]"));
                instructions.push(Box::from("\tadd rsp, 1"));
                instructions.push(Box::from("\tor al, dil"));
                instructions.push(Box::from("\tsub rsp, 1"));
                instructions.push(Box::from("\tmov byte ptr [rsp], al"));
            }
            ExprToken::Op(OperationToken::XOr) => {
                generate_expression(
                    unsafe { *expression.left.unwrap_unchecked() },
                    variables,
                    instructions,
                    program_data,
                    result_type,
                )?;
                generate_expression(
                    unsafe { *expression.right.unwrap_unchecked() },
                    variables,
                    instructions,
                    program_data,
                    result_type,
                )?;
                instructions.pop();
                instructions.pop();
                instructions.push(Box::from("\tmov dil, byte ptr [rsp]"));
                instructions.push(Box::from("\tadd rsp, 1"));
                instructions.push(Box::from("\txor al, dil"));
                instructions.push(Box::from("\tsub rsp, 1"));
                instructions.push(Box::from("\tmov byte ptr [rsp], al"));
            }
            ExprToken::Op(OperationToken::LessThan)
            | ExprToken::Op(OperationToken::LessThanOrEqual)
            | ExprToken::Op(OperationToken::GreaterThan)
            | ExprToken::Op(OperationToken::GreaterThanOrEqual)
            | ExprToken::Op(OperationToken::Equality)
            | ExprToken::Op(OperationToken::Inequality) => {
                let expr_left = type_inference(
                    unsafe { expression.left.as_ref().unwrap_unchecked() },
                    variables,
                    None,
                );
                let expr_right = type_inference(
                    unsafe { expression.right.as_ref().unwrap_unchecked() },
                    variables,
                    None,
                );
                let expr_types = if expr_left == expr_right {
                    expr_left
                } else {
                    if matches!(expr_left, Some(Type::CharacterChain(_)))
                        || matches!(expr_right, Some(Type::CharacterChain(_)))
                    {
                        Some(Type::CharacterChain(false))
                    } else {
                        expr_left.or(expr_right)
                    }
                }
                .ok_or(GenerationError::CouldntInferType)?;
                generate_expression(
                    unsafe { *expression.left.unwrap_unchecked() },
                    variables,
                    instructions,
                    program_data,
                    expr_types,
                )?;
                generate_expression(
                    unsafe { *expression.right.unwrap_unchecked() },
                    variables,
                    instructions,
                    program_data,
                    expr_types,
                )?;
                let inst = match expression.token {
                    ExprToken::Op(OperationToken::LessThan) => "setl",
                    ExprToken::Op(OperationToken::LessThanOrEqual) => "setle",
                    ExprToken::Op(OperationToken::GreaterThan) => "setg",
                    ExprToken::Op(OperationToken::GreaterThanOrEqual) => "setge",
                    ExprToken::Op(OperationToken::Equality) => "sete",
                    ExprToken::Op(OperationToken::Inequality) => "setne",
                    _ => unreachable!(),
                };
                match expr_types {
                    Type::Integer | Type::CharacterChain(_) => {
                        instructions.pop();
                        instructions.push(Box::from("\tmov rdi, rax"));
                        match expr_types {
                            Type::Integer => {
                                instructions.push(Box::from("\tpop rdx"));
                                instructions.push(Box::from("\tcmp rdx, rdi"));
                            }
                            Type::CharacterChain(_) => {
                                instructions.push(Box::from("\tpop rsi"));
                                instructions.push(Box::from("\tcall strcmp"));
                                instructions.push(Box::from("\tcmp rax, 0"));
                            }
                            _ => unreachable!(),
                        }
                        instructions.push(format!("\t{inst} al").into_boxed_str());
                        instructions.push(Box::from("\tsub rsp, 1"));
                        instructions.push(Box::from("\tmov byte ptr [rsp], al"));
                    }
                    Type::Character | Type::Boolean => {
                        if expr_types == Type::Boolean
                            && !matches!(
                                expression.token,
                                ExprToken::Op(
                                    OperationToken::Equality | OperationToken::Inequality
                                )
                            )
                        {
                            return Err(GenerationError::OperationNotSupportedByType);
                        }
                        instructions.pop();
                        instructions.pop();
                        instructions.push(Box::from("\tmov dil, byte ptr [rsp]"));
                        instructions.push(Box::from("\tadd rsp, 1"));
                        instructions.push(Box::from("\tcmp dil, al"));
                        instructions.push(format!("\t{inst} al").into_boxed_str());
                        instructions.push(Box::from("\tsub rsp, 1"));
                        instructions.push(Box::from("\tmov byte ptr [rsp], al"));
                    }
                    Type::Real => unimplemented!(),
                }
            }
            _ => return Err(GenerationError::OperationNotSupportedByType),
        },
        Type::Real => unimplemented!(),
    }
    Ok(())
}

fn generate_statement(
    statement: Statement,
    variables: &mut HashMap<Rc<str>, Variable>,
    instructions: &mut Vec<Box<str>>,
    program_data: &mut ProgramData,
) -> Result<(), GenerationError> {
    match statement {
        Statement::SingleInstruction(Instruction::Assign(ident, expression)) => {
            let var = variables
                .get_mut(&ident)
                .ok_or(GenerationError::IdentifierNotDeclared)?;
            if var.var_type == Type::CharacterChain(true) {
                var.var_type = Type::CharacterChain(false);
                instructions
                    .push(format!("\tmov rdi, qword ptr [rbp-{}]", var.offset).into_boxed_str());
                instructions.push(Box::from("\tcall free"));
            }
            let var = var.clone();
            generate_expression(
                expression,
                variables,
                instructions,
                program_data,
                var.var_type,
            )?;
            match var.var_type {
                Type::Real | Type::Integer | Type::CharacterChain(_) => {
                    instructions.pop();
                    instructions.push(
                        format!("\tmov qword ptr [rbp-{}], rax", var.offset).into_boxed_str(),
                    );
                }
                Type::Boolean | Type::Character => {
                    instructions.pop();
                    instructions.pop();
                    instructions
                        .push(format!("\tmov byte ptr [rbp-{}], al", var.offset).into_boxed_str());
                }
            }
        }
        Statement::SingleInstruction(Instruction::Write(tokens, line)) => {
            let mut buffer = String::new();
            let mut vars_to_write: Vec<Variable> = Vec::new();
            for token in tokens {
                match token {
                    ValueToken::IntLiteral(x) => buffer.push_str(&x.to_string()),
                    ValueToken::RealLiteral(x) => buffer.push_str(&x.to_string()),
                    ValueToken::StringLiteral(lit) => buffer.push_str(&lit),
                    ValueToken::Identifier(ident) => {
                        let var = variables
                            .get(&ident)
                            .ok_or(GenerationError::IdentifierNotDeclared)?;
                        buffer.push_str(match var.var_type {
                            Type::Real => "%lf",
                            Type::Integer => "%ld",
                            Type::Character => "%c",
                            Type::CharacterChain(_) => "%s",
                            Type::Boolean => "%s",
                        });
                        vars_to_write.push(var.clone());
                    }
                    ValueToken::True => buffer.push_str("verdadeiro"),
                    ValueToken::False => buffer.push_str("falso"),
                }
            }
            if line {
                buffer.push_str("\\n");
            }
            let i = program_data.add_string(buffer.into_boxed_str());
            instructions.push(format!("\tlea rdi, [str{i}]").into_boxed_str());
            let mut leftover: Vec<Variable> = Vec::new();
            let mut fp_passed = 0usize;
            let mut normal_passed = 0usize;
            for var in vars_to_write {
                match var.var_type {
                    Type::Integer | Type::CharacterChain(_) => {
                        if normal_passed < 5 {
                            normal_passed += 1;
                            instructions.push(
                                format!(
                                    "\tmov {}, qword ptr [rbp-{}]",
                                    REGISTERS[normal_passed], var.offset
                                )
                                .into_boxed_str(),
                            );
                        } else {
                            leftover.push(var);
                        }
                    }
                    Type::Boolean => {
                        if normal_passed < 5 {
                            normal_passed += 1;
                            program_data.bool_str = true;
                            instructions.push(Box::from("\tlea r10, [verdadeiro]"));
                            instructions.push(
                                format!("\tlea {}, [falso]", REGISTERS[normal_passed])
                                    .into_boxed_str(),
                            );
                            instructions.push(Box::from("\txor rax, rax"));
                            instructions.push(
                                format!("\tmov al, byte ptr [rbp-{}]", var.offset).into_boxed_str(),
                            );
                            instructions.push(Box::from("\tcmp rax, 0"));
                            instructions.push(
                                format!("\tcmovne {}, r10", REGISTERS[normal_passed])
                                    .into_boxed_str(),
                            );
                        } else {
                            leftover.push(var);
                        }
                    }
                    Type::Real => {
                        if fp_passed < 7 {
                            instructions.push(
                                format!("\tmovsd xmm{fp_passed}, qword ptr [rbp-{}]", var.offset)
                                    .into_boxed_str(),
                            );
                            fp_passed += 1;
                        } else {
                            leftover.push(var);
                        }
                    }
                    Type::Character => {
                        if normal_passed < 5 {
                            normal_passed += 1;
                            instructions.push(
                                format!(
                                    "\tmov {}, byte ptr [rbp-{}]",
                                    REGISTERS_8[normal_passed], var.offset
                                )
                                .into_boxed_str(),
                            );
                        } else {
                            leftover.push(var);
                        }
                    }
                }
            }
            leftover.reverse();
            for var in leftover {
                match var.var_type {
                    Type::Integer | Type::Real | Type::CharacterChain(_) => {
                        instructions.push(
                            format!("mov rax, qword ptr [rbp-{}]", var.offset).into_boxed_str(),
                        );
                        instructions.push(Box::from("\tpush rax"));
                    }
                    Type::Boolean => {
                        program_data.bool_str = true;
                        instructions.push(Box::from("\tlea r10, [verdadeiro]"));
                        instructions.push(format!("\tlea r11, [falso]").into_boxed_str());
                        instructions.push(Box::from("\txor rax, rax"));
                        instructions.push(
                            format!("\tmov al, byte ptr [rbp-{}]", var.offset).into_boxed_str(),
                        );
                        instructions.push(Box::from("\tcmp rax, 0"));
                        instructions.push(format!("\tcmovne r11, r10").into_boxed_str());
                        instructions.push(Box::from("\tpush r11"));
                    }
                    Type::Character => {
                        instructions.push(
                            format!("mov al, byte ptr [rbp-{}]", var.offset).into_boxed_str(),
                        );
                        instructions.push(Box::from("\tsub rsp, 1"));
                        instructions.push(Box::from("\tmov byte ptr [rsp], al"));
                    }
                }
            }
            instructions.push(Box::from("\txor rax, rax"));
            instructions.push(Box::from("\tcall printf"));
        }
        Statement::SingleInstruction(Instruction::Read(idents)) => {
            idents
                .into_iter()
                .map(|ident| {
                    if let Some(var) = variables.get_mut(&ident) {
                        let i = program_data.add_string(Box::from(match var.var_type {
                            Type::Real => "%lf",
                            Type::Integer => "%ld",
                            Type::Character => "%c",
                            Type::CharacterChain(_) => "%s",
                            Type::Boolean => return Err(GenerationError::CannotReadBooleans),
                        }));
                        match var.var_type {
                            Type::CharacterChain(_) => {
                                if var.var_type == Type::CharacterChain(false) {
                                    var.var_type = Type::CharacterChain(true);
                                    instructions.push(Box::from("\tmov rdi, 256"));
                                    instructions.push(Box::from("\tcall malloc"));
                                    instructions.push(
                                        format!("\tmov qword ptr [rbp-{}], rax", var.offset)
                                            .into_boxed_str(),
                                    );
                                    instructions.push(Box::from("\tmov rsi, rax"));
                                } else {
                                    instructions.push(
                                        format!("\tmov rsi, qword ptr [rbp-{}]", var.offset)
                                            .into_boxed_str(),
                                    );
                                }
                            }
                            _ => instructions
                                .push(format!("\tlea rsi, [rbp-{}]", var.offset).into_boxed_str()),
                        }
                        instructions.push(format!("\tlea rdi, [str{i}]").into_boxed_str());
                        instructions.push(Box::from("\txor rax, rax"));
                        instructions.push(Box::from("\tpush rbp"));
                        instructions.push(Box::from("\tmov rbp, rsp"));
                        instructions.push(Box::from("\tcall scanf"));
                        instructions.push(Box::from("\tpop rbp"));
                        Ok(())
                    } else {
                        Err(GenerationError::CouldntInferType)
                    }
                })
                .collect::<Result<(), GenerationError>>()?;
        }
        Statement::IfStatement(condition, if_stat, else_stat) => {
            generate_expression(
                condition,
                variables,
                instructions,
                program_data,
                Type::Boolean,
            )?;
            instructions.pop();
            instructions.pop();
            instructions.push(Box::from("\ttest al, al"));
            let else_branch = if else_stat.is_some() {
                let n = program_data.branches;
                program_data.branches += 1;
                Some(n)
            } else {
                None
            };
            let end_branch = program_data.branches;
            program_data.branches += 1;
            instructions
                .push(format!("\tje B{}", else_branch.unwrap_or(end_branch)).into_boxed_str());
            for stat in if_stat {
                generate_statement(stat, variables, instructions, program_data)?;
            }
            if else_stat.is_some() {
                instructions.push(format!("\tjmp B{end_branch}").into_boxed_str());
            }
            if let Some(else_stat) = else_stat {
                instructions.push(
                    format!("B{}:", unsafe { else_branch.unwrap_unchecked() }).into_boxed_str(),
                );
                for stat in else_stat {
                    generate_statement(stat, variables, instructions, program_data)?;
                }
            }
            instructions.push(format!("B{}:", end_branch).into_boxed_str());
        }
        Statement::WhileStatement(condition, while_stat) => {
            let start_branch = program_data.branches;
            let cond_branch = program_data.branches + 1;
            program_data.branches += 2;
            instructions.push(format!("\tjmp B{cond_branch}").into_boxed_str());
            instructions.push(format!("B{start_branch}:").into_boxed_str());
            for stat in while_stat {
                generate_statement(stat, variables, instructions, program_data)?;
            }
            instructions.push(format!("B{cond_branch}:").into_boxed_str());
            generate_expression(
                condition,
                variables,
                instructions,
                program_data,
                Type::Boolean,
            )?;
            instructions.pop();
            instructions.pop();
            instructions.push(Box::from("\ttest al, al"));
            instructions.push(format!("\tjne B{start_branch}").into_boxed_str());
        }
        Statement::DoWhileStatement(condition, do_while_stat) => {
            let start_branch = program_data.branches;
            program_data.branches += 1;
            instructions.push(format!("B{start_branch}:").into_boxed_str());
            for stat in do_while_stat {
                generate_statement(stat, variables, instructions, program_data)?;
            }
            generate_expression(
                condition,
                variables,
                instructions,
                program_data,
                Type::Boolean,
            )?;
            instructions.pop();
            instructions.pop();
            instructions.push(Box::from("\ttest al, al"));
            instructions.push(format!("\tjne B{start_branch}").into_boxed_str());
        }
        Statement::ForStatement(ident, start, end, step, for_stat) => {
            let var = variables
                .get(&ident)
                .ok_or(GenerationError::IdentifierNotDeclared)?;
            if var.var_type != Type::Integer {
                return Err(GenerationError::TypeError);
            }
            let offset = var.offset;
            generate_expression(end, variables, instructions, program_data, Type::Integer)?;
            generate_expression(step, variables, instructions, program_data, Type::Integer)?;
            let start_branch = program_data.branches;
            let cond_branch = program_data.branches + 1;
            program_data.branches += 2;
            generate_expression(start, variables, instructions, program_data, Type::Integer)?;
            instructions.pop();
            instructions.push(format!("\tjmp B{cond_branch}").into_boxed_str());
            instructions.push(format!("B{start_branch}:").into_boxed_str());
            instructions.push(format!("\tmov qword ptr [rbp-{offset}], rax").into_boxed_str());
            for stat in for_stat {
                generate_statement(stat, variables, instructions, program_data)?;
            }
            instructions.push(format!("\tmov rax, qword ptr [rbp-{offset}]").into_boxed_str());
            instructions.push(Box::from("\tadd rax, qword ptr [rsp]"));
            instructions.push(format!("B{cond_branch}:").into_boxed_str());
            instructions.push(Box::from("\tcmp rax, qword ptr [rsp+8]"));
            instructions.push(format!("\tjle B{start_branch}").into_boxed_str());
            instructions.push(Box::from("\tadd rsp, 16"));
        }
    }
    Ok(())
}

struct ProgramData {
    strings: Vec<Box<str>>,
    bool_str: bool,
    pow: bool,
    branches: usize,
}

impl ProgramData {
    fn add_string(&mut self, str: Box<str>) -> usize {
        let pos = self.strings.iter().position(|x| &**x == &*str);
        pos.unwrap_or_else(|| {
            let pos = self.strings.len();
            self.strings.push(str);
            pos
        })
    }
}

pub fn generate(program: Program) -> Result<Vec<Box<str>>, GenerationError> {
    let mut variables = program.variables;

    let mut program_data = ProgramData {
        strings: Vec::new(),
        bool_str: false,
        pow: false,
        branches: 0,
    };

    let mut instructions: Vec<Box<str>> = vec![
        Box::from(".global _start"),
        Box::from(".intel_syntax noprefix"),
        Box::from(".extern printf scanf strcmp malloc free"),
        Box::from("_start:"),
        Box::from("\tpush rbp"),
        Box::from("\tmov rbp, rsp"),
        format!("\tsub rsp, {}", program.stack_size).into_boxed_str(),
    ];

    for statement in program.statements {
        generate_statement(
            statement,
            &mut variables,
            &mut instructions,
            &mut program_data,
        )?;
    }

    for (_, var) in variables {
        if let Type::CharacterChain(true) = var.var_type {
            instructions
                .push(format!("\tmov rdi, qword ptr [rbp-{}]", var.offset).into_boxed_str());
            instructions.push(Box::from("\tcall free"));
        }
    }

    instructions.push(Box::from("\tleave"));
    instructions.push(Box::from("\tmov rax, 60"));
    instructions.push(Box::from("\txor rdi, rdi"));
    instructions.push(Box::from("\tsyscall"));

    for (i, string) in program_data.strings.into_iter().enumerate() {
        instructions.push(format!("str{i}:").into_boxed_str());
        instructions
            .push(format!("\t.asciz \"{}\"", string.replace("\"", "\\\"")).into_boxed_str());
    }

    if program_data.bool_str {
        instructions.push(Box::from("verdadeiro:"));
        instructions.push(Box::from("\t.asciz \"verdadeiro\""));
        instructions.push(Box::from("falso:"));
        instructions.push(Box::from("\t.asciz \"falso\""));
    }

    if program_data.pow {
        instructions.push(Box::from("pow:"));
        instructions.push(Box::from("\txor rax, rax"));
        instructions.push(Box::from("\tcmp rsi, 0"));
        instructions.push(Box::from("\tjl r_pow"));
        instructions.push(Box::from("\tmov rax, 1"));
        instructions.push(Box::from("l_pow:"));
        instructions.push(Box::from("\tcmp rsi, 0"));
        instructions.push(Box::from("\tje r_pow"));
        instructions.push(Box::from("\timul rax, rdi"));
        instructions.push(Box::from("\tdec rsi"));
        instructions.push(Box::from("\tjmp l_pow"));
        instructions.push(Box::from("r_pow:"));
        instructions.push(Box::from("\tret"));
    }

    Ok(instructions)
}
