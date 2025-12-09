use proc_macro2::{ TokenStream};
use syn::{ExprClosure, Expr, ExprField, ExprBinary, ExprLit, ExprMethodCall, Lit, Type, TypePath};
use quote::{quote};

// Fonction pour transformer le type
fn model_to_schema(model_type: &Box<Type>) -> Result<Type, String> {
    if let Type::Path(TypePath { path, .. }) = &**model_type {
        let mut new_path = path.clone();
        if let Some(last) = new_path.segments.last_mut() {
            last.ident = syn::Ident::new(&format!("{}Schema", last.ident), last.ident.span());
        }
        Ok(Type::Path(TypePath { path: new_path, qself: None }))
    } else {
        Err("Type is not a TypePath".to_string())
    }
}
fn from_expr_field(expr: &ExprField,param_name: &str,schema_type: &Type) -> Result<TokenStream, String>
{
    if let Expr::Path(base_path) = &*expr.base {
        if let Some(ident) = base_path.path.get_ident() {
            if ident.to_string() == param_name {
                let field = match &expr.member {
                    syn::Member::Named(name) => name,
                    syn::Member::Unnamed(_index) =>
                        Err(format!("{:?} Unnamed field not managed {}",expr,param_name))?,
                };

                return Ok( quote! {  welds::query::clause::AsFieldName::colname(& #schema_type::default().#field).clone() });
            }
            else{
                Err(format!("{:?} must be a field of the parameter {}",expr,param_name))?
            }
        }
    };
    Err(format!("Can only manage Expr::Path and not {:?}",expr.base))
}

fn from_expr_binary(expr_binary: &ExprBinary,param_name: &str,schema_type: &Type) -> Result<TokenStream, String>
{
    let left = &expr_binary.left;
    let right = &expr_binary.right;

    let left_cond = from_expr(left, param_name,schema_type)?;
    let right_cond = from_expr(right, param_name,schema_type)?;

    let op = &expr_binary.op;

    let result= match op {
        // Logical operators
        syn::BinOp::And(_) => quote! { welds::query::clause::ConditionInfo::and(#left_cond, #right_cond) },
        syn::BinOp::Or(_) => quote! { welds::query::clause::ConditionInfo::or(#left_cond, #right_cond) },
        // Comparison operators
        syn::BinOp::Eq(_) => quote! { welds::query::clause::ConditionInfo::binary(#left_cond, welds::query::clause::BinaryOperator::Equal, #right_cond) },
        syn::BinOp::Ne(_) => quote! { welds::query::clause::ConditionInfo::binary(#left_cond, welds::query::clause::BinaryOperator::NotEqual, #right_cond) },
        syn::BinOp::Gt(_) => quote! { welds::query::clause::ConditionInfo::binary(#left_cond, welds::query::clause::BinaryOperator::GreaterThan, #right_cond) },
        syn::BinOp::Lt(_) => quote! { welds::query::clause::ConditionInfo::binary(#left_cond, welds::query::clause::BinaryOperator::LessThan, #right_cond) },
        syn::BinOp::Ge(_) => quote! { welds::query::clause::ConditionInfo::binary(#left_cond, welds::query::clause::BinaryOperator::GreaterThanOrEqual, #right_cond) },
        syn::BinOp::Le(_) => quote! { welds::query::clause::ConditionInfo::binary(#left_cond, welds::query::clause::BinaryOperator::LessThanOrEqual, #right_cond) },
        _ => Err(format!("Binary operation not supported: {:?}", quote!(#op)))?,
    };

    Ok(result)
}

fn from_expr_lit(expr_lit: &ExprLit) -> Result<TokenStream, String>
{
    match &expr_lit.lit {
        Lit::Int(value) => Ok( quote! { #value } ),
        Lit::Str(value) => Ok( quote! { #value } ),
        Lit::Float(value) => Ok( quote! { #value } ),
        Lit::Bool(value) => Ok( quote! { #value } ),
        _ => Err(format!("Unsupported literal type: {:?}", quote!(#expr_lit))),
    }
}

fn from_expr_method(expr_method: &ExprMethodCall, param_name: &str,schema_type: &Type) -> Result<TokenStream, String>
{
    // Récupérer le receveur (l'objet sur lequel la méthode est appelée)
    let receiver = from_expr(&expr_method.receiver, param_name, schema_type)?;

    // Récupérer le nom de la méthode
    let method_name = &expr_method.method;

    Ok(match method_name.to_string().as_str() {
        "is_some" => {
            if expr_method.args.len() != 0 {
                return Err(format!("Method 'contains' expects no argument, found {}", expr_method.args.len()));
            }
            quote! { welds::query::clause::ConditionInfo::unary( welds::query::clause::UnaryOperator::IsSome, #receiver) }},
        "is_none" => {
            if expr_method.args.len() != 0 {
                return Err(format!("Method 'contains' expects no argument, found {}", expr_method.args.len()));
            }
            quote! { welds::query::clause::ConditionInfo::unary( welds::query::clause::UnaryOperator::IsNone, #receiver) }},
        "contains" =>{
            if expr_method.args.len() != 1 {
                return Err(format!("Method 'contains' expects exactly one argument, found {}", expr_method.args.len()));
            }
            let arg = from_expr(&expr_method.args[0], param_name, schema_type)?;
            quote! { welds::query::clause::ConditionInfo::binary( #receiver, welds::query::clause::BinaryOperator::Like, format!("'%{}%'",#arg)) }
        },
        _ => quote!{#receiver.#method_name(format!("Unsupported method: {}", #expr_method)) },
    })
}

pub fn from_expr(expr: &Expr,param_name: &str,schema_type: &Type)-> Result<TokenStream, String> {

    match expr {
        Expr::Binary(expr_binary) => from_expr_binary(expr_binary,param_name,schema_type),
        Expr::Field(field_expr) => from_expr_field(field_expr,param_name,schema_type),
        Expr::Paren(expr_paren) => from_expr(&expr_paren.expr, param_name,schema_type),
        Expr::Lit(expr_lit) => from_expr_lit(expr_lit),
        Expr::MethodCall(expr_method) => from_expr_method(expr_method,param_name,schema_type),
        _ => Err(format!("Unsupported expression type {:?}  for: {:?}",expr, quote!(#expr))),
    }
}

pub fn do_condition(input: TokenStream) -> Result<TokenStream, String> {

    let closure: ExprClosure = syn::parse2(input).map_err(|e| e.to_string())?;

    if let Some(first_param) = closure.inputs.first() {
        if let syn::Pat::Type(pat_type) = first_param {
            let param_name_str = if let syn::Pat::Ident(ident) = &*pat_type.pat {
                ident.ident.to_string()
            } else {
                "param".to_string()
            };

            let model_type = &pat_type.ty;
            let schema_type = model_to_schema(model_type)?;

            let body = from_expr(&closure.body,&param_name_str,&schema_type)?;

            Ok(quote! { |_: #schema_type| { Box::new( #body) } })
        }
        else {
            Err("Closure must have an explicit parameter type".to_string())
        }
    }
    else {
        Err("Closure must have a parameter".to_string())
    }
}
