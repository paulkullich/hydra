{-# LANGUAGE ScopedTypeVariables #-}

-- | Rust code generator in Hydra DSL.
-- This module provides DSL versions of Rust code generation functions.
-- Type definitions are mapped to structs/enums/newtypes; term definitions are mapped to functions.

module Hydra.Sources.Rust.Coder where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Core.File (_FileExtension)
import           Hydra.Core.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Core.Overlay.Haskell.Libraries
import qualified Hydra.Core.Dsl.Lib.Strings                as Strings
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms                   as Phantoms
import qualified Hydra.Core.Dsl.Lib.Eithers                as Eithers
import qualified Hydra.Core.Dsl.Lib.Equality               as Equality
import qualified Hydra.Core.Dsl.Lib.Lists                  as Lists
import qualified Hydra.Core.Dsl.Lib.Logic                  as Logic
import qualified Hydra.Core.Dsl.Lib.Maps                   as Maps
import qualified Hydra.Core.Dsl.Lib.Optionals                 as Optionals
import qualified Hydra.Core.Dsl.Lib.Pairs                  as Pairs
import qualified Hydra.Core.Dsl.Lib.Literals               as Literals
import qualified Hydra.Core.Dsl.Lib.Sets                   as Sets
import qualified Hydra.Core.Dsl.Coders                     as Coders
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core                       as Core
import qualified Hydra.Core.Dsl.Errors                      as Error
import qualified Hydra.Core.Dsl.Packaging                     as Packaging
import qualified Hydra.Core.Dsl.Typing                      as Typing
import qualified Hydra.Core.Typing                          as TermTyping
import qualified Hydra.Core.Dsl.Util                       as Util
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Scoping        as Scoping
import qualified Hydra.Sources.Kernel.Terms.Strip          as Strip
import qualified Hydra.Sources.Kernel.Terms.Variables      as Variables
import qualified Hydra.Sources.Kernel.Terms.Environment   as Environment
import qualified Hydra.Sources.Kernel.Terms.Serialization  as SerializationSource
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports for Rust AST
import qualified Hydra.Rust.Syntax as R
import qualified Hydra.Sources.Rust.Syntax as RustSyntax
import qualified Hydra.Sources.Rust.Serde as RustSerdeSource
import qualified Hydra.Sources.Rust.Language as RustLanguageSource


ns :: ModuleName
ns = ModuleName "hydra.rust.coder"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([moduleName RustSerdeSource.module_, moduleName RustLanguageSource.module_,
      Formatting.ns, Names.ns, Strip.ns, Variables.ns, Environment.ns, Lexical.ns, Scoping.ns, SerializationSource.ns] L.++ (RustSyntax.ns:KernelTypes.kernelTypesModuleNames)),
            moduleMetadata = descriptionMetadata (Just "Rust code generator: converts Hydra type and term modules to Rust source code")}
  where
    definitions = [
      toDefinition applicationSpine,
      toDefinition encodeApplied,
      toDefinition encodeCurriedCall,
      toDefinition encodeEnumVariant,
      toDefinition encodeLiteral,
      toDefinition encodeLiteralType,
      toDefinition encodeProjectionElim,
      toDefinition encodeStructField,
      toDefinition encodeIntegerPrimitive,
      toDefinition encodeNamedCall,
      toDefinition encodeParameter,
      toDefinition encodeTerm,
      toDefinition encodeTermDefinition,
      toDefinition encodeType,
      toDefinition encodeTypeDefinition,
      toDefinition encodeUnionElim,
      toDefinition encodeUnwrapElim,
      toDefinition moduleToRust,
      toDefinition rustApply1,
      toDefinition rustApply2,
      toDefinition rustBlock,
      toDefinition rustCall,
      toDefinition rustClosure,
      toDefinition rustExprPath,
      toDefinition rustLetStmt,
      toDefinition rustPath,
      toDefinition rustPathSegmented,
      toDefinition rustUnit,
      toDefinition standardDerives,
      toDefinition supportItems]

-- =============================================================================
-- Standard derives
-- =============================================================================

def :: String -> TypedTerm a -> TypedTermDefinition a
def = definitionInModule module_

-- Haskell helpers below only build DSL terms. The generated coder inlines them.

cloned :: TypedTerm R.Expression -> TypedTerm R.Expression
cloned expr =
  inject R._Expression R._Expression_methodCall $
    record R._MethodCallExpr [
      R._MethodCallExpr_receiver>>: expr,
      R._MethodCallExpr_method>>: string "clone",
      R._MethodCallExpr_turbofish>>: list ([] :: [TypedTerm R.Type]),
      R._MethodCallExpr_args>>: list ([] :: [TypedTerm R.Expression])]

rustBinary :: TypedTerm R.Expression -> TypedTerm R.BinaryOp -> TypedTerm R.Expression -> TypedTerm R.Expression
rustBinary left op right =
  inject R._Expression R._Expression_binary $
    record R._BinaryExpr [
      R._BinaryExpr_left>>: left,
      R._BinaryExpr_op>>: op,
      R._BinaryExpr_right>>: right]

ifExpr :: TypedTerm R.Expression -> TypedTerm R.Expression -> TypedTerm R.Expression -> TypedTerm R.Expression
ifExpr cond thenExpr elseExpr =
  inject R._Expression R._Expression_if $
    record R._IfExpr [
      R._IfExpr_condition>>: inject R._IfCondition R._IfCondition_bool cond,
      R._IfExpr_thenBlock>>: record R._Block [
        R._Block_statements>>: list ([] :: [TypedTerm R.Statement]),
        R._Block_expression>>: just thenExpr],
      -- Rust requires the else branch to be a block or another if.
      R._IfExpr_elseBranch>>: just (rustBlock @@ list ([] :: [TypedTerm R.Statement]) @@ elseExpr)]

panicStuck :: TypedTerm R.Expression
panicStuck =
  inject R._Expression R._Expression_macro $
    record R._MacroInvocation [
      R._MacroInvocation_path>>: list [string "panic"],
      R._MacroInvocation_delimiter>>: inject R._MacroDelimiter R._MacroDelimiter_paren unit,
      R._MacroInvocation_tokens>>: string "\"stuck\""]

localSnake :: TypedTerm Name -> TypedTerm String
localSnake name =
  Formatting.convertCaseCamelToLowerSnake @@
    (Formatting.sanitizeWithUnderscores @@ RustLanguageSource.rustReservedWords @@
      (Names.localNameOf @@ name))

-- Drop type arguments before the local name, and map the prelude types.
rustNominal :: TypedTerm Name -> TypedTerm String
rustNominal name =
  Optionals.match (Lists.head (Strings.splitOn (string "<") (Core.unName name)))
    (string "Unit")
    (lambda "headName" $
      Logic.ifElse (Equality.equal (var "headName") (string "integers.Order"))
        (string "IntegersOrder")
        (Logic.ifElse (Equality.equal (var "headName") (string "either"))
          (string "Either")
          (Formatting.capitalize @@ (Names.localNameOf @@ wrap _Name (var "headName")))))

genericParam :: String -> TypedTerm R.GenericParam
genericParam gname =
  record R._GenericParam [
    R._GenericParam_name>>: string gname,
    R._GenericParam_bounds>>: list ([] :: [TypedTerm R.TypeParamBound])]

unitVariant :: String -> TypedTerm R.EnumVariant
unitVariant vname =
  record R._EnumVariant [
    R._EnumVariant_name>>: string vname,
    R._EnumVariant_body>>: inject R._EnumVariantBody R._EnumVariantBody_unit unit,
    R._EnumVariant_doc>>: nothing]

tupleVariant :: String -> TypedTerm R.Type -> TypedTerm R.EnumVariant
tupleVariant vname typ =
  record R._EnumVariant [
    R._EnumVariant_name>>: string vname,
    R._EnumVariant_body>>: inject R._EnumVariantBody R._EnumVariantBody_tuple $ list [typ],
    R._EnumVariant_doc>>: nothing]

pubEnum :: TypedTerm String -> TypedTerm [R.GenericParam] -> TypedTerm [R.EnumVariant] -> TypedTerm R.ItemWithComments
pubEnum ename generics variants =
  record R._ItemWithComments [
    R._ItemWithComments_doc>>: nothing,
    R._ItemWithComments_visibility>>: inject R._Visibility R._Visibility_public unit,
    R._ItemWithComments_item>>: inject R._Item R._Item_enum $
      record R._EnumDef [
        R._EnumDef_name>>: ename,
        R._EnumDef_generics>>: generics,
        R._EnumDef_whereClause>>: nothing,
        R._EnumDef_variants>>: variants,
        R._EnumDef_derives>>: asTerm standardDerives,
        R._EnumDef_public>>: boolean True,
        R._EnumDef_doc>>: nothing]]

-- | Encode a Hydra union field as a Rust enum variant
encodeEnumVariant :: TypedTermDefinition (InferenceContext -> Graph -> FieldType -> Either Error R.EnumVariant)
encodeEnumVariant = def "encodeEnumVariant" $
  doc "Encode a Hydra union field as a Rust enum variant" $
  "cx" ~> "g" ~> lambda "ft" $
    "fname" <~ Core.unName (Core.fieldTypeName (var "ft")) $
    "ftyp" <~ Core.fieldTypeType (var "ft") $
    "dtyp" <~ (Strip.deannotateType @@ var "ftyp") $
    "isUnit" <~ (match _Type (var "dtyp") (Just $ boolean False) [
      _Type_unit>>: constant $ boolean True,
      _Type_record>>: lambda "rt" $
        Lists.isEmpty (var "rt")]) $
    Logic.ifElse (var "isUnit")
      -- Unit variant
      (right (record R._EnumVariant [
        R._EnumVariant_name>>: Formatting.capitalize @@ var "fname",
        R._EnumVariant_body>>: inject R._EnumVariantBody R._EnumVariantBody_unit unit,
        R._EnumVariant_doc>>: nothing]))
      -- Non-unit variant: check if it's a record (struct variant) or other (tuple variant)
      (match _Type (var "dtyp") (Just $
          -- Default: tuple variant with single element
          "sftyp" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "ftyp") $
            right (record R._EnumVariant [
              R._EnumVariant_name>>: Formatting.capitalize @@ var "fname",
              R._EnumVariant_body>>: inject R._EnumVariantBody R._EnumVariantBody_tuple $ list [var "sftyp"],
              R._EnumVariant_doc>>: nothing]))
        [_Type_record>>: lambda "rt" $
          "sfields" <<~ (Eithers.mapList (encodeStructField @@ var "cx" @@ var "g") (var "rt")) $
            right (record R._EnumVariant [
              R._EnumVariant_name>>: Formatting.capitalize @@ var "fname",
              R._EnumVariant_body>>: inject R._EnumVariantBody R._EnumVariantBody_struct (var "sfields"),
              R._EnumVariant_doc>>: nothing])])

-- =============================================================================
-- Type definition encoding
-- =============================================================================

-- | Encode a Hydra literal value as a Rust expression
encodeLiteral :: TypedTermDefinition (Literal -> R.Expression)
encodeLiteral = def "encodeLiteral" $
  doc "Encode a Hydra literal value as a Rust expression" $
  lambda "lit" $ match _Literal (var "lit") Nothing [
    _Literal_boolean>>: lambda "b" $
      inject R._Expression R._Expression_literal $
        inject R._Literal R._Literal_bool (var "b"),
    _Literal_string>>: lambda "s" $
      rustCall @@ (rustExprPath @@ string "String::from") @@ list [
        inject R._Expression R._Expression_literal $
          inject R._Literal R._Literal_string (var "s")],
    _Literal_float>>: lambda "fv" $
      match _FloatValue (var "fv") Nothing [
        _FloatValue_float32>>: lambda "f" $
          inject R._Expression R._Expression_literal $
            inject R._Literal R._Literal_float $
              record R._FloatLiteral [
                R._FloatLiteral_value>>: Literals.float32ToFloat64 (var "f"),
                R._FloatLiteral_suffix>>: just (string "f32")],
        _FloatValue_float64>>: lambda "f" $
          inject R._Expression R._Expression_literal $
            inject R._Literal R._Literal_float $
              record R._FloatLiteral [
                R._FloatLiteral_value>>: var "f",
                R._FloatLiteral_suffix>>: nothing]],
    _Literal_integer>>: lambda "iv" $
      match _IntegerValue (var "iv") Nothing [
        _IntegerValue_int8>>: lambda "i" $
          inject R._Expression R._Expression_literal $
            inject R._Literal R._Literal_integer $
              record R._IntegerLiteral [
                R._IntegerLiteral_value>>: Literals.int8ToBigint (var "i"),
                R._IntegerLiteral_suffix>>: just (string "i8")],
        _IntegerValue_int16>>: lambda "i" $
          inject R._Expression R._Expression_literal $
            inject R._Literal R._Literal_integer $
              record R._IntegerLiteral [
                R._IntegerLiteral_value>>: Literals.int16ToBigint (var "i"),
                R._IntegerLiteral_suffix>>: just (string "i16")],
        _IntegerValue_int32>>: lambda "i" $
          inject R._Expression R._Expression_literal $
            inject R._Literal R._Literal_integer $
              record R._IntegerLiteral [
                R._IntegerLiteral_value>>: Literals.int32ToBigint (var "i"),
                R._IntegerLiteral_suffix>>: just (string "i32")],
        _IntegerValue_int64>>: lambda "i" $
          inject R._Expression R._Expression_literal $
            inject R._Literal R._Literal_integer $
              record R._IntegerLiteral [
                R._IntegerLiteral_value>>: Literals.int64ToBigint (var "i"),
                R._IntegerLiteral_suffix>>: just (string "i64")],
        _IntegerValue_uint8>>: lambda "i" $
          inject R._Expression R._Expression_literal $
            inject R._Literal R._Literal_integer $
              record R._IntegerLiteral [
                R._IntegerLiteral_value>>: Literals.uint8ToBigint (var "i"),
                R._IntegerLiteral_suffix>>: just (string "u8")],
        _IntegerValue_uint16>>: lambda "i" $
          inject R._Expression R._Expression_literal $
            inject R._Literal R._Literal_integer $
              record R._IntegerLiteral [
                R._IntegerLiteral_value>>: Literals.uint16ToBigint (var "i"),
                R._IntegerLiteral_suffix>>: just (string "u16")],
        _IntegerValue_uint32>>: lambda "i" $
          inject R._Expression R._Expression_literal $
            inject R._Literal R._Literal_integer $
              record R._IntegerLiteral [
                R._IntegerLiteral_value>>: Literals.uint32ToBigint (var "i"),
                R._IntegerLiteral_suffix>>: just (string "u32")],
        _IntegerValue_uint64>>: lambda "i" $
          inject R._Expression R._Expression_literal $
            inject R._Literal R._Literal_integer $
              record R._IntegerLiteral [
                R._IntegerLiteral_value>>: Literals.uint64ToBigint (var "i"),
                R._IntegerLiteral_suffix>>: just (string "u64")],
        _IntegerValue_bigint>>: lambda "i" $
          inject R._Expression R._Expression_literal $
            inject R._Literal R._Literal_integer $
              record R._IntegerLiteral [
                R._IntegerLiteral_value>>: var "i",
                R._IntegerLiteral_suffix>>: nothing]]]

-- =============================================================================
-- Type encoding
-- =============================================================================

-- | Encode a Hydra literal type as a Rust type
encodeLiteralType :: TypedTermDefinition (LiteralType -> R.Type)
encodeLiteralType = def "encodeLiteralType" $
  doc "Encode a Hydra literal type as a Rust type" $
  lambda "lt" $ match _LiteralType (var "lt") Nothing [
    _LiteralType_binary>>: constant $
      rustApply1 @@ string "Vec" @@ (rustPath @@ string "u8"),
    _LiteralType_boolean>>: constant $
      rustPath @@ string "bool",
    _LiteralType_float>>: lambda "ft" $
      match _FloatType (var "ft") Nothing [
        _FloatType_float32>>: constant $ rustPath @@ string "f32",
        _FloatType_float64>>: constant $ rustPath @@ string "f64"],
    _LiteralType_integer>>: lambda "it" $
      match _IntegerType (var "it") Nothing [
        _IntegerType_bigint>>: constant $ rustPathSegmented @@ list [string "num", string "BigInt"],
        _IntegerType_int8>>: constant $ rustPath @@ string "i8",
        _IntegerType_int16>>: constant $ rustPath @@ string "i16",
        _IntegerType_int32>>: constant $ rustPath @@ string "i32",
        _IntegerType_int64>>: constant $ rustPath @@ string "i64",
        _IntegerType_uint8>>: constant $ rustPath @@ string "u8",
        _IntegerType_uint16>>: constant $ rustPath @@ string "u16",
        _IntegerType_uint32>>: constant $ rustPath @@ string "u32",
        _IntegerType_uint64>>: constant $ rustPath @@ string "u64"],
    _LiteralType_string>>: constant $
      rustPath @@ string "String"]

-- =============================================================================
-- Literal value encoding
-- =============================================================================

-- | Encode a Hydra record projection as a Rust expression.
-- Takes an optional argument for applied projections.
encodeProjectionElim :: TypedTermDefinition (InferenceContext -> Graph -> Projection -> Maybe Term -> Either Error R.Expression)
encodeProjectionElim = def "encodeProjectionElim" $
  doc "Encode a Hydra record projection as a Rust expression. Takes an optional argument for applied projections." $
  "cx" ~> "g" ~> lambda "proj" $ lambda "marg" $
        "fname" <~ (Formatting.convertCaseCamelToLowerSnake @@ Core.unName (Core.projectionFieldName (var "proj"))) $
        Optionals.match (var "marg")
          -- Unapplied projection: |v| v.field.clone()
          (right (rustClosure @@ nothing @@ list [string "v"] @@
            cloned (inject R._Expression R._Expression_fieldAccess $
              record R._FieldAccessExpr [
                R._FieldAccessExpr_object>>: rustExprPath @@ string "v",
                R._FieldAccessExpr_field>>: var "fname"])))
          (lambda "arg" $
            "sarg" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "arg") $
              right (cloned (inject R._Expression R._Expression_fieldAccess $
                record R._FieldAccessExpr [
                  R._FieldAccessExpr_object>>: var "sarg",
                  R._FieldAccessExpr_field>>: var "fname"])))

-- | Encode a Hydra record field as a Rust struct field
encodeStructField :: TypedTermDefinition (InferenceContext -> Graph -> FieldType -> Either Error R.StructField)
encodeStructField = def "encodeStructField" $
  doc "Encode a Hydra record field as a Rust struct field" $
  "cx" ~> "g" ~> lambda "ft" $
    "fname" <~ Core.unName (Core.fieldTypeName (var "ft")) $
    "ftyp" <~ Core.fieldTypeType (var "ft") $
    "sftyp" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "ftyp") $
      right (record R._StructField [
        R._StructField_name>>: Formatting.convertCaseCamelToLowerSnake @@ (Formatting.sanitizeWithUnderscores @@ RustLanguageSource.rustReservedWords @@ var "fname"),
        R._StructField_type>>: var "sftyp",
        R._StructField_public>>: boolean True,
        R._StructField_doc>>: nothing])

-- =============================================================================
-- Enum variant encoding
-- =============================================================================

-- | Encode a Hydra term as a Rust expression
encodeTerm :: TypedTermDefinition (InferenceContext -> Graph -> Term -> Either Error R.Expression)
encodeTerm = def "encodeTerm" $
  doc "Encode a Hydra term as a Rust expression" $
  "cx" ~> "g" ~> lambda "term" $
    match _Term (var "term") (Just $
      left (Error.errorOther $ Error.otherError $ string "unexpected term variant"))
    [_Term_annotated>>: lambda "at" $
       encodeTerm @@ var "cx" @@ var "g" @@ Core.annotatedTermBody (var "at"),
     _Term_application>>: lambda "app" $
       encodeApplied @@ var "cx" @@ var "g" @@ var "term",
     _Term_either>>: lambda "e" $
       Eithers.either
         (lambda "l" $
           "sl" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "l") $
             right (rustCall @@ (rustExprPath @@ string "Either::Left") @@ list [var "sl"]))
         (lambda "r" $
           "sr" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "r") $
             right (rustCall @@ (rustExprPath @@ string "Either::Right") @@ list [var "sr"]))
         (var "e"),
     _Term_lambda>>: lambda "lam" $
       "param" <~ localSnake (Core.lambdaParameter (var "lam")) $
       "body" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ Core.lambdaBody (var "lam")) $
       "ann" <<~ Optionals.match (Core.lambdaDomain (var "lam"))
         (right nothing)
         (lambda "dom" $
           "encoded" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "dom") $
             right (just (var "encoded"))) $
         right (rustClosure @@ var "ann" @@ list [var "param"] @@ var "body"),
     _Term_project>>: lambda "proj" $
       encodeProjectionElim @@ var "cx" @@ var "g" @@ var "proj" @@ nothing,
     _Term_cases>>: lambda "cs" $
       encodeUnionElim @@ var "cx" @@ var "g" @@ var "cs" @@ nothing,
     _Term_unwrap>>: lambda "name" $
       encodeUnwrapElim @@ var "cx" @@ var "g" @@ var "name" @@ nothing,
     _Term_let>>: lambda "lt" $
       "bindings" <~ Core.letBindings (var "lt") $
       "body" <~ Core.letBody (var "lt") $
       "stmts" <<~ (Eithers.mapList
         (lambda "b" $
           "bname" <~ (Formatting.convertCaseCamelToLowerSnake @@ Core.unName (Core.bindingName (var "b"))) $
           "bval" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ Core.bindingTerm (var "b")) $
             right (rustLetStmt @@ var "bname" @@ var "bval"))
         (var "bindings")) $
       "bodyExpr" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "body") $
         right (rustBlock @@ var "stmts" @@ var "bodyExpr"),
     _Term_list>>: lambda "els" $
       "sels" <<~ (Eithers.mapList (encodeTerm @@ var "cx" @@ var "g") (var "els")) $
         right (rustCall @@ (rustExprPath @@ string "Vec::from") @@
           list [inject R._Expression R._Expression_array $
             inject R._ArrayExpr R._ArrayExpr_elements (var "sels")]),
     _Term_literal>>: lambda "lit" $
       right (encodeLiteral @@ var "lit"),
     _Term_map>>: lambda "m" $
       "pairs" <<~ (Eithers.mapList
         (lambda "entry" $
           "k" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ Pairs.first (var "entry")) $
           "v" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ Pairs.second (var "entry")) $
             right (inject R._Expression R._Expression_tuple $ list [var "k", var "v"]))
         (Maps.toList (var "m" :: TypedTerm (M.Map Term Term)))) $
         right (rustCall @@ (rustExprPath @@ string "BTreeMap::from") @@
           list [inject R._Expression R._Expression_array $
             inject R._ArrayExpr R._ArrayExpr_elements (var "pairs")]),
     _Term_optional>>: lambda "mt" $
       Optionals.match (var "mt")
         (right (rustExprPath @@ string "None"))
         (lambda "val" $
           "sval" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "val") $
             right (rustCall @@ (rustExprPath @@ string "Some") @@ list [var "sval"])),
     _Term_pair>>: lambda "p" $
       "f" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ Pairs.first (var "p")) $
       "s" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ Pairs.second (var "p")) $
         right (inject R._Expression R._Expression_tuple $ list [var "f", var "s"]),
     _Term_record>>: lambda "rec" $
       "rname" <~ Core.recordTypeName (var "rec") $
       "fields" <~ Core.recordFields (var "rec") $
       "sfields" <<~ (Eithers.mapList
         (lambda "f" $
           "fname" <~ (Formatting.convertCaseCamelToLowerSnake @@ Core.unName (Core.fieldName (var "f"))) $
           "fval" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ Core.fieldTerm (var "f")) $
             right (record R._FieldValue [
               R._FieldValue_name>>: var "fname",
               R._FieldValue_value>>: just (var "fval")]))
         (var "fields")) $
         right (inject R._Expression R._Expression_struct $
           record R._StructExpr [
             R._StructExpr_path>>:
               record R._ExprPath [
                 R._ExprPath_global>>: boolean False,
                 R._ExprPath_segments>>: list [
                   record R._PathSegment [
                     R._PathSegment_name>>: Formatting.capitalize @@ (Names.localNameOf @@ var "rname"),
                     R._PathSegment_arguments>>: inject R._GenericArguments R._GenericArguments_none unit]]],
             R._StructExpr_fields>>: var "sfields",
             R._StructExpr_rest>>: nothing]),
     _Term_set>>: lambda "s" $
       "sels" <<~ (Eithers.mapList (encodeTerm @@ var "cx" @@ var "g") (Sets.toList (var "s"))) $
         right (rustCall @@ (rustExprPath @@ string "BTreeSet::from") @@
           list [inject R._Expression R._Expression_array $
             inject R._ArrayExpr R._ArrayExpr_elements (var "sels")]),
     _Term_inject>>: lambda "inj" $
       "tname" <~ rustNominal (Core.injectionTypeName (var "inj")) $
       "field" <~ Core.injectionField (var "inj") $
       "fname" <~ (Formatting.capitalize @@ Core.unName (Core.fieldName (var "field"))) $
       "fterm" <~ Core.fieldTerm (var "field") $
       "dterm" <~ (Strip.deannotateTerm @@ var "fterm") $
       "isUnit" <~ (match _Term (var "dterm") (Just $ boolean False) [
         _Term_unit>>: constant $ boolean True,
         _Term_record>>: lambda "rt" $ Lists.isEmpty (Core.recordFields (var "rt"))]) $
       -- either<unit, _> still carries a unit payload. A nominal unit variant does not.
       "nominalUnit" <~ Logic.and (var "isUnit") (Logic.not (Equality.equal (var "tname") (string "Either"))) $
       Logic.ifElse (var "nominalUnit")
         (right (rustExprPath @@ Strings.concat2 (Strings.concat2 (var "tname") (string "::")) (var "fname")))
         ("sval" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "fterm") $
           right (rustCall @@ (rustExprPath @@ Strings.concat2 (Strings.concat2 (var "tname") (string "::")) (var "fname")) @@ list [var "sval"])),
     _Term_unit>>: constant $
       right (inject R._Expression R._Expression_tuple $ list ([] :: [TypedTerm R.Expression])),
     _Term_variable>>: lambda "name" $
       right (cloned (rustExprPath @@ localSnake (var "name"))),
     _Term_wrap>>: lambda "wt" $
       "tname" <~ (Formatting.capitalize @@ (Names.localNameOf @@ Core.wrappedTermTypeName (var "wt"))) $
       "inner" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ Core.wrappedTermBody (var "wt")) $
         right (rustCall @@ (rustExprPath @@ var "tname") @@ list [var "inner"])]

-- =============================================================================
-- Elimination encoding
-- =============================================================================

-- | Encode a Hydra term definition as a Rust function item
encodeTermDefinition :: TypedTermDefinition (InferenceContext -> Graph -> TermDefinition -> Either Error R.ItemWithComments)
encodeTermDefinition = def "encodeTermDefinition" $
  doc "Encode a Hydra term definition as a Rust function item" $
  "cx" ~> "g" ~> lambda "tdef" $
    "name" <~ Packaging.termDefinitionName (var "tdef") $
    "term" <~ Packaging.termDefinitionBody (var "tdef") $
    "lname" <~ (Formatting.convertCaseCamelToLowerSnake @@ (Names.localNameOf @@ var "name")) $
    "parameters" <~ Optionals.match (Packaging.termDefinitionSignature (var "tdef")) (list ([] :: [TypedTerm TermTyping.Parameter])) (lambda "sig" $ Typing.termSignatureParameters (var "sig")) $
    "typ" <~ Optionals.match (Packaging.termDefinitionSignature (var "tdef")) (Core.typeVariable (wrap _Name (string "hydra.core.model.Unit"))) (lambda "sig" $ Typing.resultType (Typing.termSignatureResult (var "sig"))) $
    "bodyTerm" <~ Lists.foldl
      (lambda "current" $ lambda "_" $
        match _Term (Strip.deannotateTerm @@ var "current") (Just $ var "current") [
          _Term_lambda>>: lambda "lam" $ Core.lambdaBody (var "lam")])
      (var "term")
      (var "parameters") $
    "fnParams" <<~ (Eithers.mapList (encodeParameter @@ var "cx" @@ var "g") (var "parameters")) $
    "body" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "bodyTerm") $
    "retType" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "typ") $
      right (record R._ItemWithComments [
        R._ItemWithComments_doc>>: nothing,
        R._ItemWithComments_visibility>>: inject R._Visibility R._Visibility_public unit,
        R._ItemWithComments_item>>: inject R._Item R._Item_fn $
          record R._FnDef [
            R._FnDef_name>>: var "lname",
            R._FnDef_generics>>: list ([] :: [TypedTerm R.GenericParam]),
            R._FnDef_whereClause>>: nothing,
            R._FnDef_params>>: var "fnParams",
            R._FnDef_returnType>>: just (var "retType"),
            R._FnDef_body>>: record R._Block [
              R._Block_statements>>: list ([] :: [TypedTerm R.Statement]),
              R._Block_expression>>: just (var "body")],
            R._FnDef_public>>: boolean True,
            R._FnDef_async>>: boolean False,
            R._FnDef_const>>: boolean False,
            R._FnDef_unsafe>>: boolean False,
            R._FnDef_doc>>: nothing]])

-- =============================================================================
-- Module entry point
-- =============================================================================

-- | Encode a Hydra type as a Rust syntax type
encodeType :: TypedTermDefinition (InferenceContext -> Graph -> Type -> Either Error R.Type)
encodeType = def "encodeType" $
  doc "Encode a Hydra type as a Rust syntax type" $
  "cx" ~> "g" ~> lambda "t" $
    "typ" <~ (Strip.deannotateType @@ var "t") $
    match _Type (var "typ") Nothing [
      _Type_annotated>>: lambda "at" $
        encodeType @@ var "cx" @@ var "g" @@ Core.annotatedTypeBody (var "at"),
      _Type_application>>: lambda "at" $
        encodeType @@ var "cx" @@ var "g" @@ Core.applicationTypeFunction (var "at"),
      _Type_unit>>: constant $
        right (asTerm rustUnit),
      _Type_void>>: constant $
        right (asTerm rustUnit),
      _Type_literal>>: lambda "lt" $
        right (encodeLiteralType @@ var "lt"),
      _Type_list>>: lambda "inner" $
        Eithers.map (lambda "enc" $ rustApply1 @@ string "Vec" @@ var "enc")
          (encodeType @@ var "cx" @@ var "g" @@ var "inner"),
      _Type_set>>: lambda "inner" $
        Eithers.map (lambda "enc" $ rustApply1 @@ string "BTreeSet" @@ var "enc")
          (encodeType @@ var "cx" @@ var "g" @@ var "inner"),
      _Type_map>>: lambda "mt" $
        "kt" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.mapTypeKeys (var "mt")) $
        "vt" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.mapTypeValues (var "mt")) $
          right (rustApply2 @@ string "BTreeMap" @@ var "kt" @@ var "vt"),
      _Type_optional>>: lambda "inner" $
        Eithers.map (lambda "enc" $ rustApply1 @@ string "Option" @@ var "enc")
          (encodeType @@ var "cx" @@ var "g" @@ var "inner"),
      _Type_either>>: lambda "et" $
        "lt" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.eitherTypeLeft (var "et")) $
        "rt" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.eitherTypeRight (var "et")) $
          right (rustApply2 @@ string "Either" @@ var "lt" @@ var "rt"),
      _Type_pair>>: lambda "pt" $
        "ft" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.pairTypeFirst (var "pt")) $
        "st" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.pairTypeSecond (var "pt")) $
          right (inject R._Type R._Type_tuple $ list [var "ft", var "st"]),
      _Type_function>>: lambda "ft" $
        "dom" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.functionTypeDomain (var "ft")) $
        "cod" <<~ (encodeType @@ var "cx" @@ var "g" @@ Core.functionTypeCodomain (var "ft")) $
          right (rustApply1 @@ string "Box"
            @@ (inject R._Type R._Type_dynTrait $ list [
              inject R._TypeParamBound R._TypeParamBound_trait $
                record R._TypePath [
                  R._TypePath_global>>: boolean False,
                  R._TypePath_segments>>: list [
                    record R._PathSegment [
                      R._PathSegment_name>>: string "Fn",
                      R._PathSegment_arguments>>:
                        inject R._GenericArguments R._GenericArguments_parenthesized $
                          record R._ParenthesizedArgs [
                            R._ParenthesizedArgs_inputs>>: list [var "dom"],
                            R._ParenthesizedArgs_output>>: just (var "cod")]]]]])),
      _Type_record>>: lambda "_" $
        left (Error.errorOther $ Error.otherError (string "unexpected anonymous record type")),
      _Type_union>>: lambda "_" $
        left (Error.errorOther $ Error.otherError (string "unexpected anonymous union type")),
      _Type_wrap>>: lambda "_" $
        left (Error.errorOther $ Error.otherError (string "unexpected anonymous wrap type")),
      _Type_variable>>: lambda "name" $
        right (rustPath @@ rustNominal (var "name")),
      _Type_forall>>: lambda "fa" $
        encodeType @@ var "cx" @@ var "g" @@ Core.forallTypeBody (var "fa")]

-- =============================================================================
-- Term encoding
-- =============================================================================

-- | Encode a Hydra type definition as a Rust item
encodeTypeDefinition :: TypedTermDefinition (InferenceContext -> Graph -> S.Set Name -> TypeDefinition -> Either Error R.ItemWithComments)
encodeTypeDefinition = def "encodeTypeDefinition" $
  doc "Encode a Hydra type definition as a Rust item. A name defined in the same module is a reference, not a type parameter." $
  "cx" ~> "g" ~> "defined" ~> lambda "tdef" $
    "name" <~ Packaging.typeDefinitionName (var "tdef") $
    "typ" <~ (Core.typeSchemeBody $ Packaging.typeDefinitionBody (var "tdef")) $
    "lname" <~ (Formatting.capitalize @@ (Names.localNameOf @@ var "name")) $
    -- Unqualified free names are type parameters, except names this module defines.
    "freeVars" <~ (Lists.filter
      (lambda "v" $ Logic.and
        (Equality.equal (Lists.length (Strings.splitOn (string ".") (Core.unName (var "v")))) (int32 1))
        (Logic.not (Sets.member (var "v") (var "defined"))))
      (Sets.toList (Variables.freeVariablesInType @@ var "typ"))) $
    "generics" <~ (Lists.map (lambda "v" $
      record R._GenericParam [
        R._GenericParam_name>>: Formatting.capitalize @@ Core.unName (var "v"),
        R._GenericParam_bounds>>: list ([] :: [TypedTerm R.TypeParamBound])])
      (var "freeVars")) $
    "dtyp" <~ (Strip.deannotateType @@ var "typ") $
    "item" <<~ (match _Type (var "dtyp") (Just $
        -- Fallback: type alias
        "styp" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "typ") $
          right (inject R._Item R._Item_typeAlias $
            record R._TypeAlias [
              R._TypeAlias_name>>: var "lname",
              R._TypeAlias_generics>>: var "generics",
              R._TypeAlias_type>>: var "styp",
              R._TypeAlias_public>>: boolean True,
              R._TypeAlias_doc>>: nothing]))
      [_Type_record>>: lambda "rt" $
        "sfields" <<~ (Eithers.mapList (encodeStructField @@ var "cx" @@ var "g") (var "rt")) $
          right (inject R._Item R._Item_struct $
            record R._StructDef [
              R._StructDef_name>>: var "lname",
              R._StructDef_generics>>: var "generics",
              R._StructDef_whereClause>>: nothing,
              R._StructDef_body>>: inject R._StructBody R._StructBody_named (var "sfields"),
              R._StructDef_derives>>: asTerm standardDerives,
              R._StructDef_public>>: boolean True,
              R._StructDef_doc>>: nothing]),
      _Type_union>>: lambda "rt" $
        "variants" <<~ (Eithers.mapList (encodeEnumVariant @@ var "cx" @@ var "g") (var "rt")) $
          right (inject R._Item R._Item_enum $
            record R._EnumDef [
              R._EnumDef_name>>: var "lname",
              R._EnumDef_generics>>: var "generics",
              R._EnumDef_whereClause>>: nothing,
              R._EnumDef_variants>>: var "variants",
              R._EnumDef_derives>>: asTerm standardDerives,
              R._EnumDef_public>>: boolean True,
              R._EnumDef_doc>>: nothing]),
      _Type_wrap>>: lambda "wt" $
        "styp" <<~ (encodeType @@ var "cx" @@ var "g" @@ var "wt") $
          right (inject R._Item R._Item_struct $
            record R._StructDef [
              R._StructDef_name>>: var "lname",
              R._StructDef_generics>>: var "generics",
              R._StructDef_whereClause>>: nothing,
              R._StructDef_body>>: inject R._StructBody R._StructBody_tuple $ list [
                record R._TupleField [
                  R._TupleField_type>>: var "styp",
                  R._TupleField_public>>: boolean True]],
              R._StructDef_derives>>: asTerm standardDerives,
              R._StructDef_public>>: boolean True,
              R._StructDef_doc>>: nothing])]) $
    right (record R._ItemWithComments [
      R._ItemWithComments_doc>>: nothing,
      R._ItemWithComments_visibility>>: inject R._Visibility R._Visibility_public unit,
      R._ItemWithComments_item>>: var "item"])

-- =============================================================================
-- Term definition encoding
-- =============================================================================

isIntegerPrimitive :: TypedTerm String -> TypedTerm Bool
isIntegerPrimitive name =
  Logic.or (Equality.equal name (string "hydra.lib.integers.compare")) $
  Logic.or (Equality.equal name (string "hydra.lib.integers.add")) $
  Logic.or (Equality.equal name (string "hydra.lib.integers.sub")) $
  Logic.or (Equality.equal name (string "hydra.lib.integers.mul")) $
  Logic.or (Equality.equal name (string "hydra.lib.integers.div")) $
  Logic.or (Equality.equal name (string "hydra.lib.integers.max")) $
    Equality.equal name (string "hydra.lib.integers.min")

requireArg :: TypedTerm Int -> TypedTerm [Term] -> TypedTerm (Either Error Term)
requireArg index args =
  Optionals.match (Lists.at index args)
    (left (Error.errorOther $ Error.otherError (string "integer primitive is missing an operand")))
    (lambda "operand" $ right (var "operand"))

applicationSpine :: TypedTermDefinition (Term -> (Term, [Term]))
applicationSpine = def "applicationSpine" $
  doc "Flatten a curried application. The argument list is outermost-first." $
  lambda "term" $
    match _Term (Strip.deannotateTerm @@ var "term") (Just $ pair (var "term") (list ([] :: [TypedTerm Term]))) [
      _Term_application>>: lambda "app" $
        "inner" <~ (applicationSpine @@ Core.applicationFunction (var "app")) $
          pair (Pairs.first (var "inner")) (Lists.cons (Core.applicationArgument (var "app")) (Pairs.second (var "inner")))]

encodeApplied :: TypedTermDefinition (InferenceContext -> Graph -> Term -> Either Error R.Expression)
encodeApplied = def "encodeApplied" $
  doc "Encode a saturated application. Integer primitives become operators. Other names become one Rust call." $
  "cx" ~> "g" ~> lambda "term" $
    "sp" <~ (applicationSpine @@ var "term") $
    "head" <~ Pairs.first (var "sp") $
    "args" <~ Lists.reverse (Pairs.second (var "sp")) $
    "dhead" <~ (Strip.deannotateTerm @@ var "head") $
      match _Term (var "dhead") (Just $ encodeCurriedCall @@ var "cx" @@ var "g" @@ var "head" @@ var "args") [
        _Term_variable>>: lambda "name" $
          encodeNamedCall @@ var "cx" @@ var "g" @@ Core.unName (var "name") @@ var "args",
        _Term_project>>: lambda "proj" $
          Optionals.match (Lists.head (var "args"))
            (encodeProjectionElim @@ var "cx" @@ var "g" @@ var "proj" @@ nothing)
            (lambda "arg" $ encodeProjectionElim @@ var "cx" @@ var "g" @@ var "proj" @@ just (var "arg")),
        _Term_cases>>: lambda "cs" $
          Optionals.match (Lists.head (var "args"))
            (encodeUnionElim @@ var "cx" @@ var "g" @@ var "cs" @@ nothing)
            (lambda "arg" $ encodeUnionElim @@ var "cx" @@ var "g" @@ var "cs" @@ just (var "arg")),
        _Term_unwrap>>: lambda "wname" $
          Optionals.match (Lists.head (var "args"))
            (encodeUnwrapElim @@ var "cx" @@ var "g" @@ var "wname" @@ nothing)
            (lambda "arg" $ encodeUnwrapElim @@ var "cx" @@ var "g" @@ var "wname" @@ just (var "arg"))]

encodeCurriedCall :: TypedTermDefinition (InferenceContext -> Graph -> Term -> [Term] -> Either Error R.Expression)
encodeCurriedCall = def "encodeCurriedCall" $
  doc "Encode an application whose function is not a name, one argument at a time." $
  "cx" ~> "g" ~> lambda "fun" $ lambda "args" $
    "funExpr" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "fun") $
    "argExprs" <<~ (Eithers.mapList (encodeTerm @@ var "cx" @@ var "g") (var "args")) $
      right (Lists.foldl
        (lambda "acc" $ lambda "arg" $ rustCall @@ var "acc" @@ list [var "arg"])
        (var "funExpr")
        (var "argExprs"))

encodeIntegerPrimitive :: TypedTermDefinition (InferenceContext -> Graph -> String -> [Term] -> Either Error R.Expression)
encodeIntegerPrimitive = def "encodeIntegerPrimitive" $
  doc "Lower a hydra.lib.integers primitive. compare returns the IntegersOrder enum." $
  "cx" ~> "g" ~> lambda "name" $ lambda "args" $
    "leftTerm" <<~ requireArg (int32 0) (var "args") $
    "rightTerm" <<~ requireArg (int32 1) (var "args") $
    "left" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "leftTerm") $
    "right" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "rightTerm") $
      Logic.ifElse (Equality.equal (var "name") (string "hydra.lib.integers.add"))
        (right (rustBinary (var "left") (inject R._BinaryOp R._BinaryOp_add unit) (var "right")))
        (Logic.ifElse (Equality.equal (var "name") (string "hydra.lib.integers.sub"))
          (right (rustBinary (var "left") (inject R._BinaryOp R._BinaryOp_sub unit) (var "right")))
          (Logic.ifElse (Equality.equal (var "name") (string "hydra.lib.integers.mul"))
            (right (rustBinary (var "left") (inject R._BinaryOp R._BinaryOp_mul unit) (var "right")))
            (Logic.ifElse (Equality.equal (var "name") (string "hydra.lib.integers.div"))
              (right (rustBinary (var "left") (inject R._BinaryOp R._BinaryOp_div unit) (var "right")))
              (Logic.ifElse (Equality.equal (var "name") (string "hydra.lib.integers.max"))
                (right (rustCall @@ (rustExprPath @@ string "std::cmp::max") @@ list [var "left", var "right"]))
                (Logic.ifElse (Equality.equal (var "name") (string "hydra.lib.integers.min"))
                  (right (rustCall @@ (rustExprPath @@ string "std::cmp::min") @@ list [var "left", var "right"]))
                  (Logic.ifElse (Equality.equal (var "name") (string "hydra.lib.integers.compare"))
                    (right (ifExpr
                      (rustBinary (var "left") (inject R._BinaryOp R._BinaryOp_gt unit) (var "right"))
                      (rustExprPath @@ string "IntegersOrder::Gt")
                      (ifExpr
                        (rustBinary (var "left") (inject R._BinaryOp R._BinaryOp_lt unit) (var "right"))
                        (rustExprPath @@ string "IntegersOrder::Lt")
                        (rustExprPath @@ string "IntegersOrder::Eq"))))
                    (left (Error.errorOther $ Error.otherError (string "unknown integer primitive")))))))))

encodeNamedCall :: TypedTermDefinition (InferenceContext -> Graph -> String -> [Term] -> Either Error R.Expression)
encodeNamedCall = def "encodeNamedCall" $
  doc "Encode a named saturated call, lowering hydra.lib.integers primitives." $
  "cx" ~> "g" ~> lambda "name" $ lambda "args" $
    Logic.ifElse (Logic.and (isIntegerPrimitive (var "name")) (Equality.equal (Lists.length (var "args")) (int32 2)))
      (encodeIntegerPrimitive @@ var "cx" @@ var "g" @@ var "name" @@ var "args")
      ("encoded" <<~ (Eithers.mapList (encodeTerm @@ var "cx" @@ var "g") (var "args")) $
        right (rustCall @@ (rustExprPath @@ localSnake (wrap _Name (var "name"))) @@ var "encoded"))

encodeParameter :: TypedTermDefinition (InferenceContext -> Graph -> TermTyping.Parameter -> Either Error R.FnParam)
encodeParameter = def "encodeParameter" $
  doc "Encode a Hydra signature parameter as a Rust function parameter." $
  "cx" ~> "g" ~> lambda "param" $
    "pname" <~ localSnake (Typing.parameterName (var "param")) $
    "ptype" <<~ (encodeType @@ var "cx" @@ var "g" @@ Typing.parameterType (var "param")) $
      right (record R._FnParam [
        R._FnParam_pattern>>:
          inject R._Pattern R._Pattern_identifier $
            record R._IdentifierPattern [
              R._IdentifierPattern_name>>: var "pname",
              R._IdentifierPattern_mutable>>: boolean False,
              R._IdentifierPattern_atPattern>>: nothing],
        R._FnParam_type>>: var "ptype"])

-- | Encode a Hydra case statement (union elimination) as a Rust expression.
-- Takes an optional argument for applied case statements.
encodeUnionElim :: TypedTermDefinition (InferenceContext -> Graph -> CaseStatement -> Maybe Term -> Either Error R.Expression)
encodeUnionElim = def "encodeUnionElim" $
  doc "Encode a Hydra case statement (union elimination) as a Rust expression. Takes an optional argument for applied case statements." $
  "cx" ~> "g" ~> lambda "cs" $ lambda "marg" $
        "tname" <~ rustNominal (Core.caseStatementTypeName (var "cs")) $
        "caseFields" <~ Core.caseStatementCases (var "cs") $
        "defCase" <~ Core.caseStatementDefault (var "cs") $
        "arms" <<~ (Eithers.mapList
          (lambda "cf" $
            "cfname" <~ (Formatting.capitalize @@ Core.unName (Core.caseAlternativeName (var "cf"))) $
            "cfterm" <~ Core.caseAlternativeHandler (var "cf") $
            "variantPath" <~ Strings.concat2 (Strings.concat2 (var "tname") (string "::")) (var "cfname") $
            "handlerDomain" <~ match _Term (Strip.deannotateTerm @@ var "cfterm") (Just $ nothing) [
                _Term_lambda>>: lambda "lam" $ Core.lambdaDomain (var "lam")] $
            "domainIsUnit" <~ Optionals.match (var "handlerDomain") (boolean False) (lambda "dom" $
                match _Type (Strip.deannotateType @@ var "dom") (Just $ boolean False) [
                  _Type_unit>>: constant $ boolean True]) $
            "nominalUnit" <~ Logic.and (var "domainIsUnit") (Logic.not (Equality.equal (var "tname") (string "Either"))) $
            "payload" <~ Logic.ifElse (var "nominalUnit") Core.termUnit (Core.termVariable (wrap _Name (string "v"))) $
            "armBody" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ (Core.termApplication (Core.application (var "cfterm") (var "payload")))) $
            "armPattern" <~ Logic.ifElse (var "nominalUnit")
              (inject R._Pattern R._Pattern_path $
                record R._ExprPath [
                  R._ExprPath_global>>: boolean False,
                  R._ExprPath_segments>>: list [
                    record R._PathSegment [
                      R._PathSegment_name>>: var "variantPath",
                      R._PathSegment_arguments>>: inject R._GenericArguments R._GenericArguments_none unit]]])
              (inject R._Pattern R._Pattern_tupleStruct $
                record R._TupleStructPattern [
                  R._TupleStructPattern_path>>:
                    record R._ExprPath [
                      R._ExprPath_global>>: boolean False,
                      R._ExprPath_segments>>: list [
                        record R._PathSegment [
                          R._PathSegment_name>>: var "variantPath",
                          R._PathSegment_arguments>>: inject R._GenericArguments R._GenericArguments_none unit]]],
                  R._TupleStructPattern_elements>>: list [
                    inject R._Pattern R._Pattern_identifier $
                      record R._IdentifierPattern [
                        R._IdentifierPattern_name>>: string "v",
                        R._IdentifierPattern_mutable>>: boolean False,
                        R._IdentifierPattern_atPattern>>: nothing]]]) $
              right (record R._MatchArm [
                R._MatchArm_pattern>>: var "armPattern",
                R._MatchArm_guard>>: nothing,
                R._MatchArm_body>>: var "armBody"]))
          (var "caseFields")) $
        -- Add default arm if present
        "allArms" <<~ (Optionals.match (var "defCase")
          (right (var "arms"))
          (lambda "dt" $
            "defBody" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ (Core.termApplication (Core.application (var "dt") (Core.termVariable (wrap _Name (string "v")))))) $
            right (Lists.concat2 (var "arms") (list [
              record R._MatchArm [
                R._MatchArm_pattern>>:
                  inject R._Pattern R._Pattern_identifier $
                    record R._IdentifierPattern [
                      R._IdentifierPattern_name>>: string "v",
                      R._IdentifierPattern_mutable>>: boolean False,
                      R._IdentifierPattern_atPattern>>: nothing],
                R._MatchArm_guard>>: nothing,
                R._MatchArm_body>>: var "defBody"]])))) $
        Logic.ifElse (Lists.isEmpty (var "allArms"))
          (right panicStuck)
          (Optionals.match (var "marg")
            -- Unapplied: |v| match v { ... }
            (right (rustClosure @@ nothing @@ list [string "v"] @@
              (inject R._Expression R._Expression_match $
                record R._MatchExpr [
                  R._MatchExpr_scrutinee>>: rustExprPath @@ string "v",
                  R._MatchExpr_arms>>: var "allArms"])))
            (lambda "arg" $
              "sarg" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "arg") $
                right (inject R._Expression R._Expression_match $
                  record R._MatchExpr [
                    R._MatchExpr_scrutinee>>: var "sarg",
                    R._MatchExpr_arms>>: var "allArms"])))

-- | Encode a Hydra wrap elimination (unwrap) as a Rust expression.
-- Takes an optional argument for applied unwraps.
encodeUnwrapElim :: TypedTermDefinition (InferenceContext -> Graph -> Name -> Maybe Term -> Either Error R.Expression)
encodeUnwrapElim = def "encodeUnwrapElim" $
  doc "Encode a Hydra wrap elimination (unwrap) as a Rust expression. Takes an optional argument for applied unwraps." $
  "cx" ~> "g" ~> lambda "name" $ lambda "marg" $
        Optionals.match (var "marg")
          -- Unapplied: |v| v.0
          (right (rustClosure @@ nothing @@ list [string "v"] @@
            (inject R._Expression R._Expression_tupleIndex $
              record R._TupleIndexExpr [
                R._TupleIndexExpr_tuple>>: rustExprPath @@ string "v",
                R._TupleIndexExpr_index>>: int32 0])))
          (lambda "arg" $
            "sarg" <<~ (encodeTerm @@ var "cx" @@ var "g" @@ var "arg") $
              right (inject R._Expression R._Expression_tupleIndex $
                record R._TupleIndexExpr [
                  R._TupleIndexExpr_tuple>>: var "sarg",
                  R._TupleIndexExpr_index>>: int32 0]))

-- =============================================================================
-- Struct field encoding
-- =============================================================================

supportItems :: TypedTermDefinition [R.ItemWithComments]
supportItems = def "supportItems" $
  doc "Either and the integer ordering enum. Every generated module can name them." $
  list [
    pubEnum (string "Either") (list [genericParam "L", genericParam "R"]) (list [
      tupleVariant "Left" (rustPath @@ string "L"),
      tupleVariant "Right" (rustPath @@ string "R")]),
    pubEnum (string "IntegersOrder") (list ([] :: [TypedTerm R.GenericParam])) (list [
      unitVariant "Lt",
      unitVariant "Eq",
      unitVariant "Gt"])]

-- | Convert a Hydra module to a map of file paths to Rust source code strings.
moduleToRust :: TypedTermDefinition (Module -> [Definition] -> InferenceContext -> Graph -> Either Error (M.Map FilePath String))
moduleToRust = def "moduleToRust" $
  doc "Convert a Hydra module to a map of file paths to Rust source code strings" $
  "mod" ~> "defs" ~> "cx" ~> "g" ~>
    "partitioned" <~ (Environment.partitionDefinitions @@ var "defs") $
    "typeDefs" <~ Pairs.first (var "partitioned") $
    "termDefs" <~ Pairs.second (var "partitioned") $
    "defined" <~ Sets.fromList (Lists.map (lambda "td" $ Packaging.typeDefinitionName (var "td")) (var "typeDefs")) $
    "typeItems" <<~ (Eithers.mapList (encodeTypeDefinition @@ var "cx" @@ var "g" @@ var "defined") (var "typeDefs")) $
    "termItems" <<~ (Eithers.mapList (encodeTermDefinition @@ var "cx" @@ var "g") (var "termDefs")) $
    "allItems" <~ Lists.concat2 (asTerm supportItems) (Lists.concat2 (var "typeItems") (var "termItems")) $
    "crate" <~ (record R._Crate [R._Crate_items>>: var "allItems"]) $
    "code" <~ (SerializationSource.printExpr @@ (SerializationSource.parenthesize @@ (RustSerdeSource.crateToExpr @@ var "crate"))) $
    "filePath" <~ (Names.moduleNameToFilePath @@ Util.caseConventionLowerSnake @@ wrap _FileExtension (string "rs") @@ (Packaging.moduleName (var "mod"))) $
      right (Maps.singleton (var "filePath") (var "code") :: TypedTerm (M.Map FilePath String))

-- | Apply a type constructor to one type argument (e.g., Vec<T>)
rustApply1 :: TypedTermDefinition (String -> R.Type -> R.Type)
rustApply1 = def "rustApply1" $
  doc "Apply a type constructor to one type argument (e.g., Vec<T>)" $
  lambda "name" $ lambda "arg" $
    inject R._Type R._Type_path $
      record R._TypePath [
        R._TypePath_global>>: boolean False,
        R._TypePath_segments>>: list [
          record R._PathSegment [
            R._PathSegment_name>>: var "name",
            R._PathSegment_arguments>>:
              inject R._GenericArguments R._GenericArguments_angleBracketed $
                record R._AngleBracketedArgs [
                  R._AngleBracketedArgs_args>>: list [
                    inject R._GenericArg R._GenericArg_type (var "arg")]]]]]

-- | Apply a type constructor to two type arguments (e.g., BTreeMap<K, V>)
rustApply2 :: TypedTermDefinition (String -> R.Type -> R.Type -> R.Type)
rustApply2 = def "rustApply2" $
  doc "Apply a type constructor to two type arguments (e.g., BTreeMap<K, V>)" $
  lambda "name" $ lambda "arg1" $ lambda "arg2" $
    inject R._Type R._Type_path $
      record R._TypePath [
        R._TypePath_global>>: boolean False,
        R._TypePath_segments>>: list [
          record R._PathSegment [
            R._PathSegment_name>>: var "name",
            R._PathSegment_arguments>>:
              inject R._GenericArguments R._GenericArguments_angleBracketed $
                record R._AngleBracketedArgs [
                  R._AngleBracketedArgs_args>>: list [
                    inject R._GenericArg R._GenericArg_type (var "arg1"),
                    inject R._GenericArg R._GenericArg_type (var "arg2")]]]]]

-- | Block expression with statements and trailing expression
rustBlock :: TypedTermDefinition ([R.Statement] -> R.Expression -> R.Expression)
rustBlock = def "rustBlock" $
  doc "Block expression with statements and trailing expression" $
  lambda "stmts" $ lambda "expr" $
    inject R._Expression R._Expression_block $
      record R._Block [
        R._Block_statements>>: var "stmts",
        R._Block_expression>>: just (var "expr")]

-- | Function call expression
rustCall :: TypedTermDefinition (R.Expression -> [R.Expression] -> R.Expression)
rustCall = def "rustCall" $
  doc "Function call expression" $
  lambda "fun" $ lambda "args" $
    inject R._Expression R._Expression_call $
      record R._CallExpr [
        R._CallExpr_function>>: var "fun",
        R._CallExpr_args>>: var "args"]

-- | Closure expression: |params| body. The annotation is each parameter's type, when known.
rustClosure :: TypedTermDefinition (Maybe R.Type -> [String] -> R.Expression -> R.Expression)
rustClosure = def "rustClosure" $
  doc "Closure expression: |params| body. The annotation is each parameter's type, when known." $
  lambda "ann" $ lambda "params" $ lambda "body" $
    inject R._Expression R._Expression_closure $
      record R._ClosureExpr [
        R._ClosureExpr_move>>: boolean False,
        R._ClosureExpr_params>>:
          Lists.map (lambda "p" $
            record R._ClosureParam [
              R._ClosureParam_pattern>>:
                inject R._Pattern R._Pattern_identifier $
                  record R._IdentifierPattern [
                    R._IdentifierPattern_name>>: var "p",
                    R._IdentifierPattern_mutable>>: boolean False,
                    R._IdentifierPattern_atPattern>>: nothing],
              R._ClosureParam_type>>: var "ann"])
          (var "params"),
        R._ClosureExpr_returnType>>: nothing,
        R._ClosureExpr_body>>: var "body"]

-- =============================================================================
-- Literal type encoding
-- =============================================================================

-- | Variable reference as a path expression
rustExprPath :: TypedTermDefinition (String -> R.Expression)
rustExprPath = def "rustExprPath" $
  doc "Variable reference as a path expression" $
  lambda "name" $
    inject R._Expression R._Expression_path $
      record R._ExprPath [
        R._ExprPath_global>>: boolean False,
        R._ExprPath_segments>>: list [
          record R._PathSegment [
            R._PathSegment_name>>: var "name",
            R._PathSegment_arguments>>: inject R._GenericArguments R._GenericArguments_none unit]]]

-- | Let statement: let name = expr;
rustLetStmt :: TypedTermDefinition (String -> R.Expression -> R.Statement)
rustLetStmt = def "rustLetStmt" $
  doc "Let statement: let name = expr;" $
  lambda "name" $ lambda "expr" $
    inject R._Statement R._Statement_let $
      record R._LetStatement [
        R._LetStatement_pattern>>:
          inject R._Pattern R._Pattern_identifier $
            record R._IdentifierPattern [
              R._IdentifierPattern_name>>: var "name",
              R._IdentifierPattern_mutable>>: boolean False,
              R._IdentifierPattern_atPattern>>: nothing],
        R._LetStatement_mutable>>: boolean False,
        R._LetStatement_type>>: nothing,
        R._LetStatement_init>>: just (var "expr")]

-- | Construct a simple Rust path type (e.g., "String" -> String)
rustPath :: TypedTermDefinition (String -> R.Type)
rustPath = def "rustPath" $
  doc "Construct a simple Rust path type (e.g., \"String\" -> String)" $
  lambda "name" $
    inject R._Type R._Type_path $
      record R._TypePath [
        R._TypePath_global>>: boolean False,
        R._TypePath_segments>>: list [
          record R._PathSegment [
            R._PathSegment_name>>: var "name",
            R._PathSegment_arguments>>: inject R._GenericArguments R._GenericArguments_none unit]]]

-- | Construct a Rust path type with multiple segments (e.g., ["num", "BigInt"] -> num::BigInt)
rustPathSegmented :: TypedTermDefinition ([String] -> R.Type)
rustPathSegmented = def "rustPathSegmented" $
  doc "Construct a Rust path type with multiple segments (e.g., [\"num\", \"BigInt\"] -> num::BigInt)" $
  lambda "segs" $
    inject R._Type R._Type_path $
      record R._TypePath [
        R._TypePath_global>>: boolean False,
        R._TypePath_segments>>:
          Lists.map (lambda "s" $
            record R._PathSegment [
              R._PathSegment_name>>: var "s",
              R._PathSegment_arguments>>: inject R._GenericArguments R._GenericArguments_none unit])
          (var "segs")]

-- | The Rust unit type ()
rustUnit :: TypedTermDefinition R.Type
rustUnit = def "rustUnit" $
  doc "The Rust unit type ()" $
  inject R._Type R._Type_unit unit

-- =============================================================================
-- Rust expression AST helpers
-- =============================================================================

-- | The standard set of derive macros applied to generated structs and enums
standardDerives :: TypedTermDefinition [String]
standardDerives = def "standardDerives" $
  doc "The standard set of derive macros applied to generated structs and enums" $
  list $ string <$> ["Clone", "Debug", "PartialEq", "Eq", "PartialOrd", "Ord"]

-- =============================================================================
-- Rust type AST helpers
-- =============================================================================
