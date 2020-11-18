-- -*- mode: haskell -*-

{
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -w #-}

-- |
-- Module      : Language.VHDL.Parser.Parser
-- Copyright   : (c) 2014-2016 Drexel University
-- License     : BSD-style
-- Author      : Geoffrey Mainland <mainland@drexel.edu>
-- Maintainer  : Geoffrey Mainland <mainland@drexel.edu>

module Language.VHDL.Parser.Parser where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Exception
import Data.Char (toLower)
import Data.List (foldl1',
                  intersperse)
import Data.Loc
import Data.Maybe (fromMaybe, catMaybes)
#if !(MIN_VERSION_base(4,9,0))
import Data.Monoid (Monoid(..), (<>))
#endif /* !(MIN_VERSION_base(4,9,0)) */
import Data.Ratio
#if MIN_VERSION_base(4,9,0) && !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Symbol
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import Language.VHDL.Parser.Exceptions
import Language.VHDL.Parser.Lexer
import Language.VHDL.Parser.Monad
import qualified Language.VHDL.Parser.Tokens as T
import Language.VHDL.Syntax hiding (L)
}

%token
  ID          { L _ T.Tident{} }
  EXTID       { L _ T.Text_ident{} }
  TYID        { L _ T.Ttype_ident{} }
  INT         { L _ T.TintLit{} }
  REAL        { L _ T.TrealLit{} }
  CHAR        { L _ T.TcharLit{} }
  STRING      { L _ T.TstringLit{} }
  BITSTRING   { L _ T.TbitstringLit{} }
  OPERATOR    { L _ T.TstringLit{} }

  'abs'           { L _ T.Tabs }
  'access'        { L _ T.Taccess }
  'after'         { L _ T.Tafter }
  'alias'         { L _ T.Talias }
  'all'           { L _ T.Tall }
  'and'           { L _ T.Tand }
  'array'         { L _ T.Tarray }
  'architecture'  { L _ T.Tarchitecture }
  'assert'        { L _ T.Tassert }
  'attribute'     { L _ T.Tattribute }
  'begin'         { L _ T.Tbegin }
  'block'         { L _ T.Tblock }
  'body'          { L _ T.Tbody }
  'buffer'        { L _ T.Tbuffer }
  'bus'           { L _ T.Tbus }
  'case'          { L _ T.Tcase }
  'component'     { L _ T.Tcomponent }
  'context'       { L _ T.Tcontext }
  'configuration' { L _ T.Tconfiguration }
  'constant'      { L _ T.Tconstant }
  'default'       { L _ T.Tdefault }
  'disconnect'    { L _ T.Tdisconnect }
  'downto'        { L _ T.Tdownto }
  'else'          { L _ T.Telse }
  'elsif'         { L _ T.Telsif }
  'end'           { L _ T.Tend }
  'entity'        { L _ T.Tentity }
  'exit'          { L _ T.Texit }
  'file'          { L _ T.Tfile }
  'for'           { L _ T.Tfor }
  'force'         { L _ T.Tforce }
  'function'      { L _ T.Tfunction }
  'generate'      { L _ T.Tgenerate }
  'generic'       { L _ T.Tgeneric }
  'group'         { L _ T.Tgroup }
  'guarded'       { L _ T.Tguarded }
  'if'            { L _ T.Tif }
  'impure'        { L _ T.Timpure }
  'inertial'      { L _ T.Tinertial }
  'in'            { L _ T.Tin }
  'inout'         { L _ T.Tinout }
  'is'            { L _ T.Tis }
  'label'         { L _ T.Tlabel }
  'library'       { L _ T.Tlibrary }
  'linkage'       { L _ T.Tlinkage }
  'literal'       { L _ T.Tliteral }
  'loop'          { L _ T.Tloop }
  'map'           { L _ T.Tmap }
  'mod'           { L _ T.Tmod }
  'nand'          { L _ T.Tnand }
  'new'           { L _ T.Tnew }
  'next'          { L _ T.Tnext }
  'nor'           { L _ T.Tnor }
  'not'           { L _ T.Tnot }
  'null'          { L _ T.Tnull }
  'of'            { L _ T.Tof }
  'on'            { L _ T.Ton }
  'open'          { L _ T.Topen }
  'or'            { L _ T.Tor }
  'out'           { L _ T.Tout }
  'others'        { L _ T.Tothers }
  'package'       { L _ T.Tpackage }
  'parameter'     { L _ T.Tparameter }
  'port'          { L _ T.Tport }
  'postponed'     { L _ T.Tpostponed }
  'procedure'     { L _ T.Tprocedure }
  'process'       { L _ T.Tprocess }
  'property'      { L _ T.Tproperty }
  'protected'     { L _ T.Tprotected }
  'pure'          { L _ T.Tpure }
  'range'         { L _ T.Trange }
  'record'        { L _ T.Trecord }
  'register'      { L _ T.Tregister }
  'reject'        { L _ T.Treject }
  'release'       { L _ T.Trelease }
  'rem'           { L _ T.Trem }
  'report'        { L _ T.Treport }
  'return'        { L _ T.Treturn }
  'rol'           { L _ T.Trol }
  'ror'           { L _ T.Tror }
  'select'        { L _ T.Tselect }
  'sequence'      { L _ T.Tsequence }
  'severity'      { L _ T.Tseverity }
  'shared'        { L _ T.Tshared }
  'signal'        { L _ T.Tsignal }
  'sla'           { L _ T.Tsla }
  'sll'           { L _ T.Tsll }
  'sra'           { L _ T.Tsra }
  'srl'           { L _ T.Tsrl }
  'subtype'       { L _ T.Tsubtype }
  'then'          { L _ T.Tthen }
  'to'            { L _ T.Tto }
  'transport'     { L _ T.Ttransport }
  'type'          { L _ T.Ttype }
  'unaffected'    { L _ T.Tunaffected }
  'units'         { L _ T.Tunits }
  'until'         { L _ T.Tuntil }
  'use'           { L _ T.Tuse }
  'variable'      { L _ T.Tvariable }
  'vunit'         { L _ T.Tvunit }
  'wait'          { L _ T.Twait }
  'when'          { L _ T.Twhen }
  'while'         { L _ T.Twhile }
  'with'          { L _ T.Twith }
  'xnor'          { L _ T.Txnor }
  'xor'           { L _ T.Txor }

  'typename' { L _ T.Ttypename }

  '+'  { L _ T.Tplus }
  '-'  { L _ T.Tminus }
  '&'  { L _ T.Tcat }
  '*'  { L _ T.Tstar }
  '/'  { L _ T.Tdiv }

  '\'' { L _ T.Tsquote }
  '.'  { L _ T.Tdot }
  ','  { L _ T.Tcomma }
  ';'  { L _ T.Tsemi }
  ':'  { L _ T.Tcolon }
  '|'  { L _ T.Tbar }
  '@'  { L _ T.Tat }
  '^'  { L _ T.Tcaret }

  '('  { L _ T.Tlparen }
  ')'  { L _ T.Trparen }
  '['  { L _ T.Tlbrack }
  ']'  { L _ T.Trbrack }

  '=>' { L _ T.Tarrow }
  '**' { L _ T.Texp }
  ':=' { L _ T.Tassign }
  '<>' { L _ T.Tbox }
  '??' { L _ T.Tcond }
  '<<' { L _ T.Tdlt }
  '>>' { L _ T.Tdgt }

  '='  { L _ T.Teq }
  '/=' { L _ T.Tne }
  '<'  { L _ T.Tlt }
  '<=' { L _ T.Tle }
  '>=' { L _ T.Tge }
  '>'  { L _ T.Tgt }

  '?'   { L _ T.Tmatch }
  '?='  { L _ T.Tmatch_eq }
  '?/=' { L _ T.Tmatch_ne }
  '?<'  { L _ T.Tmatch_lt }
  '?<=' { L _ T.Tmatch_le }
  '?>=' { L _ T.Tmatch_ge }
  '?>'  { L _ T.Tmatch_gt }

  ANTI_ID    { L _ T.Tanti_id{} }
  ANTI_EXP   { L _ T.Tanti_exp{} }
  ANTI_EXPS  { L _ T.Tanti_exps{} }
  ANTI_INT   { L _ T.Tanti_int{} }
  ANTI_REAL  { L _ T.Tanti_real{} }
  ANTI_LIT   { L _ T.Tanti_lit{} }
  ANTI_LITS  { L _ T.Tanti_lits{} }
  ANTI_STM   { L _ T.Tanti_stm{} }
  ANTI_STMS  { L _ T.Tanti_stms{} }
  ANTI_CSTM  { L _ T.Tanti_cstm{} }
  ANTI_CSTMS { L _ T.Tanti_cstms{} }
  ANTI_TYPE  { L _ T.Tanti_type{} }

%left DIRECTION
%left '??'
%left '|'
%left 'and' 'or' 'nand' 'nor' 'xor' 'xnor'
%nonassoc 'nand' 'nor'
%left '=' '/=' '<' '<=' '>' '>=' '?=' '?/=' '?<' '?<=' '?>' '?>='
%left 'sll' 'srl' 'sla' 'sra' 'rol' 'ror'
%left '+' '-' '&'
%left SIGN
%left '*' '/' 'mod' 'rem'
%left '**' 'abs' 'not'
%left LABEL

%expect 0

%monad { P } { >>= } { return }
%lexer { lexer } { L _ T.Teof }
%tokentype { (L T.Token) }
%error { happyError }

%name parseDesignFile design_file
%name parseDesignUnit design_unit
%name parseName name
%name parseDecl declaration
%name parseIDecl interface_declaration
%name parseType subtype_indication
%name parseLit abstract_literal
%name parseExp only_expression
%name parseStm sequential_statement
%name parseCStm concurrent_statement

%%
{-
[§ 3.2]

entity_declaration ::=
  entity identifier is
    entity_header
    entity_declarative_part
  [ begin
    entity_statement_part ]
  end [ entity ] [ entity_simple_name ] ;

entity_header ::=
  [formal_generic_clause ]
  [formal_port_clause ]

entity_declarative_part ::=
  { entity_declarative_item }

entity_declarative_item ::=
    subprogram_declaration
  | subprogram_body
  | subprogram_instantiation_declaration
  | package_declaration
  | package_body
  | package_instantiation_declaration
  | type_declaration
  | subtype_declaration
  | constant_declaration
  | signal_declaration
  | shared_variable_declaration
  | file_declaration
  | alias_declaration
  | attribute_declaration
  | attribute_specification
  | disconnection_specification
  | use_clause
  | group_template_declaration
  | group_declaration
  | PSL_Property_Declaration
  | PSL_Sequence_Declaration
  | PSL_Clock_Declaration

entity_statement_part ::=
  { entity_statement }

entity_statement ::=
    concurrent_assertion_statement
  | passive_concurrent_procedure_call_statement
  | passive_process_statement
  | PSL_PSL_Directive
-}

entity_declaration :: { Decl }
entity_declaration :
    'entity' identifier 'is'
      generic_clause_opt
      port_clause_opt
      declarations_rlist
    'end' entity_opt simple_name_opt ';'
      { EntityD $2 $4 $5 (rev $6) [] ($1 `srcspan` $10) }
  | 'entity' identifier 'is'
      generic_clause_opt
      port_clause_opt
      declarations_rlist
    'begin'
      concurrent_statements
    'end' entity_opt simple_name_opt ';'
      { EntityD $2 $4 $5 (rev $6) $8 ($1 `srcspan` $12) }

entity_opt :: { () }
entity_opt :
    {- empty -} { () }
  | 'entity'    { () }

{-
[§ 3.3]

architecture_body ::=
  architecture identifier of entity_name is
    architecture_declarative_part
  begin
    architecture_statement_part
  end [ architecture ] [ architecture_simple_name ] ;

architecture_declarative_part ::=
  { block_declarative_item }

block_declarative_item ::=
    subprogram_declaration
  | subprogram_body
  | subprogram_instantiation_declaration
  | package_declaration
  | package_body
  | package_instantiation_declaration
  | type_declaration
  | subtype_declaration
  | constant_declaration
  | signal_declaration
  | shared_variable_declaration
  | file_declaration
  | alias_declaration
  | component_declaration
  | attribute_declaration
  | attribute_specification
  | configuration_specification
  | disconnection_specification
  | use_clause
  | group_template_declaration
  | group_declaration
  | PSL_Property_Declaration
  | PSL_Sequence_Declaration
  | PSL_Clock_Declaration

architecture_statement_part ::=
  { concurrent_statement }
-}

architecture_body :: { Decl }
architecture_body :
  'architecture' identifier 'of' name 'is'
    declarations_rlist
  'begin'
    concurrent_statements
  'end' architecture_opt simple_name_opt ';'
    { ArchD $2 $4 (rev $6) $8 ($1 `srcspan` $12) }

architecture_opt :: { () }
architecture_opt :
    {- empty -}    { () }
  | 'architecture' { () }

{-
[§ 3.4]

configuration_declaration ::=
  configuration identifier of entity_name is
    configuration_declarative_part
    { verification_unit_binding_indication ; }
    block_configuration
  end [ configuration ] [ configuration_simple_name ] ;

configuration_declarative_part ::=
  { configuration_declarative_item }

configuration_declarative_item ::=
    use_clause
  | attribute_specification
  | group_declaration

block_configuration ::=
  for block_specification
    { use_clause }
    { configuration_item }
  end for ;

block_specification ::=
    architecture_name
  | block_statement_label
  | generate_statement_label [ ( generate_specification ) ]

generate_specification ::=
    static_discrete_range
  | static_expression
  | alternative_label

configuration_item ::=
    block_configuration
  | component_configuration

component_configuration ::=
  for component_specification
    [ binding_indication ; ]
    { verification_unit_binding_indication ; }
    [ block_configuration ]
  end for ;
-}

configuration_declaration :: { Decl }
configuration_declaration :
  'configuration' identifier 'of' name 'is'
    declarations_rlist
    binding_indications
    block_configuration
  'end' configuration_opt simple_name_opt ';'
    { ConfigD $2 $4 (rev $6) $7 $8 ($1 `srcspan` $12) }

configuration_opt :: { () }
configuration_opt :
    {- empty -}     { () }
  | 'configuration' { () }

block_configuration :: { Config }
block_configuration :
  'for' block_specification
    use_clauses
    configuration_items
  'end' 'for' ';'
    { Block $2 $3 $4 ($1 `srcspan` $7) }

block_configuration_opt :: { Maybe Config }
block_configuration_opt :
    {- empty -}         { Nothing }
  | block_configuration { Just $1 }

block_specification :: { BlockSpec }
block_specification :
    name
      { ArchB $1 (srclocOf $1) }
{-}
  | label
      { BlockLblB $1 (srclocOf $1) }
-}
  | label '(' generate_specification ')'
      { GenLblB $1 (Just $3) ($1 `srcspan` $4) }

generate_specification :: { GenSpec }
generate_specification :
  expression
    {% checkGenerateSpec $1 }

configuration_item :: { Config }
configuration_item :
    block_configuration     { $1 }
  | component_configuration { $1 }

configuration_items :: { [Config] }
configuration_items : configuration_item_rlist { rev $1 }

configuration_item_rlist :: { RevList Config }
configuration_item_rlist :
    configuration_item                         { rsingleton $1 }
 | configuration_item_rlist configuration_item { rcons $2 $1 }

component_configuration :: { Config }
component_configuration :
  'for' component_specification
    binding_indications
    block_configuration_opt
  'end' 'for' ';'
    { Component $2 $3 $4 ($1 `srcspan` $7) }

{-
[§ 4.2]

subprogram_declaration ::=
  subprogram_specification ;

subprogram_specification ::=
  procedure_specification | function_specification

procedure_specification ::=
  procedure designator
    subprogram_header
    [ [ parameter ] ( formal_parameter_list ) ]

function_specification ::=
  [ pure | impure ] function designator
    subprogram_header
    [ [ parameter ] ( formal_parameter_list ) ] return type_mark

subprogram_header ::=
  [ generic ( generic_list )
  [ generic_map_aspect ] ]

designator ::= identifier | operator_symbol

operator_symbol ::= string_literal

formal_parameter_list ::= parameter_interface_list
-}

subprogram_declaration :: { Decl }
subprogram_declaration : subprogram_specification ';' { $1 }

subprogram_specification :: { Decl }
subprogram_specification :
    procedure_specification { $1 }
  | function_specification  { $1 }

procedure_specification :: { Decl }
procedure_specification :
    'procedure' designator
      { ProcSpecD $2 Nothing [] ($1 `srcspan` $2) }
  | 'procedure' designator formal_parameter_list
      { ProcSpecD $2 Nothing $3 ($1 `srcspan` $3) }
  | 'procedure' designator subprogram_header
      { ProcSpecD $2 (Just $3) [] ($1 `srcspan` $3) }
  | 'procedure' designator subprogram_header formal_parameter_list
      { ProcSpecD $2 (Just $3) $4 ($1 `srcspan` $4) }

function_specification_prefix :: { FunSpecP }
function_specification_prefix :
    'function' designator
      { FunSpecP $2 Nothing Nothing ($1 `srcspan` $2) }
  | purity 'function' designator
      { FunSpecP $3 (Just $1) Nothing ($1 `srcspan` $3) }
  | 'function' designator subprogram_header
      { FunSpecP $2 Nothing (Just $3) ($1 `srcspan` $3) }
  | purity 'function' designator subprogram_header
      { FunSpecP $3 (Just $1) (Just $4) ($1 `srcspan` $4) }

function_specification :: { Decl }
function_specification :
    function_specification_prefix 'return' type_mark
      {% do { let FunSpecP name purity subprog _ = $1
            ; return $ FunSpecD name purity subprog [] $3 ($1 `srcspan` $3)
            }
      }
  | function_specification_prefix formal_parameter_list 'return' type_mark
      {% do { let FunSpecP name purity subprog _ = $1
            ; return $ FunSpecD name purity subprog $2 $4 ($1 `srcspan` $4)
            }
      }

purity :: { Purity }
purity :
    'pure'      { Pure (srclocOf $1) }
  | 'impure'    { Impure (srclocOf $1) }

subprogram_header :: { SubprogramHeader }
subprogram_header :
    'generic' '(' interface_list ')'
      { SubprogramHeader $3 Nothing ($1 `srcspan` $4) }
  | 'generic' '(' interface_list ')' generic_map_aspect
      { SubprogramHeader $3 (Just $5) ($1 `srcspan` $5) }

designator :: { Name }
designator :
    identifier
      { SimpleN [] $1 (srclocOf $1) }
  | operator_symbol
      { OpN [] $1 (srclocOf $1) }

operator_symbol :: { Operator }
operator_symbol :
    OPERATOR { let { (_, op) = getOPERATOR $1 }
                in
                  mkOperator (locOf $1) op
             }

formal_parameter_list :: { [IDecl] }
formal_parameter_list :
    '(' interface_list ')'             { $2 }
  | 'parameter' '(' interface_list ')' { $3 }

{-
[§ 4.3]

subprogram_body ::=
  subprogram_specification is
    subprogram_declarative_part
  begin
    subprogram_statement_part
  end [ subprogram_kind ] [ designator ] ;

subprogram_declarative_part ::=
  { subprogram_declarative_item }

subprogram_declarative_item ::=
    subprogram_declaration
  | subprogram_body
  | subprogram_instantiation_declaration
  | package_declaration
  | package_body
  | package_instantiation_declaration
  | type_declaration
  | subtype_declaration
  | constant_declaration
  | variable_declaration
  | file_declaration
  | alias_declaration
  | attribute_declaration
  | attribute_specification
  | use_clause
  | group_template_declaration
  | group_declaration

subprogram_statement_part ::=
  { sequential_statement }

subprogram_kind ::= procedure | function
-}

subprogram_body :: { Decl }
subprogram_body :
  subprogram_specification 'is'
    declarations_rlist
  'begin'
    sequence_of_statements
  'end' subprogram_kind designator_opt ';'
    {% case $1 of
        { ProcSpecD f hdr idecls _ ->
            return $ ProcD f hdr idecls (rev $3) $5 ($1 `srcspan` $9)
        ; FunSpecD f purity hdr idecls ty _ ->
            return $ FunD f purity hdr idecls ty (rev $3) $5 ($1 `srcspan` $9)
        ; _ -> parserError $1 (text "Expected procedure or function")
        }
    }

designator_opt :: { Maybe Name }
designator_opt :
    {- empty -} { Nothing }
  | designator  { Just $1 }

subprogram_kind :: { () }
subprogram_kind :
    {- empty -} { () }
  | 'procedure' { () }
  | 'function'  { () }

{-
[§ 4.4]

subprogram_instantiation_declaration ::=
  subprogram_kind designator is new uninstantiated_subprogram_name [ signature ]
    [ generic_map_aspect ] ;
-}

subprogram_instantiation_declaration :: { Decl }
subprogram_instantiation_declaration :
    subprogram_specification 'is' 'new' name signature_opt generic_map_aspect_opt ';'
      {% do {f <- checkProcInst $1
            ; return $ f $4 $5 $6 $($1 `srcspan` $7)
            }
      }
  | 'function' designator 'is' 'new' name signature_opt generic_map_aspect_opt ';'
      { ProcInstD $2 $5 $6 $7 ($1 `srcspan` $8) }

{-
[§ 4.5]

signature ::= [ [ type_mark { , type_mark } ] [ return type_mark ] ]
-}

signature :: { Sig }
signature :
    '[' type_mark_rlist return_type_mark_opt ']'
      { Sig (rev $2) $3 ($1 `srcspan` $4) }

signature_opt :: { Maybe Sig }
signature_opt :
     {- empty -} { Nothing }
  |  signature   { Just $1 }

return_type_mark_opt :: { Maybe TypeMark }
return_type_mark_opt :
     {- empty -}        { Nothing }
  |  'return' type_mark { Just $2 }

{-
[§ 4.7]

package_declaration ::=
  package identifier is
    package_header
    package_declarative_part
  end [ package ] [ package_simple_name ] ;

package_header ::=
  [ generic_clause
  [ generic_map_aspect ; ] ]

package_declarative_part ::=
  { package_declarative_item }

package_declarative_item ::=
    subprogram_declaration
  | subprogram_instantiation_declaration
  | package_declaration
  | package_instantiation_declaration
  | type_declaration
  | subtype_declaration
  | constant_declaration
  | signal_declaration
  | variable_declaration
  | file_declaration
  | alias_declaration
  | component_declaration
  | attribute_declaration
  | attribute_specification
  | disconnection_specification
  | use_clause
  | group_template_declaration
  | group_declaration
  | PSL_Property_Declaration
  | PSL_Sequence_Declaration
-}

package_declaration :: { Decl }
package_declaration :
  'package' identifier 'is'
    generic_header_opt
    declarations_rlist
  'end' package_opt simple_name_opt ';'
    { PkgD $2 $4 (rev $5) ($1 `srcspan` $9) }

package_opt :: { () }
package_opt :
    {- empty -} { () }
  | 'package'   { () }

{-
[§ 4.8]

package_body ::=
  package body package_simple_name is
    package_body_declarative_part
  end [ package body ] [ package_simple_name ] ;

package_body_declarative_part ::=
  { package_body_declarative_item }

package_body_declarative_item ::=
    subprogram_declaration
  | subprogram_body
  | subprogram_instantiation_declaration
  | package_declaration
  | package_body
  | package_instantiation_declaration
  | type_declaration
  | subtype_declaration
  | constant_declaration
  | variable_declaration
  | file_declaration
  | alias_declaration
  | attribute_declaration
  | attribute_specification
  | use_clause
  | group_template_declaration
  | group_declaration
-}

package_body :: { Decl }
package_body :
  'package' 'body' simple_name 'is'
    declarations_rlist
  'end' package_body_opt simple_name_opt ';'
    { PkgBodyD $3 (rev $5) ($1 `srcspan` $9) }

package_body_opt :: { () }
package_body_opt :
    {- empty -}      { () }
  | 'package' 'body' { () }

{-
[§ 4.9]

package_instantiation_declaration ::=
  package identifier is new uninstantiated_package_name
    [ generic_map_aspect ] ;
-}

package_instantiation_declaration :: { Decl }
package_instantiation_declaration :
  'package' identifier 'is' 'new' name generic_map_aspect_opt ';'
    { PkgInstD $2 $5 $6 ($1 `srcspan` $7) }

{-
[§ 5.2.1]

scalar_type_definition ::=
    enumeration_type_definition
  | integer_type_definition
  | floating_type_definition
  | physical_type_definition

range_constraint ::= range range

range ::=
    range_attribute_name
  | simple_expression direction simple_expression

direction ::= to | downto
-}

scalar_type_definition :: { Type }
scalar_type_definition :
    enumeration_type_definition { $1 }
  | integer_type_definition     { $1 }
  -- We can't differentiate between this and an integer_type_definition.
  -- | floating_type_definition    { $1 }
  | physical_type_definition    { $1 }

range_constraint :: { Range }
range_constraint : 'range' range { $2 }

range :: { Range }
range :
    simple_expression direction simple_expression %prec DIRECTION
      {% do { e1 <- checkExp $1
            ; e2 <- checkExp $3
            ; pure $ Range e1 $2 e2 ($1 `srcspan` $3)
            }
      }

direction :: { Direction }
direction :
     'to'     { To (srclocOf $1) }
  |  'downto' { DownTo (srclocOf $1) }

{-
[§ 5.2.2]

enumeration_type_definition ::=
  ( enumeration_literal { , enumeration_literal } )

enumeration_literal ::= identifier | character_literal
-}

enumeration_type_definition :: { Type }
enumeration_type_definition :
    '(' enumeration_literal_rlist ')'
      { EnumT (rev $2) ($1 `srcspan` $3) }

enumeration_literal :: { Lit }
enumeration_literal :
     identifier        { IdLit $1 (srclocOf $1) }
  |  character_literal { $1 }

enumeration_literal_rlist :: { RevList Lit }
enumeration_literal_rlist :
    enumeration_literal                               { rsingleton $1 }
  | enumeration_literal_rlist ',' enumeration_literal { rcons $3 $1 }

{-
[§ 5.2.3]

integer_type_definition ::= range_constraint
-}

integer_type_definition :: { Type }
integer_type_definition : range_constraint { IntT $1 (srclocOf $1) }

{-
[§ 5.2.4]

physical_type_definition ::=
  range_constraint
    units
      primary_unit_declaration
      { secondary_unit_declaration }
    end units [ physical_type_simple_name ]

primary_unit_declaration ::= identifier ;

secondary_unit_declaration ::= identifier = physical_literal ;

physical_literal ::= [ abstract_literal ] unit_name
-}

physical_type_definition :: { Type }
physical_type_definition :
    range_constraint 'units'
      primary_unit_declaration
    'end' 'units' simple_name_opt
      { PhysT $1 $3 [] $6 ($1 `srcspan` $6) }
  | range_constraint 'units'
      primary_unit_declaration
      secondary_unit_declaration_rlist
    'end' 'units' simple_name_opt
      { PhysT $1 $3 (rev $4) $7 ($1 `srcspan` $7 )}

primary_unit_declaration :: { Id }
primary_unit_declaration : identifier ';' { $1 }

secondary_unit_declaration_rlist :: { RevList (Id, Lit) }
secondary_unit_declaration_rlist :
    secondary_unit_declaration
      { rsingleton $1 }
  | secondary_unit_declaration_rlist secondary_unit_declaration
      { rcons $2 $1 }

secondary_unit_declaration :: { (Id, Lit) }
secondary_unit_declaration :
    identifier '=' physical_literal ';'
      { ($1, $3) }

physical_literal :: { Lit }
physical_literal :
-- XXX conflicts with a variable expression
--  name
--  { PhysLit Nothing $1 (srclocOf $1) }
  abstract_literal name
    { PhysLit (Just $1) $2 ($1 `srcspan` $2) }

{-
[§ 5.2.5]

floating_type_definition ::= range_constraint
-}

{-
floating_type_definition :: { Type }
floating_type_definition : range_constraint { FloatT $1 (srclocOf $1) }
-}

{-
[§ 5.3.1]

composite_type_definition ::=
    array_type_definition
  | record_type_definition
-}

composite_type_definition :: { Type }
composite_type_definition :
    array_type_definition  { $1 }
  | record_type_definition { $1 }

{-
[§ 5.3.2]

array_type_definition ::=
  unbounded_array_definition | constrained_array_definition

unbounded_array_definition ::=
  array ( index_subtype_definition { , index_subtype_definition } )
    of element_subtype_indication

constrained_array_definition ::=
  array index_constraint of element_subtype_indication

index_subtype_definition ::= type_mark range <>

array_constraint ::=
    index_constraint [ array_element_constraint ]
  | ( open ) [ array_element_constraint ]

array_element_constraint ::= element_constraint

index_constraint ::= ( discrete_range { , discrete_range } )

discrete_range ::= discrete_subtype_indication | range
-}

array_type_definition :: { Type }
array_type_definition :
    unbounded_array_definition   { $1 }
  | constrained_array_definition { $1 }

unbounded_array_definition :: { Type }
unbounded_array_definition :
    'array' '(' index_subtype_definition_rlist ')'
      'of' element_subtype_indication
        { ArrUnboundedT (rev $3) $6 ($1 `srcspan` $6) }

constrained_array_definition :: { Type }
constrained_array_definition :
  'array' index_constraint 'of' element_subtype_indication
    { ArrT $2 $4 ($1 `srcspan` $4) }

index_subtype_definition :: { TypeMark }
index_subtype_definition :
    type_mark 'range' '<>' { $1 }

index_subtype_definition_rlist :: { RevList TypeMark }
index_subtype_definition_rlist :
    index_subtype_definition
      { rsingleton $1 }
  | index_subtype_definition_rlist ',' index_subtype_definition
      { rcons $3 $1 }

{-
array_constraint :: { Constraint }
array_constraint :
  '(' expression_rlist ')' array_element_constraint_opt
    {% checkArrayConstraint (ParensR (rev $2) ($1 `srcspan` $3)) $4 }
-}

array_element_constraint :: { Constraint }
array_element_constraint : element_constraint { $1 }

{-
array_element_constraint_opt :: { Maybe Constraint }
array_element_constraint_opt :
    {- empty -}              { Nothing }
  | array_element_constraint { Just $1 }
-}

index_constraint :: { IndexConstraint }
index_constraint :
    '(' expression_rlist ')'
      {% checkIndexConstraint (ParensR (rev $2) ($1 `srcspan` $3)) }

discrete_range :: { DiscreteRange }
discrete_range :
    range              { RangeDR $1 (srclocOf $1) }
  | subtype_indication { SubtypeDR $1 (srclocOf $1) }

{-
[§ 5.3.3]
record_type_definition ::=
  record
    element_declaration
    { element_declaration }
  end record [ record_type_simple_name ]

element_declaration ::= identifier_list : element_subtype_definition ;

identifier_list ::= identifier { , identifier }

element_subtype_definition ::= subtype_indication

record_constraint ::=
  ( record_element_constraint { , record_element_constraint } )

record_element_constraint ::= record_element_simple_name element_constraint
-}

record_type_definition :: { Type }
record_type_definition :
    'record' element_declaration_rlist 'end' 'record' simple_name_opt
      { RecT (rev $2) $5 ($1 `srcspan` $5) }

element_declaration_rlist :: { RevList ([Id], Subtype) }
element_declaration_rlist :
    element_declaration
      { rsingleton $1 }
  | element_declaration_rlist element_declaration
      { rcons $2 $1 }

element_declaration :: { ([Id], Subtype) }
element_declaration :
    identifier_list ':' element_subtype_definition ';'
        { ($1, $3) }

identifier_list :: { [Id] }
identifier_list : identifier_rlist { rev $1 }

identifier_rlist :: { RevList Id }
identifier_rlist :
    identifier                      { rsingleton $1 }
  | identifier_rlist ',' identifier { rcons $3 $1 }

element_subtype_definition :: { Subtype }
element_subtype_definition : subtype_indication { $1 }

{-
record_constraint :: { Constraint }
record_constraint :
    '(' record_element_constraint_rlist ')'
      { RecordC (map unLoc (rev $2)) ($1 `srcspan` $2) }

record_element_constraint_rlist :: { RevList (L (Name, Constraint)) }
record_element_constraint_rlist :
    record_element_constraint
      { rsingleton $1 }
  | record_element_constraint_rlist ',' record_element_constraint
      { rcons $3 $1 }

record_element_constraint :: { L (Name, Constraint) }
record_element_constraint :
    record_element_simple_name element_constraint
      { L ($1 <--> $2) ($1, $2) }
-}

{-
[§ 5.4.1]

access_type_definition ::= access subtype_indication
-}

access_type_definition :: { Type }
access_type_definition :
    'access' subtype_indication { AccessT $2 ($1 `srcspan` $2) }

{-
[§ 5.4.2]

incomplete_type_declaration ::= type identifier ;
-}

incomplete_type_declaration :: { Decl }
incomplete_type_declaration :
    'type' identifier ';'
      {% do { addTypeName $2
            ; return $ TypeD $2 Nothing ($1 `srcspan` $3)
            }
      }

{-
[§ 5.5]

file_type_definition ::= file of type_mark
-}

file_type_definition :: { Type }
file_type_definition :
    'file' 'of' type_mark { FileT $3 ($1 `srcspan` $3) }

{-
[§ 5.6]

protected_type_definition ::=
    protected_type_declaration
  | protected_type_body

protected_type_declaration ::=
  protected
    protected_type_declarative_part
  end protected [ protected_type_simple_name ]

protected_type_declarative_part ::=
  { protected_type_declarative_item }

protected_type_declarative_item ::=
    subprogram_declaration
  | subprogram_instantiation_declaration
  | attribute_specification
  | use_clause

protected_type_body ::=
  protected body
    protected_type_body_declarative_part
  end protected body [ protected_type_simple name ]

protected_type_body_declarative_part ::=
  { protected_type_body_declarative_item }

protected_type_body_declarative_item ::=
    subprogram_declaration
  | subprogram_body
  | subprogram_instantiation_declaration
  | package_declaration
  | package_body
  | package_instantiation_declaration
  | type_declaration
  | subtype_declaration
  | constant_declaration
  | variable_declaration
  | file_declaration
  | alias_declaration
  | attribute_declaration
  | attribute_specification
  | use_clause
  | group_template_declaration
  | group_declaration
-}

protected_type_definition :: { Type }
protected_type_definition :
    protected_type_declaration { $1 }
  | protected_type_body        { $1 }

protected_type_declaration :: { Type }
protected_type_declaration :
  'protected'
    declarations_rlist
  'end' 'protected' simple_name_opt
    { ProtectedT (rev $2) ($1 `srcspan` $5) }

protected_type_body :: { Type }
protected_type_body :
  'protected' 'body'
    declarations_rlist
  'end' 'body' 'protected' simple_name_opt
    { ProtectedBodyT (rev $3) ($1 `srcspan` $5) }

{-
[§ 6] Declarations
-}

declaration :: { Decl }
declaration :
    entity_declaration          { $1 }
  | architecture_body           { $1 }
  | configuration_declaration   { $1 }
  | subprogram_declaration      { $1 }
  | subprogram_body             { $1 }
  | subprogram_instantiation_declaration { $1 }
  | package_declaration         { $1 }
  | package_body                { $1 }
  | package_instantiation_declaration { $1 }
  | type_declaration            { $1 }
  | subtype_declaration         { $1 }
  | constant_declaration        { $1 }
  | signal_declaration          { $1 }
  | variable_declaration        { $1 }
  | file_declaration            { $1 }
  | alias_declaration           { $1 }
  | attribute_declaration       { $1 }
  | component_declaration       { $1 }
  | group_template_declaration  { $1 }
  | group_declaration           { $1 }
  | attribute_specification     { $1 }
  | configuration_specification { $1 }
  | disconnection_specification { $1 }
  | context_declaration         { $1 }
  | use_clause                  { UseD $1 (srclocOf $1) }

declarations_rlist :: { RevList Decl }
declarations_rlist :
    {- empty -}                   { rnil }
 | declarations_rlist declaration { rcons $2 $1 }

{-
[§ 6.2]
type_declaration ::=
    full_type_declaration
  | incomplete_type_declaration

full_type_declaration ::=
    type identifier is type_definition ;

type_definition ::=
    scalar_type_definition
  | composite_type_definition
  | access_type_definition
  | file_type_definition
  | protected_type_definition
-}

type_declaration :: { Decl }
type_declaration :
    full_type_declaration       { $1 }
  | incomplete_type_declaration { $1 }

full_type_declaration :: { Decl }
full_type_declaration :
    'type' identifier 'is' type_definition ';'
      {% do { addTypeName $2
            ; return $ TypeD $2 (Just $4) ($1 `srcspan` $5)
            }
      }

type_definition :: { Type }
type_definition :
    scalar_type_definition    { $1 }
  | composite_type_definition { $1 }
  | access_type_definition    { $1 }
  | file_type_definition      { $1 }
  | protected_type_definition { $1 }

{-
[§ 6.3]

subtype_declaration ::=
  subtype identifier is subtype_indication ;

subtype_indication ::=
  [ resolution_indication ] type_mark [ constraint ]

resolution_indication ::=
  resolution_function_name | ( element_resolution )

element_resolution ::= array_element_resolution | record_resolution

array_element_resolution ::= resolution_indication

record_resolution ::= record_element_resolution { , record_element_resolution }

record_element_resolution ::= record_element_simple_name resolution_indication

type_mark ::=
    type_name
  | subtype_name

constraint ::=
    range_constraint
  | array_constraint
  | record_constraint

element_constraint ::=
    array_constraint
  | record_constraint
-}

subtype_declaration :: { Decl }
subtype_declaration :
    'subtype' identifier 'is' subtype_indication ';'
      { SubtypeD $2 $4 ($1 `srcspan` $5) }

subtype_indication :: { Subtype }
subtype_indication :
    type_mark constraint
      { Subtype Nothing $1 (Just $2) ($1 `srcspan` $2) }
  | subtype_indication_
      { $1 }

subtype_indication_ :: { Subtype }
subtype_indication_ :
    type_mark
      { Subtype Nothing $1 Nothing (srclocOf $1) }
  | resolution_indication type_mark constraint_opt
      { Subtype (Just $1) $2 $3 ($1 `srcspan` $3) }
  | ANTI_TYPE
      { AntiType (getANTI_TYPE $1) (srclocOf $1) }

element_subtype_indication :: { Subtype }
element_subtype_indication : subtype_indication { $1 }

resolution_indication :: { Resolution }
resolution_indication :
    resolution_function_name   { FunRes $1 (srclocOf $1) }
-- XXX This causes a conflict with parenthesized expressions resulting from the
-- discrete_range non-terminal. We will leave this unresolved for now, since
-- array element resolution is less likely to be used that function calls :)
{-
  | '(' element_resolution ')' { $2 }
-}

resolution_function_name :: { Name }
resolution_function_name : function_name { $1 }

{-
element_resolution :: { Resolution }
element_resolution :
    array_element_resolution { $1 }
  | record_resolution        { $1 }

array_element_resolution :: { Resolution }
array_element_resolution : resolution_indication { $1 }

record_resolution :: { Resolution }
record_resolution :
    record_element_resolution_rlist
      { RecordRes (map unLoc (rev $1)) (srclocOf $1) }

record_element_resolution_rlist :: { RevList (L (Name, Resolution)) }
record_element_resolution_rlist :
    record_element_resolution
      { rsingleton $1 }
  | record_element_resolution_rlist ',' record_element_resolution
      { rcons $3 $1 }

record_element_resolution :: { L (Name, Resolution) }
record_element_resolution :
    record_element_simple_name resolution_indication
      { L ($1 <--> $2) ($1, $2) }
-}

type_mark :: { TypeMark }
type_mark : type_name { $1 }

type_mark_rlist :: { RevList TypeMark }
type_mark_rlist :
    type_mark                     { rsingleton $1 }
  | type_mark_rlist ',' type_mark { rcons $3 $1 }

constraint :: { Constraint }
constraint :
    range_constraint   { RangeC $1 (srclocOf $1) }
  | element_constraint { $1 }

constraint_opt :: { Maybe Constraint }
constraint_opt :
    {- empty -} { Nothing }
  | constraint  { Just $1 }

element_constraint :: { Constraint }
element_constraint :
  -- array_constraint | record_constraint
    '(' expression_rlist ')'
      {% checkElementConstraint (ParensR (rev $2) ($1 `srcspan` $3)) }
  -- array_constraint
  | '(' expression_rlist ')' array_element_constraint
      {% checkArrayConstraint (ParensR (rev $2) ($1 `srcspan` $3)) (Just $4) }

{-
[§ 6.4]

object_declaration ::=
    constant_declaration
  | signal_declaration
  | variable_declaration
  | file_declaration

constant_declaration ::=
  constant identifier_list : subtype_indication [ := expression ] ;

signal_declaration ::=
  signal identifier_list : subtype_indication [ signal_kind ] [ := expression ] ;

signal_kind ::= register | bus

variable_declaration ::=
  [ shared ] variable identifier_list : subtype_indication [ := expression ] ;

file_declaration ::=
  file identifier_list : subtype_indication [ file_open_information ] ;

file_open_information ::= [ open file_open_kind_expression ] is file_logical_name

file_logical_name ::= string_expression
-}

constant_declaration :: { Decl }
constant_declaration :
    'constant' identifier_list ':' subtype_indication assn_exp_opt ';'
      { ConstD $2 $4 $5 ($1 `srcspan` $6) }

signal_declaration :: { Decl }
signal_declaration :
    'signal' identifier_list ':' subtype_indication signal_kind_opt assn_exp_opt ';'
      { SignalD $2 $4 $5 $6 ($1 `srcspan` $7) }

signal_kind :: { SignalKind }
signal_kind :
    'register' { Register (srclocOf $1) }
  | 'bus'      { Bus (srclocOf $1) }

signal_kind_opt :: { Maybe SignalKind }
signal_kind_opt :
     {- empty -} { Nothing }
   | signal_kind { Just $1 }

variable_declaration :: { Decl }
variable_declaration :
    shared_opt 'variable' identifier_list ':' subtype_indication assn_exp_opt ';'
      { VarD (unLoc $1) $3 $5 $6 ($1 `srcspan` $7) }

shared_opt :: { L Bool }
shared_opt :
   {- empty -} { L noLoc False }
 | 'shared'    { L (locOf $1) True }

assn_exp_opt :: { Maybe Exp }
assn_exp_opt :
    {- empty -}
      { Nothing }
  | ':=' expression
    {% do { e <- checkExp $2
          ; pure $ Just e
          }
    }

file_declaration :: { Decl }
file_declaration :
    'file' identifier_list ':' subtype_indication file_open_information_opt ';'
      { FileD $2 $4 $5 ($1 `srcspan` $6) }

file_open_information_opt  :: { Maybe FileOpenInfo }
file_open_information_opt :
    {- empty -}           { Nothing }
  | file_open_information { Just $1 }

file_open_information :: { FileOpenInfo }
file_open_information :
    'open' expression 'is' expression
      {% do { kind <- checkExp $2
            ; name <- checkExp $4
            ; pure $ FileOpenInfo (Just kind) name
            }
      }
  | 'is' expression
      {% do { name <- checkExp $2
            ; pure $ FileOpenInfo Nothing name
            }
      }

{-
[§ 6.5.2]

interface_declaration ::=
    interface_object_declaration
  | interface_type_declaration
  | interface_subprogram_declaration
  | interface_package_declaration

interface_object_declaration ::=
    interface_constant_declaration
  | interface_signal_declaration
  | interface_variable_declaration
  | interface_file_declaration

interface_constant_declaration ::=
  [ constant ] identifier_list : [ in ] subtype_indication [ := static_expression ]

interface_signal_declaration ::=
  [ signal ] identifier_list : [ mode ] subtype_indication [ bus ] [ := static_expression ]

interface_variable_declaration ::=
  [ variable ] identifier_list : [ mode ] subtype_indication [ := static_expression ]

interface_file_declaration ::=
  file identifier_list : subtype_indication

mode ::= in | out | inout | buffer | linkage
-}

interface_declaration :: { IDecl }
interface_declaration :
    interface_object_declaration     { $1 }
  | interface_type_declaration       { $1 }
  | interface_subprogram_declaration { $1 }
  | interface_package_declaration    { $1 }

interface_object_declaration :: { IDecl }
interface_object_declaration :
    interface_constant_declaration { $1 }
  | interface_signal_declaration   { $1 }
  | interface_variable_declaration { $1 }
  | interface_file_declaration     { $1 }

interface_constant_declaration :: { IDecl }
interface_constant_declaration :
    'constant' identifier_list ':' in_opt subtype_indication assn_exp_opt
      { ConstID $2 $5 $6 ($1 `srcspan` $6) }

interface_signal_declaration :: { IDecl }
interface_signal_declaration :
    'signal' identifier_list ':' mode_opt subtype_indication bus_opt assn_exp_opt
      { SignalID $2 $4 $5 $6 $7 ($1 `srcspan` $7) }

interface_variable_declaration :: { IDecl }
interface_variable_declaration :
    'variable' identifier_list ':' mode_opt subtype_indication assn_exp_opt
      { VarID True $2 $4 $5 $6 ($1 `srcspan` $6) }
  | identifier_list ':' mode_opt subtype_indication assn_exp_opt
      { VarID False $1 $3 $4 $5 ($1 `srcspan` $5) }

interface_file_declaration :: { IDecl }
interface_file_declaration :
    'file' identifier_list ':' subtype_indication
      { FileID $2 $4 ($1 `srcspan` $4) }

in_opt :: { () }
in_opt :
    {- empty -} { () }
  | 'in'        { () }

mode :: { Mode }
mode :
    'in'      { In }
  | 'out'     { Out }
  | 'inout'   { InOut }
  | 'buffer'  { Buffer }
  | 'linkage' { Linkage }

mode_opt :: { Mode }
mode_opt :
    {- empty -} { In }
  | mode        { $1 }

bus_opt :: { Maybe SignalKind }
bus_opt :
    {- empty -} { Nothing }
  | 'bus'       { Just (Bus (srclocOf $1)) }

{-
[§ 6.5.3]

interface_type_declaration ::=
  interface_incomplete_type_declaration

interface_incomplete_type_declaration ::= type identifier
-}

interface_type_declaration :: { IDecl }
interface_type_declaration : interface_incomplete_type_declaration { $1 }

interface_incomplete_type_declaration :: { IDecl }
interface_incomplete_type_declaration :
    'type' identifier
      {% do { addTypeName $2
            ; return $ TypeID $2 ($1 `srcspan` $2)
            }
      }

{-
[§ 6.5.4]
interface_subprogram_declaration ::=
  interface_subprogram_specification [ is interface_subprogram_default ]

interface_subprogram_specification ::=
  interface_procedure_specification | interface_function_specification

interface_procedure_specification ::=
  procedure designator
    [ [ parameter ] ( formal_parameter_list ) ]

interface_function_specification ::=
  [ pure | impure ] function designator
    [ [ parameter ] ( formal_parameter_list ) ] return type_mark

interface_subprogram_default ::= subprogram_name | <>
-}

interface_subprogram_declaration :: { IDecl }
interface_subprogram_declaration :
    interface_procedure_specification { $1 }
  | interface_function_specification  { $1 }

interface_procedure_specification :: { IDecl }
interface_procedure_specification :
    'procedure' designator interface_subprogram_default
      { ProcID $2 [] $3 ($1 `srcspan` $3) }
  | 'procedure' designator formal_parameter_list interface_subprogram_default
      { ProcID $2 $3 $4 ($1 `srcspan` $4) }

interface_function_specification :: { IDecl }
interface_function_specification :
    'function' designator 'return' type_mark interface_subprogram_default
      { FunID $2 Nothing [] $4 $5 ($1 `srcspan` $5) }
  | purity 'function' designator 'return' type_mark interface_subprogram_default
      { FunID $3 (Just $1) [] $5 $6 ($1 `srcspan` $6) }
  | 'function' designator formal_parameter_list 'return' type_mark interface_subprogram_default
      { FunID $2 Nothing $3 $5 $6 ($1 `srcspan` $6) }
  | purity 'function' designator formal_parameter_list 'return' type_mark interface_subprogram_default
      { FunID $3 (Just $1) $4 $6 $7 ($1 `srcspan` $7) }

interface_subprogram_default :: { Maybe InterfaceSubprogramDefault }
interface_subprogram_default :
    {- empty -} { Nothing }
  | 'is' name { Just (SubprogramD $2 (srclocOf $2)) }
  | 'is' '<>' { Just (AllD ($2 `srcspan` $2)) }

{-
[§ 6.5.5]
interface_package_declaration ::=
  package identifier is new uninstantiated_package_name interface_package_generic_map_aspect

interface_package_generic_map_aspect ::=
    generic_map_aspect
  | generic map ( <> )
  | generic map ( default )
-}

interface_package_declaration :: { IDecl }
interface_package_declaration :
  'package' identifier 'is' 'new' name interface_package_generic_map_aspect
    { PkgInstID $2 $5 $6 ($1 `srcspan` $6) }

interface_package_generic_map_aspect :: { IfaceGenericMapAspect }
interface_package_generic_map_aspect :
    'generic' 'map' '(' association_list ')'
      { IGenericMapAspect $4 ($1 `srcspan` $5) }
  | 'generic' 'map' '(' '<>' ')'
      { IGenericMapAll ($1 `srcspan` $5) }
  | 'generic' 'map' '(' 'default' ')'
      { IGenericMapDefault ($1 `srcspan` $5) }

{-
[§ 6.5.6]
interface_list ::=
  interface_element { ; interface_element }

interface_element ::= interface_declaration

generic_clause ::=
  generic ( generic_list ) ;

generic_list ::= generic_interface_list

port_clause ::=
  port ( port_list ) ;

port_list ::= port_interface_list
-}

interface_list :: { [IDecl] }
interface_list : interface_element_rlist { rev $1 }

interface_element_rlist :: { RevList IDecl }
interface_element_rlist :
    interface_element                             { rsingleton $1 }
  | interface_element_rlist ';' interface_element { rcons $3 $1 }

interface_element :: { IDecl }
interface_element : interface_declaration { $1 }

generic_clause :: { GenericClause }
generic_clause :
  'generic' '(' interface_list ')' ';'
    { GenericClause $3 ($1 `srcspan` $5) }

generic_clause_opt :: { Maybe GenericClause }
generic_clause_opt :
    {- empty -}    { Nothing }
  | generic_clause { Just $1 }

port_clause :: { PortClause }
port_clause :
  'port' '(' interface_list ')' ';'
    { PortClause $3 ($1 `srcspan` $5) }

port_clause_opt :: { Maybe PortClause }
port_clause_opt :
    {- empty -} { Nothing }
  | port_clause { Just $1 }

{-
[§ 6.5.7]

association_list ::=
  association_element { , association_element }

association_element ::=
  [ formal_part => ] actual_part

formal_part ::=
    formal_designator
  | function_name ( formal_designator )
  | type_mark ( formal_designator )

formal_designator ::=
    generic_name
  | port_name
  | parameter_name

actual_part ::=
    actual_designator
  | function_name ( actual_designator )
  | type_mark ( actual_designator )

actual_designator ::=
    [ inertial ] expression
  | signal_name
  | variable_name
  | file_name
  | subtype_indication
  | subprogram_name
  | instantiated_package_name
  | open

generic_map_aspect ::=
  generic map ( generic_association_list )

port_map_aspect ::=
  port map ( port_association_list )
-}

association_list :: { [AssocElem] }
association_list : expression_rlist {% mapM checkArg (rev $1) }

generic_map_aspect :: { GenericMapAspect }
generic_map_aspect :
  'generic' 'map' '(' association_list ')'
    { GenericMapAspect $4 ($1 `srcspan` $5) }

generic_map_aspect_opt :: { Maybe GenericMapAspect }
generic_map_aspect_opt :
    {- empty -}        { Nothing }
  | generic_map_aspect { Just $1 }

port_map_aspect :: { PortMapAspect }
port_map_aspect :
  'port' 'map' '(' port_association_list ')'
    { PortMapAspect $4 ($1 `srcspan` $5) }

port_map_aspect_opt :: { Maybe PortMapAspect }
port_map_aspect_opt :
    {- empty -}     { Nothing }
  | port_map_aspect { Just $1 }

port_association_list :: { [AssocElem] }
port_association_list : association_list { $1 }

{-
[§ 6.6]

alias_declaration ::=
  alias alias_designator [ : subtype_indication ] is name [ signature ] ;

alias_designator ::= identifier | character_literal | operator_symbol
-}

alias_declaration :: { Decl }
alias_declaration :
  'alias' alias_designator subtype_indication_opt 'is' name signature_opt ';'
    { AliasD $2 $3 $5 $6 ($1 `srcspan` $7) }

alias_designator :: { Name }
alias_designator : name { $1 }

subtype_indication_opt :: { Maybe Subtype }
subtype_indication_opt :
    {- empty -}            { Nothing }
  | ':' subtype_indication { Just $2 }

{-
[§ 6.7]

attribute_declaration ::=
  attribute identifier : type_mark ;
-}

attribute_declaration :: { Decl }
attribute_declaration :
    'attribute' identifier ':' type_mark ';'
      { AttrD $2 $4 ($1 `srcspan` $5) }

{-
[§ 6.8]

component_declaration ::=
  component identifier [ is ]
    [ local_generic_clause ]
    [ local_port_clause ]
  end component [ component_simple_name ] ;
-}

component_declaration :: { Decl }
component_declaration :
    'component' identifier is_opt
        generic_clause_opt
        port_clause_opt
    'end' 'component' simple_name_opt ';'
      { ComponentD $2 $4 $5 $8 ($1 `srcspan` $9)}

is_opt :: { () }
is_opt :
    {- empty -} { () }
  | 'is'        { () }

{-
[§ 6.9]

group_template_declaration ::=
  group identifier is ( entity_class_entry_list ) ;

entity_class_entry_list ::=
  entity_class_entry { , entity_class_entry }

entity_class_entry ::= entity_class [ <> ]
-}

group_template_declaration :: { Decl }
group_template_declaration :
    'group' identifier 'is' '(' entity_class_entry_list ')' ';'
      { GroupTemplateD $2 (fst $5) (snd $5) ($1 `srcspan` $7) }

entity_class_entry_list :: { ([EntityClass], Bool )}
entity_class_entry_list :
    entity_class_rlist      { (map unLoc (rev $1), False) }
  | entity_class_rlist '<>' { (map unLoc (rev $1), True) }

{-
[§ 6.10]

group_declaration ::=
  group identifier : group_template_name ( group_constituent_list ) ;

group_constituent_list ::= group_constituent { , group_constituent }

group_constituent ::= name | character_literal
-}

group_declaration :: { Decl }
group_declaration :
    'group' identifier ':' name '(' name_rlist ')' ';'
      { GroupD $2 $4 (rev $6) ($1 `srcspan` $8) }

{-
[§ 7.2]

attribute_specification ::=
  attribute attribute_designator of entity_specification is expression ;

entity_specification ::=
  entity_name_list : entity_class

entity_class ::=
    entity
  | architecture
  | configuration
  | procedure
  | function
  | package
  | type
  | subtype
  | constant
  | signal
  | variable
  | component
  | label
  | literal
  | units
  | group
  | file
  | property
  | sequence

entity_name_list ::=
    entity_designator { , entity_designator }
  | others
  | all

entity_designator ::= entity_tag [ signature ]

entity_tag ::= simple_name | character_literal | operator_symbol
-}

attribute_specification :: { Decl }
attribute_specification :
    'attribute' attribute_designator 'of' entity_specification 'is' expression ';'
      {% do { e <- checkExp $6
            ; pure $ AttrSpecD $2 (fst $4) (snd $4) e ($1 `srcspan` $7)
            }
      }

entity_specification :: { (NameList EntityDesignator, EntityClass) }
entity_specification :
    entity_name_list ':' entity_class { ($1, unLoc $3) }

entity_class :: { L EntityClass }
entity_class :
    'entity'        { L (locOf $1) EntityC }
  | 'architecture'  { L (locOf $1) ArchitectureC }
  | 'configuration' { L (locOf $1) ConfigurationC }
  | 'procedure'     { L (locOf $1) ProcedureC }
  | 'function'      { L (locOf $1) FunctionC }
  | 'package'       { L (locOf $1) PackageC }
  | 'type'          { L (locOf $1) TypeC }
  | 'subtype'       { L (locOf $1) SubtypeC }
  | 'constant'      { L (locOf $1) ConstantC }
  | 'signal'        { L (locOf $1) SignalC }
  | 'variable'      { L (locOf $1) VariableC }
  | 'component'     { L (locOf $1) ComponentC }
  | 'label'         { L (locOf $1) LabelC }
  | 'literal'       { L (locOf $1) LiteralC }
  | 'units'         { L (locOf $1) UnitsC }
  | 'group'         { L (locOf $1) GroupC }
  | 'file'          { L (locOf $1) FileC }
  | 'property'      { L (locOf $1) PropertyC }
  | 'sequence'      { L (locOf $1) SequenceC }

entity_class_rlist :: { RevList (L EntityClass) }
entity_class_rlist :
    entity_class                        { rsingleton $1 }
  | entity_class_rlist ',' entity_class { rcons $3 $1 }

entity_name_list :: { NameList EntityDesignator }
entity_name_list :
    entity_designator_rlist { Some (rev $1) (srclocOf $1) }
  | 'others'                { Others (srclocOf $1) }
  | 'all'                   { All (srclocOf $1) }

entity_designator_rlist :: { RevList EntityDesignator }
entity_designator_rlist :
    entity_designator                             { rsingleton $1 }
  | entity_designator_rlist ',' entity_designator { rcons $3 $1 }

entity_designator :: { EntityDesignator }
entity_designator :
    entity_tag
      { EntityDesignator $1 Nothing (srclocOf $1) }
  | entity_tag signature
      { EntityDesignator $1 (Just $2) ($1 `srcspan` $2) }

entity_tag :: { Name }
entity_tag :
  name_ { $1 [] }

{-
[§ 7.3]

configuration_specification ::=
    simple_configuration_specification
  | compound_configuration_specification

simple_configuration_specification ::=
  for component_specification binding_indication ;
  [ end for ; ]

compound_configuration_specification ::=
  for component_specification binding_indication ;
    verification_unit_binding_indication ;
    { verification_unit_binding_indication ; }
  end for ;

component_specification ::=
  instantiation_list : component_name

instantiation_list ::=
    instantiation_label { , instantiation_label }
  | others
  | all

binding_indication ::=
  [ use entity_aspect ]
  [ generic_map_aspect ]
  [ port_map_aspect ]

entity_aspect ::=
    entity entity_name [ ( architecture_identifier ) ]
  | configuration configuration_name
  | open

verification_unit_binding_indication ::=
  use vunit verification_unit_list

verification_unit_list ::= verification_unit_name { , verification_unit_name }
-}

configuration_specification :: { Decl }
configuration_specification :
    'for' component_specification
      binding_indications
    'end' 'for' ';'
      { ConfigSpecD $2 $3 ($1 `srcspan` $6) }

component_specification :: { ComponentSpec }
component_specification :
    instantiation_list ':' name
      { ComponentSpec $3 $1 ($1 `srcspan` $3) }

instantiation_list :: { NameList Label }
instantiation_list :
    label_rlist { Some (rev $1) (srclocOf $1) }
  | 'others'    { Others (srclocOf $1) }
  | 'all'       { All (srclocOf $1) }

binding_indication :: { BindingIndication }
binding_indication :
    entity_aspect generic_map_aspect_opt port_map_aspect_opt
      { EntityB $1 $2 $3 ($1 `srcspan` $3) }
  | 'use' 'vunit' verification_unit_list
      { VerifB (rev $3) ($1 `srcspan` $3) }

binding_indications :: { [BindingIndication] }
binding_indication_rlist : binding_indication_rlist { rev $1 }

binding_indication_rlist :: { RevList BindingIndication }
binding_indication_rlist :
    binding_indication ';'                          { rsingleton $1 }
  | binding_indication_rlist ';' binding_indication { rcons $3 $1 }

entity_aspect :: { EntityAspect }
entity_aspect :
    'entity' name
      { EntityEA $2 Nothing ($1 `srcspan` $2) }
  | 'entity' name '(' identifier ')'
      { EntityEA $2 (Just $4) ($1 `srcspan` $5) }
  | 'configuration' name
      { ConfigEA $2 ($1 `srcspan` $2) }
  | 'open'
      { OpenEA (srclocOf $1) }

verification_unit_list :: { RevList Name }
verification_unit_list :
    verification_unit_name                            { rsingleton $1 }
  | verification_unit_list ',' verification_unit_name { rcons $3 $1 }

{-
[§ 7.4]

disconnection_specification ::=
  disconnect guarded_signal_specification after time_expression ;

guarded_signal_specification ::=
  guarded_signal_list : type_mark

signal_list ::=
    signal_name { , signal_name }
  | others
  | all
-}

disconnection_specification :: { Decl }
disconnection_specification :
    'disconnect' guarded_signal_specification 'after' expression
      {% do { e <- checkExp $4
            ; pure $ DisconnectD (fst $2) (snd $2) e ($1 `srcspan` $4)
            }
      }

guarded_signal_specification :: { (SignalList, TypeMark) }
guarded_signal_specification :
    guarded_signal_list ':' type_mark { ($1, $3) }

guarded_signal_list :: { SignalList }
guarded_signal_list : signal_list { $1 }

signal_list :: { SignalList }
signal_list :
    name_rlist { Some (rev $1) (srclocOf $1) }
  | 'others'   { Others (srclocOf $1) }
  | 'all'      { All (srclocOf $1) }

{-
[§ 8.1]

name ::=
    simple_name
  | operator_symbol
  | character_literal
  | selected_name
  | indexed_name
  | slice_name
  | attribute_name
  | external_name

prefix ::=
    name
  | function_call

[§ 8.2]

simple_name ::= identifier

[§ 8.3]

selected_name ::= prefix . suffix

suffix ::=
    simple_name
  | character_literal
  | operator_symbol
  | all

[§ 8.4]

indexed_name ::= prefix ( expression { , expression } )

[§ 8.5]

slice_name ::= prefix ( discrete_range )

[§ 8.6]

attribute_name ::=
  prefix [ signature ] ' attribute_designator [ ( expression ) ]

attribute_designator ::= attribute_simple_name
-}

name :: { Name }
name :
    name_
      {$1 [] }
  | prefix name_
      { $2 (rev $1) }
  | prefix 'all'
      { let { ids = rev $1
            ; prefix = init ids
            ; n = last ids
            }
        in
          AllN (SimpleN prefix n (srclocOf $1)) ($1 `srcspan` $2)
      }
{-
  | name '(' expression_rlist ')'
      {% checkArrayIndexOrSlice $1 (rev $3) ($1 `srcspan` $4) }
-}
  | name signature_opt '\'' attribute_designator
      { AttrN $1 $2 $4 Nothing ($1 `srcspan` $4) }
{-
  | name signature_opt '\'' attribute_designator '(' expression ')'
      {% do { e <- checkExp $6
            ; pure $ AttrN $1 $2 $4 (Just e) ($1 `srcspan` $7)
            }
      }
-}
  | external_name
      { $1 }

name_ :: { [Id] -> Name }
name_ :
    identifier
      { \prefix -> SimpleN prefix $1 (prefix `srcspan` $1) }
  | operator_symbol
      { \prefix -> OpN prefix $1 (prefix `srcspan` $1) }
  | CHAR
      { let { (lit, _) = getCHAR $1 }
        in
          \prefix -> EnumN prefix lit (prefix `srcspan` $1)
      }

prefix :: { RevList Id }
prefix :
    identifier '.'
      { rsingleton $1 }
  | identifier error
      {% expected ["`.'"] (Just $ text "identifier" <+> quote (ppr $1) <+> text "that is part of a prefix") }
  | prefix identifier '.'
      { rcons $2 $1 }

simple_type_name :: { [Id] -> Name }
simple_type_name :
    type_identifier { \prefix -> SimpleN prefix $1 (prefix `srcspan` $1) }

type_name :: { Name }
type_name :
    simple_type_name
      { $1 [] }
  | prefix simple_type_name
      { $2 (rev $1) }
  | 'typename' name_
      { $2 [] }
  | 'typename' prefix name_
      { $3 (rev $2) }

name_rlist :: { RevList Name }
name_rlist :
    name                { rsingleton $1 }
  | name_rlist ',' name { rcons $3 $1 }

function_name :: { Name }
function_name : name { $1 }

verification_unit_name :: { Name }
verification_unit_name : name { $1 }

simple_name :: { Name }
simple_name : identifier { mkIdName $1 }

simple_name_opt :: { Maybe Name }
simple_name_opt :
    {- empty -} { Nothing }
  | simple_name { Just $1 }

attribute_simple_name :: { Name }
attribute_simple_name : simple_name { $1 }

{-
record_element_simple_name :: { Name }
record_element_simple_name : simple_name { $1 }
-}

attribute_designator :: { Name }
attribute_designator : attribute_simple_name { $1 }

{-
[§ 8.7]
external_name ::=
    external_constant_name
  | external_signal_name
  | external_variable_name

external_constant_name ::=
  << constant external_pathname : subtype_indication >>

external_signal_name ::=
  << signal external_pathname : subtype_indication >>

external_variable_name ::=
  << variable external_pathname : subtype_indication >>

external_pathname ::=
    package_pathname
  | absolute_pathname
  | relative_pathname

package_pathname ::=
  @ library_logical_name . package_simple_name . { package_simple_name . } object_simple_name

absolute_pathname ::= . partial_pathname

relative_pathname ::= { ^ . } partial_pathname

partial_pathname ::= { pathname_element . } object_simple_name

pathname_element ::=
    entity_simple_name
  | component_instantiation_label
  | block_label
  | generate_statement_label [ ( static_expression ) ]
  | package_simple_name
-}

external_name :: { Name }
external_name :
    external_constant_name { $1 }
  | external_signal_name   { $1 }
  | external_variable_name { $1 }

external_constant_name :: { Name }
external_constant_name :
  '<<' 'constant' external_pathname ':' subtype_indication '>>'
    { ExtConstN $3 $5 ($1 `srcspan` $6) }

external_signal_name :: { Name }
external_signal_name :
  '<<' 'signal' external_pathname ':' subtype_indication '>>'
    { ExtSigN $3 $5 ($1 `srcspan` $6) }

external_variable_name :: { Name }
external_variable_name :
  '<<' 'variable' external_pathname ':' subtype_indication '>>'
    { ExtVarN $3 $5 ($1 `srcspan` $6) }

external_pathname :: { ExtPath }
external_pathname :
    package_pathname  { $1 }
  | absolute_pathname { $1 }
  | relative_pathname { $1 }

package_pathname :: { ExtPath }
package_pathname :
  '@' identifier '.' package_simple_names identifier
    { PkgP ($2 : rev $4) $5 ($1 `srcspan` $5) }

package_simple_names :: { RevList Id }
package_simple_names :
    identifier '.'
      { rsingleton $1 }
  | identifier error
      {% expected ["`.'"] (Just $ text "identifier" <+> quote (ppr $1) <+> text "that is part of an external package name") }
  | package_simple_names identifier '.'
      { rcons $2 $1 }

absolute_pathname :: { ExtPath }
absolute_pathname :
  '.' partial_pathname { AbsP $2 ($1 `srcspan` $2) }

relative_pathname :: { ExtPath }
relative_pathname :
  carets partial_pathname { RelP (unLoc $1) $2 ($1 `srcspan` $2) }

carets :: { L Int }
carets :
    '^' '.'
      { L ($1 <--> $2) 1 }
  | '^' error
      {% expected ["`.'"] Nothing }
  | carets '^' '.'
      { let { L l n = $1 }
        in
          L (l <--> $3) (n+1)
      }

partial_pathname :: { PartialPath }
partial_pathname :
    simple_name
      { PartialPath [] $1 (srclocOf $1) }
  | pathname_elements simple_name
      { PartialPath (rev $1) $2 ($1 `srcspan` $2) }

pathname_elements :: { RevList PathElem }
pathname_elements :
    pathname_element '.'
      { rsingleton $1 }
  | pathname_element error
      {% expected ["`.'"] Nothing }
  | pathname_elements pathname_element '.'
      { rcons $2 $1 }

pathname_element :: { PathElem }
pathname_element :
    identifier
      { NameP $1 (srclocOf $1) }
  | identifier '(' expression ')'
      {% do { e <- checkExp $3
            ; pure $ GenLabelP $1 (Just e) ($1 `srcspan` $4)
            }
      }

{-
[§ 9.1]

expression ::=
    condition_operator primary
  | logical_expression

logical_expression ::=
    relation { and relation }
  | relation { or relation }
  | relation { xor relation }
  | relation [ nand relation ]
  | relation [ nor relation ]
  | relation { xnor relation }

relation ::=
  shift_expression [ relational_operator shift_expression ]

shift_expression ::=
  simple_expression [ shift_operator simple_expression ]

simple_expression ::=
  [ sign ] term { adding_operator term }

term ::=
  factor { multiplying_operator factor }

factor ::=
    primary [ ** primary ]
  | abs primary
  | not primary
  | logical_operator primary

primary ::=
    name
  | literal
  | aggregate
  | function_call
  | qualified_expression
  | type_conversion
  | allocator
  | ( expression )

[§ 9.2.1]
Note: operators are listed in order of precedence form low to high and associate
to the left.

condition_operator ::= ??

logical_operator ::= and | or | nand | nor | xor | xnor

relational_operator ::= = | /= | < | <= | > | >= | ?= | ?/= | ?< | ?<= | ?> | ?>=

shift_operator ::= sll | srl | sla | sra | rol | ror

adding_operator ::= + | – | &

sign ::= + | –

multiplying_operator ::= * | / | mod | rem

miscellaneous_operator ::= ** | abs | not
-}

only_expression :: { Exp }
only_expression :
    expression {% checkExp $1 }

expression :: { RichExp }
expression :
    expression_
      { $1 }
  | name ':' expression %prec LABEL
      {% do { ident <- checkIdentifier $1
            ; pure $ LabeledR ident $3 ($1 `srcspan` $3)
            }
      }
  | expression '|' expression_
      {% do { cs <- checkChoices $1
            ; c  <- checkChoice $3
            ; pure $ ChoicesR (c:cs) ($1 `srcspan` $3)
            }
      }
  | range
      { RangeR $1 (srclocOf $1) }


expression_ :: { RichExp }
expression_ :
    simple_expression              { $1 }
  | '??' primary                   {% unopRE Cond $2 }
  | expression_ 'and' expression_  {% binopRE And $1 $3 }
  | expression_ 'or' expression_   {% binopRE Or $1 $3 }
  | expression_ 'nand' expression_ {% binopRE Nand $1 $3 }
  | expression_ 'nor' expression_  {% binopRE Nor $1 $3 }
  | expression_ 'xor' expression_  {% binopRE Xor $1 $3 }
  | expression_ 'xnor' expression_ {% binopRE Xnor $1 $3 }
  | expression_ 'sll' expression_  {% binopRE Sll $1 $3 }
  | expression_ 'srl' expression_  {% binopRE Srl $1 $3 }
  | expression_ 'sla' expression_  {% binopRE Sla $1 $3 }
  | expression_ 'sra' expression_  {% binopRE Sra $1 $3 }
  | expression_ 'rol' expression_  {% binopRE Rol $1 $3 }
  | expression_ 'ror' expression_  {% binopRE Ror $1 $3 }
  | expression_ '=' expression_    {% binopRE Eq $1 $3 }
  | expression_ '/=' expression_   {% binopRE Ne $1 $3 }
  | expression_ '<' expression_    {% binopRE Lt $1 $3 }
  | expression_ '<=' expression_   {% binopRE Le $1 $3 }
  | expression_ '>=' expression_   {% binopRE Ge $1 $3 }
  | expression_ '>' expression_    {% binopRE Gt $1 $3 }
  | expression_ '?=' expression_   {% binopRE EqM $1 $3 }
  | expression_ '?/=' expression_  {% binopRE NeM $1 $3 }
  | expression_ '?<' expression_   {% binopRE LtM $1 $3 }
  | expression_ '?<=' expression_  {% binopRE LeM $1 $3 }
  | expression_ '?>=' expression_  {% binopRE GeM $1 $3 }
  | expression_ '?>' expression_   {% binopRE GtM $1 $3 }

simple_expression :: { RichExp }
simple_expression :
    primary                                    { $1 }
  | '+' primary                                {% unopRE Plus $2 }
  | '-' primary                                {% unopRE Neg $2 }
  | 'and' primary                              {% unopRE UAnd $2 }
  | 'or' primary                               {% unopRE UOr $2 }
  | 'nand' primary                             {% unopRE UNand $2 }
  | 'nor' primary                              {% unopRE UNor $2 }
  | 'xor' primary                              {% unopRE UXor $2 }
  | 'xnor' primary                             {% unopRE UXnor $2 }
  | 'abs' primary                              {% unopRE Abs $2 }
  | 'not' primary                              {% unopRE Not $2 }
  | simple_expression '+' simple_expression    {% binopRE Add $1 $3 }
  | simple_expression '-' simple_expression    {% binopRE Sub $1 $3 }
  | simple_expression '&' simple_expression    {% binopRE Cat $1 $3 }
  | simple_expression '*' simple_expression    {% binopRE Mul $1 $3 }
  | simple_expression '/' simple_expression    {% binopRE Div $1 $3 }
  | simple_expression 'mod' simple_expression  {% binopRE Mod $1 $3 }
  | simple_expression 'rem' simple_expression  {% binopRE Rem $1 $3 }
  | simple_expression '**' simple_expression   {% binopRE Pow $1 $3 }

primary :: { RichExp }
primary :
    name
      { ExpR $ VarE $1 (srclocOf $1) }
  | literal
      { ExpR $ LitE $1 (srclocOf $1) }
  | name '(' expression_rlist ')'
      { CallR $1 (rev $3) ($1 `srcspan` $4) }
  | qualified_expression
      { ExpR $ QualE $1 (srclocOf $1) }
  -- type_conversion
  -- OR subtype_indication of the following form:
  --   type_mark array_constraint
  -- | type_mark record_constraint
  | type_mark '(' expression_rlist ')'
      { CastR $1 (ParensR (rev $3) ($2 `srcspan` $4)) ($1 `srcspan` $4) }
  | allocator
      { ExpR $1 }
  | '(' expression_rlist ')'
      { ParensR (rev $2) ($1 `srcspan` $3) }
  | ANTI_EXP
      { ExpR $ AntiExp (getANTI_EXP $1) (srclocOf $1) }
  -- subtype_indication
  | type_mark range_constraint
      { let { range = RangeC $2 (srclocOf $2) }
        in
            SubtypeR (Subtype Nothing $1 (Just range) ($1 `srcspan` $2))
                     ($1 `srcspan` $2)
      }
  | type_mark '(' expression_rlist ')' array_element_constraint
      {% do { c <- checkArrayConstraint (ParensR (rev $3) ($2 `srcspan` $4)) (Just $5)
            ; pure $ SubtypeR (Subtype Nothing $1 (Just c) ($1 `srcspan` $5))
                              ($1 `srcspan` $5)
            }
      }
  | subtype_indication_
      { SubtypeR $1 (srclocOf $1) }
  -- Could be an actual_designator or part of an array_constraint
  | 'open'
      { OpenR (srclocOf $1) }
  | 'others'
      { OthersR (srclocOf $1) }

expression_rlist :: { RevList RichExp }
expression_rlist :
    expression
      { rsingleton $1 }
  | 'inertial' expression
      {% do { e <- checkExp  $2
            ; pure $ rsingleton $ InertialR e ($1 `srcspan` $2)
            }
      }
  | ANTI_LITS
      { rsingleton $ AntiLitsR (getANTI_LITS $1) (srclocOf $1) }
  | ANTI_EXPS
      { rsingleton $ AntiExpsR (getANTI_EXPS $1) (srclocOf $1) }
  | expression '=>' expression
      { rsingleton $ AssocR $1 $3 ($1 `srcspan` $3) }
  | expression_rlist ',' expression
     { rcons $3 $1 }

{-
[§ 9.3.2]

literal ::=
    numeric_literal
  | enumeration_literal
  | string_literal
  | bit_string_literal
  | null

numeric_literal ::=
    abstract_literal
  | physical_literal
-}

literal :: { Lit }
literal :
     numeric_literal     { $1 }
-- XXX conflicts with operator names and more!
--  |  enumeration_literal { $1 }
-- XXX conflicts with operator names
  |  string_literal      { $1 }
  |  bit_string_literal  { $1 }
-- XXX conflicts with null waveform
--  |  'null'              { Null (srclocOf $1) }
  | ANTI_LIT { AntiLit (getANTI_LIT $1) (srclocOf $1) }

numeric_literal :: { Lit }
numeric_literal :
     abstract_literal { $1 }
  |  physical_literal { $1 }

{-
[§ 9.3.3]

aggregate ::=
  ( element_association { , element_association } )

element_association ::=
  [ choices => ] expression

choices ::= choice { | choice }

choice ::=
    simple_expression
  | discrete_range
  | element_simple_name
  | others
-}

aggregate :: { [ElemAssoc] }
aggregate :
   '(' expression_rlist ')'
     {% checkAggregate (ParensR (rev $2) ($1 `srcspan` $3)) }

{-
[§ 9.3.4]

function_call ::=
  function_name [ ( actual_parameter_part ) ]

actual_parameter_part ::= parameter_association_list
-}

{-
function_call :: { RichExp }
function_call :
  name '(' expression_rlist ')'
    { CallR $1 (rev $3) ($1 `srcspan` $4) }
-}

{-
actual_parameter_part :: { [AssocElem] }
actual_parameter_part : association_list { $1 }
-}

{-
[§ 9.3.5]

qualified_expression ::=
    type_mark ' ( expression )
  | type_mark ' aggregate
-}

qualified_expression :: { QualExp }
qualified_expression :
    type_mark '\'' '(' expression_rlist ')'
      {% case rev $4 of
           { [re] ->
               do { e <- checkExp re
                  ; pure $ QualExp $1 e ($1 `srcspan` $3)
                  }
           ; res ->
               do { agg <- mapM checkElemAssoc res
                  ; pure $ QualAgg $1 agg ($1 `srcspan` $3)
                  }
           }
      }

{-
[§ 9.3.6]

type_conversion ::= type_mark ( expression )
-}

{-
[§ 9.3.7]

allocator ::=
    new subtype_indication
  | new qualified_expression
-}

allocator :: { Exp }
allocator :
    'new' subtype_indication   { AllocTyE $2 ($1 `srcspan` $2) }
{-
  | 'new' qualified_expression { AllocE $2 ($1 `srcspan` $2) }
-}

{-
[§ 10.1]
sequence_of_statements ::=
  { sequential_statement }

sequential_statement ::=
    wait_statement
  | assertion_statement
  | report_statement
  | signal_assignment_statement
  | variable_assignment_statement
  | procedure_call_statement
  | if_statement
  | case_statement
  | loop_statement
  | next_statement
  | exit_statement
  | return_statement
  | null_statement
-}

sequential_statement :: { Stm }
sequential_statement :
    identifier ':' sequential_statement { LabelS $1 $3 ($1 `srcspan` $3) }
  | wait_statement                      { $1 }
  | assertion_statement                 { $1 }
  | report_statement                    { $1 }
  | signal_assignment_statement         { $1 }
  | variable_assignment_statement       { $1 }
  | procedure_call_statement            { $1 }
  | if_statement                        { $1 }
  | case_statement                      { $1 }
  | loop_statement                      { $1 }
  | next_statement                      { $1 }
  | exit_statement                      { $1 }
  | return_statement                    { $1 }
  | null_statement                      { $1 }
  | ANTI_STM                            { AntiStm (getANTI_STM $1) (srclocOf $1) }
  | ANTI_STMS                           { AntiStms (getANTI_STMS $1) (srclocOf $1) }

sequence_of_statements :: { [Stm] }
sequence_of_statements : sequential_statement_rlist { rev $1 }

sequential_statement_rlist :: { RevList Stm }
sequential_statement_rlist :
    {- empty -}
      { rnil }
  | sequential_statement_rlist sequential_statement
      { rcons $2 $1 }

{-
[§ 10.2]
wait_statement ::=
  [ label : ] wait [ sensitivity_clause ] [ condition_clause ] [ timeout_clause ] ;

sensitivity_clause ::= on sensitivity_list

sensitivity_list ::= signal_name { , signal_name }

condition_clause ::= until condition

condition ::= expression

timeout_clause ::= for time_expression
-}

wait_statement :: { Stm }
wait_statement :
    'wait' sensitivity_clause condition_clause timeout_clause ';'
      { WaitS $2 $3 $4 ($1 `srcspan` $5) }

sensitivity_clause :: { [SignalName] }
sensitivity_clause :
    {- empty -}           { [] }
  | 'on' sensitivity_list { $2 }

sensitivity_list :: { [Name] }
sensitivity_list : name_rlist { rev $1 }

condition_clause :: { Maybe Cond }
condition_clause :
    {- empty -}       { Nothing }
  | 'until' condition { Just $2 }

condition :: { Exp }
condition : expression {% checkExp $1 }

timeout_clause :: { Maybe Exp }
timeout_clause :
    {- empty -}     { Nothing }
  | 'for' condition { Just $2 }

{-
[§ 10.3]
assertion_statement ::= [ label : ] assertion ;

assertion ::=
  assert condition
    [ report expression ]
    [ severity expression ]
-}

assertion_statement :: { Stm }
assertion_statement :
    'assert' condition report_clause severity_clause ';'
      { AssertS $2 $3 $4 ($1 `srcspan` $5) }

report_clause :: { Maybe Exp }
report_clause :
    {- empty -}
      { Nothing }
  | 'report' expression
     {% do { e <- checkExp $2
           ; pure $ Just e
           }
     }

severity_clause :: { Maybe Exp }
severity_clause :
    {- empty -}
      { Nothing }
  | 'severity' expression
     {% do { e <- checkExp $2
           ; pure $ Just e
           }
     }


{-
[§ 10.4]
report_statement ::=
  [ label : ]
    report expression
    [ severity expression ] ;
-}

report_statement :
    'report' expression severity_clause ';'
      {% do { e <- checkExp $2
            ; pure $ ReportS e $3 ($1 `srcspan` $4)
            }
      }

{-
[§ 10.5]

signal_assignment_statement ::=
    [ label : ] simple_signal_assignment
  | [ label : ] conditional_signal_assignment
  | [ label : ] selected_signal_assignment
-}

signal_assignment_statement :: { Stm }
signal_assignment_statement :
    simple_signal_assignment      { $1 }
  | conditional_signal_assignment { $1 }
  | selected_signal_assignment    { $1 }

{-
[§ 10.5.2]
simple_signal_assignment ::=
    simple_waveform_assignment
  | simple_force_assignment
  | simple_release_assignment

simple_waveform_assignment ::=
  target <= [ delay_mechanism ] waveform ;

simple_force_assignment ::=
  target <= force [ force_mode ] expression ;

simple_release_assignment ::=
  target <= release [ force_mode ] ;

force_mode ::= in | out

delay_mechanism ::=
    transport
  | [ reject time_expression ] inertial

target ::=
    name
  | aggregate

waveform ::=
    waveform_element { , waveform_element }
  | unaffected

waveform_element ::=
    value_expression [ after time_expression ]
  | null [ after time_expression ]
-}

simple_signal_assignment :: { Stm }
simple_signal_assignment :
    simple_waveform_assignment { $1 }
  | simple_force_assignment    { $1 }
  | simple_release_assignment  { $1 }

simple_waveform_assignment :: { Stm }
simple_waveform_assignment :
    target '<=' delay_mechanism_clause waveform ';'
      { SigAssnS $1 (WaveRhs $3 $4 ($3 `srcspan` $4)) ($1 `srcspan` $5) }

simple_force_assignment :: { Stm }
simple_force_assignment :
    target '<=' 'force' force_mode_clause expression ';'
      {% do { e <- checkExp $5
            ; pure $ SigAssnS $1 (ForceRhs $4 e ($3 `srcspan` $5)) ($1 `srcspan` $6)
            }
      }

simple_release_assignment :: { Stm }
simple_release_assignment :
    target '<=' 'release' force_mode_clause ';'
      { SigAssnS $1 (ReleaseRhs $4 ($3 `srcspan` $5)) ($1 `srcspan` $5) }

force_mode_clause :: { Maybe Mode }
force_mode_clause :
    {- empty -} { Nothing }
  | 'in'        { Just In }
  | 'out'       { Just Out }

delay_mechanism_clause :: { Maybe DelayMechanism }
delay_mechanism_clause :
    {- empty -}              { Nothing }
  | 'transport'              { Just $ Transport (srclocOf $1) }
  | reject_clause 'inertial' { Just $ Inertial $1 ($1 `srcspan` $2) }

target :: { Target }
target :
    name
      { NameT $1 (srclocOf $1) }
  | name '(' expression_rlist ')'
      {% do { n <- checkArrayIndexOrSlice $1 (rev $3) ($1 `srcspan` $4)
            ; pure $ NameT n ($1 `srcspan` $4)
            }
      }
  | aggregate
      { AggT $1 (srclocOf $1) }

waveform :: { Waveform }
waveform :
    waveform_elements { rev $1 }
  | 'unaffected'      { [] }

waveform_elements :: { RevList Wave }
waveform_elements :
    waveform_element                       { rsingleton $1 }
  | waveform_elements ',' waveform_element { rcons $3 $1 }

waveform_element :: { Wave }
waveform_element :
    expression after_clause
      {% do { e <- checkExp $1
            ; pure $ Wave (Just e) $2 ($1 `srcspan` $2)
            }
      }
  | 'null' after_clause
      { Wave Nothing $2 ($1 `srcspan` $2) }

reject_clause :: { Maybe Exp }
reject_clause :
    {- empty -}
      { Nothing }
  | 'reject' expression
      {% do { e <- checkExp $2
            ; return $ Just e
            }
      }

after_clause :: { Maybe Exp }
after_clause :
    {- empty -}
      { Nothing }
  | 'after' expression
      {% do { e <- checkExp $2
            ; return $ Just e
            }
      }

{-
[§ 10.5.3]

conditional_signal_assignment ::=
    conditional_waveform_assignment
  | conditional_force_assignment

conditional_waveform_assignment ::=
  target <= [ delay_mechanism ] conditional_waveforms ;

conditional_waveforms ::=
  waveform when condition
  { else waveform when condition }
  [ else waveform ]

conditional_force_assignment ::=
  target <= force [ force_mode ] conditional_expressions ;

conditional_expressions ::=
  expression when condition
  { else expression when condition }
  [ else expression ]
-}


conditional_signal_assignment :: { Stm }
conditional_signal_assignment :
    conditional_waveform_assignment { $1 }
  | conditional_force_assignment    { $1 }

conditional_waveform_assignment :: { Stm }
conditional_waveform_assignment :
    target '<=' delay_mechanism_clause conditional_waveforms ';'
      { SigAssnS $1 (CondWaveRhs $3 $4 ($3 `srcspan` $4)) ($1 `srcspan` $5) }

conditional_waveforms :: { Conditional Waveform }
conditional_waveforms :
    waveform 'when' condition
      { Conditional [($1, $3)] Nothing ($1 `srcspan` $3)}
  | waveform 'when' condition 'else' waveform
      { Conditional [($1, $3)] (Just $5) ($1 `srcspan` $5)}

conditional_force_assignment :: { Stm }
conditional_force_assignment :
    target '<=' 'force' force_mode_clause conditional_expressions ';'
      { SigAssnS $1 (CondForceRhs $4 $5 ($3 `srcspan` $5)) ($1 `srcspan` $6) }

conditional_expressions :: { Conditional Exp }
conditional_expressions :
    expression 'when' condition
      {% do { e <- checkExp $1
            ; pure $ Conditional [(e, $3)] Nothing ($1 `srcspan` $3)
            }
      }
  | expression 'when' condition 'else' expression
      {% do { e1 <- checkExp $1
            ; e2 <- checkExp $5
            ; pure $ Conditional [(e1, $3)] (Just e2) ($1 `srcspan` $5)
            }
      }

{-
[§ 10.5.4]

selected_signal_assignment ::=
    selected_waveform_assignment
  | selected_force_assignment

selected_waveform_assignment ::=
  with expression select [ ? ]
    target <= [ delay_mechanism ] selected_waveforms ;

selected_waveforms ::=
  { waveform when choices , }
  waveform when choices

selected_force_assignment ::=
  with expression select [ ? ]
    target <= force [ force_mode ] selected_expressions ;

selected_expressions ::=
  { expression when choices , }
  expression when choices
-}

selected_signal_assignment :: { Stm }
selected_signal_assignment :
    selected_waveform_assignment { $1 }
  | selected_force_assignment    { $1 }

selected_waveform_assignment :: { Stm }
selected_waveform_assignment :
    'with' expression 'select' match_opt
      target '<=' delay_mechanism_clause selected_waveforms ';'
    {% do { e <- checkExp $2
          ; pure $ SigAssnS $5 (SelWaveRhs e $4 $7 $8 ($7 `srcspan` $8)) ($1 `srcspan` $9)
          }
    }

selected_waveforms :: { Selected Waveform }
selected_waveforms :
    selected_waveforms_ { Selected (map unLoc (rev $1)) (srclocOf $1) }

selected_waveforms_ :: { RevList (L (Waveform, Choices)) }
selected_waveforms_ :
    waveform 'when' expression
      {% do { cs <- checkChoices $3
            ; pure $ rsingleton (L ($1 <--> $3) ($1, cs))
            }
      }
  | selected_waveforms_ ',' waveform 'when' expression
      {% do { cs <- checkChoices $5
            ; pure $ rcons (L ($3 <--> $5) ($3, cs)) $1
            }
      }

selected_force_assignment :: { Stm }
selected_force_assignment :
    'with' expression 'select' match_opt
      target '<=' 'force' force_mode_clause selected_expressions ';'
    {% do { e <- checkExp $2
          ; pure $ SigAssnS $5 (SelForceRhs e $4 $8 $9 ($7 `srcspan` $9)) ($1 `srcspan` $10)
          }
    }

selected_expressions :: { Selected Exp }
selected_expressions :
    selected_expressions_ { Selected (map unLoc (rev $1)) (srclocOf $1) }

selected_expressions_ :: { RevList (L (Exp, Choices)) }
selected_expressions_ :
    expression 'when' expression
      {% do { e  <- checkExp $1
            ; cs <- checkChoices $3
            ; pure $ rsingleton (L ($1 <--> $3) (e, cs))
            }
      }
  | selected_expressions_ ',' expression 'when' expression
       {% do { e  <- checkExp $3
             ; cs <- checkChoices $5
             ; pure $ rcons (L ($3 <--> $5) (e, cs)) $1
             }
       }

match_opt :: { Bool }
match_opt :
    {- empty -} { False }
  | '?'         { True }

{-
[§ 10.6]

variable_assignment_statement ::=
    [ label : ] simple_variable_assignment
  | [ label : ] conditional_variable_assignment
  | [ label : ] selected_variable_assignment

simple_variable_assignment ::=
  target := expression ;

conditional_variable_assignment ::=
  target := conditional_expressions ;

selected_variable_assignment ::=
  with expression select [ ? ]
    target := selected_expressions ;
-}

variable_assignment_statement :: { Stm }
variable_assignment_statement :
    simple_variable_assignment      { $1 }
  | conditional_variable_assignment { $1 }
  | selected_variable_assignment    { $1 }

simple_variable_assignment :: { Stm }
simple_variable_assignment :
    target ':=' expression ';'
      {% do { e <- checkExp $3
            ; pure $ VarAssnS $1 (VarRhs e (srclocOf $3)) ($1 `srcspan` $4)
            }
      }

conditional_variable_assignment :: { Stm }
conditional_variable_assignment :
    target ':=' conditional_expressions ';'
      { VarAssnS $1 (CondRhs $3 (srclocOf $3)) ($1 `srcspan` $4) }

selected_variable_assignment :: { Stm }
selected_variable_assignment :
    'with' expression 'select' match_opt
      target ':=' selected_expressions ';'
     {% do { e <- checkExp $2
           ; pure $ VarAssnS $5 (SelRhs e $4 $7 (srclocOf $7)) ($1 `srcspan` $8)
           }
     }

{-
[§ 10.7]

procedure_call_statement ::= [ label : ] procedure_call ;

procedure_call ::= procedure_name [ ( actual_parameter_part ) ]
-}

procedure_call_statement :: { Stm }
procedure_call_statement :
    procedure_call ';'
      { let (f, args) = $1
        in
          CallS f args (f `srcspan` args)
      }

procedure_call :: { (Name, [AssocElem]) }
procedure_call :
    name
      { ($1, []) }
  | name '(' expression_rlist ')'
      {% do { args <- mapM checkArg (rev $3)
            ; pure ($1, args)
            }
      }

{-
[§ 10.8]

if_statement ::=
  [ if_label : ]
    if condition then
      sequence_of_statements
    { elsif condition then
      sequence_of_statements }
    [ else
      sequence_of_statements ]
    end if [ if_label ] ;
-}

if_statement :: { Stm }
if_statement :
    'if' condition 'then'
      sequence_of_statements
    elsif_rlist
    else_opt
    'end' 'if' label_opt ';'
      { let bs = map unLoc (rev $5)
        in
          IfS (($2, $4) : bs) $6 $9 ($1 `srcspan` $10)
      }

elsif_rlist :: { RevList (L (Cond, [Stm])) }
elsif_rlist :
    {- empty -}
      { rnil }
  | elsif_rlist 'elsif' condition 'then' sequence_of_statements
      { rcons (L ($2 <--> $5) ($3, $5)) $1 }

else_opt :: { Maybe [Stm] }
else_opt :
    {- empty -}                   { Nothing }
  | 'else' sequence_of_statements { Just $2 }

{-
[§ 10.9]

case_statement ::=
  [ case_label : ]
    case [ ? ] expression is
      case_statement_alternative
      { case_statement_alternative }
  end case [ ? ] [ case_label ] ;

case_statement_alternative ::=
  when choices =>
    sequence_of_statements
-}

case_statement :: { Stm }
case_statement :
    'case' match_opt expression 'is'
      case_statement_alternative_rlist
    'end' 'case' match_opt label_opt ';'
      {% do { e <- checkExp $3
            ; pure $ CaseS e $2 (map unLoc (rev $5)) $9 ($1 `srcspan` $10)
            }
      }

case_statement_alternative_rlist :: { RevList (L (Choices, [Stm])) }
case_statement_alternative_rlist :
    case_statement_alternative
      { rsingleton $1 }
  | case_statement_alternative_rlist case_statement_alternative
      { rcons $2 $1 }

case_statement_alternative :: { L (Choices, [Stm]) }
case_statement_alternative :
    'when' expression '=>' sequence_of_statements
      {% do { cs <- checkChoices $2
            ; pure $ L ($1 <--> $4) (cs, $4)
            }
      }

{-
[§ 10.10]

loop_statement ::=
  [ loop_label : ]
    [ iteration_scheme ] loop
      sequence_of_statements
    end loop [ loop_label ] ;

iteration_scheme ::=
    while condition
  | for loop_parameter_specification

parameter_specification ::=
  identifier in discrete_range
-}

loop_statement :: { Stm }
loop_statement :
    'while' condition 'loop'
      sequence_of_statements
    'end' 'loop' label_opt ';'
      { WhileS $2 $4 $7 ($1 `srcspan` $8) }
  | 'for' identifier 'in' discrete_range 'loop'
      sequence_of_statements
    'end' 'loop' label_opt ';'
      { ForS $2 $4 $6 $9 ($1 `srcspan` $10) }

{-
[§ 10.11]

next_statement ::=
  [ label : ] next [ loop_label ] [ when condition ] ;
-}

next_statement :: { Stm }
next_statement :
    'next' label_opt ';'
      { NextS $2 Nothing ($1 `srcspan` $3) }
  | 'next' label_opt 'when' condition ';'
      { NextS $2 (Just $4) ($1 `srcspan` $3) }

{-
[§ 10.12]

exit_statement ::=
  [ label : ] exit [ loop_label ] [ when condition ] ;
-}

exit_statement :: { Stm }
exit_statement :
    'exit' label_opt ';'
      { ExitS $2 Nothing ($1 `srcspan` $3) }
  | 'exit' label_opt 'when' condition ';'
      { ExitS $2 (Just $4) ($1 `srcspan` $3) }

{-
[§ 10.13]

return_statement ::=
  [ label : ] return [ expression ] ;
-}

return_statement :: { Stm }
return_statement :
    'return' ';'
      { ReturnS Nothing ($1 `srcspan` $2) }
  | 'return' expression ';'
      {% do { e <- checkExp $2
            ; pure $ ReturnS (Just e) ($1 `srcspan` $3)
            }
      }

{-}
[§ 10.14]

null_statement ::=
  [ label : ] null ;
-}

null_statement :: { Stm }
null_statement :
    'null' ';'
      { NullS ($1 `srcspan` $2) }

{-
[§ 11.1]

concurrent_statement ::=
    block_statement
  | process_statement
  | concurrent_procedure_call_statement
  | concurrent_assertion_statement
  | concurrent_signal_assignment_statement
  | component_instantiation_statement
  | generate_statement
  | PSL_PSL_Directive
-}

concurrent_statement :: { CStm }
concurrent_statement :
    identifier ':' concurrent_statement    { LabelCS $1 $3 ($1 `srcspan` $3) }
  | block_statement                        { $1 }
  | process_statement                      { $1 }
  | concurrent_procedure_call_statement    { $1 }
  | concurrent_assertion_statement         { $1 }
  | concurrent_signal_assignment_statement { $1 }
  | component_instantiation_statement      { $1 }
  | generate_statement                     { $1 }
  | ANTI_CSTM                              { AntiCStm (getANTI_CSTM $1) (srclocOf $1) }
  | ANTI_CSTMS                             { AntiCStms (getANTI_CSTMS $1) (srclocOf $1) }

concurrent_statements :: { [CStm] }
concurrent_statements : concurrent_statement_rlist { rev $1 }

concurrent_statement_rlist :: { RevList CStm }
concurrent_statement_rlist :
    {- empty -}
      { rnil }
  | concurrent_statement_rlist concurrent_statement
      { rcons $2 $1 }

{-
[§ 11.2]

block_statement ::=
  block_label :
    block [ ( guard_condition ) ] [ is ]
      block_header
      block_declarative_part
    begin
      block_statement_part
    end block [ block_label ] ;

block_header ::=
  [ generic_clause
  [ generic_map_aspect ; ] ]
  [ port_clause
  [ port_map_aspect ; ] ]

block_declarative_part ::=
  { block_declarative_item }

block_statement_part ::=
  { concurrent_statement }
-}

block_statement :: { CStm }
block_statement :
    'block' guard_condition is_opt
        generic_header_opt
        port_header_opt
        declarations_rlist
    'begin'
      block_statement_part
    'end' 'block' label_opt ';'
      { BlockS $2 $4 $5 (rev $6) $8 ($1 `srcspan` $12) }

block_statement_part :: { [CStm] }
block_statement_part : concurrent_statements { $1 }

guard_condition :: { Maybe Cond }
guard_condition :
    {- empty -}       { Nothing }
  | '(' condition ')' { Just $2 }

generic_header_opt :: { Maybe GenHeader }
generic_header_opt :
    {- empty -}    { Nothing }
  | generic_header { Just $1 }

generic_header :: { GenHeader }
generic_header :
    generic_clause
      { GenHeader $1 Nothing (srclocOf $1) }
  | generic_clause generic_map_aspect ';'
      { GenHeader $1 (Just $2) ($1 `srcspan` $2) }

port_header_opt :: { Maybe PortHeader }
port_header_opt :
    {- empty -}    { Nothing }
  | port_header { Just $1 }

port_header :: { PortHeader }
port_header :
    port_clause
      { PortHeader $1 Nothing (srclocOf $1) }
  | port_clause port_map_aspect ';'
      { PortHeader $1 (Just $2) ($1 `srcspan` $2) }

{-
[§ 11.3]

process_statement ::=
  [ process_label : ]
    [ postponed ] process [ ( process_sensitivity_list ) ] [ is ]
      process_declarative_part
    begin
      process_statement_part
    end [ postponed ] process [ process_label ] ;

process_sensitivity_list ::= all | sensitivity_list

process_declarative_part ::=
  { process_declarative_item }

process_declarative_item ::=
    subprogram_declaration
  | subprogram_body
  | subprogram_instantiation_declaration
  | package_declaration
  | package_body
  | package instantiation_declaration
  | type_declaration
  | subtype_declaration
  | constant_declaration
  | variable_declaration
  | file_declaration
  | alias_declaration
  | attribute_declaration
  | attribute_specification
  | use_clause
  | group_template_declaration
  | group_declaration

process_statement_part ::=
  { sequential_statement }
-}

process_statement :: { CStm }
process_statement :
    'process' process_sensitivity_list_opt is_opt
      declarations_rlist
    'begin'
      sequence_of_statements
    'end' postponed_opt 'process' label_opt ';'
      { ProcessS (False || $8) $2 (rev $4) $6 ($1 `srcspan` $11) }
  | 'process' process_sensitivity_list_opt is_opt
      declarations_rlist
    'begin'
      sequence_of_statements
    'end' postponed_opt error
      {% expected ["`process`"] Nothing }
  | 'postponed' 'process' process_sensitivity_list_opt is_opt
      declarations_rlist
    'begin'
      sequence_of_statements
    'end' postponed_opt 'process' label_opt ';'
      { ProcessS True $3 (rev $5) $7 ($2 `srcspan` $12) }
  | 'postponed' 'process' process_sensitivity_list_opt is_opt
      declarations_rlist
    'begin'
      sequence_of_statements
    'end' postponed_opt error
      {% expected ["`process`"] Nothing }

process_sensitivity_list_opt :: { Maybe ProcessSensitivityList }
process_sensitivity_list_opt :
    {- empty -}                      { Nothing }
  | '(' process_sensitivity_list ')' { Just $2 }

process_sensitivity_list :: { ProcessSensitivityList }
process_sensitivity_list :
    'all'            { AllSense (srclocOf $1) }
  | sensitivity_list { SomeSense $1 (srclocOf $1) }

postponed_opt :: { Bool }
postponed_opt :
    {- empty -} { False }
  | 'postponed' { True }

{-
[§ 11.4]

concurrent_procedure_call_statement ::=
  [ label : ] [ postponed ] procedure_call ;
-}

concurrent_procedure_call_statement :: { CStm }
concurrent_procedure_call_statement :
    'postponed' procedure_call ';'
      { let (f, args) = $2
        in
          ConcCallS True f args ($1 `srcspan` $3)
      }
  | procedure_call ';'
      { let (f, args) = $1
        in
          ConcCallS False f args (f `srcspan` $2)
      }

{-
[§ 11.5]

concurrent_assertion_statement ::=
  [ label : ] [ postponed ] assertion ;
-}

concurrent_assertion_statement :: { CStm }
concurrent_assertion_statement :
    'postponed' 'assert' condition report_clause severity_clause ';'
      { ConcAssertS True $3 $4 $5 ($1 `srcspan` $6) }
  | 'assert' condition report_clause severity_clause ';'
      { ConcAssertS False $2 $3 $4 ($1 `srcspan` $5) }

{-
[§ 11.6]

concurrent_signal_assignment_statement ::=
    [ label : ] [ postponed ] concurrent_simple_signal_assignment
  | [ label : ] [ postponed ] concurrent_conditional_signal_assignment
  | [ label : ] [ postponed ] concurrent_selected_signal_assignment

concurrent_simple_signal_assignment ::=
  target <= [ guarded ] [ delay_mechanism ] waveform ;

concurrent_conditional_signal_assignment ::=
  target <= [ guarded ] [ delay_mechanism ] conditional_waveforms ;

concurrent_selected_signal_assignment ::=
  with expression select [ ? ]
  target <= [ guarded ] [ delay_mechanism ] selected_waveforms ;
-}

concurrent_signal_assignment_statement :: { CStm }
concurrent_signal_assignment_statement :
    concurrent_simple_signal_assignment      { $1 }
  | concurrent_conditional_signal_assignment { $1 }
  | concurrent_selected_signal_assignment    { $1 }

concurrent_simple_signal_assignment :: { CStm }
concurrent_simple_signal_assignment :
    'postponed' target '<=' guarded_opt delay_mechanism_clause waveform ';'
      { ConcSigAssnS $2 True $4 $5 (ConcWaveRhs $6 (srclocOf $6)) ($1 `srcspan` $7) }
  | target '<=' guarded_opt delay_mechanism_clause waveform ';'
      { ConcSigAssnS $1 False $3 $4 (ConcWaveRhs $5 (srclocOf $5)) ($1 `srcspan` $6) }

concurrent_conditional_signal_assignment :: { CStm }
concurrent_conditional_signal_assignment :
    'postponed' target '<=' guarded_opt delay_mechanism_clause conditional_waveforms ';'
      { ConcSigAssnS $2 True $4 $5 (ConcCondRhs $6 (srclocOf $6)) ($1 `srcspan` $7) }
  | target '<=' guarded_opt delay_mechanism_clause conditional_waveforms ';'
      { ConcSigAssnS $1 False $3 $4 (ConcCondRhs $5 (srclocOf $5)) ($1 `srcspan` $6) }

concurrent_selected_signal_assignment :: { CStm }
concurrent_selected_signal_assignment :
    'postponed' 'with' expression 'select' match_opt
      target '<=' guarded_opt delay_mechanism_clause selected_waveforms ';'
        {% do { e <- checkExp $3
              ; pure $ ConcSigAssnS $6 True $8 $9 (ConcSelRhs e $5 $10 ($9 `srcspan` $10))
                                    ($1 `srcspan` $11)
              }
        }
  | 'with' expression 'select' match_opt
      target '<=' guarded_opt delay_mechanism_clause selected_waveforms ';'
        {% do { e <- checkExp $2
              ; pure $ ConcSigAssnS $5 False $7 $8 (ConcSelRhs e $4 $9 ($8 `srcspan` $9))
                                    ($1 `srcspan` $10)
              }
        }

guarded_opt :: { Bool }
guarded_opt :
    {- empty -} { False }
  | 'guarded'   { True }

{-
[§ 11.7]

component_instantiation_statement ::=
  instantiation_label :
    instantiated_unit
      [ generic_map_aspect ]
      [ port_map_aspect ] ;

instantiated_unit ::=
    [ component ] component_name
  | entity entity_name [ ( architecture_identifier ) ]
  | configuration configuration_name
-}

component_instantiation_statement :: { CStm }
component_instantiation_statement :
-- XXX This case looks like a procedure call with no arguments, so that's what
-- we parse it as for now.
{-
    instantiated_unit ';'
      { InstS $1 Nothing Nothing ($1 `srcspan` $2) }
-}
    instantiated_unit generic_map_aspect ';'
      { InstS $1 (Just $2) Nothing ($1 `srcspan` $3) }
  | instantiated_unit port_map_aspect ';'
      { InstS $1 Nothing (Just $2) ($1 `srcspan` $3) }
  | instantiated_unit generic_map_aspect port_map_aspect ';'
      { InstS $1 (Just $2) (Just $3) ($1 `srcspan` $3) }

instantiated_unit :: { InstUnit }
instantiated_unit :
    name
      { ComponentInst $1 (srclocOf $1) }
  | 'component' name
      { ComponentInst $2 ($1 `srcspan` $2) }
  | 'entity' name
      { EntityInst $2 Nothing ($1 `srcspan` $2) }
  | 'entity' name '(' identifier ')'
      { EntityInst $2 (Just $4) ($1 `srcspan` $5) }
  | 'configuration' name
      { ConfigInst $2 ($1 `srcspan` $2) }

{-
[§ 11.8]

generate_statement ::=
    for_generate_statement
  | if_generate_statement
  | case_generate_statement

for_generate_statement ::=
  generate_label :
    for generate_parameter_specification generate
      generate_statement_body
    end generate [ generate_label ] ;

if_generate_statement ::=
  generate_label :
    if [ alternative_label : ] condition generate
      generate_statement_body
    { elsif [ alternative_label : ] condition generate
      generate_statement_body }
    [ else [ alternative_label : ] generate
      generate_statement_body ]
    end generate [ generate_label ] ;

case_generate_statement ::=
  generate_label :
    case expression generate
      case_generate_alternative
      { case_generate_alternative }
    end generate [ generate_label ] ;

case_generate_alternative ::=
  when [ alternative_label : ] choices =>
    generate_statement_body

generate_statement_body ::=
    [ block_declarative_part
  begin ]
    { concurrent_statement }
  [ end [ alternative_label ] ; ]

label ::= identifier
-}

generate_statement :: { CStm }
generate_statement :
    for_generate_statement  { $1 }
  | if_generate_statement   { $1 }
  | case_generate_statement { $1 }

for_generate_statement :: { CStm }
for_generate_statement :
    'for' identifier 'in' discrete_range 'generate'
      generate_statement_body
    'end' 'generate' label_opt ';'
      { ForGenS $2 $4 $6 ($1 `srcspan` $10) }

if_generate_statement :: { CStm }
if_generate_statement :
    'if' alternative_label condition 'generate'
      generate_statement_body
    gen_elsif_rlist
    gen_else_opt
    'end' 'generate' label_opt ';'
      { let { bs  = map unLoc (rev $6)
            ; alt = GenAlt $2 $5 ($1 `srcspan` $5)
            }
        in
          IfGenS (($3, alt) : bs) $7 ($1 `srcspan` $11)
      }

case_generate_statement :: { CStm }
case_generate_statement :
    'case' expression 'generate'
      case_generate_alternative_rlist
    'end' 'generate' label_opt ';'
    {% do { e <- checkExp $2
          ; pure $ CaseGenS e (map unLoc (rev $4)) ($1 `srcspan` $8)
          }
    }

case_generate_alternative_rlist :: { RevList (L (Choices, GenAlt)) }
case_generate_alternative_rlist :
    case_generate_alternative
      { rsingleton $1 }
  | case_generate_alternative_rlist case_generate_alternative
      { rcons $2 $1 }

case_generate_alternative :: { L (Choices, GenAlt) }
case_generate_alternative :
  'when' expression '=>' generate_statement_body
    {% do { (lbl, cs) <- checkLabeledChoices $2
          ; let alt = GenAlt lbl $4 ($1 `srcspan` $4)
          ; return $ L ($1 <--> $4) (cs, alt)
          }
    }

gen_elsif_rlist :: { RevList (L (Cond, GenAlt)) }
gen_elsif_rlist :
    {- empty -}
      { rnil }
  | gen_elsif_rlist 'elsif' alternative_label condition 'generate' generate_statement_body
      { let { alt = GenAlt $3 $6 ($2 `srcspan` $6) }
        in
          rcons (L ($1 <--> $6) ($4, alt)) $1
      }

gen_else_opt :: { Maybe GenAlt }
gen_else_opt :
    {- empty -}
      { Nothing }
  | 'else' alternative_label 'generate' generate_statement_body
      { Just (GenAlt $2 $4 ($1 `srcspan` $4)) }

alternative_label :: { Maybe Label }
alternative_label :
    label ':'   { Just $1 }

generate_statement_body :: { GenBody }
generate_statement_body :
      declarations_rlist
    'begin'
      concurrent_statements
    'end' label_opt ';'
      { GenBody (rev $1) $3 ($1 `srcspan` $6) }

label :: { Label }
label : identifier { $1 }

label_rlist :: { RevList Label }
label_rlist :
    label                 { rsingleton $1 }
  | label_rlist ',' label { rcons $3 $1 }

label_opt :: { Maybe Label }
label_opt :
    {- empty -} { Nothing }
  | label       { Just $1 }

{-
[§ 12.4]

use_clause ::=
  use selected_name { , selected_name } ;
-}

use_clause :: { UseClause }
use_clause :
    'use' name_rlist ';'
      { UseClause (rev $2) ($1 `srcspan` $3) }

use_clauses :: { [UseClause] }
use_clauses : use_clause_rlist { rev $1 }

use_clause_rlist :: { RevList UseClause }
use_clause_rlist :
    use_clause                  { rsingleton $1 }
  | use_clause_rlist use_clause { rcons $2 $1 }

{-
[§ 13.1]
design_file ::= design_unit { design_unit }

design_unit ::= context_clause library_unit

library_unit ::=
    primary_unit
  | secondary_unit

primary_unit ::=
    entity_declaration
  | configuration_declaration
  | package_declaration
  | package_instantiation_declaration
  | context_declaration
  | PSL_Verification_Unit

secondary_unit ::=
    architecture_body
  | package_body
-}

design_file :: { [DesignUnit] }
design_file : design_unit_rlist { rev $1 }

design_unit_rlist :: { RevList DesignUnit }
design_unit_rlist :
    design_unit                   { rsingleton $1 }
  | design_unit_rlist design_unit { rcons $2 $1 }

design_unit :: { DesignUnit }
design_unit :
    context_item_rlist declaration
      { DesignUnit (rev $1) $2 ($1 `srcspan` $2) }

{-
[§ 13.2]

library_clause ::= library logical_name_list ;

logical_name_list ::= logical_name { , logical_name }

logical_name ::= identifier

[§ 13.3]

context_declaration ::=
  context identifier is
    context_clause
  end [ context ] [ context_simple_name ] ;

[§ 13.4]

context_clause ::= { context_item }

context_item ::=
    library_clause
  | use_clause
  | context_reference

context_reference ::=
  context selected_name { , selected_name } ;
-}

context_declaration :: { Decl }
context_declaration :
  'context' identifier 'is'
    context_item_rlist
  'end' context_opt simple_name_opt ';'
    { ContextD $2 (rev $4) ($1 `srcspan` $8) }

context_opt :: { () }
context_opt :
    {- empty -} { () }
  | 'context'   { () }

context_item_rlist :: { RevList Context }
context_item_rlist :
    {- empty -}                     { rnil }
  | context_item_rlist context_item { rcons $2 $1 }

context_item :: { Context }
context_item :
    'library' name_rlist ';' { LibC (rev $2) ($1 `srcspan` $3 ) }
  -- XXX conflicts with 'use' in declaration
  -- | 'use' name_rlist ';'     { UseC (rev $2) ($1 `srcspan` $3 ) }
  | 'context' name_rlist ';' { ContextRefC (rev $2) ($1 `srcspan` $3 ) }

{-
[§ 15.4]
identifier ::= basic_identifier | extended_identifier

basic_identifier ::= letter { [ underline ] letter_or_digit }

letter_or_digit ::= letter | digit

letter ::= upper_case_letter | lower_case_letter

extended_identifier ::= \ graphic_character { graphic_character } \
-}

identifier :: { Id }
identifier :
    ID      { mkId  (getID $1)    (srclocOf $1) }
  | EXTID   { ExtId (getEXTID $1) (srclocOf $1) }
  | ANTI_ID { AntiId (getANTI_ID $1) (srclocOf $1) }

type_identifier :: { Id }
type_identifier :
  TYID { mkId (getTYID $1) (srclocOf $1) }

{-
[§ 15.5.1]
abstract_literal ::= decimal_literal | based_literal

[§ 15.5.2]
decimal_literal ::= integer [ . integer ] [ exponent ]

integer ::= digit { [ underline ] digit }

exponent ::= E [ + ] integer | E – integer

[§ 15.5.3]
based_literal ::=
  base # based_integer [ . based_integer ] # [ exponent ]

base ::= integer

based_integer ::=
  extended_digit { [ underline ] extended_digit }

extended_digit ::= digit | letter
-}

abstract_literal :: { Lit }
abstract_literal :
    INT  { let (s, i) = getINT $1
           in
             IntLit s i (srclocOf $1)
         }
  | REAL { let (s, r) = getREAL $1
            in
              RealLit s r (srclocOf $1)
          }
  | ANTI_INT  { AntiInt (getANTI_INT $1) (srclocOf $1) }
  | ANTI_REAL { AntiReal (getANTI_REAL $1) (srclocOf $1) }

{-
[§ 15.6]
character_literal ::= ' graphic_character '
-}

character_literal :: { Lit }
character_literal :
    CHAR { let (s, c) = getCHAR $1
           in
             CharLit s c (srclocOf $1)
         }

{-
[§ 15.7]
string_literal ::= " { graphic_character } "
-}

string_literal :: { Lit }
string_literal :
    STRING
      { let (s, str) = getSTRING $1
        in
          StringLit s str (srclocOf $1)
      }

{-
[§ 15.8]
bit_string_literal ::= [ integer ] base_specifier " [ bit_value ] "

bit_value ::= graphic_character { [ underline ] graphic_character }

base_specifier ::= B | O | X | UB | UO | UX | SB | SO | SX | D
-}

bit_string_literal :: { Lit }
bit_string_literal :
    BITSTRING { BitStringLit (getBITSTRING $1) (srclocOf $1) }

{
happyError :: L T.Token -> P a
happyError (L loc t) = do
    prev <- getPrevToken
    parserError (locStart loc) $
      text "parse error on" <+> quote (ppr t) <+> pprAfter prev
  where
    pprAfter Nothing    = empty
    pprAfter (Just tok) = text "after" <+> ppr tok

getID :: L T.Token -> Symbol
getID (L _ (T.Tident ident)) = ident

getEXTID :: L T.Token -> Symbol
getEXTID (L _ (T.Text_ident ident)) = ident

getTYID :: L T.Token -> Symbol
getTYID (L _ (T.Ttype_ident ident)) = ident

getINT :: L T.Token -> (String, Integer)
getINT (L _ (T.TintLit x)) = x

getREAL :: L T.Token -> (String, Rational)
getREAL (L _ (T.TrealLit x)) = x

getCHAR :: L T.Token -> (String, Char)
getCHAR (L _ (T.TcharLit x)) = x

getSTRING :: L T.Token -> (String, String)
getSTRING (L _ (T.TstringLit x)) = x

getBITSTRING :: L T.Token -> String
getBITSTRING (L _ (T.TbitstringLit x)) = x

getOPERATOR :: L T.Token -> (String, String)
getOPERATOR (L _ (T.Toperator x)) = x

getANTI_ID :: L T.Token -> String
getANTI_ID (L _ (T.Tanti_id ident)) = ident

getANTI_EXP :: L T.Token -> String
getANTI_EXP (L _ (T.Tanti_exp e)) = e

getANTI_EXPS :: L T.Token -> String
getANTI_EXPS (L _ (T.Tanti_exps e)) = e

getANTI_INT :: L T.Token -> String
getANTI_INT (L _ (T.Tanti_int e)) = e

getANTI_REAL :: L T.Token -> String
getANTI_REAL (L _ (T.Tanti_real e)) = e

getANTI_LIT :: L T.Token -> String
getANTI_LIT (L _ (T.Tanti_lit e)) = e

getANTI_LITS :: L T.Token -> String
getANTI_LITS (L _ (T.Tanti_lits e)) = e

getANTI_STM :: L T.Token -> String
getANTI_STM (L _ (T.Tanti_stm stm)) = stm

getANTI_STMS :: L T.Token -> String
getANTI_STMS (L _ (T.Tanti_stms stms)) = stms

getANTI_CSTM :: L T.Token -> String
getANTI_CSTM (L _ (T.Tanti_cstm stm)) = stm

getANTI_CSTMS :: L T.Token -> String
getANTI_CSTMS (L _ (T.Tanti_cstms stms)) = stms

getANTI_TYPE :: L T.Token -> String
getANTI_TYPE (L _ (T.Tanti_type e)) = e

unopE :: Unop -> Exp -> Exp
unopE op e = UnopE op e (srclocOf e)

binopE :: Binop -> Exp -> Exp -> Exp
binopE op e1 e2 = BinopE op e1 e2 (e1 `srcspan` e2)

unopRE :: Unop -> RichExp -> P RichExp
unopRE op e = ExpR <$> (unopE op <$> checkExp e)

binopRE :: Binop -> RichExp -> RichExp -> P RichExp
binopRE op e1 e2 = ExpR <$> (binopE op <$> checkExp e1 <*> checkExp e2)

-- | Convert a located string into an operator symbol.
mkOperator :: Loc -> String -> Operator
mkOperator loc s = Operator (intern s) (srclocOf loc)

-- A 'rich' expression that can represent an expression, choice, or element
-- association. Used to avoid grammar ambiguity since these forms are ambiguous.
data RichExp = ExpR Exp
             | CallR Name [RichExp] !SrcLoc
             | CastR TypeMark RichExp !SrcLoc
             | RangeR Range !SrcLoc
             | SubtypeR Subtype !SrcLoc
             | OpenR !SrcLoc
             | OthersR !SrcLoc
             | ParensR [RichExp] !SrcLoc
             | LabeledR Id RichExp !SrcLoc
             | InertialR Exp !SrcLoc
             | ChoicesR Choices !SrcLoc
             | AssocR RichExp RichExp !SrcLoc
             | AntiLitsR String !SrcLoc
             | AntiExpsR String !SrcLoc
  deriving (Eq, Ord, Show)

instance Pretty RichExp where
    pprPrec p (ExpR e)           = pprPrec p e
    pprPrec _ (CallR f args _)   = ppr f <> parens (commasep (map ppr args))
    pprPrec _ (CastR ty re _)    = ppr ty <> ppr re
    pprPrec p (RangeR rng _)     = pprPrec p rng
    pprPrec p (SubtypeR ty _)    = pprPrec p ty
    pprPrec _ OpenR{}            = text "open"
    pprPrec _ OthersR{}          = text "others"
    pprPrec _ (ParensR res _)    = parens (commasep (map ppr res))
    pprPrec _ (LabeledR l re _)  = ppr l <+> colon <+> ppr re
    pprPrec _ (InertialR e _)    = text "inertial" <+> ppr e
    pprPrec p (ChoicesR cs _)    = pprPrec p cs
    pprPrec p (AssocR re1 re2 _) = ppr re1 <+> text "=>" <+> ppr re2
    pprPrec _ (AntiLitsR s _)    = pprAnti "lits" s
    pprPrec _ (AntiExpsR s _)    = pprAnti "exps" s

instance Located RichExp where
    locOf (ExpR e)         = locOf e
    locOf (CallR _ _ l)    = locOf l
    locOf (CastR _ _ l)    = locOf l
    locOf (RangeR _ l)     = locOf l
    locOf (SubtypeR _ l)   = locOf l
    locOf (OpenR l)        = locOf l
    locOf (OthersR l)      = locOf l
    locOf (ParensR _ l)    = locOf l
    locOf (LabeledR _ _ l) = locOf l
    locOf (InertialR _ l)  = locOf l
    locOf (ChoicesR _ l)   = locOf l
    locOf (AssocR _ _ l)   = locOf l
    locOf (AntiLitsR _ l)  = locOf l
    locOf (AntiExpsR _ l)  = locOf l

checkIdentifier :: Name -> P Id
checkIdentifier (SimpleN [] ident _) = pure ident
checkIdentifier re =
    parserError re $ text "Expected identifier but got" <+> ppr re

-- | Check that a RichExp is actually an expression
checkExp :: RichExp -> P Exp
checkExp (ExpR e) =
    pure e

-- function_call
checkExp (CallR f args l) =
    checkArray `catch` \(_ :: ParserException) -> checkFun
  where
    checkArray, checkFun :: P Exp
    checkArray = VarE <$> checkArrayIndexOrSlice f args l <*> pure l
    checkFun   = CallE f <$> mapM checkArg args <*> pure l

-- type_conversion
checkExp (CastR ty (ParensR [re] _) l) =
    CastE ty <$> checkExp re <*> pure l

--  ( expression )
checkExp (ParensR [re] _) =
    checkExp re

-- aggregate
checkExp re =
    AggE <$> checkAggregate re <*> pure (srclocOf re)

-- | Check that a RichExp is actually an aggregate
checkAggregate :: RichExp -> P [ElemAssoc]
checkAggregate (ParensR res _) =
    mapM checkElemAssoc res

checkAggregate (AntiLitsR s l) =
    pure [AntiLitsElemAssoc s l]

checkAggregate (AntiExpsR s l) =
    pure [AntiExpsElemAssoc s l]

checkAggregate re =
    parserError re $ text "Expected aggregate but got" <+> ppr re

checkElemAssoc :: RichExp -> P ElemAssoc
checkElemAssoc (AssocR re1 re2 l) =
    ElemAssoc <$> checkChoices re1 <*> checkExp re2 <*> pure l

checkElemAssoc (AntiLitsR s l) =
    pure $ AntiLitsElemAssoc s l

checkElemAssoc (AntiExpsR s l) =
    pure $ AntiExpsElemAssoc s l

checkElemAssoc re =
    ElemAssoc [] <$> checkExp re <*> pure (srclocOf re)

checkLabeledChoices :: RichExp -> P (Maybe Id, Choices)
checkLabeledChoices (LabeledR l re _) = (,) <$> pure (Just l) <*> checkChoices re
checkLabeledChoices re                = (,) <$> pure Nothing <*> checkChoices re
checkLabeledChoices re =
    parserError re $ text "Expected labeled expression but got" <+> ppr re

checkChoice :: RichExp -> P Choice
checkChoice (ExpR e)    = pure $ ExpC e (srclocOf e)
checkChoice (OthersR l) = pure $ OthersC (srclocOf l)
checkChoice re =
    parserError re $ text "Expected choice but got" <+> ppr re

checkChoices :: RichExp -> P [Choice]
checkChoices (ChoicesR cs _) = pure cs
checkChoices re              = (:) <$> checkChoice re <*> pure []
checkChoices re =
    parserError re $ text "Expected choices but got" <+> ppr re

checkArg :: RichExp -> P Arg
checkArg (AssocR re1 re2 l) = do
    f <- checkFormalPart re1
    a <- checkActualPart re2
    pure $ AssocElem (Just f) a l

checkArg re = do
    a <- checkActualPart re
    pure $ AssocElem Nothing a (srclocOf re)

checkFormalPart :: RichExp -> P FormalPart
checkFormalPart = go
  where
    go :: RichExp -> P FormalPart
    go (CallR f [re] l) = FunPart f <$> checkName re <*> pure l
    go (CastR ty re l)  = TypePart ty <$> checkName re <*> pure l
    go re               = Part <$> checkName re <*> pure (srclocOf re)

    checkName :: RichExp -> P Name
    checkName (ExpR (VarE name _)) = pure name
    checkName re =
        parserError re $ text "Expected name but got" <+> ppr re

checkActualPart :: RichExp -> P ActualPart
checkActualPart = go
  where
    go :: RichExp -> P ActualPart
    go (CallR f [re] l) = FunPart f <$> checkDesignator re <*> pure l
    go (CastR ty re l)  = TypePart ty <$> checkDesignator re <*> pure l
    go re               = Part <$> checkDesignator re <*> pure (srclocOf re)

    checkDesignator :: RichExp -> P ActualDesignator
    checkDesignator (ExpR (VarE name _)) =
        pure $ NameA name (srclocOf name)

    checkDesignator (InertialR e l) =
        pure $ ExpA True e l

    checkDesignator re@(CastR _ _ l) =
        SubtypeA <$> checkSubtypeIndication re <*> pure l

    checkDesignator re@(SubtypeR ty l) =
        SubtypeA <$> checkSubtypeIndication re <*> pure l

    checkDesignator (OpenR l) =
        pure $ OpenA l

    checkDesignator re = do
        e <- checkExp re
        pure $ ExpA False e (srclocOf re)

checkArrayConstraint :: RichExp -> Maybe Constraint -> P Constraint
checkArrayConstraint re@(ParensR [OpenR{}] _) c =
    pure $ ArrayOpenC c (re `srcspan` c)

checkArrayConstraint re c' = do
    c <- checkIndexConstraint re
    pure $ ArrayC c c' (re `srcspan` c')

checkIndexConstraint :: RichExp -> P IndexConstraint
checkIndexConstraint (ParensR res l) =
    IndexConstraint <$> mapM checkDiscreteRange res <*> pure l

checkIndexConstraint re =
    parserError re $ text "Expected index constraint but got" <+> ppr re

checkElementConstraint :: RichExp -> P Constraint
checkElementConstraint re =
    checkArrayConstraint re Nothing
    `catch` \(_ :: ParserException) -> checkRecordConstraint re

checkRecordConstraint :: RichExp -> P Constraint
checkRecordConstraint (ParensR res l) =
    RecordC <$> mapM checkRecordElementConstraint res <*> pure l

checkRecordConstraint re =
    parserError re $ text "Expected record constraint but got" <+> ppr re

checkRecordElementConstraint :: RichExp -> P (Id, Constraint)
checkRecordElementConstraint (CallR (SimpleN [] n _) res _) = do
    c <- checkElementConstraint (ParensR res (srclocOf res))
    pure (n, c)

checkRecordElementConstraint re =
    parserError re $ text "Expected record elementconstraint but got" <+> ppr re

checkDiscreteRange :: RichExp -> P DiscreteRange
checkDiscreteRange (RangeR rng l) = pure $ RangeDR rng l
checkDiscreteRange re             = SubtypeDR <$> checkSubtypeIndication re
                                              <*> pure (srclocOf re)

checkSubtypeIndication :: RichExp -> P Subtype
checkSubtypeIndication (CastR ty re l) = do
    c <- checkElementConstraint re
    pure $ Subtype Nothing ty (Just c) l

checkSubtypeIndication (SubtypeR ty _) =
    pure ty

checkSubtypeIndication re =
    parserError re $ text "Expected subtype but got" <+> ppr re

checkArrayIndexOrSlice :: Name -> [RichExp] -> SrcLoc -> P Name
checkArrayIndexOrSlice n [re] l =
    checkSlice `catch` \(_ :: ParserException) -> checkIndexed
  where
    checkSlice, checkIndexed :: P Name
    checkSlice   = SliceN n <$> checkDiscreteRange re <*> pure l
    checkIndexed = IndexedN n <$> ((:[]) <$> checkExp re) <*> pure l

checkArrayIndexOrSlice n res l = do
    es <- mapM checkExp res
    pure $ IndexedN n es l

checkArrayIndexOrSlice _ res l =
    parserError res $ text "Expected array index but got" <+> ppr (ParensR res l)

checkGenerateSpec :: RichExp -> P GenSpec
checkGenerateSpec (ExpR (VarE (SimpleN [] ident l) _)) =
    pure $ AltG ident l

checkGenerateSpec re =
    (RangeG <$> checkDiscreteRange re <*> pure (srclocOf re))
    `catch` \(_ :: ParserException) -> ExpG <$> checkExp re <*> pure (srclocOf re)

type FunProcInst = Name -> Maybe Sig -> Maybe GenericMapAspect -> SrcLoc -> Decl

checkProcInst :: Decl -> P FunProcInst
checkProcInst (ProcSpecD base Nothing [] l) =
    pure $ \name sig gmap l' -> ProcInstD base name sig gmap (l `srcspan` l')

checkFunProcInst decl =
    parserError decl $
    text "Expected procedure base name but got" <+> ppr decl

data FunSpecP = FunSpecP Name (Maybe Purity) (Maybe SubprogramHeader) !SrcLoc
  deriving (Eq, Ord, Show)

instance Located FunSpecP where
    locOf (FunSpecP _ _ _ l) = locOf l

lexer :: (L T.Token -> P a) -> P a
lexer cont = do
    t <- lexToken
    setCurToken t
    cont t

locate :: Loc -> (SrcLoc -> a) -> L a
locate loc f = L loc (f (SrcLoc loc))

data RevList a  =  RNil
                |  RCons a (RevList a)
                |  RApp [a] (RevList a)

rnil :: RevList a
rnil = RNil

rsingleton :: a -> RevList a
rsingleton x = RCons x RNil

infixr 5 `rcons`

rcons :: a -> RevList a -> RevList a
rcons x xs  = RCons x xs

rapp :: [a] -> RevList a -> RevList a
rapp xs ys  = RApp xs ys

rlist :: [a] -> RevList a
rlist xs = rlist' xs rnil
  where
    rlist' []     acc = acc
    rlist' (x:xs) acc = rlist' xs (rcons x acc)

rev :: RevList a -> [a]
rev xs = go [] xs
  where
    go  l  RNil          = l
    go  l  (RCons x xs)  = go (x : l) xs
    go  l  (RApp xs ys)  = go (xs ++ l) ys

instance Located a => Located (RevList a) where
    locOf RNil         = mempty
    locOf (RCons x xs) = locOf x `mappend` locOf xs
    locOf (RApp xs ys) = locOf xs `mappend` locOf ys
}
