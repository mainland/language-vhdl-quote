{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Language.VHDL.Syntax
-- Copyright   : (c) 2016-2021 Drexel University
-- License     : BSD-style
-- Author      : Geoffrey Mainland <mainland@drexel.edu>
-- Maintainer  : Geoffrey Mainland <mainland@drexel.edu>

module Language.VHDL.Syntax where

import Data.Char (toLower)
import Data.Data (Data(..))
import Data.Loc (Loc(NoLoc),
                 Located(..),
                 SrcLoc,
                 noLoc,
                 srclocOf,
                 srcspan)
#if !(MIN_VERSION_base(4,9,0))
import Data.Monoid (Monoid(..), (<>))
#endif /* !(MIN_VERSION_base(4,9,0)) */
#if MIN_VERSION_base(4,9,0) && !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif
import Data.String
import Data.Symbol
import Data.Typeable (Typeable)
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

data Extension = Antiquotation
  deriving (Eq, Ord, Enum, Show)

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

data Config = Block BlockSpec [UseClause] [Config] !SrcLoc
            | Component ComponentSpec [BindingIndication] (Maybe Config) !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty Config where
    ppr (Block spec uses confs _) =
      pprFor (ppr spec) ([ppr u <> semi | u <- uses] ++ map ppr confs)

    ppr (Component spec binds maybe_conf _) =
      pprFor (ppr spec) (map ppr binds ++ [ppr maybe_conf])

data BlockSpec = ArchB Name !SrcLoc
               | BlockLblB Label !SrcLoc
               | GenLblB Label (Maybe GenSpec) !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty BlockSpec where
    ppr (ArchB n _) =
      ppr n

    ppr (BlockLblB lbl _) =
      ppr lbl

    ppr (GenLblB lbl maybe_gen _) =
      ppr lbl <+> case maybe_gen of
                    Nothing  -> empty
                    Just gen -> parens (ppr gen)

data GenSpec = RangeG DiscreteRange !SrcLoc
             | ExpG Exp !SrcLoc
             | AltG Label !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty GenSpec where
    ppr (RangeG rng _) = ppr rng
    ppr (ExpG e _)     = ppr e
    ppr (AltG lbl _)   = ppr lbl

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

data Purity = Pure !SrcLoc
            | Impure !SrcLoc
 deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty Purity where
    ppr Pure{}   = text "pure"
    ppr Impure{} = text "Impure"

data SubprogramHeader = SubprogramHeader [IDecl] (Maybe GenericMapAspect) !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty SubprogramHeader where
    ppr (SubprogramHeader idecls genmap _) =
        text "generic" <+> parens (ppr idecls) <+/>
        ppr genmap

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

[§ 4.4]

subprogram_instantiation_declaration ::=
  subprogram_kind designator is new uninstantiated_subprogram_name [ signature ]
    [ generic_map_aspect ] ;

[§ 4.5]

signature ::= [ [ type_mark { , type_mark } ] [ return type_mark ] ]
-}

data Sig = Sig [TypeMark] (Maybe TypeMark) !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty Sig where
    ppr (Sig args maybe_ret _) =
        brackets $
        commasep (map ppr args ) <+>
        case maybe_ret of
          Nothing  -> empty
          Just ret -> text "return" <+> ppr ret

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

[§ 4.9]

package_instantiation_declaration ::=
  package identifier is new uninstantiated_package_name
    [ generic_map_aspect ] ;
-}

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

[§ 5.2.2]

enumeration_type_definition ::=
  ( enumeration_literal { , enumeration_literal } )

enumeration_literal ::= identifier | character_literal

[§ 5.2.3]

integer_type_definition ::= range_constraint

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

[§ 5.2.5]

floating_type_definition ::= range_constraint
-}

data Range = Range Exp Direction Exp !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty Range where
  ppr (Range e1 dir e2 _) = ppr e1 <+> ppr dir <+> ppr e2

data Direction = To !SrcLoc
               | DownTo !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty Direction where
  ppr To{}     = text "to"
  ppr DownTo{} = text "downto"

type UnitName = Name

{-
[§ 5.3.1]

composite_type_definition ::=
    array_type_definition
  | record_type_definition

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

data IndexConstraint = IndexConstraint [DiscreteRange] !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty IndexConstraint where
  ppr (IndexConstraint rngs _) =
    parens (commasep (map ppr rngs))

data DiscreteRange = SubtypeDR Subtype !SrcLoc
                   | RangeDR Range !SrcLoc
                   | AntiRange String !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty DiscreteRange where
    ppr (SubtypeDR subtype _) = ppr subtype
    ppr (RangeDR rng _)       = ppr rng
    ppr (AntiRange rng _)     = pprAnti "range" rng

type RecordTypeName = Name

{-
[§ 5.4.1]

access_type_definition ::= access subtype_indication

[§ 5.4.2]

incomplete_type_declaration ::= type identifier ;

[§ 5.5]

file_type_definition ::= file of type_mark

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

{-
[§ 6]
-}

data Decl = EntityD Id (Maybe GenericClause) (Maybe PortClause) [Decl] [CStm] !SrcLoc
          | ArchD Id Name [Decl] [CStm] !SrcLoc
          | ConfigD Id Name [Decl] [BindingIndication] Config !SrcLoc
          | SubtypeD Id Subtype !SrcLoc
          | TypeD Id (Maybe Type) !SrcLoc
          | ConstD [Id] Subtype (Maybe Exp) !SrcLoc
          | SignalD [Id] Subtype (Maybe SignalKind) (Maybe Exp) !SrcLoc
          | VarD Bool [Id] Subtype (Maybe Exp) !SrcLoc
          | FileD [Id] Subtype (Maybe FileOpenInfo) !SrcLoc
          | AliasD Name (Maybe Subtype) Name (Maybe Sig) !SrcLoc
          | AttrD Id TypeMark !SrcLoc
          | ComponentD Id (Maybe GenericClause) (Maybe PortClause) (Maybe Name) !SrcLoc
          | GroupTemplateD Id [EntityClass] Bool !SrcLoc
          | GroupD Id Name [Name] !SrcLoc
          | AttrSpecD Name (NameList EntityDesignator) EntityClass Exp !SrcLoc
          | ConfigSpecD ComponentSpec [BindingIndication] !SrcLoc
          | DisconnectSpecD (NameList Name) TypeMark Exp !SrcLoc
          | ProcSpecD Name (Maybe SubprogramHeader) [IDecl] !SrcLoc
          | FunSpecD Name (Maybe Purity) (Maybe SubprogramHeader) [IDecl] TypeMark !SrcLoc
          | ProcD Name (Maybe SubprogramHeader) [IDecl] [Decl] [Stm] !SrcLoc
          | FunD Name (Maybe Purity) (Maybe SubprogramHeader) [IDecl] TypeMark [Decl] [Stm] !SrcLoc
          | ProcInstD Name Name (Maybe Sig) (Maybe GenericMapAspect) !SrcLoc
          | FunInstD Name Name (Maybe Sig) (Maybe GenericMapAspect) !SrcLoc
          | PkgD Id (Maybe GenHeader) [Decl] !SrcLoc
          | PkgBodyD Name [Decl] !SrcLoc
          | PkgInstD Id Name (Maybe GenericMapAspect) !SrcLoc
          | DisconnectD SignalList TypeMark Exp !SrcLoc
          | ContextD Id [Context] !SrcLoc
          | UseD UseClause !SrcLoc
          | AntiDecl String !SrcLoc
          | AntiDecls String !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty Decl where
    ppr (EntityD ident gens ports [] [] _) =
        nest 2 (text "entity" <+> ppr ident <+> text "is" </>
                ppr gens </>
                ppr ports) </>
        text "end" <+> ppr ident

    ppr (EntityD ident gens ports decls [] _) =
        nest 2 (text "entity" <+> ppr ident <+> text "is" </>
                ppr gens </>
                ppr ports </>
                ppr decls) </>
        text "end" <+> ppr ident

    ppr (EntityD ident gens ports decls stms _) =
        pprIsBeginEnd (text "entity" <+> ppr ident)
                      (ppr gens </> ppr ports </> ppr decls)
                      (ppr stms)
                      (Just (ppr ident))

    ppr (ArchD ident name decls stms _) =
        pprIsBeginEnd (text "architecture" <+> ppr ident <+> text "of" <+> ppr name)
                      (ppr decls)
                      (ppr stms)
                      (Just (ppr ident))

    ppr (ConfigD ident name decls binds config _) =
        pprIsEnd (text "configuration" <+> ppr ident <+> text "of" <+> ppr name)
                 (ppr decls </> ppr binds </> ppr config)

    ppr (SubtypeD ident subty _) =
        text "subtype" <+> ppr ident <+> text "is" <+> ppr subty

    ppr (TypeD ident ty _) =
        text "type" <+> ppr ident <+> pprMaybe (text "is") ty

    ppr (ConstD idents subty e _) =
        text "constant" <+> commasep (map ppr idents) <+>
        colon <+> ppr subty <+>
        pprMaybe ":=" e

    ppr (SignalD idents subty kind e _) =
        text "signal" <+> commasep (map ppr idents) <+>
        colon <+> ppr subty <+>
        ppr kind <+>
        pprMaybe ":=" e

    ppr (VarD isShared idents subty e _) =
        text "variable" <+>
        if isShared then "shared" else empty <+>
        commasep (map ppr idents) <+>
        colon <+> ppr subty <+>
        pprMaybe ":=" e

    ppr (FileD idents subty info _) =
        text "file" <+>
        commasep (map ppr idents) <+>
        colon <+> ppr subty <+>
        ppr info

    ppr (AliasD alias subty name sig _) =
        text "alias" <+> ppr alias <+> pprMaybe colon subty <+>
        text "is" <+> ppr name <+> ppr sig

    ppr (AttrD attr ty _) =
        text "attribute" <+> ppr attr <+> colon <+> ppr ty

    ppr (ComponentD ident gens ports _ _) =
        nest 2 (text "component" <+> ppr ident </>
                ppr gens </>
                ppr ports) </>
        text "end component"

    ppr (GroupTemplateD ident entities isOpen _) =
        nest 2 $
        text "group" <+> ppr ident <+> text "is" <+/>
        parens (commasep (map ppr entities) <+>
        if isOpen then text "<>" else empty)

    ppr (GroupD ident template constituents _) =
        text "group" <+> ppr ident <+> colon <+> ppr template <+>
        parens (commasep (map ppr constituents))

    ppr (AttrSpecD n desig cls e _) =
        text "attribute" <+> ppr n <+> text "of" <+>
        ppr desig <+> colon <+> ppr cls <+> text "is" <+> ppr e

    ppr (ConfigSpecD compspec binds _) =
        pprFor (ppr compspec) (map ppr binds)

    ppr (DisconnectSpecD sigs ty e _) =
        text "disconnect" <+> ppr sigs <+> colon <+> ppr ty <+>
        text "after" <+> ppr e

    ppr (ProcSpecD f hdr params _) =
        text "procedure" <+> ppr f <+> ppr hdr <+>
        pprParams params

    ppr (FunSpecD f purity hdr params ret_ty _) =
        ppr purity <+>
        text "function" <+> ppr f <+> ppr hdr <+>
        pprParams params <+>
        text "return" <+> ppr ret_ty

    ppr (ProcD f hdr params decls stms _) =
        pprIsBeginEnd (ppr (ProcSpecD f hdr params noLoc))
                      (ppr decls)
                      (ppr stms)
                      Nothing

    ppr (FunD f purity hdr params ret_ty decls stms _) =
        pprIsBeginEnd (ppr (FunSpecD f purity hdr params ret_ty noLoc))
                      (ppr decls)
                      (ppr stms)
                      Nothing

    ppr (ProcInstD inst f sig genmap _) =
        text "procedure" <+> ppr inst <+> text "is new" <+> ppr f <+>
        ppr sig <+> ppr genmap

    ppr (FunInstD inst f sig genmap _) =
        text "function" <+> ppr inst <+> text "is new" <+> ppr f <+>
        ppr sig <+> ppr genmap

    ppr (PkgD pkg hdr decls _) =
        nest 2 (text "package" <+> ppr pkg <+> text "is" </>
                ppr hdr <> semi </>
                ppr decls) </>
        text "end"

    ppr (PkgBodyD pkg decls _) =
        nest 2 (text "package body" <+> ppr pkg <+> text "is" </>
              ppr decls) </>
        text "end"

    ppr (PkgInstD inst pkg genmap _) =
        text "package" <+> ppr inst <+> text "is new" <+> ppr pkg <+> ppr genmap

    ppr (DisconnectD sigs ty e _) =
        text "disconnect" <+> ppr sigs <+> colon <+> ppr ty <+> text "after" <+> ppr e

    ppr (ContextD ident cs _) =
        nest 2 (text "context" <+> ppr ident <+> text "is" <+/>
                commasep (map ppr cs)) <+/>
        text "end"

    ppr (UseD use _) =
        ppr use

    ppr (AntiDecl decl _) =
        pprAnti "decl" decl

    ppr (AntiDecls decls _) =
        pprAnti "decls" decls

    pprList = semistackall . map ppr

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

data Type = EnumT [Lit] !SrcLoc
          | IntT Range !SrcLoc
          | PhysT Range Id [(Id, Lit)] (Maybe UnitName) !SrcLoc
          | FloatT Range !SrcLoc
          | ArrT IndexConstraint Subtype !SrcLoc
          | ArrUnboundedT [TypeMark] Subtype !SrcLoc
          | RecT [([Id], Subtype)] (Maybe RecordTypeName) !SrcLoc
          | AccessT Subtype !SrcLoc
          | FileT TypeMark !SrcLoc
          | ProtectedT [Decl] !SrcLoc
          | ProtectedBodyT [Decl] !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty Type where
    ppr (EnumT lits _) =
      parens (commasep (map ppr lits))

    ppr (IntT rng _) =
      ppr rng

    ppr (PhysT cons pri sec unit _) =
      ppr cons <+/>
      nest 2 (text "units" </>
              ppr pri <> semi </>
              stack [ppr u <+> text "=" <+> ppr lit <> semi | (u, lit) <- sec]) </>
      text "end units" <+> ppr unit

    ppr (FloatT rng _) =
      ppr rng

    ppr (ArrT ixs ty _) =
      text "array" <+> ppr ixs <+> text "of" <+> ppr ty

    ppr (ArrUnboundedT ixs ty _) =
      text "array" <+>
      parens (commasep [ppr subty <+> text "range" <+> text "<>" | subty <- ixs]) <+>
      text "of" <+> ppr ty

    ppr (RecT fldss n _) =
      nest 2 (text "record" </>
              stack [commasep (map ppr flds) <+> colon <+> ppr ty <> semi | (flds, ty) <- fldss]) </>
      text "end record" <+> ppr n

    ppr (AccessT subty _) =
      text "access" <+> ppr subty

    ppr (FileT ty _) =
      text "file of" <+> ppr ty

    ppr (ProtectedT decls _) =
      nest 2 (text "protected" </> ppr decls) </>
      text "end protected"

    ppr (ProtectedBodyT decls _) =
      nest 2 (text "protected body" </> ppr decls) </>
      text "end protected body"

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

data Subtype = Subtype (Maybe Resolution) TypeMark (Maybe Constraint) !SrcLoc
             | AntiType String !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty Subtype where
    ppr (Subtype res ty cons _) = ppr res <+> ppr ty <+> ppr cons
    ppr (AntiType e _)          = pprAnti "ty" e

data Resolution = FunRes Name !SrcLoc
                | ArrayElemRes Resolution !SrcLoc
                | RecordRes [(Name, Resolution)] !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty Resolution where
    ppr (FunRes n _) =
      ppr n

    ppr (ArrayElemRes res _) =
      parens (ppr res)

    ppr (RecordRes flds _) =
      parens (commasep [ppr fld <+> ppr res | (fld, res) <- flds])

type TypeMark = Name

data Constraint = RangeC Range !SrcLoc
                | ArrayC IndexConstraint (Maybe Constraint) !SrcLoc
                | ArrayOpenC (Maybe Constraint) !SrcLoc
                | RecordC [(Id, Constraint)] !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty Constraint where
    ppr (RangeC rng _) =
      text "range" <+> ppr rng

    ppr (ArrayC ic ec _) =
      ppr ic <+> ppr ec

    ppr (ArrayOpenC ec _) =
      parens (text "open") <+> ppr ec

    ppr (RecordC ecs _) =
      parens $
      commasep [ppr n <+> ppr c | (n, c) <- ecs]

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

data SignalKind = Register !SrcLoc
                | Bus !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty SignalKind where
    ppr Register{} = text "register"
    ppr Bus{}      = text "bus"

data FileOpenInfo = FileOpenInfo (Maybe Exp) Exp
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty FileOpenInfo where
  ppr (FileOpenInfo kind name) =
    pprMaybe "open" kind <+> text "is" <+> ppr name

{-
[§ 6.5]

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

interface_type_declaration ::=
  interface_incomplete_type_declaration

interface_incomplete_type_declaration ::= type identifier

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

interface_package_declaration ::=
  package identifier is new uninstantiated_package_name interface_package_generic_map_aspect

interface_package_generic_map_aspect ::=
    generic_map_aspect
  | generic map ( <> )
  | generic map ( default )

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

data IDecl = ConstID [Id] Subtype (Maybe Exp) !SrcLoc
           | SignalID [Id] Mode Subtype (Maybe SignalKind) (Maybe Exp) !SrcLoc
           | VarID Bool [Id] Mode Subtype (Maybe Exp) !SrcLoc
           | FileID [Id] Subtype !SrcLoc
           | TypeID Id !SrcLoc
           | ProcID Name [IDecl] (Maybe InterfaceSubprogramDefault) !SrcLoc
           | FunID Name (Maybe Purity) [IDecl] TypeMark (Maybe InterfaceSubprogramDefault) !SrcLoc
           | PkgInstID Id Name IfaceGenericMapAspect !SrcLoc
           | AntiIDecl String !SrcLoc
           | AntiIDecls String !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty IDecl where
    ppr (ConstID idents subty e _) =
      text "constant" <+> commasep (map ppr idents) <+>
      colon <+> ppr subty <+>
      pprMaybe ":=" e

    ppr (SignalID idents mode subty kind e _) =
      text "signal" <+> commasep (map ppr idents) <+>
      colon <+> ppr mode <+> ppr subty <+>
      ppr kind <+>
      pprMaybe ":=" e

    ppr (VarID var idents mode subty e _) =
        if var
          then text "variable" <+> rest
          else rest
      where
        rest :: Doc
        rest = commasep (map ppr idents) <+>
               colon <+> ppr mode <+> ppr subty <+>
               pprMaybe ":=" e

    ppr (FileID idents subty _) =
      text "file" <+>
      commasep (map ppr idents) <+>
      colon <+> ppr subty

    ppr (TypeID ident _) =
      text "type" <+> ppr ident

    ppr (ProcID f args n _) =
      text "procedure" <+> ppr f <> pprArgs args <+>
      pprMaybe (text "is") n

    ppr (FunID f purity args ty n _) =
      ppr purity <+> text "function" <+>
      ppr f <> pprArgs args <+>
      text "return" <+> ppr ty <+>
      pprMaybe (text "is") n

    ppr (PkgInstID ident n genmap _) =
      text "package" <+> ppr ident <+> text "is new" <+> ppr n <+> ppr genmap

    ppr (AntiIDecl idecl _) =
        pprAnti "idecl" idecl

    ppr (AntiIDecls idecls _) =
        pprAnti "idecls" idecls

    pprList = semistack . map ppr

data InterfaceSubprogramDefault = SubprogramD Name !SrcLoc
                                | AllD !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty InterfaceSubprogramDefault where
    ppr (SubprogramD n _) = ppr n
    ppr AllD{}            = text "<>"

data IfaceGenericMapAspect = IGenericMapAspect [AssocElem] !SrcLoc
                           | IGenericMapAll !SrcLoc
                           | IGenericMapDefault !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty IfaceGenericMapAspect where
    ppr (IGenericMapAspect elems _) =
      text "generic map" <+> parens (ppr elems)

    ppr IGenericMapAll{} =
      text "generic map (<>)"

    ppr IGenericMapDefault{} =
      text "generic map (default)"

data Mode = In
          | Out
          | InOut
          | Buffer
          | Linkage
  deriving (Eq, Ord, Enum, Show, Data, Typeable)

instance Pretty Mode where
    ppr In      = text "in"
    ppr Out     = text "out"
    ppr InOut   = text "inout"
    ppr Buffer  = text "buffer"
    ppr Linkage = text "linkage"

data GenericClause = GenericClause [IDecl] !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty GenericClause where
    ppr (GenericClause idecls _) =
      text "generic" <+> parens (ppr idecls) <> semi

data PortClause = PortClause [IDecl] !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty PortClause where
    ppr (PortClause idecls _) =
      text "port" <+> parens (ppr idecls) <> semi

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

data AssocElem = AssocElem (Maybe FormalPart) ActualPart !SrcLoc
               | AntiAssocElem String !SrcLoc
               | AntiAssocElems String !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty AssocElem where
    ppr (AssocElem formal actual _) =
      case formal of
        Nothing -> empty
        Just x  -> ppr x <+> text "=>"
      <+>
      ppr actual

    ppr (AntiAssocElem assoc _) =
        pprAnti "assoc" assoc

    ppr (AntiAssocElems assocs _) =
        pprAnti "assocs" assocs

    pprList elems =
      commasep (map ppr elems)

data GenericMapAspect = GenericMapAspect [AssocElem] !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty GenericMapAspect where
    ppr (GenericMapAspect elems _) =
      text "generic map" <+> parens (ppr elems)

data PortMapAspect = PortMapAspect [AssocElem] !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty PortMapAspect where
    ppr (PortMapAspect elems _) =
      text "port map" <+> parens (ppr elems)

type FormalPart = AssocPart Name

type ActualPart = AssocPart ActualDesignator

data AssocPart a = Part a !SrcLoc
                 | FunPart Name a !SrcLoc
                 | TypePart TypeMark a !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty a => Pretty (AssocPart a) where
    ppr (Part x _)        = ppr x
    ppr (FunPart f x _)   = ppr f  <> parens (ppr x)
    ppr (TypePart ty x _) = ppr ty <> parens (ppr x)

data ActualDesignator = ExpA Bool Exp !SrcLoc
                      | NameA Name !SrcLoc
                      | SubtypeA Subtype !SrcLoc
                      | OpenA !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty ActualDesignator where
    ppr(ExpA isInertial e _) =
      if isInertial then "inertial" else empty <+>
      ppr e

    ppr (NameA n _) =
      ppr n

    ppr (SubtypeA subty _) =
      ppr subty

    ppr OpenA{} =
      text "open"

{-
[§ 6.6]

alias_declaration ::=
  alias alias_designator [ : subtype_indication ] is name [ signature ] ;

alias_designator ::= identifier | character_literal | operator_symbol

[§ 6.7]

attribute_declaration ::=
  attribute identifier : type_mark ;

[§ 6.8]

component_declaration ::=
  component identifier [ is ]
    [ local_generic_clause ]
    [ local_port_clause ]
  end component [ component_simple_name ] ;

[§ 6.9]

group_template_declaration ::=
  group identifier is ( entity_class_entry_list ) ;

entity_class_entry_list ::=
  entity_class_entry { , entity_class_entry }

entity_class_entry ::= entity_class [ <> ]

[§ 6.10]

group_declaration ::=
  group identifier : group_template_name ( group_constituent_list ) ;

group_constituent_list ::= group_constituent { , group_constituent }

group_constituent ::= name | character_literal
-}

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

data EntityDesignator = EntityDesignator Name (Maybe Sig) !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty EntityDesignator where
    ppr (EntityDesignator tag sig _) = ppr tag <+> ppr sig

data EntityClass = EntityC
                 | ArchitectureC
                 | ConfigurationC
                 | ProcedureC
                 | FunctionC
                 | PackageC
                 | TypeC
                 | SubtypeC
                 | ConstantC
                 | SignalC
                 | VariableC
                 | ComponentC
                 | LabelC
                 | LiteralC
                 | UnitsC
                 | GroupC
                 | FileC
                 | PropertyC
                 | SequenceC
  deriving (Eq, Ord, Show, Enum, Data, Typeable)

instance Pretty EntityClass where
    ppr EntityC        = text "entity"
    ppr ArchitectureC  = text "architecture"
    ppr ConfigurationC = text "configuration"
    ppr ProcedureC     = text "procedure"
    ppr FunctionC      = text "function"
    ppr PackageC       = text "package"
    ppr TypeC          = text "type"
    ppr SubtypeC       = text "subtype"
    ppr ConstantC      = text "constant"
    ppr SignalC        = text "signal"
    ppr VariableC      = text "variable"
    ppr ComponentC     = text "component"
    ppr LabelC         = text "label"
    ppr LiteralC       = text "literal"
    ppr UnitsC         = text "units"
    ppr GroupC         = text "group"
    ppr FileC          = text "file"
    ppr PropertyC      = text "property"
    ppr SequenceC      = text "sequence"

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

data ComponentSpec = ComponentSpec Name (NameList Label) !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty ComponentSpec where
    ppr (ComponentSpec comp lbls _) = ppr lbls <+> colon <+> ppr comp

data BindingIndication = EntityB EntityAspect (Maybe GenericMapAspect) (Maybe PortMapAspect) !SrcLoc
                       | VerifB [Name] !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty BindingIndication where
    ppr (EntityB ent gens ports _) =
      text "use" <+> ppr ent <+> ppr gens <+> ppr ports

    ppr (VerifB units _) =
      text "use vunit" <+> commasep (map ppr units)

data EntityAspect = EntityEA Name (Maybe Id) !SrcLoc
                  | ConfigEA Name !SrcLoc
                  | OpenEA !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty EntityAspect where
    ppr (EntityEA ent maybe_arch _) =
      text "entity" <+> ppr ent <+>
      case maybe_arch of
        Nothing   -> empty
        Just arch -> parens (ppr arch)

    ppr (ConfigEA n _) =
      text "configuration" <+> ppr n

    ppr OpenEA{} =
      text "open"

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

type SignalList = NameList Name

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

data Name = SimpleN [Id] Id !SrcLoc
          | OpN [Id] Operator !SrcLoc
          | EnumN [Id] String !SrcLoc
          | SelN Name Id !SrcLoc
          | IndexedN Name [Exp] !SrcLoc
          | SliceN Name DiscreteRange !SrcLoc
          | AttrN Name (Maybe Sig) AttrDesignator (Maybe Exp) !SrcLoc
          | ExtConstN ExtPath Subtype !SrcLoc
          | ExtSigN ExtPath Subtype !SrcLoc
          | ExtVarN ExtPath Subtype !SrcLoc
          | CallN Name [Exp] !SrcLoc
          | AllN Name !SrcLoc
          | AntiName String !SrcLoc
          | AntiNames String !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance IsString Name where
    fromString s = SimpleN [] (fromString s) noLoc

instance Pretty Name where
    ppr (SimpleN prefix n _) =
        folddoc (<>) (punctuate dot (map ppr prefix ++ [ppr n]))

    ppr (OpN prefix n _) =
        folddoc (<>) (punctuate dot (map ppr prefix ++ [ppr n]))

    ppr (EnumN prefix n _) =
        folddoc (<>) (punctuate dot (map ppr prefix ++ [ppr n]))

    ppr (SelN n ele _) =
        ppr n <> text "." <> ppr ele

    ppr (IndexedN n es _) =
        ppr n <> parens (commasep (map ppr es))

    ppr (SliceN n rng _) =
        ppr n <> parens (ppr rng)

    ppr (AttrN n sig desig e _) =
        ppr n <> ppr sig <> char '\'' <>
        ppr desig <> maybe empty ppr e

    ppr (ExtConstN path subty _) =
        text "<<" <> text "signal" <+>
        ppr path <+> colon <+> ppr subty <>
        text ">>"

    ppr (ExtSigN path subty _) =
        text "<<" <> text "signal" <+>
        ppr path <+> colon <+> ppr subty <>
        text ">>"

    ppr (ExtVarN path subty _) =
        text "<<" <> text "variable" <+>
        ppr path <+> colon <+> ppr subty <>
        text ">>"

    ppr (CallN n es _) =
        ppr n <> parens (commasep (map ppr es))

    ppr (AllN n _) =
        ppr n <> text "." <> text "all"

    ppr (AntiName e _) =
        pprAnti "name" e

    ppr (AntiNames e _) =
        pprAnti "names" e

data NameList a = Some [a] !SrcLoc
                | Others !SrcLoc
                | All !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty a => Pretty (NameList a) where
    ppr (Some ns _) = commasep (map ppr ns)
    ppr Others{}    = text "others"
    ppr All{}       = text "all"

data Operator = Operator Symbol !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty Operator where
    ppr (Operator sym _) = text (unintern sym)

type AttrDesignator = Name

type Prefix = Name

type Suffix = Name

data ExtPath = PkgP [Id] Id !SrcLoc
             | AbsP PartialPath !SrcLoc
             | RelP Int PartialPath !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty ExtPath where
    ppr (PkgP ns n _) =
        char '@' <//> cat (punctuate dot (map ppr (ns ++ [n])))

    ppr (AbsP path _) =
        dot <//> ppr path

    ppr (RelP n path _) =
        cat (replicate n (text "^.")) <//>
        ppr path

data PartialPath = PartialPath [PathElem] Name !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty PartialPath where
    ppr (PartialPath elems n _) =
        cat (punctuate dot (map ppr elems ++ [ppr n]))

data PathElem = NameP Id !SrcLoc
              | LabelP Label !SrcLoc
              | GenLabelP Label (Maybe Exp) !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty PathElem where
    ppr (NameP n _) =
        ppr n

    ppr (LabelP lbl _) =
        ppr lbl

    ppr (GenLabelP lbl maybe_e _) =
        ppr lbl <>
        case maybe_e of
          Nothing -> empty
          Just e  -> parens (ppr e)

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
-}

data Exp = VarE Name !SrcLoc
         | LitE Lit !SrcLoc
         | UnopE Unop Exp !SrcLoc
         | BinopE Binop Exp Exp !SrcLoc
         | AggE Aggregate !SrcLoc
         | CallE Name [Arg] !SrcLoc
         | QualE QualExp !SrcLoc
         | CastE TypeMark Exp !SrcLoc
         | AllocTyE Subtype !SrcLoc
         | AllocE QualExp !SrcLoc
         | AntiExp String !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty Exp where
    pprPrec _ (VarE v _) =
        ppr v

    pprPrec _ (LitE l _) =
        ppr l

    pprPrec p (UnopE op e _) =
        parensIf (p > precOf op) $
        pprUnop (pprPrec (precOf op) e)
      where
        pprUnop :: Doc -> Doc
        pprUnop doc
          | needSpace op = ppr op <+> doc
          | otherwise    = ppr op <> doc

        needSpace :: Unop -> Bool
        needSpace Cond = False
        needSpace Plus = False
        needSpace Neg  = False
        needSpace _    = True

    -- nand and nor expressions must always be parenthesized
    pprPrec _ (BinopE op e1 e2 _s) | isNotLogicalOp op =
        infixop (opPrec+1) op e1 e2
      where
        isNotLogicalOp :: Binop -> Bool
        isNotLogicalOp Nand = True
        isNotLogicalOp Nor  = True
        isNotLogicalOp _    = False

        Fixity _ opPrec = fixity op

    -- and, or, xor, and xnor expressions must be parenthesized when they are
    -- the subterm of a logical expression with same same precedence but a
    -- *different* operator.
    pprPrec p (BinopE op e1 e2 _s) | isLogicalOp op =
        infixop' e1 e2
      where
        isLogicalOp :: Binop -> Bool
        isLogicalOp And  = True
        isLogicalOp Or   = True
        isLogicalOp Xor  = True
        isLogicalOp Xnor = True
        isLogicalOp _    = False

        infixop' :: Exp -> Exp -> Doc
        infixop' l r =
            parensIf (p > opPrec) $
            pprPrec leftPrec l <+> ppr op <+/> pprPrec rightPrec r
          where
            leftPrec | opAssoc == RightAssoc = opPrec + 1
                     | otherwise             = opPrec + bias l

            rightPrec | opAssoc == LeftAssoc = opPrec + 1
                      | otherwise            = opPrec + bias r

            Fixity opAssoc opPrec = fixity op

        -- Add precedence bias if we encounter a sub-expression involving a
        -- *different* logical operator to force subexpression to be
        -- parenthesized.
        bias :: Exp -> Int
        bias (BinopE op' _ _ _) | isLogicalOp op && op' /= op = 1
        bias _                                                = 0

    pprPrec p (BinopE op e1 e2 _s) =
        infixop p op e1 e2

    pprPrec _ (AggE elems _) =
        parens $ commasep (map ppr elems)

    pprPrec _ (CallE f params _) =
        ppr f <> parens (commasep (map ppr params))

    pprPrec _ (QualE qe _) =
        ppr qe

    pprPrec _ (CastE ty e _) =
        ppr ty <> parens (ppr e)

    pprPrec _ (AllocTyE subty _) =
        text "new" <+> ppr subty

    pprPrec _ (AllocE qe _) =
        text "new" <+> ppr qe

    pprPrec _ (AntiExp e _) =
        pprAnti "exp" e

{-
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

data Unop = Cond -- ^ Conditional conversion
          | Plus -- ^ Positive
          | Neg  -- ^ Negate
          | Abs  -- ^ Absolute value
          | Not  -- ^ Not
          | UAnd
          | UOr
          | UNand
          | UNor
          | UXor
          | UXnor
  deriving (Eq, Ord, Enum, Show, Data, Typeable)

instance HasFixity Unop where
    fixity Cond  = infixl_ 0
    fixity Plus  = infixl_ 5
    fixity Neg   = infixl_ 5
    fixity Abs   = infixl_ 7
    fixity Not   = infixl_ 7
    fixity UAnd  = infixl_ 1
    fixity UOr   = infixl_ 1
    fixity UNand = infixl_ 1
    fixity UNor  = infixl_ 1
    fixity UXor  = infixl_ 1
    fixity UXnor = infixl_ 1

instance Pretty Unop where
    ppr Cond  = text "??"
    ppr Plus  = char '+'
    ppr Neg   = char '-'
    ppr Abs   = text "abs"
    ppr Not   = text "not"
    ppr UAnd  = text "and"
    ppr UOr   = text "or"
    ppr UNand = text "nand"
    ppr UNor  = text "nor"
    ppr UXor  = text "xor"
    ppr UXnor = text "xnor"

data Binop = And
           | Or
           | Nand
           | Nor
           | Xor
           | Xnor
           | Eq
           | Ne
           | Lt
           | Le
           | Ge
           | Gt
           | EqM -- ^ Matching equal
           | NeM -- ^ Matching not equal
           | LtM -- ^ Matching less than
           | LeM -- ^ Matching less than or equal
           | GeM -- ^ Matching greater than or equal
           | GtM -- ^ Matching greater than
           | Sll -- ^ Shift left logical
           | Srl -- ^ Shift right logical
           | Sla -- ^ Shift left arithmetic
           | Sra -- ^ Shift right arithmetic
           | Rol -- ^ Rotate left logical
           | Ror -- ^ Rotate right logical
           | Add -- ^ Addition
           | Sub -- ^ Subtraction
           | Cat -- ^ Concatenation
           | Mul -- ^ Multiplication
           | Div -- ^ Division
           | Mod -- ^ Modulus
           | Rem -- ^ Remainder
           | Pow -- ^ Exponentiation
  deriving (Eq, Ord, Enum, Show, Data, Typeable)

instance HasFixity Binop where
    fixity And  = infixr_ 1
    fixity Or   = infixr_ 1
    fixity Nand = infixr_ 1
    fixity Nor  = infixr_ 1
    fixity Xor  = infixr_ 1
    fixity Xnor = infixr_ 1
    fixity Eq   = infixl_ 2
    fixity Ne   = infixl_ 2
    fixity Lt   = infixl_ 2
    fixity Le   = infixl_ 2
    fixity Ge   = infixl_ 2
    fixity Gt   = infixl_ 2
    fixity EqM  = infixl_ 2
    fixity NeM  = infixl_ 2
    fixity LtM  = infixl_ 2
    fixity LeM  = infixl_ 2
    fixity GeM  = infixl_ 2
    fixity GtM  = infixl_ 2
    fixity Sll  = infixl_ 3
    fixity Srl  = infixl_ 3
    fixity Sla  = infixl_ 3
    fixity Sra  = infixl_ 3
    fixity Rol  = infixl_ 3
    fixity Ror  = infixl_ 3
    fixity Add  = infixl_ 4
    fixity Sub  = infixl_ 4
    fixity Cat  = infixl_ 4
    fixity Mul  = infixl_ 6
    fixity Div  = infixl_ 6
    fixity Mod  = infixl_ 6
    fixity Rem  = infixl_ 6
    fixity Pow  = infixl_ 7

instance Pretty Binop where
    ppr And  = text "and"
    ppr Or   = text "or"
    ppr Nand = text "nand"
    ppr Nor  = text "nor"
    ppr Xor  = text "xor"
    ppr Xnor = text "xnor"
    ppr Eq   = text "="
    ppr Ne   = text "/="
    ppr Lt   = text "<"
    ppr Le   = text "<="
    ppr Ge   = text ">="
    ppr Gt   = text ">"
    ppr EqM  = text "?="
    ppr NeM  = text "?/="
    ppr LtM  = text "?<"
    ppr LeM  = text "?<="
    ppr GeM  = text "?>="
    ppr GtM  = text "?>"
    ppr Sll  = text "sll"
    ppr Srl  = text "srl"
    ppr Sla  = text "sla"
    ppr Sra  = text "sra"
    ppr Rol  = text "rol"
    ppr Ror  = text "ror"
    ppr Add  = char '+'
    ppr Sub  = char '-'
    ppr Cat  = char '&'
    ppr Mul  = char '*'
    ppr Div  = char '/'
    ppr Mod  = text "mod"
    ppr Rem  = text "rem"
    ppr Pow  = text "**"

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

data Lit = BoolLit      Bool             !SrcLoc
         | IntLit       String Integer   !SrcLoc
         | RealLit      String Rational  !SrcLoc
         | IdLit        Id               !SrcLoc
         | CharLit      String Char      !SrcLoc
         | StringLit    String String    !SrcLoc
         | BitStringLit String           !SrcLoc
         | PhysLit      (Maybe Lit) Name !SrcLoc
         | Null                          !SrcLoc
         | AntiInt      String           !SrcLoc
         | AntiReal     String           !SrcLoc
         | AntiLit      String           !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty Lit where
    ppr (BoolLit True _)   = text "true"
    ppr (BoolLit False _)  = text "false"
    ppr (IntLit s _ _)     = text s
    ppr (RealLit s _ _)    = text s
    ppr (IdLit ident _)    = ppr ident
    ppr (CharLit s _ _)    = text s
    ppr (StringLit s _ _)  = text s
    ppr (BitStringLit s _) = text s
    ppr (PhysLit lit n _)  = ppr lit <+> ppr n
    ppr Null{}             = text "null"
    ppr (AntiInt e _)      = pprAnti "int" e
    ppr (AntiReal e _)     = pprAnti "real" e
    ppr (AntiLit e _)      = pprAnti "lit" e

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

type Aggregate = [ElemAssoc]

data ElemAssoc = ElemAssoc Choices Exp !SrcLoc
               | AntiExpsElemAssoc String !SrcLoc
               | AntiLitsElemAssoc String !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty ElemAssoc where
    ppr (ElemAssoc [] e _)      = ppr e
    ppr (ElemAssoc choices e _) = ppr choices <+> text "=>" <+> ppr e
    ppr (AntiExpsElemAssoc s _) = pprAnti "exps" s
    ppr (AntiLitsElemAssoc s _) = pprAnti "lits" s

    pprList elems = parens (commasep (map ppr elems))

type Choices = [Choice]

data Choice = ExpC Exp !SrcLoc
            | DiscreteRangeC DiscreteRange !SrcLoc
            | ElemC Name !SrcLoc
            | OthersC !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty Choice where
    ppr (ExpC e _)             = ppr e
    ppr (DiscreteRangeC rng _) = ppr rng
    ppr (ElemC n _)            = ppr n
    ppr OthersC{}              = text "others"

    pprList choices =
      folddoc (\x y -> x <+> char '|' <+> y) (map ppr choices)

{-
[§ 9.3.4]

function_call ::=
  function_name [ ( actual_parameter_part ) ]

actual_parameter_part ::= parameter_association_list
-}

type Arg = AssocElem

{-
[§ 9.3.5]

qualified_expression ::=
    type_mark ' ( expression )
  | type_mark ' aggregate
-}

data QualExp = QualExp TypeMark Exp !SrcLoc
             | QualAgg TypeMark Aggregate !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty QualExp where
    ppr (QualExp ty e _) =
        ppr ty <> char '\'' <> parens (ppr e)

    ppr (QualAgg ty agg _) =
        ppr ty <> char '\'' <> ppr agg

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

data Stm = LabelS Label Stm !SrcLoc
         | WaitS [SignalName] (Maybe Cond) (Maybe TimeExp) !SrcLoc
         | AssertS Cond (Maybe Exp) (Maybe Exp) !SrcLoc
         | ReportS Exp (Maybe Exp) !SrcLoc
         | SigAssnS Target SignalRhs !SrcLoc
         | VarAssnS Target VarRhs !SrcLoc
         | CallS Name [Arg] !SrcLoc
         | IfS [(Cond, [Stm])] (Maybe [Stm]) (Maybe Label) !SrcLoc
         | CaseS Exp Bool [(Choices, [Stm])] (Maybe Label) !SrcLoc
         | WhileS Cond [Stm] (Maybe Label) !SrcLoc
         | ForS Id DiscreteRange [Stm] (Maybe Label) !SrcLoc
         | NextS (Maybe Label) (Maybe Cond) !SrcLoc
         | ExitS (Maybe Label) (Maybe Cond) !SrcLoc
         | ReturnS (Maybe Exp) !SrcLoc
         | NullS !SrcLoc
         | AntiStm String !SrcLoc
         | AntiStms String !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty Stm where
    ppr (LabelS lbl stm _) =
        nest 2 $
        ppr lbl <> colon <+/> ppr stm

    ppr (WaitS sensitivity condition timeout _) =
        text "wait" <+>
        commasep (map ppr sensitivity) <+>
        pprMaybe (text "until") condition <+>
        pprMaybe (text "for") timeout

    ppr (AssertS cond report severity _) =
        text "assert" <+>
        ppr cond <+>
        pprMaybe (text "report") report <+>
        pprMaybe (text "severity") severity

    ppr (ReportS report severity _) =
        text "report" <+>
        ppr report <+>
        pprMaybe (text "severity") severity

    ppr (SigAssnS t rhs _) = go rhs
      where
        go :: SignalRhs -> Doc
        go (WaveRhs delay wav _) =
            ppr t <+> text "<=" <+> ppr delay <+> ppr wav

        go (ForceRhs mode e _) =
            ppr t <+> text "<=" <+> text "force" <+> ppr mode <+> ppr e

        go (ReleaseRhs mode _) =
            ppr t <+> text "<=" <+> text "release" <+> ppr mode

        go (CondWaveRhs delay cond_wavs _) =
            ppr t <+> text "<=" <+> ppr delay <+> ppr cond_wavs

        go (CondForceRhs mode cond_es _) =
            ppr t <+> text "<=" <+> text "force" <+> ppr mode <+> ppr cond_es

        go (SelWaveRhs e isMatch delay sel_wavs _) =
            nest 2 $
            text "with" <+> ppr e <+> text "select" <+> if isMatch then char '?' else empty <+/>
            ppr t <+> text "<=" <+> ppr delay <+> ppr sel_wavs

        go (SelForceRhs e isMatch mode sel_es _) =
            nest 2 $
            text "with" <+> ppr e <+> text "select" <+> if isMatch then char '?' else empty <+/>
            ppr t <+> text "<=" <+> text "force" <+> ppr mode <+> ppr sel_es

    ppr (VarAssnS t rhs _) = go rhs
      where
        go :: VarRhs -> Doc
        go (VarRhs e _) =
            ppr t <+> text ":=" <+> ppr e

        go (CondRhs cond_es _) =
            ppr t <+> text ":=" <+> ppr cond_es

        go (SelRhs e isMatch sel_es _) =
            nest 2 $
            text "with" <+> ppr e <+> text "select" <+> if isMatch then char '?' else empty <+/>
            ppr t <+> text ":=" <+> ppr sel_es

    ppr (CallS f params _) =
        ppr f <> parens (commasep (map ppr params))

    ppr (IfS [] _ _ _) =
        error "IfS: can't happen"

    ppr (IfS (b:bs) maybe_else _ _) =
       stack (pprIf b : map pprElseIf bs) </>
       pprElse maybe_else </>
       text "end if"
      where
        pprIf :: (Cond, [Stm]) -> Doc
        pprIf (cond, stms) =
          nest 2 $
          text "if" <+> ppr cond <+> text "then" </> ppr stms

        pprElseIf :: (Cond, [Stm]) -> Doc
        pprElseIf (cond, stms) =
          nest 2 $
          text "elsif" <+> ppr cond <+> text "then" </> ppr stms

        pprElse :: Maybe [Stm] -> Doc
        pprElse Nothing     = empty
        pprElse (Just stms) = nest 2 $ text "else" </> ppr stms

    ppr (CaseS e isMatch alts _ _) =
        nest 2 (text "case" <+> q <+>
                ppr e <+> text "is" </>
                stack (map pprAlt alts)) </>
        text "end case" <+> q
      where
        q :: Doc
        q = if isMatch then char '?' else empty

        pprAlt :: (Choices, [Stm]) -> Doc
        pprAlt (cs, stms) =
          nest 2 $
          text "when" <+> ppr cs <+> text "=>" </>
          ppr stms

    ppr (WhileS cond stms _ _) =
        nest 2 (text "while" <+> ppr cond <+> text "loop" </> ppr stms) </>
        text "end loop"

    ppr (ForS ident rng stms _ _) =
        nest 2 (text "for" <+> ppr ident <+> text "in" <+> ppr rng <+> text "loop" </> ppr stms) </>
        text "end loop"

    ppr (NextS lbl cond _) =
        text "next" <+> ppr lbl <+> pprMaybe (text "when") cond

    ppr (ExitS lbl cond _) =
        text "exit" <+> ppr lbl <+> pprMaybe (text "when") cond

    ppr (ReturnS e _) =
        text "return" <+> ppr e

    ppr NullS{} =
        text "null"

    ppr (AntiStm stm _) =
        pprAnti "stm" stm

    ppr (AntiStms stms _) =
        pprAnti "stms" stms

    pprList = semistackall . map ppr

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

type SignalName = Name

type Cond = Exp

type TimeExp = Exp

{-
[§ 10.3]
assertion_statement ::= [ label : ] assertion ;

assertion ::=
  assert condition
    [ report expression ]
    [ severity expression ]

[§ 10.4]
report_statement ::=
  [ label : ]
    report expression
    [ severity expression ] ;
-}

{-
[§ 10.5]

signal_assignment_statement ::=
    [ label : ] simple_signal_assignment
  | [ label : ] conditional_signal_assignment
  | [ label : ] selected_signal_assignment
-}

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

data SignalRhs = WaveRhs (Maybe DelayMechanism) Waveform !SrcLoc
               | ForceRhs (Maybe Mode) Exp !SrcLoc
               | ReleaseRhs (Maybe Mode) !SrcLoc
               | CondWaveRhs (Maybe DelayMechanism) (Conditional Waveform) !SrcLoc
               | CondForceRhs (Maybe Mode) (Conditional Exp) !SrcLoc
               | SelWaveRhs Exp Bool (Maybe DelayMechanism) (Selected Waveform) !SrcLoc
               | SelForceRhs Exp Bool (Maybe Mode) (Selected Exp) !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

data DelayMechanism = Transport !SrcLoc
                    | Inertial (Maybe Exp) !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty DelayMechanism where
    ppr Transport{}    = text "transport"
    ppr (Inertial e _) = pprMaybe (text "reject") e <+> text "intertial"

data Target = NameT Name !SrcLoc
            | AggT Aggregate !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty Target where
    ppr (NameT n _)  = ppr n
    ppr (AggT agg _) = ppr agg

type Waveform = [Wave]

data Wave = Wave (Maybe Exp) (Maybe Exp) !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty Wave where
    ppr (Wave maybe_e maybe_after _) =
      case maybe_e of
        Nothing -> text "null"
        Just e  -> ppr e
      <+>
      case maybe_after of
        Nothing    -> empty
        Just after -> text "after" <+> ppr after

    pprList elems =
      case elems of
        [] -> text "unaffected"
        _  -> commasep (map ppr elems)

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

data Conditional a = NilC
                   | FinC a !SrcLoc
                   | GuardC a Exp (Conditional a) !SrcLoc
                   | AntiCond String !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable, Functor)

instance Pretty a => Pretty (Conditional a) where
    ppr (FinC x _)     = ppr x
    ppr (AntiCond c _) = pprAnti "cond" c

    ppr cond = elsesep (pprCond cond)
      where
        pprCond :: Pretty a => Conditional a -> [Doc]
        pprCond NilC                = []
        pprCond (FinC x _)          = [ppr x]
        pprCond (GuardC x e rest _) = pprGuarded x e : pprCond rest
        pprCond (AntiCond c _)      = [pprAnti "cond" c]

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

data Selected a = Selected [(a, Choices)] !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty a => Pretty (Selected a) where
    ppr (Selected xs _) =
      commasep [ppr x <+> text "when" <+> ppr choice | (x, choice) <- xs]

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

data VarRhs = VarRhs Exp !SrcLoc
            | CondRhs (Conditional Exp) !SrcLoc
            | SelRhs Exp Bool (Selected Exp) !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

{-
[§ 10.7]

procedure_call_statement ::= [ label : ] procedure_call ;

procedure_call ::= procedure_name [ ( actual_parameter_part ) ]

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

[§ 10.11]

next_statement ::=
  [ label : ] next [ loop_label ] [ when condition ] ;

[§ 10.12]

exit_statement ::=
  [ label : ] exit [ loop_label ] [ when condition ] ;

[§ 10.13]

return_statement ::=
  [ label : ] return [ expression ] ;

[§ 10.14]
null_statement ::=
  [ label : ] null ;
-}

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

data CStm = LabelCS Label CStm !SrcLoc
          | BlockS (Maybe Cond) (Maybe GenHeader) (Maybe PortHeader) [Decl] [CStm] !SrcLoc
          | ProcessS Bool (Maybe ProcessSensitivityList) [Decl] [Stm] !SrcLoc
          | ConcCallS Bool Name [Arg] !SrcLoc
          | ConcAssertS Bool Cond (Maybe Exp) (Maybe Exp) !SrcLoc
          | ConcSigAssnS Target Bool Bool (Maybe DelayMechanism) ConcSignalRhs !SrcLoc
          | InstS InstUnit (Maybe GenericMapAspect) (Maybe PortMapAspect) !SrcLoc
          | ForGenS Id DiscreteRange GenBody !SrcLoc
          | IfGenS [(Cond, GenAlt)] (Maybe GenAlt) !SrcLoc
          | CaseGenS Exp [(Choices, GenAlt)] !SrcLoc
          | AntiCStm String !SrcLoc
          | AntiCStms String !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty CStm where
    ppr (LabelCS lbl cstm _) =
        ppr lbl <> colon <+/> ppr cstm

    ppr (BlockS cond gens ports decls stms _) =
        nest 2 (text "block" <+> pprCond cond <+> text "is" <+/>
                ppr gens </> ppr ports </> ppr decls) </>
        nest 2 (text "begin" </> ppr stms) </>
        text "end block"
      where
        pprCond :: Maybe Cond -> Doc
        pprCond Nothing      = empty
        pprCond (Just cond') = parens (ppr cond')

    ppr (ProcessS isPostponed sense decls stms _) =
        nest 2 (postponed <+> text "process" <+>
                ppr sense <+>
                text "is" </>
                ppr decls) </>
        nest 2 (text "begin" </> ppr stms) </>
        text "end process"
      where
        postponed :: Doc
        postponed | isPostponed = text "postponed"
                  | otherwise = empty

    ppr (ConcCallS isPostponed f params _) =
        postponed <+> ppr f <> parens (commasep (map ppr params))
      where
        postponed :: Doc
        postponed | isPostponed = text "postponed"
                  | otherwise = empty

    ppr (ConcAssertS isPostponed cond report severity _) =
        postponed <+> text "assert" <+>
        ppr cond <+>
        pprMaybe (text "report") report <+>
        pprMaybe (text "severity") severity
      where
        postponed :: Doc
        postponed | isPostponed = text "postponed"
                  | otherwise = empty

    ppr (ConcSigAssnS t isPostponed isGuarded delay rhs _) =
        postponed <+> go rhs
      where
        go :: ConcSignalRhs -> Doc
        go (ConcWaveRhs wav _) =
            ppr t <+> text "<=" <+> guarded <+> ppr delay <+> ppr wav

        go (ConcCondRhs cond_wavs _) =
            ppr t <+> text "<=" <+> guarded <+> ppr delay <+> ppr cond_wavs

        go (ConcSelRhs e isMatch sel_wavs _) =
            nest 2 $
            text "with" <+> ppr e <+> text "select" <+> match <+/>
            ppr t <+> text "<=" <+> guarded <+> ppr delay <+> ppr sel_wavs
          where
            match :: Doc
            match | isMatch   = char '?'
                  | otherwise = empty

        postponed :: Doc
        postponed | isPostponed = text "postponed"
                  | otherwise = empty

        guarded :: Doc
        guarded | isGuarded = text "guarded"
                | otherwise = empty

    ppr (InstS inst gens ports _) =
        ppr inst <+> ppr gens <+> ppr ports

    ppr (ForGenS ident rng body _) =
        nest 2 (text "for" <+> ppr ident <+> text "in" <+> ppr rng <+> text "generate" </> ppr body) </>
        text "end generate"

    ppr (IfGenS [] _ _) =
        error "IfGenS: can't happen"

    ppr (IfGenS (alt:alts) maybe_alt _) =
       stack (pprIf alt : map pprElseIf alts) </>
       pprElse maybe_alt </>
       text "end generate"
      where
        pprIf :: (Cond, GenAlt) -> Doc
        pprIf (cond, GenAlt lbl body _) =
          nest 2 $
          text "if" <+> pprLabel lbl <+> ppr cond <+> text "generate" </> ppr body

        pprElseIf :: (Cond, GenAlt) -> Doc
        pprElseIf (cond, GenAlt lbl body _) =
          nest 2 $
          text "elsif" <+> pprLabel lbl <+> ppr cond <+> text "generate" </> ppr body

        pprElse :: Maybe GenAlt -> Doc
        pprElse Nothing =
            empty

        pprElse (Just (GenAlt lbl body _)) =
            nest 2 $
            text "else" <+> pprLabel lbl <+> text "generate" </> ppr body

        pprLabel :: Maybe Label -> Doc
        pprLabel Nothing    = empty
        pprLabel (Just lbl) = ppr lbl <+> colon

    ppr (CaseGenS e alts _) =
        nest 2 (text "case" <+> ppr e <+> text "generate" </>
                stack (map pprAlt alts)) </>
        text "end generate"
      where
        pprAlt :: (Choices, GenAlt) -> Doc
        pprAlt (cs, GenAlt lbl body _) =
          nest 2 $
          text "when" <+> pprLabel lbl <+> ppr cs <+> text "=>" </>
          ppr body

        pprLabel :: Maybe Label -> Doc
        pprLabel Nothing    = empty
        pprLabel (Just lbl) = ppr lbl <+> colon

    ppr (AntiCStm stm _) =
        pprAnti "cstm" stm

    ppr (AntiCStms stms _) =
        pprAnti "cstms" stms

    pprList = semistackall . map ppr

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

data GenHeader = GenHeader GenericClause (Maybe GenericMapAspect) !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty GenHeader where
    ppr (GenHeader gen genmap _) = ppr gen <+> ppr genmap

data PortHeader = PortHeader PortClause (Maybe PortMapAspect) !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty PortHeader where
    ppr (PortHeader port portmap _) = ppr port <+> ppr portmap

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

data ProcessSensitivityList = AllSense !SrcLoc
                            | SomeSense [Name] !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty ProcessSensitivityList where
    ppr AllSense{}         = parens (text "all")
    ppr (SomeSense sigs _) = parens (commasep (map ppr sigs))

{-
[§ 11.4]

concurrent_procedure_call_statement ::=
  [ label : ] [ postponed ] procedure_call ;

[§ 11.5]

concurrent_assertion_statement ::=
  [ label : ] [ postponed ] assertion ;
-}

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

data ConcSignalRhs = ConcWaveRhs Waveform !SrcLoc
                   | ConcCondRhs (Conditional Waveform) !SrcLoc
                   | ConcSelRhs Exp Bool (Selected Waveform) !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

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

data InstUnit = ComponentInst Name !SrcLoc
              | EntityInst Name (Maybe Id) !SrcLoc
              | ConfigInst Name !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty InstUnit where
    ppr (ComponentInst n _) =
      text "component" <+> ppr n

    ppr (EntityInst n maybe_ident _) =
      text "entity" <+> ppr n <+>
      case maybe_ident of
        Nothing    -> empty
        Just ident -> parens (ppr ident)

    ppr (ConfigInst n _) =
      text "configuration" <+> ppr n

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

type Label = Id

data GenAlt = GenAlt (Maybe Label) GenBody !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

data GenBody = GenBody [Decl] [CStm] !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty GenBody where
    ppr (GenBody [] stms _) =
      ppr stms

    ppr (GenBody decls stms _) =
      indent 2 (ppr decls) </>
      nest 2 (text "begin" </> ppr stms) </>
      text "end" <> semi

{-
[§ 12.4]

use_clause ::=
  use selected_name { , selected_name } ;
-}

data UseClause = UseClause [Name] !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty UseClause where
    ppr (UseClause ns _) = text "use" <+> commasep (map ppr ns)

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

data DesignUnit = DesignUnit [Context] Decl !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty DesignUnit where
    ppr (DesignUnit cs decl _) =
      semistackall (map ppr cs ++ [ppr decl])

    pprList units =
      stack (map ppr units)

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

data Context = LibC [Name] !SrcLoc
             | UseC [Name] !SrcLoc
             | ContextRefC [Name] !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty Context where
    ppr (LibC idents _) =
      text "library" <+> commasep (map ppr idents)

    ppr (UseC ns _) =
      text "use" <+> commasep (map ppr ns)

    ppr (ContextRefC ns _) =
      text "context" <+> commasep (map ppr ns)

    pprList = semistackall . map ppr

{-
[§ 15.4]
identifier ::= basic_identifier | extended_identifier

basic_identifier ::= letter { [ underline ] letter_or_digit }

letter_or_digit ::= letter | digit

letter ::= upper_case_letter | lower_case_letter

extended_identifier ::= \ graphic_character { graphic_character } \
-}

data NoCase = NoCase Symbol Symbol
  deriving (Show, Data, Typeable)

instance Eq NoCase where
    NoCase _ x == NoCase _ y = x == y

instance Ord NoCase where
    compare (NoCase _ x) (NoCase _ y) = compare x y

instance IsString NoCase where
    fromString s = NoCase (intern s) (intern (map toLower s))

instance Pretty NoCase where
    ppr (NoCase sym _) = text (unintern sym)

mkNoCase :: Symbol -> NoCase
mkNoCase s = NoCase s ((intern . map toLower . unintern) s)

data Id = Id NoCase !SrcLoc
        | ExtId Symbol !SrcLoc
        | AntiId String !SrcLoc
  deriving (Eq, Ord, Show, Data, Typeable)

instance IsString Id where
    fromString s = Id (fromString s) noLoc

instance Pretty Id where
    ppr (Id sym _)       = ppr sym
    ppr (ExtId sym _)    = enclose (char '\\') (char '\\') $
                           text (unintern sym)
    ppr (AntiId ident _) = pprAnti "id" ident

    pprList [] =
        empty

    pprList [n] =
        ppr n

    pprList (n:ns) =
        ppr n <> char '.' <> ppr ns

{-
[§ 15.5.1]
abstract_literal ::= decimal_literal | based_literal

[§ 15.5.2]
decimal_literal ::= integer [ . integer ] [ exponent ]
integer ::= digit { [ underline ] digit }
exponent ::= E [ + ] integer | E – integer
-}

--
-- Pretty-printing helpers
--

-- | The document @'punctuateAll' p ds@ obeys the law:
--
-- @'punctuateAll' p [d1, d2, ..., dn] = [d1 <> p, d2 <> p, ..., dn <> p]@
punctuateAll :: Doc -> [Doc] -> [Doc]
punctuateAll _ []     = []
punctuateAll p [d]    = [d <> p]
punctuateAll p (d:ds) = (d <> p) : punctuateAll p ds

-- | The document @'semistack' ds@ semicolon-stacks @ds@, aligning the
-- resulting document to the current nesting level.
semistackall :: [Doc] -> Doc
semistackall [] = empty
semistackall xs = (align . stack . punctuateAll semi) xs

-- | The document @'semistack' ds@ semicolon-stacks @ds@, aligning the
-- resulting document to the current nesting level.
semistack :: [Doc] -> Doc
semistack = align . stack . punctuate semi

pprMaybe :: Pretty a => Doc -> Maybe a -> Doc
pprMaybe _   Nothing  = empty
pprMaybe pfx (Just x) = pfx <+> ppr x

pprAnti :: Pretty a => String -> a -> Doc
pprAnti anti x = char '$' <> text anti <> colon <> ppr x

elsesep :: [Doc] -> Doc
elsesep = align . sep . punctuate (space <> text "else")

pprParams :: Pretty a => [a] -> Doc
pprParams params = parens (semisep (map ppr params))

pprArgs :: Pretty a => [a] -> Doc
pprArgs = pprParams

pprGuarded :: (Pretty a, Pretty b) => a -> b -> Doc
pprGuarded a b = ppr a <+> text "when" <+> ppr b

pprIsEnd :: Doc -> Doc -> Doc
pprIsEnd is decls =
    nest 2 (is <+> text "is" </> decls) </>
    text "end"

pprIsBeginEnd :: Doc -> Doc -> Doc -> Maybe Doc -> Doc
pprIsBeginEnd is decls body close =
    nest 2 (is <+> text "is" </> decls) </>
    nest 2 (text "begin" </> body) </>
    text "end" <+> ppr close

pprFor :: Doc -> [Doc] -> Doc
pprFor arg body =
    group $
    nest 2 (text "for" <+> arg </>
            stack (punctuate semi body)) </>
    text "end for"

--
-- Pretty-printing with fixity
--
data Fixity = Fixity Assoc Int
  deriving (Eq, Ord)

data Assoc = LeftAssoc | RightAssoc | NonAssoc
  deriving (Eq, Ord)

infix_ :: Int -> Fixity
infix_ = Fixity NonAssoc

infixl_ :: Int -> Fixity
infixl_ = Fixity LeftAssoc

infixr_ :: Int -> Fixity
infixr_ = Fixity RightAssoc

class HasFixity a where
    fixity :: a -> Fixity

precOf :: HasFixity a => a -> Int
precOf op = p
  where
    Fixity _ p = fixity op

infixop :: (Pretty a, Pretty b, Pretty op, HasFixity op)
        => Int -- ^ Precedence of context
        -> op  -- ^ Operator
        -> a   -- ^ Left argument
        -> b   -- ^ Right argument
        -> Doc
infixop prec op l r =
    parensIf (prec > opPrec) $
    pprPrec leftPrec l <+> ppr op <+/> pprPrec rightPrec r
  where
    leftPrec | opAssoc == RightAssoc = opPrec + 1
             | otherwise             = opPrec

    rightPrec | opAssoc == LeftAssoc = opPrec + 1
              | otherwise            = opPrec

    Fixity opAssoc opPrec = fixity op

#if !defined(ONLY_TYPEDEFS)

#include "Language/VHDL/Syntax-instances.hs"

ifS :: Exp -> [Stm] -> [Stm] -> Stm
ifS c t [IfS cs e lbl l] = IfS ((c, t) : cs) e lbl (c `srcspan` l)
ifS c t e                = IfS [(c, t)] (Just e) Nothing (c `srcspan` e)

mkId :: Symbol -> SrcLoc -> Id
mkId s loc = Id (NoCase s ((intern . map toLower . unintern) s)) loc

mkIdName :: Id -> Name
mkIdName ident = SimpleN [] ident (srclocOf ident)

#endif /* !defined(ONLY_TYPEDEFS) */
