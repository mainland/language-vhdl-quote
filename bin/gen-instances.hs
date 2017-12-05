{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Data.Generics

import Language.VHDL.Syntax

import Derive

main :: IO ()
main = do
#define DERIVE(a) deriveM deriveLocated (undefined::a)
    DERIVE(ActualDesignator)
    DERIVE(AssocElem)
    DERIVE(AssocPart ActualDesignator)
    DERIVE(BaseName)
    DERIVE(BindingIndication)
    DERIVE(CStm)
    DERIVE(Choice)
    DERIVE(Conditional Exp)
    DERIVE(Constraint)
    DERIVE(Context)
    DERIVE(Decl)
    DERIVE(DelayMechanism)
    DERIVE(DiscreteRange)
    DERIVE(ElemAssoc)
    DERIVE(EntityAspect)
    DERIVE(EntityDesignator)
    DERIVE(Exp)
    DERIVE(GenBody)
    DERIVE(GenericClause)
    DERIVE(GenericMapAspect)
    DERIVE(IDecl)
    DERIVE(Id)
    DERIVE(IfaceGenericMapAspect)
    DERIVE(IndexConstraint)
    DERIVE(InstUnit)
    DERIVE(InterfaceSubprogramDefault)
    DERIVE(Lit)
    DERIVE(Name)
    DERIVE(NameList Label)
    DERIVE(Operator)
    DERIVE(PortClause)
    DERIVE(PortMapAspect)
    DERIVE(Purity)
    DERIVE(QualExp)
    DERIVE(Range)
    DERIVE(Resolution)
    DERIVE(Selected Exp)
    DERIVE(Sig)
    DERIVE(Stm)
    DERIVE(SubprogramHeader)
    DERIVE(Subtype)
    DERIVE(Target)
    DERIVE(UseClause)
    DERIVE(Wave)
