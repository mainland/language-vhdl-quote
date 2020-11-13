instance Located ActualDesignator where
  locOf (ExpA _ _ l) = locOf l
  locOf (NameA _ l) = locOf l
  locOf (SubtypeA _ l) = locOf l
  locOf (OpenA l) = locOf l
instance Located AssocElem where
  locOf (AssocElem _ _ l) = locOf l
instance Located (AssocPart a) where
  locOf (Part _ l) = locOf l
  locOf (FunPart _ _ l) = locOf l
  locOf (TypePart _ _ l) = locOf l
instance Located BindingIndication where
  locOf (EntityB _ _ _ l) = locOf l
  locOf (VerifB _ l) = locOf l
instance Located CStm where
  locOf (BlockS _ _ _ _ _ l) = locOf l
  locOf (ProcessS _ _ _ _ l) = locOf l
  locOf (ConcCallS _ _ _ l) = locOf l
  locOf (ConcAssertS _ _ _ _ l) = locOf l
  locOf (ConcSigAssnS _ _ _ _ _ l) = locOf l
  locOf (InstS _ _ _ l) = locOf l
  locOf (ForGenS _ _ _ l) = locOf l
  locOf (IfGenS _ _ l) = locOf l
  locOf (CaseGenS _ _ l) = locOf l
  locOf (AntiCStm _ l) = locOf l
  locOf (AntiCStms _ l) = locOf l
instance Located Choice where
  locOf (ExpC _ l) = locOf l
  locOf (DiscreteRangeC _ l) = locOf l
  locOf (ElemC _ l) = locOf l
  locOf (OthersC l) = locOf l
instance Located (Conditional a) where
  locOf (Conditional _ _ l) = locOf l
instance Located Constraint where
  locOf (RangeC _ l) = locOf l
  locOf (ArrayC _ _ l) = locOf l
  locOf (ArrayOpenC _ l) = locOf l
  locOf (RecordC _ l) = locOf l
instance Located Context where
  locOf (LibC _ l) = locOf l
  locOf (UseC _ l) = locOf l
  locOf (ContextRefC _ l) = locOf l
instance Located Decl where
  locOf (EntityD _ _ _ _ _ l) = locOf l
  locOf (ArchD _ _ _ _ l) = locOf l
  locOf (ConfigD _ _ _ _ _ l) = locOf l
  locOf (SubtypeD _ _ l) = locOf l
  locOf (TypeD _ _ l) = locOf l
  locOf (ConstD _ _ _ l) = locOf l
  locOf (SignalD _ _ _ _ l) = locOf l
  locOf (VarD _ _ _ _ l) = locOf l
  locOf (FileD _ _ _ l) = locOf l
  locOf (AliasD _ _ _ _ l) = locOf l
  locOf (AttrD _ _ l) = locOf l
  locOf (ComponentD _ _ _ _ l) = locOf l
  locOf (GroupTemplateD _ _ _ l) = locOf l
  locOf (GroupD _ _ _ l) = locOf l
  locOf (AttrSpecD _ _ _ _ l) = locOf l
  locOf (ConfigSpecD _ _ l) = locOf l
  locOf (DisconnectSpecD _ _ _ l) = locOf l
  locOf (ProcSpecD _ _ _ l) = locOf l
  locOf (FunSpecD _ _ _ _ _ l) = locOf l
  locOf (ProcD _ _ _ _ _ l) = locOf l
  locOf (FunD _ _ _ _ _ _ _ l) = locOf l
  locOf (ProcInstD _ _ _ _ l) = locOf l
  locOf (FunInstD _ _ _ _ l) = locOf l
  locOf (PkgD _ _ _ l) = locOf l
  locOf (PkgBodyD _ _ l) = locOf l
  locOf (PkgInstD _ _ _ l) = locOf l
  locOf (DisconnectD _ _ _ l) = locOf l
  locOf (ContextD _ _ l) = locOf l
  locOf (UseD _ l) = locOf l
instance Located DelayMechanism where
  locOf (Transport l) = locOf l
  locOf (Inertial _ l) = locOf l
instance Located DiscreteRange where
  locOf (SubtypeDR _ l) = locOf l
  locOf (RangeDR _ l) = locOf l
instance Located ElemAssoc where
  locOf (ElemAssoc _ _ l) = locOf l
  locOf (AntiExpsElemAssoc _ l) = locOf l
  locOf (AntiLitsElemAssoc _ l) = locOf l
instance Located EntityAspect where
  locOf (EntityEA _ _ l) = locOf l
  locOf (ConfigEA _ l) = locOf l
  locOf (OpenEA l) = locOf l
instance Located EntityDesignator where
  locOf (EntityDesignator _ _ l) = locOf l
instance Located Exp where
  locOf (VarE _ l) = locOf l
  locOf (LitE _ l) = locOf l
  locOf (UnopE _ _ l) = locOf l
  locOf (BinopE _ _ _ l) = locOf l
  locOf (AggE _ l) = locOf l
  locOf (CallE _ _ l) = locOf l
  locOf (QualE _ l) = locOf l
  locOf (CastE _ _ l) = locOf l
  locOf (AllocTyE _ l) = locOf l
  locOf (AllocE _ l) = locOf l
  locOf (AntiExp _ l) = locOf l
instance Located ExtPath where
  locOf (PkgP _ _ l) = locOf l
  locOf (AbsP _ l) = locOf l
  locOf (RelP _ _ l) = locOf l
instance Located GenBody where
  locOf (GenBody _ _ l) = locOf l
instance Located GenericClause where
  locOf (GenericClause _ l) = locOf l
instance Located GenericMapAspect where
  locOf (GenericMapAspect _ l) = locOf l
instance Located IDecl where
  locOf (ConstID _ _ _ l) = locOf l
  locOf (SignalID _ _ _ _ _ l) = locOf l
  locOf (VarID _ _ _ _ _ l) = locOf l
  locOf (FileID _ _ l) = locOf l
  locOf (TypeID _ l) = locOf l
  locOf (ProcID _ _ _ l) = locOf l
  locOf (FunID _ _ _ _ _ l) = locOf l
  locOf (PkgInstID _ _ _ l) = locOf l
instance Located Id where
  locOf (Id _ l) = locOf l
  locOf (ExtId _ l) = locOf l
  locOf (AntiId _ l) = locOf l
instance Located IfaceGenericMapAspect where
  locOf (IGenericMapAspect _ l) = locOf l
  locOf (IGenericMapAll l) = locOf l
  locOf (IGenericMapDefault l) = locOf l
instance Located IndexConstraint where
  locOf (IndexConstraint _ l) = locOf l
instance Located InstUnit where
  locOf (ComponentInst _ l) = locOf l
  locOf (EntityInst _ _ l) = locOf l
  locOf (ConfigInst _ l) = locOf l
instance Located InterfaceSubprogramDefault where
  locOf (SubprogramD _ l) = locOf l
  locOf (AllD l) = locOf l
instance Located Lit where
  locOf (IntLit _ _ l) = locOf l
  locOf (RealLit _ _ l) = locOf l
  locOf (IdLit _ l) = locOf l
  locOf (CharLit _ _ l) = locOf l
  locOf (StringLit _ _ l) = locOf l
  locOf (BitStringLit _ l) = locOf l
  locOf (PhysLit _ _ l) = locOf l
  locOf (Null l) = locOf l
  locOf (AntiInt _ l) = locOf l
  locOf (AntiReal _ l) = locOf l
  locOf (AntiLit _ l) = locOf l
instance Located Name where
  locOf (SimpleN _ _ l) = locOf l
  locOf (OpN _ _ l) = locOf l
  locOf (EnumN _ _ l) = locOf l
  locOf (SelN _ _ l) = locOf l
  locOf (AllN _ l) = locOf l
  locOf (IndexedN _ _ l) = locOf l
  locOf (SliceN _ _ l) = locOf l
  locOf (AttrN _ _ _ _ l) = locOf l
  locOf (ExtConstN _ _ l) = locOf l
  locOf (ExtSigN _ _ l) = locOf l
  locOf (ExtVarN _ _ l) = locOf l
instance Located (NameList a) where
  locOf (Some _ l) = locOf l
  locOf (Others l) = locOf l
  locOf (All l) = locOf l
instance Located Operator where
  locOf (Operator _ l) = locOf l
instance Located PartialPath where
  locOf (PartialPath _ _ l) = locOf l
instance Located PathElem where
  locOf (NameP _ l) = locOf l
  locOf (LabelP _ l) = locOf l
  locOf (GenLabelP _ _ l) = locOf l
instance Located PortClause where
  locOf (PortClause _ l) = locOf l
instance Located PortMapAspect where
  locOf (PortMapAspect _ l) = locOf l
instance Located Purity where
  locOf (Pure l) = locOf l
  locOf (Impure l) = locOf l
instance Located QualExp where
  locOf (QualExp _ _ l) = locOf l
  locOf (QualAgg _ _ l) = locOf l
instance Located Range where
  locOf (Range _ _ _ l) = locOf l
instance Located Resolution where
  locOf (FunRes _ l) = locOf l
  locOf (ArrayElemRes _ l) = locOf l
  locOf (RecordRes _ l) = locOf l
instance Located (Selected a) where
  locOf (Selected _ l) = locOf l
instance Located Sig where
  locOf (Sig _ _ l) = locOf l
instance Located Stm where
  locOf (LabelS _ _ l) = locOf l
  locOf (WaitS _ _ _ l) = locOf l
  locOf (AssertS _ _ _ l) = locOf l
  locOf (ReportS _ _ l) = locOf l
  locOf (SigAssnS _ _ l) = locOf l
  locOf (VarAssnS _ _ l) = locOf l
  locOf (CallS _ _ l) = locOf l
  locOf (IfS _ _ _ l) = locOf l
  locOf (CaseS _ _ _ _ l) = locOf l
  locOf (WhileS _ _ _ l) = locOf l
  locOf (ForS _ _ _ _ l) = locOf l
  locOf (NextS _ _ l) = locOf l
  locOf (ExitS _ _ l) = locOf l
  locOf (ReturnS _ l) = locOf l
  locOf (NullS l) = locOf l
  locOf (AntiStm _ l) = locOf l
  locOf (AntiStms _ l) = locOf l
instance Located SubprogramHeader where
  locOf (SubprogramHeader _ _ l) = locOf l
instance Located Subtype where
  locOf (Subtype _ _ _ l) = locOf l
  locOf (AntiType _ l) = locOf l
instance Located Target where
  locOf (NameT _ l) = locOf l
  locOf (AggT _ l) = locOf l
instance Located UseClause where
  locOf (UseClause _ l) = locOf l
instance Located Wave where
  locOf (Wave _ _ l) = locOf l
