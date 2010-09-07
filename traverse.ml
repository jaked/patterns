(* Generated automatically from the code
   
   class fold_map = Camlp4FoldMapGenerator.generated;;

   by invoking camlp4of 3.10.2 as follows:

   /usr/local/ocaml/bin/camlp4of -filter fold traverse.ml
 *)
open Camlp4.PreCast.Syntax.Ast
  
class fold_map =
  object ((o : 'self_type))
    method string : string -> ('self_type * string) = o#unknown
      
    method list :
      'a.
        ('self_type -> 'a -> ('self_type * 'a)) ->
          'a list -> ('self_type * ('a list)) =
      fun _f_a ->
        function
        | [] -> (o, [])
        | _x :: _x_i1 ->
            let (o, _x) = _f_a o _x in
            let (o, _x_i1) = o#list _f_a _x_i1 in (o, (_x :: _x_i1))
      
    method with_constr : with_constr -> ('self_type * with_constr) =
      function
      | WcNil _x -> let (o, _x) = o#loc _x in (o, (WcNil _x))
      | WcTyp (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in
          let (o, _x_i2) = o#ctyp _x_i2 in (o, (WcTyp (_x, _x_i1, _x_i2)))
      | WcMod (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ident _x_i1 in
          let (o, _x_i2) = o#ident _x_i2 in (o, (WcMod (_x, _x_i1, _x_i2)))
      | WcTyS (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in
          let (o, _x_i2) = o#ctyp _x_i2 in (o, (WcTyS (_x, _x_i1, _x_i2)))
      | WcMoS (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ident _x_i1 in
          let (o, _x_i2) = o#ident _x_i2 in (o, (WcMoS (_x, _x_i1, _x_i2)))
      | WcAnd (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#with_constr _x_i1 in
          let (o, _x_i2) = o#with_constr _x_i2
          in (o, (WcAnd (_x, _x_i1, _x_i2)))
      | WcAnt (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (WcAnt (_x, _x_i1)))
      
    method virtual_flag : virtual_flag -> ('self_type * virtual_flag) =
      function
      | ViVirtual -> (o, ViVirtual)
      | ViNil -> (o, ViNil)
      | ViAnt _x -> let (o, _x) = o#string _x in (o, (ViAnt _x))
      
    method str_item : str_item -> ('self_type * str_item) =
      function
      | StNil _x -> let (o, _x) = o#loc _x in (o, (StNil _x))
      | StCls (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#class_expr _x_i1 in (o, (StCls (_x, _x_i1)))
      | StClt (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#class_type _x_i1 in (o, (StClt (_x, _x_i1)))
      | StSem (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#str_item _x_i1 in
          let (o, _x_i2) = o#str_item _x_i2
          in (o, (StSem (_x, _x_i1, _x_i2)))
      | StDir (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#expr _x_i2 in (o, (StDir (_x, _x_i1, _x_i2)))
      | StExc (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in
          let (o, _x_i2) = o#meta_option (fun o -> o#ident) _x_i2
          in (o, (StExc (_x, _x_i1, _x_i2)))
      | StExp (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#expr _x_i1 in (o, (StExp (_x, _x_i1)))
      | StExt (_x, _x_i1, _x_i2, _x_i3) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#ctyp _x_i2 in
          let (o, _x_i3) = o#meta_list (fun o -> o#string) _x_i3
          in (o, (StExt (_x, _x_i1, _x_i2, _x_i3)))
      | StInc (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#module_expr _x_i1 in (o, (StInc (_x, _x_i1)))
      | StMod (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#module_expr _x_i2
          in (o, (StMod (_x, _x_i1, _x_i2)))
      | StRecMod (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#module_binding _x_i1
          in (o, (StRecMod (_x, _x_i1)))
      | StMty (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#module_type _x_i2
          in (o, (StMty (_x, _x_i1, _x_i2)))
      | StOpn (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ident _x_i1 in (o, (StOpn (_x, _x_i1)))
      | StTyp (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in (o, (StTyp (_x, _x_i1)))
      | StVal (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#rec_flag _x_i1 in
          let (o, _x_i2) = o#binding _x_i2 in (o, (StVal (_x, _x_i1, _x_i2)))
      | StAnt (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (StAnt (_x, _x_i1)))
      
    method sig_item : sig_item -> ('self_type * sig_item) =
      function
      | SgNil _x -> let (o, _x) = o#loc _x in (o, (SgNil _x))
      | SgCls (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#class_type _x_i1 in (o, (SgCls (_x, _x_i1)))
      | SgClt (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#class_type _x_i1 in (o, (SgClt (_x, _x_i1)))
      | SgSem (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#sig_item _x_i1 in
          let (o, _x_i2) = o#sig_item _x_i2
          in (o, (SgSem (_x, _x_i1, _x_i2)))
      | SgDir (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#expr _x_i2 in (o, (SgDir (_x, _x_i1, _x_i2)))
      | SgExc (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in (o, (SgExc (_x, _x_i1)))
      | SgExt (_x, _x_i1, _x_i2, _x_i3) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#ctyp _x_i2 in
          let (o, _x_i3) = o#meta_list (fun o -> o#string) _x_i3
          in (o, (SgExt (_x, _x_i1, _x_i2, _x_i3)))
      | SgInc (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#module_type _x_i1 in (o, (SgInc (_x, _x_i1)))
      | SgMod (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#module_type _x_i2
          in (o, (SgMod (_x, _x_i1, _x_i2)))
      | SgRecMod (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#module_binding _x_i1
          in (o, (SgRecMod (_x, _x_i1)))
      | SgMty (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#module_type _x_i2
          in (o, (SgMty (_x, _x_i1, _x_i2)))
      | SgOpn (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ident _x_i1 in (o, (SgOpn (_x, _x_i1)))
      | SgTyp (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in (o, (SgTyp (_x, _x_i1)))
      | SgVal (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#ctyp _x_i2 in (o, (SgVal (_x, _x_i1, _x_i2)))
      | SgAnt (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (SgAnt (_x, _x_i1)))
      
    method row_var_flag : row_var_flag -> ('self_type * row_var_flag) =
      function
      | RvRowVar -> (o, RvRowVar)
      | RvNil -> (o, RvNil)
      | RvAnt _x -> let (o, _x) = o#string _x in (o, (RvAnt _x))
      
    method rec_flag : rec_flag -> ('self_type * rec_flag) =
      function
      | ReRecursive -> (o, ReRecursive)
      | ReNil -> (o, ReNil)
      | ReAnt _x -> let (o, _x) = o#string _x in (o, (ReAnt _x))
      
    method rec_binding : rec_binding -> ('self_type * rec_binding) =
      function
      | RbNil _x -> let (o, _x) = o#loc _x in (o, (RbNil _x))
      | RbSem (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#rec_binding _x_i1 in
          let (o, _x_i2) = o#rec_binding _x_i2
          in (o, (RbSem (_x, _x_i1, _x_i2)))
      | RbEq (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ident _x_i1 in
          let (o, _x_i2) = o#expr _x_i2 in (o, (RbEq (_x, _x_i1, _x_i2)))
      | RbAnt (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (RbAnt (_x, _x_i1)))
      
    method private_flag : private_flag -> ('self_type * private_flag) =
      function
      | PrPrivate -> (o, PrPrivate)
      | PrNil -> (o, PrNil)
      | PrAnt _x -> let (o, _x) = o#string _x in (o, (PrAnt _x))
      
    method patt : patt -> ('self_type * patt) =
      function
      | PaNil _x -> let (o, _x) = o#loc _x in (o, (PaNil _x))
      | PaId (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ident _x_i1 in (o, (PaId (_x, _x_i1)))
      | PaAli (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#patt _x_i1 in
          let (o, _x_i2) = o#patt _x_i2 in (o, (PaAli (_x, _x_i1, _x_i2)))
      | PaAnt (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (PaAnt (_x, _x_i1)))
      | PaAny _x -> let (o, _x) = o#loc _x in (o, (PaAny _x))
      | PaApp (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#patt _x_i1 in
          let (o, _x_i2) = o#patt _x_i2 in (o, (PaApp (_x, _x_i1, _x_i2)))
      | PaArr (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#patt _x_i1 in (o, (PaArr (_x, _x_i1)))
      | PaCom (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#patt _x_i1 in
          let (o, _x_i2) = o#patt _x_i2 in (o, (PaCom (_x, _x_i1, _x_i2)))
      | PaSem (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#patt _x_i1 in
          let (o, _x_i2) = o#patt _x_i2 in (o, (PaSem (_x, _x_i1, _x_i2)))
      | PaChr (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (PaChr (_x, _x_i1)))
      | PaInt (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (PaInt (_x, _x_i1)))
      | PaInt32 (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (PaInt32 (_x, _x_i1)))
      | PaInt64 (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (PaInt64 (_x, _x_i1)))
      | PaNativeInt (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (PaNativeInt (_x, _x_i1)))
      | PaFlo (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (PaFlo (_x, _x_i1)))
      | PaLab (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#patt _x_i2 in (o, (PaLab (_x, _x_i1, _x_i2)))
      | PaOlb (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#patt _x_i2 in (o, (PaOlb (_x, _x_i1, _x_i2)))
      | PaOlbi (_x, _x_i1, _x_i2, _x_i3) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#patt _x_i2 in
          let (o, _x_i3) = o#expr _x_i3
          in (o, (PaOlbi (_x, _x_i1, _x_i2, _x_i3)))
      | PaOrp (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#patt _x_i1 in
          let (o, _x_i2) = o#patt _x_i2 in (o, (PaOrp (_x, _x_i1, _x_i2)))
      | PaRng (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#patt _x_i1 in
          let (o, _x_i2) = o#patt _x_i2 in (o, (PaRng (_x, _x_i1, _x_i2)))
      | PaRec (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#patt _x_i1 in (o, (PaRec (_x, _x_i1)))
      | PaEq (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ident _x_i1 in
          let (o, _x_i2) = o#patt _x_i2 in (o, (PaEq (_x, _x_i1, _x_i2)))
      | PaStr (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (PaStr (_x, _x_i1)))
      | PaTup (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#patt _x_i1 in (o, (PaTup (_x, _x_i1)))
      | PaTyc (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#patt _x_i1 in
          let (o, _x_i2) = o#ctyp _x_i2 in (o, (PaTyc (_x, _x_i1, _x_i2)))
      | PaTyp (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ident _x_i1 in (o, (PaTyp (_x, _x_i1)))
      | PaVrn (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (PaVrn (_x, _x_i1)))
      | PaLaz (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#patt _x_i1 in (o, (PaLaz (_x, _x_i1)))
      
    method override_flag : override_flag -> ('self_type * override_flag) =
      function
      | OvOverride -> (o, OvOverride)
      | OvNil -> (o, OvNil)
      | OvAnt _x -> let (o, _x) = o#string _x in (o, (OvAnt _x))
      
    method mutable_flag : mutable_flag -> ('self_type * mutable_flag) =
      function
      | MuMutable -> (o, MuMutable)
      | MuNil -> (o, MuNil)
      | MuAnt _x -> let (o, _x) = o#string _x in (o, (MuAnt _x))
      
    method module_type : module_type -> ('self_type * module_type) =
      function
      | MtNil _x -> let (o, _x) = o#loc _x in (o, (MtNil _x))
      | MtId (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ident _x_i1 in (o, (MtId (_x, _x_i1)))
      | MtFun (_x, _x_i1, _x_i2, _x_i3) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#module_type _x_i2 in
          let (o, _x_i3) = o#module_type _x_i3
          in (o, (MtFun (_x, _x_i1, _x_i2, _x_i3)))
      | MtQuo (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (MtQuo (_x, _x_i1)))
      | MtSig (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#sig_item _x_i1 in (o, (MtSig (_x, _x_i1)))
      | MtWit (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#module_type _x_i1 in
          let (o, _x_i2) = o#with_constr _x_i2
          in (o, (MtWit (_x, _x_i1, _x_i2)))
      | MtAnt (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (MtAnt (_x, _x_i1)))
      
    method module_expr : module_expr -> ('self_type * module_expr) =
      function
      | MeNil _x -> let (o, _x) = o#loc _x in (o, (MeNil _x))
      | MeId (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ident _x_i1 in (o, (MeId (_x, _x_i1)))
      | MeApp (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#module_expr _x_i1 in
          let (o, _x_i2) = o#module_expr _x_i2
          in (o, (MeApp (_x, _x_i1, _x_i2)))
      | MeFun (_x, _x_i1, _x_i2, _x_i3) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#module_type _x_i2 in
          let (o, _x_i3) = o#module_expr _x_i3
          in (o, (MeFun (_x, _x_i1, _x_i2, _x_i3)))
      | MeStr (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#str_item _x_i1 in (o, (MeStr (_x, _x_i1)))
      | MeTyc (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#module_expr _x_i1 in
          let (o, _x_i2) = o#module_type _x_i2
          in (o, (MeTyc (_x, _x_i1, _x_i2)))
      | MePkg (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#expr _x_i1 in (o, (MePkg (_x, _x_i1)))
      | MeAnt (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (MeAnt (_x, _x_i1)))
      
    method module_binding : module_binding -> ('self_type * module_binding) =
      function
      | MbNil _x -> let (o, _x) = o#loc _x in (o, (MbNil _x))
      | MbAnd (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#module_binding _x_i1 in
          let (o, _x_i2) = o#module_binding _x_i2
          in (o, (MbAnd (_x, _x_i1, _x_i2)))
      | MbColEq (_x, _x_i1, _x_i2, _x_i3) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#module_type _x_i2 in
          let (o, _x_i3) = o#module_expr _x_i3
          in (o, (MbColEq (_x, _x_i1, _x_i2, _x_i3)))
      | MbCol (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#module_type _x_i2
          in (o, (MbCol (_x, _x_i1, _x_i2)))
      | MbAnt (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (MbAnt (_x, _x_i1)))
      
    method meta_option :
      'a.
        ('self_type -> 'a -> ('self_type * 'a)) ->
          'a meta_option -> ('self_type * ('a meta_option)) =
      fun _f_a ->
        function
        | ONone -> (o, ONone)
        | OSome _x -> let (o, _x) = _f_a o _x in (o, (OSome _x))
        | OAnt _x -> let (o, _x) = o#string _x in (o, (OAnt _x))
      
    method meta_list :
      'a.
        ('self_type -> 'a -> ('self_type * 'a)) ->
          'a meta_list -> ('self_type * ('a meta_list)) =
      fun _f_a ->
        function
        | LNil -> (o, LNil)
        | LCons (_x, _x_i1) ->
            let (o, _x) = _f_a o _x in
            let (o, _x_i1) = o#meta_list _f_a _x_i1
            in (o, (LCons (_x, _x_i1)))
        | LAnt _x -> let (o, _x) = o#string _x in (o, (LAnt _x))
      
    method meta_bool : meta_bool -> ('self_type * meta_bool) =
      function
      | BTrue -> (o, BTrue)
      | BFalse -> (o, BFalse)
      | BAnt _x -> let (o, _x) = o#string _x in (o, (BAnt _x))
      
    method match_case : match_case -> ('self_type * match_case) =
      function
      | McNil _x -> let (o, _x) = o#loc _x in (o, (McNil _x))
      | McOr (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#match_case _x_i1 in
          let (o, _x_i2) = o#match_case _x_i2
          in (o, (McOr (_x, _x_i1, _x_i2)))
      | McArr (_x, _x_i1, _x_i2, _x_i3) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#patt _x_i1 in
          let (o, _x_i2) = o#expr _x_i2 in
          let (o, _x_i3) = o#expr _x_i3
          in (o, (McArr (_x, _x_i1, _x_i2, _x_i3)))
      | McAnt (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (McAnt (_x, _x_i1)))
      
    method option :
      'a.
        ('self_type -> 'a -> ('self_type * 'a)) ->
          'a option -> ('self_type * ('a option)) =
      fun _f_a ->
        function
        | None -> (o, None)
        | Some _x -> let (o, _x) = _f_a o _x in (o, (Some _x))

    method loc : loc -> ('self_type * loc) = o#unknown
      
    method ident : ident -> ('self_type * ident) =
      function
      | IdAcc (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ident _x_i1 in
          let (o, _x_i2) = o#ident _x_i2 in (o, (IdAcc (_x, _x_i1, _x_i2)))
      | IdApp (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ident _x_i1 in
          let (o, _x_i2) = o#ident _x_i2 in (o, (IdApp (_x, _x_i1, _x_i2)))
      | IdLid (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (IdLid (_x, _x_i1)))
      | IdUid (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (IdUid (_x, _x_i1)))
      | IdAnt (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (IdAnt (_x, _x_i1)))
      
    method expr : expr -> ('self_type * expr) =
      function
      | ExNil _x -> let (o, _x) = o#loc _x in (o, (ExNil _x))
      | ExId (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ident _x_i1 in (o, (ExId (_x, _x_i1)))
      | ExAcc (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#expr _x_i1 in
          let (o, _x_i2) = o#expr _x_i2 in (o, (ExAcc (_x, _x_i1, _x_i2)))
      | ExAnt (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (ExAnt (_x, _x_i1)))
      | ExApp (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#expr _x_i1 in
          let (o, _x_i2) = o#expr _x_i2 in (o, (ExApp (_x, _x_i1, _x_i2)))
      | ExAre (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#expr _x_i1 in
          let (o, _x_i2) = o#expr _x_i2 in (o, (ExAre (_x, _x_i1, _x_i2)))
      | ExArr (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#expr _x_i1 in (o, (ExArr (_x, _x_i1)))
      | ExSem (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#expr _x_i1 in
          let (o, _x_i2) = o#expr _x_i2 in (o, (ExSem (_x, _x_i1, _x_i2)))
      | ExAsf _x -> let (o, _x) = o#loc _x in (o, (ExAsf _x))
      | ExAsr (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#expr _x_i1 in (o, (ExAsr (_x, _x_i1)))
      | ExAss (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#expr _x_i1 in
          let (o, _x_i2) = o#expr _x_i2 in (o, (ExAss (_x, _x_i1, _x_i2)))
      | ExChr (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (ExChr (_x, _x_i1)))
      | ExCoe (_x, _x_i1, _x_i2, _x_i3) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#expr _x_i1 in
          let (o, _x_i2) = o#ctyp _x_i2 in
          let (o, _x_i3) = o#ctyp _x_i3
          in (o, (ExCoe (_x, _x_i1, _x_i2, _x_i3)))
      | ExFlo (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (ExFlo (_x, _x_i1)))
      | ExFor (_x, _x_i1, _x_i2, _x_i3, _x_i4, _x_i5) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#expr _x_i2 in
          let (o, _x_i3) = o#expr _x_i3 in
          let (o, _x_i4) = o#direction_flag _x_i4 in
          let (o, _x_i5) = o#expr _x_i5
          in (o, (ExFor (_x, _x_i1, _x_i2, _x_i3, _x_i4, _x_i5)))
      | ExFun (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#match_case _x_i1 in (o, (ExFun (_x, _x_i1)))
      | ExIfe (_x, _x_i1, _x_i2, _x_i3) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#expr _x_i1 in
          let (o, _x_i2) = o#expr _x_i2 in
          let (o, _x_i3) = o#expr _x_i3
          in (o, (ExIfe (_x, _x_i1, _x_i2, _x_i3)))
      | ExInt (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (ExInt (_x, _x_i1)))
      | ExInt32 (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (ExInt32 (_x, _x_i1)))
      | ExInt64 (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (ExInt64 (_x, _x_i1)))
      | ExNativeInt (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (ExNativeInt (_x, _x_i1)))
      | ExLab (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#expr _x_i2 in (o, (ExLab (_x, _x_i1, _x_i2)))
      | ExLaz (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#expr _x_i1 in (o, (ExLaz (_x, _x_i1)))
      | ExLet (_x, _x_i1, _x_i2, _x_i3) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#rec_flag _x_i1 in
          let (o, _x_i2) = o#binding _x_i2 in
          let (o, _x_i3) = o#expr _x_i3
          in (o, (ExLet (_x, _x_i1, _x_i2, _x_i3)))
      | ExLmd (_x, _x_i1, _x_i2, _x_i3) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#module_expr _x_i2 in
          let (o, _x_i3) = o#expr _x_i3
          in (o, (ExLmd (_x, _x_i1, _x_i2, _x_i3)))
      | ExMat (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#expr _x_i1 in
          let (o, _x_i2) = o#match_case _x_i2
          in (o, (ExMat (_x, _x_i1, _x_i2)))
      | ExNew (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ident _x_i1 in (o, (ExNew (_x, _x_i1)))
      | ExObj (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#patt _x_i1 in
          let (o, _x_i2) = o#class_str_item _x_i2
          in (o, (ExObj (_x, _x_i1, _x_i2)))
      | ExOlb (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#expr _x_i2 in (o, (ExOlb (_x, _x_i1, _x_i2)))
      | ExOvr (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#rec_binding _x_i1 in (o, (ExOvr (_x, _x_i1)))
      | ExRec (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#rec_binding _x_i1 in
          let (o, _x_i2) = o#expr _x_i2 in (o, (ExRec (_x, _x_i1, _x_i2)))
      | ExSeq (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#expr _x_i1 in (o, (ExSeq (_x, _x_i1)))
      | ExSnd (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#expr _x_i1 in
          let (o, _x_i2) = o#string _x_i2 in (o, (ExSnd (_x, _x_i1, _x_i2)))
      | ExSte (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#expr _x_i1 in
          let (o, _x_i2) = o#expr _x_i2 in (o, (ExSte (_x, _x_i1, _x_i2)))
      | ExStr (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (ExStr (_x, _x_i1)))
      | ExTry (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#expr _x_i1 in
          let (o, _x_i2) = o#match_case _x_i2
          in (o, (ExTry (_x, _x_i1, _x_i2)))
      | ExTup (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#expr _x_i1 in (o, (ExTup (_x, _x_i1)))
      | ExCom (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#expr _x_i1 in
          let (o, _x_i2) = o#expr _x_i2 in (o, (ExCom (_x, _x_i1, _x_i2)))
      | ExTyc (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#expr _x_i1 in
          let (o, _x_i2) = o#ctyp _x_i2 in (o, (ExTyc (_x, _x_i1, _x_i2)))
      | ExVrn (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (ExVrn (_x, _x_i1)))
      | ExWhi (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#expr _x_i1 in
          let (o, _x_i2) = o#expr _x_i2 in (o, (ExWhi (_x, _x_i1, _x_i2)))
      | ExOpI (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ident _x_i1 in
          let (o, _x_i2) = o#expr _x_i2 in (o, (ExOpI (_x, _x_i1, _x_i2)))
      | ExFUN (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#expr _x_i2 in (o, (ExFUN (_x, _x_i1, _x_i2)))
      | ExPkg (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#module_expr _x_i1 in (o, (ExPkg (_x, _x_i1)))
      
    method direction_flag : direction_flag -> ('self_type * direction_flag) =
      function
      | DiTo -> (o, DiTo)
      | DiDownto -> (o, DiDownto)
      | DiAnt _x -> let (o, _x) = o#string _x in (o, (DiAnt _x))
      
    method ctyp : ctyp -> ('self_type * ctyp) =
      function
      | TyNil _x -> let (o, _x) = o#loc _x in (o, (TyNil _x))
      | TyAli (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in
          let (o, _x_i2) = o#ctyp _x_i2 in (o, (TyAli (_x, _x_i1, _x_i2)))
      | TyAny _x -> let (o, _x) = o#loc _x in (o, (TyAny _x))
      | TyApp (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in
          let (o, _x_i2) = o#ctyp _x_i2 in (o, (TyApp (_x, _x_i1, _x_i2)))
      | TyArr (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in
          let (o, _x_i2) = o#ctyp _x_i2 in (o, (TyArr (_x, _x_i1, _x_i2)))
      | TyCls (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ident _x_i1 in (o, (TyCls (_x, _x_i1)))
      | TyLab (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#ctyp _x_i2 in (o, (TyLab (_x, _x_i1, _x_i2)))
      | TyId (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ident _x_i1 in (o, (TyId (_x, _x_i1)))
      | TyMan (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in
          let (o, _x_i2) = o#ctyp _x_i2 in (o, (TyMan (_x, _x_i1, _x_i2)))
      | TyDcl (_x, _x_i1, _x_i2, _x_i3, _x_i4) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#list (fun o -> o#ctyp) _x_i2 in
          let (o, _x_i3) = o#ctyp _x_i3 in
          let (o, _x_i4) =
            o#list
              (fun o (_x, _x_i1) ->
                 let (o, _x) = o#ctyp _x in
                 let (o, _x_i1) = o#ctyp _x_i1 in (o, (_x, _x_i1)))
              _x_i4
          in (o, (TyDcl (_x, _x_i1, _x_i2, _x_i3, _x_i4)))
      | TyObj (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in
          let (o, _x_i2) = o#row_var_flag _x_i2
          in (o, (TyObj (_x, _x_i1, _x_i2)))
      | TyOlb (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#ctyp _x_i2 in (o, (TyOlb (_x, _x_i1, _x_i2)))
      | TyPol (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in
          let (o, _x_i2) = o#ctyp _x_i2 in (o, (TyPol (_x, _x_i1, _x_i2)))
      | TyQuo (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (TyQuo (_x, _x_i1)))
      | TyQuP (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (TyQuP (_x, _x_i1)))
      | TyQuM (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (TyQuM (_x, _x_i1)))
      | TyVrn (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (TyVrn (_x, _x_i1)))
      | TyRec (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in (o, (TyRec (_x, _x_i1)))
      | TyCol (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in
          let (o, _x_i2) = o#ctyp _x_i2 in (o, (TyCol (_x, _x_i1, _x_i2)))
      | TySem (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in
          let (o, _x_i2) = o#ctyp _x_i2 in (o, (TySem (_x, _x_i1, _x_i2)))
      | TyCom (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in
          let (o, _x_i2) = o#ctyp _x_i2 in (o, (TyCom (_x, _x_i1, _x_i2)))
      | TySum (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in (o, (TySum (_x, _x_i1)))
      | TyOf (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in
          let (o, _x_i2) = o#ctyp _x_i2 in (o, (TyOf (_x, _x_i1, _x_i2)))
      | TyAnd (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in
          let (o, _x_i2) = o#ctyp _x_i2 in (o, (TyAnd (_x, _x_i1, _x_i2)))
      | TyOr (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in
          let (o, _x_i2) = o#ctyp _x_i2 in (o, (TyOr (_x, _x_i1, _x_i2)))
      | TyPrv (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in (o, (TyPrv (_x, _x_i1)))
      | TyMut (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in (o, (TyMut (_x, _x_i1)))
      | TyTup (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in (o, (TyTup (_x, _x_i1)))
      | TySta (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in
          let (o, _x_i2) = o#ctyp _x_i2 in (o, (TySta (_x, _x_i1, _x_i2)))
      | TyVrnEq (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in (o, (TyVrnEq (_x, _x_i1)))
      | TyVrnSup (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in (o, (TyVrnSup (_x, _x_i1)))
      | TyVrnInf (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in (o, (TyVrnInf (_x, _x_i1)))
      | TyVrnInfSup (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in
          let (o, _x_i2) = o#ctyp _x_i2
          in (o, (TyVrnInfSup (_x, _x_i1, _x_i2)))
      | TyAmp (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in
          let (o, _x_i2) = o#ctyp _x_i2 in (o, (TyAmp (_x, _x_i1, _x_i2)))
      | TyOfAmp (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in
          let (o, _x_i2) = o#ctyp _x_i2 in (o, (TyOfAmp (_x, _x_i1, _x_i2)))
      | TyPkg (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#module_type _x_i1 in (o, (TyPkg (_x, _x_i1)))
      | TyAnt (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (TyAnt (_x, _x_i1)))
      
    method class_type : class_type -> ('self_type * class_type) =
      function
      | CtNil _x -> let (o, _x) = o#loc _x in (o, (CtNil _x))
      | CtCon (_x, _x_i1, _x_i2, _x_i3) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#virtual_flag _x_i1 in
          let (o, _x_i2) = o#ident _x_i2 in
          let (o, _x_i3) = o#ctyp _x_i3
          in (o, (CtCon (_x, _x_i1, _x_i2, _x_i3)))
      | CtFun (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in
          let (o, _x_i2) = o#class_type _x_i2
          in (o, (CtFun (_x, _x_i1, _x_i2)))
      | CtSig (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in
          let (o, _x_i2) = o#class_sig_item _x_i2
          in (o, (CtSig (_x, _x_i1, _x_i2)))
      | CtAnd (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#class_type _x_i1 in
          let (o, _x_i2) = o#class_type _x_i2
          in (o, (CtAnd (_x, _x_i1, _x_i2)))
      | CtCol (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#class_type _x_i1 in
          let (o, _x_i2) = o#class_type _x_i2
          in (o, (CtCol (_x, _x_i1, _x_i2)))
      | CtEq (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#class_type _x_i1 in
          let (o, _x_i2) = o#class_type _x_i2
          in (o, (CtEq (_x, _x_i1, _x_i2)))
      | CtAnt (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (CtAnt (_x, _x_i1)))
      
    method class_str_item : class_str_item -> ('self_type * class_str_item) =
      function
      | CrNil _x -> let (o, _x) = o#loc _x in (o, (CrNil _x))
      | CrSem (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#class_str_item _x_i1 in
          let (o, _x_i2) = o#class_str_item _x_i2
          in (o, (CrSem (_x, _x_i1, _x_i2)))
      | CrCtr (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in
          let (o, _x_i2) = o#ctyp _x_i2 in (o, (CrCtr (_x, _x_i1, _x_i2)))
      | CrInh (_x, _x_i1, _x_i2, _x_i3) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#override_flag _x_i1 in
          let (o, _x_i2) = o#class_expr _x_i2 in
          let (o, _x_i3) = o#string _x_i3
          in (o, (CrInh (_x, _x_i1, _x_i2, _x_i3)))
      | CrIni (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#expr _x_i1 in (o, (CrIni (_x, _x_i1)))
      | CrMth (_x, _x_i1, _x_i2, _x_i3, _x_i4, _x_i5) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#override_flag _x_i2 in
          let (o, _x_i3) = o#private_flag _x_i3 in
          let (o, _x_i4) = o#expr _x_i4 in
          let (o, _x_i5) = o#ctyp _x_i5
          in (o, (CrMth (_x, _x_i1, _x_i2, _x_i3, _x_i4, _x_i5)))
      | CrVal (_x, _x_i1, _x_i2, _x_i3, _x_i4) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#override_flag _x_i2 in
          let (o, _x_i3) = o#mutable_flag _x_i3 in
          let (o, _x_i4) = o#expr _x_i4
          in (o, (CrVal (_x, _x_i1, _x_i2, _x_i3, _x_i4)))
      | CrVir (_x, _x_i1, _x_i2, _x_i3) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#private_flag _x_i2 in
          let (o, _x_i3) = o#ctyp _x_i3
          in (o, (CrVir (_x, _x_i1, _x_i2, _x_i3)))
      | CrVvr (_x, _x_i1, _x_i2, _x_i3) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#mutable_flag _x_i2 in
          let (o, _x_i3) = o#ctyp _x_i3
          in (o, (CrVvr (_x, _x_i1, _x_i2, _x_i3)))
      | CrAnt (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (CrAnt (_x, _x_i1)))
      
    method class_sig_item : class_sig_item -> ('self_type * class_sig_item) =
      function
      | CgNil _x -> let (o, _x) = o#loc _x in (o, (CgNil _x))
      | CgCtr (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#ctyp _x_i1 in
          let (o, _x_i2) = o#ctyp _x_i2 in (o, (CgCtr (_x, _x_i1, _x_i2)))
      | CgSem (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#class_sig_item _x_i1 in
          let (o, _x_i2) = o#class_sig_item _x_i2
          in (o, (CgSem (_x, _x_i1, _x_i2)))
      | CgInh (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#class_type _x_i1 in (o, (CgInh (_x, _x_i1)))
      | CgMth (_x, _x_i1, _x_i2, _x_i3) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#private_flag _x_i2 in
          let (o, _x_i3) = o#ctyp _x_i3
          in (o, (CgMth (_x, _x_i1, _x_i2, _x_i3)))
      | CgVal (_x, _x_i1, _x_i2, _x_i3, _x_i4) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#mutable_flag _x_i2 in
          let (o, _x_i3) = o#virtual_flag _x_i3 in
          let (o, _x_i4) = o#ctyp _x_i4
          in (o, (CgVal (_x, _x_i1, _x_i2, _x_i3, _x_i4)))
      | CgVir (_x, _x_i1, _x_i2, _x_i3) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in
          let (o, _x_i2) = o#private_flag _x_i2 in
          let (o, _x_i3) = o#ctyp _x_i3
          in (o, (CgVir (_x, _x_i1, _x_i2, _x_i3)))
      | CgAnt (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (CgAnt (_x, _x_i1)))
      
    method class_expr : class_expr -> ('self_type * class_expr) =
      function
      | CeNil _x -> let (o, _x) = o#loc _x in (o, (CeNil _x))
      | CeApp (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#class_expr _x_i1 in
          let (o, _x_i2) = o#expr _x_i2 in (o, (CeApp (_x, _x_i1, _x_i2)))
      | CeCon (_x, _x_i1, _x_i2, _x_i3) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#virtual_flag _x_i1 in
          let (o, _x_i2) = o#ident _x_i2 in
          let (o, _x_i3) = o#ctyp _x_i3
          in (o, (CeCon (_x, _x_i1, _x_i2, _x_i3)))
      | CeFun (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#patt _x_i1 in
          let (o, _x_i2) = o#class_expr _x_i2
          in (o, (CeFun (_x, _x_i1, _x_i2)))
      | CeLet (_x, _x_i1, _x_i2, _x_i3) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#rec_flag _x_i1 in
          let (o, _x_i2) = o#binding _x_i2 in
          let (o, _x_i3) = o#class_expr _x_i3
          in (o, (CeLet (_x, _x_i1, _x_i2, _x_i3)))
      | CeStr (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#patt _x_i1 in
          let (o, _x_i2) = o#class_str_item _x_i2
          in (o, (CeStr (_x, _x_i1, _x_i2)))
      | CeTyc (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#class_expr _x_i1 in
          let (o, _x_i2) = o#class_type _x_i2
          in (o, (CeTyc (_x, _x_i1, _x_i2)))
      | CeAnd (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#class_expr _x_i1 in
          let (o, _x_i2) = o#class_expr _x_i2
          in (o, (CeAnd (_x, _x_i1, _x_i2)))
      | CeEq (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#class_expr _x_i1 in
          let (o, _x_i2) = o#class_expr _x_i2
          in (o, (CeEq (_x, _x_i1, _x_i2)))
      | CeAnt (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (CeAnt (_x, _x_i1)))
      
    method binding : binding -> ('self_type * binding) =
      function
      | BiNil _x -> let (o, _x) = o#loc _x in (o, (BiNil _x))
      | BiAnd (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#binding _x_i1 in
          let (o, _x_i2) = o#binding _x_i2 in (o, (BiAnd (_x, _x_i1, _x_i2)))
      | BiEq (_x, _x_i1, _x_i2) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#patt _x_i1 in
          let (o, _x_i2) = o#expr _x_i2 in (o, (BiEq (_x, _x_i1, _x_i2)))
      | BiAnt (_x, _x_i1) ->
          let (o, _x) = o#loc _x in
          let (o, _x_i1) = o#string _x_i1 in (o, (BiAnt (_x, _x_i1)))
      
    method unknown : 'a. 'a -> ('self_type * 'a) = fun x -> (o, x)
      
  end
  

