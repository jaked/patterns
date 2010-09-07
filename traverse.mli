(* Generated automatically from the code
   
   class fold_map = Camlp4FoldMapGenerator.generated;;

   by invoking camlp4of 3.10.2 as follows:

   /usr/local/ocaml/bin/camlp4of -filter fold traverse.mli
 *)
open Camlp4.PreCast.Syntax.Ast

class fold_map :
object ('self)
  method list           : ('self -> 'b -> 'self * 'b) -> 'b list -> 'self * 'b list
  method option          : 'a . ('self -> 'a -> 'self * 'a) -> 'a option -> 'self * 'a option
  method string         : string -> 'self * string
  method unknown        : 'e -> 'self * 'e

  method binding        : binding -> 'self * binding
  method class_expr     : class_expr -> 'self * class_expr
  method class_sig_item : class_sig_item -> 'self * class_sig_item
  method class_str_item : class_str_item -> 'self * class_str_item
  method class_type     : class_type -> 'self * class_type
  method ctyp           : ctyp -> 'self * ctyp
  method expr           : expr -> 'self * expr
  method ident          : ident -> 'self * ident
  method loc            : loc -> 'self * loc
  method match_case     : match_case -> 'self * match_case
  method meta_bool      : meta_bool -> 'self * meta_bool
  method meta_list      : ('self -> 'c -> 'self * 'c) -> 'c meta_list -> 'self * 'c meta_list
  method meta_option    : ('self -> 'd -> 'self * 'd) -> 'd meta_option -> 'self * 'd meta_option
  method module_binding : module_binding -> 'self * module_binding
  method module_expr    : module_expr -> 'self * module_expr
  method module_type    : module_type -> 'self * module_type
  method patt           : patt -> 'self * patt
  method rec_binding    : rec_binding -> 'self * rec_binding
  method sig_item       : sig_item -> 'self * sig_item
  method str_item       : str_item -> 'self * str_item
  method with_constr    : with_constr -> 'self * with_constr

  method rec_flag       : rec_flag -> 'self * rec_flag
  method direction_flag : direction_flag -> 'self * direction_flag
  method mutable_flag   : mutable_flag -> 'self * mutable_flag
  method private_flag   : private_flag -> 'self * private_flag
  method virtual_flag   : virtual_flag -> 'self * virtual_flag
  method row_var_flag   : row_var_flag -> 'self * row_var_flag
  method override_flag  : override_flag -> 'self * override_flag
end

