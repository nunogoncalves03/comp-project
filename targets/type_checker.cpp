#include <string>
#include "targets/type_checker.h"
#include ".auto/all_nodes.h"  // automatically generated
#include <cdk/types/primitive_type.h>

#include "til_parser.tab.h"

#define ASSERT_UNSPEC { if (node->type() != nullptr && !node->is_typed(cdk::TYPE_UNSPEC)) return; }

//---------------------------------------------------------------------------

bool til::type_checker::types_deep_match(std::shared_ptr<cdk::basic_type> left,
      std::shared_ptr<cdk::basic_type> right, bool allow_covariance) {
  if (left->name() == cdk::TYPE_UNSPEC || right->name() == cdk::TYPE_UNSPEC) {
    return false;
  } else if (left->name() == cdk::TYPE_FUNCTIONAL) {
    if (right->name() != cdk::TYPE_FUNCTIONAL) {
      return false;
    }

    auto left_func = cdk::functional_type::cast(left);
    auto right_func = cdk::functional_type::cast(right);

    if (left_func->input_length() != right_func->input_length()
          || left_func->output_length() != right_func->output_length()) {
      return false;
    }

    // Example:
    //    ((int (double)) g1)
    //    ((int (int)) f2)
    //    (set f2 g1) ; ok: covariant types
    for (size_t i = 0; i < left_func->input_length(); i++) {
      if (!types_deep_match(right_func->input(i), left_func->input(i), allow_covariance)) {
        return false;
      }
    }

    // til only supports single return values
    if (!types_deep_match(left_func->output(0), right_func->output(0), allow_covariance)) {
      return false;
    }

    return true;
  } else if (left->name() == cdk::TYPE_POINTER) {
    if (right->name() != cdk::TYPE_POINTER) {
      return false;
    }

    return types_deep_match(cdk::reference_type::cast(left)->referenced(),
        cdk::reference_type::cast(right)->referenced(), false);
  } else if (allow_covariance && left->name() == cdk::TYPE_DOUBLE) {
    return right->name() == cdk::TYPE_DOUBLE || right->name() == cdk::TYPE_INT;
  } else {
    return left == right;
  }
}

bool til::type_checker::should_cast_pointer(std::shared_ptr<cdk::basic_type> left,
      std::shared_ptr<cdk::basic_type> right) {
  auto left_ref = cdk::reference_type::cast(left);
  auto right_ref = cdk::reference_type::cast(right);
  return right_ref->referenced()->name() == cdk::TYPE_UNSPEC
            || right_ref->referenced()->name() == cdk::TYPE_VOID
            || left_ref->referenced()->name() == cdk::TYPE_VOID;
}

//---------------------------------------------------------------------------

void til::type_checker::do_sequence_node(cdk::sequence_node *const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl);
  }
}

//---------------------------------------------------------------------------

void til::type_checker::do_nil_node(cdk::nil_node *const node, int lvl) {
  // EMPTY
}
void til::type_checker::do_data_node(cdk::data_node *const node, int lvl) {
  // EMPTY
}

void til::type_checker::do_block_node(til::block_node *const node, int lvl) {
  // EMPTY
}

void til::type_checker::do_stop_node(til::stop_node *const node, int lvl) {
  // EMPTY
}

void til::type_checker::do_next_node(til::next_node *const node, int lvl) {
  // EMPTY
}

//---------------------------------------------------------------------------

void til::type_checker::do_integer_node(cdk::integer_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::do_double_node(cdk::double_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
}

void til::type_checker::do_string_node(cdk::string_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
}

//---------------------------------------------------------------------------

void til::type_checker::process_unary_expression(cdk::unary_operation_node *const node, int lvl, bool acceptDouble) {
  ASSERT_UNSPEC;

  node->argument()->accept(this, lvl + 2);

  if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
    node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if (!node->argument()->is_typed(cdk::TYPE_INT)
        && !(acceptDouble && node->argument()->is_typed(cdk::TYPE_DOUBLE))) {
    throw std::string("wrong type in argument of unary expression");
  }

  node->type(node->argument()->type());
}

void til::type_checker::do_unary_minus_node(cdk::unary_minus_node *const node, int lvl) {
  process_unary_expression(node, lvl, true);
}

void til::type_checker::do_unary_plus_node(cdk::unary_plus_node *const node, int lvl) {
  process_unary_expression(node, lvl, true);
}

void til::type_checker::do_not_node(cdk::not_node *const node, int lvl) {
  process_unary_expression(node, lvl, false);
}

void til::type_checker::do_allocation_node(til::allocation_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->argument()->accept(this, lvl + 2);

  if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
    node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if (!node->argument()->is_typed(cdk::TYPE_INT)) {
    throw std::string("wrong type in argument of allocation expression");
  }

  node->type(cdk::reference_type::create(4, cdk::primitive_type::create(0, cdk::TYPE_UNSPEC)));
}

void til::type_checker::do_address_of_node(til::address_of_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->argument()->accept(this, lvl + 2);

  if (node->argument()->is_typed(cdk::TYPE_POINTER)) {
    auto ref = cdk::reference_type::cast(node->argument()->type());
    if (ref->referenced()->name() == cdk::TYPE_VOID) {
      // don't create a pointer to void!, just set it to void! since they represent the same thing
      node->type(node->argument()->type());
      return;
    }
  }

  node->type(cdk::reference_type::create(4, node->argument()->type()));
}

//---------------------------------------------------------------------------

void til::type_checker::process_binary_arithmetic_expression(
  cdk::binary_operation_node *const node,
  int lvl,
  bool acceptDoubles,
  bool acceptOnePointer,
  bool acceptTwoPointers
) {
  ASSERT_UNSPEC;

  node->left()->accept(this, lvl + 2);
  
  if (node->left()->is_typed(cdk::TYPE_INT) || node->left()->is_typed(cdk::TYPE_UNSPEC))  {
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_INT) || (acceptDoubles && node->right()->is_typed(cdk::TYPE_DOUBLE))) {
      node->type(node->right()->type());
    } else if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if (acceptOnePointer && node->right()->is_typed(cdk::TYPE_POINTER)) {
      node->type(node->right()->type());

      if (node->left()->is_typed(cdk::TYPE_UNSPEC)) {
        node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      }
    } else {
      throw std::string("incompatible right argument type in arithmetic binary expression");
    }

    if (node->left()->is_typed(cdk::TYPE_UNSPEC)) {
      node->left()->type(node->type());
    }
  } else if (acceptDoubles && node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_INT)) {
      node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    } else if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
      node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    } else {
      throw std::string("right node type incompatible with double in arithmetic binary expression");
    }
  } else if (acceptOnePointer && node->left()->is_typed(cdk::TYPE_POINTER)) {
    auto left_ref_type = cdk::reference_type::cast(node->left()->type());
    if (left_ref_type->referenced()->name() == cdk::TYPE_FUNCTIONAL) {
      throw std::string("cannot perform arithmetic operations on function pointers");
    }

    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_POINTER)) {
      auto right_ref = cdk::reference_type::cast(node->right()->type());
      if (right_ref->referenced()->name() == cdk::TYPE_FUNCTIONAL) {
        throw std::string("cannot perform arithmetic operations on function pointers");
      }
    }

    if (node->right()->is_typed(cdk::TYPE_INT)) {
      node->type(node->left()->type());
    } else if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      node->type(node->left()->type());
    } else if (acceptTwoPointers && types_deep_match(node->left()->type(), node->right()->type(), false)) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else {
      throw std::string("right node type incompatible with pointer in arithmetic binary expression");
    }
  } else {
    throw std::string("incompatible left argument type in arithmetic binary expression");
  }
}

void til::type_checker::do_add_node(cdk::add_node *const node, int lvl) {
  process_binary_arithmetic_expression(node, lvl, true, true, false);
}
void til::type_checker::do_sub_node(cdk::sub_node *const node, int lvl) {
  process_binary_arithmetic_expression(node, lvl, true, true, true);
}
void til::type_checker::do_mul_node(cdk::mul_node *const node, int lvl) {
  process_binary_arithmetic_expression(node, lvl, true, false, false);
}
void til::type_checker::do_div_node(cdk::div_node *const node, int lvl) {
  process_binary_arithmetic_expression(node, lvl, true, false, false);
}
void til::type_checker::do_mod_node(cdk::mod_node *const node, int lvl) {
  process_binary_arithmetic_expression(node, lvl, false, false, false);
}

void til::type_checker::process_binary_predicate_expression(
  cdk::binary_operation_node *const node,
  int lvl,
  bool acceptDoubles,
  bool acceptPointers
) {
  ASSERT_UNSPEC;

  node->left()->accept(this, lvl + 2);

  if (node->left()->is_typed(cdk::TYPE_INT)) {
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(node->left()->type());
    } else if (!node->right()->is_typed(cdk::TYPE_INT)
          && !(acceptDoubles && node->right()->is_typed(cdk::TYPE_DOUBLE))
          && !(acceptPointers && node->right()->is_typed(cdk::TYPE_POINTER))) {
      throw std::string("right node type incompatible with integer in predicate binary expression");
    }
  } else if (acceptDoubles && node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(node->left()->type());
    } else if (!node->right()->is_typed(cdk::TYPE_INT) && !node->right()->is_typed(cdk::TYPE_DOUBLE)) {
      throw std::string("right node type incompatible with double in predicate binary expression");
    }
  } else if (acceptPointers && node->left()->is_typed(cdk::TYPE_POINTER)) {
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if (!node->right()->is_typed(cdk::TYPE_INT) && !node->right()->is_typed(cdk::TYPE_POINTER)) {
      throw std::string("right node type incompatible with pointer in predicate binary expression");
    }
  } else if (node->left()->is_typed(cdk::TYPE_UNSPEC)) {
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if (node->right()->is_typed(cdk::TYPE_POINTER)) {
      node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if (node->right()->is_typed(cdk::TYPE_INT) || (acceptDoubles && node->right()->is_typed(cdk::TYPE_DOUBLE))) {
      node->left()->type(node->right()->type());
    } else {
      throw std::string("incompatible right argument type in predicate binary expression");
    }
  } else {
    throw std::string("incompatible left argument type in predicate binary expression");
  }

  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::do_lt_node(cdk::lt_node *const node, int lvl) {
  process_binary_predicate_expression(node, lvl, true, false);
}
void til::type_checker::do_le_node(cdk::le_node *const node, int lvl) {
  process_binary_predicate_expression(node, lvl, true, false);
}
void til::type_checker::do_ge_node(cdk::ge_node *const node, int lvl) {
  process_binary_predicate_expression(node, lvl, true, false);
}
void til::type_checker::do_gt_node(cdk::gt_node *const node, int lvl) {
  process_binary_predicate_expression(node, lvl, true, false);
}
void til::type_checker::do_ne_node(cdk::ne_node *const node, int lvl) {
  process_binary_predicate_expression(node, lvl, true, true);
}
void til::type_checker::do_eq_node(cdk::eq_node *const node, int lvl) {
  process_binary_predicate_expression(node, lvl, true, true);
}
void til::type_checker::do_and_node(cdk::and_node *const node, int lvl) {
  process_binary_predicate_expression(node, lvl, false, false);
}
void til::type_checker::do_or_node(cdk::or_node *const node, int lvl) {
  process_binary_predicate_expression(node, lvl, false, false);
}

//---------------------------------------------------------------------------

void til::type_checker::do_variable_node(cdk::variable_node *const node, int lvl) {
  ASSERT_UNSPEC;

  const std::string &id = node->name();
  std::shared_ptr<til::symbol> symbol = _symtab.find(id);

  if (symbol != nullptr) {
    node->type(symbol->type());
  } else {
    throw std::string("undeclared variable '" + id + "'");
  }
}

void til::type_checker::do_rvalue_node(cdk::rvalue_node *const node, int lvl) {
  ASSERT_UNSPEC;
  
  node->lvalue()->accept(this, lvl);
  node->type(node->lvalue()->type());
}

void til::type_checker::do_assignment_node(cdk::assignment_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->lvalue()->accept(this, lvl);
  node->rvalue()->accept(this, lvl);

  if (node->rvalue()->is_typed(cdk::TYPE_UNSPEC)) {
    node->rvalue()->type(node->lvalue()->type());
  } else if (node->lvalue()->is_typed(cdk::TYPE_POINTER) && node->rvalue()->is_typed(cdk::TYPE_POINTER)) {
    auto left_ref = cdk::reference_type::cast(node->lvalue()->type());
    auto right_ref = cdk::reference_type::cast(node->rvalue()->type());

    if (should_cast_pointer(left_ref, right_ref)) {
      node->rvalue()->type(node->lvalue()->type());
    }
  }

  if (!types_deep_match(node->lvalue()->type(), node->rvalue()->type(), true)) {
    throw std::string("incompatible type in right argument of assignment expression");
  }

  node->type(node->lvalue()->type());
}

void til::type_checker::do_evaluation_node(til::evaluation_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);

  if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
    node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if (node->argument()->is_typed(cdk::TYPE_POINTER)) {
    auto ref = cdk::reference_type::cast(node->argument()->type());

    if (ref->referenced()->name() == cdk::TYPE_UNSPEC) {
      node->argument()->type(cdk::reference_type::create(4, cdk::primitive_type::create(4, cdk::TYPE_INT)));
    }
  }
}

//---------------------------------------------------------------------------

void til::type_checker::do_function_node(til::function_node *const node, int lvl) {
  auto symbol = std::make_shared<til::symbol>("@", node->type());
  symbol->is_program(node->is_program());

  if (!_symtab.insert(symbol->name(), symbol)) {
    // symbol already exists in local context, replace with new one
    _symtab.replace(symbol->name(), symbol);
  }
}

void til::type_checker::do_return_node(til::return_node *const node, int lvl) {
  // We are in the block context, so we need to search in the function context itself
  auto symbol = _symtab.find("@", 1);

  if (symbol == nullptr) {
    throw std::string("return statement defined outside of a function scope");
  }

  std::shared_ptr<cdk::functional_type> func_type = cdk::functional_type::cast(symbol->type());
  // til only supports single return values
  auto ret_type = func_type->output(0);

  if (node->return_value() == nullptr) {
    if (ret_type->name() != cdk::TYPE_VOID) {
      throw std::string("no return value provided to a non-void return function");
    }
    return;
  }

  if (ret_type->name() == cdk::TYPE_VOID) {
    throw std::string("provided a return value to a void return function");
  }

  node->return_value()->accept(this, lvl + 2);

  if (!types_deep_match(ret_type, node->return_value()->type(), true)) {
    throw std::string("incompatible return type");
  }
}

//---------------------------------------------------------------------------

void til::type_checker::do_print_node(til::print_node *const node, int lvl) {
  for (size_t i = 0; i < node->arguments()->size(); i++) {
    auto child = dynamic_cast<cdk::expression_node*>(node->arguments()->node(i));

    child->accept(this, lvl + 4);

    if (child->is_typed(cdk::TYPE_UNSPEC)) {
      child->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if (!child->is_typed(cdk::TYPE_INT) && !child->is_typed(cdk::TYPE_DOUBLE)
              && !child->is_typed(cdk::TYPE_STRING)) {
      throw std::string("incompatible type for argument " + std::to_string(i + 1) + " of print instruction");
    }
  }
}

void til::type_checker::do_read_node(til::read_node *const node, int lvl) {
  ASSERT_UNSPEC; // Don't override infered type by parent

  node->type(cdk::primitive_type::create(0, cdk::TYPE_UNSPEC));
}

//---------------------------------------------------------------------------

void til::type_checker::do_loop_node(til::loop_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);

  if (node->condition()->is_typed(cdk::TYPE_UNSPEC)) {
    node->condition()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if (!node->condition()->is_typed(cdk::TYPE_INT)) {
    throw std::string("incompatible type in condition of loop instruction");
  }
}

//---------------------------------------------------------------------------

void til::type_checker::do_if_node(til::if_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);

  if (node->condition()->is_typed(cdk::TYPE_UNSPEC)) {
    node->condition()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if (!node->condition()->is_typed(cdk::TYPE_INT)) {
    throw std::string("incompatible type in condition of if statement");
  }
}

void til::type_checker::do_if_else_node(til::if_else_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);

  if (node->condition()->is_typed(cdk::TYPE_UNSPEC)) {
    node->condition()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if (!node->condition()->is_typed(cdk::TYPE_INT)) {
    throw std::string("incompatible type in condition of if statement");
  }
}

//---------------------------------------------------------------------------

void til::type_checker::do_declaration_node(til::declaration_node *const node, int lvl) {
  if (node->type() != nullptr) { // node has a type
    if (node->initializer() != nullptr) {
      node->initializer()->accept(this, lvl + 2);

      if (node->initializer()->is_typed(cdk::TYPE_UNSPEC)) {
        if (node->is_typed(cdk::TYPE_DOUBLE)) {
          node->initializer()->type(node->type());
        } else {
          node->initializer()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
        }
      } else if (node->is_typed(cdk::TYPE_POINTER) && node->initializer()->is_typed(cdk::TYPE_POINTER)) {
        auto node_ref = cdk::reference_type::cast(node->type());
        auto initializer_ref = cdk::reference_type::cast(node->initializer()->type());
        if (should_cast_pointer(node_ref, initializer_ref)) {
          node->initializer()->type(node->type());
        }
      }

      if (!types_deep_match(node->type(), node->initializer()->type(), true)) {
        throw std::string("incompatible type in initializer for variable '" + node->identifier() + "'");
      }
    }
  } else { // "var" case
    node->initializer()->accept(this, lvl + 2);

    if (node->initializer()->is_typed(cdk::TYPE_UNSPEC)) {
      node->initializer()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if (node->initializer()->is_typed(cdk::TYPE_POINTER)) {
      auto ref = cdk::reference_type::cast(node->initializer()->type());
      if (ref->referenced()->name() == cdk::TYPE_UNSPEC) {
        node->initializer()->type(cdk::reference_type::create(4,
            cdk::primitive_type::create(4, cdk::TYPE_INT)));
      }
    } else if (node->initializer()->is_typed(cdk::TYPE_VOID)) {
      throw std::string("variables can't be of type void");
    }

    node->type(node->initializer()->type());
  }

  if (node->qualifier() == tEXTERNAL && !node->is_typed(cdk::TYPE_FUNCTIONAL)) {
    throw std::string("external declaration of non-function '" + node->identifier() + "'");
  }

  auto symbol = std::make_shared<til::symbol>(node->identifier(), node->type());
  symbol->qualifier(node->qualifier());

  if (!_symtab.insert(node->identifier(), symbol)) {
    // Unable to insert, which means the symbol already exists.
    // The only way this re-declaration is valid is if we had previously declared
    // a symbol with the forward keyword and are have now found the implementation
    // for that symbol, in which case we will replace it
    
    auto prev_declaration = _symtab.find(node->identifier());
    if (prev_declaration->qualifier() == tFORWARD &&
        types_deep_match(prev_declaration->type(), symbol->type(), false)) {
      _symtab.replace(node->identifier(), symbol);
      _parent->set_new_symbol(symbol);
      return;
    }

    throw std::string("redeclaration of variable '" + node->identifier() + "'");
  }

  _parent->set_new_symbol(symbol);
}

void til::type_checker::do_null_pointer_node(til::null_pointer_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->type(cdk::reference_type::create(4, cdk::primitive_type::create(0, cdk::TYPE_UNSPEC)));
}

void til::type_checker::do_sizeof_node(til::sizeof_node *const node, int lvl) {
  ASSERT_UNSPEC;
  
  node->argument()->accept(this, lvl + 2);

  if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
    node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  }

  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void til::type_checker::do_pointer_index_node(til::pointer_index_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->base()->accept(this, lvl + 2);
  if (!node->base()->is_typed(cdk::TYPE_POINTER)) {
    throw std::string("incompatible type in pointer index's base");
  }

  node->index()->accept(this, lvl + 2);
  if (node->index()->is_typed(cdk::TYPE_UNSPEC)) {
    node->index()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if (!node->index()->is_typed(cdk::TYPE_INT)) {
    throw std::string("incompatible type in pointer index's index");
  }

  auto base_type = cdk::reference_type::cast(node->base()->type());

  if (base_type->referenced()->name() == cdk::TYPE_UNSPEC) {
    base_type = cdk::reference_type::create(4, cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->base()->type(base_type);
  }

  node->type(base_type->referenced());
}

void til::type_checker::do_function_call_node(til::function_call_node *const node, int lvl) {
  ASSERT_UNSPEC;

  std::shared_ptr<cdk::functional_type> func_type;

  if (node->function() == nullptr) { // recursive call ("@")
    auto symbol = _symtab.find("@", 1);
    if (symbol == nullptr) {
      throw std::string("recursive call outside function");
    } else if (symbol->is_program()) {
      throw std::string("recursive call inside program declaration");
    }

    func_type = cdk::functional_type::cast(symbol->type());
  } else {
    node->function()->accept(this, lvl);

    if (!node->function()->is_typed(cdk::TYPE_FUNCTIONAL)) {
      throw std::string("incompatible type in function call");
    }

    func_type = cdk::functional_type::cast(node->function()->type());
  }

  if (func_type->input()->length() != node->arguments()->size()) {
    throw std::string("wrong number of arguments in function call");
  }

  for (size_t i = 0; i < node->arguments()->size(); i++) {
    auto arg = dynamic_cast<cdk::expression_node*>(node->arguments()->node(i));
    arg->accept(this, lvl);

    auto paramtype = func_type->input(i);

    if (arg->is_typed(cdk::TYPE_UNSPEC)) {
      if (paramtype->name() == cdk::TYPE_DOUBLE) {
        arg->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
      } else {
        arg->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      }
    } else if (arg->is_typed(cdk::TYPE_POINTER) && paramtype->name() == cdk::TYPE_POINTER) {
      auto paramref = cdk::reference_type::cast(paramtype);
      auto argref = cdk::reference_type::cast(arg->type());

      if (should_cast_pointer(paramref, argref)) {
        arg->type(paramtype);
      }
    }

    if (!types_deep_match(paramtype, arg->type(), true)) {
      throw std::string("incompatible type for argument " + std::to_string(i + 1) + " in function call");
    }
  }

  node->type(func_type->output(0));
}

void til::type_checker::do_with_node(til::with_node * const node, int lvl) {
  node->function()->accept(this, lvl);

  if (!node->function()->is_typed(cdk::TYPE_FUNCTIONAL)) {
    throw std::string("incompatible type in function call");
  }

  auto func_type = cdk::functional_type::cast(node->function()->type());

  if (func_type->input_length() != 1) {
    throw std::string("incompatible type in function call");
  }

  node->vector()->accept(this, lvl);

  if (!node->vector()->is_typed(cdk::TYPE_POINTER)) {
    throw std::string("vector should be a pointer");
  }

  auto ref_type = cdk::reference_type::cast(node->vector()->type());
  if (!types_deep_match(func_type->input(0), ref_type->referenced(), true)) {
    throw std::string("vector elements type differs from function argument");
  }

  node->low()->accept(this, lvl);
  if (node->low()->is_typed(cdk::TYPE_UNSPEC)) {
    node->low()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if (!node->low()->is_typed(cdk::TYPE_INT)) {
    throw std::string("low should be int");
  }

  node->high()->accept(this, lvl);
  if (node->high()->is_typed(cdk::TYPE_UNSPEC)) {
    node->high()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if (!node->high()->is_typed(cdk::TYPE_INT)) {
    throw std::string("high should be int");
  }
}
