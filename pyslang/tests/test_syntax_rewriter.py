import pytest
import pyslang
from pyslang import SyntaxTree, SyntaxKind


def test_rewriter_handler_function_called_with_right_args():
    tree = SyntaxTree.fromText("""
        module m;
            int i;
            logic l;
        endmodule
    """, "test.sv")

    handler_tracker = {"call_count": 0}

    def handler(*args, **kwargs):
        assert len(args) == 2
        assert isinstance(args[0], pyslang.SyntaxNode)
        assert isinstance(args[1], pyslang.SyntaxRewriter)
        assert len(kwargs) == 0

        handler_tracker["call_count"] += 1

    assert handler_tracker["call_count"] == 0
    result = pyslang.rewrite(tree, handler)
    assert result is not None
    assert isinstance(result, SyntaxTree)

    assert handler_tracker["call_count"] > 0, "Handler should have been called at least once"
    assert handler_tracker["call_count"] >= 4, "Handler should have been called at least 4 times"



def test_rewriter_with_no_changes():
    tree = SyntaxTree.fromText("""
        module m;
            int i;
            logic l;
        endmodule
    """, "test.sv")
    expected = SyntaxTree.fromText("""
        module m;
            int i;
            logic l;
        endmodule
    """, "test.sv")
    result = pyslang.rewrite(tree, lambda _node, _rewriter: None)
    assert result is not None
    assert result.root.isEquivalentTo(expected.root)

def test_rewriter_remove():
    print("STARTING TEST")
    tree = SyntaxTree.fromText("""
        module m;
            int i;
            logic l;
        endmodule
    """, "test.sv")
    expected = SyntaxTree.fromText("""
        module m;
            logic l;
        endmodule
    """, "test.sv")

    check_func_called = {'called': False}
    
    def remove_int_var(node: pyslang.SyntaxNode, rewriter: pyslang.SyntaxRewriter):
        assert isinstance(node, pyslang.SyntaxNode)
        assert isinstance(rewriter, pyslang.SyntaxRewriter)

        check_func_called['called'] = True

        if node.kind == SyntaxKind.DataDeclaration:
            if node[0].kind == SyntaxKind.DataType and node[0][0].rawText == "int":
                rewriter.remove(node)

    assert check_func_called['called'] is False

    result = pyslang.rewrite(tree, remove_int_var)
    assert check_func_called['called'] is True
    assert result is not None
    assert result.root.isEquivalentTo(expected.root)

def test_rewriter_replace():
    tree = SyntaxTree.fromText("""
        module m;
            int i;
            logic l;
        endmodule
    """, "test.sv")
    expected = SyntaxTree.fromText("""
        module m;
            logic j;
            logic l;
        endmodule
    """, "test.sv")
    
    def replace_int_var(node: pyslang.SyntaxNode, rewriter: pyslang.SyntaxRewriter):
        # Replace int i with logic j
        if node.kind == SyntaxKind.DataDeclaration:
            if node[0].kind == SyntaxKind.DataType and node[0][0].rawText == "int":
                # Create a new variable declaration
                factory = rewriter.factory
                data_type = factory.DataType(factory.Token(SyntaxKind.DataType, "logic"))
                name_token = factory.Token(SyntaxKind.Identifier, "j")
                var_decl = factory.VariableDeclarator(name_token)
                new_decl = factory.DataDeclaration(data_type, [var_decl])
                
                rewriter.replace(node, new_decl)
    
    result = pyslang.rewrite(tree, replace_int_var)
    assert result is not None
    assert result.root.isEquivalentTo(expected.root)

def test_rewriter_insert():
    tree = SyntaxTree.fromText("""
        module m;
            int i;
        endmodule
    """, "test.sv")
    expected = SyntaxTree.fromText("""
        module m;
            int i;
            logic j;
        endmodule
    """, "test.sv")
    
    def insert_logic_var(node: pyslang.SyntaxNode, rewriter: pyslang.SyntaxRewriter):
        # Insert logic j after int i
        if node.kind == SyntaxKind.DataDeclaration:
            if node[0].kind == SyntaxKind.DataType and node[0][0].rawText == "int":
                # Create a new variable declaration to insert
                factory = rewriter.factory
                data_type = factory.DataType(factory.Token(SyntaxKind.DataType, "logic"))
                name_token = factory.Token(SyntaxKind.Identifier, "j")
                var_decl = factory.VariableDeclarator(name_token)
                new_decl = factory.DataDeclaration(data_type, [var_decl])
                
                rewriter.insert_after(node, new_decl)
    
    result = pyslang.rewrite(tree, insert_logic_var)
    assert result is not None
    assert result.root.isEquivalentTo(expected.root)

def test_rewriter_nested():
    tree = SyntaxTree.fromText("""
        module m;
            struct {
                int i;
                logic l;
            } s;
        endmodule
    """, "test.sv")
    expected = SyntaxTree.fromText("""
        module m;
            logic j;
            struct {
                logic l;
            } s;
        endmodule
    """, "test.sv")
    
    def modify_struct(node: pyslang.SyntaxNode, rewriter: pyslang.SyntaxRewriter):
        # Multiple operations:
        # 1. Remove int i from the struct
        # 2. Add logic j before the struct
        
        # Handle removing int i from inside struct
        if node.kind == SyntaxKind.DataDeclaration:
            if node[0].kind == SyntaxKind.DataType and node[0][0].rawText == "int":
                rewriter.remove(node)
        
        # Handle adding logic j before struct
        if node.kind == SyntaxKind.StructUnionDeclaration:
            factory = rewriter.factory
            data_type = factory.DataType(factory.Token(SyntaxKind.DataType, "logic"))
            name_token = factory.Token(SyntaxKind.Identifier, "j")
            var_decl = factory.VariableDeclarator(name_token)
            new_decl = factory.DataDeclaration(data_type, [var_decl])
            
            rewriter.insert_before(node, new_decl)
    
    result = pyslang.rewrite(tree, modify_struct)
    assert result is not None
    assert result.root.isEquivalentTo(expected.root)

def test_rewriter_skip():
    tree = SyntaxTree.fromText("""
        module m;
            int i;
            logic l;
        endmodule
    """, "test.sv")
    expected = SyntaxTree.fromText("""
        module m;
            int i;
            logic l;
        endmodule
    """, "test.sv")
    
    def skip_module_body(node: pyslang.SyntaxNode, rewriter: pyslang.SyntaxRewriter):
        # Skip processing the module's body
        if node.kind == SyntaxKind.ModuleDeclaration:
            rewriter.remove(node)
    
    result = pyslang.rewrite(tree, skip_module_body)
    assert result is not None
    assert result.root.isEquivalentTo(expected.root)
