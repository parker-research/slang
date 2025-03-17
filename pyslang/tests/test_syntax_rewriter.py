import pytest
import pyslang
from pyslang import SyntaxTree, VisitAction, SyntaxKind
from typing import List

def test_rewriter_with_no_changes():
    """Ensure that the rewriter does not change the tree if no changes are requested."""
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

    def visitor(node):
        return VisitAction.Advance

    result = pyslang.rewrite(tree, visitor)
    assert result is not None
    assert result.root.isEquivalentTo(expected.root)

def test_rewriter_remove():
    # Test removing nodes
    tree = SyntaxTree.fromText("""
        module m;
            int i;
            logic l;
        endmodule
    """, "test.sv")

    def visitor(node):
        if node.kind == SyntaxKind.VariableDeclaration:
            if node.children[1].token.kind == "IntKeyword":
                return pyslang.rewriter.remove(node)
        return VisitAction.Advance

    expected = SyntaxTree.fromText("""
        module m;
            logic l;
        endmodule
    """, "test.sv")

    result = pyslang.rewrite(tree, visitor)
    assert result is not None
    assert result.root.isEquivalentTo(expected.root)

def test_rewriter_replace():
    # Test replacing nodes
    tree = SyntaxTree.fromText("""
        module m;
            int i;
            logic l;
        endmodule
    """, "test.sv")

    def visitor(node):
        if node.kind == SyntaxKind.VariableDeclaration:
            if node.children[1].token.kind == "IntKeyword":
                with node.rewriter:
                    new_node = node.rewriter.factory.createVariableDeclaration(
                        "logic", "j"
                    )
                    node.rewriter.replace(node, new_node)
        return VisitAction.Advance

    expected = SyntaxTree.fromText("""
        module m;
            logic j;
            logic l;
        endmodule
    """, "test.sv")

    result = pyslang.rewrite(tree, visitor)
    assert result is not None
    assert result.root.isEquivalentTo(expected.root)

def test_rewriter_insert():
    # Test inserting nodes
    tree = SyntaxTree.fromText("""
        module m;
            int i;
        endmodule
    """, "test.sv")

    def visitor(node):
        if node.kind == SyntaxKind.VariableDeclaration:
            if node.children[1].token.kind == "IntKeyword":
                with node.rewriter:
                    new_node = node.rewriter.factory.createVariableDeclaration(
                        "logic", "j"
                    )
                    node.rewriter.insertAfter(node, new_node)
        return VisitAction.Advance

    expected = SyntaxTree.fromText("""
        module m;
            int i;
            logic j;
        endmodule
    """, "test.sv")

    result = pyslang.rewrite(tree, visitor)
    assert result is not None
    assert result.root.isEquivalentTo(expected.root)

def test_rewriter_nested():
    # Test nested modifications
    tree = SyntaxTree.fromText("""
        module m;
            struct {
                int i;
                logic l;
            } s;
        endmodule
    """, "test.sv")

    def visitor(node):
        if node.kind == SyntaxKind.VariableDeclaration:
            if node.children[1].token.kind == "IntKeyword":
                with node.rewriter:
                    # Remove int i
                    node.rewriter.remove(node)
                    # Add new variable
                    new_node = node.rewriter.factory.createVariableDeclaration(
                        "logic", "j"
                    )
                    node.rewriter.insertBefore(node.parent, new_node)
        return VisitAction.Advance

    expected = SyntaxTree.fromText("""
        module m;
            logic j;
            struct {
                logic l;
            } s;
        endmodule
    """, "test.sv")

    result = pyslang.rewrite(tree, visitor)
    assert result is not None
    assert result.root.isEquivalentTo(expected.root)

def test_rewriter_skip():
    # Test skipping nodes
    tree = SyntaxTree.fromText("""
        module m;
            int i;
            logic l;
        endmodule
    """, "test.sv")

    def visitor(node):
        if node.kind == SyntaxKind.VariableDeclaration:
            if node.children[1].token.kind == "IntKeyword":
                return VisitAction.Skip
        return VisitAction.Advance

    expected = SyntaxTree.fromText("""
        module m;
            int i;
            logic l;
        endmodule
    """, "test.sv")

    result = pyslang.rewrite(tree, visitor)
    assert result is not None
    assert result.root.isEquivalentTo(expected.root)
