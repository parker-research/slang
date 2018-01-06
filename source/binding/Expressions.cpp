//------------------------------------------------------------------------------
// Expressions.cpp
// Expression creation and analysis.
//
// File is under the MIT license; see LICENSE for details.
//------------------------------------------------------------------------------
#include "Expressions.h"

#include "compilation/Compilation.h"
#include "symbols/TypeSymbols.h"

namespace {

using namespace slang;

const Type* binaryOperatorType(Compilation& compilation, const Type* lt, const Type* rt, bool forceFourState) {
    uint32_t width = std::max(lt->getBitWidth(), rt->getBitWidth());
    if (lt->isFloating() || rt->isFloating()) {
        // TODO: The spec is unclear for binary operators what to do if the operands are a shortreal and a larger
        // integral type. For the conditional operator it is clear that this case should lead to a shortreal, and
        // it isn't explicitly mentioned for other binary operators
        if (width >= 64)
            return &compilation.getRealType();
        else
            return &compilation.getShortRealType();
    }
    else {
        const auto& li = lt->as<IntegralType>();
        const auto& ri = rt->as<IntegralType>();
        bool isSigned = li.isSigned && ri.isSigned;
        bool fourState = forceFourState || li.isFourState || ri.isFourState;
        return &compilation.getType((uint16_t)width, isSigned, fourState);
    }
}

}

namespace slang {

const InvalidExpression InvalidExpression::Instance(nullptr, ErrorType::Instance);

bool Expression::isLValue() const {
    switch (kind) {
        case ExpressionKind::ParameterRef:
        case ExpressionKind::VariableRef:
        case ExpressionKind::Select:
            return true;
        default:
            return false;
    }
}

Expression& Expression::fromSyntax(Compilation& compilation, const ExpressionSyntax& syntax, const Scope& scope) {
    switch (syntax.kind) {
        case SyntaxKind::NullLiteralExpression:
        case SyntaxKind::StringLiteralExpression:
        case SyntaxKind::TimeLiteralExpression:
        case SyntaxKind::WildcardLiteralExpression:
        case SyntaxKind::OneStepLiteralExpression:
            // TODO: unimplemented
            break;
        case SyntaxKind::IdentifierName:
        case SyntaxKind::IdentifierSelectName:
        case SyntaxKind::ScopedName:
            return bindName(compilation, syntax.as<NameSyntax>(), scope);
        case SyntaxKind::RealLiteralExpression:
            return RealLiteral::fromSyntax(compilation, syntax.as<LiteralExpressionSyntax>());
        case SyntaxKind::IntegerLiteralExpression:
            return IntegerLiteral::fromSyntax(compilation, syntax.as<LiteralExpressionSyntax>());
        case SyntaxKind::UnbasedUnsizedLiteralExpression:
            return UnbasedUnsizedIntegerLiteral::fromSyntax(compilation, syntax.as<LiteralExpressionSyntax>());
        case SyntaxKind::IntegerVectorExpression:
            return IntegerLiteral::fromSyntax(compilation, syntax.as<IntegerVectorExpressionSyntax>());
        case SyntaxKind::ParenthesizedExpression:
            return Expression::fromSyntax(compilation, syntax.as<ParenthesizedExpressionSyntax>().expression, scope);
        case SyntaxKind::UnaryPlusExpression:
        case SyntaxKind::UnaryMinusExpression:
        case SyntaxKind::UnaryBitwiseNotExpression:
        case SyntaxKind::UnaryBitwiseAndExpression:
        case SyntaxKind::UnaryBitwiseOrExpression:
        case SyntaxKind::UnaryBitwiseXorExpression:
        case SyntaxKind::UnaryBitwiseNandExpression:
        case SyntaxKind::UnaryBitwiseNorExpression:
        case SyntaxKind::UnaryBitwiseXnorExpression:
        case SyntaxKind::UnaryLogicalNotExpression:
            return UnaryExpression::fromSyntax(compilation, syntax.as<PrefixUnaryExpressionSyntax>(), scope);
        case SyntaxKind::AddExpression:
        case SyntaxKind::SubtractExpression:
        case SyntaxKind::MultiplyExpression:
        case SyntaxKind::DivideExpression:
        case SyntaxKind::ModExpression:
        case SyntaxKind::BinaryAndExpression:
        case SyntaxKind::BinaryOrExpression:
        case SyntaxKind::BinaryXorExpression:
        case SyntaxKind::BinaryXnorExpression:
        case SyntaxKind::EqualityExpression:
        case SyntaxKind::InequalityExpression:
        case SyntaxKind::CaseEqualityExpression:
        case SyntaxKind::CaseInequalityExpression:
        case SyntaxKind::GreaterThanEqualExpression:
        case SyntaxKind::GreaterThanExpression:
        case SyntaxKind::LessThanEqualExpression:
        case SyntaxKind::LessThanExpression:
        case SyntaxKind::WildcardEqualityExpression:
        case SyntaxKind::WildcardInequalityExpression:
        case SyntaxKind::LogicalAndExpression:
        case SyntaxKind::LogicalOrExpression:
        case SyntaxKind::LogicalImplicationExpression:
        case SyntaxKind::LogicalEquivalenceExpression:
        case SyntaxKind::LogicalShiftLeftExpression:
        case SyntaxKind::LogicalShiftRightExpression:
        case SyntaxKind::ArithmeticShiftLeftExpression:
        case SyntaxKind::ArithmeticShiftRightExpression:
        case SyntaxKind::PowerExpression:
        case SyntaxKind::AssignmentExpression:
        case SyntaxKind::AddAssignmentExpression:
        case SyntaxKind::SubtractAssignmentExpression:
        case SyntaxKind::MultiplyAssignmentExpression:
        case SyntaxKind::DivideAssignmentExpression:
        case SyntaxKind::ModAssignmentExpression:
        case SyntaxKind::AndAssignmentExpression:
        case SyntaxKind::OrAssignmentExpression:
        case SyntaxKind::XorAssignmentExpression:
        case SyntaxKind::LogicalLeftShiftAssignmentExpression:
        case SyntaxKind::LogicalRightShiftAssignmentExpression:
        case SyntaxKind::ArithmeticLeftShiftAssignmentExpression:
        case SyntaxKind::ArithmeticRightShiftAssignmentExpression:
            return BinaryExpression::fromSyntax(compilation, syntax.as<BinaryExpressionSyntax>(), scope);
        case SyntaxKind::InvocationExpression:
            return CallExpression::fromSyntax(compilation, syntax.as<InvocationExpressionSyntax>(), scope);
        case SyntaxKind::ConditionalExpression:
            return ConditionalExpression::fromSyntax(compilation, syntax.as<ConditionalExpressionSyntax>(), scope);
        case SyntaxKind::ConcatenationExpression:
            return ConcatenationExpression::fromSyntax(compilation, syntax.as<ConcatenationExpressionSyntax>(),
                                                       scope);
        case SyntaxKind::MultipleConcatenationExpression:
            return BinaryExpression::fromSyntax(compilation, syntax.as<MultipleConcatenationExpressionSyntax>(),
                                                scope);
        case SyntaxKind::ElementSelectExpression:
            return bindSelectExpression(compilation, syntax.as<ElementSelectExpressionSyntax>(), scope);
        default:
            THROW_UNREACHABLE;
    }
    return scope.getCompilation().badExpression(nullptr);
}

Expression& Expression::bindName(Compilation& compilation, const NameSyntax& syntax, const Scope& scope) {
    if (syntax.kind != SyntaxKind::IdentifierName)
        return compilation.badExpression(nullptr);

    string_view name = syntax.as<IdentifierNameSyntax>().identifier.valueText();
    const Symbol* symbol = scope.lookupUnqualified(name);
    if (!symbol) {
        compilation.addError(DiagCode::UndeclaredIdentifier, syntax.as<IdentifierNameSyntax>().identifier.location()) << name;
        return compilation.badExpression(nullptr);
    }
    
    switch (symbol->kind) {
        case SymbolKind::Variable:
        case SymbolKind::FormalArgument:
            return *compilation.emplace<VariableRefExpression>(symbol->as<VariableSymbol>(), syntax.sourceRange());
    
        case SymbolKind::Parameter:
            return *compilation.emplace<ParameterRefExpression>(symbol->as<ParameterSymbol>(), syntax.sourceRange());
    
        default:
            THROW_UNREACHABLE;
    }
}

//Expression& Binder::bindSelectName(const IdentifierSelectNameSyntax& syntax) {
//    // TODO: once we fully support more complex non-integral types and actual support
//    // part selects, we need to be able to handle multiple accesses like
//    // foo[2 : 4][3 : 1][7 : 8] where each access depends on the type of foo, not just the type of the preceding
//    // expression. For now though, we implement the most simple case:
//    // foo[SELECT] where foo is an integral type.
//
//    ASSERT(syntax.selectors.count() == 1);
//    ASSERT(syntax.selectors[0]->selector);
//    // spoof this being just a simple ElementSelectExpression
//    return bindSelectExpression(syntax,
//        bindName(*compilation.emplace<IdentifierNameSyntax>(syntax.identifier)), *syntax.selectors[0]->selector);
//}
//
//Expression& Binder::bindScopedName(const ScopedNameSyntax& syntax) {
//    // TODO: only handles packages right now
//    if (syntax.separator.kind != TokenKind::DoubleColon || syntax.left.kind != SyntaxKind::IdentifierName)
//        return badExpr(nullptr);
//
//    string_view identifier = syntax.left.as<IdentifierNameSyntax>().identifier.valueText();
//    if (identifier.empty())
//        return badExpr(nullptr);
//
//    auto package = scope.getCompilation().getPackage(identifier);
//    if (!package)
//        return badExpr(nullptr);
//
//    return Binder(*package).bindName(syntax.right);
//}

Expression& Expression::bindSelectExpression(Compilation& compilation, const ElementSelectExpressionSyntax& syntax, const Scope& scope) {
    Expression& expr = Expression::fromSyntax(compilation, syntax.left, scope);
    // TODO: null selector?
    return bindSelectExpression(compilation, syntax, expr, *syntax.select.selector, scope);
}

Expression& Expression::bindSelectExpression(Compilation& compilation, const ExpressionSyntax& syntax, Expression& expr, const SelectorSyntax& selector, const Scope& scope) {
    // if (down), the indices are declares going down, [15:0], so
    // msb > lsb
    if (expr.bad())
        return compilation.badExpression(&expr);

    const auto& integralType = expr.type->as<IntegralType>();
    bool down = integralType.getBitVectorRange().isLittleEndian();
    Expression* left = nullptr;
    Expression* right = nullptr;
    int width = 0;

    // TODO: errors if things that should be constant expressions aren't actually constant expressions
    SyntaxKind kind = selector.kind;
    switch (kind) {
        case SyntaxKind::BitSelect:
            left = &Expression::fromSyntax(compilation, selector.as<BitSelectSyntax>().expr, scope);
            right = left;
            width = 1;
            break;
        case SyntaxKind::SimpleRangeSelect:
            left = &Expression::fromSyntax(compilation, selector.as<RangeSelectSyntax>().left, scope); // msb
            right = &Expression::fromSyntax(compilation, selector.as<RangeSelectSyntax>().right, scope); // lsb
            width = (down ? 1 : -1) * (int)(left->eval().integer().as<int64_t>().value() -
                    right->eval().integer().as<int64_t>().value());
            break;
        case SyntaxKind::AscendingRangeSelect:
        case SyntaxKind::DescendingRangeSelect:
            left = &Expression::fromSyntax(compilation, selector.as<RangeSelectSyntax>().left, scope); // msb/lsb
            right = &Expression::fromSyntax(compilation, selector.as<RangeSelectSyntax>().right, scope); // width
            width = int(right->eval().integer().as<int64_t>().value());
            break;
        default:
            THROW_UNREACHABLE;
    }

    return *compilation.emplace<SelectExpression>(
        compilation.getType((uint16_t)width, integralType.isSigned, integralType.isFourState),
        kind,
        expr,
        *left,
        *right,
        syntax.sourceRange()
    );
}

Expression& Expression::convert(Compilation& compilation, ConversionKind conversionKind, const Type& type,
                                Expression& expr) {
    return *compilation.emplace<ConversionExpression>(conversionKind, type, expr, expr.sourceRange);
}

Expression& IntegerLiteral::fromSyntax(Compilation& compilation, const LiteralExpressionSyntax& syntax) {
    ASSERT(syntax.kind == SyntaxKind::IntegerLiteralExpression);

    return *compilation.emplace<IntegerLiteral>(
        compilation, compilation.getIntType(),
        syntax.literal.intValue(), syntax.sourceRange()
    );
}

Expression& IntegerLiteral::fromSyntax(Compilation& compilation, const IntegerVectorExpressionSyntax& syntax) {
    const SVInt& value = syntax.value.intValue();
    const Type& type = compilation.getType(value.getBitWidth(), value.isSigned(), value.hasUnknown());
    return *compilation.emplace<IntegerLiteral>(compilation, type, value, syntax.sourceRange());
}

Expression& RealLiteral::fromSyntax(Compilation& compilation, const LiteralExpressionSyntax& syntax) {
    ASSERT(syntax.kind == SyntaxKind::RealLiteralExpression);

    return *compilation.emplace<RealLiteral>(
        compilation.getRealType(), syntax.literal.realValue(),
        syntax.sourceRange()
    );
}

Expression& UnbasedUnsizedIntegerLiteral::fromSyntax(Compilation& compilation,
                                                     const LiteralExpressionSyntax& syntax) {
    ASSERT(syntax.kind == SyntaxKind::UnbasedUnsizedLiteralExpression);

    // UnsizedUnbasedLiteralExpressions default to a size of 1 in an undetermined
    // context, but can grow to be wider during propagation.
    logic_t val = syntax.literal.bitValue();
    return *compilation.emplace<UnbasedUnsizedIntegerLiteral>(
        compilation.getType(1, false, val.isUnknown()),
        val, syntax.sourceRange()
    );
}

Expression& UnaryExpression::fromSyntax(Compilation& compilation, const PrefixUnaryExpressionSyntax& syntax,
                                        const Scope& scope) {
    Expression& operand = Expression::fromSyntax(compilation, syntax.operand, scope);
    const Type* type = operand.type;

    Expression* result = compilation.emplace<UnaryExpression>(
        getUnaryOperator(syntax.kind), *type, operand, syntax.sourceRange()
    );

    if (operand.bad())
        return compilation.badExpression(result);

    bool good;
    switch (syntax.kind) {
        case SyntaxKind::UnaryPlusExpression:
        case SyntaxKind::UnaryMinusExpression:
        case SyntaxKind::UnaryLogicalNotExpression:
            // Supported for both integral and real types.
            good = type->isNumeric();
            break;
        case SyntaxKind::UnaryBitwiseNotExpression:
        case SyntaxKind::UnaryBitwiseAndExpression:
        case SyntaxKind::UnaryBitwiseOrExpression:
        case SyntaxKind::UnaryBitwiseXorExpression:
        case SyntaxKind::UnaryBitwiseNandExpression:
        case SyntaxKind::UnaryBitwiseNorExpression:
        case SyntaxKind::UnaryBitwiseXnorExpression:
            // Supported for integral only. Result type is always a single bit.
            good = type->isNumeric();
            result->type = &compilation.getLogicType();
            break;
        default:
            THROW_UNREACHABLE;
    }

    if (!good) {
        auto& diag = compilation.addError(DiagCode::BadUnaryExpression, syntax.operatorToken.location());
        diag << *type;
        diag << operand.sourceRange;
        return compilation.badExpression(result);
    }

    return *result;
}

Expression& BinaryExpression::fromSyntax(Compilation& compilation, const BinaryExpressionSyntax& syntax,
                                         const Scope& scope) {
    Expression& lhs = Expression::fromSyntax(compilation, syntax.left, scope);
    Expression& rhs = Expression::fromSyntax(compilation, syntax.right, scope);
    const Type* lt = lhs.type;
    const Type* rt = rhs.type;

    BinaryExpression* result = compilation.emplace<BinaryExpression>(
        getBinaryOperator(syntax.kind), *lhs.type,
        lhs, rhs, syntax.sourceRange()
    );

    if (lhs.bad() || rhs.bad())
        return compilation.badExpression(result);

    bool bothIntegral = lt->isIntegral() && rt->isIntegral();
    bool bothNumeric = lt->isNumeric() && rt->isNumeric();

    bool good;
    switch (syntax.kind) {
        case SyntaxKind::AddExpression:
        case SyntaxKind::SubtractExpression:
        case SyntaxKind::MultiplyExpression:
        case SyntaxKind::AddAssignmentExpression:
        case SyntaxKind::SubtractAssignmentExpression:
        case SyntaxKind::MultiplyAssignmentExpression:
            good = bothNumeric;
            result->type = binaryOperatorType(compilation, lt, rt, false);
            break;
        case SyntaxKind::BinaryAndExpression:
        case SyntaxKind::BinaryOrExpression:
        case SyntaxKind::BinaryXorExpression:
        case SyntaxKind::BinaryXnorExpression:
        case SyntaxKind::LogicalShiftLeftExpression:
        case SyntaxKind::LogicalShiftRightExpression:
        case SyntaxKind::ArithmeticShiftLeftExpression:
        case SyntaxKind::ArithmeticShiftRightExpression:
        case SyntaxKind::AndAssignmentExpression:
        case SyntaxKind::OrAssignmentExpression:
        case SyntaxKind::XorAssignmentExpression:
        case SyntaxKind::LogicalLeftShiftAssignmentExpression:
        case SyntaxKind::LogicalRightShiftAssignmentExpression:
        case SyntaxKind::ArithmeticLeftShiftAssignmentExpression:
        case SyntaxKind::ArithmeticRightShiftAssignmentExpression:
            good = bothIntegral;
            result->type = binaryOperatorType(compilation, lt, rt, false);
            break;
        case SyntaxKind::DivideExpression:
        case SyntaxKind::DivideAssignmentExpression:
        case SyntaxKind::PowerExpression:
            // Result is forced to 4-state because result can be X.
            good = bothNumeric;
            result->type = binaryOperatorType(compilation, lt, rt, true);
            break;
        case SyntaxKind::ModExpression:
        case SyntaxKind::ModAssignmentExpression:
            // Result is forced to 4-state because result can be X.
            good = bothIntegral;
            result->type = binaryOperatorType(compilation, lt, rt, true);
            break;
        case SyntaxKind::GreaterThanEqualExpression:
        case SyntaxKind::GreaterThanExpression:
        case SyntaxKind::LessThanEqualExpression:
        case SyntaxKind::LessThanExpression:
        case SyntaxKind::LogicalAndExpression:
        case SyntaxKind::LogicalOrExpression:
        case SyntaxKind::LogicalImplicationExpression:
        case SyntaxKind::LogicalEquivalenceExpression:
            // Result is always a single bit.
            good = bothNumeric;
            result->type = bothIntegral ? &compilation.getLogicType() : &compilation.getBitType();
            break;
        case SyntaxKind::EqualityExpression:
        case SyntaxKind::InequalityExpression:
        case SyntaxKind::WildcardEqualityExpression:
        case SyntaxKind::WildcardInequalityExpression:
        case SyntaxKind::CaseEqualityExpression:
        case SyntaxKind::CaseInequalityExpression:
            // Two types are comparable if:
            // - they are both integral or floating
            // - both classes or null, and assignment compatible
            // - both chandles or null
            // - both aggregates and equivalent
            if (bothNumeric) {
                good = true;
                result->type = bothIntegral ? &compilation.getLogicType() : &compilation.getBitType();
            }
            else if (lt->isAggregate() && lt->isEquivalent(*rt)) {
                // TODO: drill into the aggregate and figure out if it's all 2-state
                good = true;
                result->type = &compilation.getLogicType();
            }
            else if ((lt->isClass() && lt->isAssignmentCompatible(*rt)) ||
                     (rt->isClass() && rt->isAssignmentCompatible(*lt))) {
                good = true;
                result->type = &compilation.getBitType();
            }
            else if ((lt->isCHandle() || lt->isNull()) &&
                     (rt->isCHandle() || rt->isNull())) {
                good = true;
                result->type = &compilation.getBitType();
            }
            else {
                good = false;
            }
            break;
        case SyntaxKind::AssignmentExpression:
            // No particular restriction on types here. We'll handle
            // assignability below.
            good = true;
            break;
        default:
            THROW_UNREACHABLE;
    }

    auto location = syntax.operatorToken.location();
    if (!good) {
        auto& diag = compilation.addError(DiagCode::BadBinaryExpression, location);
        diag << *lt << *rt;
        diag << lhs.sourceRange;
        diag << rhs.sourceRange;
        return compilation.badExpression(result);
    }

    if (result->isAssignment()) {
        if (!lhs.isLValue()) {
            auto& diag = compilation.addError(DiagCode::ExpressionNotAssignable, location);
            diag << lhs.sourceRange;
            return compilation.badExpression(result);
        }

        // TODO: check for const assignment

        if (!lt->isAssignmentCompatible(*rt)) {
            DiagCode code = lt->isCastCompatible(*rt) ? DiagCode::NoImplicitConversion : DiagCode::BadAssignment;
            auto& diag = compilation.addError(code, location);
            diag << *rt << *lt;
            diag << lhs.sourceRange;
            diag << rhs.sourceRange;
            return compilation.badExpression(result);
        }

        // TODO: unify this with Compilation::bindAssignment
        if (lt->getBitWidth() > rt->getBitWidth()) {
            if (!lt->isFloating() && !rt->isFloating()) {
                const auto& ri = rt->as<IntegralType>();
                rt = &compilation.getType((uint16_t)lt->getBitWidth(), ri.isSigned, ri.isFourState);
            }
            else {
                if (lt->getBitWidth() > 32)
                    rt = &compilation.getRealType();
                else
                    rt = &compilation.getShortRealType();
            }
            // TODO: return value?
            Expression::propagateAndFold(compilation, rhs, *rt);
        }
        else {
            Expression::propagateAndFold(compilation, rhs, *rhs.type);
        }
        result->type = lhs.type;
    }

    return *result;
}

Expression& BinaryExpression::fromSyntax(Compilation& compilation,
                                         const MultipleConcatenationExpressionSyntax& syntax, const Scope& scope) {
    Expression& left  = Expression::fromSyntax(compilation, syntax.expression, scope);
    Expression& right = Expression::fromSyntax(compilation, syntax.concatenation, scope);
    // TODO: check applicability
    // TODO: left must be compile-time evaluatable, and it must be known in order to
    // compute the type of a multiple concatenation. Have a nice error when this isn't the case?
    // TODO: in cases like these, should we bother storing the bound expression? should we at least cache the result
    // so we don't have to compute it again elsewhere?
    uint16_t replicationTimes = left.eval().integer().as<uint16_t>().value();
    const Type& resultType = compilation.getType((uint16_t)right.type->getBitWidth() * replicationTimes, false);
    return *compilation.emplace<BinaryExpression>(BinaryOperator::Replication, resultType, left,
                                                  right, syntax.sourceRange());
}

Expression& ConditionalExpression::fromSyntax(Compilation& compilation, const ConditionalExpressionSyntax& syntax,
                                              const Scope& scope) {
    // TODO: handle the pattern matching conditional predicate case, rather than just assuming that it's a simple
    // expression
    ASSERT(syntax.predicate.conditions.count() == 1);
    Expression& pred = Expression::fromSyntax(compilation, syntax.predicate.conditions[0]->expr, scope);
    Expression& left = Expression::fromSyntax(compilation, syntax.left, scope);
    Expression& right = Expression::fromSyntax(compilation, syntax.right, scope);

    // TODO: handle non-integral and non-real types properly
    // force four-state return type for ambiguous condition case
    const Type* type = binaryOperatorType(compilation, left.type, right.type, true);
    return *compilation.emplace<ConditionalExpression>(*type, pred, left, right, syntax.sourceRange());
}

Expression& ConcatenationExpression::fromSyntax(Compilation& compilation,
                                                const ConcatenationExpressionSyntax& syntax, const Scope& scope) {
    SmallVectorSized<const Expression*, 8> buffer;
    uint16_t totalWidth = 0;
    for (auto argSyntax : syntax.expressions) {
        // All operands are self-determined.
        Expression& arg = Expression::fromSyntax(compilation, *argSyntax, scope);
        buffer.append(&arg);

        const Type& type = *arg.type;
        if (!type.isIntegral()) {
            return compilation.badExpression(compilation.emplace<ConcatenationExpression>(compilation.getErrorType(),
                                                                                          nullptr,
                                                                                          syntax.sourceRange()));
        }
        totalWidth += (uint16_t)type.as<IntegralType>().bitWidth;
    }

    return *compilation.emplace<ConcatenationExpression>(compilation.getType(totalWidth, false),
                                                         buffer.copy(compilation), syntax.sourceRange());
}

Expression& CallExpression::fromSyntax(Compilation& compilation, const InvocationExpressionSyntax& syntax,
                                       const Scope& scope) {
    // TODO: check for something other than a simple name on the LHS
    auto name = syntax.left.getFirstToken();
    const Symbol* symbol = scope.lookupUnqualified(name.valueText(), LookupLocation::max, LookupNameKind::Callable);
    ASSERT(symbol && symbol->kind == SymbolKind::Subroutine);

    auto actualArgs = syntax.arguments->parameters;
    const SubroutineSymbol& subroutine = symbol->as<SubroutineSymbol>();

    // TODO: handle too few args as well, which requires looking at default values
    auto formalArgs = subroutine.arguments;
    if (formalArgs.size() < actualArgs.count()) {
        auto& diag = compilation.addError(DiagCode::TooManyArguments, name.location());
        diag << syntax.left.sourceRange();
        diag << formalArgs.size();
        diag << actualArgs.count();
        return compilation.badExpression(nullptr);
    }

    // TODO: handle named arguments in addition to ordered
    SmallVectorSized<const Expression*, 8> buffer;
    for (uint32_t i = 0; i < actualArgs.count(); i++) {
        const auto& arg = actualArgs[i]->as<OrderedArgumentSyntax>();
        buffer.append(&compilation.bindAssignment(*formalArgs[i]->type, arg.expr, scope,
                                                  arg.getFirstToken().location()));
    }

    return *compilation.emplace<CallExpression>(subroutine, buffer.copy(compilation), syntax.sourceRange());
}

IntegerLiteral::IntegerLiteral(BumpAllocator& alloc, const Type& type, const SVInt& value,
                               SourceRange sourceRange) :
    Expression(ExpressionKind::IntegerLiteral, type, sourceRange),
    valueStorage(value.getBitWidth(), value.isSigned(), value.hasUnknown())
{
    if (value.isSingleWord())
        valueStorage.val = *value.getRawData();
    else {
        valueStorage.pVal = (uint64_t*)alloc.allocate(sizeof(uint64_t) * value.getNumWords(), alignof(uint64_t));
        memcpy(valueStorage.pVal, value.getRawData(), sizeof(uint64_t) * value.getNumWords());
    }
}

bool BinaryExpression::isAssignment() const {
    switch (op) {
        case BinaryOperator::Assignment:
        case BinaryOperator::AddAssignment:
        case BinaryOperator::SubtractAssignment:
        case BinaryOperator::MultiplyAssignment:
        case BinaryOperator::DivideAssignment:
        case BinaryOperator::ModAssignment:
        case BinaryOperator::AndAssignment:
        case BinaryOperator::OrAssignment:
        case BinaryOperator::XorAssignment:
        case BinaryOperator::LogicalLeftShiftAssignment:
        case BinaryOperator::LogicalRightShiftAssignment:
        case BinaryOperator::ArithmeticLeftShiftAssignment:
        case BinaryOperator::ArithmeticRightShiftAssignment:
            return true;
        default:
            return false;
    }
}

Expression& Expression::propagateAndFold(Compilation& compilation, Expression& expr, const Type& newType) {
    if (expr.type->isError() || newType.isError())
        return expr;

    // If we're propagating a floating type down to a non-floating type, that operand
    // will instead be converted in a self-determined context.
    if (newType.isFloating() && !expr.type->isFloating() && expr.kind != ExpressionKind::Conversion)
        return convert(compilation, ConversionKind::IntToFloat, newType, expr);

    switch (expr.kind) {
        case ExpressionKind::Invalid:
            return expr;
        case ExpressionKind::IntegerLiteral:
            return IntegerLiteral::propagateAndFold(compilation, expr.as<IntegerLiteral>(), newType);
        case ExpressionKind::RealLiteral:
            return RealLiteral::propagateAndFold(compilation, expr.as<RealLiteral>(), newType);
        case ExpressionKind::UnbasedUnsizedIntegerLiteral:
            return UnbasedUnsizedIntegerLiteral::propagateAndFold(
                compilation,
                expr.as<UnbasedUnsizedIntegerLiteral>(),
                newType
            );
        case ExpressionKind::Call:
        case ExpressionKind::VariableRef:
        case ExpressionKind::ParameterRef:
        case ExpressionKind::Concatenation:
        case ExpressionKind::Select:
            return expr;
        case ExpressionKind::UnaryOp:
            return UnaryExpression::propagateAndFold(compilation, expr.as<UnaryExpression>(), newType);
        case ExpressionKind::BinaryOp:
            return BinaryExpression::propagateAndFold(compilation, expr.as<BinaryExpression>(), newType);
        case ExpressionKind::ConditionalOp:
            return ConditionalExpression::propagateAndFold(compilation, expr.as<ConditionalExpression>(), newType);
        case ExpressionKind::Conversion:
            return ConversionExpression::propagateAndFold(compilation, expr.as<ConversionExpression>(), newType);
    }
    THROW_UNREACHABLE;
}

void Expression::contextDetermined(Compilation& compilation, Expression*& expr, const Type& newType) {
    expr = &Expression::propagateAndFold(compilation, *expr, newType);
}

void Expression::selfDetermined(Compilation& compilation, Expression*& expr) {
    expr = &Expression::propagateAndFold(compilation, *expr, *expr->type);
}

Expression& IntegerLiteral::propagateAndFold(Compilation& compilation, IntegerLiteral& expr,
                                             const Type& newType) {
    ASSERT(newType.isIntegral());
    ASSERT(newType.getBitWidth() >= expr.type->getBitWidth());

    if (newType.getBitWidth() != expr.type->getBitWidth())
        return convert(compilation, ConversionKind::IntExtension, newType, expr);

    expr.type = &newType;
    return expr;
}

Expression& RealLiteral::propagateAndFold(Compilation& compilation, RealLiteral& expr,
                                          const Type& newType) {
    ASSERT(newType.isFloating());
    ASSERT(newType.getBitWidth() >= expr.type->getBitWidth());

    if (newType.getBitWidth() != expr.type->getBitWidth())
        return convert(compilation, ConversionKind::FloatExtension, newType, expr);

    expr.type = &newType;
    return expr;
}

Expression& UnbasedUnsizedIntegerLiteral::propagateAndFold(Compilation&,
                                                           UnbasedUnsizedIntegerLiteral& expr,
                                                           const Type& newType) {
    ASSERT(newType.isIntegral());
    ASSERT(newType.getBitWidth() >= expr.type->getBitWidth());

    expr.type = &newType;
    return expr;
}

Expression& UnaryExpression::propagateAndFold(Compilation& compilation, UnaryExpression& expr,
                                              const Type& newType) {
    switch (expr.op) {
        case UnaryOperator::Plus:
        case UnaryOperator::Minus:
        case UnaryOperator::BitwiseNot:
            expr.type = &newType;
            contextDetermined(compilation, expr.operand_, newType);
            break;
        case UnaryOperator::BitwiseAnd:
        case UnaryOperator::BitwiseOr:
        case UnaryOperator::BitwiseXor:
        case UnaryOperator::BitwiseNand:
        case UnaryOperator::BitwiseNor:
        case UnaryOperator::BitwiseXnor:
        case UnaryOperator::LogicalNot:
            // Type is already set (always 1 bit).
            selfDetermined(compilation, expr.operand_);
            break;
    }
    return expr;
}

Expression& BinaryExpression::propagateAndFold(Compilation& compilation, BinaryExpression& expr,
                                               const Type& newType) {
    switch (expr.op) {
        case BinaryOperator::Add:
        case BinaryOperator::Subtract:
        case BinaryOperator::Multiply:
        case BinaryOperator::Divide:
        case BinaryOperator::Mod:
        case BinaryOperator::BinaryAnd:
        case BinaryOperator::BinaryOr:
        case BinaryOperator::BinaryXor:
        case BinaryOperator::BinaryXnor:
            expr.type = &newType;
            contextDetermined(compilation, expr.left_, newType);
            contextDetermined(compilation, expr.right_, newType);
            break;
        case BinaryOperator::Equality:
        case BinaryOperator::Inequality:
        case BinaryOperator::CaseEquality:
        case BinaryOperator::CaseInequality:
        case BinaryOperator::GreaterThanEqual:
        case BinaryOperator::GreaterThan:
        case BinaryOperator::LessThanEqual:
        case BinaryOperator::LessThan:
        case BinaryOperator::WildcardEquality:
        case BinaryOperator::WildcardInequality: {
            // Relational expressions affect each other but don't affect the result type,
            // which has already been set at 1 bit. Figure out which type to propagate.
            // TODO: handle non-integer
            auto nt = binaryOperatorType(compilation, expr.left().type, expr.right().type, false);
            contextDetermined(compilation, expr.left_, *nt);
            contextDetermined(compilation, expr.right_, *nt);
            break;
        }
        case BinaryOperator::LogicalAnd:
        case BinaryOperator::LogicalOr:
        case BinaryOperator::LogicalImplication:
        case BinaryOperator::LogicalEquivalence:
            // Type is already set (always 1 bit).
            selfDetermined(compilation, expr.left_);
            selfDetermined(compilation, expr.right_);
            break;
        case BinaryOperator::LogicalShiftLeft:
        case BinaryOperator::LogicalShiftRight:
        case BinaryOperator::ArithmeticShiftLeft:
        case BinaryOperator::ArithmeticShiftRight:
        case BinaryOperator::Power:
            // Only the left hand side gets propagated; the rhs is self determined.
            expr.type = &newType;
            contextDetermined(compilation, expr.left_, newType);
            selfDetermined(compilation, expr.right_);
            break;
        case BinaryOperator::Assignment:
        case BinaryOperator::AddAssignment:
        case BinaryOperator::SubtractAssignment:
        case BinaryOperator::MultiplyAssignment:
        case BinaryOperator::DivideAssignment:
        case BinaryOperator::ModAssignment:
        case BinaryOperator::AndAssignment:
        case BinaryOperator::OrAssignment:
        case BinaryOperator::XorAssignment:
        case BinaryOperator::LogicalLeftShiftAssignment:
        case BinaryOperator::LogicalRightShiftAssignment:
        case BinaryOperator::ArithmeticLeftShiftAssignment:
        case BinaryOperator::ArithmeticRightShiftAssignment:
            // Essentially self determined, logic handled at creation time.
            break;
        case BinaryOperator::Replication:
            expr.type = &newType;
            break;
    }
    return expr;
}

Expression& ConditionalExpression::propagateAndFold(Compilation& compilation, ConditionalExpression& expr,
                                                    const Type& newType) {
    // predicate is self determined
    expr.type = &newType;
    selfDetermined(compilation, expr.pred_);
    contextDetermined(compilation, expr.left_, newType);
    contextDetermined(compilation, expr.right_, newType);
    return expr;
}

Expression& ConversionExpression::propagateAndFold(Compilation& compilation, ConversionExpression& expr,
                                                   const Type& newType) {
    // predicate is self determined
    expr.type = &newType;
    selfDetermined(compilation, expr.operand_);
    return expr;
}

UnaryOperator getUnaryOperator(SyntaxKind kind) {
    switch (kind) {
        case SyntaxKind::UnaryPlusExpression: return UnaryOperator::Plus;
        case SyntaxKind::UnaryMinusExpression: return UnaryOperator::Minus;
        case SyntaxKind::UnaryBitwiseNotExpression: return UnaryOperator::BitwiseNot;
        case SyntaxKind::UnaryBitwiseAndExpression: return UnaryOperator::BitwiseAnd;
        case SyntaxKind::UnaryBitwiseOrExpression: return UnaryOperator::BitwiseOr;
        case SyntaxKind::UnaryBitwiseXorExpression: return UnaryOperator::BitwiseXor;
        case SyntaxKind::UnaryBitwiseNandExpression: return UnaryOperator::BitwiseNand;
        case SyntaxKind::UnaryBitwiseNorExpression: return UnaryOperator::BitwiseNor;
        case SyntaxKind::UnaryBitwiseXnorExpression: return UnaryOperator::BitwiseXnor;
        case SyntaxKind::UnaryLogicalNotExpression: return UnaryOperator::LogicalNot;
        default: THROW_UNREACHABLE;
    }
}

BinaryOperator getBinaryOperator(SyntaxKind kind) {
    switch (kind) {
        case SyntaxKind::AddExpression: return BinaryOperator::Add;
        case SyntaxKind::SubtractExpression: return BinaryOperator::Subtract;
        case SyntaxKind::MultiplyExpression: return BinaryOperator::Multiply;
        case SyntaxKind::DivideExpression: return BinaryOperator::Divide;
        case SyntaxKind::ModExpression: return BinaryOperator::Mod;
        case SyntaxKind::BinaryAndExpression: return BinaryOperator::BinaryAnd;
        case SyntaxKind::BinaryOrExpression: return BinaryOperator::BinaryOr;
        case SyntaxKind::BinaryXorExpression: return BinaryOperator::BinaryXor;
        case SyntaxKind::BinaryXnorExpression: return BinaryOperator::BinaryXnor;
        case SyntaxKind::EqualityExpression: return BinaryOperator::Equality;
        case SyntaxKind::InequalityExpression: return BinaryOperator::Inequality;
        case SyntaxKind::CaseEqualityExpression: return BinaryOperator::CaseEquality;
        case SyntaxKind::CaseInequalityExpression: return BinaryOperator::CaseInequality;
        case SyntaxKind::GreaterThanEqualExpression: return BinaryOperator::GreaterThanEqual;
        case SyntaxKind::GreaterThanExpression: return BinaryOperator::GreaterThan;
        case SyntaxKind::LessThanEqualExpression: return BinaryOperator::LessThanEqual;
        case SyntaxKind::LessThanExpression: return BinaryOperator::LessThan;
        case SyntaxKind::WildcardEqualityExpression: return BinaryOperator::WildcardEquality;
        case SyntaxKind::WildcardInequalityExpression: return BinaryOperator::WildcardInequality;
        case SyntaxKind::LogicalAndExpression: return BinaryOperator::LogicalAnd;
        case SyntaxKind::LogicalOrExpression: return BinaryOperator::LogicalOr;
        case SyntaxKind::LogicalImplicationExpression: return BinaryOperator::LogicalImplication;
        case SyntaxKind::LogicalEquivalenceExpression: return BinaryOperator::LogicalEquivalence;
        case SyntaxKind::LogicalShiftLeftExpression: return BinaryOperator::LogicalShiftLeft;
        case SyntaxKind::LogicalShiftRightExpression: return BinaryOperator::LogicalShiftRight;
        case SyntaxKind::ArithmeticShiftLeftExpression: return BinaryOperator::ArithmeticShiftLeft;
        case SyntaxKind::ArithmeticShiftRightExpression: return BinaryOperator::ArithmeticShiftRight;
        case SyntaxKind::PowerExpression: return BinaryOperator::Power;
        case SyntaxKind::MultipleConcatenationExpression: return BinaryOperator::Replication;
        case SyntaxKind::AssignmentExpression: return BinaryOperator::Assignment;
        case SyntaxKind::AddAssignmentExpression: return BinaryOperator::AddAssignment;
        case SyntaxKind::SubtractAssignmentExpression: return BinaryOperator::SubtractAssignment;
        case SyntaxKind::MultiplyAssignmentExpression: return BinaryOperator::MultiplyAssignment;
        case SyntaxKind::DivideAssignmentExpression: return BinaryOperator::DivideAssignment;
        case SyntaxKind::ModAssignmentExpression: return BinaryOperator::ModAssignment;
        case SyntaxKind::AndAssignmentExpression: return BinaryOperator::AndAssignment;
        case SyntaxKind::OrAssignmentExpression: return BinaryOperator::OrAssignment;
        case SyntaxKind::XorAssignmentExpression: return BinaryOperator::XorAssignment;
        case SyntaxKind::LogicalLeftShiftAssignmentExpression: return BinaryOperator::LogicalLeftShiftAssignment;
        case SyntaxKind::LogicalRightShiftAssignmentExpression: return BinaryOperator::LogicalRightShiftAssignment;
        case SyntaxKind::ArithmeticLeftShiftAssignmentExpression: return BinaryOperator::ArithmeticLeftShiftAssignment;
        case SyntaxKind::ArithmeticRightShiftAssignmentExpression: return BinaryOperator::ArithmeticRightShiftAssignment;
        default: THROW_UNREACHABLE;
    }
}

}
