// Generated from C:/Year five/Project/Scala-Mungo/Scala-Mungo-Prototype/src/main/scala\Typestate.g4 by ANTLR 4.8
package ProtocolParse;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link TypestateParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface TypestateVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link TypestateParser#start}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStart(TypestateParser.StartContext ctx);
	/**
	 * Visit a parse tree produced by {@link TypestateParser#ref}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRef(TypestateParser.RefContext ctx);
	/**
	 * Visit a parse tree produced by {@link TypestateParser#package_statement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPackage_statement(TypestateParser.Package_statementContext ctx);
	/**
	 * Visit a parse tree produced by {@link TypestateParser#import_statement}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitImport_statement(TypestateParser.Import_statementContext ctx);
	/**
	 * Visit a parse tree produced by {@link TypestateParser#typestate_declaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypestate_declaration(TypestateParser.Typestate_declarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link TypestateParser#typestate_body}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypestate_body(TypestateParser.Typestate_bodyContext ctx);
	/**
	 * Visit a parse tree produced by {@link TypestateParser#state_declaration}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitState_declaration(TypestateParser.State_declarationContext ctx);
	/**
	 * Visit a parse tree produced by {@link TypestateParser#state}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitState(TypestateParser.StateContext ctx);
	/**
	 * Visit a parse tree produced by {@link TypestateParser#method}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMethod(TypestateParser.MethodContext ctx);
	/**
	 * Visit a parse tree produced by {@link TypestateParser#decision_state}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDecision_state(TypestateParser.Decision_stateContext ctx);
	/**
	 * Visit a parse tree produced by {@link TypestateParser#decision}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDecision(TypestateParser.DecisionContext ctx);
	/**
	 * Visit a parse tree produced by {@link TypestateParser#id}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitId(TypestateParser.IdContext ctx);
}