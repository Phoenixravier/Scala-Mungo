// Generated from C:/Year five/Project/Scala-Mungo/Scala-Mungo-Prototype/src/main/scala\Typestate.g4 by ANTLR 4.8
package ProtocolParse;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.ATN;
import org.antlr.v4.runtime.atn.ATNDeserializer;
import org.antlr.v4.runtime.atn.ParserATNSimulator;
import org.antlr.v4.runtime.atn.PredictionContextCache;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.tree.ParseTreeListener;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;
import org.antlr.v4.runtime.tree.TerminalNode;

import java.util.ArrayList;
import java.util.List;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class TypestateParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.8", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, T__3=4, T__4=5, T__5=6, T__6=7, T__7=8, T__8=9, 
		T__9=10, T__10=11, T__11=12, T__12=13, T__13=14, T__14=15, T__15=16, DROP=17, 
		END=18, ID=19, WS=20, BlockComment=21, LineComment=22;
	public static final int
		RULE_start = 0, RULE_ref = 1, RULE_package_statement = 2, RULE_import_statement = 3, 
		RULE_typestate_declaration = 4, RULE_typestate_body = 5, RULE_state_declaration = 6, 
		RULE_state = 7, RULE_method = 8, RULE_decision_state = 9, RULE_decision = 10, 
		RULE_id = 11;
	private static String[] makeRuleNames() {
		return new String[] {
			"start", "ref", "package_statement", "import_statement", "typestate_declaration", 
			"typestate_body", "state_declaration", "state", "method", "decision_state", 
			"decision", "id"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'.'", "'package'", "'import'", "'static'", "'*'", "'typestate'", 
			"'{'", "'}'", "'='", "','", "':'", "'('", "')'", "'=>'", "'<'", "'>'", 
			"'drop'", "'end'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, null, null, null, null, null, null, null, null, null, null, null, 
			null, null, null, null, null, "DROP", "END", "ID", "WS", "BlockComment", 
			"LineComment"
		};
	}
	private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() { return "Typestate.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public TypestateParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	public static class StartContext extends ParserRuleContext {
		public Package_statementContext p;
		public Import_statementContext import_statement;
		public List<Import_statementContext> i = new ArrayList<Import_statementContext>();
		public Typestate_declarationContext t;
		public Typestate_declarationContext typestate_declaration() {
			return getRuleContext(Typestate_declarationContext.class,0);
		}
		public Package_statementContext package_statement() {
			return getRuleContext(Package_statementContext.class,0);
		}
		public List<Import_statementContext> import_statement() {
			return getRuleContexts(Import_statementContext.class);
		}
		public Import_statementContext import_statement(int i) {
			return getRuleContext(Import_statementContext.class,i);
		}
		public StartContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_start; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof TypestateListener ) ((TypestateListener)listener).enterStart(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof TypestateListener ) ((TypestateListener)listener).exitStart(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof TypestateVisitor ) return ((TypestateVisitor<? extends T>)visitor).visitStart(this);
			else return visitor.visitChildren(this);
		}
	}

	public final StartContext start() throws RecognitionException {
		StartContext _localctx = new StartContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_start);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(25);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==T__1) {
				{
				setState(24);
				((StartContext)_localctx).p = package_statement();
				}
			}

			setState(30);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__2) {
				{
				{
				setState(27);
				((StartContext)_localctx).import_statement = import_statement();
				((StartContext)_localctx).i.add(((StartContext)_localctx).import_statement);
				}
				}
				setState(32);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(33);
			((StartContext)_localctx).t = typestate_declaration();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class RefContext extends ParserRuleContext {
		public RefContext r;
		public IdContext id() {
			return getRuleContext(IdContext.class,0);
		}
		public RefContext ref() {
			return getRuleContext(RefContext.class,0);
		}
		public RefContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_ref; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof TypestateListener ) ((TypestateListener)listener).enterRef(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof TypestateListener ) ((TypestateListener)listener).exitRef(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof TypestateVisitor ) return ((TypestateVisitor<? extends T>)visitor).visitRef(this);
			else return visitor.visitChildren(this);
		}
	}

	public final RefContext ref() throws RecognitionException {
		return ref(0);
	}

	private RefContext ref(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		RefContext _localctx = new RefContext(_ctx, _parentState);
		RefContext _prevctx = _localctx;
		int _startState = 2;
		enterRecursionRule(_localctx, 2, RULE_ref, _p);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(36);
			id();
			}
			_ctx.stop = _input.LT(-1);
			setState(43);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,2,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new RefContext(_parentctx, _parentState);
					_localctx.r = _prevctx;
					_localctx.r = _prevctx;
					pushNewRecursionContext(_localctx, _startState, RULE_ref);
					setState(38);
					if (!(precpred(_ctx, 1))) throw new FailedPredicateException(this, "precpred(_ctx, 1)");
					setState(39);
					match(T__0);
					setState(40);
					id();
					}
					} 
				}
				setState(45);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,2,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	public static class Package_statementContext extends ParserRuleContext {
		public Token t;
		public RefContext ref() {
			return getRuleContext(RefContext.class,0);
		}
		public Package_statementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_package_statement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof TypestateListener ) ((TypestateListener)listener).enterPackage_statement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof TypestateListener ) ((TypestateListener)listener).exitPackage_statement(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof TypestateVisitor ) return ((TypestateVisitor<? extends T>)visitor).visitPackage_statement(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Package_statementContext package_statement() throws RecognitionException {
		Package_statementContext _localctx = new Package_statementContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_package_statement);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(46);
			((Package_statementContext)_localctx).t = match(T__1);
			setState(47);
			ref(0);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Import_statementContext extends ParserRuleContext {
		public Token t;
		public Token s;
		public Token star;
		public RefContext ref() {
			return getRuleContext(RefContext.class,0);
		}
		public Import_statementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_import_statement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof TypestateListener ) ((TypestateListener)listener).enterImport_statement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof TypestateListener ) ((TypestateListener)listener).exitImport_statement(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof TypestateVisitor ) return ((TypestateVisitor<? extends T>)visitor).visitImport_statement(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Import_statementContext import_statement() throws RecognitionException {
		Import_statementContext _localctx = new Import_statementContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_import_statement);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(49);
			((Import_statementContext)_localctx).t = match(T__2);
			setState(51);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==T__3) {
				{
				setState(50);
				((Import_statementContext)_localctx).s = match(T__3);
				}
			}

			setState(53);
			ref(0);
			setState(56);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==T__0) {
				{
				setState(54);
				match(T__0);
				setState(55);
				((Import_statementContext)_localctx).star = match(T__4);
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Typestate_declarationContext extends ParserRuleContext {
		public Token t;
		public TerminalNode ID() { return getToken(TypestateParser.ID, 0); }
		public Typestate_bodyContext typestate_body() {
			return getRuleContext(Typestate_bodyContext.class,0);
		}
		public TerminalNode EOF() { return getToken(TypestateParser.EOF, 0); }
		public Typestate_declarationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typestate_declaration; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof TypestateListener ) ((TypestateListener)listener).enterTypestate_declaration(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof TypestateListener ) ((TypestateListener)listener).exitTypestate_declaration(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof TypestateVisitor ) return ((TypestateVisitor<? extends T>)visitor).visitTypestate_declaration(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Typestate_declarationContext typestate_declaration() throws RecognitionException {
		Typestate_declarationContext _localctx = new Typestate_declarationContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_typestate_declaration);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(58);
			((Typestate_declarationContext)_localctx).t = match(T__5);
			setState(59);
			match(ID);
			setState(60);
			match(T__6);
			setState(61);
			typestate_body();
			setState(62);
			match(T__7);
			setState(63);
			match(EOF);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Typestate_bodyContext extends ParserRuleContext {
		public State_declarationContext state_declaration;
		public List<State_declarationContext> s = new ArrayList<State_declarationContext>();
		public List<State_declarationContext> state_declaration() {
			return getRuleContexts(State_declarationContext.class);
		}
		public State_declarationContext state_declaration(int i) {
			return getRuleContext(State_declarationContext.class,i);
		}
		public Typestate_bodyContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typestate_body; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof TypestateListener ) ((TypestateListener)listener).enterTypestate_body(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof TypestateListener ) ((TypestateListener)listener).exitTypestate_body(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof TypestateVisitor ) return ((TypestateVisitor<? extends T>)visitor).visitTypestate_body(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Typestate_bodyContext typestate_body() throws RecognitionException {
		Typestate_bodyContext _localctx = new Typestate_bodyContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_typestate_body);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(68);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==ID) {
				{
				{
				setState(65);
				((Typestate_bodyContext)_localctx).state_declaration = state_declaration();
				((Typestate_bodyContext)_localctx).s.add(((Typestate_bodyContext)_localctx).state_declaration);
				}
				}
				setState(70);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class State_declarationContext extends ParserRuleContext {
		public Token name;
		public StateContext state() {
			return getRuleContext(StateContext.class,0);
		}
		public TerminalNode ID() { return getToken(TypestateParser.ID, 0); }
		public State_declarationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_state_declaration; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof TypestateListener ) ((TypestateListener)listener).enterState_declaration(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof TypestateListener ) ((TypestateListener)listener).exitState_declaration(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof TypestateVisitor ) return ((TypestateVisitor<? extends T>)visitor).visitState_declaration(this);
			else return visitor.visitChildren(this);
		}
	}

	public final State_declarationContext state_declaration() throws RecognitionException {
		State_declarationContext _localctx = new State_declarationContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_state_declaration);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(71);
			((State_declarationContext)_localctx).name = match(ID);
			setState(72);
			match(T__8);
			setState(73);
			state();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class StateContext extends ParserRuleContext {
		public Token t;
		public MethodContext method;
		public List<MethodContext> m = new ArrayList<MethodContext>();
		public List<MethodContext> method() {
			return getRuleContexts(MethodContext.class);
		}
		public MethodContext method(int i) {
			return getRuleContext(MethodContext.class,i);
		}
		public TerminalNode DROP() { return getToken(TypestateParser.DROP, 0); }
		public TerminalNode END() { return getToken(TypestateParser.END, 0); }
		public StateContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_state; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof TypestateListener ) ((TypestateListener)listener).enterState(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof TypestateListener ) ((TypestateListener)listener).exitState(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof TypestateVisitor ) return ((TypestateVisitor<? extends T>)visitor).visitState(this);
			else return visitor.visitChildren(this);
		}
	}

	public final StateContext state() throws RecognitionException {
		StateContext _localctx = new StateContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_state);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(75);
			((StateContext)_localctx).t = match(T__6);
			setState(90);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==ID) {
				{
				setState(76);
				((StateContext)_localctx).method = method();
				((StateContext)_localctx).m.add(((StateContext)_localctx).method);
				setState(81);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,6,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(77);
						match(T__9);
						setState(78);
						((StateContext)_localctx).method = method();
						((StateContext)_localctx).m.add(((StateContext)_localctx).method);
						}
						} 
					}
					setState(83);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,6,_ctx);
				}
				setState(88);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==T__9) {
					{
					setState(84);
					match(T__9);
					setState(85);
					match(DROP);
					setState(86);
					match(T__10);
					setState(87);
					match(END);
					}
				}

				}
			}

			setState(92);
			match(T__7);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class MethodContext extends ParserRuleContext {
		public Token name;
		public RefContext ref;
		public List<RefContext> args = new ArrayList<RefContext>();
		public RefContext return_type;
		public TerminalNode ID() { return getToken(TypestateParser.ID, 0); }
		public List<RefContext> ref() {
			return getRuleContexts(RefContext.class);
		}
		public RefContext ref(int i) {
			return getRuleContext(RefContext.class,i);
		}
		public StateContext state() {
			return getRuleContext(StateContext.class,0);
		}
		public Decision_stateContext decision_state() {
			return getRuleContext(Decision_stateContext.class,0);
		}
		public MethodContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_method; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof TypestateListener ) ((TypestateListener)listener).enterMethod(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof TypestateListener ) ((TypestateListener)listener).exitMethod(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof TypestateVisitor ) return ((TypestateVisitor<? extends T>)visitor).visitMethod(this);
			else return visitor.visitChildren(this);
		}
	}

	public final MethodContext method() throws RecognitionException {
		MethodContext _localctx = new MethodContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_method);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(94);
			((MethodContext)_localctx).name = match(ID);
			setState(95);
			match(T__11);
			setState(104);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << DROP) | (1L << END) | (1L << ID))) != 0)) {
				{
				setState(96);
				((MethodContext)_localctx).ref = ref(0);
				((MethodContext)_localctx).args.add(((MethodContext)_localctx).ref);
				setState(101);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==T__9) {
					{
					{
					setState(97);
					match(T__9);
					setState(98);
					((MethodContext)_localctx).ref = ref(0);
					((MethodContext)_localctx).args.add(((MethodContext)_localctx).ref);
					}
					}
					setState(103);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				}
			}

			setState(106);
			match(T__12);
			setState(107);
			match(T__10);
			setState(108);
			((MethodContext)_localctx).return_type = ref(0);
			setState(109);
			match(T__13);
			setState(113);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case DROP:
			case END:
			case ID:
				{
				setState(110);
				ref(0);
				}
				break;
			case T__6:
				{
				setState(111);
				state();
				}
				break;
			case T__14:
				{
				setState(112);
				decision_state();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Decision_stateContext extends ParserRuleContext {
		public Token t;
		public DecisionContext decision;
		public List<DecisionContext> decisions = new ArrayList<DecisionContext>();
		public List<DecisionContext> decision() {
			return getRuleContexts(DecisionContext.class);
		}
		public DecisionContext decision(int i) {
			return getRuleContext(DecisionContext.class,i);
		}
		public Decision_stateContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_decision_state; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof TypestateListener ) ((TypestateListener)listener).enterDecision_state(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof TypestateListener ) ((TypestateListener)listener).exitDecision_state(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof TypestateVisitor ) return ((TypestateVisitor<? extends T>)visitor).visitDecision_state(this);
			else return visitor.visitChildren(this);
		}
	}

	public final Decision_stateContext decision_state() throws RecognitionException {
		Decision_stateContext _localctx = new Decision_stateContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_decision_state);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(115);
			((Decision_stateContext)_localctx).t = match(T__14);
			setState(116);
			((Decision_stateContext)_localctx).decision = decision();
			((Decision_stateContext)_localctx).decisions.add(((Decision_stateContext)_localctx).decision);
			setState(121);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__9) {
				{
				{
				setState(117);
				match(T__9);
				setState(118);
				((Decision_stateContext)_localctx).decision = decision();
				((Decision_stateContext)_localctx).decisions.add(((Decision_stateContext)_localctx).decision);
				}
				}
				setState(123);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(124);
			match(T__15);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class DecisionContext extends ParserRuleContext {
		public Token label;
		public TerminalNode ID() { return getToken(TypestateParser.ID, 0); }
		public RefContext ref() {
			return getRuleContext(RefContext.class,0);
		}
		public StateContext state() {
			return getRuleContext(StateContext.class,0);
		}
		public DecisionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_decision; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof TypestateListener ) ((TypestateListener)listener).enterDecision(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof TypestateListener ) ((TypestateListener)listener).exitDecision(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof TypestateVisitor ) return ((TypestateVisitor<? extends T>)visitor).visitDecision(this);
			else return visitor.visitChildren(this);
		}
	}

	public final DecisionContext decision() throws RecognitionException {
		DecisionContext _localctx = new DecisionContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_decision);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(126);
			((DecisionContext)_localctx).label = match(ID);
			setState(127);
			match(T__13);
			setState(130);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case DROP:
			case END:
			case ID:
				{
				setState(128);
				ref(0);
				}
				break;
			case T__6:
				{
				setState(129);
				state();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class IdContext extends ParserRuleContext {
		public TerminalNode DROP() { return getToken(TypestateParser.DROP, 0); }
		public TerminalNode END() { return getToken(TypestateParser.END, 0); }
		public TerminalNode ID() { return getToken(TypestateParser.ID, 0); }
		public IdContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_id; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof TypestateListener ) ((TypestateListener)listener).enterId(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof TypestateListener ) ((TypestateListener)listener).exitId(this);
		}
		@Override
		public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
			if ( visitor instanceof TypestateVisitor ) return ((TypestateVisitor<? extends T>)visitor).visitId(this);
			else return visitor.visitChildren(this);
		}
	}

	public final IdContext id() throws RecognitionException {
		IdContext _localctx = new IdContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_id);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(132);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << DROP) | (1L << END) | (1L << ID))) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public boolean sempred(RuleContext _localctx, int ruleIndex, int predIndex) {
		switch (ruleIndex) {
		case 1:
			return ref_sempred((RefContext)_localctx, predIndex);
		}
		return true;
	}
	private boolean ref_sempred(RefContext _localctx, int predIndex) {
		switch (predIndex) {
		case 0:
			return precpred(_ctx, 1);
		}
		return true;
	}

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\3\30\u0089\4\2\t\2"+
		"\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13"+
		"\t\13\4\f\t\f\4\r\t\r\3\2\5\2\34\n\2\3\2\7\2\37\n\2\f\2\16\2\"\13\2\3"+
		"\2\3\2\3\3\3\3\3\3\3\3\3\3\3\3\7\3,\n\3\f\3\16\3/\13\3\3\4\3\4\3\4\3\5"+
		"\3\5\5\5\66\n\5\3\5\3\5\3\5\5\5;\n\5\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\7\7"+
		"\7E\n\7\f\7\16\7H\13\7\3\b\3\b\3\b\3\b\3\t\3\t\3\t\3\t\7\tR\n\t\f\t\16"+
		"\tU\13\t\3\t\3\t\3\t\3\t\5\t[\n\t\5\t]\n\t\3\t\3\t\3\n\3\n\3\n\3\n\3\n"+
		"\7\nf\n\n\f\n\16\ni\13\n\5\nk\n\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\5\nt\n\n"+
		"\3\13\3\13\3\13\3\13\7\13z\n\13\f\13\16\13}\13\13\3\13\3\13\3\f\3\f\3"+
		"\f\3\f\5\f\u0085\n\f\3\r\3\r\3\r\2\3\4\16\2\4\6\b\n\f\16\20\22\24\26\30"+
		"\2\3\3\2\23\25\2\u008b\2\33\3\2\2\2\4%\3\2\2\2\6\60\3\2\2\2\b\63\3\2\2"+
		"\2\n<\3\2\2\2\fF\3\2\2\2\16I\3\2\2\2\20M\3\2\2\2\22`\3\2\2\2\24u\3\2\2"+
		"\2\26\u0080\3\2\2\2\30\u0086\3\2\2\2\32\34\5\6\4\2\33\32\3\2\2\2\33\34"+
		"\3\2\2\2\34 \3\2\2\2\35\37\5\b\5\2\36\35\3\2\2\2\37\"\3\2\2\2 \36\3\2"+
		"\2\2 !\3\2\2\2!#\3\2\2\2\" \3\2\2\2#$\5\n\6\2$\3\3\2\2\2%&\b\3\1\2&\'"+
		"\5\30\r\2\'-\3\2\2\2()\f\3\2\2)*\7\3\2\2*,\5\30\r\2+(\3\2\2\2,/\3\2\2"+
		"\2-+\3\2\2\2-.\3\2\2\2.\5\3\2\2\2/-\3\2\2\2\60\61\7\4\2\2\61\62\5\4\3"+
		"\2\62\7\3\2\2\2\63\65\7\5\2\2\64\66\7\6\2\2\65\64\3\2\2\2\65\66\3\2\2"+
		"\2\66\67\3\2\2\2\67:\5\4\3\289\7\3\2\29;\7\7\2\2:8\3\2\2\2:;\3\2\2\2;"+
		"\t\3\2\2\2<=\7\b\2\2=>\7\25\2\2>?\7\t\2\2?@\5\f\7\2@A\7\n\2\2AB\7\2\2"+
		"\3B\13\3\2\2\2CE\5\16\b\2DC\3\2\2\2EH\3\2\2\2FD\3\2\2\2FG\3\2\2\2G\r\3"+
		"\2\2\2HF\3\2\2\2IJ\7\25\2\2JK\7\13\2\2KL\5\20\t\2L\17\3\2\2\2M\\\7\t\2"+
		"\2NS\5\22\n\2OP\7\f\2\2PR\5\22\n\2QO\3\2\2\2RU\3\2\2\2SQ\3\2\2\2ST\3\2"+
		"\2\2TZ\3\2\2\2US\3\2\2\2VW\7\f\2\2WX\7\23\2\2XY\7\r\2\2Y[\7\24\2\2ZV\3"+
		"\2\2\2Z[\3\2\2\2[]\3\2\2\2\\N\3\2\2\2\\]\3\2\2\2]^\3\2\2\2^_\7\n\2\2_"+
		"\21\3\2\2\2`a\7\25\2\2aj\7\16\2\2bg\5\4\3\2cd\7\f\2\2df\5\4\3\2ec\3\2"+
		"\2\2fi\3\2\2\2ge\3\2\2\2gh\3\2\2\2hk\3\2\2\2ig\3\2\2\2jb\3\2\2\2jk\3\2"+
		"\2\2kl\3\2\2\2lm\7\17\2\2mn\7\r\2\2no\5\4\3\2os\7\20\2\2pt\5\4\3\2qt\5"+
		"\20\t\2rt\5\24\13\2sp\3\2\2\2sq\3\2\2\2sr\3\2\2\2t\23\3\2\2\2uv\7\21\2"+
		"\2v{\5\26\f\2wx\7\f\2\2xz\5\26\f\2yw\3\2\2\2z}\3\2\2\2{y\3\2\2\2{|\3\2"+
		"\2\2|~\3\2\2\2}{\3\2\2\2~\177\7\22\2\2\177\25\3\2\2\2\u0080\u0081\7\25"+
		"\2\2\u0081\u0084\7\20\2\2\u0082\u0085\5\4\3\2\u0083\u0085\5\20\t\2\u0084"+
		"\u0082\3\2\2\2\u0084\u0083\3\2\2\2\u0085\27\3\2\2\2\u0086\u0087\t\2\2"+
		"\2\u0087\31\3\2\2\2\20\33 -\65:FSZ\\gjs{\u0084";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}