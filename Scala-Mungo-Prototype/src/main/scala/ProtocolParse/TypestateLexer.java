// Generated from C:/Year five/Project/Scala-Mungo/Scala-Mungo-Prototype/src/main/scala\Typestate.g4 by ANTLR 4.8
package ProtocolParse;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.ATN;
import org.antlr.v4.runtime.atn.ATNDeserializer;
import org.antlr.v4.runtime.atn.LexerATNSimulator;
import org.antlr.v4.runtime.atn.PredictionContextCache;
import org.antlr.v4.runtime.dfa.DFA;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class TypestateLexer extends Lexer {
	static { RuntimeMetaData.checkVersion("4.8", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, T__3=4, T__4=5, T__5=6, T__6=7, T__7=8, T__8=9, 
		T__9=10, T__10=11, T__11=12, T__12=13, T__13=14, T__14=15, T__15=16, DROP=17, 
		END=18, ID=19, WS=20, BlockComment=21, LineComment=22;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	private static String[] makeRuleNames() {
		return new String[] {
			"T__0", "T__1", "T__2", "T__3", "T__4", "T__5", "T__6", "T__7", "T__8", 
			"T__9", "T__10", "T__11", "T__12", "T__13", "T__14", "T__15", "DROP", 
			"END", "ID", "WS", "BlockComment", "LineComment"
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


	public TypestateLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "Typestate.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	//@Override
	public String[] getChannelNames() { return channelNames; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2\30\u009c\b\1\4\2"+
		"\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4"+
		"\13\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22"+
		"\t\22\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\3\2\3\2\3\3\3"+
		"\3\3\3\3\3\3\3\3\3\3\3\3\3\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\5\3\5\3\5\3\5"+
		"\3\5\3\5\3\5\3\6\3\6\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\b\3\b\3"+
		"\t\3\t\3\n\3\n\3\13\3\13\3\f\3\f\3\r\3\r\3\16\3\16\3\17\3\17\3\17\3\20"+
		"\3\20\3\21\3\21\3\22\3\22\3\22\3\22\3\22\3\23\3\23\3\23\3\23\3\24\6\24"+
		"s\n\24\r\24\16\24t\3\24\7\24x\n\24\f\24\16\24{\13\24\3\25\6\25~\n\25\r"+
		"\25\16\25\177\3\25\3\25\3\26\3\26\3\26\3\26\7\26\u0088\n\26\f\26\16\26"+
		"\u008b\13\26\3\26\3\26\3\26\3\26\3\26\3\27\3\27\3\27\3\27\7\27\u0096\n"+
		"\27\f\27\16\27\u0099\13\27\3\27\3\27\3\u0089\2\30\3\3\5\4\7\5\t\6\13\7"+
		"\r\b\17\t\21\n\23\13\25\f\27\r\31\16\33\17\35\20\37\21!\22#\23%\24\'\25"+
		")\26+\27-\30\3\2\6\6\2&&C\\aac|\7\2&&\62;C\\aac|\5\2\13\f\17\17\"\"\4"+
		"\2\f\f\17\17\2\u00a0\2\3\3\2\2\2\2\5\3\2\2\2\2\7\3\2\2\2\2\t\3\2\2\2\2"+
		"\13\3\2\2\2\2\r\3\2\2\2\2\17\3\2\2\2\2\21\3\2\2\2\2\23\3\2\2\2\2\25\3"+
		"\2\2\2\2\27\3\2\2\2\2\31\3\2\2\2\2\33\3\2\2\2\2\35\3\2\2\2\2\37\3\2\2"+
		"\2\2!\3\2\2\2\2#\3\2\2\2\2%\3\2\2\2\2\'\3\2\2\2\2)\3\2\2\2\2+\3\2\2\2"+
		"\2-\3\2\2\2\3/\3\2\2\2\5\61\3\2\2\2\79\3\2\2\2\t@\3\2\2\2\13G\3\2\2\2"+
		"\rI\3\2\2\2\17S\3\2\2\2\21U\3\2\2\2\23W\3\2\2\2\25Y\3\2\2\2\27[\3\2\2"+
		"\2\31]\3\2\2\2\33_\3\2\2\2\35a\3\2\2\2\37d\3\2\2\2!f\3\2\2\2#h\3\2\2\2"+
		"%m\3\2\2\2\'r\3\2\2\2)}\3\2\2\2+\u0083\3\2\2\2-\u0091\3\2\2\2/\60\7\60"+
		"\2\2\60\4\3\2\2\2\61\62\7r\2\2\62\63\7c\2\2\63\64\7e\2\2\64\65\7m\2\2"+
		"\65\66\7c\2\2\66\67\7i\2\2\678\7g\2\28\6\3\2\2\29:\7k\2\2:;\7o\2\2;<\7"+
		"r\2\2<=\7q\2\2=>\7t\2\2>?\7v\2\2?\b\3\2\2\2@A\7u\2\2AB\7v\2\2BC\7c\2\2"+
		"CD\7v\2\2DE\7k\2\2EF\7e\2\2F\n\3\2\2\2GH\7,\2\2H\f\3\2\2\2IJ\7v\2\2JK"+
		"\7{\2\2KL\7r\2\2LM\7g\2\2MN\7u\2\2NO\7v\2\2OP\7c\2\2PQ\7v\2\2QR\7g\2\2"+
		"R\16\3\2\2\2ST\7}\2\2T\20\3\2\2\2UV\7\177\2\2V\22\3\2\2\2WX\7?\2\2X\24"+
		"\3\2\2\2YZ\7.\2\2Z\26\3\2\2\2[\\\7<\2\2\\\30\3\2\2\2]^\7*\2\2^\32\3\2"+
		"\2\2_`\7+\2\2`\34\3\2\2\2ab\7?\2\2bc\7@\2\2c\36\3\2\2\2de\7>\2\2e \3\2"+
		"\2\2fg\7@\2\2g\"\3\2\2\2hi\7f\2\2ij\7t\2\2jk\7q\2\2kl\7r\2\2l$\3\2\2\2"+
		"mn\7g\2\2no\7p\2\2op\7f\2\2p&\3\2\2\2qs\t\2\2\2rq\3\2\2\2st\3\2\2\2tr"+
		"\3\2\2\2tu\3\2\2\2uy\3\2\2\2vx\t\3\2\2wv\3\2\2\2x{\3\2\2\2yw\3\2\2\2y"+
		"z\3\2\2\2z(\3\2\2\2{y\3\2\2\2|~\t\4\2\2}|\3\2\2\2~\177\3\2\2\2\177}\3"+
		"\2\2\2\177\u0080\3\2\2\2\u0080\u0081\3\2\2\2\u0081\u0082\b\25\2\2\u0082"+
		"*\3\2\2\2\u0083\u0084\7\61\2\2\u0084\u0085\7,\2\2\u0085\u0089\3\2\2\2"+
		"\u0086\u0088\13\2\2\2\u0087\u0086\3\2\2\2\u0088\u008b\3\2\2\2\u0089\u008a"+
		"\3\2\2\2\u0089\u0087\3\2\2\2\u008a\u008c\3\2\2\2\u008b\u0089\3\2\2\2\u008c"+
		"\u008d\7,\2\2\u008d\u008e\7\61\2\2\u008e\u008f\3\2\2\2\u008f\u0090\b\26"+
		"\2\2\u0090,\3\2\2\2\u0091\u0092\7\61\2\2\u0092\u0093\7\61\2\2\u0093\u0097"+
		"\3\2\2\2\u0094\u0096\n\5\2\2\u0095\u0094\3\2\2\2\u0096\u0099\3\2\2\2\u0097"+
		"\u0095\3\2\2\2\u0097\u0098\3\2\2\2\u0098\u009a\3\2\2\2\u0099\u0097\3\2"+
		"\2\2\u009a\u009b\b\27\2\2\u009b.\3\2\2\2\b\2ty\177\u0089\u0097\3\b\2\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}