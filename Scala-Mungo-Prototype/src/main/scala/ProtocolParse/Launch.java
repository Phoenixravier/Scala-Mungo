package ProtocolParse;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;

import java.io.IOException;

import static org.antlr.v4.runtime.CharStreams.fromFileName;

public class Launch {

    public static void main(String[] args){
        try{
            String source = "MyProtocol.txt";
            CharStream stream = fromFileName(source);
            TypestateLexer lexer = new TypestateLexer(stream);
            CommonTokenStream tokens = new CommonTokenStream(lexer);
            TypestateParser parser = new TypestateParser(tokens);
            ParseTree tree = parser.start();

            MyVisitor visitor = new MyVisitor();
            visitor.visit(tree);


        } catch(IOException e){
            e.printStackTrace();
        }
    }
}
