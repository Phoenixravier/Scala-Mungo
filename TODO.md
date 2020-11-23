


### Todo
BUGS
I think the function code will fail if there is a variable with 
the same name as a new variable in an earlier scope.
cat
func{
	val cat = new Cat()
}
Match statements can also return things and this is not dealt with

TESTS
Test that the constructor of a class like Cat without parameters is gone through (one which matches List(Ident(name)) rather than List(Apply(elementName, arg2)) (which is tested)
Test that only the relevant instances exist in the set of instances (as opposed to null scope for example or instances which should have been deleted upon leaving a function)

************SEP***************
MUST HAVE

SHOULD HAVE
- [ ] Do the match thing but with if for methods that return boolean values (if(cat.run()))
- [ ] Deal with functions inside a class which use parameters defined in that class (the richint, to/until thing discovered in for loop conditions) (CHECK)
- [ ] Deal with return in functions


COULD HAVE
- [ ] Deal with Lamdba functions
- [ ] Deal with special cases of for loops (known counts)
- [ ] Deal with threading
- [ ] Deal with foreach and other map like things
- [ ] Deal with try-catch (advanced, handling exceptions)
- [ ] Deal with globals (have special scope like GLOBAL, or empty scope, does my code deal with this already?) (CHECK)
- [ ] Deal with collections (for-yield assignment)
- [ ] Deal with traits
- [ ] Short circuit boolean expressions
- [ ] Add end to a protocol file if it doesn't have end in it
- [ ] Add syntactic sugar for in and when so the parenthesis aren't needed
- [ ] Print instances at end of file/at error (EASY)

************OCT****************
MUST HAVE
- [ ] Make code readable
- [ ] More tests (on Util)
- [ ] Tutorial

SHOULD HAVE
- [ ] User testing
- [ ] Make nice documentation hosted somewhere (git pages)
- [ ] Support Linux

************NOV****************
- [ ] Scala-stMungo

************DEC****************
- [ ] Case study 


### In Progress
- [ ] Add interesting aliasing examples
- [ ] Deal with class attributes


### Done ✓

- [x] Read some research papers
- [x] Install editor, scala, mungo, new mungo
- [x] Contact Joao, Laura, scala contributors, stack overflow, scala users
- [x] Create the DSL for writing a protocol
- [x] Write tests for the DSL
- [x] Deal with basic method call protocol checking for one class
- [x] Deal with multiple instances of the same class
- [x] Deal with multiple classes
- [x] Write tests for the plugin
- [x] Deal with If-Else statements 
- [x] Deal with try-catch (basic)
- [x] Deal with loops
- [x] Deal with function calls
- [x] Deal with return values (and subsequent path possibilities)
- [x] Does not work with methods with parameters
- [x] Only works with code inside "App"
- [x] Deal with if on its own
- [x] Deal with code inside object which contains main function 
- [x] Deal with while(true) and do while(true) as special cases
- [x] Deal with code inside parameters and conditions
- [x] Take into account linearity and deal with return values when they are protocolled instances, assignments, if/else (aliasing)
- [x] Deal with match statements
- [x] Deal with duplicate aliases in a function call
- [x] Cash statex to statey for functions to make things faster so we don't have to do the entire check each time 
- [x] Recursive functions
- [x] Make things faster
- [x] Make caching work with duplicate parameters
- [x] Deal with parameters/returned things which are "new Class"
- [x] Get functions to check what is returned in params rather than just strings given 
- [x] Get function to return something sensible and deal with that returned value
- [x] Deal with apply function
- [x] Deal with companion objects
- [x] Deal with code on itself in constructors
- [x] Functions in protocol when called should check that, if they change the state of a protocolled object, change it in the same way as it should be by protocol
- [x] Functions should track "this" as well
- [x] Build local (basic)
- [x] Finish MVP 
- [x] Functions should also check that parameter types are the same to match functions together
- [x] Make match statements check if they are matching protocolled methods and advance state by method:returnvalue if possible in a case statement
- [x] Deal with break
- [x] Build online
- [x] Build local
- [x] Technical report on Scala-Mungo
- [x] Remove need for batch script
- [x] Make work on more than a single file
- [x] Add interesting non aliasing examples
- [x] Make possible transitions show on transition error