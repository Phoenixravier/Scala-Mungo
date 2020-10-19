


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

************SEP***************
MUST HAVE
- [ ] Functions should also check that parameter types are the same to match functions together

SHOULD HAVE
- [ ] Deal with class attributes
- [ ] Deal with functions inside a class which use parameters defined in that class (the richint, to/until thing discovered in for loop conditions)
- [ ] Deal with break and return (alternative control flows)
- [ ] Deal with Lamdba functions
- [ ] Deal with special cases of for loops (known counts)
- [ ] Deal with threading
- [ ] Deal with foreach and other map like things
- [ ] Deal with try-catch (advanced, handling exceptions)
- [ ] Deal with globals (have special scope like GLOBAL?)
- [ ] Deal with collections (for-yield assignment)

COULD HAVE
- [ ] Traits
- [ ] Add end to a protocol file if it doesn't have end in it
- [ ] Add syntactic sugar for in and when so the parenthesis aren't needed
- [ ] Print instances at end of file/at error

************OCT****************
MUST HAVE
- [ ] Finish MVP 
- [ ] Make work on more than a single file
- [ ] Make code readable

SHOULD HAVE
- [ ] Technical report on Scala-Mungo
- [ ] Make nice documentation hosted somewhere (git pages)
- [ ] Add interesting examples
- [ ] Support Linux

************NOV****************
- [ ] Scala-stMungo

************DEC****************
- [ ] Case study 


### In Progress
- [ ] Build
- [ ] Functions in protocol when called should check that, if they change the state of a protocolled object, change it in the same way as it should be by protocol
- [ ] Functions should track "this" as well

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