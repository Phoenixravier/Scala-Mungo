


### Todo
BUGS
I think the function code will fail if there is a variable with 
the same name as a new variable in an earlier scope.
cat
func{
	val cat = new Cat()
}
Object is created every time checkInsideFunctionBody is called


************SEP***************
MUST HAVE
- [ ] Deal with parameters/returned things which are "new Class"
- [ ] Deal with companion objects
- [ ] Deal with apply function
- [ ] Deal with code on itself in constructor
- [ ] Deal with functions inside a class which use parameters defined in that class (the richint, to/until thing discovered in for loop conditions)
- [ ] Deal with Lamdba functions


SHOULD HAVE
- [ ] Deal with special cases of for loops (known counts)
- [ ] Deal with threading
- [ ] Deal with foreach and other map like things
- [ ] Deal with break and return (alternative control flows)
- [ ] Deal with try-catch (advanced, handling exceptions)
- [ ] Deal with globals (have special scope like GLOBAL?)
- [ ] Deal with collections (for-yield assignment)
- [ ] Deal with class attributes

COULD HAVE
- [ ] Add end to a protocol file if it doesn't have end in it


************OCT****************
- [ ] Build
- [ ] Make work on more than a single file
- [x] Make things faster

************NOV****************
- [ ] Scala-stMungo

************DEC****************
- [ ] Case study 


### In Progress
- [ ] Deal with duplicate aliases in a function call
- [ ] Cash statex to statey for functions to make things faster so we don't have to do the entire check each time 
- [ ] Recursive functions


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
- [x] Does not deal with loops
- [x] Does not deal with function calls
- [x] Does not deal with return values (and subsequent path possibilities)
- [x] Does not work with methods with parameters
- [x] Only works with code inside "App"
- [x] Deal with if on its own
- [x] Deal with code inside object which contains main function 
- [x] Deal with while(true) and do while(true) as special cases
- [x] Deal with code inside parameters and conditions
- [x] Take into account linearity and deal with return values when they are protocolled instances, assignments, if/else (aliasing)
- [x] Deal with match statements
