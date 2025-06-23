/* -*- mode: asm; fill-column: 88; tab-width: 8; -*- */

	/*

	Get Started with Arpilisp

	Side note: if this file looks funny, use a text editor that displays a
	fixed-width font and uses 8 spaces for tabs.

	Arpilisp is the Assembled Raspberry Pi Lisp.  This tutorial uses assembly
	language, the lowest level programming language, to implement an interpreter for
	Lisp, the highest level language.  Arpilisp implements Lisp from scratch, with
	no help from any libraries and minimal help from the kernel.

	Before you use arpilisp, you need to build it.  To build arpilisp, you need
	a few things:

	* A Raspberry Pi running Raspbian

	* That's it.

	To build arpilisp, type the following in a shell:

	gcc -nostdlib -o arpilisp arpilisp.s

	This tells the GNU compiler (gcc) to use arpilisp.s, this file that you are
	reading, as input, to build an executable file named arpilisp.  Later we
	cover what the -nostdlib option does.

	To use arpilisp itself, type this:

	./arpilisp

	To quit arpilisp, type Ctrl-D.

	References

	https://www.raspberrypi.org

	https://www.raspbian.org
	
	________________________________________________________________________________

	Acknowledgements

	Thanks to Richard W.M. Jones, John McCarthy, and Jack W. Crenshaw for showing us
	the simplicity and elegance hiding in complex things, and for reminding us that
	computers are awesome.

	Thanks also to Chris Hinsley and Jay Sissom for feedback.

	________________________________________________________________________________

	The Shortest Introduction to Lisp

	Lisp is one of the oldest programming languages in computing.  John McCarthy and
	his team implemented the first Lisp interpreter in 1960.  They wanted a
	programming language for AI research.  Since then, he and many others discovered
	amazing things about how Lisp can make computers do surprising, interesting
	things.

	Most people acknowledge Lisp's influence without knowing what it is exactly.
	Others are turned off by Lisp's weirdness.  Lisp continues to influence
	programming languages today.  It's not as inaccessible, alien, or irrelevant as
	you might believe.

	Lisp is an acronym for LISt Processor.  A list is a sequence of items.  To
	specify a list, we start with an opening parenthesis, continue with the things
	in the list, and end with a closing parenthesis.

	For example, here's a list of ingredients for a salad:

	(lettuce tomato oil vinegar)

	We can use extra spaces to clarify what we type but Lisp doesn't care about
	excessive white space between list items, as long as there is some.  And there's
	no need to put white space around parentheses.  Here are some lists that look
	different to us but mean the same to Lisp:

	(lettuce  tomato   oil	  vinegar     )

	(lettuce
	tomato
	oil
	vinegar)

	A list may itself also contain lists.  Here's a list that is different from our
	previous list:

	((lettuce tomato) (oil vinegar))

	While it contains the same items, they are arranged into two sub-lists, the
	first for the vegetables (lettuce tomato), the second for the dressing (oil
	vinegar).

	Here's an empty list:

	()

	The empty list comes up so much that Lisp has a symbol for it:

	nil

	A symbol is just a name that represents something.  What the symbol represents
	depends on you, the programmer.  In our list above, lettuce, tomato, oil, and
	vinegar are symbols for salad ingredients.  We sometimes refer to symbols as
	atoms.  Unlike lists, atoms are not composed of parts.

	In Lisp, lists and atoms are called "S-expressions".  We also use just
	"expression" to mean the same thing.

	The Processor part of LISt Processor means that we program a Lisp
	interpreter to manipulate lists.  We manipulate lists with functions that accept
	S-expression arguments.

	Here's a cool thing about Lisp: functions are also written as S-expressions.  In
	other words, data and programs have the same form.

	Side note: A lot of people make a big deal about the interchangeability of Lisp
	data and programs.  That's certainly remarkable and accounts for a lot of Lisp's
	elegance and expressiveness.  The irony is that after getting used to this idea,
	you find it odd that other programming languages don't have this feature.

	To see how functions work, let's take a closer look at nil and the empty list.
	To verify that they are the same, we enter this expression in Lisp:

	(eq nil () )

	The first item in the list of our expression is eq, which is a function.  A
	function tells Lisp what needs to be done: we want to determine equality.  The
	remaining items in the list, nil and (), specify the data that we want the eq
	function to process.

	Lisp responds with:

	t

	The t symobl means that the result of this expression is true; nil and () are
	indeed equal. Using the t symbol to represent "true" is a Lisp convention, while
	nil is the conventional value for false.

	In Lisp, we say "function application" to mean applying a function to arguments.
	In our example, we apply eq to two arguments.  The function compares them for
	equality, then returns a "t" for true or "nil" for false.

	Before Lisp applies a function to its arguments, it first evaluates each
	argument.  For example, let's take a look at a more complicated expression:

	(eq
	  (eq nil ())
	  (eq () nil))

	In this expression, we give eq these arguments:

	(eq nil ())

	and

	(eq () nil)
	
	We can't determine how to compute the value of the first eq application until we
	know the values of its arguments.  Behind the scenes, that's what Lisp does.  If
	we could watch the internal computation of our S-expression, we would see Lisp
	first compute then substitute the values of the arguments.

	This substitution renders our expression from this:

	(eq
	  (eq nil ())
	  (eq () nil))

	to this:
	
	(eq
	  t
	  (eq () nil))

	then this:

	(eq
	  t
	  t)

	and finally:

	t

	Lisp evaluates expressions recursively by computing the values of the deepest
	argument expressions before computing arguments that are higher up.

	Lisp has some useful built-in functions.  An important function is quote.  It
	takes a single argument and returns it unevaluated.

	For example, when we type our salad list into Lisp:

	(lettuce tomato oil vinegar)

	Lisp returns an error about "lettuce" not being a function.

	To make it clear that our salad list is a list and not an applicaton of the
	lettuce function, we enter:

	(quote (lettuce tomato oil vinegar))

	Lisp responds with:

	(lettuce tomato oil vinegar)

	Lisp wouldn't be useful if we couldn't create our own functions.  To describe a
	function, we use a lambda expression.  A lambda expression takes a list of
	parameters then a sequence of S-expressions to evaluate.

	Here's an example of a lambda function that accepts a single parameter to
	compute its equality with nil:

	(lambda (x) (eq nil x))

	Each parameter in the parameter list is a symbol that represents an argument at
	application time.  The sequence of S-expressions in the lambda may refer to
	these parameters. In fact, it's good practice to make sure that the
	S-expressions in a lambda refer only to the lambda parameters.

	Lisp treats the last S-expression in the lambda specially.  Its value is the
	value that the lambda returns when Lisp applies it.

	In our lambda above, (x) is the list of parameters.  In this case, we have a
	single parameter, x.  The S-expression (eq nil x) is the only expression in our
	lambda.  It's also the last expression, so the lambda returns the value of this
	expression when we apply the lambda.

	When you enter a lambda by itself:

	(lambda (x) (eq nil x))

	Lisp returns the lambda, unapplied: 

	(lambda (x) (eq nil x))

	If we want to apply our lambda to an argument, we need to use the same form as a
	function application:

	(function argument ...)

	We just need to replace function with a lambda expression.

	For example, if we enter:

	((lambda (x) (eq nil x))
	  (quote (lettuce tomato oil vinegar)))

	Lisp applies our lambda like a regular function application by following these
	steps:

	1. Evaluate the expressions of the arguments. 

	2. Bind each evaluated argument, in the order it appears in the application, to
	each of the parameters in the order that they appear in lambda parameter list.

	3. Evaluate each expression in the lambda.  When an expression refers to a
	parameter, Lisp evaluates it by substituting its bound value from step 2.

	4. Return the value of the last expression in the lambda and unbind its
	parameters.

	In our example above, in step 1, Lisp first evaluates the lone argument:

	(quote (lettuce tomato oil vinegar))

	which gives:

	(lettuce tomato oil vinegar)

	In step 2, Lisp binds this evaluated argument to the parameter, x. 

	For step 3, Lisp evaluates the lambda body, replacing occurrences of x with the
	value it is bound to. This:

	(eq nil x)

	becomes:

	(eq nil (lettuce tomato oil vinegar))

	Our argument, (lettuce tomato oil vinegar), is not nil, so eq returns nil. Since
	this is the only and last expression in the lambda, the lambda application
	returns nil.  Returning nil for a non-nil argument is ironic until you remember
	that nil means false in this case.
	
	The parameter bindings in a lambda application last only during the application
	of the lambda.  In step 4, Lisp unbinds the lambda's parameters.  The x argument
	has no value.

	So entering this expression after applying our lambda:

	x

	gives an error about an unbound variable.

	Outside of a lambda, we can bind a symbol to a value so that when you enter the
	symbol, Lisp returns the value.  For example this expression binds "name"
	to Valerie:

	(define name (quote Valerie))

	So entering this expression:

	name

	evaluates to what you would expect:
	
	Valerie

	You can also change an existing binding:

	(define name (quote Isabelle))

	Bindings in a lambda temporarily override outside bindings.  For example:

	(define name (quote James))

	((lambda (name) name) (quote Rose))

	returns:

	Rose

	Inside the lambda application, the name symbol is bound to Rose.  When the
	lambda application returns, the previous binding to name is restored, so that
	entering this expression:

	name

	returns this:

	James

	Of course, you can also bind a lambda to a symbol, which makes the lambda easier
	to use if you intend to refer to it frequently.  For example, comparison with
	nil is something we see enough that we define a handy function for it:

	(define null (lambda (x) (eq x nil)))

	Now comparison to nil is more convenient:

	(null (quote Gus))

	returns:

	nil

	Notice that the define function plays by different rules than a normal function.
	Instead of evaluating its first argument, define takes it literally, as if it
	were quoted.  Therefore by definition (ahem), define isn't a true function.  In
	Lisp, we call this a "special form".  If you paid enough attention earlier, you
	noticed that quote and lambda are also special forms.  Lambda doesn't evaluate
	its list of parameters. And Lisp delays the evaluation of a lambda's expressions
	until it applies the lambda.

	Another special form is cond, which is short for "conditional".  It takes a list
	of clauses.

	(cond clause1 clause2... clauseN)
	
	Each clause is a list of 2 expressions:

	(test result)
	
	If the test expression is true, then cond returns the value of the corresponding
	result expression and stops processing subsequent clauses.  If the test is
	false, cond proceeds to the next clause.  It continues doing so until it finds a
	test that returns true or there are no more expressions.

	For example, to evaluate this expression:

	(cond
	  ((eq name (quote Valerie))  (quote funny))
	  ((eq name (quote James))    (quote silly))
	  (t                          (quote goodbye)))

	Lisp starts with the first clause:

	((eq name (quote Valerie))  (quote funny))

	which has this test expression:

	(eq name (quote Valerie))

	Given our most recent binding for name, this test evaluates to false.  Lisp
	skips to the next clause:

	((eq name (quote James)) (quote silly))

	The test expression evaluates to true.  So Lisp evaluates the second
	expression in this clause, which returns:

	silly

	which becomes the value of our cond expression.  Lisp ignores subsequent
	clauses, so the clause:

	(t (quote goodbye))
	
	doesn't get evaluated.
	
	As a matter of good habit, we always put a final clause in a cond that has t for
	a condition clause's test.  That way we assure ourselves that a cond test
	returns a value that we specify when all other clause tests are false.

	Lisp isn't all special forms.  In fact, there are only a handful.  Most Lisp
	built-in functions evaluate arguments normally.

	For example, the most frequently-used Lisp functions are car and cdr.  For now,
	we partly describe what they do, with full details of their awesomeness later.

	The car function returns the first item in a list and cdr returns a list without
	its first item.  Helpful synonyms for these functions are "first" and "rest",
	respectively.

	For example, let's define our salad list:

	(define salad (quote (lettuce tomato oil vinegar)))

	Entering this:	

	(car salad)

	returns this:

	lettuce

	And entering

	(cdr salad)

	returns

	(tomato oil vinegar)

	To pick individual items in a list, we combine car and cdr.  Entering:

	(car (cdr salad))

	gives us:

	tomato

	This expression:

	(car (cdr (cdr salad)))

	gives us:

	oil

	Note that car and cdr do not modify the list that is bound to salad.  Car
	doesn't reduce a list to its first item, it only tells you what that first item
	is.  Likewise, cdr doesn't remove the first item from a list, it only tells you
	what a list is without its first item.  So after applying car and cdr to salad,
	salad still has its most recent binding.  Entering:

	salad

	still gives us:
	
	(lettuce tomato oil vinegar)	

	Functions that don't modify bindings other than their own are called "pure
	functions", which is another way of saying that you, the Lisp user, don't have
	to worry about unintended consequences when applying a function.  Function purity
	is an important and useful quality in Lisp programming.

	So that's most of the basics of Lisp that we implement in arpilisp.  We'll cover
	the rest as we go.

	References

	McCarthy, "Recursive Functions of Symbolic Expressions and Their Computation by
	Machine, Part I". MIT. 1960.

	McCarthy, et al. "Lisp I Programmer's Manual." MIT. 1960.

	________________________________________________________________________________

	The Shortest Introduction to ARM Assembly Language

	Today's computers have ample capacity to let us pile on layers of operating
	systems, shells, libraries, frameworks, sandboxes, and containers.  So much that
	we've successfully buried assembly language completely out of mind.  But the
	machine is still there, and not as inaccessible, alien, or irrelevant as you
	might believe.

	The Raspberry Pi uses an ARM processor.  The ARM registers that we use in
	arpilisp are 32 bits wide.  Registers r0 to r12 are general-purpose and registers
	r13 to r15 are single-purpose, at least for our purposes.   Register r13 is the
	stack pointer (sp), r14 is the link register (lr), and r15 the program counter
	(pc).

	An extra register, the processor status register (apsr), offers a few single-bit
	condition flags that optionally record effects of previous instructions.

	To manipulate data, like arithmetic, comparison, bit fiddling, and
	so on, ARM requires that we use registers.  ARM has no instructions to
	manipulate data in memory.  Before we operate on something, we first need to
	load it from memory to a register.  In fact, we can't even directly load from or
	store to memory.  We need to load an address into a register then use that
	register to specify the location of a load (ldr) or store (str) operation.

	Side note: Yes, loading an address before loading the contents of the address
	seems like a chicken and egg situation.  It's not, of course.  The assembler and
	the processor play some convenient tricks to solve this problem for us.

	In GNU assembler, constants begin with a pound sign (#).  Addresses start with an
	equal sign (=).

	Labels end with a colon (:).  Labels must be unique with one exception: numeric
	labels.  An assembly file can repeat numeric labels, which makes them handy for
	throw-away labels in an assembly procedure.  An instruction refers to a
	numerical label by post-fixing an "f" or "b", which refers to the matching
	numerical label "forward" or "before" the instruction.

	ARM, the company, and others specify calling conventions and ABIs, but we'll
	ignore them to keep things simple.  Arpilisp is for learning Lisp and assembler,
	not for conforming.

	References

	https://en.wikipedia.org/wiki/IBM_704

	ARM Limited. "ARMv6-M Architecture Reference Manual." 2010.

	________________________________________________________________________________

	Targeting the Processor

	Let's get started.  We begin at the foundation then go on until we get to the
	end: a working Lisp interpreter.

	We target the ARMv6 instruction set, which works with all variants of the
	Raspberry Pi up to the Pi 3.

	*/
